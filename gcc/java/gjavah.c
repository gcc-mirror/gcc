/* Program to write C++-suitable header files from a Java(TM) .class
   file.  This is similar to SUN's javah.

Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <math.h>

#include "jcf.h"
#include "tree.h"
#include "version.h"
#include "javaop.h"
#include "java-tree.h"
#include "java-opcodes.h"
#include "ggc.h"
#include "hashtab.h"
#include "intl.h"

#include <getopt.h>



/* The output file.  */
FILE *out = NULL;

/* Nonzero on failure.  */
static int found_error = 0;

#ifdef JNI_DEFAULT
#define TOOLNAME "gjnih"

/* Nonzero if we're generating JNI output.  */
int flag_jni = 1;
#else
#define TOOLNAME "gcjh"

int flag_jni = 0;
#endif

/* When nonzero, warn when source file is newer than matching class
   file.  */
int flag_newer = 1;

/* Directory to place resulting files in. Set by -d option. */
static const char *output_directory = "";

/* Directory to place temporary file.  Set by -td option.  Currently unused. */
static const char *temp_directory = "/tmp";

/* Number of friend functions we have to declare.  */
static int friend_count;

/* A class can optionally have a `friend' function declared.  If
   non-NULL, this is that function.  */
static char **friend_specs = NULL;

/* Number of lines we are prepending before the class.  */
static int prepend_count;

/* We can prepend extra lines before the class's start. */
static char **prepend_specs = NULL;

/* Number of lines we are appending at the end of the class.  */
static int add_count;

/* We can append extra lines just before the class's end. */
static char **add_specs = NULL;

/* Number of lines we are appending after the class.  */
static int append_count;

/* We can append extra lines after the class's end. */
static char **append_specs = NULL;

int verbose = 0;

int stubs = 0;

struct JCF *current_jcf;

/* This holds access information for the last field we examined.  They
   let us generate "private:", "public:", and "protected:" properly.
   If 0 then we haven't previously examined any field.  */
static JCF_u2 last_access;

/* Pass this macro the flags for a class and for a method.  It will
   return true if the method should be considered `final'.  */
#define METHOD_IS_FINAL(Class, Method) \
   (((Class) & ACC_FINAL) || ((Method) & (ACC_FINAL | ACC_PRIVATE)))

/* Pass this macro the flags for a method.  It will return true if the
   method is native.  */
#define METHOD_IS_NATIVE(Method) \
   ((Method) & ACC_NATIVE)

#define METHOD_IS_PRIVATE(Class, Method) \
  (((Method) & ACC_PRIVATE) != 0)

/* We keep a linked list of all method names we have seen.  This lets
   us determine if a method name and a field name are in conflict.  */
struct method_name
{
  unsigned char *name;
  int length;
  unsigned char *signature;
  int sig_length;
  int is_native;
  struct method_name *next;
};

/* List of method names we've seen.  */
static struct method_name *method_name_list;

static void print_field_info (FILE*, JCF*, int, int, JCF_u2);
static void print_mangled_classname (FILE*, JCF*, const char*, int);
static int  print_cxx_classname (FILE*, const char*, JCF*, int, int);
static void print_method_info (FILE*, JCF*, int, int, JCF_u2);
static void print_c_decl (FILE*, JCF*, int, int, int, const char *, int);
static void print_stub_or_jni (FILE*, JCF*, int, int, int, const char *, int);
static void print_full_cxx_name (FILE*, JCF*, int, int, int, const char *, int);
static void decompile_method (FILE*, JCF*, int) ATTRIBUTE_UNUSED;
static void add_class_decl (FILE*, JCF*, JCF_u2);

static void print_name (FILE *, JCF *, int);
static void print_base_classname (FILE *, JCF *, int);
static int utf8_cmp (const unsigned char *, int, const char *);
static char *cxx_keyword_subst (const unsigned char *, int);
static void generate_access (FILE *, JCF_u2);
static int name_is_method_p (const unsigned char *, int);
static char *get_field_name (JCF *, int, JCF_u2);
static void print_field_name (FILE *, JCF *, int, JCF_u2);
static const unsigned char *super_class_name (JCF *, int *);
static void print_include (FILE *, const unsigned char *, int);
static int gcjh_streq (const void *p1, const void *p2);
static int throwable_p (const unsigned char *signature);
static const unsigned char *
  decode_signature_piece (FILE *, const unsigned char *,
			  const unsigned char *, int *);
static void print_class_decls (FILE *, JCF *, int);
static void error (const char *gmsgid, ...);
static void usage (void) ATTRIBUTE_NORETURN;
static void help (void) ATTRIBUTE_NORETURN;
static void version (void) ATTRIBUTE_NORETURN;
static int overloaded_jni_method_exists_p (const unsigned char *, int,
					   const char *, int);
static void jni_print_char (FILE *, int);
static void jni_print_float (FILE *, jfloat);
static void jni_print_double (FILE *, jdouble);
static void decompile_return_statement (FILE *, JCF *, int, int, int);

static void handle_inner_classes (int);

JCF_u2 current_field_name;
JCF_u2 current_field_value;
JCF_u2 current_field_signature;
JCF_u2 current_field_flags;

#define HANDLE_START_FIELD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
( current_field_name = (NAME), current_field_signature = (SIGNATURE), \
  current_field_flags = (ACCESS_FLAGS), current_field_value = 0)

/* We pass over fields twice.  The first time we just note the types
   of the fields and then the start of the methods.  Then we go back
   and parse the fields for real.  This is ugly.  */
static int field_pass;
/* Likewise we pass over methods twice.  The first time we generate
   class decl information; the second time we generate actual method
   decls.  */
static int method_pass;

#define HANDLE_END_FIELD()						      \
  if (field_pass)							      \
    {									      \
      if (out && ! stubs)						      \
	print_field_info (out, jcf, current_field_name,			      \
			  current_field_signature,			      \
 			  current_field_flags);				      \
    }									      \
  else if (! stubs && ! flag_jni)					      \
    add_class_decl (out, jcf, current_field_signature);

#define HANDLE_CONSTANTVALUE(VALUEINDEX) current_field_value = (VALUEINDEX)

static int method_declared = 0;
static int method_access = 0;
static int method_printed = 0;
static int method_synthetic = 0;
static int method_signature = 0;

/* Set to 1 while the very first data member of a class is being handled.  */
static int is_first_data_member = 0;

#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT)	\
  {									\
    method_synthetic = 0;						\
    method_printed = 0;							\
    decompiled = 0;							\
    method_signature = SIGNATURE;					\
    if (ATTRIBUTE_COUNT)						\
      method_synthetic = peek_attribute (jcf, ATTRIBUTE_COUNT,		\
				  (const char *)"Synthetic", 9);	\
    /* If a synthetic methods have been declared, its attribute aren't	\
       worth reading (and triggering side-effects). We skip them an	\
       set ATTRIBUTE_COUNT to zero so that they'll be skipped in	\
       jcf_parse_one_method.  */					\
    if (method_synthetic)						\
      {									\
	skip_attribute (jcf, ATTRIBUTE_COUNT);				\
	ATTRIBUTE_COUNT = 0;						\
      } 								\
    if (method_pass && !method_synthetic)				\
      {									\
	if (out)							\
	  print_method_info (out, jcf, NAME, SIGNATURE,			\
			     ACCESS_FLAGS);				\
      }									\
    else if (!method_synthetic)						\
      {									\
	print_method_info (NULL, jcf, NAME, SIGNATURE,			\
			   ACCESS_FLAGS);				\
	if (! stubs && ! flag_jni)					\
	  add_class_decl (out, jcf, SIGNATURE);				\
      }									\
  }

/* Only include byte-code decompilation optimizations for ELF targets
   since the generated headers are only known to work with ELF weak
   symbol semnatics. Specifically, these optimizations are known to
   not work on PE-COFF and possibly others.  */
#ifdef OBJECT_FORMAT_ELF
#define HANDLE_CODE_ATTRIBUTE(MAX_STACK, MAX_LOCALS, CODE_LENGTH)	\
  if (out && method_declared) decompile_method (out, jcf, CODE_LENGTH);
#endif

static int decompiled = 0;
#define HANDLE_END_METHOD()				\
  if (out && method_printed && !method_synthetic) 	\
    fputs (decompiled || stubs ? "\n" : ";\n", out);

#define HANDLE_INNERCLASSES_ATTRIBUTE(COUNT) handle_inner_classes (COUNT)

/* We're going to need {peek,skip}_attribute, enable their definition.   */
#define NEED_PEEK_ATTRIBUTE
#define NEED_SKIP_ATTRIBUTE

#include "jcf-reader.c"

/* Print an error message and set found_error.
   Not really gcc-internal-format message, but as error elsewhere
   uses it, assume all users will use intersection between
   c-format and gcc-internal-format.  */
static void
error (const char *gmsgid, ...)
{
  va_list ap;

  va_start (ap, gmsgid);

  fprintf (stderr, TOOLNAME ": ");
  vfprintf (stderr, _(gmsgid), ap);
  va_end (ap);
  fprintf (stderr, "\n");
  found_error = 1;
}

/* Print a single-precision float, suitable for parsing by g++.  */
static void
jni_print_float (FILE *stream, jfloat f)
{
  /* It'd be nice to use __builtin_nan/__builtin_inf here but they don't
     work in data initializers.  FIXME.  */
  if (JFLOAT_FINITE (f))
    {
      if (flag_jni)
        {
          fputs (" ", out);
          if (f.negative)
            putc ('-', stream);
          if (f.exponent)
            fprintf (stream, "0x1.%.6xp%+df",
                     ((unsigned int)f.mantissa) << 1,
                     f.exponent - JFLOAT_EXP_BIAS);
          else
            /* Exponent of 0x01 is -125; exponent of 0x00 is *also* -125,
               because the implicit leading 1 bit is no longer present.  */
            fprintf (stream, "0x0.%.6xp%+df",
                     ((unsigned int)f.mantissa) << 1,
                     f.exponent + 1 - JFLOAT_EXP_BIAS);
        }
    }
  if (! flag_jni)
    fputs (";\n", stream);
}

/* Print a double-precision float, suitable for parsing by g++.  */
static void
jni_print_double (FILE *stream, jdouble f)
{
  /* It'd be nice to use __builtin_nan/__builtin_inf here but they don't
     work in data initializers.  FIXME.  */
  if (JDOUBLE_FINITE (f))
    {
      if (flag_jni)
        {
          fputs (" ", out);
          if (f.negative)
            putc ('-', stream);
          if (f.exponent)
            fprintf (stream, "0x1.%.5x%.8xp%+d",
                     f.mantissa0, f.mantissa1,
                     f.exponent - JDOUBLE_EXP_BIAS);
          else
            /* Exponent of 0x001 is -1022; exponent of 0x000 is *also* -1022,
               because the implicit leading 1 bit is no longer present.  */
            fprintf (stream, "0x0.%.5x%.8xp%+d",
                     f.mantissa0, f.mantissa1,
                     f.exponent + 1 - JDOUBLE_EXP_BIAS);
        }
    }
  fputs (flag_jni ? "\n" : ";\n", stream);
}

/* Print a character, appropriately mangled for JNI.  */

static void
jni_print_char (FILE *stream, int ch)
{
  if (! flag_jni)
    jcf_print_char (stream, ch);
  else if (ch == '(' || ch == ')')
    {
      /* Ignore.  */
    }
  else if (ch == '_')
    fputs ("_1", stream);
  else if (ch == ';')
    fputs ("_2", stream);
  else if (ch == '[')
    fputs ("_3", stream);
  else if (ch == '/')
    fputs ("_", stream);
  else if (ISALNUM (ch))
    fputc (ch, stream);
  else
    {
      /* "Unicode" character.  */
      fprintf (stream, "_0%04x", ch);
    }
}

/* Print a name from the class data.  If the index does not point to a
   string, an error results.  */

static void
print_name (FILE* stream, JCF* jcf, int name_index)
{
  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
    }
  else if (! flag_jni)
    jcf_print_utf8 (stream, JPOOL_UTF_DATA (jcf, name_index),
		    JPOOL_UTF_LENGTH (jcf, name_index));
  else
    {
      /* For JNI we must correctly quote each character.  */
      const unsigned char *str = JPOOL_UTF_DATA (jcf, name_index);
      int length = JPOOL_UTF_LENGTH (jcf, name_index);
      const unsigned char *limit = str + length;
      while (str < limit)
	{
	  int ch = UTF8_GET (str, limit);
	  if (ch < 0)
	    {
	      fprintf (stream, "\\<invalid>");
	      return;
	    }
	  jni_print_char (stream, ch);
	}
    }
}

/* Print base name of class.  The base name is everything after the
   final separator.  */

static void
print_base_classname (FILE *stream, JCF *jcf, int index)
{
  int name_index = JPOOL_USHORT1 (jcf, index);
  int len;
  const unsigned char *s, *p, *limit;

  s = JPOOL_UTF_DATA (jcf, name_index);
  len = JPOOL_UTF_LENGTH (jcf, name_index);
  limit = s + len;
  p = s;
  while (s < limit)
    {
      int c = UTF8_GET (s, limit);
      if (c == '/')
	p = s;
    }

  while (p < limit)
    {
      int ch = UTF8_GET (p, limit);
      if (ch == '/')
	fputs ("::", stream);
      else
	jcf_print_char (stream, ch);
    }
}

/* Return 0 if NAME is equal to STR, -1 if STR is "less" than NAME,
   and 1 if STR is "greater" than NAME.  */

static int
utf8_cmp (const unsigned char *str, int length, const char *name)
{
  const unsigned char *limit = str + length;
  int i;

  for (i = 0; name[i]; ++i)
    {
      int ch = UTF8_GET (str, limit);
      if (ch != name[i])
	return ch - name[i];
    }

  return str == limit ? 0 : 1;
}

/* This is a sorted list of all C++ keywords.  */

static const char *const cxx_keywords[] =
{
  "_Complex",
  "__alignof",
  "__alignof__",
  "__asm",
  "__asm__",
  "__attribute",
  "__attribute__",
  "__builtin_va_arg",
  "__complex",
  "__complex__",
  "__const",
  "__const__",
  "__extension__",
  "__imag",
  "__imag__",
  "__inline",
  "__inline__",
  "__label__",
  "__null",
  "__real",
  "__real__",
  "__restrict",
  "__restrict__",
  "__signed",
  "__signed__",
  "__typeof",
  "__typeof__",
  "__volatile",
  "__volatile__",
  "and",
  "and_eq",
  "asm",
  "auto",
  "bitand",
  "bitor",
  "bool",
  "break",
  "case",
  "catch",
  "char",
  "class",
  "compl",
  "const",
  "const_cast",
  "continue",
  "default",
  "delete",
  "do",
  "double",
  "dynamic_cast",
  "else",
  "enum",
  "explicit",
  "export",
  "extern",
  "false",
  "float",
  "for",
  "friend",
  "goto",
  "if",
  "inline",
  "int",
  "long",
  "mutable",
  "namespace",
  "new",
  "not",
  "not_eq",
  "operator",
  "or",
  "or_eq",
  "private",
  "protected",
  "public",
  "register",
  "reinterpret_cast",
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "static_cast",
  "struct",
  "switch",
  "template",
  "this",      
  "throw",
  "true",
  "try",
  "typedef",
  "typeid",
  "typename",
  "typeof",
  "union",
  "unsigned",
  "using",
  "virtual",
  "void",
  "volatile",
  "wchar_t",
  "while",
  "xor",
  "xor_eq"
};


/* If NAME is the name of a C++ keyword, then return an override name.
   This is a name that can be used in place of the keyword.
   Otherwise, return NULL.  The return value is malloc()d.  */

static char *
cxx_keyword_subst (const unsigned char *str, int length)
{
  int last = ARRAY_SIZE (cxx_keywords);
  int first = 0;
  int mid = (last + first) / 2;
  int old = -1;

  for (mid = (last + first) / 2;
       mid != old;
       old = mid, mid = (last + first) / 2)
    {
      int kwl = strlen (cxx_keywords[mid]);
      int min_length = kwl > length ? length : kwl;
      int r = utf8_cmp (str, min_length, cxx_keywords[mid]);

      if (r == 0)
	{
	  int i;

	  /* Skip all trailing `$'.  */
	  for (i = min_length; i < length && str[i] == '$'; ++i)
	    ;
	  /* We've only found a match if all the remaining characters
	     are `$'.  */
	  if (i == length)
	    {
	      char *dup = xmalloc (2 + length - min_length + kwl);
	      strcpy (dup, cxx_keywords[mid]);
	      for (i = kwl; i < length + 1; ++i)
		dup[i] = '$';
	      dup[i] = '\0';
	      return dup;
	    }
	  r = 1;
	}
	
      if (r < 0)
	last = mid;
      else
	first = mid;
    }
  return NULL;
}

/* Generate an access control keyword based on FLAGS.  */

static void
generate_access (FILE *stream, JCF_u2 flags)
{
  if ((flags & ACC_VISIBILITY) == last_access)
    return;
  last_access = (flags & ACC_VISIBILITY);

  switch (last_access)
    {
    case 0:
      fputs ("public: // actually package-private\n", stream);
      break;
    case ACC_PUBLIC:
      fputs ("public:\n", stream);
      break;
    case ACC_PRIVATE:
      fputs ("private:\n", stream);
      break;
    case ACC_PROTECTED:
      fputs ("public:  // actually protected\n", stream);
      break;
    default:
      found_error = 1;
      fprintf (stream, "#error unrecognized visibility %d\n",
	       (flags & ACC_VISIBILITY));
      break;
    }
}

/* See if NAME is already the name of a method.  */
static int
name_is_method_p (const unsigned char *name, int length)
{
  struct method_name *p;

  for (p = method_name_list; p != NULL; p = p->next)
    {
      if (p->length == length && ! memcmp (p->name, name, length))
	return 1;
    }
  return 0;
}

/* Free the method name list.  */
static void
free_method_name_list (void)
{
  struct method_name *p = method_name_list;
  while (p != NULL)
    {
      struct method_name *next = p->next;
      free (p->name);
      free (p->signature);
      free (p);
      p = next;
    }
  method_name_list = NULL;
}

/* If there is already a native method named NAME, whose signature is not
   SIGNATURE, then return true.  Otherwise return false.  */
static int
overloaded_jni_method_exists_p (const unsigned char *name, int length,
				const char *signature, int sig_length)
{
  struct method_name *p;

  for (p = method_name_list; p != NULL; p = p->next)
    {
      if (p->is_native
          && p->length == length
	  && ! memcmp (p->name, name, length)
	  && (p->sig_length != sig_length
	      || memcmp (p->signature, signature, sig_length)))
	return 1;
    }
  return 0;
}

/* Get name of a field.  This handles renamings due to C++ clash.  */
static char *
get_field_name (JCF *jcf, int name_index, JCF_u2 flags)
{
  unsigned char *name = JPOOL_UTF_DATA (jcf, name_index);
  int length = JPOOL_UTF_LENGTH (jcf, name_index);
  char *override;

  if (name_is_method_p (name, length))
    {
      /* This field name matches a method.  So override the name with
	 a dummy name.  This is yucky, but it isn't clear what else to
	 do.  FIXME: if the field is static, then we'll be in real
	 trouble.  */
      if ((flags & ACC_STATIC))
	{
	  error ("static field has same name as method");
	  return NULL;
	}

      override = xmalloc (length + 3);
      memcpy (override, name, length);
      strcpy (override + length, "__");
    }
  else if (flag_jni)
    override = NULL;
  else
    override = cxx_keyword_subst (name, length);

  return override;
}

/* Print a field name.  Convenience function for use with
   get_field_name.  */
static void
print_field_name (FILE *stream, JCF *jcf, int name_index, JCF_u2 flags)
{
  char *override = get_field_name (jcf, name_index, flags);

  if (override)
    {
      fputs (override, stream);
      free (override);
    }
  else
    jcf_print_utf8 (stream, JPOOL_UTF_DATA (jcf, name_index),
		    JPOOL_UTF_LENGTH (jcf, name_index));
}

static void
print_field_info (FILE *stream, JCF* jcf, int name_index, int sig_index,
		  JCF_u2 flags)
{
  char *override = NULL;

  if (! flag_jni)
    generate_access (stream, flags);
  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
      return;
    }

  if (flag_jni)
    {
      /* For JNI we only want to print real constants.  */
      int val;
      if (! (flags & ACC_STATIC)
	  || ! (flags & ACC_FINAL)
	  || current_field_value <= 0)
	return;
      val = JPOOL_TAG (jcf, current_field_value);
      if (val != CONSTANT_Integer && val != CONSTANT_Long
	  && val != CONSTANT_Float && val != CONSTANT_Double)
	return;
    }
  else
    {
      /* Initial indentation.  */
      fputs ("  ", stream);
    }

  if ((flags & ACC_STATIC))
    {
      if (flag_jni)
	{
	  print_cxx_classname (stream, "#undef ", jcf, jcf->this_class, 1);
	  fputs ("_", stream);
	  print_field_name (stream, jcf, name_index, 0);
	  fputs ("\n", stream);
	  print_cxx_classname (stream, "#define ", jcf, jcf->this_class, 1);
	  fputs ("_", stream);
	}
      else
	fputs ("static ", stream);

      if ((flags & ACC_FINAL) && current_field_value > 0)
	{
	  char buffer[25];
	  int done = 1;

	  switch (JPOOL_TAG (jcf, current_field_value))
	    {
	    case CONSTANT_Integer:
	      {
		jint num;
		int most_negative = 0;
		if (! flag_jni)
		  fputs ("const jint ", stream);
		print_field_name (stream, jcf, name_index, 0);
		fputs (flag_jni ? " " : " = ", stream);
		num = JPOOL_INT (jcf, current_field_value);
		/* We single out the most negative number to print
		   specially.  This avoids later warnings from g++.  */
		if (num == (jint) 0x80000000)
		  {
		    most_negative = 1;
		    ++num;
		  }
		format_int (buffer, (jlong) num, 10);
		fprintf (stream, "%sL%s%s\n", buffer,
			 most_negative ? " - 1" : "",
			 flag_jni ? "" : ";");
	      }
	      break;
	    case CONSTANT_Long:
	      {
		jlong num;
		int most_negative = 0;
		if (! flag_jni)
		  fputs ("const jlong ", stream);
		print_field_name (stream, jcf, name_index, 0);
		fputs (flag_jni ? " " : " = ", stream);
		num = JPOOL_LONG (jcf, current_field_value);
		/* We single out the most negative number to print
                   specially..  This avoids later warnings from g++.  */
		if (num == (jlong) 0x8000000000000000LL)
		  {
		    most_negative = 1;
		    ++num;
		  }
		format_int (buffer, num, 10);
		fprintf (stream, "%sLL%s%s\n", buffer,
			 most_negative ? " - 1" :"",
			 flag_jni ? "" : ";");
	      }
	      break;
	    case CONSTANT_Float:
	      {
		jfloat fnum = JPOOL_FLOAT (jcf, current_field_value);
		if (! flag_jni)
		  fputs ("const jfloat ", stream);
		print_field_name (stream, jcf, name_index, 0);
		jni_print_float (stream, fnum);
	      }
	      break;
	    case CONSTANT_Double:
	      {
		jdouble dnum = JPOOL_DOUBLE (jcf, current_field_value);
		if (! flag_jni)
		  fputs ("const jdouble ", stream);
		print_field_name (stream, jcf, name_index, 0);
		jni_print_double (stream, dnum);
	      }
	      break;
	    default:
 	      /* We can't print this as a constant, but we can still
 		 print something sensible.  */
 	      done = 0;
 	      break;
	    }

	  if (done)
	    return;
	}
    }

  /* assert (! flag_jni);  */
  override = get_field_name (jcf, name_index, flags);
  print_c_decl (stream, jcf, name_index, sig_index, 0, override, flags);
  fputs (";\n", stream);

  if (override)
    free (override);
}


static void
print_method_info (FILE *stream, JCF* jcf, int name_index, int sig_index,
		   JCF_u2 flags)
{
  const unsigned char *str;
  int length, is_init = 0;
  char *override = NULL;

  method_declared = 0;
  method_access = flags;
  if (stream && JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    fprintf (stream, "<not a UTF8 constant>");
  str = JPOOL_UTF_DATA (jcf, name_index);
  length = JPOOL_UTF_LENGTH (jcf, name_index);

  if (str[0] == '<')
    {
      /* Ignore the internally generated method <clinit>. However,
         treat <init> as a constructor.  */
      if (! utf8_cmp (str, length, "<init>"))
	is_init = 1;
      else if (! METHOD_IS_FINAL (jcf->access_flags, flags)
	       && ! (flags & ACC_STATIC))
	{
	  /* FIXME: i18n bug here.  Order of prints should not be
	     fixed.  */
	  fprintf (stderr, _("ignored method '"));
	  jcf_print_utf8 (stderr, str, length);
	  fprintf (stderr, _("' marked virtual\n"));
	  found_error = 1;
	  return;
	}
      else
	return;
    }

  /* During the first method pass, build a list of method names. This will
  be used to determine if field names conflict with method names. */
  if (! stream)
    {
      struct method_name *nn;

      nn = xmalloc (sizeof (struct method_name));
      nn->name = xmalloc (length);
      memcpy (nn->name, str, length);
      nn->length = length;
      nn->next = method_name_list;
      nn->sig_length = JPOOL_UTF_LENGTH (jcf, sig_index);
      nn->signature = xmalloc (nn->sig_length);
      nn->is_native = METHOD_IS_NATIVE (flags);
      memcpy (nn->signature, JPOOL_UTF_DATA (jcf, sig_index),
	      nn->sig_length);
      method_name_list = nn;
      
      /* The rest of this function doesn't matter. */
      return;
    }

  /* We don't worry about overrides in JNI mode.  */
  if (! flag_jni)
    {
      /* We can't generate a method whose name is a C++ reserved word.
	 We can't just ignore the function, because that will cause
	 incorrect code to be generated if the function is virtual
	 (not only for calls to this function for for other functions
	 after it in the vtbl).  So we give it a dummy name instead.  */
      override = cxx_keyword_subst (str, length);
    }

  if (! stubs && ! flag_jni)
    {
      method_printed = 1;

      generate_access (stream, flags);
      
      fputs ("  ", out);
      if ((flags & ACC_STATIC))
	fputs ("static ", out);
      else if (! METHOD_IS_PRIVATE (jcf->access_flags, flags))
	{
	  /* Don't print `virtual' if we have a constructor.  */
	  if (! is_init)
	    fputs ("virtual ", out);
	}
      print_c_decl (out, jcf, name_index, sig_index, is_init, override, flags);
      
      if ((flags & ACC_ABSTRACT))
	fputs (" = 0", out);
      else
	method_declared = 1;
    }
  else
    {
      if (METHOD_IS_NATIVE (flags)) 
	{
	  method_printed = 1;
	  print_stub_or_jni (out, jcf, name_index, sig_index,
			     is_init, override, flags);
	}
    }

  if (override)
    free (override);
}

/* A helper for the decompiler which prints a `return' statement where
   the type is a reference type.  If METHODTYPE and OBJECTTYPE are not
   identical, we emit a cast.  We do this because the C++ compiler
   doesn't know that a reference can be cast to the type of an
   interface it implements.  METHODTYPE is the index of the method's
   signature.  NAMEINDEX is the index of the field name; -1 for
   `this'.  OBJECTTYPE is the index of the object's type.  */
static void
decompile_return_statement (FILE *out, JCF *jcf, int methodtype,
			    int nameindex, int objecttype)
{
  int cast = 0;
  int obj_name_len, method_name_len;
  const unsigned char *obj_data, *method_data;

  obj_name_len = JPOOL_UTF_LENGTH (jcf, objecttype);
  obj_data = JPOOL_UTF_DATA (jcf, objecttype);

  method_name_len = JPOOL_UTF_LENGTH (jcf, methodtype);
  method_data = JPOOL_UTF_DATA (jcf, methodtype);

  /* Skip forward to return type part of method.  */
  while (*method_data != ')')
    {
      ++method_data;
      --method_name_len;
    }
  /* Skip past `)'.  */
  ++method_data;
  --method_name_len;

  /* If we see an `L', skip it and the trailing `;'.  */
  if (method_data[0] == 'L' && method_data[method_name_len - 1] == ';')
    {
      ++method_data;
      method_name_len -= 2;
    }
  if (obj_data[0] == 'L' && obj_data[obj_name_len - 1] == ';')
    {
      ++obj_data;
      obj_name_len -= 2;
    }

  /* FIXME: if METHODTYPE is a superclass of OBJECTTYPE then we don't
     need a cast.  Right now there is no way to determine if this is
     the case.  */
  if (method_name_len != obj_name_len)
    cast = 1;
  else
    {
      int i;
      for (i = 0; i < method_name_len; ++i)
	{
	  if (method_data[i] != obj_data[i])
	    {
	      cast = 1;
	      break;
	    }
	}
    }

  fputs (" { return ", out);

  if (cast)
    {
      int array_depth = 0;
      const unsigned char *limit;

      fputs ("reinterpret_cast<", out);

      while (*method_data == '[')
	{
	  ++method_data;
	  ++array_depth;
	  --method_name_len;
	  fputs ("JArray<", out);
	}

      /* Leading space to avoid C++ digraphs.  */
      fputs (" ::", out);

      /* If we see an `L', skip it and the trailing `;'.  Only do this
	 if we've seen an array specification.  If we don't have an
	 array then the `L' was stripped earlier.  */
      if (array_depth && method_data[0] == 'L'
	  && method_data[method_name_len - 1] == ';')
	{
	  ++method_data;
	  method_name_len -= 2;
	}

      limit = method_data + method_name_len;
      while (method_data < limit)
	{
	  int ch = UTF8_GET (method_data, limit);
	  if (ch == '/')
	    fputs ("::", out);
	  else
	    jcf_print_char (out, ch);
	}
      fputs (" *", out);

      /* Close each array.  */
      while (array_depth > 0)
	{
	  fputs ("> *", out);
	  --array_depth;
	}

      /* Close the cast.  */
      fputs ("> (", out);
    }

  if (nameindex == -1)
    fputs ("this", out);
  else
    print_field_name (out, jcf, nameindex, 0);

  if (cast)
    fputs (")", out);

  fputs ("; }", out);
}


/* Try to decompile a method body.  Right now we just try to handle a
   simple case that we can do.  Expand as desired.  */
static void
decompile_method (FILE *out, JCF *jcf, int code_len)
{
  const unsigned char *codes = jcf->read_ptr;
  int index;
  uint16 name_and_type, name;

  /* If the method is synchronized, don't touch it.  */
  if ((method_access & ACC_SYNCHRONIZED))
    return;

  if (code_len == 5
      && codes[0] == OPCODE_aload_0
      && codes[1] == OPCODE_getfield
      && (codes[4] == OPCODE_areturn
	  || codes[4] == OPCODE_dreturn
	  || codes[4] == OPCODE_freturn
	  || codes[4] == OPCODE_ireturn
	  || codes[4] == OPCODE_lreturn))
    {
      /* Found code like `return FIELD'.  */
      index = (codes[2] << 8) | codes[3];
      /* FIXME: ensure that tag is CONSTANT_Fieldref.  */
      name_and_type = JPOOL_USHORT2 (jcf, index);
      /* FIXME: ensure that tag is CONSTANT_NameAndType.  */
      name = JPOOL_USHORT1 (jcf, name_and_type);
      if (codes[4] == OPCODE_areturn)
	decompile_return_statement (out, jcf, method_signature,
				    name, JPOOL_USHORT2 (jcf, name_and_type));
      else
	{
	  fputs (" { return ", out);
	  /* FIXME: flags.  */
	  print_field_name (out, jcf, name, 0);
	  fputs ("; }", out);
	}
      decompiled = 1;
    }
  else if (code_len == 2
	   && codes[0] == OPCODE_aload_0
	   && codes[1] == OPCODE_areturn
	   /* We're going to generate `return this'.  This only makes
	      sense for non-static methods.  */
	   && ! (method_access & ACC_STATIC))
    {
      decompile_return_statement (out, jcf, method_signature, -1,
				  JPOOL_USHORT1 (jcf, jcf->this_class));
      decompiled = 1;
    }
  else if (code_len == 1 && codes[0] == OPCODE_return)
    {
      /* Found plain `return'.  */
      fputs (" { }", out);
      decompiled = 1;
    }
  else if (code_len == 2
	   && codes[0] == OPCODE_aconst_null
	   && codes[1] == OPCODE_areturn)
    {
      /* Found `return null'.  We don't want to depend on NULL being
	 defined.  */
      fputs (" { return 0; }", out);
      decompiled = 1;
    }
}

/* Like strcmp, but invert the return result for the hash table.  This
   should probably be in hashtab.c to complement the existing string
   hash function.  */
static int
gcjh_streq (const void *p1, const void *p2)
{
  return ! strcmp ((char *) p1, (char *) p2);
}

/* Return 1 if the initial part of CLNAME names a subclass of throwable, 
   or 0 if not.  CLNAME may be extracted from a signature, and can be 
   terminated with either `;' or NULL.  */
static int
throwable_p (const unsigned char *clname)
{
  int length;
  unsigned char *current;
  int i;
  int result = 0;

  /* We keep two hash tables of class names.  In one we list all the
     classes which are subclasses of Throwable.  In the other we will
     all other classes.  We keep two tables to make the code a bit
     simpler; we don't have to have a structure mapping class name to
     a `throwable?' bit.  */
  static htab_t throw_hash;
  static htab_t non_throw_hash;
  static int init_done = 0;

  if (! init_done)
    {
      void **slot;
      unsigned char *str;

      /* Self-initializing.  The cost of this really doesn't matter.
	 We also don't care about freeing these, either.  */
      throw_hash = htab_create (10, htab_hash_string, gcjh_streq,
				(htab_del) free);
      non_throw_hash = htab_create (10, htab_hash_string, gcjh_streq,
				    (htab_del) free);

      /* Make sure the root classes show up in the tables.  */
      str = (unsigned char *) xstrdup ("java.lang.Throwable");
      slot = htab_find_slot (throw_hash, str, INSERT);
      *slot = str;

      str = (unsigned char *) xstrdup ("java.lang.Object");
      slot = htab_find_slot (non_throw_hash, str, INSERT);
      *slot = str;

      init_done = 1;
    }

  for (length = 0; clname[length] != ';' && clname[length] != '\0'; ++length)
    ;
  current = ALLOC (length + 1);
  for (i = 0; i < length; ++i)
    current[i] = clname[i] == '/' ? '.' : clname[i];
  current[length] = '\0';

  /* We don't compute the hash slot here because the table might be
     modified by the recursion.  In that case the slot could be
     invalidated.  */
  if (htab_find (throw_hash, current))
    result = 1;
  else if (htab_find (non_throw_hash, current))
    result = 0;
  else
    {
      JCF jcf;
      void **slot;
      unsigned char *super, *tmp;
      int super_length = -1;
      const char *classfile_name = find_class ((char *) current, strlen ((const char *) current),
					       &jcf, 0);

      if (! classfile_name)
	{
	  error ("couldn't find class %s", current);
	  return 0;
	}
      if (jcf_parse_preamble (&jcf) != 0
	  || jcf_parse_constant_pool (&jcf) != 0
	  || verify_constant_pool (&jcf) > 0)
	{
	  error ("parse error while reading %s", classfile_name);
	  return 0;
	}
      jcf_parse_class (&jcf);

      tmp = (unsigned char *) super_class_name (&jcf, &super_length);
      super = ALLOC (super_length + 1);
      memcpy (super, tmp, super_length);      
      super[super_length] = '\0';

      result = throwable_p (super);
      slot = htab_find_slot (result ? throw_hash : non_throw_hash,
			     current, INSERT);
      *slot = current;
      current = NULL;

      JCF_FINISH (&jcf);
    }

  return result;
}

/* Print one piece of a signature.  Returns pointer to next parseable
   character on success, NULL on error.  */
static const unsigned char *
decode_signature_piece (FILE *stream, const unsigned char *signature,
			const unsigned char *limit, int *need_space)
{
  const char *ctype;
  int array_depth = 0;

  switch (signature[0])
    {
    case '[':
      /* More spaghetti.  */

    array_loop:
      for (signature++; (signature < limit
			 && ISDIGIT (*signature)); signature++)
	;
      switch (*signature)
	{
	case 'B':
	  ctype = "jbyteArray";
	  break;
	case 'C':
	  ctype = "jcharArray";
	  break;
	case 'D':
	  ctype = "jdoubleArray";
	  break;
	case 'F':
	  ctype = "jfloatArray";
	  break;
	case 'I':
	  ctype = "jintArray";
	  break;
	case 'S':
	  ctype = "jshortArray";
	  break;
	case 'J':
	  ctype = "jlongArray";
	  break;
	case 'Z':
	  ctype = "jbooleanArray";
	  break;
	case '[':
	  /* We have a nested array.  */
	  ++array_depth;
	  if (! flag_jni)
	    fputs ("JArray<", stream);
	  goto array_loop;

	case 'L':
	  /* We have to generate a reference to JArray here, so that
	     our output matches what the compiler does.  */
	  ++signature;
	  /* Space between `<' and `:' to avoid C++ digraphs.  */
	  if (! flag_jni)
	    fputs ("JArray< ::", stream);
	  while (signature < limit && *signature != ';')
	    {
	      int ch = UTF8_GET (signature, limit);
	      if (! flag_jni)
		{
		  if (ch == '/')
		    fputs ("::", stream);
		  else
		    jcf_print_char (stream, ch);
		}
	    }
	  if (! flag_jni)
	    fputs (" *> *", stream);
	  *need_space = 0;
	  ctype = NULL;
	  break;
	default:
	  /* Unparseable signature.  */
	  return NULL;
	}

      /* If the previous iterations left us with something to print,
	 print it.  For JNI, we always print `jobjectArray' in the
	 nested cases.  */
      if (flag_jni && (ctype == NULL || array_depth > 0))
	{
	  ctype = "jobjectArray";
	  *need_space = 1;
	}
      /* The `printit' case will advance SIGNATURE for us.  If we
	 don't go there, we must advance past the `;' ourselves.  */
      if (ctype != NULL)
	goto printit;
      ++signature;
      break;

    case '(':
    case ')':
      /* This shouldn't happen.  */
      return NULL;

    case 'B': ctype = "jbyte";  goto printit;
    case 'C': ctype = "jchar";  goto printit;
    case 'D': ctype = "jdouble";  goto printit;
    case 'F': ctype = "jfloat";  goto printit;
    case 'I': ctype = "jint";  goto printit;
    case 'J': ctype = "jlong";  goto printit;
    case 'S': ctype = "jshort";  goto printit;
    case 'Z': ctype = "jboolean";  goto printit;
    case 'V': ctype = "void";  goto printit;
    case 'L':
      if (flag_jni)
	{
	  /* We know about certain types and special-case their names.  */
	  if (! strncmp ((const char *) signature, "Ljava/lang/String;",
			 sizeof ("Ljava/lang/String;") -1))
	    ctype = "jstring";
	  else if (! strncmp ((const char *) signature, "Ljava/lang/Class;",
			      sizeof ("Ljava/lang/Class;") - 1))
	    ctype = "jclass";
	  /* Skip leading 'L' for throwable_p call.  */
	  else if (throwable_p (signature + 1))
	    ctype = "jthrowable";
	  else
	    ctype = "jobject";

	  while (*signature && *signature != ';')
	    ++signature;

	  goto printit;
	}
      /* Print a leading "::" so we look in the right namespace.  */
      fputs ("::", stream);
      ++signature;
      while (*signature && *signature != ';')
	{
	  int ch = UTF8_GET (signature, limit);
	  if (ch == '/')
	    fputs ("::", stream);
	  else
	    jcf_print_char (stream, ch);
	}
      fputs (" *", stream);
      if (*signature == ';')
	signature++;
      *need_space = 0;
      break;
    default:
      *need_space = 1;
      jni_print_char (stream, *signature++);
      break;
    printit:
      signature++;
      *need_space = 1;
      fputs (ctype, stream);
      break;
    }

  if (! flag_jni)
    {
      while (array_depth-- > 0)
	fputs ("> *", stream);
    }

  return signature;
}

static void
print_c_decl (FILE* stream, JCF* jcf, int name_index, int signature_index,
	      int is_init, const char *name_override, int flags)
{
  if (JPOOL_TAG (jcf, signature_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
    }
  else
    {
      int length = JPOOL_UTF_LENGTH (jcf, signature_index);
      const unsigned char *str0 = JPOOL_UTF_DATA (jcf, signature_index);
      const unsigned char *str = str0;
      const unsigned char *limit = str + length;
      int need_space = 0;
      int is_method = str[0] == '(';
      const unsigned char *next;

      /* If printing a method, skip to the return signature and print
	 that first.  However, there is no return value if this is a
	 constructor.  */
      if (is_method && ! is_init)
	{
	  while (str < limit)
	    {
	      int ch = *str++;
	      if (ch == ')')
		break;
	    }
	}

      /* If printing a field or an ordinary method, then print the
	 "return value" now.  */
      if (! is_method || ! is_init)
	{
	  next = decode_signature_piece (stream, str, limit, &need_space);
	  if (! next)
	    {
	      error ("unparseable signature: '%s'", str0);
	      return;
	    }
	}

      /* Force the alignment of the first data member.  This is
	 because the "new" C++ ABI changed the alignment of non-POD
	 classes.  gcj, however, still uses the "old" alignment.  */
      if (is_first_data_member && ! (flags & ACC_STATIC) && ! is_method)
	{
	  is_first_data_member = 0;
	  print_cxx_classname (out, " __attribute__((aligned(__alignof__( ",
			       jcf, jcf->super_class, 1);
	  fputs (" )))) ", stream);
	}

      /* Now print the name of the thing.  */
      if (need_space)
	fputs (" ", stream);
      print_full_cxx_name (stream, jcf, name_index, 
			   signature_index, is_init, name_override,
			   flags);
    }
}

/* Print the unqualified method name followed by the signature. */
static void
print_full_cxx_name (FILE* stream, JCF* jcf, int name_index,
		     int signature_index, int is_init,
		     const char *name_override, int flags)
{
  int length = JPOOL_UTF_LENGTH (jcf, signature_index);
  const unsigned char *str0 = JPOOL_UTF_DATA (jcf, signature_index);
  const unsigned char *str = str0;
  const unsigned char *limit = str + length;
  int need_space = 0;
  int is_method = str[0] == '(';
  const unsigned char *next;

  if (name_override)
    fputs (name_override, stream);
  else if (name_index)
    {
      /* Declare constructors specially.  */
      if (is_init)
	print_base_classname (stream, jcf, jcf->this_class);
      else
	print_name (stream, jcf, name_index);
    }

  if (flag_jni)
    {
      unsigned char *signature = JPOOL_UTF_DATA (jcf, signature_index);
      int sig_len = JPOOL_UTF_LENGTH (jcf, signature_index);
      if (overloaded_jni_method_exists_p (JPOOL_UTF_DATA (jcf, name_index),
					  JPOOL_UTF_LENGTH (jcf, name_index),
					  (const char *) signature, sig_len))
	{
	  /* If this method is overloaded by another native method,
	     then include the argument information in the mangled
	     name.  */
	  unsigned char *limit = signature + sig_len;
	  fputs ("__", stream);
	  while (signature < limit)
	    {
	      int ch = UTF8_GET (signature, limit);
	      jni_print_char (stream, ch);
	      if (ch == ')')
		{
		  /* Done.  */
		  break;
		}
	    }
	}
    }

  if (is_method)
    {
      /* Have a method or a constructor.  Print signature pieces
	 until done.  */
      fputs (" (", stream);

      str = str0 + 1;

      /* In JNI mode, add extra arguments.  */
      if (flag_jni)
	{
	  /* FIXME: it would be nice to know if we are printing a decl
	     or a definition, and only print `env' for the latter.  */
	  fputs ("JNIEnv *env", stream);

	  fputs ((flags & ACC_STATIC) ? ", jclass" : ", jobject", stream);

	  if (*str != ')')
	    fputs (", ", stream);
	}

      while (str < limit && *str != ')')
	{
	  next = decode_signature_piece (stream, str, limit, &need_space);
	  if (! next)
	    {
	      error ("unparseable signature: '%s'", str0);
	      return;
	    }
	  
	  if (next < limit && *next != ')')
	    fputs (", ", stream);
	  str = next;
	}
      
      fputs (")", stream);
    }
}

/* This is a helper for print_stub_or_jni.  */
static void
print_name_for_stub_or_jni (FILE *stream, JCF *jcf, int name_index,
			    int signature_index, int is_init,
			    const char *name_override, int flags)
{
  const char *const prefix = flag_jni ? "Java_" : "";
  print_cxx_classname (stream, prefix, jcf, jcf->this_class, 1);
  fputs (flag_jni ? "_" : "::", stream);
  print_full_cxx_name (stream, jcf, name_index, 
		       signature_index, is_init, name_override,
		       flags);
}

static void
print_stub_or_jni (FILE* stream, JCF* jcf, int name_index,
		   int signature_index, int is_init,
		   const char *name_override, int flags)
{
  if (JPOOL_TAG (jcf, signature_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
    }
  else
    {
      int length = JPOOL_UTF_LENGTH (jcf, signature_index);
      const unsigned char *str0 = JPOOL_UTF_DATA (jcf, signature_index);
      const unsigned char *str = str0;
      const unsigned char *limit = str + length;
      int need_space = 0;
      int is_method = str[0] == '(';
      const unsigned char *next;

      /* Don't print fields in the JNI case.  */
      if (! is_method && flag_jni)
	return;

      if (flag_jni && ! stubs)
	fputs ("JNIEXPORT ", stream);

      /* If printing a method, skip to the return signature and print
	 that first.  However, there is no return value if this is a
	 constructor.  */
      if (is_method && ! is_init)
	{
	  while (str < limit)
	    {
	      int ch = *str++;
	      if (ch == ')')
		break;
	    }
	}

      /* If printing a field or an ordinary method, then print the
	 "return value" now.  Note that a constructor can't be native,
	 so we don't bother checking this in the JNI case.  */
      if (! is_method || ! is_init)
	{
	  next = decode_signature_piece (stream, str, limit, &need_space);
	  if (! next)
	    {
	      error ("unparseable signature: '%s'", str0);
	      return;
	    }
	}

      /* When printing a JNI header we need to respect the space.  In
	 other cases we're just going to insert a newline anyway.  */
      fputs (need_space && ! stubs ? " " : "\n", stream);

      if (flag_jni && ! stubs)
	fputs ("JNICALL ", stream);
      
      /* Now print the name of the thing.  */
      print_name_for_stub_or_jni (stream, jcf, name_index,
				  signature_index, is_init, name_override,
				  flags);

      /* Print the body.  */
      if (stubs)
	{
	  if (flag_jni)
	    fputs ("\n{\n  (*env)->FatalError (env, \"", stream);
	  else
	    fputs ("\n{\n  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 (\"", stream);
	  print_name_for_stub_or_jni (stream, jcf, name_index,
				      signature_index, is_init,
				      name_override,
				      flags);
	  fprintf (stream, " not implemented\")%s;\n}\n\n",
		   flag_jni ? "" : ")");
	}
    }
}

static void
print_mangled_classname (FILE *stream, JCF *jcf, const char *prefix, int index)
{
  int name_index = JPOOL_USHORT1 (jcf, index);
  fputs (prefix, stream);
  jcf_print_utf8_replace (out,
			  JPOOL_UTF_DATA (jcf, name_index),
			  JPOOL_UTF_LENGTH (jcf, name_index),
			  '/', '_');
}

/* Print PREFIX, then a class name in C++ format.  If the name refers
   to an array, ignore it and don't print PREFIX.  Returns 1 if
   something was printed, 0 otherwise.  */
static int
print_cxx_classname (FILE *stream, const char *prefix,
		     JCF *jcf, int index, int add_scope)
{
  int name_index = JPOOL_USHORT1 (jcf, index);
  int len, c;
  const unsigned char *s, *p, *limit;

  s = JPOOL_UTF_DATA (jcf, name_index);
  len = JPOOL_UTF_LENGTH (jcf, name_index);
  limit = s + len;

  /* Explicitly omit arrays here.  */
  p = s;
  c = UTF8_GET (p, limit);
  if (c == '[')
    return 0;

  fputs (prefix, stream);

  /* Print a leading "::" so we look in the right namespace.  */
  if (! flag_jni && ! stubs && add_scope)
    fputs ("::", stream);

  while (s < limit)
    {
      c = UTF8_GET (s, limit);
      if (c == '/')
	fputs (flag_jni ? "_" : "::", stream);
      else
	jni_print_char (stream, c);
    }

  return 1;
}

int written_class_count = 0;

/* Return name of superclass.  If LEN is not NULL, fill it with length
   of name.  */
static const unsigned char *
super_class_name (JCF *derived_jcf, int *len)
{
  int supername_index = JPOOL_USHORT1 (derived_jcf, derived_jcf->super_class);
  int supername_length = JPOOL_UTF_LENGTH (derived_jcf, supername_index);
  const unsigned char *supername =
    JPOOL_UTF_DATA (derived_jcf, supername_index);

  if (len)
    *len = supername_length;

  return supername;
}

static void
handle_inner_classes (int count)
{
  int i;

  if (out && ! flag_jni && ! stubs && count > 0)
    fprintf (out, "\n");

  for (i = 0; i < count; ++i)
    {
      JCF_u2 inner_info_index = JCF_readu2 (current_jcf);

      /* There are a few more values here, but we don't care about
	 them.  The (void) cast is apparently the only way to avoid a
	 warning here.  */
      (void) JCF_readu2 (current_jcf);
      (void) JCF_readu2 (current_jcf);
      (void) JCF_readu2 (current_jcf);

      if (out && ! flag_jni && ! stubs)
	{
	  print_mangled_classname (out, current_jcf, "  friend class ",
				   inner_info_index);
	  fprintf (out, ";\n");
	}
    }
}



/* We keep track of all the `#include's we generate, so we can avoid
   duplicates.  */
struct include
{
  char *name;
  struct include *next;
};

/* List of all includes.  */
static struct include *all_includes = NULL;

/* Generate a #include.  */
static void
print_include (FILE *out, const unsigned char *utf8, int len)
{
  struct include *incl;

  if (! out)
    return;

  if (len == -1)
    len = strlen ((const char *) utf8);

  for (incl = all_includes; incl; incl = incl->next)
    {
      /* We check the length because we might have a proper prefix.  */
      if (len == (int) strlen (incl->name)
	  && ! strncmp (incl->name, (const char *) utf8, len))
	return;
    }

  incl = xmalloc (sizeof (struct include));
  incl->name = xmalloc (len + 1);
  strncpy (incl->name, (const char *) utf8, len);
  incl->name[len] = '\0';
  incl->next = all_includes;
  all_includes = incl;

  fputs ("#include <", out);
  jcf_print_utf8_replace (out, utf8, len,
			  '/',
			  flag_jni ? '_' : '/');
  fputs (".h>\n", out);
}



/* This is used to represent part of a package or class name.  */
struct namelet
{
  /* The text of this part of the name.  */
  char *name;
  /* True if this represents a class.  */
  int is_class;
  /* Linked list of all classes and packages inside this one.  */
  struct namelet *subnamelets;
  /* Pointer to next sibling.  */
  struct namelet *next;
};

static void add_namelet (const unsigned char *, const unsigned char *,
			 struct namelet *);
static void print_namelet (FILE *, struct namelet *, int);

/* The special root namelet.  */
static struct namelet root =
{
  NULL,
  0,
  NULL,
  NULL
};

/* This extracts the next name segment from the full UTF-8 encoded
   package or class name and links it into the tree.  It does this
   recursively.  */
static void
add_namelet (const unsigned char *name, const unsigned char *name_limit,
	     struct namelet *parent)
{
  const unsigned char *p;
  struct namelet *n = NULL, *np;

  /* We want to skip the standard namespaces that we assume the
     runtime already knows about.  We only do this at the top level,
     though, hence the check for `root'.  */
  if (parent == &root)
    {
#define JAVALANG "java/lang/"
#define JAVAIO "java/io/"
#define JAVAUTIL "java/util/"
      if ((name_limit - name >= (int) sizeof (JAVALANG) - 1
	   && ! strncmp ((const char *) name, JAVALANG, sizeof (JAVALANG) - 1))
	  || (name_limit - name >= (int) sizeof (JAVAUTIL) - 1
	      && ! strncmp ((const char *) name, JAVAUTIL, sizeof (JAVAUTIL) - 1))
	  || (name_limit - name >= (int) sizeof (JAVAIO) - 1
	      && ! strncmp ((const char *) name, JAVAIO, sizeof (JAVAIO) - 1)))
	return;
    }

  for (p = name; p < name_limit && *p != '/'; ++p)
    ;

  /* Search for this name beneath the PARENT node.  */
  for (np = parent->subnamelets; np != NULL; np = np->next)
    {
      /* We check the length because we might have a proper prefix.  */
      if ((int) strlen (np->name) == p - name &&
	  ! strncmp ((const char *) name, np->name, p - name))
	{
	  n = np;
	  break;
	}
    }

  if (n == NULL)
    {
      n = xmalloc (sizeof (struct namelet));
      n->name = xmalloc (p - name + 1);
      strncpy (n->name, (const char *) name, p - name);
      n->name[p - name] = '\0';
      n->is_class = (p == name_limit);
      n->subnamelets = NULL;
      n->next = parent->subnamelets;
      parent->subnamelets = n;
    }

  /* We recurse if there is more text, and if the trailing piece does
     not represent an inner class. */
  if (p < name_limit)
    add_namelet (p + 1, name_limit, n);
}

/* Print a single namelet.  Destroys namelets while printing.  */
static void
print_namelet (FILE *out, struct namelet *name, int depth)
{
  int i, term = 0;
  struct namelet *c;

  if (name->name)
    {
      for (i = 0; i < depth; ++i)
	fputc (' ', out);
      fprintf (out, "%s %s", name->is_class ? "class" : "namespace",
	       name->name);
      if (name->is_class && name->subnamelets == NULL)
	fputs (";\n", out);
      else
	{
	  term = 1;
	  fputs ("\n", out);
	  for (i = 0; i < depth; ++i)
	    fputc (' ', out);
	  fputs ("{\n", out);
	}
    }

  c = name->subnamelets;
  while (c != NULL)
    {
      struct namelet *next = c->next;
      print_namelet (out, c, depth + 2);
      c = next;
    }
  name->subnamelets = NULL;

  if (name->name)
    {
      if (term)
	{
	  for (i = 0; i < depth; ++i)
	    fputc (' ', out);
	  fputs ("}\n", out);
	  /* Only print a `;' when printing a class.  C++ is evil.  */
	  if (name->is_class)
	    fputs (";", out);
	}

      free (name->name);
      free (name);
    }
}

/* This is called to add some classes to the list of classes for which
   we need decls.  The signature argument can be a function
   signature.  */
static void
add_class_decl (FILE *out, JCF *jcf, JCF_u2 signature)
{
  const unsigned char *s = JPOOL_UTF_DATA (jcf, signature);
  int len = JPOOL_UTF_LENGTH (jcf, signature);
  int i;

  for (i = 0; i < len; ++i)
    {
      int start;

      /* If we see an array, then we include the array header.  */
      if (s[i] == '[')
	{
	  print_include (out, (const unsigned char *) "gcj/array", -1);
	  continue;
	}

      /* We're looking for `L<stuff>;' -- everything else is
	 ignorable.  */
      if (s[i] != 'L')
	continue;

      for (start = ++i; i < len && s[i] != ';'; ++i)
	;

      add_namelet (&s[start], &s[i], &root);
    }
}

/* Print declarations for all classes required by this class.  Any
   class or package in the `java' package is assumed to be handled
   statically in libjava; we don't generate declarations for these.
   This makes the generated headers a bit easier to read.  */
static void
print_class_decls (FILE *out, JCF *jcf, int self)
{
  /* Make sure to always add the current class to the list of things
     that should be declared.  */
  int name_index = JPOOL_USHORT1 (jcf, self);
  int len;
  const unsigned char *s;

  s = JPOOL_UTF_DATA (jcf, name_index);
  len = JPOOL_UTF_LENGTH (jcf, name_index);
  add_namelet (s, s + len, &root);

  if (root.subnamelets)
    {
      fputs ("extern \"Java\"\n{\n", out);
      /* We use an initial offset of 0 because the root namelet
	 doesn't cause anything to print.  */
      print_namelet (out, &root, 0);
      fputs ("}\n\n", out);
    }
}



static void
process_file (JCF *jcf, FILE *out)
{
  int code, i;
  uint32 field_start, method_end, method_start;

  current_jcf = jcf;

  last_access = -1;

  if (jcf_parse_preamble (jcf) != 0)
    {
      error ("Not a valid Java .class file.");
      return;
    }

  /* Parse and possibly print constant pool */
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    {
      error ("error while parsing constant pool");
      return;
    }
  code = verify_constant_pool (jcf);
  if (code > 0)
    {
      error ("error in constant pool entry #%d", code);
      return;
    }

  jcf_parse_class (jcf);

  if (written_class_count++ == 0 && out)
    {
      const char *cstart, *cstart2, *mode, *cend, *what, *jflag;
      if (flag_jni)
	{
	  cstart = "/*";
	  cstart2 = "  ";
	  cend = " */";
	  mode = "";
	  what = "JNI";
	  jflag = " -jni";
	}
      else
	{
	  cstart = "//";
	  cstart2 = "//";
	  cend = "";
	  mode = " -*- c++ -*-";
	  what = "CNI";
	  jflag = "";
	}

      if (! stubs)
	fprintf (out, "%s DO NOT EDIT THIS FILE - it is machine generated%s%s\n\n",
		 cstart, mode, cend);
      else
	{
	  fprintf (out, "%s This file was created by `" TOOLNAME " -stubs%s'.%s\n\
%s\n\
%s This file is intended to give you a head start on implementing native\n\
%s methods using %s.\n\
%s Be aware: running `" TOOLNAME " -stubs %s' once more for this class may\n\
%s overwrite any edits you have made to this file.%s\n\n",
		   cstart, jflag, mode,
		   cstart2,
		   cstart2,
		   cstart2,
		   what,
		   cstart2,
		   jflag,
		   cstart2,
		   cend);
	}
    }

  if (out)
    {
      if (! stubs)
	{
	  print_mangled_classname (out, jcf, "#ifndef __", jcf->this_class);
	  fprintf (out, "__\n");

	  print_mangled_classname (out, jcf, "#define __", jcf->this_class);
	  fprintf (out, "__\n\n");

	  if (flag_jni)
	    {
	      fprintf (out, "#include <jni.h>\n\n");
	      fprintf (out, "#ifdef __cplusplus\n");
	      fprintf (out, "extern \"C\"\n");
	      fprintf (out, "{\n");
	      fprintf (out, "#endif\n");
	    }
	  else  
	    {
	      /* We do this to ensure that inline methods won't be
		 `outlined' by g++.  This works as long as method and
		 fields are not added by the user.  */
	      fprintf (out, "#pragma interface\n");

	      if (jcf->super_class)
		{
		  int super_length;
		  const unsigned char *supername =
		    super_class_name (jcf, &super_length);

		  fputs ("\n", out);
		  print_include (out, supername, super_length);
		}
	    }
	}
      else
	{
	  /* Strip off the ".class" portion of the name when printing
	     the include file name.  */
	  char *name;
	  int i, len = strlen (jcf->classname);
	  if (len > 6 && ! strcmp (&jcf->classname[len - 6], ".class"))
	    len -= 6;
	  /* Turn the class name into a file name.  */
	  name = xmalloc (len + 1);
	  for (i = 0; i < len; ++i)
	    name[i] = jcf->classname[i] == '.' ? '/' : jcf->classname[i];
	  name[i] = '\0';
	  print_include (out, (const unsigned char *) name, len);
	  free (name);

	  if (! flag_jni)
	    {
	      print_include (out, (const unsigned char *) "gcj/cni", -1);
	      print_include (out, (const unsigned char *) "java/lang/UnsupportedOperationException",
			     -1);
	    }
	}
    }

  /* We want to parse the methods first.  But we need to find where
     they start.  So first we skip the fields, then parse the methods.
     Then we parse the fields and skip the methods.  This is ugly, but
     not too bad since we need two full passes to get class decl
     information anyway.  */
  field_pass = 0;
  field_start = JCF_TELL (jcf);
  jcf_parse_fields (jcf);

  method_start = JCF_TELL (jcf);
  method_pass = 0;
  jcf_parse_methods (jcf);

  if (out)
    fputs ("\n", out);

  if (out && ! flag_jni)
    {
      if (! stubs)
	print_class_decls (out, jcf, jcf->this_class);

      for (i = 0; i < prepend_count; ++i)
	fprintf (out, "%s\n", prepend_specs[i]);
      if (prepend_count > 0)
	fputc ('\n', out);

      if (! stubs)
	{
	  if (! print_cxx_classname (out, "class ", jcf,
				     jcf->this_class, 0))
	    {
	      error ("class is of array type\n");
	      return;
	    }
	  if (jcf->super_class)
	    {
	      if (! print_cxx_classname (out, " : public ", 
					 jcf, jcf->super_class, 1))
		{
		  error ("base class is of array type");
		  return;
		}
	    }

	  fputs ("\n{\n", out);
	}
    }

  /* Now go back for second pass over methods and fields.  */
  is_first_data_member = 1;

  JCF_SEEK (jcf, method_start);
  method_pass = 1;
  jcf_parse_methods (jcf);
  method_end = JCF_TELL (jcf);

  field_pass = 1;
  JCF_SEEK (jcf, field_start);
  jcf_parse_fields (jcf);
  JCF_SEEK (jcf, method_end);

  jcf_parse_final_attributes (jcf);

  if (out && ! stubs)
    {
      if (flag_jni)
	{
	  fprintf (out, "\n#ifdef __cplusplus\n");
	  fprintf (out, "}\n");
	  fprintf (out, "#endif\n");
	}
      else
	{
	  /* Generate friend decl if we still must.  */
	  for (i = 0; i < friend_count; ++i)
	    fprintf (out, "  friend %s\n", friend_specs[i]);

	  /* Generate extra declarations.  */
	  if (add_count > 0)
	    fputc ('\n', out);
	  for (i = 0; i < add_count; ++i)
	    fprintf (out, "  %s\n", add_specs[i]);

	  /* Generate an entry for the class object.  */
	  generate_access (out, ACC_PUBLIC);
	  fprintf (out, "\n  static ::java::lang::Class class$;\n");

	  fputs ("}", out);
	  
	  if (jcf->access_flags & ACC_INTERFACE)
	    fputs (" __attribute__ ((java_interface))", out);

	  fputs (";\n", out);

	  if (append_count > 0)
	    fputc ('\n', out);
	  for (i = 0; i < append_count; ++i)
	    fprintf (out, "%s\n", append_specs[i]);
	}

      print_mangled_classname (out, jcf, 
			       "\n#endif /* __", jcf->this_class);
      fprintf (out, "__ */\n");
    }
}



/* This is used to mark options with no short value.  */
#define LONG_OPT(Num)  ((Num) + 128)

#define OPT_classpath     LONG_OPT (0)
#define OPT_CLASSPATH     OPT_classpath
#define OPT_bootclasspath LONG_OPT (1)
#define OPT_extdirs       LONG_OPT (2)
#define OPT_HELP          LONG_OPT (3)
#define OPT_TEMP          LONG_OPT (4)
#define OPT_VERSION       LONG_OPT (5)
#define OPT_PREPEND       LONG_OPT (6)
#define OPT_FRIEND        LONG_OPT (7)
#define OPT_ADD           LONG_OPT (8)
#define OPT_APPEND        LONG_OPT (9)
#define OPT_M             LONG_OPT (10)
#define OPT_MM            LONG_OPT (11)
#define OPT_MG            LONG_OPT (12)
#define OPT_MD            LONG_OPT (13)
#define OPT_MMD           LONG_OPT (14)
#define OPT_FORCE         LONG_OPT (15)
#define OPT_OLD           LONG_OPT (16)
#define OPT_TRACE         LONG_OPT (17)

static const struct option options[] =
{
  { "classpath",     required_argument, NULL, OPT_classpath },
  { "bootclasspath", required_argument, NULL, OPT_bootclasspath },
  { "extdirs",       required_argument, NULL, OPT_extdirs },
  { "CLASSPATH",     required_argument, NULL, OPT_CLASSPATH },
  { "help",          no_argument,       NULL, OPT_HELP },
  { "stubs",         no_argument,       &stubs, 1 },
  { "td",            required_argument, NULL, OPT_TEMP },
  { "verbose",       no_argument,       NULL, 'v' },
  { "version",       no_argument,       NULL, OPT_VERSION },
  { "prepend",       required_argument, NULL, OPT_PREPEND },
  { "friend",        required_argument, NULL, OPT_FRIEND },
  { "add",           required_argument, NULL, OPT_ADD },
  { "append",        required_argument, NULL, OPT_APPEND },
  { "M",             no_argument,       NULL, OPT_M   },
  { "MM",            no_argument,       NULL, OPT_MM  },
  { "MG",            no_argument,       NULL, OPT_MG  },
  { "MD",            no_argument,       NULL, OPT_MD  },
  { "MMD",           no_argument,       NULL, OPT_MMD },
  { "jni",           no_argument,       &flag_jni, 1 },
  { "force",         no_argument,       NULL, OPT_FORCE },
  /* If the output file should be named "ld" then a space is needed
     between -o and its argument, ld. */
  { "old",           no_argument,       NULL, OPT_OLD },
  { "trace",         no_argument,       NULL, OPT_TRACE },
  { NULL,            required_argument, NULL, 'J' },
  { NULL,            no_argument,       NULL, 0 }
};

static void
usage (void)
{
  fprintf (stderr, _("Try '" TOOLNAME " --help' for more information.\n"));
  exit (1);
}

static void
help (void)
{
  printf (_("Usage: " TOOLNAME " [OPTION]... CLASS...\n\n"));
  printf (_("Generate C or C++ header files from .class files\n\n"));
  printf (_("  -stubs                  Generate an implementation stub file\n"));
  printf (_("  -jni                    Generate a JNI header or stub\n"));
  printf (_("  -force                  Always overwrite output files\n"));
  printf (_("  -old                    Unused compatibility option\n"));
  printf (_("  -trace                  Unused compatibility option\n"));
  printf (_("  -J OPTION               Unused compatibility option\n"));
  printf ("\n");
  printf (_("  -add TEXT               Insert TEXT into class body\n"));
  printf (_("  -append TEXT            Insert TEXT after class declaration\n"));
  printf (_("  -friend TEXT            Insert TEXT as 'friend' declaration\n"));
  printf (_("  -prepend TEXT           Insert TEXT before start of class\n"));
  printf ("\n");
  printf (_("  --classpath PATH        Set path to find .class files\n"));
  printf (_("  -IDIR                   Append directory to class path\n"));
  printf (_("  --bootclasspath PATH    Override built-in class path\n"));
  printf (_("  --extdirs PATH          Set extensions directory path\n"));
  printf (_("  -d DIRECTORY            Set output directory name\n"));
  printf (_("  -o FILE                 Set output file name\n"));
  printf (_("  -td DIRECTORY           Set temporary directory name\n"));
  printf ("\n");
  printf (_("  --help                  Print this help, then exit\n"));
  printf (_("  --version               Print version number, then exit\n"));
  printf (_("  -v, --verbose           Print extra information while running\n"));
  printf ("\n");
  printf (_("  -M                      Print all dependencies to stdout;\n"
	    "                             suppress ordinary output\n"));
  printf (_("  -MM                     Print non-system dependencies to stdout;\n"
	    "                             suppress ordinary output\n"));
  printf (_("  -MD                     Print all dependencies to stdout\n"));
  printf (_("  -MMD                    Print non-system dependencies to stdout\n"));
  /* We omit -MG until it is implemented.  */
  printf ("\n");
  printf (_("For bug reporting instructions, please see:\n"
	    "%s.\n"), bug_report_url);
  exit (0);
}

static void
version (void)
{
  printf (TOOLNAME " (GCC) %s\n\n", version_string);
  printf ("Copyright %s 2004 Free Software Foundation, Inc.\n", _("(C)"));
  printf (_("This is free software; see the source for copying conditions.  There is NO\n"
	    "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
  exit (0);
}

int
main (int argc, char** argv)
{
  JCF jcf;
  int argi;
  char *output_file = NULL;
  int emit_dependencies = 0, suppress_output = 0;
  int opt;

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  if (argc <= 1)
    {
      error ("no classes specified");
      usage ();
    }

  jcf_path_init ();

  /* We use getopt_long_only to allow single `-' long options.  For
     some of our options this is more natural.  */
  while ((opt = getopt_long_only (argc, argv, "J:I:d:o:v", options, NULL)) != -1)
    {
      switch (opt)
	{
	case 0:
	  /* Already handled.  */
	  break;

	case 'o':
	  output_file = optarg;
	  break;

	case 'd':
	  output_directory = optarg;
	  break;

	case 'I':
	  jcf_path_include_arg (optarg);
	  break;

	case 'v':
	  verbose++;
	  break;

	case OPT_classpath:
	  jcf_path_classpath_arg (optarg);
	  break;

	case OPT_bootclasspath:
	  jcf_path_bootclasspath_arg (optarg);
	  break;

	case OPT_extdirs:
	  jcf_path_extdirs_arg (optarg);
	  break;

	case OPT_HELP:
	  help ();
	  break;

	case OPT_TEMP:
	  temp_directory = optarg;
	  break;

	case OPT_VERSION:
	  version ();
	  break;

	case OPT_PREPEND:
	  if (prepend_count == 0)
	    prepend_specs = ALLOC (argc * sizeof (char*));
	  prepend_specs[prepend_count++] = optarg;
	  break;

	case OPT_FRIEND:
	  if (friend_count == 0)
	    friend_specs = ALLOC (argc * sizeof (char*));
	  friend_specs[friend_count++] = optarg;
	  break;

	case OPT_ADD:
	  if (add_count == 0)
	    add_specs = ALLOC (argc * sizeof (char*));
	  add_specs[add_count++] = optarg;
	  break;

	case OPT_APPEND:
	  if (append_count == 0)
	    append_specs = ALLOC (argc * sizeof (char*));
	  append_specs[append_count++] = optarg;
	  break;

	case OPT_M:
	  emit_dependencies = 1;
	  suppress_output = 1;
	  jcf_dependency_init (1);
	  break;

	case OPT_MM:
	  emit_dependencies = 1;
	  suppress_output = 1;
	  jcf_dependency_init (0);
	  break;

	case OPT_MG:
	  error ("'-MG' option is unimplemented");
	  exit (1);

	case OPT_MD:
	  emit_dependencies = 1;
	  jcf_dependency_init (1);
	  break;

	case OPT_MMD:
	  emit_dependencies = 1;
	  jcf_dependency_init (0);
	  break;

	case OPT_FORCE:
	  break;

	case OPT_OLD:
	  break;

	case OPT_TRACE:
	  break;

	case 'J':
          /* Ignore -J options. */
	  break;

	default:
	  usage ();
	  break;
	}
    }

  if (optind == argc)
    {
      error ("no classes specified");
      usage ();
    }

  jcf_path_seal (verbose);

  if (output_file && emit_dependencies)
    {
      error ("can't specify both -o and -MD");
      exit (1);
    }

  for (argi = optind; argi < argc; argi++)
    {
      char *classname = argv[argi];
      char *current_output_file;
      const char *classfile_name;

      if (verbose)
	printf (_("Processing %s\n"), classname);
      if (! output_file)
	jcf_dependency_reset ();
      classfile_name = find_class (classname, strlen (classname), &jcf, 0);
      if (classfile_name == NULL)
	{
	  error ("%s: no such class", classname);
	  exit (1);
	}
      if (verbose)
	printf (_("Found in %s\n"), classfile_name);
      if (output_file)
	{
	  if (strcmp (output_file, "-") == 0)
	    out = stdout;
	  else if (out == NULL)
	    {
	      out = fopen (output_file, "w");
	    }
	  if (out == NULL)
	    {
	      perror (output_file);
	      exit (1);
	    }
	  current_output_file = output_file;
	}
      else
	{
	  int dir_len = strlen (output_directory);
	  int i, classname_length = strlen (classname);
	  current_output_file = ALLOC (dir_len + classname_length + 5);
	  strcpy (current_output_file, output_directory);
	  if (dir_len > 0 && output_directory[dir_len-1] != '/')
	    current_output_file[dir_len++] = '/';
	  for (i = 0; classname[i] != '\0'; i++)
	    {
	      char ch = classname[i];
	      if (ch == '.')
		ch = '/';
	      if (flag_jni && ch == '/')
		ch = '_';
	      current_output_file[dir_len++] = ch;
	    }
	  if (emit_dependencies)
	    {
	      if (suppress_output)
		{
		  jcf_dependency_set_dep_file ("-");
		  out = NULL;
		}
	      else
		{
		  /* We use `.hd' and not `.d' to avoid clashes with
		     dependency tracking from straight compilation.  */
		  strcpy (current_output_file + dir_len, ".hd");
		  jcf_dependency_set_dep_file (current_output_file);
		}
	    }
	  strcpy (current_output_file + dir_len, 
		  stubs ? (flag_jni ? ".c" : ".cc") : ".h");
	  jcf_dependency_set_target (current_output_file);
	  if (! suppress_output)
	    {
	      out = fopen (current_output_file, "w");
	      if (out == NULL)
		{
		  perror (current_output_file);
		  exit (1);
		}
	    }
	}
      free_method_name_list ();
      process_file (&jcf, out);
      JCF_FINISH (&jcf);
      if (current_output_file != output_file)
	free (current_output_file);
      jcf_dependency_write ();
    }

  if (out != NULL && out != stdout)
    fclose (out);

  return found_error;
}
