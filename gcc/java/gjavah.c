/* Program to write C++-suitable header files from a Java(TM) .class
   file.  This is similar to SUN's javah.

Copyright (C) 1996, 1998, 1999, 2000 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#include "config.h"
#include "system.h"
#include <math.h>

#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "java-opcodes.h"

#include "version.c"

#include <getopt.h>



/* The output file.  */
FILE *out = NULL;

/* Nonzero on failure.  */
static int found_error = 0;

/* Nonzero if we're generating JNI output.  */
static int flag_jni = 0;

/* Directory to place resulting files in. Set by -d option. */
const char *output_directory = "";

/* Directory to place temporary file.  Set by -td option.  Currently unused. */
const char *temp_directory = "/tmp";

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

#define ACC_VISIBILITY (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED)

/* Pass this macro the flags for a class and for a method.  It will
   return true if the method should be considered `final'.  */
#define METHOD_IS_FINAL(Class, Method) \
   (((Class) & ACC_FINAL) || ((Method) & (ACC_FINAL | ACC_PRIVATE)))

/* Pass this macro the flags for a method.  It will return true if the
   method is native.  */
#define METHOD_IS_NATIVE(Method) \
   ((Method) & ACC_NATIVE)

/* We keep a linked list of all method names we have seen.  This lets
   us determine if a method name and a field name are in conflict.  */
struct method_name
{
  unsigned char *name;
  int length;
  unsigned char *signature;
  int sig_length;
  struct method_name *next;
};

/* List of method names we've seen.  */
static struct method_name *method_name_list;

static void print_field_info PARAMS ((FILE*, JCF*, int, int, JCF_u2));
static void print_mangled_classname PARAMS ((FILE*, JCF*, const char*, int));
static int  print_cxx_classname PARAMS ((FILE*, const char*, JCF*, int));
static void print_method_info PARAMS ((FILE*, JCF*, int, int, JCF_u2));
static void print_c_decl PARAMS ((FILE*, JCF*, int, int, int, const char *,
				  int));
static void print_stub_or_jni PARAMS ((FILE*, JCF*, int, int, int,
				       const char *, int));
static void print_full_cxx_name PARAMS ((FILE*, JCF*, int, int, int,
					 const char *, int));
static void decompile_method PARAMS ((FILE*, JCF*, int));
static void add_class_decl PARAMS ((FILE*, JCF*, JCF_u2));

static int java_float_finite PARAMS ((jfloat));
static int java_double_finite PARAMS ((jdouble));
static void print_name PARAMS ((FILE *, JCF *, int));
static void print_base_classname PARAMS ((FILE *, JCF *, int));
static int utf8_cmp PARAMS ((const unsigned char *, int, const char *));
static const char *cxx_keyword_subst PARAMS ((const unsigned char *, int));
static void generate_access PARAMS ((FILE *, JCF_u2));
static int name_is_method_p PARAMS ((const unsigned char *, int));
static char *get_field_name PARAMS ((JCF *, int, JCF_u2));
static void print_field_name PARAMS ((FILE *, JCF *, int, JCF_u2));
static const unsigned char *super_class_name PARAMS ((JCF *, int *));
static void print_include PARAMS ((FILE *, const unsigned char *, int));
static const unsigned char *decode_signature_piece
  PARAMS ((FILE *, const unsigned char *, const unsigned char *, int *));
static void print_class_decls PARAMS ((FILE *, JCF *, int));
static void usage PARAMS ((void)) ATTRIBUTE_NORETURN;
static void help PARAMS ((void)) ATTRIBUTE_NORETURN;
static void version PARAMS ((void)) ATTRIBUTE_NORETURN;

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
      if (out && ! stubs && ! flag_jni)					      \
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
#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT)	      \
  if (method_pass)							      \
    {									      \
      decompiled = 0; method_printed = 0;				      \
      if (out)								      \
        print_method_info (out, jcf, NAME, SIGNATURE, ACCESS_FLAGS);	      \
    }									      \
  else if (flag_jni)							      \
    print_method_info (NULL, jcf, NAME, SIGNATURE, ACCESS_FLAGS);	      \
  else if (! stubs) add_class_decl (out, jcf, SIGNATURE);

#define HANDLE_CODE_ATTRIBUTE(MAX_STACK, MAX_LOCALS, CODE_LENGTH) \
  if (out && method_declared) decompile_method (out, jcf, CODE_LENGTH);

static int decompiled = 0;
#define HANDLE_END_METHOD() \
  if (out && method_printed) fputs (decompiled || stubs ? "\n" : ";\n", out);

#include "jcf-reader.c"

/* Some useful constants.  */
#define F_NAN_MASK 0x7f800000
#define D_NAN_MASK 0x7ff0000000000000LL

/* Return 1 if F is not Inf or NaN.  */
static int
java_float_finite (f)
     jfloat f;
{
  union {
    jfloat f;
    int32 i;
  } u;
  u.f = f;

  /* We happen to know that F_NAN_MASK will match all NaN values, and
     also positive and negative infinity.  That's why we only need one
     test here.  See The Java Language Specification, section 20.9.  */
  return (u.i & F_NAN_MASK) != F_NAN_MASK;
}

/* Return 1 if D is not Inf or NaN.  */
static int
java_double_finite (d)
     jdouble d;
{
  union {
    jdouble d;
    int64 i;
  } u;
  u.d = d;

  /* Now check for all NaNs.  */
  return (u.i & D_NAN_MASK) != D_NAN_MASK;
}

static void
DEFUN(print_name, (stream, jcf, name_index),
      FILE* stream AND JCF* jcf AND int name_index)
{
  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    fprintf (stream, "<not a UTF8 constant>");
  else
    jcf_print_utf8 (stream, JPOOL_UTF_DATA (jcf, name_index),
		    JPOOL_UTF_LENGTH (jcf, name_index));
}

/* Print base name of class.  The base name is everything after the
   final separator.  */

static void
print_base_classname (stream, jcf, index)
     FILE *stream;
     JCF *jcf;
     int index;
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

/* Return 0 if NAME is equal to STR, nonzero otherwise.  */

static int
utf8_cmp (str, length, name)
     const unsigned char *str;
     int length;
     const char *name;
{
  const unsigned char *limit = str + length;
  int i;

  for (i = 0; name[i]; ++i)
    {
      int ch = UTF8_GET (str, limit);
      if (ch != name[i])
	return 1;
    }

  return str != limit;
}

/* If NAME is the name of a C++ keyword, then return an override name.
   This is a name that can be used in place of the keyword.
   Otherwise, return NULL.  FIXME: for now, we only handle those
   keywords we know to be a problem for libgcj.  */

static const char *
cxx_keyword_subst (str, length)
     const unsigned char *str;
     int length;
{
  if (! utf8_cmp (str, length, "delete"))
    return "__dummy_delete";
  else if (! utf8_cmp (str, length, "enum"))
    return "__dummy_enum";
  return NULL;
}

/* Generate an access control keyword based on FLAGS.  */

static void
generate_access (stream, flags)
     FILE *stream;
     JCF_u2 flags;
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
name_is_method_p (name, length)
     const unsigned char *name;
     int length;
{
  struct method_name *p;

  for (p = method_name_list; p != NULL; p = p->next)
    {
      if (p->length == length && ! memcmp (p->name, name, length))
	return 1;
    }
  return 0;
}

/* If there is already a method named NAME, whose signature is not
   SIGNATURE, then return true.  Otherwise return false.  */
static int
overloaded_jni_method_exists_p (name, length, signature, sig_length)
     const unsigned char *name;
     int length;
     const char *signature;
     int sig_length;
{
  struct method_name *p;

  for (p = method_name_list; p != NULL; p = p->next)
    {
      if (p->length == length
	  && ! memcmp (p->name, name, length)
	  && (p->sig_length != sig_length
	      || memcmp (p->signature, signature, sig_length)))
	return 1;
    }
  return 0;
}

/* Get name of a field.  This handles renamings due to C++ clash.  */
static char *
get_field_name (jcf, name_index, flags)
     JCF *jcf;
     int name_index;
     JCF_u2 flags;
{
  unsigned char *name = JPOOL_UTF_DATA (jcf, name_index);
  int length = JPOOL_UTF_LENGTH (jcf, name_index);
  char *override;
  const char *tmpconstptr;


  if (name_is_method_p (name, length))
    {
      /* This field name matches a method.  So override the name with
	 a dummy name.  This is yucky, but it isn't clear what else to
	 do.  FIXME: if the field is static, then we'll be in real
	 trouble.  */
      if ((flags & ACC_STATIC))
	{
	  fprintf (stderr, "static field has same name as method\n");
	  found_error = 1;
	  return NULL;
	}

      override = xmalloc (length + 3);
      memcpy (override, name, length);
      strcpy (override + length, "__");
    }
  else if ((tmpconstptr = cxx_keyword_subst (name, length)) != NULL)
    {
      /* Must malloc OVERRIDE.  */
      override = xstrdup (tmpconstptr);
    }
  else
    override = NULL;
  
  return override;
}

/* Print a field name.  Convenience function for use with
   get_field_name.  */
static void
print_field_name (stream, jcf, name_index, flags)
     FILE *stream;
     JCF *jcf;
     int name_index;
     JCF_u2 flags;
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
DEFUN(print_field_info, (stream, jcf, name_index, sig_index, flags),
      FILE *stream AND JCF* jcf
      AND int name_index AND int sig_index AND JCF_u2 flags)
{
  char *override = NULL;

  generate_access (stream, flags);
  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
      return;
    }

  fputs ("  ", out);
  if ((flags & ACC_STATIC))
    fputs ("static ", out);

  if ((flags & ACC_FINAL))
    {
      if (current_field_value > 0)
	{
	  char buffer[25];
	  int done = 1;

	  switch (JPOOL_TAG (jcf, current_field_value))
	    {
	    case CONSTANT_Integer:
	      {
		jint num;
		int most_negative = 0;
		fputs ("const jint ", out);
		print_field_name (out, jcf, name_index, 0);
		fputs (" = ", out);
		num = JPOOL_INT (jcf, current_field_value);
		/* We single out the most negative number to print
		   specially.  This avoids later warnings from g++.  */
		if (num == (jint) 0x80000000)
		  {
		    most_negative = 1;
		    ++num;
		  }
		format_int (buffer, (jlong) num, 10);
		fprintf (out, "%sL%s;\n", buffer, most_negative ? " - 1" : "");
	      }
	      break;
	    case CONSTANT_Long:
	      {
		jlong num;
		int most_negative = 0;
		fputs ("const jlong ", out);
		print_field_name (out, jcf, name_index, 0);
		fputs (" = ", out);
		num = JPOOL_LONG (jcf, current_field_value);
		/* We single out the most negative number to print
                   specially..  This avoids later warnings from g++.  */
		if (num == (jlong) 0x8000000000000000LL)
		  {
		    most_negative = 1;
		    ++num;
		  }
		format_int (buffer, num, 10);
		fprintf (out, "%sLL%s;\n", buffer, most_negative ? " - 1" :"");
	      }
	      break;
	    case CONSTANT_Float:
	      {
		jfloat fnum = JPOOL_FLOAT (jcf, current_field_value);
		fputs ("const jfloat ", out);
		print_field_name (out, jcf, name_index, 0);
		if (! java_float_finite (fnum))
		  fputs (";\n", out);
		else
		  fprintf (out, " = %.10g;\n",  fnum);
	      }
	      break;
	    case CONSTANT_Double:
	      {
		jdouble dnum = JPOOL_DOUBLE (jcf, current_field_value);
		fputs ("const jdouble ", out);
		print_field_name (out, jcf, name_index, 0);
		if (! java_double_finite (dnum))
		  fputs (";\n", out);
		else
		  fprintf (out, " = %.17g;\n",  dnum);
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

  override = get_field_name (jcf, name_index, flags);
  print_c_decl (out, jcf, name_index, sig_index, 0, override, flags);
  fputs (";\n", out);

  if (override)
    free (override);
}

static void
DEFUN(print_method_info, (stream, jcf, name_index, sig_index, flags),
      FILE *stream AND JCF* jcf
      AND int name_index AND int sig_index AND JCF_u2 flags)
{
  const unsigned char *str;
  int length, is_init = 0;
  const char *override = NULL;

  method_declared = 0;
  method_access = flags;
  if (stream && JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    fprintf (stream, "<not a UTF8 constant>");
  str = JPOOL_UTF_DATA (jcf, name_index);
  length = JPOOL_UTF_LENGTH (jcf, name_index);
  if (str[0] == '<' || str[0] == '$')
    {
      /* Ignore internally generated methods like <clinit> and
	 $finit$.  However, treat <init> as a constructor.  */
      if (! utf8_cmp (str, length, "<init>"))
	is_init = 1;
      else if (! METHOD_IS_FINAL (jcf->access_flags, flags)
	       && ! (flags & ACC_STATIC))
	{
	  /* FIXME: i18n bug here.  Order of prints should not be
	     fixed.  */
	  fprintf (stderr, "ignored method `");
	  jcf_print_utf8 (stderr, str, length);
	  fprintf (stderr, "' marked virtual\n");
	  found_error = 1;
	  return;
	}
      else
	return;
    }
  else
    {
      struct method_name *nn;

      nn = (struct method_name *) xmalloc (sizeof (struct method_name));
      nn->name = (char *) xmalloc (length);
      memcpy (nn->name, str, length);
      nn->length = length;
      nn->next = method_name_list;
      nn->sig_length = JPOOL_UTF_LENGTH (jcf, sig_index);
      nn->signature = (char *) xmalloc (nn->sig_length);
      memcpy (nn->signature, JPOOL_UTF_DATA (jcf, sig_index),
	      nn->sig_length);
      method_name_list = nn;
    }

  /* If we're not printing, then the rest of this function doesn't
     matter.  This happens during the first method pass in JNI mode.
     Eww.  */
  if (! stream)
    return;

  /* We can't generate a method whose name is a C++ reserved word.  We
     can't just ignore the function, because that will cause incorrect
     code to be generated if the function is virtual (not only for
     calls to this function for for other functions after it in the
     vtbl).  So we give it a dummy name instead.  */
  override = cxx_keyword_subst (str, length);
  if (override)
    {
      /* If the method is static or final, we can safely skip it.  If
	 we don't skip it then we'll have problems since the mangling
	 will be wrong.  FIXME.  */
      if (METHOD_IS_FINAL (jcf->access_flags, flags)
	  || (flags & ACC_STATIC))
	return;
    }

  if (! stubs && ! flag_jni)
    {
      method_printed = 1;

      generate_access (stream, flags);
      
      fputs ("  ", out);
      if ((flags & ACC_STATIC))
	fputs ("static ", out);
      else if (! METHOD_IS_FINAL (jcf->access_flags, flags))
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
}

/* Try to decompile a method body.  Right now we just try to handle a
   simple case that we can do.  Expand as desired.  */
static void
decompile_method (out, jcf, code_len)
     FILE *out;
     JCF *jcf;
     int code_len;
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
      fputs (" { return ", out);
      index = (codes[2] << 8) | codes[3];
      /* FIXME: ensure that tag is CONSTANT_Fieldref.  */
      /* FIXME: ensure that the field's class is this class.  */
      name_and_type = JPOOL_USHORT2 (jcf, index);
      /* FIXME: ensure that tag is CONSTANT_NameAndType.  */
      name = JPOOL_USHORT1 (jcf, name_and_type);
      print_name (out, jcf, name);
      fputs ("; }", out);
      decompiled = 1;
    }
  else if (code_len == 2
	   && codes[0] == OPCODE_aload_0
	   && codes[1] == OPCODE_areturn)
    {
      /* Found `return this'.  */
      fputs (" { return this; }", out);
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

/* Print one piece of a signature.  Returns pointer to next parseable
   character on success, NULL on error.  */
static const unsigned char *
decode_signature_piece (stream, signature, limit, need_space)
     FILE *stream;
     const unsigned char *signature, *limit;
     int *need_space;
{
  const char *ctype;
  int array_depth = 0;

  switch (signature[0])
    {
    case '[':
      /* More spaghetti.  */

    array_loop:
      for (signature++; (signature < limit
			 && *signature >= '0'
			 && *signature <= '9'); signature++)
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
      if (flag_jni && ctype == NULL)
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
	  /* We know about certain types and special-case their
	     names.
	     FIXME: something like java.lang.Exception should be
	     printed as `jthrowable', because it is a subclass.  This
	     means that gcjh must read the entire hierarchy and
	     comprehend it.  */
	  if (! strncmp (signature, "Ljava/lang/String;",
			 sizeof ("Ljava/lang/String;") -1))
	    ctype = "jstring";
	  else if (! strncmp (signature, "Ljava/lang/Class;",
			      sizeof ("Ljava/lang/Class;") - 1))
	    ctype = "jclass";
	  else if (! strncmp (signature, "Ljava/lang/Throwable;",
			      sizeof ("Ljava/lang/Throwable;") - 1))
	    ctype = "jthrowable";
	  else if (! strncmp (signature, "Ljava/lang/ref/WeakReference;",
			      sizeof ("Ljava/lang/ref/WeakReference;") - 1))
	    ctype = "jweak";
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
	  /* `$' is the separator for an inner class.  */
	  if (ch == '/' || ch == '$')
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
      jcf_print_char (stream, *signature++);
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
DEFUN(print_c_decl, (stream, jcf, name_index, signature_index, is_init,
		     name_override, flags),
      FILE* stream AND JCF* jcf
      AND int name_index AND int signature_index
      AND int is_init AND const char *name_override AND int flags)
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
      register const  unsigned char *str = str0;
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
	      fprintf (stderr, "unparseable signature: `%s'\n", str0);
	      found_error = 1;
	      return;
	    }
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
DEFUN(print_full_cxx_name, (stream, jcf, name_index, signature_index,
			    is_init, name_override, flags),
      FILE* stream AND JCF* jcf
      AND int name_index AND int signature_index AND int is_init 
      AND const char *name_override AND int flags)
{
  int length = JPOOL_UTF_LENGTH (jcf, signature_index);
  const unsigned char *str0 = JPOOL_UTF_DATA (jcf, signature_index);
  register const unsigned char *str = str0;
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
					  signature, sig_len))
	{
	  /* If this method is overloaded by another native method,
	     then include the argument information in the mangled
	     name.  */
	  unsigned char *limit = signature + sig_len;
	  fputs ("__", stream);
	  while (signature < limit)
	    {
	      int ch = UTF8_GET (signature, limit);
	      if (ch == '(')
		{
		  /* Ignore.  */
		}
	      else if (ch == ')')
		{
		  /* Done.  */
		  break;
		}
	      else if (ch == '_')
		fputs ("_1", stream);
	      else if (ch == ';')
		fputs ("_2", stream);
	      else if (ch == '[')
		fputs ("_3", stream);
	      else if (ch == '/')
		fputs ("_", stream);
	      else if ((ch >= '0' && ch <= '9')
		       || (ch >= 'a' && ch <= 'z')
		       || (ch >= 'A' && ch <= 'Z'))
		fputc (ch, stream);
	      else
		{
		  /* "Unicode" character.  FIXME: upper or lower case
		     letters?  */
		  fprintf (stream, "_0%04x", ch);
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
	      fprintf (stderr, "unparseable signature: `%s'\n", str0);
	      found_error = 1;
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
DEFUN (print_name_for_stub_or_jni, (stream, jcf, name_index, signature_index,
				    is_init, name_override, flags),
       FILE *stream AND JCF *jcf
       AND int name_index AND int signature_index
       AND int is_init AND const char *name_override AND int flags)
{
  char *prefix = flag_jni ? "Java_" : "\n";
  print_cxx_classname (stream, prefix, jcf, jcf->this_class);
  fputs (flag_jni ? "_" : "::", stream);
  print_full_cxx_name (stream, jcf, name_index, 
		       signature_index, is_init, name_override,
		       flags);
}

static void
DEFUN(print_stub_or_jni, (stream, jcf, name_index, signature_index, is_init,
			  name_override, flags),
      FILE* stream AND JCF* jcf
      AND int name_index AND int signature_index
      AND int is_init AND const char *name_override AND int flags)
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
      register const unsigned char *str = str0;
      const unsigned char *limit = str + length;
      int need_space = 0;
      int is_method = str[0] == '(';
      const unsigned char *next;

      /* Don't print fields in the JNI case.  */
      if (! is_method && flag_jni)
	return;

      if (flag_jni && ! stubs)
	fputs ("extern ", stream);

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
	      fprintf (stderr, "unparseable signature: `%s'\n", str0);
	      found_error = 1;
	      return;
	    }
	}

      /* When printing a JNI header we need to respect the space.  In
	 other cases we're just going to insert a newline anyway.  */
      if (flag_jni)
	fputs (need_space && ! stubs ? " " : "\n", stream);

      /* Now print the name of the thing.  */
      print_name_for_stub_or_jni (stream, jcf, name_index,
				  signature_index, is_init, name_override,
				  flags);

      /* Print the body.  */
      if (stubs)
	{
	  if (flag_jni)
	    fputs ("\n{\n  (*env)->FatalError (\"", stream);
	  else
	    fputs ("\n{\n  JvFail (\"", stream);
	  print_name_for_stub_or_jni (stream, jcf, name_index,
				      signature_index, is_init,
				      name_override,
				      flags);
	  fputs (" not implemented\");\n}\n\n", stream);
	}
    }
}

static void
DEFUN(print_mangled_classname, (stream, jcf, prefix, index),
      FILE *stream AND JCF *jcf AND const char *prefix AND int index)
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
print_cxx_classname (stream, prefix, jcf, index)
     FILE *stream;
     const char *prefix;
     JCF *jcf;
     int index;
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
  if (! flag_jni)
    fputs ("::", stream);

  while (s < limit)
    {
      c = UTF8_GET (s, limit);
      if (c == '/')
	fputs (flag_jni ? "_" : "::", stream);
      else
	jcf_print_char (stream, c);
    }

  return 1;
}

int written_class_count = 0;

/* Return name of superclass.  If LEN is not NULL, fill it with length
   of name.  */
static const unsigned char *
super_class_name (derived_jcf, len)
     JCF *derived_jcf;
     int *len;
{
  int supername_index = JPOOL_USHORT1 (derived_jcf, derived_jcf->super_class);
  int supername_length = JPOOL_UTF_LENGTH (derived_jcf, supername_index);
  const unsigned char *supername =
    JPOOL_UTF_DATA (derived_jcf, supername_index);

  if (len)
    *len = supername_length;

  return supername;
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
print_include (out, utf8, len)
     FILE *out;
     const unsigned char *utf8;
     int len;
{
  struct include *incl;

  if (! out)
    return;

  if (len == -1)
    len = strlen (utf8);

  for (incl = all_includes; incl; incl = incl->next)
    {
      /* We check the length because we might have a proper prefix.  */
      if (len == (int) strlen (incl->name)
	  && ! strncmp (incl->name, utf8, len))
	return;
    }

  incl = (struct include *) xmalloc (sizeof (struct include));
  incl->name = xmalloc (len + 1);
  strncpy (incl->name, utf8, len);
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

static void add_namelet PARAMS ((const unsigned char *,
				const unsigned char *, struct namelet *));
static void print_namelet PARAMS ((FILE *, struct namelet *, int));

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
add_namelet (name, name_limit, parent)
     const unsigned char *name, *name_limit;
     struct namelet *parent;
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
	   && ! strncmp (name, JAVALANG, sizeof (JAVALANG) - 1))
	  || (name_limit - name >= (int) sizeof (JAVAUTIL) - 1
	      && ! strncmp (name, JAVAUTIL, sizeof (JAVAUTIL) - 1))
	  || (name_limit - name >= (int) sizeof (JAVAIO) - 1
	      && ! strncmp (name, JAVAIO, sizeof (JAVAIO) - 1)))
	return;
    }

  for (p = name; p < name_limit && *p != '/' && *p != '$'; ++p)
    ;

  /* Search for this name beneath the PARENT node.  */
  for (np = parent->subnamelets; np != NULL; np = np->next)
    {
      /* We check the length because we might have a proper prefix.  */
      if ((int) strlen (np->name) == p - name &&
	  ! strncmp (name, np->name, p - name))
	{
	  n = np;
	  break;
	}
    }

  if (n == NULL)
    {
      n = (struct namelet *) xmalloc (sizeof (struct namelet));
      n->name = xmalloc (p - name + 1);
      strncpy (n->name, name, p - name);
      n->name[p - name] = '\0';
      n->is_class = (p == name_limit || *p == '$');
      n->subnamelets = NULL;
      n->next = parent->subnamelets;
      parent->subnamelets = n;
    }

  /* We recurse if there is more text, and if the trailing piece does
     not represent an inner class. */
  if (p < name_limit && *p != '$')
    add_namelet (p + 1, name_limit, n);
}

/* Print a single namelet.  Destroys namelets while printing.  */
static void
print_namelet (out, name, depth)
     FILE *out;
     struct namelet *name;
     int depth;
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
add_class_decl (out, jcf, signature)
     FILE *out;
     JCF *jcf;
     JCF_u2 signature;
{
  const unsigned char *s = JPOOL_UTF_DATA (jcf, signature);
  int len = JPOOL_UTF_LENGTH (jcf, signature);
  int i;
  /* Name of class we are processing.  */
  int name_index = JPOOL_USHORT1 (jcf, jcf->this_class);
  int tlen = JPOOL_UTF_LENGTH (jcf, name_index);
  const char *tname = JPOOL_UTF_DATA (jcf, name_index);

  for (i = 0; i < len; ++i)
    {
      int start, saw_dollar;

      /* If we see an array, then we include the array header.  */
      if (s[i] == '[')
	{
	  print_include (out, "gcj/array", -1);
	  continue;
	}

      /* We're looking for `L<stuff>;' -- everything else is
	 ignorable.  */
      if (s[i] != 'L')
	continue;

      saw_dollar = 0;
      for (start = ++i; i < len && s[i] != ';'; ++i)
	{
	  if (! saw_dollar && s[i] == '$' && out)
	    {
	      saw_dollar = 1;
	      /* If this class represents an inner class, then
		 generate a `#include' for the outer class.  However,
		 don't generate the include if the outer class is the
		 class we are processing.  */
	      if (i - start < tlen || strncmp (&s[start], tname, i - start))
		print_include (out, &s[start], i - start);
	      break;
	    }
	}

      /* If we saw an inner class, then the generated #include will
	 declare the class.  So in this case we needn't bother.  */
      if (! saw_dollar)
	add_namelet (&s[start], &s[i], &root);
    }
}

/* Print declarations for all classes required by this class.  Any
   class or package in the `java' package is assumed to be handled
   statically in libjava; we don't generate declarations for these.
   This makes the generated headers a bit easier to read.  */
static void
print_class_decls (out, jcf, self)
     FILE *out;
     JCF *jcf;
     int self;
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
      fputs ("};\n\n", out);
    }
}



static void
DEFUN(process_file, (jcf, out),
      JCF *jcf AND FILE *out)
{
  int code, i;
  uint32 field_start, method_end, method_start;

  current_jcf = jcf;

  last_access = -1;

  if (jcf_parse_preamble (jcf) != 0)
    {
      fprintf (stderr, "Not a valid Java .class file.\n");
      found_error = 1;
      return;
    }

  /* Parse and possibly print constant pool */
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    {
      fprintf (stderr, "error while parsing constant pool\n");
      found_error = 1;
      return;
    }
  code = verify_constant_pool (jcf);
  if (code > 0)
    {
      fprintf (stderr, "error in constant pool entry #%d\n", code);
      found_error = 1;
      return;
    }

  jcf_parse_class (jcf);

  if (written_class_count++ == 0 && out)
    {
      char *cstart, *cstart2, *mode, *cend, *what, *jflag;
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
	  fprintf (out, "%s This file was created by `gcjh -stubs%s'.%s\n\
%s\n\
%s This file is intended to give you a head start on implementing native\n\
%s methods using %s.\n\
%s Be aware: running `gcjh -stubs %s' once more for this class may\n\
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
	  int len = strlen (jcf->classname);
	  if (len > 6 && ! strcmp (&jcf->classname[len - 6], ".class"))
	    len -= 6;
	  print_include (out, jcf->classname, len);
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
	  if (! print_cxx_classname (out, "class ", jcf, jcf->this_class))
	    {
	      fprintf (stderr, "class is of array type\n");
	      found_error = 1;
	      return;
	    }
	  if (jcf->super_class)
	    {
	      if (! print_cxx_classname (out, " : public ", 
					 jcf, jcf->super_class))
		{
		  fprintf (stderr, "base class is of array type\n");
		  found_error = 1;
		  return;
		}
	    }

	  fputs ("\n{\n", out);
	}
    }

  /* Now go back for second pass over methods and fields.  */
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

	  if (! stubs)
	    fputs ("};\n", out);

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

#define OPT_classpath LONG_OPT (0)
#define OPT_CLASSPATH LONG_OPT (1)
#define OPT_HELP      LONG_OPT (2)
#define OPT_TEMP      LONG_OPT (3)
#define OPT_VERSION   LONG_OPT (4)
#define OPT_PREPEND   LONG_OPT (5)
#define OPT_FRIEND    LONG_OPT (6)
#define OPT_ADD       LONG_OPT (7)
#define OPT_APPEND    LONG_OPT (8)
#define OPT_M         LONG_OPT (9)
#define OPT_MM        LONG_OPT (10)
#define OPT_MG        LONG_OPT (11)
#define OPT_MD        LONG_OPT (12)
#define OPT_MMD       LONG_OPT (13)

static struct option options[] =
{
  { "classpath", required_argument, NULL, OPT_classpath },
  { "CLASSPATH", required_argument, NULL, OPT_CLASSPATH },
  { "help",      no_argument,       NULL, OPT_HELP },
  { "stubs",     no_argument,       &stubs, 1 },
  { "td",        required_argument, NULL, OPT_TEMP },
  { "verbose",   no_argument,       NULL, 'v' },
  { "version",   no_argument,       NULL, OPT_VERSION },
  { "prepend",   required_argument, NULL, OPT_PREPEND },
  { "friend",    required_argument, NULL, OPT_FRIEND },
  { "add",       required_argument, NULL, OPT_ADD },
  { "append",    required_argument, NULL, OPT_APPEND },
  { "M",         no_argument,       NULL, OPT_M   },
  { "MM",        no_argument,       NULL, OPT_MM  },
  { "MG",        no_argument,       NULL, OPT_MG  },
  { "MD",        no_argument,       NULL, OPT_MD  },
  { "MMD",       no_argument,       NULL, OPT_MMD },
  { "jni",       no_argument,       &flag_jni, 1 },
  { NULL,        no_argument,       NULL, 0 }
};

static void
usage ()
{
  fprintf (stderr, "Try `gcjh --help' for more information.\n");
  exit (1);
}

static void
help ()
{
  printf ("Usage: gcjh [OPTION]... CLASS...\n\n");
  printf ("Generate C++ header files from .class files\n\n");
  printf ("  -stubs                  Generate an implementation stub file\n");
  printf ("  -jni                    Generate a JNI header or stub\n");
  printf ("\n");
  printf ("  -add TEXT               Insert TEXT into class body\n");
  printf ("  -append TEXT            Insert TEXT after class declaration\n");
  printf ("  -friend TEXT            Insert TEXT as `friend' declaration\n");
  printf ("  -prepend TEXT           Insert TEXT before start of class\n");
  printf ("\n");
  printf ("  --classpath PATH        Set path to find .class files\n");
  printf ("  --CLASSPATH PATH        Set path to find .class files\n");
  printf ("  -IDIR                   Append directory to class path\n");
  printf ("  -d DIRECTORY            Set output directory name\n");
  printf ("  -o FILE                 Set output file name\n");
  printf ("  -td DIRECTORY           Set temporary directory name\n");
  printf ("\n");
  printf ("  --help                  Print this help, then exit\n");
  printf ("  --version               Print version number, then exit\n");
  printf ("  -v, --verbose           Print extra information while running\n");
  printf ("\n");
  printf ("  -M                      Print all dependencies to stdout;\n");
  printf ("                             suppress ordinary output\n");
  printf ("  -MM                     Print non-system dependencies to stdout;\n");
  printf ("                             suppress ordinary output\n");
  printf ("  -MD                     Print all dependencies to stdout\n");
  printf ("  -MMD                    Print non-system dependencies to stdout\n");
  /* We omit -MG until it is implemented.  */
  printf ("\n");
  printf ("For bug reporting instructions, please see:\n");
  printf ("<URL:http://www.gnu.org/software/gcc/faq.html#bugreport>.\n");
  exit (0);
}

static void
version ()
{
  printf ("gcjh (%s)\n\n", version_string);
  printf ("Copyright (C) 1998, 1999 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n");
  printf ("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
  exit (0);
}

int
DEFUN(main, (argc, argv),
      int argc AND char** argv)
{
  JCF jcf;
  int argi;
  char *output_file = NULL;
  int emit_dependencies = 0, suppress_output = 0;
  int opt;

  if (argc <= 1)
    {
      fprintf (stderr, "gcjh: no classes specified\n");
      usage ();
    }

  jcf_path_init ();

  /* We use getopt_long_only to allow single `-' long options.  For
     some of our options this is more natural.  */
  while ((opt = getopt_long_only (argc, argv, "I:d:o:v", options, NULL)) != -1)
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

	case OPT_CLASSPATH:
	  jcf_path_CLASSPATH_arg (optarg);
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
	    prepend_specs = (char**) ALLOC (argc * sizeof (char*));
	  prepend_specs[prepend_count++] = optarg;
	  break;

	case OPT_FRIEND:
	  if (friend_count == 0)
	    friend_specs = (char**) ALLOC (argc * sizeof (char*));
	  friend_specs[friend_count++] = optarg;
	  break;

	case OPT_ADD:
	  if (add_count == 0)
	    add_specs = (char**) ALLOC (argc * sizeof (char*));
	  add_specs[add_count++] = optarg;
	  break;

	case OPT_APPEND:
	  if (append_count == 0)
	    append_specs = (char**) ALLOC (argc * sizeof (char*));
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
	  fprintf (stderr, "gcjh: `%s' option is unimplemented\n", argv[argi]);
	  exit (1);

	case OPT_MD:
	  emit_dependencies = 1;
	  jcf_dependency_init (1);
	  break;

	case OPT_MMD:
	  emit_dependencies = 1;
	  jcf_dependency_init (0);
	  break;

	default:
	  usage ();
	  break;
	}
    }

  if (optind == argc)
    {
      fprintf (stderr, "gcjh: no classes specified\n");
      usage ();
    }

  jcf_path_seal ();

  if (output_file && emit_dependencies)
    {
      fprintf (stderr, "gcjh: can't specify both -o and -MD\n");
      exit (1);
    }

  for (argi = optind; argi < argc; argi++)
    {
      char *classname = argv[argi];
      char *current_output_file;
      const char *classfile_name;

      if (verbose)
	fprintf (stderr, "Processing %s\n", classname);
      if (! output_file)
	jcf_dependency_reset ();
      classfile_name = find_class (classname, strlen (classname), &jcf, 0);
      if (classfile_name == NULL)
	{
	  fprintf (stderr, "%s: no such class\n", classname);
	  exit (1);
	}
      if (verbose)
	fprintf (stderr, "Found in %s\n", classfile_name);
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
	  current_output_file = (char*) ALLOC (dir_len + classname_length + 5);
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
