/* Program to write C++-suitable header files from a Java(TM) .class
   file.  This is similar to SUN's javah.

Copyright (C) 1996, 1998 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "jcf.h"
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <math.h>

#include <string.h>

/* The output file.  */
FILE *out = NULL;

/* Nonzero on failure.  */
static int found_error = 0;

/* Directory to place resulting files in. Set by -d option. */
char *output_directory = "";

/* Directory to place temporary file.  Set by -td option.  Currently unused. */
char *temp_directory = "/tmp";

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
struct JCF *main_jcf;

/* This holds access information for the last field we examined.  They
   let us generate "private:", "public:", and "protected:" properly.
   If 0 then we haven't previously examined any field.  */
static JCF_u2 last_access;

#define ACC_VISIBILITY (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED)

/* We keep a linked list of all method names we have seen.  This lets
   us determine if a method name and a field name are in conflict.  */
struct method_name
{
  unsigned char *name;
  int length;
  struct method_name *next;
};

/* List of method names we've seen.  */
static struct method_name *method_name_list;

static void print_field_info PROTO ((FILE *, JCF*, int, int, JCF_u2));
static void print_method_info PROTO ((FILE *, JCF*, int, int, JCF_u2));
static void print_c_decl PROTO ((FILE*, JCF*, int, int, JCF_u2, int, char *));

JCF_u2 current_field_name;
JCF_u2 current_field_value;
JCF_u2 current_field_signature;
JCF_u2 current_field_flags;

#define HANDLE_START_FIELD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
( current_field_name = (NAME), current_field_signature = (SIGNATURE), \
  current_field_flags = (ACCESS_FLAGS), current_field_value = 0)

/* We pass over fields twice.  The first time we just note the start
   of the methods.  Then we go back and parse the fields for real.
   This is ugly.  */
static int field_pass;

#define HANDLE_END_FIELD() \
  if (out && field_pass) print_field_info (out, jcf, current_field_name, \
					   current_field_signature, \
					   current_field_flags);

#define HANDLE_CONSTANTVALUE(VALUEINDEX) current_field_value = (VALUEINDEX)

#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
  if (out) print_method_info (out, jcf, NAME, SIGNATURE, ACCESS_FLAGS)

#include "jcf-reader.c"

/* Some useful constants.  */
#define F_NAN_MASK 0x7f800000
#define D_NAN_MASK 0x7ff0000000000000LL

/* Return 1 if F is not Inf or NaN.  */
static int
java_float_finite (f)
     jfloat f;
{
  int32 *ip = (int32 *) &f;

  /* We happen to know that F_NAN_MASK will match all NaN values, and
     also positive and negative infinity.  That's why we only need one
     test here.  See The Java Language Specification, section 20.9.  */
  return (*ip & F_NAN_MASK) != F_NAN_MASK;
}

/* Return 1 if D is not Inf or NaN.  */
static int
java_double_finite (d)
     jdouble d;
{
  int64 *ip = (int64 *) &d;

  /* Now check for all NaNs.  */
  return (*ip & D_NAN_MASK) != D_NAN_MASK;
}

void
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
  unsigned char *s, *p, *limit;

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
     unsigned char *str;
     int length;
     char *name;
{
  unsigned char *limit = str + length;
  int i;

  for (i = 0; name[i]; ++i)
    {
      int ch = UTF8_GET (str, limit);
      if (ch != name[i])
	return 1;
    }

  return str != limit;
}

/* Generate an access control keyword based on FLAGS.  Returns 0 if
   FLAGS matches the saved access information, nonzero otherwise.  */

static void
generate_access (stream, flags)
     FILE *stream;
     JCF_u2 flags;
{
  /* FIXME: Java's "protected" and "no access specifier" modes don't
     actually map to C++ "protected".  That's how we map them for now,
     though.  */

  if (! (flags & ACC_VISIBILITY))
    flags = ACC_PROTECTED;

  if ((flags & ACC_VISIBILITY) == last_access)
    return;
  last_access = (flags & ACC_VISIBILITY);

  switch (last_access)
    {
    case ACC_PUBLIC:
      fputs ("public:\n", stream);
      break;
    case ACC_PRIVATE:
      fputs ("private:\n", stream);
      break;
    case ACC_PROTECTED:
      fputs ("protected:\n", stream);
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
     unsigned char *name;
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

static void
DEFUN(print_field_info, (stream, jcf, name_index, sig_index, flags),
      FILE *stream AND JCF* jcf
      AND int name_index AND int sig_index AND JCF_u2 flags)
{
  char *override = NULL;

  if (flags & ACC_FINAL)
    {
      if (current_field_value > 0)
	{
	  jlong num;
	  char buffer[25];

	  generate_access (stream, flags);
	  switch (JPOOL_TAG (jcf, current_field_value))
	    {
	    case CONSTANT_Integer:
	      fputs ("  static const jint ", out);
	      print_name (out, jcf, name_index);
	      fputs (" = ", out);
	      num = JPOOL_INT (jcf, current_field_value);
	      format_int (buffer, num, 10);
	      fprintf (out, "%sL;\n", buffer);
	      break;
	    case CONSTANT_Long:
	      fputs ("  static const jlong ", out);
	      print_name (out, jcf, name_index);
	      fputs (" = ", out);
	      num = JPOOL_LONG (jcf, current_field_value);
	      format_int (buffer, num, 10);
	      fprintf (out, "%sLL;\n", buffer);
	      break;
	    case CONSTANT_Float:
	      {
		jfloat fnum = JPOOL_FLOAT (jcf, current_field_value);
		fputs ("  static const jfloat ", out);
		print_name (out, jcf, name_index);
		if (! java_float_finite (fnum))
		  fputs (";\n", out);
		else
		  fprintf (out, " = %.10g;\n",  fnum);
	      }
	      break;
	    case CONSTANT_Double:
	      {
		jdouble dnum = JPOOL_DOUBLE (jcf, current_field_value);
		fputs ("  static const jdouble ", out);
		print_name (out, jcf, name_index);
		if (! java_double_finite (dnum))
		  fputs (";\n", out);
		else
		  fprintf (out, " = %.17g;\n",  dnum);
	      }
	      break;
	    default:
	      fprintf(out, " <<inappropriate constant type>>\n");
	    }

	  return;
	}
    }

  generate_access (stream, flags);
  fputs ("  ", out);
  if ((flags & ACC_STATIC))
    fputs ("static ", out);

  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
    }
  else
    {
      unsigned char *name = JPOOL_UTF_DATA (jcf, name_index);
      int length = JPOOL_UTF_LENGTH (jcf, name_index);

      if (name_is_method_p (name, length))
	{
	  /* This field name matches a method.  So override the name
	     with a dummy name.  This is yucky, but it isn't clear
	     what else to do.  FIXME: if the field is static, then
	     we'll be in real trouble.  */
	  if ((flags & ACC_STATIC))
	    {
	      fprintf (stderr, "static field has same name as method\n");
	      found_error = 1;
	    }

	  override = (char *) malloc (length + 3);
	  memcpy (override, name, length);
	  strcpy (override + length, "__");
	}
    }

  print_c_decl (out, jcf, name_index, sig_index, flags, 0, override);
  fputs (";\n", out);

  if (override)
    free (override);
}

static void
DEFUN(print_method_info, (stream, jcf, name_index, sig_index, flags),
      FILE *stream AND JCF* jcf
      AND int name_index AND int sig_index AND JCF_u2 flags)
{
  unsigned char *str;
  int length, is_init = 0;
  char *override = NULL;

  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    fprintf (stream, "<not a UTF8 constant>");
  str = JPOOL_UTF_DATA (jcf, name_index);
  length = JPOOL_UTF_LENGTH (jcf, name_index);
  if (str[0] == '<')
    {
      /* Ignore internally generated methods like <clinit>.  However,
	 treat <init> as a constructor.  */
      if (! utf8_cmp (str, length, "<init>"))
	is_init = 1;
      else  
	return;
    }
  else
    {
      struct method_name *nn;

      nn = (struct method_name *) malloc (sizeof (struct method_name));
      nn->name = (char *) malloc (length);
      memcpy (nn->name, str, length);
      nn->length = length;
      nn->next = method_name_list;
      method_name_list = nn;
    }

  /* We can't generate a method whose name is a C++ reserved word.
     For now the only problem has been `delete'; add more here as
     required.  We can't just ignore the function, because that will
     cause incorrect code to be generated if the function is virtual
     (not only for calls to this function for for other functions
     after it in the vtbl).  So we give it a dummy name instead.  */
  if (! utf8_cmp (str, length, "delete"))
    {
      /* If the method is static, we can safely skip it.  If we don't
	 skip it then we'll have problems since the mangling will be
	 wrong.  FIXME.  */
      if ((flags & ACC_STATIC))
	return;
      override = "__dummy_delete";
    }

  generate_access (stream, flags);

  fputs ("  ", out);
  if ((flags & ACC_STATIC))
    fputs ("static ", out);
  else if (! (flags & ACC_FINAL) && ! (jcf->access_flags & ACC_FINAL))
    {
      /* Don't print `virtual' if we have a constructor.  */
      if (! is_init)
	fputs ("virtual ", out);
    }
  print_c_decl (out, jcf, name_index, sig_index, flags, is_init, override);

  /* FIXME: it would be nice to decompile small methods here.  That
     would allow for inlining.  */

  fprintf(out, ";\n");
}

/* Print one piece of a signature.  Returns pointer to next parseable
   character on success, NULL on error.  */
static unsigned char *
decode_signature_piece (stream, signature, limit, need_space)
     FILE *stream;
     unsigned char *signature, *limit;
     int *need_space;
{
  char *ctype;

  switch (signature[0])
    {
    case '[':
      for (signature++; (signature < limit
			 && *signature >= '0'
			 && *signature <= '9'); signature++)
	;
      switch (*signature)
	{
	case 'B': ctype = "jbyteArray";  goto printit;
	case 'C': ctype = "jcharArray";  goto printit;
	case 'D': ctype = "jdoubleArray";  goto printit;
	case 'F': ctype = "jfloatArray";  goto printit;
	case 'I': ctype = "jintArray";  goto printit;
	case 'S': ctype = "jshortArray";  goto printit;
	case 'J': ctype = "jlongArray";  goto printit;
	case 'Z': ctype = "jbooleanArray";  goto printit;
	case '[': ctype = "jobjectArray"; goto printit;
	case 'L':
	  /* We have to generate a reference to JArray here,
	     so that our output matches what the compiler
	     does.  */
	  ++signature;
	  fputs ("JArray<", stream);
	  while (signature < limit && *signature != ';')
	    {
	      int ch = UTF8_GET (signature, limit);
	      if (ch == '/')
		fputs ("::", stream);
	      else
		jcf_print_char (stream, ch);
	    }
	  fputs (" *> *", stream);
	  *need_space = 0;
	  ++signature;
	  break;
	default:
	  /* Unparseable signature.  */
	  return NULL;
	}
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
      jcf_print_char (stream, *signature++);
      break;
    printit:
      signature++;
      *need_space = 1;
      fputs (ctype, stream);
      break;
    }

  return signature;
}

static void
DEFUN(print_c_decl, (stream, jcf, name_index, signature_index, flags, is_init,
		     name_override),
      FILE* stream AND JCF* jcf
      AND int name_index AND int signature_index AND JCF_u2 flags
      AND int is_init AND char *name_override)
{
  if (JPOOL_TAG (jcf, signature_index) != CONSTANT_Utf8)
    {
      fprintf (stream, "<not a UTF8 constant>");
      found_error = 1;
    }
  else
    {
      int length = JPOOL_UTF_LENGTH (jcf, signature_index);
      unsigned char *str0 = JPOOL_UTF_DATA (jcf, signature_index);
      register  unsigned char *str = str0;
      unsigned char *limit = str + length;
      int need_space = 0;
      int is_method = str[0] == '(';
      unsigned char *next;

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

      if (is_method)
	{
	  /* Have a method or a constructor.  Print signature pieces
	     until done.  */
	  fputs (" (", stream);
	  str = str0 + 1;
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
}

void
DEFUN(print_mangled_classname, (stream, jcf, prefix, index),
      FILE *stream AND JCF *jcf AND char *prefix AND int index)
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
     char *prefix;
     JCF *jcf;
     int index;
{
  int name_index = JPOOL_USHORT1 (jcf, index);
  int len, c;
  unsigned char *s, *p, *limit;

  s = JPOOL_UTF_DATA (jcf, name_index);
  len = JPOOL_UTF_LENGTH (jcf, name_index);
  limit = s + len;

  /* Explicitly omit arrays here.  */
  p = s;
  c = UTF8_GET (p, limit);
  if (c == '[')
    return 0;

  fputs (prefix, stream);
  while (s < limit)
    {
      c = UTF8_GET (s, limit);
      if (c == '/')
	fputs ("::", stream);
      else
	jcf_print_char (stream, c);
    }

  return 1;
}

int written_class_count = 0;

/* Return name of superclass.  If LEN is not NULL, fill it with length
   of name.  */
static unsigned char *
super_class_name (derived_jcf, len)
     JCF *derived_jcf;
     int *len;
{
  int supername_index = JPOOL_USHORT1 (derived_jcf, derived_jcf->super_class);
  int supername_length = JPOOL_UTF_LENGTH (derived_jcf, supername_index);
  unsigned char *supername = JPOOL_UTF_DATA (derived_jcf, supername_index);

  if (len)
    *len = supername_length;

  return supername;
}

/* Print declarations for all classes required by this class.  FIXME:
   the current implementation just prints every class name from the
   constant pool.  This is too much.  We really only need to print a
   declaration for each class which is the type of a return value, a
   field, or an argument.  */
static void
print_class_decls (out, jcf)
     FILE *out;
     JCF *jcf;
{
  int i, seen_one = 0;

  for (i = 1; i < JPOOL_SIZE (jcf); ++i)
    {
      int kind = JPOOL_TAG (jcf, i);
      if (kind == CONSTANT_Class)
	{
	  if (print_cxx_classname (out, "class ", jcf, i))
	    fputs (";\n", out);
	  seen_one = 1;
	}
    }

  if (seen_one)
    fputs ("\n", out);
}

static void
DEFUN(process_file, (jcf, out),
      JCF *jcf AND FILE *out)
{
  int code, i;
  uint32 field_start, method_end;

  current_jcf = main_jcf = jcf;

  last_access = 0;

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
    fputs ("// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-\n\n",
	   out);

  if (out)
    {
      print_mangled_classname (out, jcf, "#ifndef __", jcf->this_class);
      fprintf (out, "__\n");

      print_mangled_classname (out, jcf, "#define __", jcf->this_class);
      fprintf (out, "__\n\n");
    }

  if (jcf->super_class && out)
    {
      int super_length;
      unsigned char *supername = super_class_name (jcf, &super_length);

      fputs ("#include <", out);
      jcf_print_utf8 (out, supername, super_length);
      fputs (".h>\n", out);

      /* FIXME: If our superclass is Object, then we include
	 java-array.h.  The right thing to do here is look at all the
	 methods and fields and see if an array is in use.  Only then
	 would we need to include java-array.h.  */
      if (! utf8_cmp (supername, super_length, "java/lang/Object"))
	fputs ("#include <java-array.h>\n", out);

      fputs ("\n", out);
    }

  if (out)
    {
      print_class_decls (out, jcf);

      for (i = 0; i < prepend_count; ++i)
	fprintf (out, "%s\n", prepend_specs[i]);
      if (prepend_count > 0)
	fputc ('\n', out);
    }

  if (out && ! print_cxx_classname (out, "class ", jcf, jcf->this_class))
    {
      fprintf (stderr, "class is of array type\n");
      found_error = 1;
      return;
    }
  if (out && jcf->super_class)
    {
      if (! print_cxx_classname (out, " : public ", jcf, jcf->super_class))
	{
	  fprintf (stderr, "base class is of array type\n");
	  found_error = 1;
	  return;
	}
    }
  if (out)
    fputs ("\n{\n", out);

  /* We make a single pass over the file, printing methods and fields
     as we see them.  We have to list the methods in the same order
     that they appear in the class file, so that the Java and C++
     vtables have the same layout.  */
  /* We want to parse the methods first.  But we need to find where
     they start.  So first we skip the fields, then parse the
     methods.  Then we parse the fields and skip the methods.  FIXME:
     this is ugly.  */
  field_pass = 0;
  field_start = JCF_TELL (jcf);
  jcf_parse_fields (jcf);

  jcf_parse_methods (jcf);
  method_end = JCF_TELL (jcf);

  field_pass = 1;
  JCF_SEEK (jcf, field_start);
  jcf_parse_fields (jcf);
  JCF_SEEK (jcf, method_end);

  jcf_parse_final_attributes (jcf);

  if (out)
    {
      /* Generate friend decl if we still must.  */
      for (i = 0; i < friend_count; ++i)
	fprintf (out, "  friend %s\n", friend_specs[i]);

      /* Generate extra declarations.  */
      if (add_count > 0)
	fputc ('\n', out);
      for (i = 0; i < add_count; ++i)
	fprintf (out, "  %s\n", add_specs[i]);

      fputs ("};\n", out);

      if (append_count > 0)
	fputc ('\n', out);
      for (i = 0; i < append_count; ++i)
	fprintf (out, "%s\n", append_specs[i]);

      print_mangled_classname (out, jcf, "\n#endif /* __", jcf->this_class);
      fprintf (out, "__ */\n");
    }
}

static void
usage ()
{
  fprintf (stderr, "gcjh: no classes specified\n");
  exit (1);
}

static void
help ()
{
  printf ("Usage: gcjh [OPTION]... CLASS...\n\n");
  printf ("Generate C++ header files from .class files\n\n");
  printf ("  --classpath PATH        Set path to find .class files\n");
  printf ("  --CLASSPATH PATH        Set path to find .class files\n");
  printf ("  -IDIR                   Append directory to class path\n");
  printf ("  -d DIRECTORY            Set output directory name\n");
  printf ("  --help                  Print this help, then exit\n");
  printf ("  -o FILE                 Set output file name\n");
  printf ("  -td DIRECTORY           Set temporary directory name\n");
  printf ("  -v, --verbose           Print extra information while running\n");
  printf ("  --version               Print version number, then exit\n");
  /* FIXME: print bug-report information.  */
  exit (0);
}

static void
java_no_argument (opt)
     char *opt;
{
  fprintf (stderr, "gcjh: no argument given for option `%s'\n", opt);
  exit (1);
}

static void
version ()
{
  /* FIXME: use version.c?  */
  printf ("gcjh (GNU gcc) 0.0\n\n");
  printf ("Copyright (C) 1998 Free Software Foundation, Inc.\n");
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

  if (argc <= 1)
    usage ();

  jcf_path_init ();

  for (argi = 1; argi < argc; argi++)
    {
      char *arg = argv[argi];

      if (arg[0] != '-' || ! strcmp (arg, "--"))
	break;

      /* Just let all arguments be given in either "-" or "--" form.  */
      if (arg[1] == '-')
	++arg;

      if (strcmp (arg, "-o") == 0)
	{
	  if (argi + 1 < argc)
	    output_file = argv[++argi];
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-d") == 0)
	{
	  if (argi + 1 < argc)
	    output_directory = argv[++argi];
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-td") == 0)
	{
	  if (argi + 1 < argc)
	    temp_directory = argv[++argi];
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-prepend") == 0)
	{
	  if (argi + 1 < argc)
	    {
	      if (prepend_count == 0)
		prepend_specs = (char**) ALLOC ((argc-argi) * sizeof (char*));
	      prepend_specs[prepend_count++] = argv[++argi];
	    }
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-friend") == 0)
	{
	  if (argi + 1 < argc)
	    {
	      if (friend_count == 0)
		friend_specs = (char**) ALLOC ((argc-argi) * sizeof (char*));
	      friend_specs[friend_count++] = argv[++argi];
	    }
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-add") == 0)
	{
	  if (argi + 1 < argc)
	    {
	      if (add_count == 0)
		add_specs = (char**) ALLOC ((argc-argi) * sizeof (char*));
	      add_specs[add_count++] = argv[++argi];
	    }
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-append") == 0)
	{
	  if (argi + 1 < argc)
	    {
	      if (append_count == 0)
		append_specs = (char**) ALLOC ((argc-argi) * sizeof (char*));
	      append_specs[append_count++] = argv[++argi];
	    }
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-classpath") == 0)
	{
	  if (argi + 1 < argc)
	    jcf_path_classpath_arg (argv[++argi]);
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strcmp (arg, "-CLASSPATH") == 0)
	{
	  if (argi + 1 < argc)
	    jcf_path_CLASSPATH_arg (argv[++argi]);
	  else
	    java_no_argument (argv[argi]);
	}
      else if (strncmp (arg, "-I", 2) == 0)
	jcf_path_include_arg (arg + 2);
      else if (strcmp (arg, "-verbose") == 0 || strcmp (arg, "-v") == 0)
	verbose++;
      else if (strcmp (arg, "-stubs") == 0)
	stubs++;
      else if (strcmp (arg, "-help") == 0)
	help ();
      else if (strcmp (arg, "-version") == 0)
	version ();
      else if (strcmp (arg, "-M") == 0)
	{
	  emit_dependencies = 1;
	  suppress_output = 1;
	  jcf_dependency_init (1);
	}
      else if (strcmp (arg, "-MM") == 0)
	{
	  emit_dependencies = 1;
	  suppress_output = 1;
	  jcf_dependency_init (0);
	}
      else if (strcmp (arg, "-MG") == 0)
	{
	  fprintf (stderr, "gcjh: `%s' option is unimplemented\n", argv[argi]);
	  exit (1);
	}
      else if (strcmp (arg, "-MD") == 0)
	{
	  emit_dependencies = 1;
	  jcf_dependency_init (1);
	}
      else if (strcmp (arg, "-MMD") == 0)
	{
	  emit_dependencies = 1;
	  jcf_dependency_init (0);
	}
      else
	{
	  fprintf (stderr, "%s: illegal argument\n", argv[argi]);
	  exit (1);
	}
    }

  if (argi == argc)
    usage ();

  jcf_path_seal ();

  if (output_file && emit_dependencies)
    {
      fprintf (stderr, "gcjh: can't specify both -o and -MD\n");
      exit (1);
    }

  for (; argi < argc; argi++)
    {
      char *classname = argv[argi];
      char *classfile_name, *current_output_file;

      if (verbose)
	fprintf (stderr, "Processing %s\n", classname);
      if (! output_file)
	jcf_dependency_reset ();
      classfile_name = find_class (classname, strlen (classname), &jcf, 1);
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
	  current_output_file = (char*) ALLOC (dir_len + classname_length + 4);
	  strcpy (current_output_file, output_directory);
	  if (dir_len > 0 && output_directory[dir_len-1] != '/')
	    current_output_file[dir_len++] = '/';
	  for (i = 0; classname[i] != '\0'; i++)
	    {
	      char ch = classname[i];
	      if (ch == '.')
		ch = '/';
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
	  strcpy (current_output_file + dir_len, ".h");
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

/* TODO:

 * Do whatever the javah -stubs flag does.

 * Emit "structure forward declarations" when needed.

 * Generate C headers, like javah

 */
