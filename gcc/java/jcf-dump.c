/* Program to dump out a Java(TM) .class file.
   Functionally similar to Sun's javap.

   Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

/*
  jcf-dump is a program to print out the contents of class files.
  Usage:  jcf-dump [FLAGS] CLASS
  Each CLASS is either:
  + the name of a class in the CLASSPATH (e.g "java.lang.String"), or
  + the name of a class *file* (e.g. "/home/me/work/package/Foo.class").
  + The name of a .zip or .jar file (which prints all the classes in the
  archive).

  OPTIONS:
  -c
	Dis-assemble each method.
  -classpath PATH
	Overrides $CLASSPATH.
  --print-main
	Print nothing if there is no valid "main" method;
	otherwise, print only the class name.
  --javap
	Print output in the style of Sun's javap program.  VERY UNFINISHED.
 */
    

#include "config.h"
#include "system.h"

#include "jcf.h"
#include "tree.h"
#include "java-tree.h"

/* Outout file. */
FILE *out;
/* Name of output file, if NULL if stdout. */
char *output_file = NULL;

int verbose = 0;

int flag_disassemble_methods = 0;
int flag_print_class_info = 1;
int flag_print_constant_pool = 1;
int flag_print_fields = 1;
int flag_print_methods = 1;
int flag_print_attributes = 1;

/* Print names of classes that have a "main" method. */
int flag_print_main = 0;

/* Index in constant pool of this class. */
int this_class_index = 0;

int class_access_flags = 0;

/* Print in format similar to javap.  VERY IMCOMPLETE. */
int flag_javap_compatible = 0;

static int print_access_flags PROTO ((FILE *, uint16, char));
static void print_constant_terse PROTO ((FILE*, JCF*, int, int));
static void print_constant PROTO ((FILE *, JCF *, int, int));
static void print_constant_ref PROTO ((FILE *, JCF *, int));
static void disassemble_method PROTO ((JCF*, unsigned char *, int));
static void print_name PROTO ((FILE*, JCF*, int));
static void print_signature PROTO ((FILE*, JCF*, int, int));
static int utf8_equal_string PROTO ((struct JCF*, int, const char *));
static int usage PROTO ((void));
static void process_class PROTO ((struct JCF *));

#define PRINT_SIGNATURE_RESULT_ONLY 1
#define PRINT_SIGNATURE_ARGS_ONLY 2

static int
DEFUN(utf8_equal_string, (jcf, index, value),
      JCF *jcf AND int index AND const char * value)
{
  if (CPOOL_INDEX_IN_RANGE (&jcf->cpool, index)
      && JPOOL_TAG (jcf, index) == CONSTANT_Utf8)
    {
      int len = strlen (value);
      if (JPOOL_UTF_LENGTH (jcf, index) == len
	  && memcmp (JPOOL_UTF_DATA (jcf, index), value, len) == 0)
	return 1;
    }
  return 0;
}

#define HANDLE_MAGIC(MAGIC, MINOR, MAJOR) \
  this_class_index = 0; \
  if (flag_print_class_info) \
    fprintf (out, \
             "Magic number: 0x%0lx, minor_version: %ld, major_version: %ld.\n",\
	     (long) MAGIC, (long) MINOR, (long) MAJOR)

#define HANDLE_START_CONSTANT_POOL(COUNT) \
  if (flag_print_constant_pool) \
    fprintf (out, "\nConstant pool (count: %d):\n", COUNT)

#define HANDLE_SOURCEFILE(INDEX) \
{ fprintf (out, "Attribute "); \
  print_constant_terse (out, jcf, attribute_name, CONSTANT_Utf8); \
  fprintf (out, ", length:%ld, #%d=", (long) attribute_length, INDEX); \
  print_constant_terse (out, jcf, INDEX, CONSTANT_Utf8); fputc ('\n', out); }

#define HANDLE_CLASS_INFO(ACCESS_FLAGS, THIS, SUPER, INTERFACES_COUNT) \
  this_class_index = THIS; \
  class_access_flags = ACCESS_FLAGS; \
  if (flag_print_class_info) \
    { fprintf (out, "\nAccess flags: 0x%x", ACCESS_FLAGS); \
      print_access_flags (out, ACCESS_FLAGS, 'c'); \
      fputc ('\n', out); \
      fprintf (out, "This class: "); \
      if (flag_print_constant_pool) \
        fprintf (out, "%d=", THIS); \
      print_constant_terse (out, jcf, THIS, CONSTANT_Class); \
      if (flag_print_constant_pool || SUPER != 0) \
        fprintf (out, ", super: "); \
      if (flag_print_constant_pool) \
        { \
          fprintf (out, "%d", SUPER); \
          if (SUPER != 0) \
            fputc ('=', out); \
        } \
      if (SUPER != 0) \
        print_constant_terse (out, jcf, SUPER, CONSTANT_Class); \
      fprintf (out, "\nInterfaces (count: %d):\n", INTERFACES_COUNT); \
    }

#define IGNORE_ATTRIBUTE(JCF, NAME, NAME_LENGTH) \
  (flag_print_attributes <= 0)

#define HANDLE_CLASS_INTERFACE(INDEX) \
  if (flag_print_class_info) \
    { fprintf (out, "- Implements: %d=", INDEX); \
      print_constant_terse (out, jcf, INDEX, CONSTANT_Class); \
      fputc ('\n', out); }

#define HANDLE_START_FIELDS(FIELDS_COUNT) \
  if (flag_print_fields) \
    fprintf (out, "\nFields (count: %d):\n", FIELDS_COUNT)

#define HANDLE_START_FIELD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
  if (flag_print_fields) \
    { fprintf (out, "Field name:"); \
      print_constant_terse (out, jcf, NAME, CONSTANT_Utf8); \
      print_access_flags (out, ACCESS_FLAGS, 'f'); \
      fprintf (out, " Signature: "); \
      if (flag_print_constant_pool) \
        fprintf (out, "%d=", SIGNATURE); \
      print_signature (out, jcf, SIGNATURE, 0); \
      fputc ('\n', out); } \
  else \
    flag_print_attributes--;

#define HANDLE_END_FIELD() \
  if (! flag_print_fields) \
    flag_print_attributes++;

#define HANDLE_START_METHODS(METHODS_COUNT) \
  if (flag_print_methods) \
    fprintf (out, "\nMethods (count: %d):\n", METHODS_COUNT); \
  else \
    flag_print_attributes--;


#define HANDLE_END_METHODS() \
  if (! flag_print_methods) \
    flag_print_attributes++;

#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
{ \
  if (flag_print_methods) \
    { \
      if (flag_javap_compatible) \
        { \
	  fprintf (out, "    "); \
	  print_access_flags (out, ACCESS_FLAGS, 'm'); \
	  fputc (' ', out); \
	  print_signature (out, jcf, SIGNATURE, PRINT_SIGNATURE_RESULT_ONLY); \
	  fputc (' ', out); \
	  print_constant_terse (out, jcf, NAME, CONSTANT_Utf8); \
	  print_signature (out, jcf, SIGNATURE, PRINT_SIGNATURE_ARGS_ONLY); \
	  fputc ('\n', out); \
	} \
      else \
	{ \
	  fprintf (out, "\nMethod name:"); \
	  print_constant_terse (out, jcf, NAME, CONSTANT_Utf8); \
	  print_access_flags (out, ACCESS_FLAGS, 'm'); \
	  fprintf (out, " Signature: "); \
	  if (flag_print_constant_pool) \
	    fprintf (out, "%d=", SIGNATURE); \
	  print_signature (out, jcf, SIGNATURE, 0); \
	  fputc ('\n', out); \
	} \
    } \
  if (flag_print_main && ACCESS_FLAGS == (ACC_STATIC|ACC_PUBLIC) \
      && utf8_equal_string (jcf, NAME, "main") \
      && utf8_equal_string (jcf, SIGNATURE, "([Ljava/lang/String;)V") \
      && this_class_index > 0 \
      && (class_access_flags & ACC_PUBLIC)) \
    { \
      print_constant_terse(out, jcf, this_class_index, CONSTANT_Class); \
      fputc  ('\n', out); \
   } \
}

#define COMMON_HANDLE_ATTRIBUTE(JCF, INDEX, LENGTH) \
( fprintf (out, "Attribute "), \
  print_constant_terse (out, jcf, INDEX, CONSTANT_Utf8), \
  fprintf (out, ", length:%ld", (long) LENGTH) )

#define HANDLE_CONSTANTVALUE(VALUE_INDEX) \
( COMMON_HANDLE_ATTRIBUTE(JCF, attribute_name, attribute_length), \
  fprintf (out, ", value: "), \
  print_constant_ref (out, jcf, VALUE_INDEX), \
  fprintf (out, "\n") )

#define HANDLE_CODE_ATTRIBUTE(MAX_STACK, MAX_LOCALS, CODE_LENGTH) \
{ COMMON_HANDLE_ATTRIBUTE(JCF, attribute_name, attribute_length); \
  fprintf (out, ", max_stack:%ld, max_locals:%ld, code_length:%ld\n", \
    (long) MAX_STACK, (long) MAX_LOCALS, (long) CODE_LENGTH); \
  disassemble_method (jcf, jcf->read_ptr, CODE_LENGTH); }

#define HANDLE_EXCEPTION_TABLE(ENTRIES, COUNT) \
  print_exception_table (jcf, ENTRIES, COUNT)

#define HANDLE_EXCEPTIONS_ATTRIBUTE(COUNT) \
{ int n = (COUNT); int i; \
  COMMON_HANDLE_ATTRIBUTE(JCF, attribute_name, attribute_length); \
  fprintf (out, ", count: %d\n", n); \
  for (i = 0; i < n; i++) {\
    int ex_index = JCF_readu2 (jcf); \
    fprintf (out, "%3d: ", i); \
    print_constant_ref (out, jcf, ex_index); \
    fputc ('\n', out); } }

#define HANDLE_LOCALVARIABLETABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); int i; \
  COMMON_HANDLE_ATTRIBUTE(JCF, attribute_name, attribute_length); \
  fprintf (out, ", count: %d\n", n); \
  for (i = 0; i < n; i++) {\
    int start_pc = JCF_readu2 (jcf); \
    int length = JCF_readu2 (jcf); \
    int name_index = JCF_readu2 (jcf); \
    int signature_index = JCF_readu2 (jcf); \
    int slot = JCF_readu2 (jcf); \
    fprintf (out, "  slot#%d: name: %d=", slot, name_index); \
    print_name (out, jcf, name_index); \
    fprintf (out, ", type: %d=", signature_index); \
    print_signature (out, jcf, signature_index, 0); \
    fprintf (out, " (pc: %d length: %d)\n", start_pc, length); }}

#define HANDLE_LINENUMBERTABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); int i; \
  COMMON_HANDLE_ATTRIBUTE(jcf, attribute_name, attribute_length); \
  fprintf (out, ", count: %d\n", n); \
  if (flag_disassemble_methods) \
    for (i = 0; i < n; i++) {\
      int start_pc = JCF_readu2 (jcf); \
      int line_number = JCF_readu2 (jcf); \
      fprintf (out, "  line: %d at pc: %d\n", line_number, start_pc); }\
  else \
    JCF_SKIP (jcf, 4 * n); }

#define PROCESS_OTHER_ATTRIBUTE(JCF, INDEX, LENGTH) \
{ COMMON_HANDLE_ATTRIBUTE(JCF, INDEX, LENGTH); \
  fputc ('\n', out); JCF_SKIP (JCF, LENGTH); }

#define START_FINAL_ATTRIBUTES(ATTRIBUTES_COUNT) \
  if (flag_print_attributes > 0) \
    fprintf (out, "\nAttributes (count: %d):\n", attributes_count);

#include "javaop.h"

static void
DEFUN(print_constant_ref, (stream, jcf, index),
      FILE *stream AND JCF *jcf AND int index)
{
  fprintf (stream, "#%d=<", index);
  if (index <= 0 || index >= JPOOL_SIZE(jcf))
    fprintf (stream, "out of range");
  else
    print_constant (stream, jcf, index, 1);
  fprintf (stream, ">");
}

/* Print the access flags given by FLAGS.
   The CONTEXT is one of 'c' (class flags), 'f' (field flags),
   or 'm' (method flags). */

static int
DEFUN (print_access_flags, (stream, flags, context),
       FILE *stream AND uint16 flags AND char context)
{
  if (flags & ACC_PUBLIC) fprintf (stream, " public");
  if (flags & ACC_PRIVATE) fprintf (stream, " private");
  if (flags & ACC_PROTECTED) fprintf (stream, " protected");
  if (flags & ACC_STATIC) fprintf (stream, " static");
  if (flags & ACC_FINAL) fprintf (stream, " final");
  if (flags & ACC_SYNCHRONIZED)
    {
      if (context == 'c')
	fprintf (stream, " super");
      else
	fprintf (stream, " synchronized");
    }
  if (flags & ACC_VOLATILE) fprintf (stream, " volatile");
  if (flags & ACC_TRANSIENT) fprintf (stream, " transient");
  if (flags & ACC_NATIVE) fprintf (stream, " native");
  if (flags & ACC_INTERFACE) fprintf (stream, " interface");
  if (flags & ACC_ABSTRACT) fprintf (stream, " abstract");
}


static void
DEFUN(print_name, (stream, jcf, name_index),
      FILE* stream AND JCF* jcf AND int name_index)
{
  if (JPOOL_TAG (jcf, name_index) != CONSTANT_Utf8)
    fprintf (stream, "<not a UTF8 constant>");
  else
    jcf_print_utf8 (stream, JPOOL_UTF_DATA (jcf,name_index),
		    JPOOL_UTF_LENGTH (jcf, name_index));
}

/* If the type of the constant at INDEX matches EXPECTED,
   print it tersely, otherwise more verbosely. */

static void
DEFUN(print_constant_terse, (out, jcf, index, expected),
      FILE *out AND JCF *jcf AND int index AND int expected)
{
  if (! CPOOL_INDEX_IN_RANGE (&jcf->cpool, index))
    fprintf (out, "<constant pool index %d not in range>", index);
  else if (JPOOL_TAG (jcf, index) != expected)
    {
      fprintf (out, "<Unexpected constant type ");
      print_constant (out, jcf, index, 1);
      fprintf (out, ">");
    }
  else
    print_constant (out, jcf, index, 0);
}

/* Print the constant at INDEX in JCF's constant pool.
   If verbosity==0, print very tersely (no extraneous text).
   If verbosity==1, prefix the type of the constant.
   If verbosity==2, add more descriptive text. */

static void
DEFUN(print_constant, (out, jcf, index, verbosity),
      FILE *out AND JCF *jcf AND int index AND int verbosity)
{
  int j, n;
  jlong num;
  const char *str;
  int kind = JPOOL_TAG (jcf, index);
  switch (kind)
    {
    case CONSTANT_Class:
      n = JPOOL_USHORT1 (jcf, index);
      if (verbosity > 0)
	fprintf (out, verbosity > 1 ? "Class name: %d=" : "Class ", n);
      if (! CPOOL_INDEX_IN_RANGE (&jcf->cpool, n))
	fprintf (out, "<out of range>");
      else if (verbosity < 2 && JPOOL_TAG (jcf, n) == CONSTANT_Utf8)
	{
	  int len = JPOOL_UTF_LENGTH (jcf, n);
	  jcf_print_utf8_replace (out, JPOOL_UTF_DATA(jcf,n), len, '/', '.');
	}
      else
	print_constant_terse (out, jcf, n, CONSTANT_Utf8);
      break;
    case CONSTANT_Fieldref:
      str = "Field"; goto field_or_method;
    case CONSTANT_Methodref:
      str = "Method"; goto field_or_method;
    case CONSTANT_InterfaceMethodref:
      str = "InterfaceMethod"; goto field_or_method;
    field_or_method:
      {
	uint16 tclass = JPOOL_USHORT1 (jcf, index);
	uint16 name_and_type = JPOOL_USHORT2 (jcf, index);
	if (verbosity == 2)
	  fprintf (out, "%sref class: %d=", str, tclass);
	else if (verbosity > 0)
	    fprintf (out, "%s ", str);
	print_constant_terse (out, jcf, tclass, CONSTANT_Class);
	fprintf (out, verbosity < 2 ? "." : " name_and_type: %d=<",
		 name_and_type);
	print_constant_terse (out, jcf, name_and_type, CONSTANT_NameAndType);
	if (verbosity == 2)
	  fputc ('>', out);
      }
      break;
    case CONSTANT_String:
      j = JPOOL_USHORT1 (jcf, index);
      if (verbosity > 0)
	fprintf (out, verbosity > 1 ? "String %d=" : "String ", j);
      print_constant_terse (out, jcf, j, CONSTANT_Utf8);
      break;
    case CONSTANT_Integer:
      if (verbosity > 0)
	fprintf (out, "Integer ");
      num = JPOOL_INT (jcf, index);
      goto integer;
    case CONSTANT_Long:
      if (verbosity > 0)
	fprintf (out, "Long ");
      num = JPOOL_LONG (jcf, index);
      goto integer;
    integer:
      {
	char buffer[25];
	format_int (buffer, num, 10);
	fprintf (out, "%s", buffer);
	if (verbosity > 1)
	  {
	    format_uint (buffer, (uint64)num, 16);
	    fprintf (out, "=0x%s", buffer);
	  }
      }
      break;
    case CONSTANT_Float:
      {
	jfloat fnum = JPOOL_FLOAT (jcf, index);
	fprintf (out, "%s%.10g", verbosity > 1 ? "Float " : "", (double) fnum);
	if (verbosity > 1)
	  fprintf (out, ", bits = 0x%08lx", (long) (* (int32 *) &fnum));
	break;
      }
    case CONSTANT_Double:
      {
	jdouble dnum = JPOOL_DOUBLE (jcf, index);
	fprintf (out, "%s%.20g", verbosity > 1 ? "Double " : "", dnum);
	if (verbosity > 1)
	  {
	    int32 hi, lo;
	    hi = JPOOL_UINT (jcf, index);
	    lo = JPOOL_UINT (jcf, index + 1);
	    fprintf (out, ", bits = 0x%08lx%08lx", (long) hi, (long) lo);
	  }
	break;
      }
    case CONSTANT_NameAndType:
      {
	uint16 name = JPOOL_USHORT1 (jcf, index);
	uint16 sig = JPOOL_USHORT2 (jcf, index);
	if (verbosity > 0)
	  fprintf (out, verbosity > 1 ? "%s name: %d=" : "%s ",
		   "NameAndType", name);
	print_name (out, jcf, name);
	if (verbosity <= 1)
	  fputc (' ', out);
	else
	  fprintf (out, ", signature: %d=", sig);
	print_signature (out, jcf, sig, 0);
      }
      break;
    case CONSTANT_Utf8:
      {
	register unsigned char *str = JPOOL_UTF_DATA (jcf, index);
	int length = JPOOL_UTF_LENGTH (jcf, index);
	if (verbosity > 0)
	  { /* Print as 8-bit bytes. */
	    fputs ("Utf8: \"", out);
	    while (--length >= 0)
	      jcf_print_char (out, *str++);
	  }
	else
	  { /* Print as Unicode. */
	    fputc ('\"', out);
	    jcf_print_utf8 (out, str, length);
	  }
	fputc ('\"', out);
      }
      break;
    default:
      fprintf (out, "(Unknown constant type %d)", kind);
    }
}

void
DEFUN(print_constant_pool, (jcf),
      JCF *jcf)
{
  int i;
  for (i = 1; i < JPOOL_SIZE(jcf); i++)
    {
      int kind = JPOOL_TAG (jcf, i);
      fprintf (out, "#%d: ", i);
      print_constant (out, jcf, i, 2);
      fprintf (out, "\n");
      if (kind == CONSTANT_Double || kind == CONSTANT_Long)
	i++; /* These take up two slots in the constant table */
    }
}

static void
DEFUN(print_signature_type, (stream, ptr, limit),
     FILE* stream AND const unsigned char **ptr AND const unsigned char *limit)
{
  int array_size;
  if ((*ptr) >= limit)
    return;
  switch (*(*ptr))
    {
    case '[':
      array_size = -1;
      for ((*ptr)++; (*ptr) < limit && ISDIGIT (**ptr); (*ptr)++)
	{
	  array_size = (array_size < 0 ? 0 : 10 * array_size) + *(*ptr) - '0';
	}
      print_signature_type (stream, ptr, limit);
      if (array_size == -1)
	fprintf (stream, "[]");
      else
	fprintf (stream, "[%d]", array_size);
      break;
    case '(':
      {
	int nargs = 0;
	fputc (*(*ptr)++, stream);
	for (; **ptr != ')' && *ptr < limit; nargs++)
	  {
	    if (nargs > 0)
	      fputc (',', stream);
	    print_signature_type (stream, ptr, limit);
	  }
	if (*ptr < limit)
	  {
	    fputc (*(*ptr)++, stream);
	    print_signature_type (stream, ptr, limit);
	  }
	else
	  fprintf (stream, "???");
      }
    break;
      
    case 'B':  fprintf (stream, "byte");  (*ptr)++;  break;
    case 'C':  fprintf (stream, "char");  (*ptr)++;  break;
    case 'D':  fprintf (stream, "double");  (*ptr)++;  break;
    case 'F':  fprintf (stream, "float");  (*ptr)++;  break;
    case 'S':  fprintf (stream, "short");  (*ptr)++;  break;
    case 'I':  fprintf (stream, "int");  (*ptr)++;  break;
    case 'J':  fprintf (stream, "long");  (*ptr)++;  break;
    case 'Z':  fprintf (stream, "boolean");  (*ptr)++;  break;
    case 'V':  fprintf (stream, "void");  (*ptr)++;  break;

    case 'L':
      for ((*ptr)++; (*ptr)<limit && *(*ptr) != ';'; (*ptr)++)
	jcf_print_char (stream, *(*ptr) == '/' ? '.' : *(*ptr));
      if (*(*ptr) == ';')
	(*ptr)++;
      break;
    default:
      jcf_print_char (stream, *(*ptr)++);
    }
}

static void
DEFUN(print_signature, (stream, jcf, signature_index, int options),
      FILE* stream AND JCF *jcf AND int signature_index AND int options)
{
  if (JPOOL_TAG (jcf, signature_index) != CONSTANT_Utf8)
    print_constant_terse (out, jcf, signature_index, CONSTANT_Utf8);
  else
    {
      const unsigned char *str = JPOOL_UTF_DATA (jcf, signature_index);
      int length = JPOOL_UTF_LENGTH (jcf, signature_index);
      const unsigned char *limit;
      limit = str + length;
      if (str >= limit)
	fprintf (stream, "<empty signature string>");
      else
	{
	  if (options & PRINT_SIGNATURE_RESULT_ONLY)
	    {
	      while (str < limit && *str++ != ')') ;
	    }
	  if (options & PRINT_SIGNATURE_ARGS_ONLY)
	    {
	      str++;
	      fputc ('(', stream);
	      while (str < limit && *str != ')')
		{
		  print_signature_type (stream, &str, limit);
		  if (*str != ')')
		    fputs (", ", stream);
		}
	      fputc (')', stream);
	    }
	  else
	    {
	      print_signature_type (stream, &str, limit);
	      if (str < limit)
		{
		  fprintf (stream, "<junk:");
		  jcf_print_utf8 (stream, str, limit - str);
		  fputc ('>', stream);
		}
	    }
	}
    }
}


static void
DEFUN(print_exception_table, (jcf, entries, count),
      JCF *jcf AND unsigned char *entries AND int count)
{
  /* Print exception table. */
  int i = count;
  if (i > 0)
    {
      unsigned char *ptr = entries;
      fprintf (out, "Exceptions (count: %d):\n", i);
      for (; --i >= 0;  ptr+= 8)
	{
	  int start_pc = GET_u2 (ptr);
	  int end_pc = GET_u2 (ptr+2);
	  int handler_pc = GET_u2 (ptr+4);
	  int catch_type = GET_u2 (ptr+6);
	  fprintf (out, "  start: %d, end: %d, handler: %d, type: %d",
		   start_pc, end_pc, handler_pc, catch_type);
	  if (catch_type == 0)
	    fputs (" /* finally */", out);
	  else
	    {
	      fputc('=', out);
	      print_constant_terse (out, jcf, catch_type, CONSTANT_Class);
	    }
	  fputc ('\n', out);
	}
    }
}

#include "jcf-reader.c"

static int
DEFUN (usage, (), )
{
  fprintf (stderr, "Usage: jcf-dump [-o outputfile] [-c] classname\n");
  exit(1);
}

static void
DEFUN(process_class, (jcf),
      JCF *jcf)
{
  int code;
  if (jcf_parse_preamble (jcf) != 0)
    fprintf (stderr, "Not a valid Java .class file.\n");    

  /* Parse and possibly print constant pool */
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    {
      fprintf (stderr, "error while parsing constant pool\n");
      exit (FATAL_EXIT_CODE);
    }
  code = verify_constant_pool (jcf);
  if (code > 0)
    {
      fprintf (stderr, "error in constant pool entry #%d\n", code);
      exit (FATAL_EXIT_CODE);
    }
  if (flag_print_constant_pool)
    print_constant_pool (jcf);

  jcf_parse_class (jcf);
  code = jcf_parse_fields (jcf);
  if (code != 0)
    {
      fprintf (stderr, "error while parsing fields\n");
      exit (FATAL_EXIT_CODE);
    }
  code = jcf_parse_methods (jcf);
  if (code != 0)
    {
      fprintf (stderr, "error while parsing methods\n");
      exit (FATAL_EXIT_CODE);
    }
  code = jcf_parse_final_attributes (jcf);
  if (code != 0)
    {
      fprintf (stderr, "error while parsing final attributes\n");
      exit (FATAL_EXIT_CODE);
    }
  jcf->filename = NULL;
}

int
DEFUN(main, (argc, argv),
      int argc AND char** argv)
{
  JCF jcf[1];
  int argi;
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

      if (strcmp (arg, "-o") == 0 && argi + 1 < argc)
	output_file = argv[++argi];
      else if (strcmp (arg, "-classpath") == 0 && argi + 1 < argc)
	jcf_path_classpath_arg (argv[++argi]);
      else if (strcmp (arg, "-CLASSPATH") == 0 && argi + 1 < argc)
	jcf_path_CLASSPATH_arg (argv[++argi]);
      else if (strncmp (arg, "-I", 2) == 0)
	jcf_path_include_arg (arg + 2);
      else if (strcmp (arg, "-verbose") == 0)
	verbose++;
      else if (strcmp (arg, "-print-main") == 0)
	flag_print_main++;
      else if (strcmp (arg, "-c") == 0)
	flag_disassemble_methods++;
      else if (strcmp (arg, "-javap") == 0)
	{
	  flag_javap_compatible++;
	  flag_print_constant_pool = 0;
	}
      else
	{
	  fprintf (stderr, "%s: illegal argument\n", argv[argi]);
	  exit (FATAL_EXIT_CODE);
	}
    }

  if (argi == argc)
    usage ();

  jcf_path_seal ();

  if (flag_print_main)
    {
      flag_print_fields = 0;
      flag_print_methods = 0;
      flag_print_constant_pool = 0;
      flag_print_attributes = 0;
      flag_print_class_info = 0;
    }

  if (output_file)
    {
      out = fopen (output_file, "w");
      if (out)
	{
	  fprintf (stderr, "Cannot open '%s' for output.\n", output_file);
	  exit (FATAL_EXIT_CODE);
	}
    }
  else
    out = stdout;

  if (argi >= argc)
    {
      fprintf (out, "Reading .class from <standard input>.\n");
#if JCF_USE_STDIO
      open_class ("<stdio>", jcf, stdin, NULL);
#else
      open_class ("<stdio>", jcf, 0, NULL);
#endif
      process_class (jcf);
    }
  else
    {
      for (; argi < argc; argi++)
	{
	  char *arg = argv[argi];
	  char* class_filename = find_class (arg, strlen (arg), jcf, 0);
	  if (class_filename == NULL)
	    class_filename = find_classfile (arg, jcf, NULL);
	  if (class_filename == NULL)
	    {
	      perror ("Could not find class");
	      exit (FATAL_EXIT_CODE);
	    }
	  JCF_FILL (jcf, 4);
	  if (GET_u4 (jcf->read_ptr) == ZIPMAGIC)
	    {
	      long compressed_size, member_size;
	      int compression_method, filename_length, extra_length;
	      int general_purpose_bits;
	      char *filename;
	      int total_length;
	      if (flag_print_class_info)
		fprintf (out, "Reading classes from archive %s.\n",
			 class_filename);
	      for (;;)
		{
		  int skip = 0;
		  jcf_filbuf_t save_filbuf = jcf->filbuf;
		  long magic = JCF_readu4_le (jcf);
		  if (magic == 0x02014b50 || magic == 0x06054b50)
		    break;  /* got to central directory */
		  if (magic != 0x04034b50) /* ZIPMAGIC (little-endian) */
		    {
		      fprintf (stderr, "bad format of .zip/.jar archive\n");
		      exit (FATAL_EXIT_CODE);
		    }
		  JCF_FILL (jcf, 26);
		  JCF_SKIP (jcf, 2);
		  general_purpose_bits = JCF_readu2_le (jcf);
		  compression_method = JCF_readu2_le (jcf);
		  JCF_SKIP (jcf, 8);
		  compressed_size = JCF_readu4_le (jcf);
		  member_size = JCF_readu4_le (jcf);
		  filename_length = JCF_readu2_le (jcf);
		  extra_length = JCF_readu2_le (jcf);
		  total_length = filename_length + extra_length
		    + compressed_size;
		  if (jcf->read_end - jcf->read_ptr < total_length)
		    jcf_trim_old_input (jcf);
		  JCF_FILL (jcf, total_length);
		  filename = jcf->read_ptr;
		  JCF_SKIP (jcf, filename_length);
		  JCF_SKIP (jcf, extra_length);
		  if (filename_length > 0
		      && filename[filename_length-1] == '/')
		    {
		      if (flag_print_class_info)
			fprintf (out, "[Skipping directory %.*s]\n",
				 filename_length, filename);
		      skip = 1;
		    }
		  else if (compression_method != 0)
		    {
		      if (flag_print_class_info)
			fprintf (out, "[Skipping compressed file %.*s]\n",
				 filename_length, filename);
		      skip = 1;
		    }
		  else if (member_size < 4
			   || GET_u4 (jcf->read_ptr) != 0xcafebabe)
		    {
		      if (flag_print_class_info)
			fprintf (out, "[Skipping non-.class member %.*s]\n",
				 filename_length, filename);
		      skip = 1;
		    }
		  else
		    {
		      if (flag_print_class_info)
			fprintf (out, "Reading class member: %.*s.\n",
				 filename_length, filename);
		    }
		  if (skip)
		    {
		      JCF_SKIP (jcf, compressed_size);
		    }
		  else
		    {
		      unsigned char *save_end;
		      jcf->filbuf = jcf_unexpected_eof;
		      save_end = jcf->read_end;
		      jcf->read_end = jcf->read_ptr + compressed_size;
		      process_class (jcf);
		      jcf->filbuf = save_filbuf;
		      jcf->read_end = save_end;
		    }
		}
	    }
	  else
	    {
	      if (flag_print_class_info)
		fprintf (out, "Reading .class from %s.\n", class_filename);
	      process_class (jcf);
	    }
	  JCF_FINISH(jcf);
	}
    }

  exit (SUCCESS_EXIT_CODE);
}

static void
DEFUN(disassemble_method, (jcf, byte_ops, len),
      JCF* jcf AND unsigned char *byte_ops AND int len)
{
#undef AND /* Causes problems with opcodes for iand and land. */
#undef PTR
  int PC;
  int i;
  int saw_wide = 0;
  if (flag_disassemble_methods == 0)
    return;
#define BCODE byte_ops
  for (PC = 0; PC < len;)
    {
      int oldpc = PC;
      int saw_index;
      jint INT_temp;
      switch (byte_ops[PC++])
	{

/* This is the actual code emitted for each of opcodes in javaops.def.
   The actual opcode-specific stuff is handled by the OPKIND macro.
   I.e. for an opcode whose OPKIND is BINOP, the BINOP will be called.
   Those macros are defiend below.  The OPKINDs that do not have any
   inline parameters (such as BINOP) and therefore do mot need anything
   else to me printed out just use an empty body. */

#define JAVAOP(OPNAME, OPCODE, OPKIND, OPERAND_TYPE, OPERAND_VALUE) \
        case OPCODE: \
	  fprintf (out, "%3d: %s", oldpc, #OPNAME); \
	  OPKIND(OPERAND_TYPE, OPERAND_VALUE); \
	  fputc ('\n', out); \
	  break;

#define CONST_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define CONST_INDEX_2 (saw_index = 1, IMMEDIATE_u2)
#define VAR_INDEX_1 (saw_index = 1, IMMEDIATE_u1)
#define VAR_INDEX_2 (saw_index = 1, IMMEDIATE_u2)

#define CHECK_PC_IN_RANGE(PC) (PC < 0 || PC > len ? \
  (fprintf(stderr, "Bad byte codes.\n"), exit(-1)) : 1)

/* Print out operand (if not implied by the opcode) for PUSCH opcodes.
   These all push a constant onto the opcode stack. */
#define PUSHC(OPERAND_TYPE, OPERAND_VALUE) \
  saw_index = 0, i = (OPERAND_VALUE); \
  if (oldpc+1 == PC) /* nothing */; \
  else if (saw_index) fprintf (out, " "), print_constant_ref (out, jcf, i); \
  else fprintf (out, " %d", i);

/* Print out operand (a local variable index) for LOAD opcodes.
   These all push local variable onto the opcode stack. */
#define LOAD(OPERAND_TYPE, OPERAND_VALUE) \
  INT_temp = saw_wide ? IMMEDIATE_u2 : (OPERAND_VALUE); goto load_store;

/* Handle STORE opcodes same as LOAD opcodes.
   These all store a value from the opcode stack in a local variable. */
#define STORE LOAD

/* Handle more kind of opcodes. */
#define STACK(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define UNOP(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define BINOP(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define CONVERT(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define CONVERT2(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define RETURN(OPERAND_TYPE, OPERAND_VALUE) /* nothing */
#define UNKNOWN(OPERAND_TYPE, OPERAND_VALUE) /* nothing */

/* Handle putfield and getfield opcodes, with static versions. */
#define FIELD(MAYBE_STATIC, PUT_OR_GET) \
  fputc (' ', out); print_constant_ref (out, jcf, IMMEDIATE_u2)

/* Print operand for invoke opcodes. */
#define INVOKE(OPERAND_TYPE, OPERAND_VALUE) \
  fputc (' ', out); print_constant_ref (out, jcf, IMMEDIATE_u2);\
  if (OPERAND_VALUE) /* for invokeinterface */ \
  { int nargs = IMMEDIATE_u1;  PC++; \
    fprintf (out, " nargs:%d", nargs); }

#define OBJECT(OPERAND_TYPE, OPERAND_VALUE) \
  fputc (' ', out); print_constant_ref (out, jcf, IMMEDIATE_u2);

#define ARRAY(OPERAND_TYPE, SUBOP) \
  ARRAY_##SUBOP(OPERAND_TYPE)
/* Handle sub-categories of ARRAY opcodes. */
#define ARRAY_LOAD(TYPE) /* nothing */
#define ARRAY_STORE(TYPE) /* nothing */
#define ARRAY_LENGTH(TYPE) /* nothing */
#define ARRAY_NEW(TYPE) ARRAY_NEW_##TYPE
#define ARRAY_NEW_NUM \
 INT_temp = IMMEDIATE_u1; \
 { char *str; \
  switch (INT_temp) {  \
    case  4: str = "boolean"; break; \
    case  5: str = "char"; break; \
    case  6: str = "float"; break; \
    case  7: str = "double"; break; \
    case  8: str = "byte"; break; \
    case  9: str = "short"; break; \
    case 10: str = "int"; break; \
    case 11: str = "long"; break; \
    default: str = "<unknown type code %d>"; break; \
  } \
  fputc (' ', out); fprintf (out, str, INT_temp); }

#define ARRAY_NEW_PTR  \
  fputc (' ', out); print_constant_ref (out, jcf, IMMEDIATE_u2);

#define ARRAY_NEW_MULTI \
  fputc (' ', out); print_constant_ref (out, jcf, IMMEDIATE_u2); \
  fprintf (out, " %d", IMMEDIATE_u1); /* number of dimensions */

#define TEST(OPERAND_TYPE, OPERAND_VALUE) \
  fprintf (out, " %d", oldpc + IMMEDIATE_s2)

#define BRANCH(OPERAND_TYPE, OPERAND_VALUE) \
  saw_index = 0, INT_temp = (OPERAND_VALUE); \
  fprintf (out, " %ld", (long) (saw_index ? INT_temp : oldpc + INT_temp))

#define JSR(OPERAND_TYPE, OPERAND_VALUE) \
  saw_index = 0, INT_temp = (OPERAND_VALUE); \
  fprintf (out, " %ld", (long) (saw_index ? INT_temp : oldpc + INT_temp))

#undef RET /* Defined by config/i386/i386.h */
#define RET(OPERAND_TYPE, OPERAND_VALUE) \
  INT_temp = saw_wide ? IMMEDIATE_u2 : (OPERAND_VALUE); \
  saw_wide = 0; \
  fprintf (out, " %ld", (long) INT_temp);

#define SWITCH(OPERAND_TYPE, TABLE_OR_LOOKUP) \
  PC = (PC + 3) / 4 * 4; TABLE_OR_LOOKUP##_SWITCH

#define LOOKUP_SWITCH \
  { jint default_offset = IMMEDIATE_s4;  jint npairs = IMMEDIATE_s4; \
    fprintf (out, " npairs=%ld, default=%ld", (long) npairs, (long) default_offset+oldpc); \
    while (--npairs >= 0) { \
     jint match = IMMEDIATE_s4; jint offset = IMMEDIATE_s4; \
     fprintf (out, "\n%10ld: %ld", (long)match, (long)(offset+oldpc)); } \
  }

#define TABLE_SWITCH \
  { jint default_offset = IMMEDIATE_s4; \
    jint low = IMMEDIATE_s4; jint high = IMMEDIATE_s4; \
    fprintf (out, " low=%ld, high=%ld, default=%ld", \
      (long) low, (long) high, (long) default_offset+oldpc); \
    for (; low <= high; low++) { \
     jint offset = IMMEDIATE_s4; \
     fprintf (out, "\n%10ld: %ld", (long)low, (long)(offset+oldpc)); } \
  }

#define SPECIAL(OPERAND_TYPE, OPERAND_VALUE) \
  SPECIAL_##OPERAND_VALUE(OPERAND_TYPE)

#define SPECIAL_IINC(OPERAND_TYPE) \
  i = saw_wide ? IMMEDIATE_u2 : IMMEDIATE_u1; \
  fprintf (out, " %d", i); \
  INT_temp = saw_wide ? IMMEDIATE_s2 : IMMEDIATE_s1; \
  saw_wide = 0; \
  fprintf (out, " %d", i)

#define SPECIAL_WIDE(OPERAND_TYPE) \
  saw_wide = 1;

#define SPECIAL_EXIT(OPERAND_TYPE) /* nothing */
#define SPECIAL_ENTER(OPERAND_TYPE) /* nothing */
#define SPECIAL_BREAK(OPERAND_TYPE) /* nothing */
#define SPECIAL_THROW(OPERAND_TYPE) /* nothing */

#define IMPL(OPERAND_TYPE, OPERAND_VALUE) \
  fprintf (out, " %d", IMMEDIATE_u##OPERAND_VALUE)

#define COND(OPERAND_TYPE, OPERAND_VALUE) \
   TEST(OPERAND_TYPE, OPERAND_VALUE)

#include "javaop.def"

	load_store:
	  if (oldpc+1 == PC) /* nothing - local index implied by opcode */;
	  else
	    {
	      saw_wide = 0;
	      fprintf (out, " %ld", (long) INT_temp);
	    }
	  fputc ('\n', out);
	  break;

	default:
	  fprintf (out, "%3d: unknown(%3d)\n", oldpc, byte_ops[PC]);
	}
    }
}
