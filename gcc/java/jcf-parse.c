/* Parser for Java(TM) .class files.
   Copyright (C) 1996, 1998, 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
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

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "java-except.h"
#include "input.h"
#include "java-tree.h"
#include "toplev.h"
#include "parse.h"

/* A CONSTANT_Utf8 element is converted to an IDENTIFIER_NODE at parse time. */
#define JPOOL_UTF(JCF, INDEX) CPOOL_UTF(&(JCF)->cpool, INDEX)
#define JPOOL_UTF_LENGTH(JCF, INDEX) IDENTIFIER_LENGTH (JPOOL_UTF (JCF, INDEX))
#define JPOOL_UTF_DATA(JCF, INDEX) \
  ((const unsigned char *) IDENTIFIER_POINTER (JPOOL_UTF (JCF, INDEX)))
#define HANDLE_CONSTANT_Utf8(JCF, INDEX, LENGTH) \
  do { \
    unsigned char save;  unsigned char *text; \
    JCF_FILL (JCF, (LENGTH)+1); /* Make sure we read 1 byte beyond string. */ \
    text = (JCF)->read_ptr; \
    save = text[LENGTH]; \
    text[LENGTH] = 0; \
    (JCF)->cpool.data[INDEX] = (jword) get_identifier (text); \
    text[LENGTH] = save; \
    JCF_SKIP (JCF, LENGTH); } while (0)

#include "jcf.h"

extern struct obstack *saveable_obstack;
extern struct obstack temporary_obstack;
extern struct obstack permanent_obstack;

/* The class we are currently processing. */
tree current_class = NULL_TREE;

/* The class we started with. */
tree main_class = NULL_TREE;

/* List of all class DECL seen so far.  */
tree all_class_list = NULL_TREE;

/* The FIELD_DECL for the current field.  */
static tree current_field = NULL_TREE;

/* The METHOD_DECL for the current method.  */
static tree current_method = NULL_TREE;

/* The Java .class file that provides main_class;  the main input file. */
static struct JCF main_jcf[1];

/* Declarations of some functions used here.  */
static tree give_name_to_class PROTO ((JCF *jcf, int index));
static void parse_zip_file_entries PROTO ((void));
static void process_zip_dir PROTO ((void));
static void parse_source_file PROTO ((tree));
static void jcf_parse_source PROTO ((void));
static int jcf_figure_file_type PROTO ((JCF *));
static int find_in_current_zip PROTO ((const char *, struct JCF **));
static void parse_class_file PROTO ((void));
static void set_source_filename PROTO ((JCF *, int));
static int predefined_filename_p PROTO ((tree));

/* Handle "SourceFile" attribute. */

static void
set_source_filename (jcf, index)
     JCF *jcf;
     int index;
{
  tree sfname_id = get_name_constant (jcf, index);
  char *sfname = IDENTIFIER_POINTER (sfname_id);
  if (input_filename != NULL)
    {
      int old_len = strlen (input_filename);
      int new_len = IDENTIFIER_LENGTH (sfname_id);
      /* Use the current input_filename (derived from the class name)
	 if it has a directory prefix, but otherwise matches sfname. */
      if (old_len > new_len
	  && strcmp (sfname, input_filename + old_len - new_len) == 0
	  && (input_filename[old_len - new_len - 1] == '/'
	      || input_filename[old_len - new_len - 1] == '\\'))
	return;
    }
  input_filename = sfname;
  DECL_SOURCE_FILE (TYPE_NAME (current_class)) = sfname;
  if (current_class == main_class) main_input_filename = input_filename;
}

#define HANDLE_SOURCEFILE(INDEX) set_source_filename (jcf, INDEX)

#define HANDLE_CLASS_INFO(ACCESS_FLAGS, THIS, SUPER, INTERFACES_COUNT) \
{ tree super_class = SUPER==0 ? NULL_TREE : get_class_constant (jcf, SUPER); \
  current_class = give_name_to_class (jcf, THIS); \
  set_super_info (ACCESS_FLAGS, current_class, super_class, INTERFACES_COUNT);}

#define HANDLE_CLASS_INTERFACE(INDEX) \
  add_interface (current_class, get_class_constant (jcf, INDEX))

#define HANDLE_START_FIELD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
{ int sig_index = SIGNATURE; \
  current_field = add_field (current_class, get_name_constant (jcf, NAME), \
			     parse_signature (jcf, sig_index), ACCESS_FLAGS); \
 set_java_signature (TREE_TYPE (current_field), JPOOL_UTF (jcf, sig_index)); }

#define HANDLE_END_FIELDS() \
  (current_field = NULL_TREE)

#define HANDLE_CONSTANTVALUE(INDEX) \
{ tree constant;  int index = INDEX; \
  if (! flag_emit_class_files && JPOOL_TAG (jcf, index) == CONSTANT_String) { \
    tree name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index)); \
    constant = build_utf8_ref (name); \
  } \
  else \
    constant = get_constant (jcf, index); \
  set_constant_value (current_field, constant); }

#define HANDLE_METHOD(ACCESS_FLAGS, NAME, SIGNATURE, ATTRIBUTE_COUNT) \
 (current_method = add_method (current_class, ACCESS_FLAGS, \
			       get_name_constant (jcf, NAME), \
			       get_name_constant (jcf, SIGNATURE)), \
  DECL_LOCALVARIABLES_OFFSET (current_method) = 0, \
  DECL_LINENUMBERS_OFFSET (current_method) = 0)

#define HANDLE_END_METHODS() \
{ tree handle_type = CLASS_TO_HANDLE_TYPE (current_class); \
  if (handle_type != current_class) layout_type (handle_type); }

#define HANDLE_CODE_ATTRIBUTE(MAX_STACK, MAX_LOCALS, CODE_LENGTH) \
{ DECL_MAX_STACK (current_method) = (MAX_STACK); \
  DECL_MAX_LOCALS (current_method) = (MAX_LOCALS); \
  DECL_CODE_LENGTH (current_method) = (CODE_LENGTH); \
  DECL_CODE_OFFSET (current_method) = JCF_TELL (jcf); }

#define HANDLE_LOCALVARIABLETABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); \
  DECL_LOCALVARIABLES_OFFSET (current_method) = JCF_TELL (jcf) - 2; \
  JCF_SKIP (jcf, n * 10); }

#define HANDLE_LINENUMBERTABLE_ATTRIBUTE(COUNT) \
{ int n = (COUNT); \
  DECL_LINENUMBERS_OFFSET (current_method) = JCF_TELL (jcf) - 2; \
  JCF_SKIP (jcf, n * 4); }

#define HANDLE_EXCEPTIONS_ATTRIBUTE(COUNT) \
{ \
  int n = COUNT; \
  tree list = DECL_FUNCTION_THROWS (current_method); \
  while (--n >= 0) \
    { \
      tree thrown_class = get_class_constant (jcf, JCF_readu2 (jcf)); \
      list = tree_cons (NULL_TREE, thrown_class, list); \
    } \
  DECL_FUNCTION_THROWS (current_method) = nreverse (list); \
}

#include "jcf-reader.c"

static int yydebug;

tree
parse_signature (jcf, sig_index)
     JCF *jcf;
     int sig_index;
{
  if (sig_index <= 0 || sig_index >= JPOOL_SIZE(jcf)
      || JPOOL_TAG (jcf, sig_index) != CONSTANT_Utf8)
    fatal ("invalid field/method signature");
  else
    {
      return parse_signature_string (JPOOL_UTF_DATA (jcf, sig_index),
				     JPOOL_UTF_LENGTH (jcf, sig_index));
    }
}

void
init_lex ()
{
  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));
}

void
set_yydebug (value)
     int value;
{
  yydebug = value;
}

tree
get_constant (jcf, index)
  JCF *jcf;
  int index;
{
  tree value;
  int tag;
  if (index <= 0 || index >= JPOOL_SIZE(jcf))
    goto bad;
  tag = JPOOL_TAG (jcf, index);
  if ((tag & CONSTANT_ResolvedFlag) || tag == CONSTANT_Utf8)
    return (tree) jcf->cpool.data[index];
  push_obstacks (&permanent_obstack, &permanent_obstack);
  switch (tag)
    {
    case CONSTANT_Integer:
      {
	jint num = JPOOL_INT(jcf, index);
	value = build_int_2 (num, num < 0 ? -1 : 0);
	TREE_TYPE (value) = int_type_node;
	break;
      }
    case CONSTANT_Long:
      {
	jint num = JPOOL_INT (jcf, index);
	HOST_WIDE_INT lo, hi;
	lshift_double (num, 0, 32, 64, &lo, &hi, 0);
	num = JPOOL_INT (jcf, index+1);
	add_double (lo, hi, num, 0, &lo, &hi);
	value = build_int_2 (lo, hi);
	TREE_TYPE (value) = long_type_node;
	force_fit_type (value, 0);
	break;
      }
#if TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
    case CONSTANT_Float:
      {
	jint num = JPOOL_INT(jcf, index);
	REAL_VALUE_TYPE d;
#ifdef REAL_ARITHMETIC
	d = REAL_VALUE_FROM_TARGET_SINGLE (num);
#else
	union { float f;  jint i; } u;
	u.i = num;
	d = u.f;
#endif
	value = build_real (float_type_node, d);
	break;
      }
    case CONSTANT_Double:
      {
	HOST_WIDE_INT num[2];
	REAL_VALUE_TYPE d;
	HOST_WIDE_INT lo, hi;
	num[0] = JPOOL_INT (jcf, index);
	lshift_double (num[0], 0, 32, 64, &lo, &hi, 0);
	num[0] = JPOOL_INT (jcf, index+1);
	add_double (lo, hi, num[0], 0, &lo, &hi);
	if (FLOAT_WORDS_BIG_ENDIAN)
	  {
	    num[0] = hi;
	    num[1] = lo;
	  }
	else
	  {
	    num[0] = lo;
	    num[1] = hi;
	  }
#ifdef REAL_ARITHMETIC
	d = REAL_VALUE_FROM_TARGET_DOUBLE (num);
#else
	{
	  union { double d;  jint i[2]; } u;
	  u.i[0] = (jint) num[0];
	  u.i[1] = (jint) num[1];
	  d = u.d;
	}
#endif
	value = build_real (double_type_node, d);
	break;
      }
#endif /* TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT */
    case CONSTANT_String:
      {
	extern struct obstack *expression_obstack;
	tree name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index));
	const char *utf8_ptr = IDENTIFIER_POINTER (name);
	unsigned char *str_ptr;
	int utf8_len = IDENTIFIER_LENGTH (name);
	const unsigned char *str = (const unsigned char *)utf8_ptr;
	int i = utf8_len;
	int str_len;

	/* Count the number of Unicode characters in the string,
	   while checking for a malformed Utf8 string. */
	for (str_len = 0; i > 0; str_len++)
	  {
	    int char_len = UT8_CHAR_LENGTH (*str);
	    if (char_len < 0 || char_len > 2 || char_len > i)
	      fatal ("bad string constant");
	    str += char_len;
	    i -= char_len;
	  }

	value = make_node (STRING_CST);
	TREE_TYPE (value) = build_pointer_type (string_type_node);
	TREE_STRING_LENGTH (value) = 2 * str_len;
	TREE_STRING_POINTER (value)
	  = obstack_alloc (expression_obstack, 2 * str_len);
	str_ptr = (unsigned char *) TREE_STRING_POINTER (value);
	str = (const unsigned char *)utf8_ptr;
	for (i = 0; i < str_len; i++)
	  {
	    int char_value;
	    int char_len = UT8_CHAR_LENGTH (*str);
	    switch (char_len)
	      {
	      case 1:
		char_value = *str++;
		break;
	      case 2:
		char_value = *str++ & 0x1F;
		char_value = (char_value << 6) | (*str++ & 0x3F);
		break;
	      case 3:
		char_value = *str_ptr++ & 0x0F;
		char_value = (char_value << 6) | (*str++ & 0x3F);
		char_value = (char_value << 6) | (*str++ & 0x3F);
		break;
	      default:
		goto bad;
	      }
	    if (BYTES_BIG_ENDIAN)
	      {
		*str_ptr++ = char_value >> 8;
		*str_ptr++ = char_value & 0xFF;
	      }
	    else
	      {
		*str_ptr++ = char_value & 0xFF;
		*str_ptr++ = char_value >> 8;
	      }
	  }
      }
      break;
    default:
      goto bad;
    }
  pop_obstacks ();
  JPOOL_TAG(jcf, index) = tag | CONSTANT_ResolvedFlag;
  jcf->cpool.data [index] = (jword) value;
  return value;
 bad:
  fatal ("bad value constant type %d, index %d", 
	 JPOOL_TAG( jcf, index ), index);
}

tree
get_name_constant (jcf, index)
  JCF *jcf;
  int index;
{
  tree name = get_constant (jcf, index);
  if (TREE_CODE (name) != IDENTIFIER_NODE)
    fatal ("bad nameandtype index %d", index);
  return name;
}

static tree
give_name_to_class (jcf, i)
     JCF *jcf;
     int i;
{
  if (i <= 0 || i >= JPOOL_SIZE(jcf)
      || JPOOL_TAG (jcf, i) != CONSTANT_Class)
    fatal ("bad class index %d", i);
  else
    {
      tree this_class;
      int j = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that j is a CONSTANT_Utf8. */
      tree class_name = unmangle_classname (JPOOL_UTF_DATA (jcf, j),
					    JPOOL_UTF_LENGTH (jcf, j));
      this_class = lookup_class (class_name);
      input_filename = DECL_SOURCE_FILE (TYPE_NAME (this_class));
      lineno = 0;
      if (main_input_filename == NULL && jcf == main_jcf)
	main_input_filename = input_filename;

      jcf->cpool.data[i] = (jword) this_class;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
      return this_class;
    }
}

/* Get the class of the CONSTANT_Class whose constant pool index is I. */

tree
get_class_constant (JCF *jcf , int i)
{
  tree type;
  if (i <= 0 || i >= JPOOL_SIZE(jcf)
      || (JPOOL_TAG (jcf, i) & ~CONSTANT_ResolvedFlag) != CONSTANT_Class)
    fatal ("bad class index %d", i);

  if (JPOOL_TAG (jcf, i) != CONSTANT_ResolvedClass)
    {
      int name_index = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that name_index is a CONSTANT_Utf8. */
      const char *name = JPOOL_UTF_DATA (jcf, name_index);
      int nlength = JPOOL_UTF_LENGTH (jcf, name_index);
      if (name[0] == '[')  /* Handle array "classes". */
	  type = TREE_TYPE (parse_signature_string (name, nlength));
      else
        { 
          tree cname = unmangle_classname (name, nlength);
          type = lookup_class (cname);
	}
      jcf->cpool.data[i] = (jword) type;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
    }
  else
    type = (tree) jcf->cpool.data[i];
  return type;
}

/* Read a class with the fully qualified-name NAME.
   Return 1 iff we read the requested file.
   (It is still possible we failed if the file did not
   define the class it is supposed to.) */

int
read_class (name)
     tree name;
{
  JCF this_jcf, *jcf;
  tree save_current_class = current_class;
  char *save_input_filename = input_filename;
  JCF *save_current_jcf = current_jcf;
  long saved_pos = 0;
  if (current_jcf->read_state)
    saved_pos = ftell (current_jcf->read_state);

  push_obstacks (&permanent_obstack, &permanent_obstack);

  /* Search in current zip first.  */
  if (find_in_current_zip (IDENTIFIER_POINTER (name), &jcf) == 0)
    {
      if (find_class (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name),
		      &this_jcf, 1) == 0)
	{
	  pop_obstacks ();	/* FIXME: one pop_obstack() per function */
	  return 0;
	}
      else
	{
	  this_jcf.seen_in_zip = 0;
	  current_jcf = &this_jcf;
	}
    }
  else
    current_jcf = jcf;

  if (current_jcf->java_source)
    jcf_parse_source ();
  else {
    java_parser_context_save_global ();
    java_push_parser_context ();
    input_filename = current_jcf->filename;
    jcf_parse (current_jcf);
    java_pop_parser_context (0);
    java_parser_context_restore_global ();
  }

  if (!current_jcf->seen_in_zip)
    JCF_FINISH (current_jcf);
  pop_obstacks ();

  current_class = save_current_class;
  input_filename = save_input_filename;
  current_jcf = save_current_jcf;
  if (current_jcf->read_state)
    fseek (current_jcf->read_state, saved_pos, SEEK_SET);
  return 1;
}

/* Load CLASS_OR_NAME. CLASS_OR_NAME can be a mere identifier if
   called from the parser, otherwise it's a RECORD_TYPE node. If
   VERBOSE is 1, print error message on failure to load a class. */

/* Replace calls to load_class by having callers call read_class directly
   - and then perhaps rename read_class to load_class.  FIXME */

void
load_class (class_or_name, verbose)
     tree class_or_name;
     int verbose;
{
  tree name;

  /* class_or_name can be the name of the class we want to load */
  if (TREE_CODE (class_or_name) == IDENTIFIER_NODE)
    name = class_or_name;
  /* In some cases, it's a dependency that we process earlier that
     we though */
  else if (TREE_CODE (class_or_name) == TREE_LIST)
    name = TYPE_NAME (TREE_PURPOSE (class_or_name));
  /* Or it's a type in the making */
  else
    name = DECL_NAME (TYPE_NAME (class_or_name));

  if (read_class (name) == 0 && verbose)
    {
      error ("Cannot find file for class %s.",
	     IDENTIFIER_POINTER (name));
      if (TREE_CODE (class_or_name) == RECORD_TYPE)
	TYPE_SIZE (class_or_name) = error_mark_node;
#if 0
      /* FIXME: what to do here?  */
      if (!strcmp (classpath, DEFAULT_CLASS_PATH))
	fatal ("giving up");
#endif
      return;
    }
}

/* Parse a source file when JCF refers to a source file.  */

static void
jcf_parse_source ()
{
  tree file;

  java_parser_context_save_global ();
  java_push_parser_context ();
  input_filename = current_jcf->filename;
  file = get_identifier (input_filename);
  if (!HAS_BEEN_ALREADY_PARSED_P (file))
    {
      if (!(finput = fopen (input_filename, "r")))
	fatal ("input file `%s' just disappeared - jcf_parse_source",
	       input_filename);
      parse_source_file (file);
      if (fclose (finput))
	fatal ("can't close input file `%s' stream - jcf_parse_source",
	       input_filename);
    }
  java_pop_parser_context (IS_A_COMMAND_LINE_FILENAME_P (file));
  java_parser_context_restore_global ();
}

/* Parse the .class file JCF. */

void
jcf_parse (jcf)
     JCF* jcf;
{
  int i, code;

  if (jcf_parse_preamble (jcf) != 0)
    fatal ("Not a valid Java .class file.\n");
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    fatal ("error while parsing constant pool");
  code = verify_constant_pool (jcf);
  if (code > 0)
    fatal ("error in constant pool entry #%d\n", code);

  jcf_parse_class (jcf);
  if (main_class == NULL_TREE)
    main_class = current_class;
  if (! quiet_flag && TYPE_NAME (current_class))
    fprintf (stderr, " %s %s",
	     (jcf->access_flags & ACC_INTERFACE) ? "interface" : "class", 
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))));
  if (CLASS_LOADED_P (current_class))
    return;
  CLASS_LOADED_P (current_class) = 1;

  for (i = 1; i < JPOOL_SIZE(jcf); i++)
    {
      switch (JPOOL_TAG (jcf, i))
	{
	case CONSTANT_Class:
	  get_class_constant (jcf, i);
	  break;
	}
    }
  
  code = jcf_parse_fields (jcf);
  if (code != 0)
    fatal ("error while parsing fields");
  code = jcf_parse_methods (jcf);
  if (code != 0)
    fatal ("error while parsing methods");
  code = jcf_parse_final_attributes (jcf);
  if (code != 0)
    fatal ("error while parsing final attributes");

  /* The fields of class_type_node are already in correct order. */
  if (current_class != class_type_node && current_class != object_type_node)
    TYPE_FIELDS (current_class) = nreverse (TYPE_FIELDS (current_class));

  push_obstacks (&permanent_obstack, &permanent_obstack);
  layout_class (current_class);
  if (current_class == object_type_node)
    layout_class_methods (object_type_node);
  else
    all_class_list = tree_cons (NULL_TREE, 
				TYPE_NAME (current_class), all_class_list );
  pop_obstacks ();
}

void
init_outgoing_cpool ()
{
  current_constant_pool_data_ref = NULL_TREE; 
  if (outgoing_cpool == NULL)
    {
      static CPool outgoing_cpool_buffer;
      outgoing_cpool = &outgoing_cpool_buffer;
      CPOOL_INIT(outgoing_cpool);
    }
  else
    {
      CPOOL_REINIT(outgoing_cpool);
    }
}

static void
parse_class_file ()
{
  tree method;
  char *save_input_filename = input_filename;
  int save_lineno = lineno;

  java_layout_seen_class_methods ();

  input_filename = DECL_SOURCE_FILE (TYPE_NAME (current_class));
  lineno = 0;
  debug_start_source_file (input_filename);
  init_outgoing_cpool ();

  for ( method = TYPE_METHODS (CLASS_TO_HANDLE_TYPE (current_class));
	method != NULL_TREE; method = TREE_CHAIN (method))
    {
      JCF *jcf = current_jcf;

      if (METHOD_NATIVE (method) || METHOD_ABSTRACT (method))
	continue;

      if (DECL_CODE_OFFSET (method) == 0)
	{
	  error ("missing Code attribute");
	  continue;
	}

      lineno = 0;
      if (DECL_LINENUMBERS_OFFSET (method))
	{
	  register int i;
	  register unsigned char *ptr;
	  JCF_SEEK (jcf, DECL_LINENUMBERS_OFFSET (method));
	  linenumber_count = i = JCF_readu2 (jcf);
	  linenumber_table = ptr = jcf->read_ptr;

	  for (ptr += 2; --i >= 0; ptr += 4)
	    {
	      int line = GET_u2 (ptr);
	      /* Set initial lineno lineno to smallest linenumber.
	       * Needs to be set before init_function_start. */
	      if (lineno == 0 || line < lineno)
		lineno = line;
	    }  
	}
      else
	{
	  linenumber_table = NULL;
	  linenumber_count = 0;
	}

      start_java_method (method);

      give_name_to_locals (jcf);

      /* Actually generate code. */
      expand_byte_code (jcf, method);

      end_java_method ();
    }

  if (flag_emit_class_files)
    write_classfile (current_class);

  finish_class ();

  debug_end_source_file (save_lineno);
  input_filename = save_input_filename;
  lineno = save_lineno;
}

/* Parse a source file, as pointed by the current value of INPUT_FILENAME. */

static void
parse_source_file (file)
     tree file;
{
  int save_error_count = java_error_count;
  /* Mark the file as parsed */
  HAS_BEEN_ALREADY_PARSED_P (file) = 1;

  jcf_dependency_add_file (input_filename, 0);

  lang_init_source (1);		    /* Error msgs have no method prototypes */

  java_init_lex ();		    /* Initialize the parser */
  java_parse_abort_on_error ();

  java_parse ();		    /* Parse and build partial tree nodes. */
  java_parse_abort_on_error ();
  java_complete_class ();	    /* Parse unsatisfied class decl. */
  java_parse_abort_on_error ();
  java_check_circular_reference (); /* Check on circular references */
  java_parse_abort_on_error ();
  java_fix_constructors ();	    /* Fix the constructors */
  java_parse_abort_on_error ();
}

static int
predefined_filename_p (node)
     tree node;
{
  int i;
  for (i = 0; i < predef_filenames_size; i++)
    if (predef_filenames [i] == node)
      return 1;
  return 0;
}

int
yyparse ()
{
  int several_files = 0;
  char *list = xstrdup (input_filename), *next;
  tree node, current_file_list = NULL_TREE;

  do 
    {
      next = strchr (list, '&');
      if (next)
	{
	  *next++ = '\0';
	  several_files = 1;
	}

      if (list[0]) 
	{
	  char *value;
	  tree id;
	  int twice = 0;

	  int len = strlen (list);

	  if (*list != '/' && several_files)
	    obstack_grow (&temporary_obstack, "./", 2);

	  obstack_grow0 (&temporary_obstack, list, len);
	  value = obstack_finish (&temporary_obstack);

	  /* Exclude file that we see twice on the command line. For
	     all files except {Class,Error,Object,RuntimeException,String,
	     Throwable}.java we can rely on maybe_get_identifier. For
	     these files, we need to do a linear search of
	     current_file_list. This search happens only for these
	     files, presumably only when we're recompiling libgcj. */
	     
	  if ((id = maybe_get_identifier (value)))
	    {
	      if (predefined_filename_p (id))
		{
		  tree c;
		  for (c = current_file_list; c; c = TREE_CHAIN (c))
		    if (TREE_VALUE (c) == id)
		      twice = 1;
		}
	      else
		twice = 1;
	    }

	  if (twice)
	    {
	      char *saved_input_filename = input_filename;
	      input_filename = value;
	      warning ("source file seen twice on command line and will be compiled only once.");
	      input_filename = saved_input_filename;
	    }
	  else
	    {
	      node = get_identifier (value);
	      IS_A_COMMAND_LINE_FILENAME_P (node) = 1;
	      current_file_list = tree_cons (NULL_TREE, node, 
					     current_file_list);
	    }
	}
      list = next;
    }
  while (next);

  current_jcf = main_jcf;
  current_file_list = nreverse (current_file_list);
  for (node = current_file_list; node; node = TREE_CHAIN (node))
    {
      tree name = TREE_VALUE (node);

      /* Skip already parsed files */
      if (HAS_BEEN_ALREADY_PARSED_P (name))
	continue;
      
      /* Close previous descriptor, if any */
      if (main_jcf->read_state && fclose (main_jcf->read_state))
	fatal ("failed to close input file `%s' - yyparse",
	       (main_jcf->filename ? main_jcf->filename : "<unknown>"));
      
      /* Set jcf up and open a new file */
      JCF_ZERO (main_jcf);
      main_jcf->read_state = fopen (IDENTIFIER_POINTER (name), "rb");
      if (main_jcf->read_state == NULL)
	pfatal_with_name (IDENTIFIER_POINTER (name));
      
      /* Set new input_filename and finput */
      finput = main_jcf->read_state;
#ifdef IO_BUFFER_SIZE
      setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE),
	       _IOFBF, IO_BUFFER_SIZE);
#endif
      input_filename = IDENTIFIER_POINTER (name);
      main_jcf->filbuf = jcf_filbuf_from_stdio;

      switch (jcf_figure_file_type (current_jcf))
	{
	case JCF_ZIP:
	  parse_zip_file_entries ();
	  break;
	case JCF_CLASS:
	  jcf_parse (current_jcf);
	  parse_class_file ();
	  break;
	case JCF_SOURCE:
	  java_push_parser_context ();
	  java_parser_context_save_global ();
	  parse_source_file (name);
	  java_parser_context_restore_global ();
	  java_pop_parser_context (1);
	  break;
	}
    }

  java_expand_classes ();
  if (!java_report_errors () && !flag_syntax_only)
    emit_register_classes ();
  return 0;
}

static struct ZipFileCache *localToFile;

/* Process all class entries found in the zip file.  */
static void
parse_zip_file_entries (void)
{
  struct ZipDirectory *zdir;
  int i;

  for (i = 0, zdir = (ZipDirectory *)localToFile->z.central_directory;
       i < localToFile->z.count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      tree class;
      
      /* We don't need to consider those files.  */
      if (!zdir->size || !zdir->filename_offset)
	continue;

      class = lookup_class (get_identifier (ZIPDIR_FILENAME (zdir)));
      current_jcf = TYPE_LANG_SPECIFIC (class)->jcf;
      current_class = class;

      if ( !CLASS_LOADED_P (class))
	{
          fseek (current_jcf->read_state, current_jcf->zip_offset, SEEK_SET);
	  jcf_parse (current_jcf);
	}

      if (TYPE_SIZE (current_class) != error_mark_node)
	{
	  input_filename = current_jcf->filename;
	  parse_class_file ();
	  FREE (current_jcf->buffer); /* No longer necessary */
	  /* Note: there is a way to free this buffer right after a
	     class seen in a zip file has been parsed. The idea is the
	     set its jcf in such a way that buffer will be reallocated
	     the time the code for the class will be generated. FIXME. */
	}
    }
}

/* Read all the entries of the zip file, creates a class and a JCF. Sets the
   jcf up for further processing and link it to the created class.  */

static void process_zip_dir()
{
  int i;
  ZipDirectory *zdir;

  for (i = 0, zdir = (ZipDirectory *)localToFile->z.central_directory;
       i < localToFile->z.count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      char *class_name, *file_name, *class_name_in_zip_dir;
      tree class;
      JCF  *jcf;
      int   j;

      class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

      /* We choose to not to process entries with a zero size or entries
	 not bearing the .class extention.  */
      if (!zdir->size || !zdir->filename_offset ||
	  strncmp (&class_name_in_zip_dir[zdir->filename_length-6], 
		   ".class", 6))
	{
	  /* So it will be skipped in parse_zip_file_entries  */
	  zdir->size = 0;  
	  continue;
	}

      class_name = ALLOC (zdir->filename_length+1-6);
      file_name  = ALLOC (zdir->filename_length+1);
      jcf = ALLOC (sizeof (JCF));
      JCF_ZERO (jcf);

      strncpy (class_name, class_name_in_zip_dir, zdir->filename_length-6);
      class_name [zdir->filename_length-6] = '\0';
      strncpy (file_name, class_name_in_zip_dir, zdir->filename_length);
      file_name [zdir->filename_length] = '\0';

      for (j=0; class_name[j]; j++)
        class_name [j] = (class_name [j] == '/' ? '.' : class_name [j]);

      /* Yes, we write back the true class name into the zip directory.  */
      strcpy (class_name_in_zip_dir, class_name);
      zdir->filename_length = j;
      class = lookup_class (get_identifier (class_name));

      jcf->read_state  = finput;
      jcf->filbuf      = jcf_filbuf_from_stdio;
      jcf->seen_in_zip = 1;
      jcf->java_source = 0;
      jcf->zip_offset  = zdir->filestart;
      jcf->classname   = class_name;
      jcf->filename    = file_name;

      TYPE_LANG_SPECIFIC (class) = 
        (struct lang_type *) perm_calloc (1, sizeof (struct lang_type));
      TYPE_LANG_SPECIFIC (class)->jcf = jcf;
    }
}

/* Lookup class NAME and figure whether is a class already found in the current
   zip file.  */
static int
DEFUN(find_in_current_zip, (name, length, jcf),
      const char *name AND JCF **jcf)
{
  JCF *local_jcf;
  tree class_name = maybe_get_identifier (name), class, icv;

  if (!class_name)
    return 0;

  if (!(icv = IDENTIFIER_CLASS_VALUE (class_name)))
    return 0;

  class = TREE_TYPE (icv);

  /* Doesn't have jcf specific info ? It's not ours */
  if (!TYPE_LANG_SPECIFIC (class) || !TYPE_LANG_SPECIFIC (class)->jcf)
    return 0;

  *jcf = local_jcf = TYPE_LANG_SPECIFIC (class)->jcf;
  fseek (local_jcf->read_state, local_jcf->zip_offset, SEEK_SET);
  return 1;
}

/* Figure what kind of file we're dealing with */
static int
DEFUN(jcf_figure_file_type, (jcf),
      JCF *jcf)
{
  unsigned char magic_string[4];
  uint32 magic;

  if (fread (magic_string, 1, 4, jcf->read_state) != 4)
    jcf_unexpected_eof (jcf, 4);

  fseek (jcf->read_state, 0L, SEEK_SET);
  magic = GET_u4 (magic_string);

  if (magic == 0xcafebabe)
    return JCF_CLASS;

  /* FIXME: is it a system file?  */
  if (magic ==  (JCF_u4)ZIPMAGIC
      && !open_in_zip (jcf, input_filename, NULL, 0))
    {
      localToFile = ALLOC (sizeof (struct ZipFileCache));
      bcopy ((PTR) SeenZipFiles, (PTR) localToFile, sizeof (struct ZipFileCache));
      process_zip_dir ();	/* Register all the class defined there */
      return JCF_ZIP;
    }

  return JCF_SOURCE;
}

