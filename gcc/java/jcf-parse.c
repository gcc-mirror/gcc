/* Parser for Java(TM) .class files.
   Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

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
#include "real.h"
#include "obstack.h"
#include "flags.h"
#include "java-except.h"
#include "input.h"
#include "java-tree.h"
#include "toplev.h"
#include "parse.h"
#include "ggc.h"
#include "debug.h"
#include "assert.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

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

extern struct obstack temporary_obstack;

/* Set to nonzero value in order to emit class initilization code
   before static field references.  */
extern int always_initialize_class_p;

static GTY(()) tree parse_roots[3];

/* The FIELD_DECL for the current field.  */
#define current_field parse_roots[0]

/* The METHOD_DECL for the current method.  */
#define current_method parse_roots[1]

/* A list of file names.  */
#define current_file_list parse_roots[2]

/* The Java archive that provides main_class;  the main input file. */
static struct JCF main_jcf[1];

static struct ZipFile *localToFile;

/* Declarations of some functions used here.  */
static void handle_innerclass_attribute PARAMS ((int count, JCF *));
static tree give_name_to_class PARAMS ((JCF *jcf, int index));
static void parse_zip_file_entries PARAMS ((void));
static void process_zip_dir PARAMS ((FILE *));
static void parse_source_file_1 PARAMS ((tree, FILE *));
static void parse_source_file_2 PARAMS ((void));
static void parse_source_file_3 PARAMS ((void));
static void parse_class_file PARAMS ((void));
static void set_source_filename PARAMS ((JCF *, int));
static void ggc_mark_jcf PARAMS ((void**));
static void jcf_parse PARAMS ((struct JCF*));
static void load_inner_classes PARAMS ((tree));

/* Mark (for garbage collection) all the tree nodes that are
   referenced from JCF's constant pool table. Do that only if the JCF
   hasn't been marked finished.  */

static void
ggc_mark_jcf (elt)
     void **elt;
{
  JCF *jcf = *(JCF**) elt;
  if (jcf != NULL && !jcf->finished)
    {
      CPool *cpool = &jcf->cpool;
      int size = CPOOL_COUNT(cpool);
      int index;
      for (index = 1; index < size;  index++)
	{
	  int tag = JPOOL_TAG (jcf, index);
	  if ((tag & CONSTANT_ResolvedFlag) || tag == CONSTANT_Utf8)
	    ggc_mark_tree ((tree) cpool->data[index]);
	}
    }
}

/* Handle "SourceFile" attribute. */

static void
set_source_filename (jcf, index)
     JCF *jcf;
     int index;
{
  tree sfname_id = get_name_constant (jcf, index);
  const char *sfname = IDENTIFIER_POINTER (sfname_id);
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
 set_java_signature (TREE_TYPE (current_field), JPOOL_UTF (jcf, sig_index)); \
 if ((ACCESS_FLAGS) & ACC_FINAL) \
   MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC (current_field); \
}

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
{ current_method = NULL_TREE; }

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

/* Link seen inner classes to their outer context and register the
   inner class to its outer context. They will be later loaded.  */
#define HANDLE_INNERCLASSES_ATTRIBUTE(COUNT) \
  handle_innerclass_attribute (COUNT, jcf)

#define HANDLE_SYNTHETIC_ATTRIBUTE()					\
{									\
  /* Irrelevant decls should have been nullified by the END macros.	\
     We only handle the `Synthetic' attribute on method DECLs.		\
     DECL_ARTIFICIAL on fields is used for something else (See		\
     PUSH_FIELD in java-tree.h) */					\
  if (current_method)							\
    DECL_ARTIFICIAL (current_method) = 1;				\
}

#define HANDLE_GCJCOMPILED_ATTRIBUTE()		\
{ 						\
  if (current_class == object_type_node)	\
    jcf->right_zip = 1;				\
}

#include "jcf-reader.c"

tree
parse_signature (jcf, sig_index)
     JCF *jcf;
     int sig_index;
{
  if (sig_index <= 0 || sig_index >= JPOOL_SIZE (jcf)
      || JPOOL_TAG (jcf, sig_index) != CONSTANT_Utf8)
    abort ();
  else
    return parse_signature_string (JPOOL_UTF_DATA (jcf, sig_index),
				   JPOOL_UTF_LENGTH (jcf, sig_index));
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
	unsigned HOST_WIDE_INT num = JPOOL_UINT (jcf, index);
	HOST_WIDE_INT lo, hi;
	lshift_double (num, 0, 32, 64, &lo, &hi, 0);
	num = JPOOL_UINT (jcf, index+1);
	add_double (lo, hi, num, 0, &lo, &hi);
	value = build_int_2 (lo, hi);
	TREE_TYPE (value) = long_type_node;
	force_fit_type (value, 0);
	break;
      }

    case CONSTANT_Float:
      {
	jint num = JPOOL_INT(jcf, index);
	long buf = num;
	REAL_VALUE_TYPE d;

	real_from_target_fmt (&d, &buf, &ieee_single_format);
	value = build_real (float_type_node, d);
	break;
      }

    case CONSTANT_Double:
      {
	long buf[2], lo, hi;
	REAL_VALUE_TYPE d;

	hi = JPOOL_UINT (jcf, index);
	lo = JPOOL_UINT (jcf, index+1);

	if (FLOAT_WORDS_BIG_ENDIAN)
	  buf[0] = hi, buf[1] = lo;
	else
	  buf[0] = lo, buf[1] = hi;

	real_from_target_fmt (&d, buf, &ieee_double_format);
	value = build_real (double_type_node, d);
	break;
      }

    case CONSTANT_String:
      {
	tree name = get_name_constant (jcf, JPOOL_USHORT1 (jcf, index));
	const char *utf8_ptr = IDENTIFIER_POINTER (name);
	int utf8_len = IDENTIFIER_LENGTH (name);
	const unsigned char *utf8;
	int i;

	/* Check for a malformed Utf8 string.  */
	utf8 = (const unsigned char *) utf8_ptr;
	i = utf8_len;
	while (i > 0)
	  {
	    int char_len = UT8_CHAR_LENGTH (*utf8);
	    if (char_len < 0 || char_len > 3 || char_len > i)
 	      fatal_error ("bad string constant");

	    utf8 += char_len;
	    i -= char_len;
	  }

	/* Allocate a new string value.  */
	value = build_string (utf8_len, utf8_ptr);
	TREE_TYPE (value) = build_pointer_type (string_type_node);
      }
      break;
    default:
      goto bad;
    }
  JPOOL_TAG (jcf, index) = tag | CONSTANT_ResolvedFlag;
  jcf->cpool.data [index] = (jword) value;
  return value;
 bad:
  internal_error ("bad value constant type %d, index %d", 
		  JPOOL_TAG (jcf, index), index);
}

tree
get_name_constant (jcf, index)
  JCF *jcf;
  int index;
{
  tree name = get_constant (jcf, index);

  if (TREE_CODE (name) != IDENTIFIER_NODE)
    abort ();

  return name;
}

/* Handle reading innerclass attributes. If a nonzero entry (denoting
   a non anonymous entry) is found, We augment the inner class list of
   the outer context with the newly resolved innerclass.  */

static void
handle_innerclass_attribute (count, jcf)
     int count;
     JCF *jcf;
{
  int c = (count);
  while (c--)
    {
      /* Read inner_class_info_index. This may be 0 */
      int icii = JCF_readu2 (jcf);
      /* Read outer_class_info_index. If the innerclasses attribute
	 entry isn't a member (like an inner class) the value is 0. */
      int ocii = JCF_readu2 (jcf);
      /* Read inner_name_index. If the class we're dealing with is
	 an annonymous class, it must be 0. */
      int ini = JCF_readu2 (jcf);
      /* Read the access flag. */
      int acc = JCF_readu2 (jcf);
      /* If icii is 0, don't try to read the class. */
      if (icii >= 0)
	{
	  tree class = get_class_constant (jcf, icii);
	  tree decl = TYPE_NAME (class);
          /* Skip reading further if ocii is null */
          if (DECL_P (decl) && !CLASS_COMPLETE_P (decl) && ocii)
	    {
	      tree outer = TYPE_NAME (get_class_constant (jcf, ocii));
	      tree alias = (ini ? get_name_constant (jcf, ini) : NULL_TREE);
	      set_class_decl_access_flags (acc, decl);
	      DECL_CONTEXT (decl) = outer;
	      DECL_INNER_CLASS_LIST (outer) =
		tree_cons (decl, alias, DECL_INNER_CLASS_LIST (outer));
	      CLASS_COMPLETE_P (decl) = 1;
            }
	}
    }
}

static tree
give_name_to_class (jcf, i)
     JCF *jcf;
     int i;
{
  if (i <= 0 || i >= JPOOL_SIZE (jcf)
      || JPOOL_TAG (jcf, i) != CONSTANT_Class)
    abort ();
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
  if (i <= 0 || i >= JPOOL_SIZE (jcf)
      || (JPOOL_TAG (jcf, i) & ~CONSTANT_ResolvedFlag) != CONSTANT_Class)
    abort ();

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
  tree icv, class = NULL_TREE;
  tree save_current_class = current_class;
  const char *save_input_filename = input_filename;
  JCF *save_current_jcf = current_jcf;

  if ((icv = IDENTIFIER_CLASS_VALUE (name)) != NULL_TREE)
    {
      class = TREE_TYPE (icv);
      jcf = TYPE_JCF (class);
    }
  else
    jcf = NULL;

  if (jcf == NULL)
    {
      this_jcf.zipd = NULL;
      jcf = &this_jcf;
      if (find_class (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name),
		      &this_jcf, 1) == 0)
	return 0;
    }

  current_jcf = jcf;

  if (current_jcf->java_source)
    {
      const char *filename = current_jcf->filename;
      tree file;
      FILE *finput;
      int generate;

      java_parser_context_save_global ();
      java_push_parser_context ();
      BUILD_FILENAME_IDENTIFIER_NODE (file, filename);
      generate = IS_A_COMMAND_LINE_FILENAME_P (file);
      if (wfl_operator == NULL_TREE)
	wfl_operator = build_expr_wfl (NULL_TREE, NULL, 0, 0);
      EXPR_WFL_FILENAME_NODE (wfl_operator) = file;
      input_filename = ggc_strdup (filename);
      current_class = NULL_TREE;
      current_function_decl = NULL_TREE;
      if (!HAS_BEEN_ALREADY_PARSED_P (file))
	{
	  if (!(finput = fopen (input_filename, "r")))
	    fatal_io_error ("can't reopen %s", input_filename);
	  parse_source_file_1 (file, finput);
	  parse_source_file_2 ();
	  parse_source_file_3 ();
	  if (fclose (finput))
	    fatal_io_error ("can't close %s", input_filename);
	}
      JCF_FINISH (current_jcf);
      java_pop_parser_context (generate);
      java_parser_context_restore_global ();
    }
  else
    {
      if (class == NULL_TREE || ! CLASS_PARSED_P (class))
	{
	  java_parser_context_save_global ();
	  java_push_parser_context ();
	  current_class = class;
	  input_filename = current_jcf->filename;
	  if (JCF_SEEN_IN_ZIP (current_jcf))
	    read_zip_member(current_jcf,
			    current_jcf->zipd, current_jcf->zipd->zipf);
	  jcf_parse (current_jcf);
	  /* Parsing might change the class, in which case we have to
	     put it back where we found it.  */
	  if (current_class != class && icv != NULL_TREE)
	    TREE_TYPE (icv) = current_class;
	  class = current_class;
	  java_pop_parser_context (0);
	  java_parser_context_restore_global ();
	}
      layout_class (class);
      load_inner_classes (class);
    }

  current_class = save_current_class;
  input_filename = save_input_filename;
  current_jcf = save_current_jcf;
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
  tree name, saved;
  int class_loaded;

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

  saved = name;
  while (1)
    {
      char *separator;

      if ((class_loaded = read_class (name)))
	break;

      /* We failed loading name. Now consider that we might be looking
	 for a inner class. */
      if ((separator = strrchr (IDENTIFIER_POINTER (name), '$'))
	  || (separator = strrchr (IDENTIFIER_POINTER (name), '.')))
	{
	  int c = *separator;
	  *separator = '\0';
	  name = get_identifier (IDENTIFIER_POINTER (name));
	  *separator = c;
	}
      /* Otherwise, we failed, we bail. */
      else
	break;
    }

  if (!class_loaded && verbose)
    error ("cannot find file for class %s", IDENTIFIER_POINTER (saved));
}

/* Parse the .class file JCF. */

void
jcf_parse (jcf)
     JCF* jcf;
{
  int i, code;

  if (jcf_parse_preamble (jcf) != 0)
    fatal_error ("not a valid Java .class file");
  code = jcf_parse_constant_pool (jcf);
  if (code != 0)
    fatal_error ("error while parsing constant pool");
  code = verify_constant_pool (jcf);
  if (code > 0)
    fatal_error ("error in constant pool entry #%d\n", code);

  jcf_parse_class (jcf);
  if (main_class == NULL_TREE)
    main_class = current_class;
  if (! quiet_flag && TYPE_NAME (current_class))
    fprintf (stderr, " %s %s",
	     (jcf->access_flags & ACC_INTERFACE) ? "interface" : "class", 
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))));
  if (CLASS_PARSED_P (current_class))
    {
      /* FIXME - where was first time */
      fatal_error ("reading class %s for the second time from %s",
		   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class))),
		   jcf->filename);
    }
  CLASS_PARSED_P (current_class) = 1;

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
    fatal_error ("error while parsing fields");
  code = jcf_parse_methods (jcf);
  if (code != 0)
    fatal_error ("error while parsing methods");
  code = jcf_parse_final_attributes (jcf);
  if (code != 0)
    fatal_error ("error while parsing final attributes");

  /* The fields of class_type_node are already in correct order. */
  if (current_class != class_type_node && current_class != object_type_node)
    TYPE_FIELDS (current_class) = nreverse (TYPE_FIELDS (current_class));

  if (current_class == object_type_node)
    {
      layout_class_methods (object_type_node);
      /* If we don't have the right archive, emit a verbose warning.
	 If we're generating bytecode, emit the warning only if
	 -fforce-classes-archive-check was specified. */
      if (!jcf->right_zip
	  && (!flag_emit_class_files || flag_force_classes_archive_check))
	fatal_error ("the `java.lang.Object' that was found in `%s' didn't have the special zero-length `gnu.gcj.gcj-compiled' attribute.  This generally means that your classpath is incorrectly set.  Use `info gcj \"Input Options\"' to see the info page describing how to set the classpath", jcf->filename);
    }
  else
    all_class_list = tree_cons (NULL_TREE,
				TYPE_NAME (current_class), all_class_list );
}

/* If we came across inner classes, load them now. */
static void
load_inner_classes (cur_class)
     tree cur_class;
{
  tree current;
  for (current = DECL_INNER_CLASS_LIST (TYPE_NAME (cur_class)); current;
       current = TREE_CHAIN (current))
    {
      tree name = DECL_NAME (TREE_PURPOSE (current));
      tree decl = IDENTIFIER_GLOBAL_VALUE (name);
      if (decl && ! CLASS_LOADED_P (TREE_TYPE (decl))
	  && !CLASS_BEING_LAIDOUT (TREE_TYPE (decl)))
	load_class (name, 1);
    }
}

void
init_outgoing_cpool ()
{
  current_constant_pool_data_ref = NULL_TREE;
  outgoing_cpool = xmalloc (sizeof (struct CPool));
  memset (outgoing_cpool, 0, sizeof (struct CPool));
}

static void
parse_class_file ()
{
  tree method, field;
  const char *save_input_filename = input_filename;
  int save_lineno = lineno;

  java_layout_seen_class_methods ();

  input_filename = DECL_SOURCE_FILE (TYPE_NAME (current_class));
  lineno = 0;
  (*debug_hooks->start_source_file) (lineno, input_filename);
  init_outgoing_cpool ();

  /* Currently we always have to emit calls to _Jv_InitClass when
     compiling from class files.  */
  always_initialize_class_p = 1;

  for (field = TYPE_FIELDS (current_class);
       field != NULL_TREE; field = TREE_CHAIN (field))
    if (FIELD_STATIC (field))
      DECL_EXTERNAL (field) = 0;

  for (method = TYPE_METHODS (current_class);
       method != NULL_TREE; method = TREE_CHAIN (method))
    {
      JCF *jcf = current_jcf;

      if (METHOD_ABSTRACT (method))
	continue;

      if (METHOD_NATIVE (method))
	{
	  tree arg;
	  int  decl_max_locals;

	  if (! flag_jni)
	    continue;
	  /* We need to compute the DECL_MAX_LOCALS. We need to take
             the wide types into account too. */
	  for (arg = TYPE_ARG_TYPES (TREE_TYPE (method)), decl_max_locals = 0; 
	       arg != end_params_node;
	       arg = TREE_CHAIN (arg), decl_max_locals += 1)
	    {
	      if (TREE_VALUE (arg) && TYPE_IS_WIDE (TREE_VALUE (arg)))
		decl_max_locals += 1;
	    }
	  DECL_MAX_LOCALS (method) = decl_max_locals;
	  start_java_method (method);
	  give_name_to_locals (jcf);
	  expand_expr_stmt (build_jni_stub (method));
	  end_java_method ();
	  continue;
	}

      if (DECL_CODE_OFFSET (method) == 0)
	{
	  current_function_decl = method;
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

      note_instructions (jcf, method);

      give_name_to_locals (jcf);

      /* Actually generate code. */
      expand_byte_code (jcf, method);

      end_java_method ();
    }

  if (flag_emit_class_files)
    write_classfile (current_class);

  finish_class ();

  (*debug_hooks->end_source_file) (save_lineno);
  input_filename = save_input_filename;
  lineno = save_lineno;
}

/* Parse a source file, as pointed by the current value of INPUT_FILENAME. */

static void
parse_source_file_1 (file, finput)
     tree file;
     FILE *finput;
{
  int save_error_count = java_error_count;
  /* Mark the file as parsed */
  HAS_BEEN_ALREADY_PARSED_P (file) = 1;

  jcf_dependency_add_file (input_filename, 0);

  lang_init_source (1);		    /* Error msgs have no method prototypes */

  /* There's no point in trying to find the current encoding unless we
     are going to do something intelligent with it -- hence the test
     for iconv.  */
#if defined (HAVE_LOCALE_H) && defined (HAVE_ICONV) && defined (HAVE_LANGINFO_CODESET)
  setlocale (LC_CTYPE, "");
  if (current_encoding == NULL)
    current_encoding = nl_langinfo (CODESET);
#endif 
  if (current_encoding == NULL || *current_encoding == '\0')
    current_encoding = DEFAULT_ENCODING;

  /* Initialize the parser */
  java_init_lex (finput, current_encoding);
  java_parse_abort_on_error ();

  java_parse ();		    /* Parse and build partial tree nodes. */
  java_parse_abort_on_error ();
}

/* Process a parsed source file, resolving names etc. */

static void
parse_source_file_2 ()
{
  int save_error_count = java_error_count;
  java_complete_class ();	    /* Parse unsatisfied class decl. */
  java_parse_abort_on_error ();
}

static void
parse_source_file_3 ()
{
  int save_error_count = java_error_count;
  java_check_circular_reference (); /* Check on circular references */
  java_parse_abort_on_error ();
  java_fix_constructors ();	    /* Fix the constructors */
  java_parse_abort_on_error ();
  java_reorder_fields ();	    /* Reorder the fields */
}

void
add_predefined_file (name)
     tree name;
{
  predef_filenames = tree_cons (NULL_TREE, name, predef_filenames);
}

int
predefined_filename_p (node)
     tree node;
{
  tree iter;

  for (iter = predef_filenames; iter != NULL_TREE; iter = TREE_CHAIN (iter))
    {
      if (TREE_VALUE (iter) == node)
	return 1;
    }
  return 0;
}

void
java_parse_file (set_yydebug)
     int set_yydebug ATTRIBUTE_UNUSED;
{
  int filename_count = 0;
  char *list, *next;
  tree node;
  FILE *finput = NULL;

  if (flag_filelist_file)
    {
      int avail = 2000;
      finput = fopen (input_filename, "r");
      if (finput == NULL)
	fatal_io_error ("can't open %s", input_filename);
      list = xmalloc(avail);
      next = list;
      for (;;)
	{
	  int count;
	  if (avail < 500)
	    {
	      count = next - list;
	      avail = 2 * (count + avail);
	      list = xrealloc (list, avail);
	      next = list + count;
	      avail = avail - count;
	    }
	  /* Subtract to to guarantee space for final '\0'. */
	  count = fread (next, 1, avail - 1, finput);
	  if (count == 0)
	    {
	      if (! feof (finput))
		fatal_io_error ("error closing %s", input_filename);
	      *next = '\0';
	      break;
	    }
	  avail -= count;
	  next += count;
	}
      fclose (finput);
      finput = NULL;
    }
  else
    list = xstrdup (input_filename);

  do 
    {
      for (next = list; ; )
	{
	  char ch = *next;
	  if (ch == '\n' || ch == '\r' || ch == '\t' || ch == ' '
	      || ch == '&' /* FIXME */)
	    {
	      if (next == list)
		{
		  next++;
		  list = next;
		  continue;
		}
	      else
		{
		  *next++ = '\0';
		  break;
		}
	    }
	  if (ch == '\0')
	    {
	      next = NULL;
	      break;
	    }
	  next++;
	}

      if (list[0]) 
	{
	  char *value;
	  tree id;
	  int twice = 0;

	  int len = strlen (list);

	  obstack_grow0 (&temporary_obstack, list, len);
	  value = obstack_finish (&temporary_obstack);

	  filename_count++;

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
	      const char *saved_input_filename = input_filename;
	      input_filename = value;
	      warning ("source file seen twice on command line and will be compiled only once");
	      input_filename = saved_input_filename;
	    }
	  else
	    {
	      BUILD_FILENAME_IDENTIFIER_NODE (node, value);
	      IS_A_COMMAND_LINE_FILENAME_P (node) = 1;
	      current_file_list = tree_cons (NULL_TREE, node, 
					     current_file_list);
	    }
	}
      list = next;
    }
  while (next);

  if (filename_count == 0)
    warning ("no input file specified");

  if (resource_name)
    {
      const char *resource_filename;
      
      /* Only one resource file may be compiled at a time.  */
      assert (TREE_CHAIN (current_file_list) == NULL);

      resource_filename = IDENTIFIER_POINTER (TREE_VALUE (current_file_list));
      compile_resource_file (resource_name, resource_filename);

      return;
    }

  current_jcf = main_jcf;
  current_file_list = nreverse (current_file_list);
  for (node = current_file_list; node; node = TREE_CHAIN (node))
    {
      unsigned char magic_string[4];
      uint32 magic = 0;
      tree name = TREE_VALUE (node);

      /* Skip already parsed files */
      if (HAS_BEEN_ALREADY_PARSED_P (name))
	continue;
      
      /* Close previous descriptor, if any */
      if (finput && fclose (finput))
	fatal_io_error ("can't close input file %s", main_input_filename);
      
      finput = fopen (IDENTIFIER_POINTER (name), "rb");
      if (finput == NULL)
	fatal_io_error ("can't open %s", IDENTIFIER_POINTER (name));
      
#ifdef IO_BUFFER_SIZE
      setvbuf (finput, xmalloc (IO_BUFFER_SIZE),
	       _IOFBF, IO_BUFFER_SIZE);
#endif
      input_filename = IDENTIFIER_POINTER (name);

      /* Figure what kind of file we're dealing with */
      if (fread (magic_string, 1, 4, finput) == 4)
	{
	  fseek (finput, 0L, SEEK_SET);
	  magic = GET_u4 (magic_string);
	}
      if (magic == 0xcafebabe)
	{
	  CLASS_FILE_P (node) = 1;
	  current_jcf = ALLOC (sizeof (JCF));
	  JCF_ZERO (current_jcf);
	  current_jcf->read_state = finput;
	  current_jcf->filbuf = jcf_filbuf_from_stdio;
	  jcf_parse (current_jcf);
	  TYPE_JCF (current_class) = current_jcf;
	  CLASS_FROM_CURRENTLY_COMPILED_P (current_class) = 1;
	  TREE_PURPOSE (node) = current_class;
	}
      else if (magic == (JCF_u4)ZIPMAGIC)
	{
	  ZIP_FILE_P (node) = 1;
	  JCF_ZERO (main_jcf);
	  main_jcf->read_state = finput;
	  main_jcf->filbuf = jcf_filbuf_from_stdio;
	  if (open_in_zip (main_jcf, input_filename, NULL, 0) <  0)
	    fatal_error ("bad zip/jar file %s", IDENTIFIER_POINTER (name));
	  localToFile = SeenZipFiles;
	  /* Register all the class defined there.  */
	  process_zip_dir (main_jcf->read_state);
	  parse_zip_file_entries ();
	  /*
	  for (each entry)
	    CLASS_FROM_CURRENTLY_COMPILED_P (current_class) = 1;
	  */
	}
      else
	{
	  JAVA_FILE_P (node) = 1;
	  java_push_parser_context ();
	  java_parser_context_save_global ();
	  parse_source_file_1 (name, finput);
	  java_parser_context_restore_global ();
	  java_pop_parser_context (1);
	}
    }

  for (ctxp = ctxp_for_generation;  ctxp;  ctxp = ctxp->next)
    {
      input_filename = ctxp->filename;
      parse_source_file_2 ();
    }

  for (ctxp = ctxp_for_generation; ctxp; ctxp = ctxp->next)
    {
      input_filename = ctxp->filename;
      parse_source_file_3 ();
    }

  for (node = current_file_list; node; node = TREE_CHAIN (node))
    {
      input_filename = IDENTIFIER_POINTER (TREE_VALUE (node));
      if (CLASS_FILE_P (node))
	{
	  current_class = TREE_PURPOSE (node);
	  current_jcf = TYPE_JCF (current_class);
	  layout_class (current_class);
	  load_inner_classes (current_class);
	  parse_class_file ();
	  JCF_FINISH (current_jcf);
	}
    }
  input_filename = main_input_filename;

  java_expand_classes ();
  if (!java_report_errors () && !flag_syntax_only)
    {
      emit_register_classes ();
      if (flag_indirect_dispatch)
	emit_offset_symbol_table ();
    }
}

/* Process all class entries found in the zip file.  */
static void
parse_zip_file_entries (void)
{
  struct ZipDirectory *zdir;
  int i;

  for (i = 0, zdir = (ZipDirectory *)localToFile->central_directory;
       i < localToFile->count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      tree class;
      
      /* We don't need to consider those files.  */
      if (!zdir->size || !zdir->filename_offset)
	continue;

      class = lookup_class (get_identifier (ZIPDIR_FILENAME (zdir)));
      current_jcf = TYPE_JCF (class);
      current_class = class;

      if ( !CLASS_LOADED_P (class))
	{
	  if (! CLASS_PARSED_P (class))
	    {
	      read_zip_member(current_jcf, zdir, localToFile);
	      jcf_parse (current_jcf);
	    }
	  layout_class (current_class);
	  load_inner_classes (current_class);
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

static void
process_zip_dir (FILE *finput)
{
  int i;
  ZipDirectory *zdir;

  for (i = 0, zdir = (ZipDirectory *)localToFile->central_directory;
       i < localToFile->count; i++, zdir = ZIPDIR_NEXT (zdir))
    {
      char *class_name, *file_name, *class_name_in_zip_dir;
      tree class;
      JCF  *jcf;
      int   j;

      class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

      /* We choose to not to process entries with a zero size or entries
	 not bearing the .class extension.  */
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
      jcf->java_source = 0;
      jcf->classname   = class_name;
      jcf->filename    = file_name;
      jcf->zipd        = zdir;

      TYPE_JCF (class) = jcf;
    }
}

/* Initialization.  */

void
init_jcf_parse ()
{
  /* Register roots with the garbage collector.  */
  ggc_add_root (&current_jcf, 1, sizeof (JCF), (void (*)(void *))ggc_mark_jcf);

  init_src_parse ();
}

#include "gt-java-jcf-parse.h"
