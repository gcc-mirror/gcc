/* Parser for Java(TM) .class files.
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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
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
#include "tm_p.h"
#include "cgraph.h"

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
    (JCF)->cpool.data[INDEX].t = get_identifier ((const char *) text); \
    text[LENGTH] = save; \
    JCF_SKIP (JCF, LENGTH); } while (0)

#include "jcf.h"

extern struct obstack temporary_obstack;

static GTY(()) tree parse_roots[3];

/* The FIELD_DECL for the current field.  */
#define current_field parse_roots[0]

/* The METHOD_DECL for the current method.  */
#define current_method parse_roots[1]

/* A list of TRANSLATION_UNIT_DECLs for the files to be compiled.  */
#define current_file_list parse_roots[2]

/* Line 0 in current file, if compiling from bytecode. */
static location_t file_start_location;

/* The Java archive that provides main_class;  the main input file. */
static GTY(()) struct JCF * main_jcf;

static struct ZipFile *localToFile;

/* Declarations of some functions used here.  */
static void handle_innerclass_attribute (int count, JCF *);
static tree give_name_to_class (JCF *jcf, int index);
static char *compute_class_name (struct ZipDirectory *zdir);
static int classify_zip_file (struct ZipDirectory *zdir);
static void parse_zip_file_entries (void);
static void process_zip_dir (FILE *);
static void parse_source_file_1 (tree, const char *, FILE *);
static void parse_source_file_2 (void);
static void parse_source_file_3 (void);
static void parse_class_file (void);
static void handle_deprecated (void);
static void set_source_filename (JCF *, int);
static void jcf_parse (struct JCF*);
static void load_inner_classes (tree);

/* Handle "Deprecated" attribute.  */
static void
handle_deprecated (void)
{
  if (current_field != NULL_TREE)
    FIELD_DEPRECATED (current_field) = 1;
  else if (current_method != NULL_TREE)
    METHOD_DEPRECATED (current_method) = 1;
  else if (current_class != NULL_TREE)
    CLASS_DEPRECATED (TYPE_NAME (current_class)) = 1;
  else
    {
      /* Shouldn't happen.  */
      abort ();
    }
}

/* Handle "SourceFile" attribute. */

static void
set_source_filename (JCF *jcf, int index)
{
  tree sfname_id = get_name_constant (jcf, index);
  const char *sfname = IDENTIFIER_POINTER (sfname_id);
  const char *old_filename = input_filename;
  int new_len = IDENTIFIER_LENGTH (sfname_id);
  if (old_filename != NULL)
    {
      int old_len = strlen (old_filename);
      /* Use the current input_filename (derived from the class name)
	 if it has a directory prefix, but otherwise matches sfname. */
      if (old_len > new_len
	  && strcmp (sfname, old_filename + old_len - new_len) == 0
	  && (old_filename[old_len - new_len - 1] == '/'
	      || old_filename[old_len - new_len - 1] == '\\'))
	{
#ifndef USE_MAPPED_LOCATION
	  DECL_SOURCE_LOCATION (TYPE_NAME (current_class)) = input_location;
	  file_start_location = input_location;
#endif
	  return;
	}
    }
  if (strchr (sfname, '/') == NULL && strchr (sfname, '\\') == NULL)
    {
      const char *class_name
	= IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (current_class)));
      char *dot = strrchr (class_name, '.');
      if (dot != NULL)
	{
	  /* Length of prefix, not counting final dot. */
	  int i = dot - class_name;
	  /* Concatenate current package prefix with new sfname. */
	  char *buf = xmalloc (i + new_len + 2); /* Space for '.' and '\0'. */
	  strcpy (buf + i + 1, sfname);
	  /* Copy package from class_name, replacing '.' by DIR_SEPARATOR.
	     Note we start at the end with the final package dot. */
	  for (; i >= 0;  i--)
	    {
	      char c = class_name[i];
	      if (c == '.')
		c = DIR_SEPARATOR;
	      buf[i] = c;
	    }
	  sfname_id = get_identifier (buf);
	  free (buf);
	  sfname = IDENTIFIER_POINTER (sfname_id);
	}
    }
      
#ifdef USE_MAPPED_LOCATION
  line_table.maps[line_table.used-1].to_file = sfname;
#else
  input_filename = sfname;
  DECL_SOURCE_LOCATION (TYPE_NAME (current_class)) = input_location;
  file_start_location = input_location;
#endif
  if (current_class == main_class) main_input_filename = sfname;
}

#define HANDLE_SOURCEFILE(INDEX) set_source_filename (jcf, INDEX)

#define HANDLE_CLASS_INFO(ACCESS_FLAGS, THIS, SUPER, INTERFACES_COUNT) \
{ tree super_class = SUPER==0 ? NULL_TREE : get_class_constant (jcf, SUPER); \
  output_class = current_class = give_name_to_class (jcf, THIS); \
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

#define HANDLE_DEPRECATED_ATTRIBUTE()  handle_deprecated ()

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
parse_signature (JCF *jcf, int sig_index)
{
  if (sig_index <= 0 || sig_index >= JPOOL_SIZE (jcf)
      || JPOOL_TAG (jcf, sig_index) != CONSTANT_Utf8)
    abort ();
  else
    return parse_signature_string (JPOOL_UTF_DATA (jcf, sig_index),
				   JPOOL_UTF_LENGTH (jcf, sig_index));
}

tree
get_constant (JCF *jcf, int index)
{
  tree value;
  int tag;
  if (index <= 0 || index >= JPOOL_SIZE(jcf))
    goto bad;
  tag = JPOOL_TAG (jcf, index);
  if ((tag & CONSTANT_ResolvedFlag) || tag == CONSTANT_Utf8)
    return jcf->cpool.data[index].t;
  switch (tag)
    {
    case CONSTANT_Integer:
      {
	jint num = JPOOL_INT(jcf, index);
	value = build_int_cst (int_type_node, num);
	break;
      }
    case CONSTANT_Long:
      {
	unsigned HOST_WIDE_INT num = JPOOL_UINT (jcf, index);
	unsigned HOST_WIDE_INT lo;
	HOST_WIDE_INT hi;
	
	lshift_double (num, 0, 32, 64, &lo, &hi, 0);
	num = JPOOL_UINT (jcf, index+1);
	add_double (lo, hi, num, 0, &lo, &hi);
	value = build_int_cst_wide (long_type_node, lo, hi);
	value = force_fit_type (value, 0, false, false);
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
  jcf->cpool.data[index].t = value;
  return value;
 bad:
  internal_error ("bad value constant type %d, index %d", 
		  JPOOL_TAG (jcf, index), index);
}

tree
get_name_constant (JCF *jcf, int index)
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
handle_innerclass_attribute (int count, JCF *jcf)
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
	 an anonymous class, it must be 0. */
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
give_name_to_class (JCF *jcf, int i)
{
  if (i <= 0 || i >= JPOOL_SIZE (jcf)
      || JPOOL_TAG (jcf, i) != CONSTANT_Class)
    abort ();
  else
    {
      tree package_name = NULL_TREE, tmp;
      tree this_class;
      int j = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that j is a CONSTANT_Utf8. */
      tree class_name = unmangle_classname ((const char *) JPOOL_UTF_DATA (jcf, j),
					    JPOOL_UTF_LENGTH (jcf, j));
      this_class = lookup_class (class_name);
#ifdef USE_MAPPED_LOCATION
      {
      tree source_name = identifier_subst (class_name, "", '.', '/', ".java");
      const char *sfname = IDENTIFIER_POINTER (source_name);
      linemap_add (&line_table, LC_ENTER, false, sfname, 0);
      input_location = linemap_line_start (&line_table, 0, 1);
      file_start_location = input_location;
      DECL_SOURCE_LOCATION (TYPE_NAME (this_class)) = input_location;
      if (main_input_filename == NULL && jcf == main_jcf)
	main_input_filename = sfname;
      }
#else
      input_location = DECL_SOURCE_LOCATION (TYPE_NAME (this_class));
      if (main_input_filename == NULL && jcf == main_jcf)
	main_input_filename = input_filename;
#endif

      jcf->cpool.data[i].t = this_class;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
      split_qualified_name (&package_name, &tmp, 
      			    DECL_NAME (TYPE_NAME (this_class)));
      TYPE_PACKAGE (this_class) = package_name;
      return this_class;
    }
}

/* Get the class of the CONSTANT_Class whose constant pool index is I. */

tree
get_class_constant (JCF *jcf, int i)
{
  tree type;
  if (i <= 0 || i >= JPOOL_SIZE (jcf)
      || (JPOOL_TAG (jcf, i) & ~CONSTANT_ResolvedFlag) != CONSTANT_Class)
    abort ();

  if (JPOOL_TAG (jcf, i) != CONSTANT_ResolvedClass)
    {
      int name_index = JPOOL_USHORT1 (jcf, i);
      /* verify_constant_pool confirmed that name_index is a CONSTANT_Utf8. */
      const char *name = (const char *) JPOOL_UTF_DATA (jcf, name_index);
      int nlength = JPOOL_UTF_LENGTH (jcf, name_index);

      if (name[0] == '[')  /* Handle array "classes". */
	  type = TREE_TYPE (parse_signature_string ((const unsigned char *) name, nlength));
      else
        { 
          tree cname = unmangle_classname (name, nlength);
          type = lookup_class (cname);
	}
      jcf->cpool.data[i].t = type;
      JPOOL_TAG (jcf, i) = CONSTANT_ResolvedClass;
    }
  else
    type = jcf->cpool.data[i].t;
  return type;
}

/* Read a class with the fully qualified-name NAME.
   Return 1 iff we read the requested file.
   (It is still possible we failed if the file did not
   define the class it is supposed to.) */

int
read_class (tree name)
{
  JCF this_jcf, *jcf;
  tree icv, class = NULL_TREE;
  tree save_current_class = current_class;
  tree save_output_class = output_class;
  location_t save_location = input_location;
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
      const char* path_name;
      this_jcf.zipd = NULL;
      jcf = &this_jcf;
      
      path_name = find_class (IDENTIFIER_POINTER (name),
			      IDENTIFIER_LENGTH (name),
			      &this_jcf, 1);
      if (path_name == 0)
	return 0;
      else
	free((char *) path_name);
    }

  current_jcf = jcf;

  if (current_jcf->java_source)
    {
      const char *filename = current_jcf->filename;
      char *real_path;
      tree given_file, real_file;
      FILE *finput;
      int generate;

      java_parser_context_save_global ();
      java_push_parser_context ();

      given_file = get_identifier (filename);
      filename = IDENTIFIER_POINTER (given_file);
      real_path = lrealpath (filename);
      real_file = get_identifier (real_path);
      free (real_path);

      generate = IS_A_COMMAND_LINE_FILENAME_P (given_file);
      output_class = current_class = NULL_TREE;
      current_function_decl = NULL_TREE;

      if (! HAS_BEEN_ALREADY_PARSED_P (real_file))
	{
	  if (! (finput = fopen (filename, "r")))
	    fatal_error ("can't reopen %s: %m", filename);

	  parse_source_file_1 (real_file, filename, finput);
	  parse_source_file_2 ();
	  parse_source_file_3 ();

	  if (fclose (finput))
	    fatal_error ("can't close %s: %m", input_filename);
#ifdef USE_MAPPED_LOCATION
	  linemap_add (&line_table, LC_LEAVE, false, NULL, 0);
#endif
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
	  output_class = current_class = class;
	  ctxp->save_location = input_location;
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

  output_class = save_output_class;
  current_class = save_current_class;
  input_location = save_location;
  current_jcf = save_current_jcf;
  return 1;
}

/* Load CLASS_OR_NAME. CLASS_OR_NAME can be a mere identifier if
   called from the parser, otherwise it's a RECORD_TYPE node. If
   VERBOSE is 1, print error message on failure to load a class. */
void
load_class (tree class_or_name, int verbose)
{
  tree name, saved;
  int class_loaded = 0;
  tree class_decl = NULL_TREE;
  bool is_compiled_class = false;

  /* We've already failed, don't try again.  */
  if (TREE_CODE (class_or_name) == RECORD_TYPE
      && TYPE_DUMMY (class_or_name))
    return;

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

  class_decl = IDENTIFIER_CLASS_VALUE (name);
  if (class_decl != NULL_TREE)
    {
      tree type = TREE_TYPE (class_decl);
      is_compiled_class
	= ((TYPE_JCF (type) && JCF_SEEN_IN_ZIP (TYPE_JCF (type)))
	   || CLASS_FROM_CURRENTLY_COMPILED_P (type));
    }

  /* If the class is from source code, then it must already be loaded.  */
  class_decl = IDENTIFIER_CLASS_VALUE (name);
  if (class_decl && CLASS_FROM_SOURCE_P (TREE_TYPE (class_decl)))
    return;

  saved = name;
  
  /* If flag_verify_invocations is unset, we don't try to load a class
     unless we're looking for Object (which is fixed by the ABI) or
     it's a class that we're going to compile.  */
  if (flag_verify_invocations
      || class_or_name == object_type_node
      || is_compiled_class
      || TREE_CODE (class_or_name) == IDENTIFIER_NODE)
    {
      while (1)
	{
	  char *separator;

	  /* We've already loaded it.  */
	  if (IDENTIFIER_CLASS_VALUE (name) != NULL_TREE)
	    {
	      tree tmp_decl = IDENTIFIER_CLASS_VALUE (name);
	      if (CLASS_PARSED_P (TREE_TYPE (tmp_decl)))
		break;
	    }
	
	  if (read_class (name))
	    break;

	  /* We failed loading name. Now consider that we might be looking
	     for an inner class.  */
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

      {
	/* have we found the class we're looking for?  */
	tree type_decl = IDENTIFIER_CLASS_VALUE (saved);
	tree type = type_decl ? TREE_TYPE (type_decl) : NULL;
	class_loaded = type && CLASS_PARSED_P (type);
      }	      
    }
  
  if (!class_loaded)
    {
      if (flag_verify_invocations || ! flag_indirect_dispatch
	  || flag_emit_class_files)
	{
	  if (verbose)
	    error ("cannot find file for class %s", IDENTIFIER_POINTER (saved));
	}
      else if (verbose)
	{
	  /* This is just a diagnostic during testing, not a real problem.  */
	  if (!quiet_flag)
	    warning (0, "cannot find file for class %s", 
		     IDENTIFIER_POINTER (saved));
	  
	  /* Fake it.  */
	  if (TREE_CODE (class_or_name) == RECORD_TYPE)
	    {
	      set_super_info (0, class_or_name, object_type_node, 0);
	      TYPE_DUMMY (class_or_name) = 1;
	      /* We won't be able to output any debug info for this class.  */
	      DECL_IGNORED_P (TYPE_NAME (class_or_name)) = 1;
	    }
	}
    }
}

/* Parse the .class file JCF. */

static void
jcf_parse (JCF* jcf)
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
#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_LEAVE, false, NULL, 0);
#endif

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
	fatal_error ("the %<java.lang.Object%> that was found in %qs didn't have the special zero-length %<gnu.gcj.gcj-compiled%> attribute.  This generally means that your classpath is incorrectly set.  Use %<info gcj \"Input Options\"%> to see the info page describing how to set the classpath", jcf->filename);
    }
  else
    all_class_list = tree_cons (NULL_TREE,
				TYPE_NAME (current_class), all_class_list );
}

/* If we came across inner classes, load them now. */
static void
load_inner_classes (tree cur_class)
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

static void
duplicate_class_warning (const char *filename)
{
  location_t warn_loc;
#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_RENAME, 0, filename, 0);
  warn_loc = linemap_line_start (&line_table, 0, 1);
#else
  warn_loc.file = filename;
  warn_loc.line = 0;
#endif
  warning (0, "%Hduplicate class will only be compiled once", &warn_loc);
}

static void
parse_class_file (void)
{
  tree method;
  location_t save_location = input_location;

  java_layout_seen_class_methods ();

  input_location = DECL_SOURCE_LOCATION (TYPE_NAME (current_class));
  file_start_location = input_location;
  (*debug_hooks->start_source_file) (input_line, input_filename);

  gen_indirect_dispatch_tables (current_class);

  java_mark_class_local (current_class);

  for (method = TYPE_METHODS (current_class);
       method != NULL_TREE; method = TREE_CHAIN (method))
    {
      JCF *jcf = current_jcf;

      if (METHOD_ABSTRACT (method) || METHOD_DUMMY (method))
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
	  *get_stmts () = build_jni_stub (method);
	  end_java_method ();
	  continue;
	}

      if (DECL_CODE_OFFSET (method) == 0)
	{
	  current_function_decl = method;
	  error ("missing Code attribute");
	  continue;
	}

      input_location = DECL_SOURCE_LOCATION (TYPE_NAME (current_class));
      if (DECL_LINENUMBERS_OFFSET (method))
	{
	  int i;
	  int min_line = 0;
	  unsigned char *ptr;
	  JCF_SEEK (jcf, DECL_LINENUMBERS_OFFSET (method));
	  linenumber_count = i = JCF_readu2 (jcf);
	  linenumber_table = ptr = jcf->read_ptr;

	  for (ptr += 2; --i >= 0; ptr += 4)
	    {
	      int line = GET_u2 (ptr);
	      /* Set initial input_line to smallest linenumber.
	       * Needs to be set before init_function_start. */
	      if (min_line == 0 || line < min_line)
		min_line = line;
	    }
#ifdef USE_MAPPED_LOCATION
	  if (min_line != 0)
	    input_location = linemap_line_start (&line_table, min_line, 1);
#else
	  if (min_line != 0)
	    input_line = min_line;
#endif
	}
      else
	{
	  linenumber_table = NULL;
	  linenumber_count = 0;
	}

      start_java_method (method);

      note_instructions (jcf, method);

      give_name_to_locals (jcf);

      /* Bump up start_label_pc_this_method so we get a unique label number
	 and reset highest_label_pc_this_method. */
      if (highest_label_pc_this_method >= 0)
	{
	  /* We adjust to the next multiple of 1000.  This is just a frill
	     so the last 3 digits of the label number match the bytecode
	     offset, which might make debugging marginally more convenient. */
	  start_label_pc_this_method
	    = ((((start_label_pc_this_method + highest_label_pc_this_method)
		 / 1000)
		+ 1)
	       * 1000);
	  highest_label_pc_this_method = -1;
	}

      /* Convert bytecode to trees.  */
      expand_byte_code (jcf, method);

      end_java_method ();
    }

  if (flag_emit_class_files)
    write_classfile (current_class);

  finish_class ();

  (*debug_hooks->end_source_file) (LOCATION_LINE (save_location));
  input_location = save_location;
}

/* Parse a source file, as pointed by the current value of INPUT_FILENAME. */

static void
parse_source_file_1 (tree real_file, const char *filename, FILE *finput)
{
  int save_error_count = java_error_count;

  /* Mark the file as parsed.  */
  HAS_BEEN_ALREADY_PARSED_P (real_file) = 1;

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

#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_ENTER, false, filename, 0);
  input_location = linemap_line_start (&line_table, 0, 125);
#else
  input_filename = filename;
  input_line = 0;
#endif
  ctxp->file_start_location = input_location;
  ctxp->filename = filename;

  jcf_dependency_add_file (input_filename, 0);

  /* Initialize the parser */
  java_init_lex (finput, current_encoding);
  java_parse_abort_on_error ();

  java_parse ();		    /* Parse and build partial tree nodes. */
  java_parse_abort_on_error ();
}

/* Process a parsed source file, resolving names etc. */

static void
parse_source_file_2 (void)
{
  int save_error_count = java_error_count;
  flag_verify_invocations = true;
  java_complete_class ();	    /* Parse unsatisfied class decl. */
  java_parse_abort_on_error ();
}

static void
parse_source_file_3 (void)
{
  int save_error_count = java_error_count;
  java_check_circular_reference (); /* Check on circular references */
  java_parse_abort_on_error ();
  java_fix_constructors ();	    /* Fix the constructors */
  java_parse_abort_on_error ();
  java_reorder_fields ();	    /* Reorder the fields */
}

void
add_predefined_file (tree name)
{
  predef_filenames = tree_cons (NULL_TREE, name, predef_filenames);
}

int
predefined_filename_p (tree node)
{
  tree iter;

  for (iter = predef_filenames; iter != NULL_TREE; iter = TREE_CHAIN (iter))
    {
      if (TREE_VALUE (iter) == node)
	return 1;
    }
  return 0;
}

/* Generate a function that does all static initialization for this 
   translation unit.  */

static void
java_emit_static_constructor (void)
{
  tree body = NULL;

  emit_register_classes (&body);
  write_resource_constructor (&body);

  if (body)
    cgraph_build_static_cdtor ('I', body, DEFAULT_INIT_PRIORITY);
}

void
java_parse_file (int set_yydebug ATTRIBUTE_UNUSED)
{
  int filename_count = 0;
  location_t save_location = input_location;
  char *file_list = NULL, *list, *next;
  tree node;
  FILE *finput = NULL;
  int in_quotes = 0;
 
  if (flag_filelist_file)
    {
      int avail = 2000;
      finput = fopen (main_input_filename, "r");
      if (finput == NULL)
	fatal_error ("can't open %s: %m", input_filename);
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
		fatal_error ("error closing %s: %m", input_filename);
	      *next = '\0';
	      break;
	    }
	  avail -= count;
	  next += count;
	}
      fclose (finput);
      finput = NULL;
      file_list = list;
    }
  else
    list = (char *) main_input_filename;

  while (list)
    {
      for (next = list; ; )
	{
	  char ch = *next;
	  if (flag_filelist_file && ! in_quotes
	      && (ch == '\n' || ch == '\r' || ch == '\t' || ch == ' '
		  || ch == '&') /* FIXME */)
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
	  if (flag_filelist_file && ch == '"')
	    {
	      in_quotes = ! in_quotes;
	      *next++ = '\0';
	      if (in_quotes) 
		list = next;
	      else 
		break;
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
	  node = get_identifier (list);

	  filename_count++;

	  /* Exclude file that we see twice on the command line. */
	     
	  if (IS_A_COMMAND_LINE_FILENAME_P (node))
	    duplicate_class_warning (IDENTIFIER_POINTER (node));
	  else
	    {
	      tree file_decl = build_decl (TRANSLATION_UNIT_DECL, node, NULL);
	      TREE_CHAIN (file_decl) = current_file_list;
	      current_file_list = file_decl;
	      IS_A_COMMAND_LINE_FILENAME_P (node) = 1;
	    }
	}
      list = next;
    }

  if (file_list != NULL)
    free (file_list);

  if (filename_count == 0)
    warning (0, "no input file specified");

  if (resource_name)
    {
      const char *resource_filename;
      
      /* Only one resource file may be compiled at a time.  */
      assert (TREE_CHAIN (current_file_list) == NULL);

      resource_filename = IDENTIFIER_POINTER (DECL_NAME (current_file_list));
      compile_resource_file (resource_name, resource_filename);

      goto finish;
    }

  current_jcf = main_jcf;
  current_file_list = nreverse (current_file_list);
  for (node = current_file_list; node; node = TREE_CHAIN (node))
    {
      unsigned char magic_string[4];
      char *real_path;
      uint32 magic = 0;
      tree name = DECL_NAME (node);
      tree real_file;
      const char *filename = IDENTIFIER_POINTER (name);

      /* Skip already parsed files */
      real_path = lrealpath (filename);
      real_file = get_identifier (real_path);
      free (real_path);
      if (HAS_BEEN_ALREADY_PARSED_P (real_file))
	continue;

      /* Close previous descriptor, if any */
      if (finput && fclose (finput))
	fatal_error ("can't close input file %s: %m", main_input_filename);
      
      finput = fopen (filename, "rb");
      if (finput == NULL)
	fatal_error ("can't open %s: %m", filename);

#ifdef IO_BUFFER_SIZE
      setvbuf (finput, xmalloc (IO_BUFFER_SIZE),
	       _IOFBF, IO_BUFFER_SIZE);
#endif

      /* Figure what kind of file we're dealing with */
      if (fread (magic_string, 1, 4, finput) == 4)
	{
	  fseek (finput, 0L, SEEK_SET);
	  magic = GET_u4 (magic_string);
	}
      if (magic == 0xcafebabe)
	{
	  CLASS_FILE_P (node) = 1;
	  current_jcf = ggc_alloc (sizeof (JCF));
	  JCF_ZERO (current_jcf);
	  current_jcf->read_state = finput;
	  current_jcf->filbuf = jcf_filbuf_from_stdio;
	  jcf_parse (current_jcf);
	  DECL_SOURCE_LOCATION (node) = file_start_location;
	  TYPE_JCF (current_class) = current_jcf;
	  if (CLASS_FROM_CURRENTLY_COMPILED_P (current_class))
	    {
	      /* We've already compiled this class.  */
	      duplicate_class_warning (filename);
	      continue;
	    }
	  CLASS_FROM_CURRENTLY_COMPILED_P (current_class) = 1;
	  TREE_TYPE (node) = current_class;
	}
      else if (magic == (JCF_u4)ZIPMAGIC)
	{
	  main_jcf = ggc_alloc (sizeof (JCF));
	  JCF_ZERO (main_jcf);
	  main_jcf->read_state = finput;
	  main_jcf->filbuf = jcf_filbuf_from_stdio;
#ifdef USE_MAPPED_LOCATION
	  linemap_add (&line_table, LC_ENTER, false, filename, 0);
	  input_location = linemap_line_start (&line_table, 0, 1);
#endif
	  if (open_in_zip (main_jcf, filename, NULL, 0) <  0)
	    fatal_error ("bad zip/jar file %s", filename);
	  localToFile = SeenZipFiles;
	  /* Register all the classes defined there.  */
	  process_zip_dir (main_jcf->read_state);
#ifdef USE_MAPPED_LOCATION
	  linemap_add (&line_table, LC_LEAVE, false, NULL, 0);
#endif
	  parse_zip_file_entries ();
	}
      else
	{
	  java_push_parser_context ();
	  java_parser_context_save_global ();

	  parse_source_file_1 (real_file, filename, finput);
	  java_parser_context_restore_global ();
	  java_pop_parser_context (1);
#ifdef USE_MAPPED_LOCATION
	  linemap_add (&line_table, LC_LEAVE, false, NULL, 0);
#endif
	}
    }

  for (ctxp = ctxp_for_generation;  ctxp;  ctxp = ctxp->next)
    {
      input_location = ctxp->file_start_location;
      parse_source_file_2 ();
    }

  for (ctxp = ctxp_for_generation; ctxp; ctxp = ctxp->next)
    {
      input_location = ctxp->file_start_location;
      parse_source_file_3 ();
    }

  for (node = current_file_list; node; node = TREE_CHAIN (node))
    {
      input_location = DECL_SOURCE_LOCATION (node);
      if (CLASS_FILE_P (node))
	{
	  /* FIXME: These two flags really should be independent.  We
	     should be able to compile fully binary compatible, but
	     with flag_verify_invocations on.  */
	  flag_verify_invocations = ! flag_indirect_dispatch;
	  output_class = current_class = TREE_TYPE (node);

	  current_jcf = TYPE_JCF (current_class);
	  layout_class (current_class);
	  load_inner_classes (current_class);
	  parse_class_file ();
	  JCF_FINISH (current_jcf);
	}
    }
  input_location = save_location;

  java_expand_classes ();
  if (java_report_errors () || flag_syntax_only)
    return;
    
  /* Expand all classes compiled from source.  */
  java_finish_classes ();

 finish:
  /* Arrange for any necessary initialization to happen.  */
  java_emit_static_constructor ();

  /* Only finalize the compilation unit after we've told cgraph which
     functions have their addresses stored.  */
  cgraph_finalize_compilation_unit ();
  cgraph_optimize ();
}


/* Return the name of the class corresponding to the name of the file
   in this zip entry.  The result is newly allocated using ALLOC.  */
static char *
compute_class_name (struct ZipDirectory *zdir)
{
  char *class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);
  char *class_name;
  int i;
  int filename_length = zdir->filename_length;

  while (filename_length > 2 && strncmp (class_name_in_zip_dir, "./", 2) == 0)
    {
      class_name_in_zip_dir += 2;
      filename_length -= 2;
    }

  filename_length -= strlen (".class");
  class_name = ALLOC (filename_length + 1);
  memcpy (class_name, class_name_in_zip_dir, filename_length);
  class_name [filename_length] = '\0';

  for (i = 0; i < filename_length; i++)
    if (class_name[i] == '/')
      class_name[i] = '.';

  return class_name;
}

/* Return 0 if we should skip this entry, 1 if it is a .class file, 2
   if it is a property file of some sort.  */
static int
classify_zip_file (struct ZipDirectory *zdir)
{
  char *class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

  if (zdir->filename_length > 6
      && !strncmp (&class_name_in_zip_dir[zdir->filename_length - 6],
		   ".class", 6))
    return 1;

  /* For now we drop the manifest, but not other information.  */
  if (zdir->filename_length == 20
      && !strncmp (class_name_in_zip_dir, "META-INF/MANIFEST.MF", 20))
    return 0;

  /* Drop directory entries.  */
  if (zdir->filename_length > 0
      && class_name_in_zip_dir[zdir->filename_length - 1] == '/')
    return 0;

  return 2;
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

      switch (classify_zip_file (zdir))
	{
	case 0:
	  continue;

	case 1:
	  {
	    char *class_name = compute_class_name (zdir);
	    class = lookup_class (get_identifier (class_name));
	    FREE (class_name);
	    current_jcf = TYPE_JCF (class);
	    output_class = current_class = class;

	    if (CLASS_FROM_CURRENTLY_COMPILED_P (current_class))
	      {
	        /* We've already compiled this class.  */
		duplicate_class_warning (current_jcf->filename);
		break;
	      }
	    
	    CLASS_FROM_CURRENTLY_COMPILED_P (current_class) = 1;

	    if (TYPE_DUMMY (class))
	      {
		/* This is a dummy class, and now we're compiling it
		   for real.  */
		abort ();
	      }

	    /* This is for a corner case where we have a superclass
	       but no superclass fields.  

	       This can happen if we earlier failed to lay out this
	       class because its superclass was still in the process
	       of being laid out; this occurs when we have recursive
	       class dependencies via inner classes.  Setting
	       TYPE_SIZE to null here causes CLASS_LOADED_P to return
	       false, so layout_class() will be called again.  */
	    if (TYPE_SIZE (class) && CLASSTYPE_SUPER (class)
		&& integer_zerop (TYPE_SIZE (class)))
	      TYPE_SIZE (class) = NULL_TREE;

	    if (! CLASS_LOADED_P (class))
	      {
		if (! CLASS_PARSED_P (class))
		  {
		    read_zip_member (current_jcf, zdir, localToFile);
		    jcf_parse (current_jcf);
		  }
		layout_class (current_class);
		load_inner_classes (current_class);
	      }

	    if (TYPE_SIZE (current_class) != error_mark_node)
	      {
		parse_class_file ();
		FREE (current_jcf->buffer); /* No longer necessary */
		/* Note: there is a way to free this buffer right after a
		   class seen in a zip file has been parsed. The idea is the
		   set its jcf in such a way that buffer will be reallocated
		   the time the code for the class will be generated. FIXME. */
	      }
	  }
	  break;

	case 2:
	  {
	    char *file_name, *class_name_in_zip_dir, *buffer;
	    JCF *jcf;
	    file_name = ALLOC (zdir->filename_length + 1);
	    class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);
	    strncpy (file_name, class_name_in_zip_dir, zdir->filename_length);
	    file_name[zdir->filename_length] = '\0';
	    jcf = ALLOC (sizeof (JCF));
	    JCF_ZERO (jcf);
	    jcf->read_state  = finput;
	    jcf->filbuf      = jcf_filbuf_from_stdio;
	    jcf->java_source = 0;
	    jcf->classname   = NULL;
	    jcf->filename    = file_name;
	    jcf->zipd        = zdir;

	    if (read_zip_member (jcf, zdir, localToFile) < 0)
	      fatal_error ("error while reading %s from zip file", file_name);

	    buffer = ALLOC (zdir->filename_length + 1 +
			    (jcf->buffer_end - jcf->buffer));
	    strcpy (buffer, file_name);
	    /* This is not a typo: we overwrite the trailing \0 of the
	       file name; this is just how the data is laid out.  */
	    memcpy (buffer + zdir->filename_length,
		    jcf->buffer, jcf->buffer_end - jcf->buffer);

	    compile_resource_data (file_name, buffer,
				   jcf->buffer_end - jcf->buffer);
	    JCF_FINISH (jcf);
	    FREE (jcf);
	    FREE (buffer);
	  }
	  break;

	default:
	  abort ();
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

      class_name_in_zip_dir = ZIPDIR_FILENAME (zdir);

      /* Here we skip non-class files; we handle them later.  */
      if (classify_zip_file (zdir) != 1)
	continue;

      class_name = compute_class_name (zdir);
      file_name  = ALLOC (zdir->filename_length+1);
      jcf = ggc_alloc (sizeof (JCF));
      JCF_ZERO (jcf);

      strncpy (file_name, class_name_in_zip_dir, zdir->filename_length);
      file_name [zdir->filename_length] = '\0';

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
init_jcf_parse (void)
{
  init_src_parse ();
}

#include "gt-java-jcf-parse.h"
