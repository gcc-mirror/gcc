/* Implement classes and message passing for Objective C.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Author: Steve Naroff.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 *      Purpose: This module implements the Objective-C 4.0 language.
 *
 *      compatibility issues (with the Stepstone translator):
 *
 *	- does not recognize the following 3.3 constructs.
 *	  @requires, @classes, @messages, = (...)
 *	- methods with variable arguments must conform to ANSI standard.
 *	- tagged structure definitions that appear in BOTH the interface
 *	  and implementation are not allowed.
 *      - public/private: all instance variables are public within the
 *        context of the implementation...I consider this to be a bug in
 *        the translator.
 *      - statically allocated objects are not supported. the user will
 *        receive an error if this service is requested.
 *
 *      code generation `options':
 *
 *      - OBJC_INT_SELECTORS, OBJC_SELECTORS_WITHOUT_LABELS, NEXT_OBJC_RUNTIME
 */

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "c-tree.h"
#include "c-lex.h"
#include "flags.h"
#include "objc-actions.h"
#include "input.h"

/* The GNU run time requires the selectors in a vector
   so it can store the operation numbers in them.  */
#ifndef NEXT_OBJC_RUNTIME
#define OBJC_SELECTORS_WITHOUT_LABELS
#endif

/* Define the special tree codes that we use.  */

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See objc-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *objc_tree_code_type[] = {
  "x",
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int objc_tree_code_length[] = {
  0,
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *objc_tree_code_name[] = {
  "@@dummy",
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Set up for use of obstacks.  */

#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* This obstack is used to accumulate the encoding of a data type.  */
static struct obstack util_obstack;
/* This points to the beginning of obstack contents,
   so we can free the whole contents.  */
char *util_firstobj;

/* for encode_method_def */
#include "rtl.h"

#define OBJC_VERSION	2

#define NULLT	(tree) 0

#define OBJC_ENCODE_INLINE_DEFS 	0
#define OBJC_ENCODE_DONT_INLINE_DEFS	1

/*** Private Interface (procedures) ***/

/* code generation */

static void synth_module_prologue ();
static char *build_module_descriptor ();
static tree init_module_descriptor ();
static void build_module_entry ();
static tree build_objc_method_call ();
static void build_message_selector_pool ();
static void build_selector_translation_table ();
static tree build_ivar_chain ();

static tree build_ivar_template ();
static tree build_method_template ();
static tree build_private_template ();
static void build_class_template ();
static void build_category_template ();
static tree build_super_template ();

static void synth_forward_declarations ();
static void generate_ivar_lists ();
static void generate_dispatch_tables ();
static void generate_shared_structures ();

static tree build_msg_pool_reference ();
static tree init_selector ();
static tree build_keword_selector ();
static tree synth_id_with_class_suffix ();

/* misc. bookkeeping */

typedef struct hashedEntry 	*hash;
typedef struct hashedAttribute  *attr;

struct hashedAttribute {
        attr    next;
        tree    value;
};
struct hashedEntry {
        attr    list;
	hash	next;
	tree 	key;
};
static void hash_init ();
static void hash_enter ();
static hash hash_lookup ();
static void hash_add_attr ();
static tree lookup_method ();
static tree lookup_instance_method_static ();
static tree lookup_class_method_static ();
static tree add_class ();
static int  add_selector_reference ();
static void add_class_reference ();
static int  add_objc_string ();

/* type encoding */

static void encode_aggregate ();
static void encode_bitfield ();
static void encode_type ();
static void encode_field_decl ();

static void really_start_method ();
static int  comp_method_with_proto ();
static int  comp_proto_with_proto ();
static tree get_arg_type_list ();
static tree expr_last ();

/* utilities for debugging and error diagnostics: */

static void warn_with_method ();
static void error_with_method ();
static void error_with_ivar ();
static char *gen_method_decl ();
static char *gen_declaration ();
static char *gen_declarator ();
static int is_complex_decl ();
static void adorn_decl ();
static void dump_interfaces ();

/*** Private Interface (data) ***/

/* reserved tag definitions: */

#define TYPE_ID			"id"
#define TAG_OBJECT		"objc_object"
#define TAG_CLASS		"objc_class"
#define TAG_SUPER		"objc_super"
#define TAG_SELECTOR		"objc_selector"

#define _TAG_CLASS		"_objc_class"
#define _TAG_IVAR		"_objc_ivar"
#define _TAG_IVAR_LIST		"_objc_ivar_list"
#define _TAG_METHOD		"_objc_method"
#define _TAG_METHOD_LIST	"_objc_method_list"
#define _TAG_CATEGORY		"_objc_category"
#define _TAG_MODULE		"_objc_module"
#define _TAG_SYMTAB		"_objc_symtab"
#define _TAG_SUPER		"_objc_super"

/* set by `continue_class ()' and checked by `is_public ()' */

#define TREE_STATIC_TEMPLATE(record_type) (TREE_PUBLIC(record_type))
#define TYPED_OBJECT(type) \
       (TREE_CODE (type) == RECORD_TYPE && TREE_STATIC_TEMPLATE (type))

/* some commonly used instances of "identifier_node". */

static tree self_id, _cmd_id;

static tree self_decl, _msg_decl, _msgSuper_decl;
static tree objc_getClass_decl, objc_getMetaClass_decl;

static tree super_type, selector_type, id_type, class_type;
static tree instance_type;

static tree interface_chain = NULLT;

/* chains to manage selectors that are referenced and defined in the module */

static tree cls_ref_chain = NULLT;	/* classes referenced */
static tree sel_ref_chain = NULLT;	/* selectors referenced */
static tree sel_refdef_chain = NULLT;	/* selectors references & defined */
static int  max_selector_index;		/* total # of selector referenced */

/* hash tables to manage the global pool of method prototypes */

static hash *nst_method_hash_list = 0;
static hash *cls_method_hash_list = 0;

/* the following are used when compiling a class implementation.
 *
 * implementation_template will normally be an anInterface, however if
 * none exists this will be equal to implementation_context...it is
 * set in start_class.
 */

/* backend data declarations */

static tree _OBJC_SYMBOLS_decl;
static tree 	_OBJC_INSTANCE_VARIABLES_decl, _OBJC_CLASS_VARIABLES_decl;
static tree 	_OBJC_INSTANCE_METHODS_decl, _OBJC_CLASS_METHODS_decl;
static tree 	_OBJC_CLASS_decl, _OBJC_METACLASS_decl;
#ifdef OBJC_SELECTORS_WITHOUT_LABELS
static tree 	_OBJC_SELECTOR_REFERENCES_decl;
#endif
static tree _OBJC_MODULES_decl;
static tree _OBJC_STRINGS_decl;

static tree implementation_context = NULLT,
	    implementation_template = NULLT;

struct imp_entry {
  struct imp_entry *next;
  tree imp_context;
  tree imp_template;
  tree class_decl;		/* _OBJC_CLASS_<my_name>; */
  tree meta_decl;		/* _OBJC_METACLASS_<my_name>; */
};
static struct imp_entry *imp_list = 0;
static int imp_count = 0;	/* `@implementation' */
static int cat_count = 0;	/* `@category' */

static tree objc_class_template, objc_category_template, _PRIVATE_record;
static tree _clsSuper_ref, __clsSuper_ref;

static tree objc_method_template, objc_ivar_template;
static tree objc_symtab_template, objc_module_template;
static tree objc_super_template, objc_object_reference;

static tree objc_object_id, objc_class_id;
static tree _OBJC_SUPER_decl;

static tree method_context = NULLT;
static int  method_slot = 0;	/* used by start_method_def */

#define BUFSIZE		512

static char *errbuf;	/* a buffer for error diagnostics */

extern char *strcpy (), *strcat ();

extern tree groktypename_in_parm_context ();

extern struct obstack permanent_obstack, *current_obstack,  *rtl_obstack;

/* data imported from toplev.c  */

extern char *dump_base_name;

/* Open and close the file for outputting class declarations, if requested.  */

int flag_gen_declaration = 0;

FILE *gen_declaration_file;

/* Warn if multiple methods are seen for the same selector, but with
   different argument types. */

int warn_selector = 0;

void
lang_init ()
{
  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  ungetc (check_newline (), finput);

  /* If gen_declaration desired, open the output file.  */
  if (flag_gen_declaration)
    {
      int dump_base_name_length = strlen (dump_base_name);
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".decl");
      gen_declaration_file = fopen (dumpname, "w");
      if (gen_declaration_file == 0)
	pfatal_with_name (dumpname);
    }

  if (doing_objc_thang)
    init_objc ();
}

void
objc_finish ()
{
  if (doing_objc_thang)
    finish_objc ();		/* Objective-C finalization */

  if (gen_declaration_file)
    fclose (gen_declaration_file);
}

void
lang_finish ()
{
}

int
lang_decode_option (p)
     char *p;
{
  if (!strcmp (p, "-lang-objc"))
    doing_objc_thang = 1;
  else if (!strcmp (p, "-gen-decls"))
    flag_gen_declaration = 1;
  else if (!strcmp (p, "-Wselector"))
    warn_selector = 1;
  else if (!strcmp (p, "-Wno-selector"))
    warn_selector = 0;
  else
    return c_decode_option (p);

  return 1;
}

static tree
define_decl (declarator, declspecs)
     tree declarator;
     tree declspecs;
{
  tree decl = start_decl (declarator, declspecs, 0);
  finish_decl (decl, NULLT, NULLT);
  return decl;
}

/*
 * rules for statically typed objects...called from `c-typeck.comptypes'.
 *
 * an assignment of the form `a' = `b' is permitted if:
 *
 *   - `a' is of type "id".
 *   - `a' and `b' are the same class type.
 *   - `a' and `b' are of class types A and B such that B is a descendant
 *     of A.
 */

int
maybe_objc_comptypes (lhs, rhs)
     tree lhs, rhs;
{
  if (doing_objc_thang)
    return objc_comptypes (lhs, rhs);
  return 0;
}

int
objc_comptypes (lhs, rhs)
     tree lhs;
     tree rhs;
{
  /* `id' = `<class> *', `<class> *' = `id' */

  if ((TYPE_NAME (lhs) == objc_object_id && TYPED_OBJECT (rhs))
      || (TYPED_OBJECT (lhs) && TYPE_NAME (rhs) == objc_object_id))
    return 1;

  /* `id' = `Class', `Class' = `id' */


  else if ((TYPE_NAME (lhs) == objc_object_id &&
	    TYPE_NAME (rhs) == objc_class_id) ||
	   (TYPE_NAME (lhs) == objc_class_id &&
	    TYPE_NAME (rhs) == objc_object_id))
    return 1;

  /* `<class> *' = `<class> *' */

  else if (TYPED_OBJECT (lhs) && TYPED_OBJECT (rhs))
    {
      tree lname = TYPE_NAME (lhs), rname = TYPE_NAME (rhs);

      if (lname == rname)
	return 1;
      else
	{
	  /* if the left hand side is a super class of the right hand side,
	     allow it...
	     */
	  tree rinter = lookup_interface (rname);

	  while (rinter)
	    {
	      if (lname == CLASS_SUPER_NAME (rinter))
		return 1;

	      rinter = lookup_interface (CLASS_SUPER_NAME (rinter));
	    }

	  return 0;
	}
    }
  else
    return 0;
}

/* Called from c-decl.c before all calls to rest_of_decl_compilation.  */

void
maybe_objc_check_decl (decl)
     tree decl;
{
  if (doing_objc_thang)
    objc_check_decl (decl);
}

void
objc_check_decl (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);
  static int alreadyWarned = 0;

  if (TREE_CODE (type) == RECORD_TYPE && TREE_STATIC_TEMPLATE (type))
    {
      if (!alreadyWarned)
	{
	  error ("GNU compiler does not support statically allocated objects");
	  alreadyWarned = 1;
	}
      error_with_decl (decl, "`%s' cannot be statically allocated");
    }
}

/* implement static typing. at this point, we know we have an interface... */

tree
get_static_reference (interface)
     tree interface;
{
  return xref_tag (RECORD_TYPE, CLASS_NAME (interface));
}

/* Create and push a decl for a built-in external variable or field NAME.
   CODE says which.
   TYPE is its data type.  */

static tree
create_builtin_decl (code, type, name)
     enum tree_code code;
     tree type;
     char *name;
{
  tree decl = build_decl (code, get_identifier (name), type);
  if (code == VAR_DECL)
    {
      TREE_STATIC (decl) = 1;
      make_decl_rtl (decl, 0, 1);
      pushdecl (decl);
    }
  return decl;
}

/*
 *	purpose: "play" parser, creating/installing representations
 *		 of the declarations that are required by Objective-C.
 *
 *	model:
 *
 *		type_spec--------->sc_spec
 *		(tree_list)        (tree_list)
 *		    |                  |
 *		    |                  |
 *		identifier_node    identifier_node
 */
static void
synth_module_prologue ()
{
  tree expr_decl, temp_type;

  /* defined in `objc.h' */
  objc_object_id = get_identifier (TAG_OBJECT);

  objc_object_reference = xref_tag (RECORD_TYPE, objc_object_id);

  id_type = build_pointer_type (objc_object_reference);

  objc_class_id = get_identifier (TAG_CLASS);
  
  class_type = build_pointer_type (xref_tag (RECORD_TYPE, objc_class_id));

  /* Declare type of selector-objects that represent an operation name.  */

#ifdef OBJC_INT_SELECTORS
  /* `unsigned int' */
  selector_type = unsigned_type_node;
#else
  /* `struct objc_selector *' */
  selector_type
    = build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier (TAG_SELECTOR)));
#endif /* not OBJC_INT_SELECTORS */

  /* struct objc_object *objc_msgSend (id, SEL, ...); */

  temp_type
    = build_function_type (id_type,
			   tree_cons (NULL_TREE, id_type,
				      tree_cons (NULLT, selector_type, NULLT)));

  _msg_decl = builtin_function ("objc_msgSend", temp_type, NOT_BUILT_IN, 0);

  /* struct objc_object *objc_msgSendSuper (void *, SEL, ...); */

  temp_type
    = build_function_type (id_type,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULLT, selector_type, NULLT)));

  _msgSuper_decl = builtin_function ("objc_msgSendSuper",
				     temp_type, NOT_BUILT_IN, 0);

  /* id objc_getClass (); */
  
  temp_type = build_function_type (id_type, NULLT);

  objc_getClass_decl
    = builtin_function ("objc_getClass", temp_type, NOT_BUILT_IN, 0);

  /* id objc_getMetaClass (); */

  objc_getMetaClass_decl
    = builtin_function ("objc_getMetaClass", temp_type, NOT_BUILT_IN, 0);

  /* extern SEL _OBJC_SELECTOR_REFERENCES[]; */

#ifdef OBJC_SELECTORS_WITHOUT_LABELS
  _OBJC_SELECTOR_REFERENCES_decl
    = create_builtin_decl (VAR_DECL, build_array_type (selector_type, NULLT),
			   "_OBJC_SELECTOR_REFERENCES");
#endif
}

/*
 * custom "build_string ()" which sets TREE_TYPE!
 */
static tree
my_build_string (len, str)
     int len;
     char *str;
{
  int wide_flag = 0;
  tree aString = build_string (len, str);
  /*
   *  some code from "combine_strings ()", which is local to c-parse.y.
   */
  if (TREE_TYPE (aString) == int_array_type_node)
    wide_flag = 1;

  TREE_TYPE (aString) =
    build_array_type (wide_flag ? integer_type_node : char_type_node,
		      build_index_type (build_int_2 (len - 1, 0)));

  TREE_CONSTANT (aString) = 1;	/* puts string in the ".text" segment */
  TREE_STATIC (aString) = 1;

  return aString;
}

/* Take care of defining and initializing _OBJC_SYMBOLS.  */

/* Predefine the following data type:

	struct _objc_symtab {
		long sel_ref_cnt;
		SEL *refs;
		short cls_def_cnt;
		short cat_def_cnt;
		void *defs[cls_def_cnt + cat_def_cnt];
	}; */

static void
build_objc_symtab_template ()
{
  tree field_decl, field_decl_chain, index;

  objc_symtab_template = start_struct (RECORD_TYPE, get_identifier (_TAG_SYMTAB));

  /* long sel_ref_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    long_integer_type_node,
				    "sel_ref_cnt");
  field_decl_chain = field_decl;

  /* SEL *refs; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    build_pointer_type (selector_type),
				    "refs");
  chainon (field_decl_chain, field_decl);

  /* short cls_def_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    short_integer_type_node,
				    "cls_def_cnt");
  chainon (field_decl_chain, field_decl);

  /* short cat_def_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    short_integer_type_node,
				    "cat_def_cnt");
  chainon (field_decl_chain, field_decl);

  /* void *defs[cls_def_cnt + cat_def_cnt]; */

  index = build_index_type (build_int_2 (imp_count + cat_count - 1, 0));
  field_decl = create_builtin_decl (FIELD_DECL,
				    build_array_type (ptr_type_node, index),
				    "defs");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_symtab_template, field_decl_chain);
}

/* Create the initial value for the `defs' field of _objc_symtab.
   This is a CONSTRUCTOR.  */

static tree
init_def_list ()
{
  tree expr, initlist = NULLT;
  struct imp_entry *impent;

  if (imp_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == IMPLEMENTATION_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULLT, expr, initlist);
	  }
      }

  if (cat_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CATEGORY_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULLT, expr, initlist);
	  }
      }
  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/* Construct the initial value for all of _objc_symtab.  */

static tree
init_objc_symtab ()
{
  tree initlist;

  /* sel_ref_cnt = { ..., 5, ... } */

  if (sel_ref_chain)
    initlist = build_tree_list (NULLT, build_int_2 (max_selector_index, 0));
  else
    initlist = build_tree_list (NULLT, build_int_2 (0, 0));

  /* refs = { ..., _OBJC_SELECTOR_REFERENCES, ... } */

#ifndef OBJC_SELECTORS_WITHOUT_LABELS
  initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
#else
  if (sel_ref_chain)
    initlist = tree_cons (NULLT, _OBJC_SELECTOR_REFERENCES_decl, initlist);
  else
    initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
#endif

  /* cls_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULLT, build_int_2 (imp_count, 0), initlist);

  /* cat_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULLT, build_int_2 (cat_count, 0), initlist);

  /* cls_def = { ..., { &Foo, &Bar, ...}, ... } */

  if (imp_count || cat_count)
    initlist = tree_cons (NULLT, init_def_list (), initlist);

  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/* Push forward-declarations of all the categories
   so that init_def_list can use them in a CONSTRUCTOR.  */

static void
forward_declare_categories ()
{
  struct imp_entry *impent;
  tree sav = implementation_context;
  for (impent = imp_list; impent; impent = impent->next)
    {
      if (TREE_CODE (impent->imp_context) == CATEGORY_TYPE)
	{
	  /* Set an invisible arg to synth_id_with_class_suffix.  */
	  implementation_context = impent->imp_context;
	  impent->class_decl
	    = create_builtin_decl (VAR_DECL, objc_category_template,
				   IDENTIFIER_POINTER (synth_id_with_class_suffix ("_OBJC_CATEGORY")));
	}
    }
  implementation_context = sav;
}

/* Create the declaration of _OBJC_SYMBOLS, with type `strict _objc_symtab'
   and initialized appropriately.  */

static void
generate_objc_symtab_decl ()
{
  tree sc_spec;

  if (!objc_category_template)
    build_category_template ();

  /* forward declare categories */
  if (cat_count)
    forward_declare_categories ();

  if (!objc_symtab_template)
    build_objc_symtab_template ();

  sc_spec = build_tree_list (NULLT, ridpointers[(int) RID_STATIC]);

  _OBJC_SYMBOLS_decl = start_decl (get_identifier ("_OBJC_SYMBOLS"),
				   tree_cons (NULLT, objc_symtab_template, sc_spec), 1);

  finish_decl (_OBJC_SYMBOLS_decl, init_objc_symtab (), NULLT);
}

/*
 *	tree_node------->tree_node----->...
 *          |                |
 *          | (value)        | (value)
 *          |                |
 *	  expr             expr
 */
static tree
init_module_descriptor ()
{
  tree initlist, expr;

  /* version = { 1, ... } */

  expr = build_int_2 (OBJC_VERSION, 0);
  initlist = build_tree_list (NULLT, expr);

  /* size = { ..., sizeof (struct objc_module), ... } */

  expr = build_int_2 (TREE_INT_CST_LOW (TYPE_SIZE (objc_module_template)) /
		      BITS_PER_UNIT, 0);
  initlist = tree_cons (NULLT, expr, initlist);

  /* name = { ..., "foo.m", ... } */

  expr = build_msg_pool_reference (
				   add_objc_string (get_identifier (input_filename)));
  initlist = tree_cons (NULLT, expr, initlist);

  /* symtab = { ..., _OBJC_SYMBOLS, ... } */

  if (_OBJC_SYMBOLS_decl)
    expr = build_unary_op (ADDR_EXPR, _OBJC_SYMBOLS_decl, 0);
  else
    expr = build_int_2 (0, 0);
  initlist = tree_cons (NULLT, expr, initlist);

  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/* Write out the data structures to describe Objective C classes defined.
   If appropriate, compile and output a setup function to initialize them.
   Return a string which is the name of a function to call to initialize
   the Objective C data structures for this file (and perhaps for other files
   also).  */

static char *
build_module_descriptor ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_module_template = start_struct (RECORD_TYPE, get_identifier (_TAG_MODULE));

  /* long version; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("version");
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* long  size; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("size");
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* char  *name; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("name"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_symtab *symtab; */

  decl_specs = get_identifier (_TAG_SYMTAB);
  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE, decl_specs));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("symtab"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_module_template, field_decl_chain);

  /* create an instance of "objc_module" */

  decl_specs = tree_cons (NULLT, objc_module_template,
			  build_tree_list (NULLT, ridpointers[(int) RID_STATIC]));

  _OBJC_MODULES_decl = start_decl (get_identifier ("_OBJC_MODULES"),
				   decl_specs, 1);

  finish_decl (_OBJC_MODULES_decl, init_module_descriptor (), NULLT);

  /* Mark the decl to avoid "defined but not used" warning. */
  DECL_IN_SYSTEM_HEADER (_OBJC_MODULES_decl) = 1;

  /* Generate a constructor call for the module descriptor. 
     This code was generated by reading the grammar rules
     of c-parse.y;  Therefore, it may not be the most efficient
     way of generating the requisite code. */
#ifndef NEXT_OBJC_RUNTIME
  {
    tree parms, function_decl, decelerator, void_list_node;
    tree function_type;
    char *buf;
    char *global_object_name = 0;
    tree t;

    /* Use a global object (which is already required to be unique over
       the program) rather than the file name (which imposes extra
       constraints).  -- Raeburn@MIT.EDU, 10 Jan 1990.  */

    /* Find the name of some global object defined in this file.  */
    for (t = getdecls (); t; t = TREE_CHAIN (t))
      if (TREE_PUBLIC (t) && !DECL_EXTERNAL (t) && DECL_INITIAL (t) != 0)
	{
	  global_object_name = IDENTIFIER_POINTER (DECL_NAME (t));
	  break;
	}

    /* If none, use the name of the file.  */
    if (!global_object_name)
      {
	char *p, *q;
	global_object_name
	  = (char *) alloca (strlen (main_input_filename) + 1);

	p = main_input_filename;
	q = global_object_name;

	/* Replace any weird characters in the file name.  */
	for (; *p; p++)
	  if (! ((*p >= '0' && *p <= '9')
		 || (*p >= 'A' && *p <= 'Z')
		 || (*p >= 'a' && *p <= 'z')))
	    *q++ = '_';
	  else
	    *q++ = *p;
	*q = 0;
      }

    /* Make the constructor name from the name we have found.  */
    buf = (char *) xmalloc (sizeof (CONSTRUCTOR_NAME_FORMAT)
			    + strlen (global_object_name));
    sprintf (buf, CONSTRUCTOR_NAME_FORMAT, global_object_name);

    /* Declare void __objc_execClass (void*); */

    void_list_node = build_tree_list (NULL_TREE, void_type_node);
    function_type
      = build_function_type (void_type_node,  
			     tree_cons (NULL_TREE, ptr_type_node,  
					void_list_node));
    function_decl = build_decl (FUNCTION_DECL,  
				get_identifier ("__objc_execClass"),  
				function_type);
    DECL_EXTERNAL (function_decl) = 1;
    TREE_PUBLIC (function_decl) = 1;
    pushdecl (function_decl);
    rest_of_decl_compilation (function_decl, 0, 0, 0);

    parms
      = build_tree_list (NULLT,
			 build_unary_op (ADDR_EXPR, _OBJC_MODULES_decl, 0));
    decelerator = build_function_call (function_decl, parms);

    /* void __objc_file_init () {objc_execClass(&L_OBJC_MODULES);}  */

    start_function (void_list_node,
		    build_parse_node (CALL_EXPR, get_identifier (buf),
				      /* This has the format of the output
					 of get_parm_info.  */
				      tree_cons (NULL_TREE, NULL_TREE,
						 void_list_node),
				      NULL_TREE),
		    0, 0);
#if 0 /* This should be turned back on later
	 for the systems where collect is not needed.  */
    /* Make these functions nonglobal
       so each file can use the same name.  */
    TREE_PUBLIC (current_function_decl) = 0;
#endif
    TREE_USED (current_function_decl) = 1;
    store_parm_decls ();

    assemble_external (function_decl);
    c_expand_expr_stmt (decelerator);

    finish_function (0);

    /* Return the name of the constructor function.  */
    return buf;
  }
#else /* NEXT_OBJC_RUNTIME */
  return 0;
#endif /* NEXT_OBJC_RUNTIME */
}

/* extern const char _OBJC_STRINGS[]; */

static void
generate_forward_declaration_to_string_table ()
{
  tree sc_spec, decl_specs, expr_decl;

  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_EXTERN], NULLT);
  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_CHAR], sc_spec);

  expr_decl = build_nt (ARRAY_REF, get_identifier ("_OBJC_STRINGS"), NULLT);

  _OBJC_STRINGS_decl = define_decl (expr_decl, decl_specs);
}

/* static char _OBJC_STRINGS[] = "..."; */

static void
build_message_selector_pool ()
{
  tree sc_spec, decl_specs, expr_decl;
  tree chain, string_expr;
  int goolengthtmp = 0, msg_pool_size = 0;
  char *string_goo;

  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_STATIC], NULLT);
  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_CHAR], sc_spec);

  expr_decl = build_nt (ARRAY_REF, get_identifier ("_OBJC_STRINGS"), NULLT);

  _OBJC_STRINGS_decl = start_decl (expr_decl, decl_specs, 1);

  for (chain = sel_refdef_chain; chain; chain = TREE_CHAIN (chain))
    msg_pool_size += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;

  msg_pool_size++;

  string_goo = (char *)xmalloc (msg_pool_size);
  bzero (string_goo, msg_pool_size);

  for (chain = sel_refdef_chain; chain; chain = TREE_CHAIN (chain))
    {
      strcpy (string_goo + goolengthtmp, IDENTIFIER_POINTER (TREE_VALUE (chain)));
      goolengthtmp += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;
    }

  string_expr = my_build_string (msg_pool_size, string_goo);

  finish_decl (_OBJC_STRINGS_decl, string_expr, NULLT);
}

/*
 * synthesize the following expr: (char *)&_OBJC_STRINGS[<offset>]
 *
 * the cast stops the compiler from issuing the following message:
 *
 * grok.m: warning: initialization of non-const * pointer from const *
 * grok.m: warning: initialization between incompatible pointer types
 */
static tree
build_msg_pool_reference (offset)
     int offset;
{
  tree expr = build_int_2 (offset, 0);
  tree cast;

  expr = build_array_ref (_OBJC_STRINGS_decl, expr);
  expr = build_unary_op (ADDR_EXPR, expr, 0);

  cast = build_tree_list (build_tree_list (NULLT, ridpointers[(int) RID_CHAR]),
			  build1 (INDIRECT_REF, NULLT, NULLT));
  TREE_TYPE (expr) = groktypename (cast);
  return expr;
}

#ifndef OBJC_SELECTORS_WITHOUT_LABELS
static tree
build_selector_reference (idx)
      int idx;
{
  tree ref, decl, name, ident;
  char buf[256];
  struct obstack *save_current_obstack = current_obstack;
  struct obstack *save_rtl_obstack = rtl_obstack;

  sprintf (buf, "_OBJC_SELECTOR_REFERENCES_%d", idx);

  /* new stuff */
  rtl_obstack = current_obstack = &permanent_obstack;
  ident = get_identifier (buf);

  if (IDENTIFIER_GLOBAL_VALUE (ident))
    decl = IDENTIFIER_GLOBAL_VALUE (ident); /* set by pushdecl() */
  else 
    {
      decl = build_decl (VAR_DECL, ident, selector_type);
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      TREE_USED (decl) = 1;
  
      make_decl_rtl (decl, 0, 1); /* usually called from `rest_of_decl_compilation' */
      pushdecl_top_level (decl);  /* our `extended/custom' pushdecl in c-decl.c */
    }
  current_obstack = save_current_obstack;
  rtl_obstack = save_rtl_obstack;

  return decl;
}
#endif

static tree
init_selector (offset)
     int offset;
{
  tree expr = build_msg_pool_reference (offset);
  TREE_TYPE (expr) = selector_type; /* cast */
  return expr;
}

static void
build_selector_translation_table ()
{
  tree sc_spec, decl_specs, expr_decl;
  tree chain, initlist = NULLT;
  int offset = 0;
#ifndef OBJC_SELECTORS_WITHOUT_LABELS
  tree decl, var_decl;
  int idx = 0;
  char buf[256];
#else
  /* The corresponding pop_obstacks is in finish_decl,
     called at the end of this function.  */
  push_obstacks_nochange ();
#endif

  for (chain = sel_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      tree expr;

#ifndef OBJC_SELECTORS_WITHOUT_LABELS
      sprintf (buf, "_OBJC_SELECTOR_REFERENCES_%d", idx);
      sc_spec = build_tree_list (NULLT, ridpointers[(int) RID_STATIC]);

      /* static SEL _OBJC_SELECTOR_REFERENCES_n = ...; */
      decl_specs = tree_cons (NULLT, selector_type, sc_spec);
      var_decl = get_identifier (buf);

      /* the `decl' that is returned from start_decl is the one that we
	* forward declared in `build_selector_reference()'
	*/
      decl = start_decl (var_decl, decl_specs, 1); 
#endif

      expr = init_selector (offset);

      /* add one for the '\0' character */
      offset += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;

#ifndef OBJC_SELECTORS_WITHOUT_LABELS
      finish_decl (decl, expr, NULLT);
      idx++;
#else
      initlist = tree_cons (NULLT, expr, initlist);
#endif
    }

#ifdef OBJC_SELECTORS_WITHOUT_LABELS
  /* Cause the variable and its initial value to be actually output.  */
  DECL_EXTERNAL (_OBJC_SELECTOR_REFERENCES_decl) = 0;
  TREE_STATIC (_OBJC_SELECTOR_REFERENCES_decl) = 1;
  /* NULL terminate the list and fix the decl for output. */
  initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
  DECL_INITIAL (_OBJC_SELECTOR_REFERENCES_decl) = (tree) 1;
  initlist = build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
  finish_decl (_OBJC_SELECTOR_REFERENCES_decl, initlist, NULLT);
#endif
}

static void
add_class_reference (ident)
     tree ident;
{
  tree chain;

  if (chain = cls_ref_chain)
    {
      tree tail;
      do
        {
	  if (ident == TREE_VALUE (chain))
	    return;

	  tail = chain;
	  chain = TREE_CHAIN (chain);
        }
      while (chain);

      /* append to the end of the list */
      TREE_CHAIN (tail) = perm_tree_cons (NULLT, ident, NULLT);
    }
  else
    cls_ref_chain = perm_tree_cons (NULLT, ident, NULLT);
}

/*
 * sel_ref_chain is a list whose "value" fields will be instances of
 * identifier_node that represent the selector.
 */
static int
add_selector_reference (ident)
     tree ident;
{
  tree chain;
  int index = 0;

  /* this adds it to sel_refdef_chain, the global pool of selectors */
  add_objc_string (ident);

  if (chain = sel_ref_chain)
    {
      tree tail;
      do
        {
	  if (ident == TREE_VALUE (chain))
	    return index;

	  index++;
	  tail = chain;
	  chain = TREE_CHAIN (chain);
        }
      while (chain);

      /* append to the end of the list */
      TREE_CHAIN (tail) = perm_tree_cons (NULLT, ident, NULLT);
    }
  else
    sel_ref_chain = perm_tree_cons (NULLT, ident, NULLT);

  max_selector_index++;
  return index;
}

/*
 * sel_refdef_chain is a list whose "value" fields will be instances of
 * identifier_node that represent the selector. It returns the offset of
 * the selector from the beginning of the _OBJC_STRINGS pool. This offset
 * is typically used by "init_selector ()" during code generation.
 */
static int
add_objc_string (ident)
     tree ident;
{
  tree chain;
  int offset = 0;

  if (chain = sel_refdef_chain)
    {
      tree tail;
      do
        {
	  if (ident == TREE_VALUE (chain))
	    return offset;

	  /* add one for the '\0' character */
	  offset += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;
	  tail = chain;
	  chain = TREE_CHAIN (chain);
        }
      while (chain);

      /* append to the end of the list */
      TREE_CHAIN (tail) = perm_tree_cons (NULLT, ident, NULLT);
    }
  else
    sel_refdef_chain = perm_tree_cons (NULLT, ident, NULLT);

  return offset;
}

tree
lookup_interface (ident)
     tree ident;
{
  tree chain;

  for (chain = interface_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == CLASS_NAME (chain))
	return chain;
    }
  return NULLT;
}

static tree
objc_copy_list (list, head)
     tree list;
     tree *head;
{
  tree newlist = NULL_TREE, tail = NULL_TREE;

  while (list)
    {
      tail = copy_node (list);

      /* the following statement fixes a bug when inheriting instance
	 variables that are declared to be bitfields. finish_struct () expects
	 to find the width of the bitfield in DECL_INITIAL (), which it
	 nulls out after processing the decl of the super class...rather
	 than change the way finish_struct () works (which is risky),
	 I create the situation it expects...s.naroff (7/23/89).
	 */
      if (DECL_BIT_FIELD (tail) && DECL_INITIAL (tail) == 0)
	DECL_INITIAL (tail) = build_int_2 (DECL_FIELD_SIZE (tail), 0);

      newlist = chainon (newlist, tail);
      list = TREE_CHAIN (list);
    }
  *head = newlist;
  return tail;
}

/* used by:
 * build_private_template (), get_class_ivars (), and get_static_reference ().
 */
static tree
build_ivar_chain (interface)
     tree interface;
{
  tree my_name, super_name, ivar_chain;

  my_name = CLASS_NAME (interface);
  super_name = CLASS_SUPER_NAME (interface);

  /* "leaf" ivars never get copied...there is no reason to. */
  ivar_chain = CLASS_IVARS (interface);

  while (super_name)
    {
      tree op1;
      tree super_interface = lookup_interface (super_name);

      if (!super_interface)
        {
	  /* fatal did not work with 2 args...should fix */
	  error ("Cannot find interface declaration for `%s', superclass of `%s'",
		 IDENTIFIER_POINTER (super_name), IDENTIFIER_POINTER (my_name));
	  exit (34);
        }
      if (super_interface == interface)
        {
          fatal ("Circular inheritance in interface declaration for `%s'",
                 IDENTIFIER_POINTER (super_name));
        }
      interface = super_interface;
      my_name = CLASS_NAME (interface);
      super_name = CLASS_SUPER_NAME (interface);

      op1 = CLASS_IVARS (interface);
      if (op1)
        {
	  tree head, tail = objc_copy_list (op1, &head);

	  /* prepend super class ivars...make a copy of the list, we
	   * do not want to alter the original.
	   */
	  TREE_CHAIN (tail) = ivar_chain;
	  ivar_chain = head;
        }
    }
  return ivar_chain;
}

/*
 *  struct <classname> {
 *    struct objc_class *isa;
 *    ...
 *  };
 */
static tree
build_private_template (class)
     tree class;
{
  tree ivar_context;

  if (CLASS_STATIC_TEMPLATE (class))
    {
      _PRIVATE_record = CLASS_STATIC_TEMPLATE (class);
      ivar_context = TYPE_FIELDS (CLASS_STATIC_TEMPLATE (class));
    }
  else
    {
      _PRIVATE_record = start_struct (RECORD_TYPE, CLASS_NAME (class));

      ivar_context = build_ivar_chain (class);

      finish_struct (_PRIVATE_record, ivar_context);

      CLASS_STATIC_TEMPLATE (class) = _PRIVATE_record;

      /* mark this record as class template - for class type checking */
      TREE_STATIC_TEMPLATE (_PRIVATE_record) = 1;
    }
  instance_type = groktypename (
				build_tree_list (build_tree_list (NULLT, _PRIVATE_record),
						 build1 (INDIRECT_REF, NULLT, NULLT)));
  return ivar_context;
}

/*
 *  struct objc_category {
 *    char *category_name;
 *    char *class_name;
 *    struct objc_method_list *instance_methods;
 *    struct objc_method_list *class_methods;
 *  };
 */
static void
build_category_template ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_category_template = start_struct (RECORD_TYPE,
					 get_identifier (_TAG_CATEGORY));
  /* char *category_name; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("category_name"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* char *class_name; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("class_name"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *instance_methods; */

  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE,
						 get_identifier (_TAG_METHOD_LIST)));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("instance_methods"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *class_methods; */

  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE,
						 get_identifier (_TAG_METHOD_LIST)));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("class_methods"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_category_template, field_decl_chain);
}

/*
 *  struct objc_class {
 *    struct objc_class *isa;
 *    struct objc_class *super_class;
 *    char *name;
 *    long version;
 *    long info;
 *    long instance_size;
 *    struct objc_ivar_list *ivars;
 *    struct objc_method_list *methods;
 *    struct objc_cache *cache;
 *  };
 */
static void
build_class_template ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_class_template = start_struct (RECORD_TYPE, get_identifier (_TAG_CLASS));

  /* struct objc_class *isa; */

  decl_specs = build_tree_list (NULLT, objc_class_template);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("isa"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* struct objc_class *super_class; */

  decl_specs = build_tree_list (NULLT, objc_class_template);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("super_class"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* char *name; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("name"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* long version; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("version");
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* long info; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("info");
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* long instance_size; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("instance_size");
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_ivar_list *ivars; */

  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE,
						 get_identifier (_TAG_IVAR_LIST)));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("ivars"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *methods; */

  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE,
						 get_identifier (_TAG_METHOD_LIST)));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("methods"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_cache *cache; */

  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE,
						 get_identifier ("objc_cache")));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("cache"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_class_template, field_decl_chain);
}

/*
 * generate appropriate forward declarations for an implementation
 */
static void
synth_forward_declarations ()
{
  tree sc_spec, decl_specs, factory_id, anId;

  /* extern struct objc_class _OBJC_CLASS_<my_name>; */

  anId = synth_id_with_class_suffix ("_OBJC_CLASS");

  sc_spec = build_tree_list (NULLT, ridpointers[(int) RID_EXTERN]);
  decl_specs = tree_cons (NULLT, objc_class_template, sc_spec);
  _OBJC_CLASS_decl = define_decl (anId, decl_specs);

  /* extern struct objc_class _OBJC_METACLASS_<my_name>; */

  anId = synth_id_with_class_suffix ("_OBJC_METACLASS");

  _OBJC_METACLASS_decl = define_decl (anId, decl_specs);

  /* pre-build the following entities - for speed/convenience. */

  anId = get_identifier ("super_class");
  _clsSuper_ref = build_component_ref (_OBJC_CLASS_decl, anId);
  __clsSuper_ref = build_component_ref (_OBJC_METACLASS_decl, anId);
}

static void
error_with_ivar (message, decl, rawdecl)
     char *message;
     tree decl;
     tree rawdecl;
{
  count_error (0);
  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
  bzero (errbuf, BUFSIZE);
  fprintf (stderr, "%s `%s'\n", message, gen_declaration (rawdecl, errbuf));
}

#define USERTYPE(t)	(TREE_CODE (t) == RECORD_TYPE || \
			 TREE_CODE (t) == UNION_TYPE ||  \
			 TREE_CODE (t) == ENUMERAL_TYPE)

static void
check_ivars (inter, imp)
     tree inter;
     tree imp;
{
  tree intdecls = CLASS_IVARS (inter);
  tree impdecls = CLASS_IVARS (imp);
  tree rawintdecls = CLASS_RAW_IVARS (inter);
  tree rawimpdecls = CLASS_RAW_IVARS (imp);

  while (1)
    {
      tree t1, t2;

      if (intdecls == 0 && impdecls == 0)
	break;
      if (intdecls == 0 || impdecls == 0)
	{
	  error ("inconsistent instance variable specification");
	  break;
	}
      t1 = TREE_TYPE (intdecls); t2 = TREE_TYPE (impdecls);

      if (!comptypes (t1, t2))
	{
	  if (DECL_NAME (intdecls) == DECL_NAME (impdecls))
	    {
	      error_with_ivar ("conflicting instance variable type",
			       impdecls, rawimpdecls);
	      error_with_ivar ("previous declaration of",
			       intdecls, rawintdecls);
	    }
	  else			/* both the type and the name don't match */
	    {
	      error ("inconsistent instance variable specification");
	      break;
	    }
	}
      else if (DECL_NAME (intdecls) != DECL_NAME (impdecls))
	{
	  error_with_ivar ("conflicting instance variable name",
			   impdecls, rawimpdecls);
	  error_with_ivar ("previous declaration of",
			   intdecls, rawintdecls);
	}
      intdecls = TREE_CHAIN (intdecls);
      impdecls = TREE_CHAIN (impdecls);
      rawintdecls = TREE_CHAIN (rawintdecls);
      rawimpdecls = TREE_CHAIN (rawimpdecls);
    }
}

/* Set super_type to the data type node for struct objc_super *,
   first defining struct objc_super itself.
   This needs to be done just once per compilation.  */

static tree
build_super_template ()
{
  tree record, decl_specs, field_decl, field_decl_chain;

  record = start_struct (RECORD_TYPE, get_identifier (_TAG_SUPER));

  /* struct objc_object *self; */

  decl_specs = build_tree_list (NULLT, objc_object_reference);
  field_decl = get_identifier ("self");
  field_decl = build1 (INDIRECT_REF, NULLT, field_decl);
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* struct objc_class *class; */

  decl_specs = get_identifier (_TAG_CLASS);
  decl_specs = build_tree_list (NULLT, xref_tag (RECORD_TYPE, decl_specs));
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("class"));

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (record, field_decl_chain);

  /* `struct objc_super *' */
  super_type = groktypename (build_tree_list (build_tree_list (NULLT, record),
					      build1 (INDIRECT_REF, NULLT, NULLT)));
  return record;
}

/*
 * 	struct objc_ivar {
 *		char *ivar_name;
 *		char *ivar_type;
 *		int ivar_offset;
 *	};
 */
static tree
build_ivar_template ()
{
  tree objc_ivar_id, objc_ivar_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_id = get_identifier (_TAG_IVAR);
  objc_ivar_record = start_struct (RECORD_TYPE, objc_ivar_id);

  /* char *ivar_name; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("ivar_name"));

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* char *ivar_type; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("ivar_type"));

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* int ivar_offset; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("ivar_offset");

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_record, field_decl_chain);

  return objc_ivar_record;
}

/*
 * 	struct {
 *		int ivar_count;
 *		struct objc_ivar ivar_list[ivar_count];
 *	};
 */
static tree
build_ivar_list_template (list_type, size)
     tree list_type;
     int size;
{
  tree objc_ivar_list_id, objc_ivar_list_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULLT);

  /* int ivar_count; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("ivar_count");

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* struct objc_ivar ivar_list[]; */

  decl_specs = build_tree_list (NULLT, list_type);
  field_decl = build_nt (ARRAY_REF, get_identifier ("ivar_list"),
			 build_int_2 (size, 0));

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain);

  return objc_ivar_list_record;
}

/*
 * 	struct {
 *		int method_next;
 *		int method_count;
 *		struct objc_method method_list[method_count];
 *	};
 */
static tree
build_method_list_template (list_type, size)
     tree list_type;
     int size;
{
  tree objc_ivar_list_id, objc_ivar_list_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULLT);

  /* int method_next; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("method_next");

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  /* int method_count; */

  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("method_count");

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method method_list[]; */

  decl_specs = build_tree_list (NULLT, list_type);
  field_decl = build_nt (ARRAY_REF, get_identifier ("method_list"),
			 build_int_2 (size, 0));

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain);

  return objc_ivar_list_record;
}

static tree
build_ivar_list_initializer (field_decl, size)
     tree field_decl;
     int *size;
{
  tree initlist = NULLT;

  do
    {
      int offset;

    /* set name */
    if (DECL_NAME (field_decl))
      {
        offset = add_objc_string (DECL_NAME (field_decl));
        initlist = tree_cons (NULLT, build_msg_pool_reference (offset), initlist);
      }
    else
      {
        /* unnamed bit-field ivar (yuck). */
        initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
      }

      /* set type */
      encode_field_decl (field_decl, OBJC_ENCODE_DONT_INLINE_DEFS);
      offset = add_objc_string (get_identifier (obstack_finish (&util_obstack)));
      obstack_free (&util_obstack, util_firstobj);

      initlist = tree_cons (NULLT, build_msg_pool_reference (offset), initlist);

      /* set offset */
      initlist = tree_cons (NULLT,
			    build_int_2 (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field_decl)) / BITS_PER_UNIT, 0),
			    
			    initlist);
      (*size)++;
      field_decl = TREE_CHAIN (field_decl);
    }
  while (field_decl);

  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

static tree
generate_ivars_list (type, name, size, list)
     tree type;
     char *name;
     int size;
     tree list;
{
  tree sc_spec, decl_specs, decl, initlist;

  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_STATIC], NULLT);
  decl_specs = tree_cons (NULLT, type, sc_spec);

  decl = start_decl (synth_id_with_class_suffix (name), decl_specs, 1);

  initlist = build_tree_list (NULLT, build_int_2 (size, 0));
  initlist = tree_cons (NULLT, list, initlist);

  finish_decl (decl, build_nt (CONSTRUCTOR, NULLT, nreverse (initlist)), NULLT);

  return decl;
}

static void
generate_ivar_lists ()
{
  tree initlist, ivar_list_template, chain;
  tree cast, variable_length_type;
  int size;

  if (!objc_ivar_template)
    objc_ivar_template = build_ivar_template ();

  cast = build_tree_list (build_tree_list (NULLT, xref_tag (RECORD_TYPE,
							    get_identifier (_TAG_IVAR_LIST))), NULLT);
  variable_length_type = groktypename (cast);

  /* only generate class variables for the root of the inheritance
     hierarchy since these will be the same for every class */

  if (CLASS_SUPER_NAME (implementation_template) == NULLT
      && (chain = TYPE_FIELDS (objc_class_template)))
    {
      size = 0;
      initlist = build_ivar_list_initializer (chain, &size);

      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);

      _OBJC_CLASS_VARIABLES_decl =
	generate_ivars_list (ivar_list_template, "_OBJC_CLASS_VARIABLES",
			     size, initlist);
      /* cast! */
      TREE_TYPE (_OBJC_CLASS_VARIABLES_decl) = variable_length_type;
    }
  else
    _OBJC_CLASS_VARIABLES_decl = 0;

  chain = CLASS_IVARS (implementation_template);
  if (chain)
    {
      size = 0;
      initlist = build_ivar_list_initializer (chain, &size);

      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);

      _OBJC_INSTANCE_VARIABLES_decl =
	generate_ivars_list (ivar_list_template, "_OBJC_INSTANCE_VARIABLES",
			     size, initlist);
      /* cast! */
      TREE_TYPE (_OBJC_INSTANCE_VARIABLES_decl) = variable_length_type;
    }
  else
    _OBJC_INSTANCE_VARIABLES_decl = 0;
}

static tree
build_dispatch_table_initializer (entries, size)
     tree entries;
     int *size;
{
  tree initlist = NULLT;

  do
    {
      int offset = add_objc_string (METHOD_SEL_NAME (entries));

      initlist = tree_cons (NULLT, init_selector (offset), initlist);

      offset = add_objc_string (METHOD_ENCODING (entries));
      initlist = tree_cons (NULLT, build_msg_pool_reference (offset), initlist);

      initlist = tree_cons (NULLT, METHOD_DEFINITION (entries), initlist);

      (*size)++;
      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/*
 * To accomplish method prototyping without generating all kinds of
 * inane warnings, the definition of the dispatch table entries were
 * changed from:
 *
 * 	struct objc_method { SEL _cmd; id (*_imp)(); };
 * to:
 * 	struct objc_method { SEL _cmd; void *_imp; };
 */
static tree
build_method_template ()
{
  tree _SLT_record;
  tree decl_specs, field_decl, field_decl_chain, parms;

  _SLT_record = start_struct (RECORD_TYPE, get_identifier (_TAG_METHOD));

#ifdef OBJC_INT_SELECTORS
  /* unsigned int _cmd; */
  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_UNSIGNED], NULLT);
  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_INT], decl_specs);
  field_decl = get_identifier ("_cmd");
#else /* not OBJC_INT_SELECTORS */
  /* struct objc_selector *_cmd; */
  decl_specs = tree_cons (NULLT,
			  xref_tag (RECORD_TYPE,
				    get_identifier (TAG_SELECTOR)),
			  NULLT);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("_cmd"));
#endif /* not OBJC_INT_SELECTORS */

  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  field_decl_chain = field_decl;

  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_CHAR], NULLT);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("method_types"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  /* void *_imp; */

  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_VOID], NULLT);
  field_decl = build1 (INDIRECT_REF, NULLT, get_identifier ("_imp"));
  field_decl = grokfield (input_filename, lineno, field_decl, decl_specs, NULLT);
  chainon (field_decl_chain, field_decl);

  finish_struct (_SLT_record, field_decl_chain);

  return _SLT_record;
}


static tree
generate_dispatch_table (type, name, size, list)
     tree type;
     char *name;
     int size;
     tree list;
{
  tree sc_spec, decl_specs, decl, initlist;

  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_STATIC], NULLT);
  decl_specs = tree_cons (NULLT, type, sc_spec);

  decl = start_decl (synth_id_with_class_suffix (name), decl_specs, 1);

  initlist = build_tree_list (NULLT, build_int_2 (0, 0));
  initlist = tree_cons (NULLT, build_int_2 (size, 0), initlist);
  initlist = tree_cons (NULLT, list, initlist);

  finish_decl (decl, build_nt (CONSTRUCTOR, NULLT, nreverse (initlist)), NULLT);

  return decl;
}

static void
generate_dispatch_tables ()
{
  tree initlist, chain, method_list_template;
  tree cast, variable_length_type;
  int size;

  if (!objc_method_template)
    objc_method_template = build_method_template ();

  cast = build_tree_list (build_tree_list (NULLT, xref_tag (RECORD_TYPE,
							    get_identifier (_TAG_METHOD_LIST))), NULLT);
  variable_length_type = groktypename (cast);

  chain = CLASS_CLS_METHODS (implementation_context);
  if (chain)
    {
      size = 0;
      initlist = build_dispatch_table_initializer (chain, &size);

      method_list_template = build_method_list_template (objc_method_template,
							 size);
      if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
	_OBJC_CLASS_METHODS_decl = 
	    generate_dispatch_table (method_list_template,
				     "_OBJC_CLASS_METHODS", 
				     size, initlist);
      else
	/* we have a category */
	_OBJC_CLASS_METHODS_decl = 
	    generate_dispatch_table (method_list_template,
				     "_OBJC_CATEGORY_CLASS_METHODS", 
				     size, initlist);
      /* cast! */
      TREE_TYPE (_OBJC_CLASS_METHODS_decl) = variable_length_type;
    }
  else
    _OBJC_CLASS_METHODS_decl = 0;

  chain = CLASS_NST_METHODS (implementation_context);
  if (chain)
    {
      size = 0;
      initlist = build_dispatch_table_initializer (chain, &size);

      method_list_template = build_method_list_template (objc_method_template,
							 size);
      if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
	_OBJC_INSTANCE_METHODS_decl = 
	    generate_dispatch_table (method_list_template,
				     "_OBJC_INSTANCE_METHODS", 
				     size, initlist);
      else
	/* we have a category */
	_OBJC_INSTANCE_METHODS_decl = 
	    generate_dispatch_table (method_list_template,
				     "_OBJC_CATEGORY_INSTANCE_METHODS", 
				     size, initlist);
      /* cast! */
      TREE_TYPE (_OBJC_INSTANCE_METHODS_decl) = variable_length_type;
    }
  else
    _OBJC_INSTANCE_METHODS_decl = 0;
}

static tree
build_category_initializer (cat_name, class_name,
			    instance_methods, class_methods)
     tree cat_name;
     tree class_name;
     tree instance_methods;
     tree class_methods;
{
  tree initlist = NULLT, expr;

  initlist = tree_cons (NULLT, cat_name, initlist);
  initlist = tree_cons (NULLT, class_name, initlist);

  if (!instance_methods)
    initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, instance_methods, 0);
      initlist = tree_cons (NULLT, expr, initlist);
    }
  if (!class_methods)
    initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, class_methods, 0);
      initlist = tree_cons (NULLT, expr, initlist);
    }
  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/*
 *  struct objc_class {
 *    struct objc_class *isa;
 *    struct objc_class *super_class;
 *    char *name;
 *    long version;
 *    long info;
 *    long instance_size;
 *    struct objc_ivar_list *ivars;
 *    struct objc_method_list *methods;
 *    struct objc_cache *cache;
 *  };
 */
static tree
build_shared_structure_initializer (isa, super, name, size, status,
				    dispatch_table, ivar_list)
     tree isa;
     tree super;
     tree name;
     tree size;
     int status;
     tree dispatch_table;
     tree ivar_list;
{
  tree initlist = NULLT, expr;

  /* isa = */
  initlist = tree_cons (NULLT, isa, initlist);

  /* super_class = */
  initlist = tree_cons (NULLT, super, initlist);

  /* name = */
  initlist = tree_cons (NULLT, name, initlist);

  /* version = */
  initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);

  /* info = */
  initlist = tree_cons (NULLT, build_int_2 (status, 0), initlist);

  /* instance_size = */
  initlist = tree_cons (NULLT, size, initlist);

  /* objc_ivar_list = */
  if (!ivar_list)
    initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, ivar_list, 0);
      initlist = tree_cons (NULLT, expr, initlist);
    }

  /* objc_method_list = */
  if (!dispatch_table)
    initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, dispatch_table, 0);
      initlist = tree_cons (NULLT, expr, initlist);
    }

  /* method_cache = */
  initlist = tree_cons (NULLT, build_int_2 (0, 0), initlist);

  return build_nt (CONSTRUCTOR, NULLT, nreverse (initlist));
}

/*
 * static struct objc_category _OBJC_CATEGORY_<name> = { ... };
 */
static void
generate_category (cat)
     tree cat;
{
  tree sc_spec, decl_specs, decl;
  tree initlist, cat_name_expr, class_name_expr;
  int offset;

  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_STATIC], NULLT);
  decl_specs = tree_cons (NULLT, objc_category_template, sc_spec);

  decl = start_decl (synth_id_with_class_suffix ("_OBJC_CATEGORY"),
		     decl_specs, 1);

  offset = add_objc_string (CLASS_SUPER_NAME (cat));
  cat_name_expr = build_msg_pool_reference (offset);

  offset = add_objc_string (CLASS_NAME (cat));
  class_name_expr = build_msg_pool_reference (offset);

  initlist = build_category_initializer (
					 cat_name_expr, class_name_expr,
					 _OBJC_INSTANCE_METHODS_decl, _OBJC_CLASS_METHODS_decl);

  finish_decl (decl, initlist, NULLT);
}

/*
 * static struct objc_class _OBJC_METACLASS_Foo={ ... };
 * static struct objc_class _OBJC_CLASS_Foo={ ... };
 */
static void
generate_shared_structures ()
{
  tree sc_spec, decl_specs, expr_decl, decl;
  tree name_expr, super_expr, root_expr;
  tree my_root_id = NULLT, my_super_id = NULLT;
  tree cast_type, initlist;
  int offset;

  my_super_id = CLASS_SUPER_NAME (implementation_template);
  if (my_super_id)
    {
      add_class_reference (my_super_id);

      /* compute "my_root_id" - this is required for code generation.
       * the "isa" for all meta class structures points to the root of
       * the inheritance hierarchy (e.g. "__Object")...
       */
      my_root_id = my_super_id;
      do
	{
	  tree my_root_int = lookup_interface (my_root_id);

	  if (my_root_int && CLASS_SUPER_NAME (my_root_int))
	    my_root_id = CLASS_SUPER_NAME (my_root_int);
	  else
	    break;
	}
      while (1);
    }
  else				/* no super class */
    {
      my_root_id = CLASS_NAME (implementation_template);
    }

  cast_type = groktypename (build_tree_list (build_tree_list (NULLT,
							      objc_class_template), build1 (INDIRECT_REF, NULLT, NULLT)));

  offset = add_objc_string (CLASS_NAME (implementation_template));
  name_expr = build_msg_pool_reference (offset);

  /* install class `isa' and `super' pointers at runtime */
  if (my_super_id)
    {
      offset = add_objc_string (my_super_id);
      super_expr = build_msg_pool_reference (offset);
      TREE_TYPE (super_expr) = cast_type; /* cast! */
    }
  else
    super_expr = build_int_2 (0, 0);

  offset = add_objc_string (my_root_id);
  root_expr = build_msg_pool_reference (offset);
  TREE_TYPE (root_expr) = cast_type; /* cast! */

  /* static struct objc_class _OBJC_METACLASS_Foo = { ... }; */

  sc_spec = build_tree_list (NULLT, ridpointers[(int) RID_STATIC]);
  decl_specs = tree_cons (NULLT, objc_class_template, sc_spec);

  decl = start_decl (DECL_NAME (_OBJC_METACLASS_decl), decl_specs, 1);

  initlist = build_shared_structure_initializer (
						 root_expr, super_expr, name_expr,
						 build_int_2 (TREE_INT_CST_LOW (TYPE_SIZE (objc_class_template)) / BITS_PER_UNIT, 0),
						 2 /*CLS_META*/,
						 _OBJC_CLASS_METHODS_decl, _OBJC_CLASS_VARIABLES_decl);

  finish_decl (decl, initlist, NULLT);

  /* static struct objc_class _OBJC_CLASS_Foo={ ... }; */

  decl = start_decl (DECL_NAME (_OBJC_CLASS_decl), decl_specs, 1);

  initlist = build_shared_structure_initializer (
						 build_unary_op (ADDR_EXPR, _OBJC_METACLASS_decl, 0),
						 super_expr, name_expr,
						 build_int_2 (TREE_INT_CST_LOW (TYPE_SIZE (CLASS_STATIC_TEMPLATE (implementation_template))) / BITS_PER_UNIT, 0),
						 1 /*CLS_FACTORY*/,
						 _OBJC_INSTANCE_METHODS_decl, _OBJC_INSTANCE_VARIABLES_decl);

  finish_decl (decl, initlist, NULLT);
}

static tree
synth_id_with_class_suffix (preamble)
     char *preamble;
{
  char *string;
  if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
    {
      string = (char *) alloca (strlen (preamble)
				+ strlen (IDENTIFIER_POINTER (CLASS_NAME (implementation_context)))
				+ 3);
      sprintf (string, "%s_%s", preamble,
	       IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
    }
  else
    {
      /* we have a category */
      string = (char *) alloca (strlen (preamble)
				+ strlen (IDENTIFIER_POINTER (CLASS_NAME (implementation_context)))
				+ strlen (IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)))
				+ 3);
      sprintf (string, "%s_%s_%s", preamble,
	       IDENTIFIER_POINTER (CLASS_NAME (implementation_context)),
	       IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
    }
  return get_identifier (string);
}

/*
 *   usage:
 *		keyworddecl:
 *			selector ':' '(' typename ')' identifier
 *
 *   purpose:
 *		transform an Objective-C keyword argument into
 *		the C equivalent parameter declarator.
 *
 *   in:	key_name, an "identifier_node" (optional).
 *		arg_type, a  "tree_list" (optional).
 *		arg_name, an "identifier_node".
 *
 *   note:	it would be really nice to strongly type the preceding
 *		arguments in the function prototype; however, then i
 *		could not use the "accessor" macros defined in "tree.h".
 *
 *   out:	an instance of "keyword_decl".
 *
 */

tree
build_keyword_decl (key_name, arg_type, arg_name)
     tree key_name;
     tree arg_type;
     tree arg_name;
{
  tree keyword_decl;

  /* if no type is specified, default to "id" */
  if (arg_type == NULLT)
    arg_type = build_tree_list (build_tree_list (NULLT, objc_object_reference),
				build1 (INDIRECT_REF, NULLT, NULLT));

  keyword_decl = make_node (KEYWORD_DECL);

  TREE_TYPE (keyword_decl) = arg_type;
  KEYWORD_ARG_NAME (keyword_decl) = arg_name;
  KEYWORD_KEY_NAME (keyword_decl) = key_name;

  return keyword_decl;
}

/*
 *  given a chain of keyword_decl's, synthesize the full keyword selector.
 */
static tree
build_keyword_selector (selector)
     tree selector;
{
  int len = 0;
  tree key_chain, key_name;
  char *buf;

  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	key_name = TREE_PURPOSE (key_chain);

      if (key_name)
	len += IDENTIFIER_LENGTH (key_name) + 1;
      else			/* just a ':' arg */
	len++;
    }
  buf = (char *)alloca (len + 1);
  bzero (buf, len + 1);

  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	key_name = TREE_PURPOSE (key_chain);

      if (key_name)
	strcat (buf, IDENTIFIER_POINTER (key_name));
      strcat (buf, ":");
    }
  return get_identifier (buf);
}

/* used for declarations and definitions */

tree
build_method_decl (code, ret_type, selector, add_args)
     enum tree_code code;
     tree ret_type;
     tree selector;
     tree add_args;
{
  tree method_decl;

  /* if no type is specified, default to "id" */
  if (ret_type == NULLT)
    ret_type = build_tree_list (build_tree_list (NULLT, objc_object_reference),
				build1 (INDIRECT_REF, NULLT, NULLT));

  method_decl = make_node (code);
  TREE_TYPE (method_decl) = ret_type;

  /*
   *  if we have a keyword selector, create an identifier_node that
   *  represents the full selector name (`:' included)...
   */
  if (TREE_CODE (selector) == KEYWORD_DECL)
    {
      METHOD_SEL_NAME (method_decl) = build_keyword_selector (selector);
      METHOD_SEL_ARGS (method_decl) = selector;
      METHOD_ADD_ARGS (method_decl) = add_args;
    }
  else
    {
      METHOD_SEL_NAME (method_decl) = selector;
      METHOD_SEL_ARGS (method_decl) = NULLT;
      METHOD_ADD_ARGS (method_decl) = NULLT;
    }

  return method_decl;
}

#define METHOD_DEF 0
#define METHOD_REF 1
/* Used by `build_message_expr' and `comp_method_types'.
   Return an argument list for method METH.
   CONTEXT is either METHOD_DEF or METHOD_REF,
    saying whether we are trying to define a method or call one.
   SUPERFLAG says this is for a send to super;
    this makes a difference for the NeXT calling sequence
    in which the lookup and the method call are done together.  */

static tree
get_arg_type_list (meth, context, superflag)
     tree meth;
     int context;
     int superflag;
{
  tree arglist, akey;

#ifdef NEXT_OBJC_RUNTIME
  /* receiver type */
  if (superflag)
    {
      arglist = build_tree_list (NULLT, super_type);
    }
  else
#endif
    {
      if (context == METHOD_DEF)
	arglist = build_tree_list (NULLT, TREE_TYPE (self_decl));
      else
	arglist = build_tree_list (NULLT, id_type);
    }

  /* selector type - will eventually change to `int' */
  chainon (arglist, build_tree_list (NULLT, selector_type));

  /* build a list of argument types */
  for (akey = METHOD_SEL_ARGS (meth); akey; akey = TREE_CHAIN (akey))
    {
      tree arg_decl = groktypename_in_parm_context (TREE_TYPE (akey));
      chainon (arglist, build_tree_list (NULLT, TREE_TYPE (arg_decl)));
    }

  if (METHOD_ADD_ARGS (meth) == (tree)1)
    /*
     * we have a `, ...' immediately following the selector,
     * finalize the arglist...simulate get_parm_info (0)
     */
    ;
  else if (METHOD_ADD_ARGS (meth))
    {
      /* we have a variable length selector */
      tree add_arg_list = TREE_CHAIN (METHOD_ADD_ARGS (meth));
      chainon (arglist, add_arg_list);
    }
  else				/* finalize the arglist...simulate get_parm_info (1) */
    chainon (arglist, build_tree_list (NULLT, void_type_node));

  return arglist;
}

static tree
check_duplicates (hsh)
     hash hsh;
{
  tree meth = NULLT;

  if (hsh)
    {
      meth = hsh->key;

      if (hsh->list)
        {
	  /* we have two methods with the same name and different types */
	  attr loop;
	  char type;

	  type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL) ? '-' : '+';

	  warning ("multiple declarations for method `%s'",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));

	  warn_with_method ("using", type, meth);
	  for (loop = hsh->list; loop; loop = loop->next)
	    warn_with_method ("also found", type, loop->value);
        }
    }
  return meth;
}

static tree
receiver_is_class_object (receiver)
      tree receiver;
{
  /* the receiver is a function call that returns an id...
   * ...check if it is a call to objc_getClass, if so, give it
   * special treatment.
   */
  tree exp = TREE_OPERAND (receiver, 0);

  if (exp != 0 && (TREE_CODE (exp) == ADDR_EXPR))
    {
      exp = TREE_OPERAND (exp, 0);
      if (exp != 0
	  && TREE_CODE (exp) == FUNCTION_DECL && exp == objc_getClass_decl)
	{
	  /* we have a call to objc_getClass! */
	  tree arg = TREE_OPERAND (receiver, 1);

	  if (arg != 0
	      && TREE_CODE (arg) == TREE_LIST
	      && (arg = TREE_VALUE (arg))
	      && TREE_CODE (arg) == NOP_EXPR
	      && (arg = TREE_OPERAND (arg, 0))
	      && TREE_CODE (arg) == ADDR_EXPR
	      && (arg = TREE_OPERAND (arg, 0))
	      && TREE_CODE (arg) == STRING_CST)
	    /* finally, we have the class name */
	    return get_identifier (TREE_STRING_POINTER (arg));
	}
    }
  return 0;
}

/* If we are currently building a message expr, this holds
   the identifier of the selector of the message.  This is
   used when printing warnings about argument mismatches. */

static tree building_objc_message_expr = 0;

tree
maybe_building_objc_message_expr ()
{
  return building_objc_message_expr;
}

/* Construct an expression for sending a message.
   MESS has the object to send to in TREE_PURPOSE
   and the argument list (including selector) in TREE_VALUE.  */

tree
build_message_expr (mess)
     tree mess;
{
  tree receiver = TREE_PURPOSE (mess);
  tree selector, self_object;
  tree rtype, sel_name;
  tree args = TREE_VALUE (mess);
  tree method_params = NULLT;
  tree method_prototype = NULLT;
  int selTransTbl_index;
  tree retval;
  int statically_typed = 0, statically_allocated = 0;
  tree class_ident = 0;

  /* 1 if this is sending to the superclass.  */
  int super;

  if (!doing_objc_thang)
    fatal ("Objective-C text in C source file");

  if (TREE_CODE (receiver) == ERROR_MARK)
    return error_mark_node;

  /* determine receiver type */
  rtype = TREE_TYPE (receiver);
  super = (TREE_TYPE (receiver) == super_type);

  if (! super)
    {
      if (TREE_STATIC_TEMPLATE (rtype))
	statically_allocated = 1;
      else if (TREE_CODE (rtype) == POINTER_TYPE
	       && TREE_STATIC_TEMPLATE (TREE_TYPE (rtype)))
	statically_typed = 1;
      /* classfix -smn */
      else if (TREE_CODE (receiver) == CALL_EXPR && rtype == id_type
	       && (class_ident = receiver_is_class_object (receiver)))
	;
      else if (TYPE_MAIN_VARIANT (rtype) != TYPE_MAIN_VARIANT (id_type)
	       && TYPE_MAIN_VARIANT (rtype) != TYPE_MAIN_VARIANT (class_type))
	{
	  bzero (errbuf, BUFSIZE);
	  warning ("invalid receiver type `%s'", gen_declaration (rtype, errbuf));
	}
      if (statically_allocated)
	receiver = build_unary_op (ADDR_EXPR, receiver, 0);

      /* Don't evaluate the receiver twice. */
      receiver = save_expr (receiver);
      self_object = receiver;
    }
  else
    /* If sending to `super', use current self as the object.  */
    self_object = self_decl;

  /* Obtain the full selector name.  */

  if (TREE_CODE (args) == IDENTIFIER_NODE)
    /* a unary selector */
    sel_name = args;
  else if (TREE_CODE (args) == TREE_LIST)
    sel_name = build_keyword_selector (args);

  selTransTbl_index = add_selector_reference (sel_name);

  /* Build the parameters list for looking up the method.
     These are the object itself and the selector.  */
  
#ifndef OBJC_SELECTORS_WITHOUT_LABELS
  selector = build_selector_reference (selTransTbl_index);
#else
  selector = build_array_ref (_OBJC_SELECTOR_REFERENCES_decl,
			      build_int_2 (selTransTbl_index, 0));
#endif

  /* Build the parameter list to give to the method.  */

  method_params = NULLT;
  if (TREE_CODE (args) == TREE_LIST)
    {
      tree chain = args, prev = NULLT;

      /* We have a keyword selector--check for comma expressions.  */
      while (chain)
	{
	  tree element = TREE_VALUE (chain);

	  /* We have a comma expression, must collapse...  */
	  if (TREE_CODE (element) == TREE_LIST)
	    {
	      if (prev)
		TREE_CHAIN (prev) = element;
	      else
		args = element;
	    }
	  prev = chain;
	  chain = TREE_CHAIN (chain);
        }
      method_params = args;
    }

  /* Determine operation return type.  */

  if (super_type != 0
      && TYPE_MAIN_VARIANT (rtype) == TYPE_MAIN_VARIANT (super_type))
    {
      tree iface;

      if (CLASS_SUPER_NAME (implementation_template))
	{
	  iface = lookup_interface (CLASS_SUPER_NAME (implementation_template));
    
	  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
	    method_prototype = lookup_instance_method_static (iface, sel_name);
	  else
	    method_prototype = lookup_class_method_static (iface, sel_name);
    
	  if (iface && !method_prototype)
	    warning ("`%s' does not respond to `%s'",
		     IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_template)),
		     IDENTIFIER_POINTER (sel_name));
	}
      else
	{
	  error ("no super class declared in interface for `%s'",
		 IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	  return error_mark_node;
	}

    }
  else if (statically_allocated)
    {
      tree iface = lookup_interface (TYPE_NAME (rtype));

      if (iface && !(method_prototype = lookup_instance_method_static (iface, sel_name)))
	warning ("`%s' does not respond to `%s'",
		 IDENTIFIER_POINTER (TYPE_NAME (rtype)),
		 IDENTIFIER_POINTER (sel_name));
    }
  else if (statically_typed)
    {
      tree ctype = TREE_TYPE (rtype);

      /* `self' is now statically_typed...all methods should be visible
       * within the context of the implementation...
       */
      if (implementation_context
	  && CLASS_NAME (implementation_context) == TYPE_NAME (ctype))
	{
	  method_prototype = lookup_instance_method_static (implementation_template, sel_name);

	  if (!method_prototype && implementation_template != implementation_context)
	    /* the method is not published in the interface...check locally */
	    method_prototype = lookup_method (CLASS_NST_METHODS (implementation_context),
				  sel_name);
	}
      else
	{
	  tree iface;

	  if (iface = lookup_interface (TYPE_NAME (ctype)))
	    method_prototype = lookup_instance_method_static (iface, sel_name);
	}

      if (!method_prototype)
        warning ("`%s' does not respond to `%s'",
		 IDENTIFIER_POINTER (TYPE_NAME (ctype)),
		 IDENTIFIER_POINTER (sel_name));
    }
  else if (class_ident)
    {
      if (implementation_context
	  && CLASS_NAME (implementation_context) == class_ident)
	{
	  method_prototype
	    = lookup_class_method_static (implementation_template, sel_name);
    
	  if (!method_prototype
	      && implementation_template != implementation_context)
	    /* the method is not published in the interface...check locally */
	    method_prototype
	      = lookup_method (CLASS_CLS_METHODS (implementation_context),
			       sel_name);
	}
      else
	{
	  tree iface;
    
	  if (iface = lookup_interface (class_ident))
	    method_prototype = lookup_class_method_static (iface, sel_name);
	}
  
      if (!method_prototype)
	{
	  warning ("cannot find class (factory) method.");
	  warning ("return type for `%s' defaults to id",
		   IDENTIFIER_POINTER (sel_name));
	}
    }
  else
    {
      hash hsh;

      /* we think we have an instance...loophole: extern id Object; */
      hsh = hash_lookup (nst_method_hash_list, sel_name);
      if (!hsh)
	/* for various loopholes...like sending messages to self in a
	   factory context... */
	hsh = hash_lookup (cls_method_hash_list, sel_name);

      method_prototype = check_duplicates (hsh);
      if (!method_prototype)
	{
	  warning ("cannot find method.");
	  warning ("return type for `%s' defaults to id",
		   IDENTIFIER_POINTER (sel_name));
	}
    }

  /* Save the selector name for printing error messages.  */
  building_objc_message_expr = sel_name;

  retval = build_objc_method_call (super, method_prototype,
				   receiver, self_object,
				   selector, method_params);

  building_objc_message_expr = 0;

  return retval;
}

/* Build a tree expression to send OBJECT the operation SELECTOR,
   looking up the method on object LOOKUP_OBJECT (often same as OBJECT),
   assuming the method has prototype METHOD_PROTOTYPE.
   (That is an INSTANCE_METHOD_DECL or CLASS_METHOD_DECL.)
   Use METHOD_PARAMS as list of args to pass to the method.
   If SUPER_FLAG is nonzero, we look up the superclass's method.  */

static tree
build_objc_method_call (super_flag, method_prototype, lookup_object, object,
			selector, method_params)
     int super_flag;
     tree method_prototype, lookup_object, object, selector, method_params;
{
  tree sender = (super_flag ? _msgSuper_decl : _msg_decl);

#ifdef NEXT_OBJC_RUNTIME
  if (!method_prototype)
    {
      method_params = tree_cons (NULLT, lookup_object,
				 tree_cons (NULLT, selector, method_params));
      return build_function_call (sender, method_params);
    }
  else
    {
      /* This is a real kludge, but it is used only for the Next.
	 Clobber the data type of SENDER temporarily to accept
	 all the arguments for this operation, and to return
	 whatever this operation returns.  */
      tree arglist = NULLT;
      tree retval;

      /* Save the proper contents of SENDER's data type.  */
      tree savarg = TYPE_ARG_TYPES (TREE_TYPE (sender));
      tree savret = TREE_TYPE (TREE_TYPE (sender));

      /* Install this method's argument types.  */
      arglist = get_arg_type_list (method_prototype, METHOD_REF, super_flag);
      TYPE_ARG_TYPES (TREE_TYPE (sender)) = arglist;

      /* Install this method's return type.  */
      TREE_TYPE (TREE_TYPE (sender))
	= groktypename (TREE_TYPE (method_prototype));

      /* Call SENDER with all the parameters.
	 This will do type checking using the arg types for this method.  */
      method_params = tree_cons (NULLT, lookup_object,
				 tree_cons (NULLT, selector, method_params));
      retval = build_function_call (sender, method_params);

      /* Restore SENDER's return/argument types.  */
      TYPE_ARG_TYPES (TREE_TYPE (sender)) = savarg;
      TREE_TYPE (TREE_TYPE (sender)) = savret;
      return retval;
    }
#else /* not NEXT_OBJC_RUNTIME */
  /* This is the portable way.
     First call the lookup function to get a pointer to the method, 
     then cast the pointer, then call it with the method arguments.  */
  tree method;

  /* Avoid trouble since we may evaluate each of these twice.  */
  object = save_expr (object);
  selector = save_expr (selector);

  method
    = build_function_call (sender,
			   tree_cons (NULLT, lookup_object,
				      tree_cons (NULLT, selector, NULLT)));

  /* If we have a method prototype, construct the data type this method needs,
     and cast what we got from SENDER into a pointer to that type.  */
  if (method_prototype)
    {
      tree arglist = get_arg_type_list (method_prototype, METHOD_REF, super_flag);
      tree valtype = groktypename (TREE_TYPE (method_prototype));
      tree fake_function_type = build_function_type (valtype, arglist);
      TREE_TYPE (method) = build_pointer_type (fake_function_type);
    }
  else
    {
      TREE_TYPE (method)
	= build_pointer_type (build_function_type (ptr_type_node, NULLT));
    }
  /* Pass the object to the method.  */
  return build_function_call (method,
			      tree_cons (NULLT, object,
					 tree_cons (NULLT, selector,
						    method_params)));
#endif /* not NEXT_OBJC_RUNTIME */
}

tree
build_selector_expr (selnamelist)
     tree selnamelist;
{
  tree selname;
  int selTransTbl_index;

  if (!doing_objc_thang)
    fatal ("Objective-C text in C source file");

  /* obtain the full selector name */
  if (TREE_CODE (selnamelist) == IDENTIFIER_NODE)
    /* a unary selector */
    selname = selnamelist;
  else if (TREE_CODE (selnamelist) == TREE_LIST)
    selname = build_keyword_selector (selnamelist);

  selTransTbl_index = add_selector_reference (selname);

#ifndef OBJC_SELECTORS_WITHOUT_LABELS
  return build_selector_reference (selTransTbl_index);
#else
  /* synthesize a reference into the selector translation table */
  return build_array_ref (_OBJC_SELECTOR_REFERENCES_decl,
			  build_int_2 (selTransTbl_index, 0));
#endif
}

tree
build_encode_expr (type)
     tree type;
{
  tree result;
  char *string;

  if (!doing_objc_thang)
    fatal ("Objective-C text in C source file");

  encode_type (type, OBJC_ENCODE_INLINE_DEFS);
  obstack_1grow (&util_obstack, 0);    /* null terminate string */
  string = obstack_finish (&util_obstack);

  /* synthesize a string that represents the encoded struct/union */
  result = my_build_string (strlen (string) + 1, string);
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

tree
build_ivar_reference (id)
     tree id;
{
  if (TREE_CODE (method_context) == CLASS_METHOD_DECL)
    TREE_TYPE (self_decl) = instance_type; /* cast */

  return build_component_ref (build_indirect_ref (self_decl, "->"), id);
}

#define HASH_ALLOC_LIST_SIZE	170
#define ATTR_ALLOC_LIST_SIZE	170
#define SIZEHASHTABLE 		257
#define HASHFUNCTION(key)	((int)key >> 2) 	/* divide by 4 */

static void
hash_init ()
{
  nst_method_hash_list = (hash *)xmalloc (SIZEHASHTABLE * sizeof (hash));
  cls_method_hash_list = (hash *)xmalloc (SIZEHASHTABLE * sizeof (hash));

  if (!nst_method_hash_list || !cls_method_hash_list)
    perror ("unable to allocate space in objc-tree.c");
  else
    {
      int i;

      for (i = 0; i < SIZEHASHTABLE; i++)
	{
	  nst_method_hash_list[i] = 0;
	  cls_method_hash_list[i] = 0;
	}
    }
}

static void
hash_enter (hashlist, method)
     hash *hashlist;
     tree method;
{
  static hash 	hash_alloc_list = 0;
  static int	hash_alloc_index = 0;
  hash obj;
  int slot = HASHFUNCTION (METHOD_SEL_NAME (method)) % SIZEHASHTABLE;

  if (!hash_alloc_list || hash_alloc_index >= HASH_ALLOC_LIST_SIZE)
    {
      hash_alloc_index = 0;
      hash_alloc_list = (hash)xmalloc (sizeof (struct hashedEntry) *
				      HASH_ALLOC_LIST_SIZE);
      if (!hash_alloc_list)
	perror ("unable to allocate in objc-tree.c");
    }
  obj = &hash_alloc_list[hash_alloc_index++];
  obj->list = 0;
  obj->next = hashlist[slot];
  obj->key = method;

  hashlist[slot] = obj;		/* append to front */
}

static hash
hash_lookup (hashlist, sel_name)
     hash *hashlist;
     tree sel_name;
{
  hash target;

  target = hashlist[HASHFUNCTION (sel_name) % SIZEHASHTABLE];

  while (target)
    {
      if (sel_name == METHOD_SEL_NAME (target->key))
	return target;

      target = target->next;
    }
  return 0;
}

static void
hash_add_attr (entry, value)
     hash entry;
     tree value;
{
  static attr 	attr_alloc_list = 0;
  static int	attr_alloc_index = 0;
  attr obj;

  if (!attr_alloc_list || attr_alloc_index >= ATTR_ALLOC_LIST_SIZE)
    {
      attr_alloc_index = 0;
      attr_alloc_list = (attr)xmalloc (sizeof (struct hashedAttribute) *
				      ATTR_ALLOC_LIST_SIZE);
      if (!attr_alloc_list)
	perror ("unable to allocate in objc-tree.c");
    }
  obj = &attr_alloc_list[attr_alloc_index++];
  obj->next = entry->list;
  obj->value = value;

  entry->list = obj;		/* append to front */
}

static tree
lookup_method (mchain, method)
     tree mchain;
     tree method;
{
  tree key;

  if (TREE_CODE (method) == IDENTIFIER_NODE)
    key = method;
  else
    key = METHOD_SEL_NAME (method);

  while (mchain)
    {
      if (METHOD_SEL_NAME (mchain) == key)
	return mchain;
      mchain = TREE_CHAIN (mchain);
    }
  return NULLT;
}

static tree
lookup_instance_method_static (interface, ident)
     tree interface;
     tree ident;
{
  tree inter = interface;
  tree chain = CLASS_NST_METHODS (inter);
  tree meth = NULLT;

  do
    {
      if (meth = lookup_method (chain, ident))
	return meth;

      if (CLASS_CATEGORY_LIST (inter))
	{
	  tree category = CLASS_CATEGORY_LIST (inter);
	  chain = CLASS_NST_METHODS (category);
    
	  do 
	    {
	      if (meth = lookup_method (chain, ident))
		return meth;
      
	      if (category = CLASS_CATEGORY_LIST (category))
		chain = CLASS_NST_METHODS (category);
	    }
	  while (category);
	}

      if (inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	chain = CLASS_NST_METHODS (inter);
    }
  while (inter);

  return meth;
}

static tree
lookup_class_method_static (interface, ident)
     tree interface;
     tree ident;
{
  tree inter = interface;
  tree chain = CLASS_CLS_METHODS (inter);
  tree meth = NULLT;

  do
    {
      if (meth = lookup_method (chain, ident))
	return meth;

      if (CLASS_CATEGORY_LIST (inter))
	{
	  tree category = CLASS_CATEGORY_LIST (inter);
	  chain = CLASS_CLS_METHODS (category);
    
	  do 
	    {
	      if (meth = lookup_method (chain, ident))
		return meth;
      
	      if (category = CLASS_CATEGORY_LIST (category))
		chain = CLASS_CLS_METHODS (category);
	    }
	  while (category);
	}

      if (inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	chain = CLASS_CLS_METHODS (inter);
    }
  while (inter);

  return meth;
}

tree
add_class_method (class, method)
     tree class;
     tree method;
{
  tree mth;
  hash hsh;

  if (!(mth = lookup_method (CLASS_CLS_METHODS (class), method)))
    {
      /* put method on list in reverse order */
      TREE_CHAIN (method) = CLASS_CLS_METHODS (class);
      CLASS_CLS_METHODS (class) = method;
    }
  else
    {
      if (TREE_CODE (class) == IMPLEMENTATION_TYPE)
	error ("duplicate definition of class method `%s'.",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
      else
        {
	  /* check types, if different complain */
	  if (!comp_proto_with_proto (method, mth))
	    error ("duplicate declaration of class method `%s'.",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
        }
    }

  if (!(hsh = hash_lookup (cls_method_hash_list, METHOD_SEL_NAME (method))))
    {
      /* install on a global chain */
      hash_enter (cls_method_hash_list, method);
    }
  else
    {
      /* check types, if different add to a list */
      if (!comp_proto_with_proto (method, hsh->key))
        hash_add_attr (hsh, method);
    }
  return method;
}

tree
add_instance_method (class, method)
     tree class;
     tree method;
{
  tree mth;
  hash hsh;

  if (!(mth = lookup_method (CLASS_NST_METHODS (class), method)))
    {
      /* put method on list in reverse order */
      TREE_CHAIN (method) = CLASS_NST_METHODS (class);
      CLASS_NST_METHODS (class) = method;
    }
  else
    {
      if (TREE_CODE (class) == IMPLEMENTATION_TYPE)
	error ("duplicate definition of instance method `%s'.",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
      else
        {
	  /* check types, if different complain */
	  if (!comp_proto_with_proto (method, mth))
	    error ("duplicate declaration of instance method `%s'.",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
        }
    }

  if (!(hsh = hash_lookup (nst_method_hash_list, METHOD_SEL_NAME (method))))
    {
      /* install on a global chain */
      hash_enter (nst_method_hash_list, method);
    }
  else
    {
      /* check types, if different add to a list */
      if (!comp_proto_with_proto (method, hsh->key))
        hash_add_attr (hsh, method);
    }
  return method;
}

static tree
add_class (class)
     tree class;
{
  /* put interfaces on list in reverse order */
  TREE_CHAIN (class) = interface_chain;
  interface_chain = class;
  return interface_chain;
}

static void
add_category (class, category)
      tree class;
      tree category;
{
  /* put categories on list in reverse order */
  CLASS_CATEGORY_LIST (category) = CLASS_CATEGORY_LIST (class);
  CLASS_CATEGORY_LIST (class) = category;
}

/* called after parsing each instance variable declaration. Necessary to
 * preserve typedefs and implement public/private...
 */
tree
add_instance_variable (class, public, declarator, declspecs, width)
     tree class;
     int public;
     tree declarator;
     tree declspecs;
     tree width;
{
  tree field_decl, raw_decl;

  raw_decl = build_tree_list (declspecs	/*purpose*/, declarator/*value*/);

  if (CLASS_RAW_IVARS (class))
    chainon (CLASS_RAW_IVARS (class), raw_decl);
  else
    CLASS_RAW_IVARS (class) = raw_decl;

  field_decl = grokfield (input_filename, lineno,
			  declarator, declspecs, width);

  /* overload the public attribute, it is not used for FIELD_DECL's */
  if (public)
    TREE_PUBLIC (field_decl) = 1;

  if (CLASS_IVARS (class))
    chainon (CLASS_IVARS (class), field_decl);
  else
    CLASS_IVARS (class) = field_decl;

  return class;
}

tree
is_ivar (decl_chain, ident)
     tree decl_chain;
     tree ident;
{
  for ( ; decl_chain; decl_chain = TREE_CHAIN (decl_chain))
    if (DECL_NAME (decl_chain) == ident)
      return decl_chain;
  return NULL_TREE;
}

/* we have an instance variable reference, check to see if it is public...*/

int
is_public (expr, identifier)
     tree expr;
     tree identifier;
{
  tree basetype = TREE_TYPE (expr);
  enum tree_code code = TREE_CODE (basetype);
  tree decl;

  if (code == RECORD_TYPE)
    {
      if (TREE_STATIC_TEMPLATE (basetype))
	{
	  if (decl = is_ivar (TYPE_FIELDS (basetype), identifier))
	    {
	      /* important difference between the Stepstone translator:
		 
		 all instance variables should be public within the context
		 of the implementation...
		 */
	      if (implementation_context)
		{
		  if ((TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE
		       && CLASS_NAME (implementation_context) == TYPE_NAME (basetype))
		      || (TREE_CODE (implementation_context) == CATEGORY_TYPE
			  && CLASS_NAME (implementation_context) == TYPE_NAME (basetype)))
		    return 1;
		}

	      if (TREE_PUBLIC (decl))
		return 1;

	      error ("instance variable `%s' is declared private",
		     IDENTIFIER_POINTER (identifier));
	      return 0;
	    }
	}
      else if (implementation_context && (basetype == objc_object_reference))
	{
	  TREE_TYPE (expr) = _PRIVATE_record;
	  if (extra_warnings)
	    {
	      warning ("static access to object of type `id'");
	      warning ("please change to type `%s *'",
		       IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
	    }
	}
    }
  return 1;
}

/* implement @defs (<classname>) within struct bodies. */

tree
get_class_ivars (interface)
     tree interface;
{
  if (!doing_objc_thang)
    fatal ("Objective-C text in C source file");

  return build_ivar_chain (interface);
}

tree
get_class_reference (interface)
     tree interface;
{
  tree params;

  add_class_reference (CLASS_NAME (interface));

  params = build_tree_list (NULLT,
			    my_build_string (IDENTIFIER_LENGTH (CLASS_NAME (interface)) + 1,
					     IDENTIFIER_POINTER (CLASS_NAME (interface))));

  return build_function_call (objc_getClass_decl, params);
}

/* make sure all entries in "chain" are also in "list" */

static void
check_methods (chain, list, mtype)
     tree chain;
     tree list;
     int mtype;
{
  int first = 1;

  while (chain)
    {
      if (!lookup_method (list, chain))
	{
	  if (first)
	    {
	      if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of class `%s'",
			 IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
	      else if (TREE_CODE (implementation_context) == CATEGORY_TYPE)
		warning ("incomplete implementation of category `%s'",
			 IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
	      first = 0;
	    }
	  warning ("method definition for `%c%s' not found",
		   mtype, IDENTIFIER_POINTER (METHOD_SEL_NAME (chain)));
	}
      chain = TREE_CHAIN (chain);
    }
}

/* Make sure that the class CLASS_NAME is defined
   CODE says which kind of thing CLASS_NAME ought to be.
   It can be INTERFACE_TYPE, IMPLEMENTATION_TYPE, PROTOCOL_TYPE
   or CATEGORY_TYPE.

   If CODE is INTERFACE_TYPE, we also do a push_obstacks_nochange
   whose matching pop is in continue_class.  */

tree
start_class (code, class_name, super_name)
     enum tree_code code;
     tree class_name;
     tree super_name;
{
  tree class;

  if (code == INTERFACE_TYPE)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
    }

  if (!doing_objc_thang)
    fatal ("Objective-C text in C source file");

  class = make_node (code);

  CLASS_NAME (class) = class_name;
  CLASS_SUPER_NAME (class) = super_name;
  CLASS_CLS_METHODS (class) = NULL_TREE;

  if (code == IMPLEMENTATION_TYPE)
    {
      /* pre-build the following entities - for speed/convenience. */
      if (!self_id)
        self_id = get_identifier ("self");
      if (!_cmd_id)
        _cmd_id = get_identifier ("_cmd");

      if (!objc_super_template)
	objc_super_template = build_super_template ();

      method_slot = 0;		/* reset for multiple classes per file */

      implementation_context = class;

      /* lookup the interface for this implementation. */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  warning ("Cannot find interface declaration for `%s'",
		   IDENTIFIER_POINTER (class_name));
	  add_class (implementation_template = implementation_context);
        }

      /* if a super class has been specified in the implementation,
	 insure it conforms to the one specified in the interface */

      if (super_name
	  && (super_name != CLASS_SUPER_NAME (implementation_template)))
        {
	  error ("conflicting super class name `%s'",
		 IDENTIFIER_POINTER (super_name));
	  error ("previous declaration of `%s'",
		 IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_template)));
        }
    }
  else if (code == INTERFACE_TYPE)
    {
      if (lookup_interface (class_name))
        warning ("duplicate interface declaration for class `%s'",
                 IDENTIFIER_POINTER (class_name));
      else
        add_class (class);
    }
  else if (code == PROTOCOL_TYPE)
    {
      tree class_category_is_assoc_with;

      /* for a category, class_name is really the name of the class that
	 the following set of methods will be associated with...we must
	 find the interface so that can derive the objects template */

      if (!(class_category_is_assoc_with = lookup_interface (class_name)))
	{
	  error ("Cannot find interface declaration for `%s'",
		 IDENTIFIER_POINTER (class_name));
	  exit (1);
	}
      else
        add_category (class_category_is_assoc_with, class);
    }
  else if (code == CATEGORY_TYPE)
    {
      /* pre-build the following entities - for speed/convenience. */
      if (!self_id)
        self_id = get_identifier ("self");
      if (!_cmd_id)
        _cmd_id = get_identifier ("_cmd");

      if (!objc_super_template)
	objc_super_template = build_super_template ();

      method_slot = 0;		/* reset for multiple classes per file */

      implementation_context = class;

      /* for a category, class_name is really the name of the class that
	 the following set of methods will be associated with...we must
	 find the interface so that can derive the objects template */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  error ("Cannot find interface declaration for `%s'",
		 IDENTIFIER_POINTER (class_name));
	  exit (1);
        }
    }
  return class;
}

tree
continue_class (class)
     tree class;
{
  if (TREE_CODE (class) == IMPLEMENTATION_TYPE
      || TREE_CODE (class) == CATEGORY_TYPE)
    {
      struct imp_entry *impEntry;
      tree ivar_context;

      /* check consistency of the instance variables. */

      if (CLASS_IVARS (class))
	check_ivars (implementation_template, class);

      /* code generation */

      ivar_context = build_private_template (implementation_template);

      if (!objc_class_template)
	build_class_template ();

      if (!(impEntry = (struct imp_entry *)xmalloc (sizeof (struct imp_entry))))
	perror ("unable to allocate in objc-tree.c");

      impEntry->next = imp_list;
      impEntry->imp_context = class;
      impEntry->imp_template = implementation_template;

      synth_forward_declarations ();
      impEntry->class_decl = _OBJC_CLASS_decl;
      impEntry->meta_decl = _OBJC_METACLASS_decl;

      /* append to front and increment count */
      imp_list = impEntry;
      if (TREE_CODE (class) == IMPLEMENTATION_TYPE)
	imp_count++;
      else
	cat_count++;

      return ivar_context;
    }
  else if (TREE_CODE (class) == INTERFACE_TYPE)
    {
      tree record = xref_tag (RECORD_TYPE, CLASS_NAME (class));

      if (!TYPE_FIELDS (record))
	{
	  finish_struct (record, build_ivar_chain (class));
	  CLASS_STATIC_TEMPLATE (class) = record;

	  /* mark this record as a class template - for static typing */
	  TREE_STATIC_TEMPLATE (record) = 1;
	}
      return NULLT;
    }
  else
    return error_mark_node;
}

/*
 * this is called once we see the "@end" in an interface/implementation.
 */
void
finish_class (class)
     tree class;
{
  if (TREE_CODE (class) == IMPLEMENTATION_TYPE)
    {
      /* all code generation is done in finish_objc */

      if (implementation_template != implementation_context)
	{
	  /* ensure that all method listed in the interface contain bodies! */
	  check_methods (CLASS_CLS_METHODS (implementation_template),
			 CLASS_CLS_METHODS (implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (implementation_template),
			 CLASS_NST_METHODS (implementation_context), '-');
	}
    }
  else if (TREE_CODE (class) == CATEGORY_TYPE)
    {
      tree category = CLASS_CATEGORY_LIST (implementation_template);
  
      /* find the category interface from the class it is associated with */
      while (category)
	{
	  if (CLASS_SUPER_NAME (class) == CLASS_SUPER_NAME (category))
	    break;
	  category = CLASS_CATEGORY_LIST (category);
	}
  
      if (category)
	{
	  /* ensure that all method listed in the interface contain bodies! */
	  check_methods (CLASS_CLS_METHODS (category),
			 CLASS_CLS_METHODS (implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (category),
			 CLASS_NST_METHODS (implementation_context), '-');
	}
    } 
  else if (TREE_CODE (class) == INTERFACE_TYPE)
    {
      tree decl_specs;
      char *string = (char *) alloca (strlen (IDENTIFIER_POINTER (CLASS_NAME (class))) + 3);

      /* extern struct objc_object *_<my_name>; */

      sprintf (string, "_%s", IDENTIFIER_POINTER (CLASS_NAME (class)));

      decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_EXTERN]);
      decl_specs = tree_cons (NULLT, objc_object_reference, decl_specs);
      define_decl (build1 (INDIRECT_REF, NULLT, get_identifier (string)),
		   decl_specs);
    }
}

/* "Encode" a data type into a string, whichg rows  in util_obstack.
   ??? What is the FORMAT?  Someone please document this!  */

/* Encode a pointer type.  */

static void
encode_pointer (type, format)
     tree type;
     int format;
{
  tree pointer_to = TREE_TYPE (type);

  if (TREE_CODE (pointer_to) == RECORD_TYPE)
    {
      if (TYPE_NAME (pointer_to)
	  && TREE_CODE (TYPE_NAME (pointer_to)) == IDENTIFIER_NODE)
	{
	  char *name = IDENTIFIER_POINTER (TYPE_NAME (pointer_to));

	  if ((strcmp (name, TAG_OBJECT) == 0) /* '@' */
	      || TREE_STATIC_TEMPLATE (pointer_to))
	    {
	      obstack_1grow (&util_obstack, '@');
	      return;
	    }
	  else if (strcmp (name, TAG_CLASS) == 0) /* '#' */
	    {
	      obstack_1grow (&util_obstack, '#');
	      return;
	    }
#ifndef OBJC_INT_SELECTORS
	  else if (strcmp (name, TAG_SELECTOR) == 0) /* ':' */
	    {
	      obstack_1grow (&util_obstack, ':');
	      return;
	    }
#endif /* OBJC_INT_SELECTORS */
	}
    }
  else if (TREE_CODE (pointer_to) == INTEGER_TYPE
	   && TYPE_MODE (pointer_to) == QImode)
    {
      obstack_1grow (&util_obstack, '*');
      return;
    }

  /* we have a type that does not get special treatment... */

  /* NeXT extension */
  obstack_1grow (&util_obstack, '^');
  encode_type (pointer_to, format);
}

static void
encode_array (type, format)
     tree type;
     int format;
{
  tree anIntCst = TYPE_SIZE (type);
  tree array_of = TREE_TYPE (type);
  char buffer[40];

  /* An incomplete array is treated like a pointer.  */
  if (anIntCst == NULL)
    {
      /* split for obvious reasons.  North-Keys 30 Mar 1991 */
      encode_pointer (type, format);
      return;
    }
  
  sprintf (buffer, "[%d",
	   TREE_INT_CST_LOW (anIntCst)
	   / TREE_INT_CST_LOW (TYPE_SIZE (array_of)));
  obstack_grow (&util_obstack, buffer, strlen (buffer));
  encode_type (array_of, format);
  obstack_1grow (&util_obstack, ']');
  return;
}

static void
encode_aggregate (type, format)
     tree type;
     int format;
{
  enum tree_code code = TREE_CODE (type);

  switch (code)
    {
    case RECORD_TYPE:
      {
	int have_pointer = 0;

	if (obstack_object_size (&util_obstack) > 0     
	    && *(obstack_next_free (&util_obstack)-1) == '^')
	  have_pointer = 1;

	obstack_1grow (&util_obstack, '{');
        if (TYPE_NAME (type))
	  {
	    if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	      {
	        obstack_grow (&util_obstack,
			      IDENTIFIER_POINTER (TYPE_NAME (type)),
			      strlen (IDENTIFIER_POINTER (TYPE_NAME (type))));
	      }
	    else /* we have an untagged structure or a typedef */
	      {
	        obstack_1grow (&util_obstack, '?');
	      }
	  }
	
	if (have_pointer
	    || format == OBJC_ENCODE_DONT_INLINE_DEFS)
	  {
	    /* we have a pointer
	       or we don't want the details.  */
	    obstack_1grow (&util_obstack, '}');
	  }
	else
	  {
	    tree fields = TYPE_FIELDS (type);
	    obstack_1grow (&util_obstack, '=');
	    for ( ; fields; fields = TREE_CHAIN (fields))
	      encode_field_decl (fields, format);
	    obstack_1grow (&util_obstack, '}');
	  }
	break;
      }

    case UNION_TYPE:
      {
	int have_pointer = 0;

	if (obstack_object_size (&util_obstack) > 0     
	    && *(obstack_next_free (&util_obstack)-1) == '^')
	  have_pointer = 1;

	obstack_1grow (&util_obstack, '(');
        if (have_pointer && TYPE_NAME (type))
	  {
	    if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	      {
	        obstack_grow (&util_obstack,
			      IDENTIFIER_POINTER (TYPE_NAME (type)),
			      strlen (IDENTIFIER_POINTER (TYPE_NAME (type))));
	      }
	    else /* we have an untagged structure or a typedef */
	      {
	        obstack_1grow (&util_obstack, '?');
	      }
	  }
	
	if (have_pointer
	    || format == OBJC_ENCODE_DONT_INLINE_DEFS)
	  {
	    /* we have a pointer
	       or we don't want the details.  */
	    obstack_1grow (&util_obstack, ')');
	  }
	else
	  {
	    tree fields = TYPE_FIELDS (type);
	    for ( ; fields; fields = TREE_CHAIN (fields))
	      encode_field_decl (fields, format);
	    obstack_1grow (&util_obstack, ')');
	  }
	break;
      }


    case ENUMERAL_TYPE:
      obstack_1grow (&util_obstack, 'i');
      break;
    }
}

/*
 *  support bitfields, the current version of Objective-C does not support
 *  them. the string will consist of one or more "b:n"'s where n is an
 *  integer describing the width of the bitfield. Currently, classes in
 *  the kit implement a method "-(char *)describeBitfieldStruct:" that
 *  simulates this...if they do not implement this method, the archiver
 *  assumes the bitfield is 16 bits wide (padded if necessary) and packed
 *  according to the GNU compiler. After looking at the "kit", it appears
 *  that all classes currently rely on this default behavior, rather than
 *  hand generating this string (which is tedious).
 */
static void
encode_bitfield (width, format)
     int width;
     int format;
{
  char buffer[40];
  sprintf (buffer, "b%d", width);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

/*
 *	format will be:
 *
 *	OBJC_ENCODE_INLINE_DEFS or OBJC_ENCODE_DONT_INLINE_DEFS
 */
static void
encode_type (type, format)
     tree type;
     int format;
{
  enum tree_code code = TREE_CODE (type);

  if (code == INTEGER_TYPE)
    {
      if (TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) == 0)
	{
	  /* unsigned integer types */

	  if (TYPE_MODE (type) == QImode) /* 'C' */
	    obstack_1grow (&util_obstack, 'C');
	  else if (TYPE_MODE (type) == HImode) /* 'S' */
	    obstack_1grow (&util_obstack, 'S');
	  else if (TYPE_MODE (type) == SImode)
	    {
	      if (type == long_unsigned_type_node)
		obstack_1grow (&util_obstack, 'L'); /* 'L' */
	      else
		obstack_1grow (&util_obstack, 'I'); /* 'I' */
	    }
	}
      else			/* signed integer types */
	{
	  if (TYPE_MODE (type) == QImode) /* 'c' */
	    obstack_1grow (&util_obstack, 'c');
	  else if (TYPE_MODE (type) == HImode) /* 's' */
	    obstack_1grow (&util_obstack, 's');
	  else if (TYPE_MODE (type) == SImode) /* 'i' */
	    {
	      if (type == long_integer_type_node)
		obstack_1grow (&util_obstack, 'l'); /* 'l' */
	      else
		obstack_1grow (&util_obstack, 'i'); /* 'i' */
	    }
	}
    }
  else if (code == REAL_TYPE)
    {
      /* floating point types */

      if (TYPE_MODE (type) == SFmode) /* 'f' */
	obstack_1grow (&util_obstack, 'f');
      else if (TYPE_MODE (type) == DFmode
	       || TYPE_MODE (type) == TFmode) /* 'd' */
	obstack_1grow (&util_obstack, 'd');
    }

  else if (code == VOID_TYPE)	/* 'v' */
    obstack_1grow (&util_obstack, 'v');

  else if (code == ARRAY_TYPE)
    encode_array (type, format);

  else if (code == POINTER_TYPE)
    encode_pointer (type, format);

  else if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
    encode_aggregate (type, format);

  else if (code == FUNCTION_TYPE) /* '?' */
    obstack_1grow (&util_obstack, '?');
}

static void
encode_field_decl (field_decl, format)
     tree field_decl;
     int format;
{
  if (DECL_BIT_FIELD (field_decl))
    encode_bitfield (DECL_FIELD_SIZE (field_decl), format);
  else
    encode_type (TREE_TYPE (field_decl), format);
}

static tree
expr_last (complex_expr)
     tree complex_expr;
{
  tree next;

  if (complex_expr)
    while (next = TREE_OPERAND (complex_expr, 0))
      complex_expr = next;
  return complex_expr;
}

/* The selector of the current method,
   or NULL if we aren't compiling a method.  */

tree
maybe_objc_method_name (decl)
      tree decl;
{
  if (method_context)
    return METHOD_SEL_NAME (method_context);
  else
    return 0;
}

/*
 *  Transform a method definition into a function definition as follows:
 *
 *  - synthesize the first two arguments, "self" and "_cmd".
 */

void
start_method_def (method)
     tree method;
{
  tree decl_specs;

  /* required to implement _msgSuper () */
  method_context = method;
  _OBJC_SUPER_decl = NULLT;

  pushlevel (0); 		/* must be called BEFORE "start_function ()" */

  /* generate prototype declarations for arguments..."new-style" */

  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
    decl_specs = build_tree_list (NULLT, _PRIVATE_record);
  else
    /* really a `struct objc_class *'...however we allow people to
       assign to self...which changes its type midstream.
       */
    decl_specs = build_tree_list (NULLT, objc_object_reference);

  push_parm_decl (build_tree_list (decl_specs,
				   build1 (INDIRECT_REF, NULLT, self_id)));

#ifdef OBJC_INT_SELECTORS
  decl_specs = build_tree_list (NULLT, ridpointers[(int) RID_UNSIGNED]);
  decl_specs = tree_cons (NULLT, ridpointers[(int) RID_INT], decl_specs);
  push_parm_decl (build_tree_list (decl_specs, _cmd_id));
#else /* not OBJC_INT_SELECTORS */
  decl_specs = build_tree_list (NULLT,
				xref_tag (RECORD_TYPE,
					  get_identifier (TAG_SELECTOR)));
  push_parm_decl (build_tree_list (decl_specs, 
				   build1 (INDIRECT_REF, NULLT, _cmd_id)));
#endif /* not OBJC_INT_SELECTORS */

  /* generate argument declarations if a keyword_decl */
  if (METHOD_SEL_ARGS (method))
    {
      tree arglist = METHOD_SEL_ARGS (method);
      do
	{
	  tree arg_spec = TREE_PURPOSE (TREE_TYPE (arglist));
	  tree arg_decl = TREE_VALUE (TREE_TYPE (arglist));

	  if (arg_decl)
	    {
	      tree last_expr = expr_last (arg_decl);

	      /* unite the abstract decl with its name */
	      TREE_OPERAND (last_expr, 0) = KEYWORD_ARG_NAME (arglist);
	      push_parm_decl (build_tree_list (arg_spec, arg_decl));
	      /* unhook...restore the abstract declarator */
	      TREE_OPERAND (last_expr, 0) = NULLT;
	    }
	  else
	    push_parm_decl (build_tree_list (arg_spec, KEYWORD_ARG_NAME (arglist)));

	  arglist = TREE_CHAIN (arglist);
	}
      while (arglist);
    }

  if (METHOD_ADD_ARGS (method) > (tree)1)
    {
      /* we have a variable length selector - in "prototype" format */
      tree akey = TREE_PURPOSE (METHOD_ADD_ARGS (method));
      while (akey)
	{
	  /* this must be done prior to calling pushdecl (). pushdecl () is
	   * going to change our chain on us...
	   */
	  tree nextkey = TREE_CHAIN (akey);
	  pushdecl (akey);
	  akey = nextkey;
	}
    }
}

static void
error_with_method (message, mtype, method)
     char *message;
     char mtype;
     tree method;
{
  count_error (0);
  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (method), DECL_SOURCE_LINE (method));
  bzero (errbuf, BUFSIZE);
  fprintf (stderr, "%s `%c%s'\n", message, mtype, gen_method_decl (method, errbuf));
}

static void
warn_with_method (message, mtype, method)
     char *message;
     char mtype;
     tree method;
{
  count_error (1);
  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (method), DECL_SOURCE_LINE (method));
  bzero (errbuf, BUFSIZE);
  fprintf (stderr, "%s `%c%s'\n", message, mtype, gen_method_decl (method, errbuf));
}

/* return 1 if `method' is consistent with `proto' */

static int
comp_method_with_proto (method, proto)
     tree method, proto;
{
  static tree function_type = 0;

  /* create a function_type node once */
  if (!function_type)
    {
      struct obstack *ambient_obstack = current_obstack;
      
      current_obstack = &permanent_obstack;
      function_type = make_node (FUNCTION_TYPE);
      current_obstack = ambient_obstack;
    }

  /* install argument types - normally set by "build_function_type ()". */
  TYPE_ARG_TYPES (function_type) = get_arg_type_list (proto, METHOD_DEF, 0);

  /* install return type */
  TREE_TYPE (function_type) = groktypename (TREE_TYPE (proto));

  return comptypes (TREE_TYPE (METHOD_DEFINITION (method)), function_type);
}

/* return 1 if `proto1' is consistent with `proto2' */

static int
comp_proto_with_proto (proto1, proto2)
     tree proto1, proto2;
{
  static tree function_type1 = 0, function_type2 = 0;

  /* create a couple function_type node's once */
  if (!function_type1)
    {
      struct obstack *ambient_obstack = current_obstack;
      
      current_obstack = &permanent_obstack;
      function_type1 = make_node (FUNCTION_TYPE);
      function_type2 = make_node (FUNCTION_TYPE);
      current_obstack = ambient_obstack;
    }

  /* install argument types - normally set by "build_function_type ()". */
  TYPE_ARG_TYPES (function_type1) = get_arg_type_list (proto1, METHOD_REF, 0);
  TYPE_ARG_TYPES (function_type2) = get_arg_type_list (proto2, METHOD_REF, 0);

  /* install return type */
  TREE_TYPE (function_type1) = groktypename (TREE_TYPE (proto1));
  TREE_TYPE (function_type2) = groktypename (TREE_TYPE (proto2));

  return comptypes (function_type1, function_type2);
}

/*
 *  - generate an identifier for the function. the format is "_n_cls",
 *    where 1 <= n <= nMethods, and cls is the name the implementation we
 *    are processing.
 *  - install the return type from the method declaration.
 *  - if we have a prototype, check for type consistency.
 */
static void
really_start_method (method, parmlist)
     tree method, parmlist;
{
  tree sc_spec, ret_spec, ret_decl, decl_specs;
  tree method_decl, method_id;
  char *buf;

  /* synth the storage class & assemble the return type */
  sc_spec = tree_cons (NULLT, ridpointers[(int) RID_STATIC], NULLT);
  ret_spec = TREE_PURPOSE (TREE_TYPE (method));
  decl_specs = chainon (sc_spec, ret_spec);

  if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
    {
      /* Make sure this is big enough for any plausible method label.  */
      buf = (char *) alloca (50
			     + strlen (IDENTIFIER_POINTER (METHOD_SEL_NAME (method)))
			     + strlen (IDENTIFIER_POINTER (CLASS_NAME (implementation_context))));
#ifdef OBJC_GEN_METHOD_LABEL
      OBJC_GEN_METHOD_LABEL (buf,
			     TREE_CODE (method) == INSTANCE_METHOD_DECL,
			     IDENTIFIER_POINTER (CLASS_NAME (implementation_context)),
			     NULL,
			     IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));
#else
      sprintf (buf, "_%d_%s", ++method_slot,
	       IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
#endif
    }
  else				/* we have a category */
    {
      /* Make sure this is big enough for any plausible method label.  */
      buf = (char *) alloca (50
			     + strlen (IDENTIFIER_POINTER (METHOD_SEL_NAME (method)))
			     + strlen (IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)))
			     + strlen (IDENTIFIER_POINTER (CLASS_NAME (implementation_context))));
#ifdef OBJC_GEN_METHOD_LABEL
      OBJC_GEN_METHOD_LABEL (buf,
			     TREE_CODE (method) == INSTANCE_METHOD_DECL,
			     IDENTIFIER_POINTER (CLASS_NAME (implementation_context)),
			     IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)),
			     IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));
#else
      sprintf (buf, "_%d_%s_%s", ++method_slot,
	       IDENTIFIER_POINTER (CLASS_NAME (implementation_context)),
	       IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
#endif
    }

  method_id = get_identifier (buf);

  method_decl = build_nt (CALL_EXPR, method_id, parmlist, NULLT);

  /* check the declarator portion of the return type for the method */
  if (ret_decl = TREE_VALUE (TREE_TYPE (method)))
    {
      /*
       * unite the complex decl (specified in the abstract decl) with the
       * function decl just synthesized...(int *), (int (*)()), (int (*)[]).
       */
      tree save_expr = expr_last (ret_decl);

      TREE_OPERAND (save_expr, 0) = method_decl;
      method_decl = ret_decl;
      /* fool the parser into thinking it is starting a function */
      start_function (decl_specs, method_decl, 0);
      /* unhook...this has the effect of restoring the abstract declarator */
      TREE_OPERAND (save_expr, 0) = NULLT;
    }
  else
    {
      TREE_VALUE (TREE_TYPE (method)) = method_decl;
      /* fool the parser into thinking it is starting a function */
      start_function (decl_specs, method_decl, 0);
      /* unhook...this has the effect of restoring the abstract declarator */
      TREE_VALUE (TREE_TYPE (method)) = NULLT;
    }

  METHOD_DEFINITION (method) = current_function_decl;

  /* check consistency...start_function (), pushdecl (), duplicate_decls (). */

  if (implementation_template != implementation_context)
    {
      tree chain, proto;

      if (TREE_CODE (method) == INSTANCE_METHOD_DECL)
	chain = CLASS_NST_METHODS (implementation_template);
      else
	chain = CLASS_CLS_METHODS (implementation_template);

      if (proto = lookup_method (chain, METHOD_SEL_NAME (method)))
	{
	  if (!comp_method_with_proto (method, proto))
	    {
	      fprintf (stderr, "%s: In method `%s'\n", input_filename,
		       IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));
	      if (TREE_CODE (method) == INSTANCE_METHOD_DECL)
		{
		  error_with_method ("conflicting types for", '-', method);
		  error_with_method ("previous declaration of", '-', proto);
		}
	      else
		{
		  error_with_method ("conflicting types for", '+', method);
		  error_with_method ("previous declaration of", '+', proto);
		}
	    }
	}
    }
}

/*
 * the following routine is always called...this "architecture" is to
 * accommodate "old-style" variable length selectors.
 *
 *	- a:a b:b // prototype  ; id c; id d; // old-style
 */
void
continue_method_def ()
{
  tree parmlist;

  if (METHOD_ADD_ARGS (method_context) == (tree)1)
    /*
     * we have a `, ...' immediately following the selector.
     */
    parmlist = get_parm_info (0);
  else
    parmlist = get_parm_info (1); /* place a `void_at_end' */

  /* set self_decl from the first argument...this global is used by
   * build_ivar_reference ().build_indirect_ref ().
   */
  self_decl = TREE_PURPOSE (parmlist);

  poplevel (0, 0, 0);		/* must be called BEFORE "start_function ()" */

  really_start_method (method_context, parmlist);

  store_parm_decls ();		/* must be called AFTER "start_function ()" */
}

void
add_objc_decls ()
{
  if (!_OBJC_SUPER_decl)
    _OBJC_SUPER_decl = start_decl (get_identifier (_TAG_SUPER),
				   build_tree_list (NULLT, objc_super_template), 0);

  /* this prevents `unused variable' warnings when compiling with `-Wall' */
  DECL_IN_SYSTEM_HEADER (_OBJC_SUPER_decl) = 1;
}

/*
 *	_n_Method (id self, SEL sel, ...)
 *	{
 *		struct objc_super _S;
 *
 *		_msgSuper ((_S.self = self, _S.class = _cls, &_S), ...);
 *	}
 */
tree
get_super_receiver ()
{
  if (method_context)
    {
      tree super_expr, super_expr_list;

      /* set receiver to self */
      super_expr = build_component_ref (_OBJC_SUPER_decl, self_id);
      super_expr = build_modify_expr (super_expr, NOP_EXPR, self_decl);
      super_expr_list = build_tree_list (NULLT, super_expr);

      /* set class to begin searching */
      super_expr = build_component_ref (_OBJC_SUPER_decl, get_identifier ("class"));

      if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
	{
	  /* [_cls, __cls]Super are "pre-built" in synth_foward_declarations () */

	  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
	    super_expr = build_modify_expr (super_expr, NOP_EXPR, _clsSuper_ref);
	  else
	    super_expr = build_modify_expr (super_expr, NOP_EXPR, __clsSuper_ref);
	}
      else			/* we have a category... */
	{
	  tree params, super_name = CLASS_SUPER_NAME (implementation_template);
	  tree funcCall;

	  if (!super_name)  /* Barf if super used in a category of Object. */
	    {
	      error("no super class declared in interface for `%s'",
		    IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	      return error_mark_node;
	    }

	  add_class_reference (super_name);

	  params = build_tree_list (NULLT,
				    my_build_string (IDENTIFIER_LENGTH (super_name) + 1,
						     IDENTIFIER_POINTER (super_name)));

	  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
	    funcCall = build_function_call (objc_getClass_decl, params);
	  else
	    funcCall = build_function_call (objc_getMetaClass_decl, params);

	  /* cast! */
	  TREE_TYPE (funcCall) = TREE_TYPE (_clsSuper_ref);
	  super_expr = build_modify_expr (super_expr, NOP_EXPR, funcCall);
	}
      chainon (super_expr_list, build_tree_list (NULL_TREE, super_expr));

      super_expr = build_unary_op (ADDR_EXPR, _OBJC_SUPER_decl, 0);
      chainon (super_expr_list, build_tree_list (NULL_TREE, super_expr));

      return build_compound_expr (super_expr_list);
    }
  else
    {
      error ("[super ...] must appear in a method context");
      return error_mark_node;
    }
}

static tree
encode_method_def (func_decl)
      tree func_decl;
{
  tree parms;
  int stack_size = 0;
  char buffer[40];
  tree result;

  /* return type */
  encode_type (TREE_TYPE (TREE_TYPE (func_decl)),
	       OBJC_ENCODE_DONT_INLINE_DEFS);
  /* stack size */
  for (parms = DECL_ARGUMENTS (func_decl); parms;
       parms = TREE_CHAIN (parms))
    stack_size += TREE_INT_CST_LOW (TYPE_SIZE (DECL_ARG_TYPE (parms)))
		  / BITS_PER_UNIT;

  sprintf (buffer, "%d", stack_size);
  obstack_grow (&util_obstack, buffer, strlen (buffer));

  /* argument types */
  for (parms = DECL_ARGUMENTS (func_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      int offset_in_bytes;
  
      /* type */ 
      encode_type (TREE_TYPE (parms), OBJC_ENCODE_DONT_INLINE_DEFS);
  
      /* compute offset */
      if (GET_CODE (DECL_INCOMING_RTL (parms)) == MEM)
        {
	  rtx addr = XEXP (DECL_INCOMING_RTL (parms), 0);
	  
	  /* ??? Here we assume that the parm address is indexed
	      off the frame pointer or arg pointer.
	      If that is not true, we produce meaningless results,
	      but do not crash.  */
	  if (GET_CODE (addr) == PLUS
	      && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	    offset_in_bytes = INTVAL (XEXP (addr, 1));
	  else
	    offset_in_bytes = 0;
	  
	  /* This is the case where the parm is passed as an int or double
	      and it is converted to a char, short or float and stored back
	      in the parmlist.  In this case, describe the parm
	      with the variable's declared type, and adjust the address
	      if the least significant bytes (which we are using) are not
	      the first ones.  */
#if BYTES_BIG_ENDIAN
	  if (TREE_TYPE (parms) != DECL_ARG_TYPE (parms))
	    offset_in_bytes += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
				- GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif
	}
      else
        offset_in_bytes = 0;
      
      /* The "+ 4" is a total hack to account for the return pc and
         saved fp on the 68k.  We should redefine this format! */
      sprintf (buffer, "%d", offset_in_bytes + 8);
      obstack_grow (&util_obstack, buffer, strlen (buffer));
    }

  result = get_identifier (obstack_finish (&util_obstack));
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

void
finish_method_def ()
{
  METHOD_ENCODING (method_context) =
    encode_method_def (current_function_decl);

  finish_function (0);

  /* this must be done AFTER finish_function, since the optimizer may
     find "may be used before set" errors.  */
  method_context = NULLT;	/* required to implement _msgSuper () */
}

int
lang_report_error_function (decl)
      tree decl;
{
  if (method_context)
    {
      fprintf (stderr, "In method `%s'\n",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (method_context)));
      return 1;
    }
  else
    return 0;
}

static int
is_complex_decl (type)
     tree type;
{
  return (TREE_CODE (type) == ARRAY_TYPE
	  || TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == POINTER_TYPE);
}


/* Code to convert a decl node into text for a declaration in C.  */

static char tmpbuf[256];

static void
adorn_decl (decl, str)
     tree decl;
     char *str;
{
  enum tree_code code = TREE_CODE (decl);

  if (code == ARRAY_REF)
    {
      tree anIntCst = TREE_OPERAND (decl, 1);

      sprintf (str + strlen (str), "[%d]", TREE_INT_CST_LOW (anIntCst));
    }
  else if (code == ARRAY_TYPE)
    {
      tree anIntCst = TYPE_SIZE (decl);
      tree array_of = TREE_TYPE (decl);

      sprintf (str + strlen (str), "[%d]",
	       TREE_INT_CST_LOW (anIntCst)/TREE_INT_CST_LOW (TYPE_SIZE (array_of)));
    }
  else if (code == CALL_EXPR)
    strcat (str, "()");
  else if (code == FUNCTION_TYPE)
    {
      tree chain  = TYPE_ARG_TYPES (decl); /* a list of types */
      strcat (str, "(");
      while (chain && TREE_VALUE (chain) != void_type_node)
	{
	  gen_declaration (TREE_VALUE (chain), str);
	  chain = TREE_CHAIN (chain);
	  if (chain && TREE_VALUE (chain) != void_type_node)
	    strcat (str, ",");
	}
      strcat (str, ")");
    }
  else
    {
      strcpy (tmpbuf, "*"); strcat (tmpbuf, str);
      strcpy (str, tmpbuf);
    }
}

static char *
gen_declarator (decl, buf, name)
     tree decl;
     char *buf;
     char *name;
{
  if (decl)
    {
      enum tree_code code = TREE_CODE (decl);
      char *str;
      tree op;
      int wrap = 0;

      switch (code)
	{
	case ARRAY_REF: case INDIRECT_REF: case CALL_EXPR:
	  {
	    op = TREE_OPERAND (decl, 0);

	    /* we have a pointer to a function or array...(*)(), (*)[] */
	    if ((code == ARRAY_REF || code == CALL_EXPR) &&
		(op && TREE_CODE (op) == INDIRECT_REF))
	      wrap = 1;

	    str = gen_declarator (op, buf, name);

	    if (wrap)
	      {
		strcpy (tmpbuf, "("); strcat (tmpbuf, str); strcat (tmpbuf, ")");
		strcpy (str, tmpbuf);
	      }

	    adorn_decl (decl, str);
	    break;
	  }
	case ARRAY_TYPE: case FUNCTION_TYPE: case POINTER_TYPE:
	  {
	    str = strcpy (buf, name);

	    /* this clause is done iteratively...rather than recursively */
	    do
	      {
		op = is_complex_decl (TREE_TYPE (decl))
		  ? TREE_TYPE (decl)
		    : NULLT;

		adorn_decl (decl, str);

		/* we have a pointer to a function or array...(*)(), (*)[] */
		if ((code == POINTER_TYPE) &&
		    (op && (TREE_CODE (op) == FUNCTION_TYPE
			    || TREE_CODE (op) == ARRAY_TYPE)))
		  {
		    strcpy (tmpbuf, "("); strcat (tmpbuf, str); strcat (tmpbuf, ")");
		    strcpy (str, tmpbuf);
		  }

		decl = is_complex_decl (TREE_TYPE (decl))
		  ? TREE_TYPE (decl)
		    : NULLT;
	      }
	    while (decl && (code = TREE_CODE (decl)));

	    break;
	  }
	case IDENTIFIER_NODE:
	  /* will only happen if we are processing a "raw" expr-decl. */
	  return strcpy (buf, IDENTIFIER_POINTER (decl));
	}

      return str;
    }
  else				/* we have an abstract declarator or a _DECL node */
    {
      return strcpy (buf, name);
    }
}

static void
gen_declspecs (declspecs, buf, raw)
     tree declspecs;
     char *buf;
     int raw;
{
  if (raw)
    {
      tree chain;

      for (chain = declspecs; chain; chain = TREE_CHAIN (chain))
	{
	  tree aspec = TREE_VALUE (chain);

	  if (TREE_CODE (aspec) == IDENTIFIER_NODE)
	    strcat (buf, IDENTIFIER_POINTER (aspec));
	  else if (TREE_CODE (aspec) == RECORD_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  if (!TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "struct ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));
		}
	      else
		strcat (buf, "untagged struct");
	    }
	  else if (TREE_CODE (aspec) == UNION_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  if (!TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "union ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));
		}
	      else
		strcat (buf, "untagged union");
	    }
	  else if (TREE_CODE (aspec) == ENUMERAL_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  if (!TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "enum ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));
		}
	      else
		strcat (buf, "untagged enum");
	    }
	  strcat (buf, " ");
	}
    }
  else
    switch (TREE_CODE (declspecs))
      {
	/* type specifiers */

      case INTEGER_TYPE:	/* signed integer types */

        if (declspecs == short_integer_type_node) /* 's' */
          strcat (buf, "short int ");
        else if (declspecs == integer_type_node) /* 'i' */
          strcat (buf, "int ");
        else if (declspecs == long_integer_type_node) /* 'l' */
          strcat (buf, "long int ");
        else if (declspecs == signed_char_type_node || /* 'c' */
  	         declspecs == char_type_node)
          strcat (buf, "char ");

        /* unsigned integer types */

        else if (declspecs == short_unsigned_type_node)	/* 'S' */
          strcat (buf, "unsigned short ");
        else if (declspecs == unsigned_type_node) /* 'I' */
          strcat (buf, "unsigned int ");
        else if (declspecs == long_unsigned_type_node) /* 'L' */
          strcat (buf, "unsigned long ");
        else if (declspecs == unsigned_char_type_node) /* 'C' */
          strcat (buf, "unsigned char ");
	break;

      case REAL_TYPE:		/* floating point types */

        if (declspecs == float_type_node) /* 'f' */
          strcat (buf, "float ");
        else if (declspecs == double_type_node)	/* 'd' */
          strcat (buf, "double ");
	else if (declspecs == long_double_type_node) /* 'd' */
          strcat (buf, "long double ");
	break;

      case RECORD_TYPE:
	if (!TREE_STATIC_TEMPLATE (declspecs))
	  strcat (buf, "struct ");
	if (TYPE_NAME (declspecs) &&
	    (TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE))
	  {
	    strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));
	    strcat (buf, " ");
	  }
	break;
      case UNION_TYPE:
	strcat (buf, "union ");
	if (TYPE_NAME (declspecs) &&
	    (TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE))
	  {
	    strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));
	    strcat (buf, " ");
	  }
	break;
      case ENUMERAL_TYPE:
	strcat (buf, "enum ");
	if (TYPE_NAME (declspecs) &&
	    (TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE))
	  {
	    strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));
	    strcat (buf, " ");
	  }
	break;
      case VOID_TYPE:
	strcat (buf, "void ");
      }
}

static char *
gen_declaration (atype_or_adecl, buf)
     tree atype_or_adecl;
     char *buf;
{
  char declbuf[256];

  if (TREE_CODE (atype_or_adecl) == TREE_LIST)
    {
      tree declspecs;		/* "identifier_node", "record_type" */
      tree declarator;		/* "array_ref", "indirect_ref", "call_expr"... */

      /* we have a "raw", abstract declarator (typename) */
      declarator = TREE_VALUE (atype_or_adecl);
      declspecs  = TREE_PURPOSE (atype_or_adecl);

      gen_declspecs (declspecs, buf, 1);
      strcat (buf, gen_declarator (declarator, declbuf, ""));
    }
  else
    {
      tree atype;
      tree declspecs;		/* "integer_type", "real_type", "record_type"... */
      tree declarator;		/* "array_type", "function_type", "pointer_type". */

      if (TREE_CODE (atype_or_adecl) == FIELD_DECL
	  || TREE_CODE (atype_or_adecl) == PARM_DECL
	  || TREE_CODE (atype_or_adecl) == FUNCTION_DECL)
	atype = TREE_TYPE (atype_or_adecl);
      else
	atype = atype_or_adecl;	/* assume we have a *_type node */

      if (is_complex_decl (atype))
	{
	  tree chain;

	  /* get the declaration specifier...it is at the end of the list */
	  declarator = chain = atype;
	  do
	    chain = TREE_TYPE (chain); /* not TREE_CHAIN (chain); */
	  while (is_complex_decl (chain));
	  declspecs = chain;
	}
      else
	{
	  declspecs = atype;
	  declarator = NULLT;
	}

      gen_declspecs (declspecs, buf, 0);

      if (TREE_CODE (atype_or_adecl) == FIELD_DECL
	  || TREE_CODE (atype_or_adecl) == PARM_DECL
	  || TREE_CODE (atype_or_adecl) == FUNCTION_DECL)
	{
	  if (declarator)
	    {
	      strcat (buf, gen_declarator (declarator, declbuf,
					   IDENTIFIER_POINTER (DECL_NAME (atype_or_adecl))));
	    }
	  else
	    strcat (buf, IDENTIFIER_POINTER (DECL_NAME (atype_or_adecl)));
	}
      else
	{
	  strcat (buf, gen_declarator (declarator, declbuf, ""));
	}
    }
  return buf;
}

#define RAW_TYPESPEC(meth) (TREE_VALUE (TREE_PURPOSE (TREE_TYPE (meth))))

static char *
gen_method_decl (method, buf)
     tree method;
     char *buf;
{
  tree chain;

  if (RAW_TYPESPEC (method) != objc_object_reference)
    {
      strcpy (buf, "(");
      gen_declaration (TREE_TYPE (method), buf);
      strcat (buf, ")");
    }

  chain = METHOD_SEL_ARGS (method);
  if (chain)
    {				/* we have a chain of keyword_decls */
      do
        {
	  if (KEYWORD_KEY_NAME (chain))
	    strcat (buf, IDENTIFIER_POINTER (KEYWORD_KEY_NAME (chain)));

	  strcat (buf, ":");
	  if (RAW_TYPESPEC (chain) != objc_object_reference)
	    {
	      strcat (buf, "(");
	      gen_declaration (TREE_TYPE (chain), buf);
	      strcat (buf, ")");
	    }
	  strcat (buf, IDENTIFIER_POINTER (KEYWORD_ARG_NAME (chain)));
	  if (chain = TREE_CHAIN (chain))
	    strcat (buf, " ");
        }
      while (chain);

      if (METHOD_ADD_ARGS (method) == (tree)1)
        strcat (buf, ", ...");
      else if (METHOD_ADD_ARGS (method))
        {			/* we have a tree list node as generate by `get_parm_info ()' */
	  chain  = TREE_PURPOSE (METHOD_ADD_ARGS (method));
          /* know we have a chain of parm_decls */
          while (chain)
            {
	      strcat (buf, ", ");
	      gen_declaration (chain, buf);
	      chain = TREE_CHAIN (chain);
            }
	}
    }
  else				/* we have a unary selector */
    {
      strcat (buf, IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));
    }

  return buf;
}

void
gen_prototype (fp, decl)
     FILE *fp;
     tree decl;
{
  /* we have a function definition - generate prototype */
  bzero (errbuf, BUFSIZE);
  gen_declaration (decl, errbuf);
  fprintf (fp, "%s;\n", errbuf);
}
/*
 *  debug info...
 */
static void
dump_interface (fp, chain)
     FILE *fp;
     tree chain;
{
  char *buf = (char *)xmalloc (256);
  char *my_name = IDENTIFIER_POINTER (CLASS_NAME (chain));
  tree ivar_decls = CLASS_RAW_IVARS (chain);
  tree nst_methods = CLASS_NST_METHODS (chain);
  tree cls_methods = CLASS_CLS_METHODS (chain);

  fprintf (fp, "\n@interface %s", my_name);

  if (CLASS_SUPER_NAME (chain))
    {
      char *super_name = IDENTIFIER_POINTER (CLASS_SUPER_NAME (chain));
      fprintf (fp, " : %s\n", super_name);
    }
  else
    fprintf (fp, "\n");

  if (ivar_decls)
    {
      fprintf (fp, "{\n");
      do
	{
	  bzero (buf, 256);
	  fprintf (fp, "\t%s;\n", gen_declaration (ivar_decls, buf));
	  ivar_decls = TREE_CHAIN (ivar_decls);
	}
      while (ivar_decls);
      fprintf (fp, "}\n");
    }

  while (nst_methods)
    {
      bzero (buf, 256);
      fprintf (fp, "- %s;\n", gen_method_decl (nst_methods, buf));
      nst_methods = TREE_CHAIN (nst_methods);
    }

  while (cls_methods)
    {
      bzero (buf, 256);
      fprintf (fp, "+ %s;\n", gen_method_decl (cls_methods, buf));
      cls_methods = TREE_CHAIN (cls_methods);
    }
  fprintf (fp, "\n@end");
}

void
init_objc ()
{
  /* Add the special tree codes of Objective C to the tables.  */

  gcc_obstack_init (&util_obstack);
  util_firstobj = (char *) obstack_finish (&util_obstack);

  tree_code_type
    = (char **) realloc (tree_code_type,
			 sizeof (char *) * LAST_OBJC_TREE_CODE);
  tree_code_length
    = (int *) realloc (tree_code_length,
		       sizeof (int) * LAST_OBJC_TREE_CODE);
  tree_code_name
    = (char **) realloc (tree_code_name,
			 sizeof (char *) * LAST_OBJC_TREE_CODE);
  bcopy (objc_tree_code_type,
	 tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE,
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));
  bcopy (objc_tree_code_length,
	 tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE,
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (int)));
  bcopy (objc_tree_code_name,
	 tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE,
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));

  errbuf = (char *)xmalloc (BUFSIZE);
  hash_init ();
  synth_module_prologue ();
}

void
finish_objc ()
{
  struct imp_entry *impent;
  tree chain;

  generate_forward_declaration_to_string_table ();

#ifdef OBJC_PROLOGUE
  OBJC_PROLOGUE;
#endif

  if (implementation_context || sel_refdef_chain)
    generate_objc_symtab_decl ();

  for (impent = imp_list; impent; impent = impent->next)
    {
      implementation_context = impent->imp_context;
      implementation_template = impent->imp_template;

      _OBJC_CLASS_decl = impent->class_decl;
      _OBJC_METACLASS_decl = impent->meta_decl;

      if (TREE_CODE (implementation_context) == IMPLEMENTATION_TYPE)
	{
	  /* all of the following reference the string pool...  */
	  generate_ivar_lists ();
	  generate_dispatch_tables ();
	  generate_shared_structures ();
	}
      else
	{
	  generate_dispatch_tables ();
	  generate_category (implementation_context);
	}
    }

  /* If we are using an array of selectors, we must always
     finish up the array decl even if no selectors were used.  */
#ifndef OBJC_SELECTORS_WITHOUT_LABELS
  if (sel_ref_chain)
#endif
    build_selector_translation_table ();

  if (implementation_context || sel_refdef_chain)
    {
      /* Arrange for Objc data structures to be initialized at run time.  */

      char *init_name = build_module_descriptor ();
      if (init_name)
	assemble_constructor (init_name);
    }

  /* dump the string table last */

  if (sel_refdef_chain)
    {
      build_message_selector_pool ();
    }

  /* dump the class references...this forces the appropriate classes
     to be linked into the executable image, preserving unix archive
     semantics...this can be removed when we move to a more dynamically
     linked environment
     */
  for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
    handle_class_ref (chain);

  for (impent = imp_list; impent; impent = impent->next)
    handle_impent (impent);

#if 0 /* If GAS has such a bug, let's fix it.  */
  /*** this fixes a gross bug in the assembler...it `expects' #APP to have
   *** a matching #NO_APP, or it crashes (sometimes). app_disable () will
   *** insure this is the case. 5/19/89, s.naroff.
   ***/
  if (cls_ref_chain || imp_list)
    app_disable ();
#endif

  if (flag_gen_declaration)
    {
      add_class (implementation_context);
      dump_interface (gen_declaration_file, implementation_context);
    }
  if (warn_selector)
    {
      int slot;
      
      /* Run through the selector hash tables and print a warning for any
         selector which has multiple methods. */
      
      for (slot = 0; slot < SIZEHASHTABLE; slot++)
        {
	  hash hsh;
	  
	  for (hsh = cls_method_hash_list[slot]; hsh; hsh = hsh->next)
	    {
	      if (hsh->list)
	        {
	          tree meth = hsh->key;
	          char type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL)
			      ? '-' : '+';
	          attr loop;
	      
		  warning ("potential selector conflict for method `%s'",
			   IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));
	          warn_with_method ("found", type, meth);
	          for (loop = hsh->list; loop; loop = loop->next)
	            warn_with_method ("found", type, loop->value);
	        }
	    }
	}
    
      for (slot = 0; slot < SIZEHASHTABLE; slot++)
        {
	  hash hsh;
	  
	  for (hsh = nst_method_hash_list[slot]; hsh; hsh = hsh->next)
	    {
	      if (hsh->list)
	        {
	          tree meth = hsh->key;
	          char type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL)
			      ? '-' : '+';
	          attr loop;
	      
		  warning ("potential selector conflict for method `%s'",
			   IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));
	          warn_with_method ("found", type, meth);
	          for (loop = hsh->list; loop; loop = loop->next)
	            warn_with_method ("found", type, loop->value);
	        }
	    }
	}
    }
}

/* Subroutines of finish_objc.  */

handle_class_ref (chain)
     tree chain;
{
  tree decl;
  char *string
    = (char *) alloca (strlen (IDENTIFIER_POINTER (TREE_VALUE (chain))) + 30);

  sprintf (string, "__objc_class_name_%s",
	   IDENTIFIER_POINTER (TREE_VALUE (chain)));

  /* Make a decl for this name, so we can use its address in a tree.  */
  decl = build_decl (VAR_DECL, get_identifier (string), char_type_node);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  pushdecl (decl);
  rest_of_decl_compilation (decl, 0, 0, 0);

  /* Make following constant read-only (why not)?  */
  text_section ();

  /* Output a constant to reference this address.  */
  output_constant (build1 (ADDR_EXPR, string_type_node, decl),
		   int_size_in_bytes (string_type_node));
}

handle_impent (impent)
     struct imp_entry *impent;
{
  implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;

  if (TREE_CODE (impent->imp_context) == IMPLEMENTATION_TYPE)
    {
      char *string
	= (char *) alloca (strlen (IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context))) + 30);

      sprintf (string, "__objc_class_name_%s",
	       IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)));
      assemble_global (string);
      assemble_label (string);
    }
  else if (TREE_CODE (impent->imp_context) == CATEGORY_TYPE)
    {
      char *string
	= (char *) alloca (strlen (IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)))
			   + strlen (IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context)))
			   + 30);

      /* Do the same for categories.  Even though no references to these
	  symbols are generated automatically by the compiler, it gives
	  you a handle to pull them into an archive by hand. */
      sprintf (string, "__objc_category_name_%s_%s",
	       IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context)),
	       IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context)));
      assemble_global (string);
      assemble_label (string);
    }
}

#ifdef DEBUG

static void
objc_debug (fp)
     FILE *fp;
{
  char *buf = (char *)xmalloc (256);

  {				/* dump function prototypes */
    tree loop = _OBJC_MODULES_decl;

    fprintf (fp, "\n\nfunction prototypes:\n");
    while (loop)
      {
	if (TREE_CODE (loop) == FUNCTION_DECL && DECL_INITIAL (loop))
	  {
	    /* we have a function definition - generate prototype */
            bzero (errbuf, BUFSIZE);
	    gen_declaration (loop, errbuf);
	    fprintf (fp, "%s;\n", errbuf);
	  }
	loop = TREE_CHAIN (loop);
      }
  }
  {				/* dump global chains */
    tree loop;
    int i, index = 0, offset = 0;
    hash hashlist;

    for (i = 0; i < SIZEHASHTABLE; i++)
      {
	if (hashlist = nst_method_hash_list[i])
	  {
	    fprintf (fp, "\n\nnst_method_hash_list[%d]:\n", i);
	    do
	      {
		bzero (buf, 256);
		fprintf (fp, "-%s;\n", gen_method_decl (hashlist->key, buf));
		hashlist = hashlist->next;
	      }
	    while (hashlist);
	  }
      }
    for (i = 0; i < SIZEHASHTABLE; i++)
      {
	if (hashlist = cls_method_hash_list[i])
	  {
	    fprintf (fp, "\n\ncls_method_hash_list[%d]:\n", i);
	    do
	      {
		bzero (buf, 256);
		fprintf (fp, "-%s;\n", gen_method_decl (hashlist->key, buf));
		hashlist = hashlist->next;
	      }
	    while (hashlist);
	  }
      }
    fprintf (fp, "\nsel_refdef_chain:\n");
    for (loop = sel_refdef_chain; loop; loop = TREE_CHAIN (loop))
      {
	fprintf (fp, "(index: %4d offset: %4d) %s\n", index, offset,
		 IDENTIFIER_POINTER (TREE_VALUE (loop)));
	index++;
	/* add one for the '\0' character */
	offset += IDENTIFIER_LENGTH (TREE_VALUE (loop)) + 1;
      }
    fprintf (fp, "\n (max_selector_index: %4d.\n", max_selector_index);
  }
}
#endif

void
print_lang_statistics ()
{
}
