/* Process declarations and variables for C compiler.
   Copyright (C) 1988, 92-98, 1999 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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
Boston, MA 02111-1307, USA.  */


/* Process declarations and symbol lookup for C front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "cp-tree.h"
#include "decl.h"
#include "lex.h"
#include <signal.h>
#include "obstack.h"
#include "defaults.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "../hash.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern tree builtin_return_address_fndecl;

extern struct obstack permanent_obstack;
extern struct obstack* saveable_obstack;

extern int current_class_depth;

extern tree static_ctors, static_dtors;

extern int static_labelno;

extern tree current_namespace;
extern tree global_namespace;

extern void (*print_error_function) PROTO((char *));
extern int (*valid_lang_attribute) PROTO ((tree, tree, tree, tree));

/* Obstack used for remembering local class declarations (like
   enums and static (const) members.  */
#include "stack.h"
struct obstack decl_obstack;
static struct stack_level *decl_stack;

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef BOOL_TYPE_SIZE
#ifdef SLOW_BYTE_ACCESS
#define BOOL_TYPE_SIZE ((SLOW_BYTE_ACCESS) ? (POINTER_SIZE) : (CHAR_TYPE_SIZE))
#else
#define BOOL_TYPE_SIZE CHAR_TYPE_SIZE
#endif
#endif

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

#ifndef WCHAR_TYPE
#define WCHAR_TYPE "int"
#endif

static tree grokparms				PROTO((tree, int));
static tree lookup_nested_type			PROTO((tree, tree));
static const char *redeclaration_error_message	PROTO((tree, tree));

static struct stack_level *push_decl_level PROTO((struct stack_level *,
						  struct obstack *));
static void push_binding_level PROTO((struct binding_level *, int,
				      int));
static void pop_binding_level PROTO((void));
static void suspend_binding_level PROTO((void));
static void resume_binding_level PROTO((struct binding_level *));
static struct binding_level *make_binding_level PROTO((void));
static void declare_namespace_level PROTO((void));
static void signal_catch PROTO((int)) ATTRIBUTE_NORETURN;
static void storedecls PROTO((tree));
static void require_complete_types_for_parms PROTO((tree));
static void push_overloaded_decl_1 PROTO((tree));
static int ambi_op_p PROTO((tree));
static int unary_op_p PROTO((tree));
static tree store_bindings PROTO((tree, tree));
static tree lookup_tag_reverse PROTO((tree, tree));
static tree obscure_complex_init PROTO((tree, tree));
static tree maybe_build_cleanup_1 PROTO((tree, tree));
static tree lookup_name_real PROTO((tree, int, int, int));
static void warn_extern_redeclared_static PROTO((tree, tree));
static void grok_reference_init PROTO((tree, tree, tree));
static tree grokfndecl PROTO((tree, tree, tree, tree, int,
			      enum overload_flags, tree,
			      tree, int, int, int, int, int, int, tree));
static tree grokvardecl PROTO((tree, tree, RID_BIT_TYPE *, int, int, tree));
static tree lookup_tag PROTO((enum tree_code, tree,
			      struct binding_level *, int));
static void set_identifier_type_value_with_scope
	PROTO((tree, tree, struct binding_level *));
static void record_builtin_type PROTO((enum rid, const char *, tree));
static void record_unknown_type PROTO((tree, const char *));
static int member_function_or_else PROTO((tree, tree, const char *));
static void bad_specifiers PROTO((tree, const char *, int, int, int, int,
				  int));
static void lang_print_error_function PROTO((char *));
static tree maybe_process_template_type_declaration PROTO((tree, int, struct binding_level*));
static void check_for_uninitialized_const_var PROTO((tree));
static unsigned long typename_hash PROTO((hash_table_key));
static boolean typename_compare PROTO((hash_table_key, hash_table_key));
static void push_binding PROTO((tree, tree, struct binding_level*));
static int add_binding PROTO((tree, tree));
static void pop_binding PROTO((tree, tree));
static tree local_variable_p PROTO((tree));
static tree find_binding PROTO((tree, tree));
static tree select_decl PROTO((tree, int));
static tree unqualified_namespace_lookup PROTO((tree, int));
static int lookup_flags PROTO((int, int));
static tree qualify_lookup PROTO((tree, int));
static tree record_builtin_java_type PROTO((const char *, int));
static const char *tag_name PROTO((enum tag_types code));
static void find_class_binding_level PROTO((void));
static struct binding_level *innermost_nonclass_level PROTO((void));
static tree poplevel_class PROTO((void));
static void warn_about_implicit_typename_lookup PROTO((tree, tree));
static int walk_namespaces_r PROTO((tree, walk_namespaces_fn, void *));
static int walk_globals_r PROTO((tree, void *));

#if defined (DEBUG_CP_BINDING_LEVELS)
static void indent PROTO((void));
#endif

/* A node which has tree code ERROR_MARK, and whose type is itself.
   All erroneous expressions are replaced with this node.  All functions
   that accept nodes as arguments should avoid generating error messages
   if this node is one of the arguments, since it is undesirable to get
   multiple error messages from one error in the input.  */

tree error_mark_node;

/* Erroneous argument lists can use this *IFF* they do not modify it.  */
tree error_mark_list;

/* INTEGER_TYPE and REAL_TYPE nodes for the standard data types */

tree short_integer_type_node;
tree integer_type_node;
tree long_integer_type_node;
tree long_long_integer_type_node;

tree short_unsigned_type_node;
tree unsigned_type_node;
tree long_unsigned_type_node;
tree long_long_unsigned_type_node;

tree ptrdiff_type_node;

tree unsigned_char_type_node;
tree signed_char_type_node;
tree char_type_node;
tree wchar_type_node;
tree signed_wchar_type_node;
tree unsigned_wchar_type_node;

tree wchar_decl_node;

tree float_type_node;
tree double_type_node;
tree long_double_type_node;

tree complex_integer_type_node;
tree complex_float_type_node;
tree complex_double_type_node;
tree complex_long_double_type_node;

tree intQI_type_node;
tree intHI_type_node;
tree intSI_type_node;
tree intDI_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
tree intTI_type_node;
#endif

tree unsigned_intQI_type_node;
tree unsigned_intHI_type_node;
tree unsigned_intSI_type_node;
tree unsigned_intDI_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
tree unsigned_intTI_type_node;
#endif

tree java_byte_type_node;
tree java_short_type_node;
tree java_int_type_node;
tree java_long_type_node;
tree java_float_type_node;
tree java_double_type_node;
tree java_char_type_node;
tree java_boolean_type_node;

/* A VOID_TYPE node, and the same, packaged in a TREE_LIST.  */

tree void_type_node, void_list_node;
tree void_zero_node;

/* Nodes for types `void *' and `const void *'.  */

tree ptr_type_node;
tree const_ptr_type_node;

/* Nodes for types `char *' and `const char *'.  */

tree string_type_node, const_string_type_node;

/* Type `char[256]' or something like it.
   Used when an array of char is needed and the size is irrelevant.  */

tree char_array_type_node;

/* Type `int[256]' or something like it.
   Used when an array of int needed and the size is irrelevant.  */

tree int_array_type_node;

/* Type `wchar_t[256]' or something like it.
   Used when a wide string literal is created.  */

tree wchar_array_type_node;

/* The bool data type, and constants */
tree boolean_type_node, boolean_true_node, boolean_false_node;

/* Type `int ()' -- used for implicit declaration of functions.  */

tree default_function_type;

/* Function types `double (double)' and `double (double, double)', etc.  */

static tree double_ftype_double, double_ftype_double_double;
static tree int_ftype_int, long_ftype_long;
static tree float_ftype_float;
static tree ldouble_ftype_ldouble;

/* Function type `int (const void *, const void *, size_t)' */
static tree int_ftype_cptr_cptr_sizet;

/* C++ extensions */
tree vtable_entry_type;
tree delta_type_node;
#if 0
/* Old rtti stuff.  */
tree __baselist_desc_type_node;
tree __i_desc_type_node, __m_desc_type_node;
tree __t_desc_array_type, __i_desc_array_type, __m_desc_array_type;
#endif
tree __t_desc_type_node;
#if 0
tree __tp_desc_type_node;
#endif
tree __access_mode_type_node;
tree __bltn_desc_type_node, __user_desc_type_node, __class_desc_type_node;
tree __ptr_desc_type_node, __attr_desc_type_node, __func_desc_type_node;
tree __ptmf_desc_type_node, __ptmd_desc_type_node;
#if 0
/* Not needed yet?  May be needed one day?  */
tree __bltn_desc_array_type, __user_desc_array_type, __class_desc_array_type;
tree __ptr_desc_array_type, __attr_dec_array_type, __func_desc_array_type;
tree __ptmf_desc_array_type, __ptmd_desc_array_type;
#endif

/* Indicates that there is a type value in some namespace, although
   that is not necessarily in scope at the moment. */

static tree global_type_node;

tree class_star_type_node;
tree class_type_node, record_type_node, union_type_node, enum_type_node;
tree unknown_type_node;
tree opaque_type_node, signature_type_node;
tree sigtable_entry_type;

/* Array type `vtable_entry_type[]' */
tree vtbl_type_node;
tree vtbl_ptr_type_node;

/* namespace std */
tree std_node;
int in_std = 0;

/* Expect only namespace names now. */
static int only_namespace_names;

/* In a destructor, the point at which all derived class destroying
   has been done, just before any base class destroying will be done.  */

tree dtor_label;

/* In a destructor, the last insn emitted after the start of the
   function and the parms.  */

static rtx last_dtor_insn;

/* In a constructor, the last insn emitted after the start of the
   function and the parms, the exception specification and any
   function-try-block.  The constructor initializers are emitted after
   this insn.  */

static rtx last_parm_cleanup_insn;

/* In a constructor, the point at which we are ready to return
   the pointer to the initialized object.  */

tree ctor_label;

/* A FUNCTION_DECL which can call `abort'.  Not necessarily the
   one that the user will declare, but sufficient to be called
   by routines that want to abort the program.  */

tree abort_fndecl;

/* A FUNCTION_DECL for the default `::operator delete'.  */

tree global_delete_fndecl;

extern rtx cleanup_label, return_label;

/* If original DECL_RESULT of current function was a register,
   but due to being an addressable named return value, would up
   on the stack, this variable holds the named return value's
   original location.  */
static rtx original_result_rtx;

/* Sequence of insns which represents base initialization.  */
tree base_init_expr;

/* C++: Keep these around to reduce calls to `get_identifier'.
   Identifiers for `this' in member functions and the auto-delete
   parameter for destructors.  */
tree this_identifier, in_charge_identifier;
tree ctor_identifier, dtor_identifier;
/* Used in pointer to member functions, in vtables, and in sigtables.  */
tree pfn_identifier, index_identifier, delta_identifier, delta2_identifier;
tree pfn_or_delta2_identifier, tag_identifier;
tree vt_off_identifier;

struct named_label_list
{
  struct binding_level *binding_level;
  tree names_in_scope;
  tree label_decl;
  char *filename_o_goto;
  int lineno_o_goto;
  struct named_label_list *next;
};

/* A list (chain of TREE_LIST nodes) of named label uses.
   The TREE_PURPOSE field is the list of variables defined
   in the label's scope defined at the point of use.
   The TREE_VALUE field is the LABEL_DECL used.
   The TREE_TYPE field holds `current_binding_level' at the
   point of the label's use.

   BWAHAHAAHAHahhahahahaah.  No, no, no, said the little chicken.

   Look at the pretty struct named_label_list. See the pretty struct
   with the pretty named fields that describe what they do. See the
   pretty lack of gratuitous casts. Notice the code got a lot cleaner.

   Used only for jumps to as-yet undefined labels, since
   jumps to defined labels can have their validity checked
   by stmt.c.  */

static struct named_label_list *named_label_uses = NULL;

/* A list of objects which have constructors or destructors
   which reside in the global scope.  The decl is stored in
   the TREE_VALUE slot and the initializer is stored
   in the TREE_PURPOSE slot.  */
tree static_aggregates;

/* -- end of C++ */

/* Two expressions that are constants with value zero.
   The first is of type `int', the second of type `void *'.  */

tree integer_zero_node;
tree null_pointer_node;

/* The value for __null (NULL), namely, a zero of an integer type with
   the same number of bits as a pointer.  */
tree null_node;

/* A node for the integer constants 1, 2, and 3.  */

tree integer_one_node, integer_two_node, integer_three_node;

/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  */

static tree enum_next_value;

/* Nonzero means that there was overflow computing enum_next_value.  */

static int enum_overflow;

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */

tree last_function_parms;

/* Parsing a function declarator leaves here a chain of structure
   and enum types declared in the parmlist.  */

static tree last_function_parm_tags;

/* After parsing the declarator that starts a function definition,
   `start_function' puts here the list of parameter names or chain of decls.
   `store_parm_decls' finds it here.  */

static tree current_function_parms;

/* Similar, for last_function_parm_tags.  */
static tree current_function_parm_tags;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static tree shadowed_labels;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

int current_function_returns_null;

/* Set to 0 at beginning of a function definition, and whenever
   a label (case or named) is defined.  Set to value of expression
   returned from function when that value can be transformed into
   a named return value.  */

tree current_function_return_value;

/* Nonzero means give `double' the same size as `float'.  */

extern int flag_short_double;

/* Nonzero means don't recognize any builtin functions.  */

extern int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

extern int flag_no_nonansi_builtin;

/* Nonzero means enable obscure ANSI features and disable GNU extensions
   that might cause ANSI-compliant code to be miscompiled.  */

extern int flag_ansi;

/* Nonzero if we want to support huge (> 2^(sizeof(short)*8-1) bytes)
   objects.  */
extern int flag_huge_objects;

/* Nonzero if we want to conserve space in the .o files.  We do this
   by putting uninitialized data and runtime initialized data into
   .common instead of .data at the expense of not flagging multiple
   definitions.  */
extern int flag_conserve_space;

/* Pointers to the base and current top of the language name stack.  */

extern tree *current_lang_base, *current_lang_stack;

/* C and C++ flags are in decl2.c.  */

/* Set to 0 at beginning of a constructor, set to 1
   if that function does an allocation before referencing its
   instance variable.  */
static int current_function_assigns_this;
int current_function_just_assigned_this;

/* Set to 0 at beginning of a function.  Set non-zero when
   store_parm_decls is called.  Don't call store_parm_decls
   if this flag is non-zero!  */
int current_function_parms_stored;

/* Flag used when debugging spew.c */

extern int spew_debug;

/* This is a copy of the class_shadowed list of the previous class binding
   contour when at global scope.  It's used to reset IDENTIFIER_CLASS_VALUEs
   when entering another class scope (i.e. a cache miss).  */
extern tree previous_class_values;

/* A expression of value 0 with the same precision as a sizetype
   node, but signed.  */
tree signed_size_zero_node;

/* The name of the anonymous namespace, throughout this translation
   unit.  */
tree anonymous_namespace_name;


/* Allocate a level of searching.  */

static
struct stack_level *
push_decl_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct stack_level tem;
  tem.prev = stack;

  return push_stack_level (obstack, (char *)&tem, sizeof (tem));
}

/* For each binding contour we allocate a binding_level structure
   which records the names defined in that contour.
   Contours include:
    0) the global one
    1) one for each function definition,
       where internal declarations of the parameters appear.
    2) one for each compound statement,
       to record its declarations.

   The current meaning of a name can be found by searching the levels
   from the current one out to the global one.

   Off to the side, may be the class_binding_level.  This exists only
   to catch class-local declarations.  It is otherwise nonexistent.

   Also there may be binding levels that catch cleanups that must be
   run when exceptions occur.  Thus, to see whether a name is bound in
   the current scope, it is not enough to look in the
   CURRENT_BINDING_LEVEL.  You should use lookup_name_current_level
   instead.  */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order
       supplied.  There may be OVERLOADs on this list, too, but they
       are wrapped in TREE_LISTs; the TREE_VALUE is the OVERLOAD.  */
    tree names;

    /* A list of structure, union and enum definitions, for looking up
       tag names.
       It is a chain of TREE_LIST nodes, each of whose TREE_PURPOSE is a name,
       or NULL_TREE; and whose TREE_VALUE is a RECORD_TYPE, UNION_TYPE,
       or ENUMERAL_TYPE node.

       C++: the TREE_VALUE nodes can be simple types for
       component_bindings.  */
    tree tags;

    /* A list of USING_DECL nodes. */
    tree usings;

    /* A list of used namespaces. PURPOSE is the namespace,
       VALUE the common ancestor with this binding_level's namespace. */
    tree using_directives;

    /* If this binding level is the binding level for a class, then
       class_shadowed is a TREE_LIST.  The TREE_PURPOSE of each node
       is the name of an entity bound in the class; the TREE_VALUE is
       the IDENTIFIER_CLASS_VALUE before we entered the class.  Thus,
       when leaving class scope, we can restore the
       IDENTIFIER_CLASS_VALUE by walking this list.  The TREE_TYPE is
       the DECL bound by this name in the class.  */
    tree class_shadowed;

    /* Similar to class_shadowed, but for IDENTIFIER_TYPE_VALUE, and
       is used for all binding levels.  */
    tree type_shadowed;

    /* For each level (except not the global one),
       a chain of BLOCK nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The BLOCK node for this level, if one has been preallocated.
       If 0, the BLOCK is allocated (if needed) when the level is popped.  */
    tree this_block;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* List of decls in `names' that have incomplete
       structure or union types.  */
    tree incomplete;

    /* List of VAR_DECLS saved from a previous for statement.
       These would be dead in ANSI-conforming code, but might
       be referenced in ARM-era code.  These are stored in a
       TREE_LIST; the TREE_VALUE is the actual declaration.  */
    tree dead_vars_from_for;

    /* 1 for the level that holds the parameters of a function.
       2 for the level that holds a class declaration.
       3 for levels that hold parameter declarations.  */
    unsigned parm_flag : 4;

    /* 1 means make a BLOCK for this level regardless of all else.
       2 for temporary binding contours created by the compiler.  */
    unsigned keep : 3;

    /* Nonzero if this level "doesn't exist" for tags.  */
    unsigned tag_transparent : 1;

    /* Nonzero if this level can safely have additional
       cleanup-needing variables added to it.  */
    unsigned more_cleanups_ok : 1;
    unsigned have_cleanups : 1;

    /* Nonzero if this level is for storing the decls for template
       parameters and generic decls; these decls will be discarded and
       replaced with a TEMPLATE_DECL.  */
    unsigned pseudo_global : 1;

    /* This is set for a namespace binding level.  */
    unsigned namespace_p : 1;

    /* True if this level is that of a for-statement where we need to
       worry about ambiguous (ARM or ANSI) scope rules.  */
    unsigned is_for_scope : 1;

    /* Two bits left for this word.  */

#if defined(DEBUG_CP_BINDING_LEVELS)
    /* Binding depth at which this level began.  */
    unsigned binding_depth;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
  };

#define NULL_BINDING_LEVEL ((struct binding_level *) NULL)
  
/* The binding level currently in effect.  */

static struct binding_level *current_binding_level;

/* The binding level of the current class, if any.  */

static struct binding_level *class_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level;

/* Nonzero means unconditionally make a BLOCK for the next level pushed.  */

static int keep_next_level_flag;

#if defined(DEBUG_CP_BINDING_LEVELS)
static int binding_depth = 0;
static int is_class_level = 0;

static void
indent ()
{
  register unsigned i;

  for (i = 0; i < binding_depth*2; i++)
    putc (' ', stderr);
}
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */

static tree pushdecl_with_scope	PROTO((tree, struct binding_level *));

static void
push_binding_level (newlevel, tag_transparent, keep)
     struct binding_level *newlevel;
     int tag_transparent, keep;
{
  /* Add this level to the front of the chain (stack) of levels that
     are active.  */
  *newlevel = clear_binding_level;
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
  newlevel->tag_transparent = tag_transparent;
  newlevel->more_cleanups_ok = 1;
  newlevel->keep = keep;
#if defined(DEBUG_CP_BINDING_LEVELS)
  newlevel->binding_depth = binding_depth;
  indent ();
  fprintf (stderr, "push %s level 0x%08x line %d\n",
	   (is_class_level) ? "class" : "block", newlevel, lineno);
  is_class_level = 0;
  binding_depth++;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
}

/* Find the innermost enclosing class scope, and reset
   CLASS_BINDING_LEVEL appropriately.  */

static void
find_class_binding_level ()
{
  struct binding_level *level = current_binding_level;

  while (level && level->parm_flag != 2)
    level = level->level_chain;
  if (level && level->parm_flag == 2)
    class_binding_level = level;
  else
    class_binding_level = 0;
}

static void
pop_binding_level ()
{
  if (global_binding_level)
    {
      /* Cannot pop a level, if there are none left to pop.  */
      if (current_binding_level == global_binding_level)
	my_friendly_abort (123);
    }
  /* Pop the current level, and free the structure for reuse.  */
#if defined(DEBUG_CP_BINDING_LEVELS)
  binding_depth--;
  indent ();
  fprintf (stderr, "pop  %s level 0x%08x line %d\n",
	  (is_class_level) ? "class" : "block",
	  current_binding_level, lineno);
  if (is_class_level != (current_binding_level == class_binding_level))
    {
      indent ();
      fprintf (stderr, "XXX is_class_level != (current_binding_level == class_binding_level)\n");
    }
  is_class_level = 0;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
  {
    register struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;
    level->level_chain = free_binding_level;
#if 0 /* defined(DEBUG_CP_BINDING_LEVELS) */
    if (level->binding_depth != binding_depth)
      abort ();
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
    free_binding_level = level;
    find_class_binding_level ();
  }
}

static void
suspend_binding_level ()
{
  if (class_binding_level)
    current_binding_level = class_binding_level;

  if (global_binding_level)
    {
      /* Cannot suspend a level, if there are none left to suspend.  */
      if (current_binding_level == global_binding_level)
	my_friendly_abort (123);
    }
  /* Suspend the current level.  */
#if defined(DEBUG_CP_BINDING_LEVELS)
  binding_depth--;
  indent ();
  fprintf (stderr, "suspend  %s level 0x%08x line %d\n",
	  (is_class_level) ? "class" : "block",
	  current_binding_level, lineno);
  if (is_class_level != (current_binding_level == class_binding_level))
    {
      indent ();
      fprintf (stderr, "XXX is_class_level != (current_binding_level == class_binding_level)\n");
    }
  is_class_level = 0;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
  current_binding_level = current_binding_level->level_chain;
  find_class_binding_level ();
}

static void
resume_binding_level (b)
     struct binding_level *b;
{
  /* Resuming binding levels is meant only for namespaces,
     and those cannot nest into classes. */
  my_friendly_assert(!class_binding_level, 386);
  /* Also, resuming a non-directly nested namespace is a no-no.  */
  my_friendly_assert(b->level_chain == current_binding_level, 386);
  current_binding_level = b;
#if defined(DEBUG_CP_BINDING_LEVELS)
  b->binding_depth = binding_depth;
  indent ();
  fprintf (stderr, "resume %s level 0x%08x line %d\n",
	   (is_class_level) ? "class" : "block", b, lineno);
  is_class_level = 0;
  binding_depth++;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */
}

/* Create a new `struct binding_level'.  */

static
struct binding_level *
make_binding_level ()
{
  /* NOSTRICT */
  return (struct binding_level *) xmalloc (sizeof (struct binding_level));
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level;
}

/* Return the innermost binding level that is not for a class scope.  */

static struct binding_level *
innermost_nonclass_level ()
{
  struct binding_level *b;

  b = current_binding_level;
  while (b->parm_flag == 2)
    b = b->level_chain;

  return b;
}

/* Nonzero if we are currently in a toplevel binding level.  This
   means either the global binding level or a namespace in a toplevel
   binding level.  Since there are no non-toplevel namespace levels,
   this really means any namespace or pseudo-global level.  We also
   include a class whose context is toplevel.  */

int
toplevel_bindings_p ()
{
  struct binding_level *b = innermost_nonclass_level ();

  return b->namespace_p || b->pseudo_global;
}

/* Nonzero if this is a namespace scope, or if we are defining a class
   which is itself at namespace scope, or whose enclosing class is
   such a class, etc.  */

int
namespace_bindings_p ()
{
  struct binding_level *b = innermost_nonclass_level ();

  return b->namespace_p;
}

void
keep_next_level ()
{
  keep_next_level_flag = 1;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return (current_binding_level->blocks != NULL_TREE
	  || current_binding_level->keep
	  || current_binding_level->names != NULL_TREE
	  || (current_binding_level->tags != NULL_TREE
	      && !current_binding_level->tag_transparent));
}

/* Identify this binding level as a level of parameters.  */

void
declare_parm_level ()
{
  current_binding_level->parm_flag = 1;
}

void
declare_pseudo_global_level ()
{
  current_binding_level->pseudo_global = 1;
}

static void
declare_namespace_level ()
{
  current_binding_level->namespace_p = 1;
}

int
pseudo_global_level_p ()
{
  struct binding_level *b = innermost_nonclass_level ();

  return b->pseudo_global;
}

void
set_class_shadows (shadows)
     tree shadows;
{
  class_binding_level->class_shadowed = shadows;
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  register struct binding_level *newlevel = NULL_BINDING_LEVEL;

  /* If this is the top level of a function,
     just make sure that NAMED_LABELS is 0.
     They should have been set to 0 at the end of the previous function.  */

  if (current_binding_level == global_binding_level)
    my_friendly_assert (named_labels == NULL_TREE, 134);

  /* Reuse or create a struct for this binding level.  */

#if defined(DEBUG_CP_BINDING_LEVELS)
  if (0)
#else /* !defined(DEBUG_CP_BINDING_LEVELS) */
  if (free_binding_level)
#endif /* !defined(DEBUG_CP_BINDING_LEVELS) */
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }

  push_binding_level (newlevel, tag_transparent, keep_next_level_flag);
  GNU_xref_start_scope ((HOST_WIDE_INT) newlevel);
  keep_next_level_flag = 0;
}

void
note_level_for_for ()
{
  current_binding_level->is_for_scope = 1;
}

void
pushlevel_temporary (tag_transparent)
     int tag_transparent;
{
  pushlevel (tag_transparent);
  current_binding_level->keep = 2;
  clear_last_expr ();

  /* Note we don't call push_momentary() here.  Otherwise, it would cause
     cleanups to be allocated on the momentary obstack, and they will be
     overwritten by the next statement.  */

  expand_start_bindings (0);
}

/* For a binding between a name and an entity at a block scope,
   this is the `struct binding_level' for the block.  */
#define BINDING_LEVEL(NODE) \
   (((struct tree_binding*)NODE)->scope.level)

/* These are currently unused, but permanent, CPLUS_BINDING nodes.
   They are kept here because they are allocated from the permanent
   obstack and cannot be easily freed.  */
static tree free_binding_nodes;

/* Make DECL the innermost binding for ID.  The LEVEL is the binding
   level at which this declaration is being bound.  */

static void
push_binding (id, decl, level)
     tree id;
     tree decl;
     struct binding_level* level;
{
  tree binding;

  if (!free_binding_nodes)
    {
      /* There are no free nodes, so we must build one here.  */
      push_obstacks_nochange ();
      end_temporary_allocation ();
      binding = make_node (CPLUS_BINDING);
      pop_obstacks ();
    }
  else
    {
      /* There are nodes on the free list.  Grab the first one.  */
      binding = free_binding_nodes;
      
      /* And update the free list.  */
      free_binding_nodes = TREE_CHAIN (free_binding_nodes);
    }

  /* Now, fill in the binding information.  */
  BINDING_VALUE (binding) = decl;
  BINDING_TYPE (binding) = NULL_TREE;
  BINDING_LEVEL (binding) = level;
  INHERITED_VALUE_BINDING_P (binding) = 0;
  LOCAL_BINDING_P (binding) = (level != class_binding_level);

  /* And put it on the front of the list of bindings for ID.  */
  TREE_CHAIN (binding) = IDENTIFIER_BINDING (id);
  IDENTIFIER_BINDING (id) = binding;
}

/* ID is already bound in the current scope.  But, DECL is an
   additional binding for ID in the same scope.  This is the `struct
   stat' hack whereby a non-typedef class-name or enum-name can be
   bound at the same level as some other kind of entity.  It's the
   responsibility of the caller to check that inserting this name is
   legal here.  Returns nonzero if the new binding was successful.  */
static int
add_binding (id, decl)
     tree id;
     tree decl;
{
  tree binding = IDENTIFIER_BINDING (id);
  int ok = 1;

  if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl))
    /* The new name is the type name.  */
    BINDING_TYPE (binding) = decl;
  else if (!BINDING_VALUE (binding))
    /* This situation arises when push_class_level_binding moves an
       inherited type-binding out of the way to make room for a new
       value binding.  */
    BINDING_VALUE (binding) = decl;
  else if (TREE_CODE (BINDING_VALUE (binding)) == TYPE_DECL
	   && DECL_ARTIFICIAL (BINDING_VALUE (binding)))
    {
      /* The old binding was a type name.  It was placed in
	 BINDING_VALUE because it was thought, at the point it was
	 declared, to be the only entity with such a name.  Move the
	 type name into the type slot; it is now hidden by the new
	 binding.  */
      BINDING_TYPE (binding) = BINDING_VALUE (binding);
      BINDING_VALUE (binding) = decl;
      INHERITED_VALUE_BINDING_P (binding) = 0;
    }
  else
    {
      cp_error ("declaration of `%#D'", decl);
      cp_error_at ("conflicts with previous declaration `%#D'",
		   BINDING_VALUE (binding));
      ok = 0;
    }

  return ok;
}

/* Bind DECL to ID in the current_binding_level.
   If PUSH_USING is set in FLAGS, we know that DECL doesn't really belong
   to this binding level, that it got here through a using-declaration.  */

void
push_local_binding (id, decl, flags)
     tree id;
     tree decl;
     int flags;
{
  struct binding_level *b;

  /* Skip over any local classes.  This makes sense if we call
     push_local_binding with a friend decl of a local class.  */
  b = current_binding_level;
  while (b->parm_flag == 2)
    b = b->level_chain;

  if (lookup_name_current_level (id))
    {
      /* Supplement the existing binding.  */
      if (!add_binding (id, decl))
	/* It didn't work.  Something else must be bound at this
	   level.  Do not add DECL to the list of things to pop
	   later.  */
	return;
    }
  else
    /* Create a new binding.  */
    push_binding (id, decl, b);

  if (TREE_CODE (decl) == OVERLOAD || (flags & PUSH_USING))
    /* We must put the OVERLOAD into a TREE_LIST since the
       TREE_CHAIN of an OVERLOAD is already used.  Similarly for
       decls that got here through a using-declaration.  */
    decl = build_tree_list (NULL_TREE, decl);

  /* And put DECL on the list of things declared by the current
     binding level.  */
  TREE_CHAIN (decl) = b->names;
  b->names = decl;
}

/* Bind DECL to ID in the class_binding_level.  Returns nonzero if the
   binding was successful.  */

int
push_class_binding (id, decl)
     tree id;
     tree decl;
{
  int result = 1;
  tree binding = IDENTIFIER_BINDING (id);
  tree context;

  /* Note that we declared this value so that we can issue an error if
     this an illegal redeclaration of a name already used for some
     other purpose.  */
  note_name_declared_in_class (id, decl);

  if (binding && BINDING_LEVEL (binding) == class_binding_level)
    /* Supplement the existing binding.  */
    result = add_binding (id, decl);
  else
    /* Create a new binding.  */
    push_binding (id, decl, class_binding_level);

  /* Update the IDENTIFIER_CLASS_VALUE for this ID to be the
     class-level declaration.  Note that we do not use DECL here
     because of the possibility of the `struct stat' hack; if DECL is
     a class-name or enum-name we might prefer a field-name, or some
     such.  */
  IDENTIFIER_CLASS_VALUE (id) = BINDING_VALUE (IDENTIFIER_BINDING (id));

  /* If this is a binding from a base class, mark it as such.  */
  binding = IDENTIFIER_BINDING (id);
  if (BINDING_VALUE (binding) == decl && TREE_CODE (decl) != TREE_LIST)
    {
      /* Any implicit typename must be from a base-class.  The
	 context for an implicit typename declaration is always
	 the derived class in which the lookup was done, so the checks
	 based on the context of DECL below will not trigger.  */
      if (TREE_CODE (decl) == TYPE_DECL 
	  && IMPLICIT_TYPENAME_P (TREE_TYPE (decl)))
	INHERITED_VALUE_BINDING_P (binding) = 1;
      else
	{
	  if (TREE_CODE (decl) == OVERLOAD)
	    context = DECL_REAL_CONTEXT (OVL_CURRENT (decl));
	  else
	    {
	      my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (decl)) == 'd',
				  0);
	      context = DECL_REAL_CONTEXT (decl);
	    }

	  if (is_properly_derived_from (current_class_type, context))
	    INHERITED_VALUE_BINDING_P (binding) = 1;
	  else
	    INHERITED_VALUE_BINDING_P (binding) = 0;
	}
    }
  else if (BINDING_VALUE (binding) == decl)
    /* We only encounter a TREE_LIST when push_class_decls detects an
       ambiguity.  Such an ambiguity can be overridden by a definition
       in this class.  */
    INHERITED_VALUE_BINDING_P (binding) = 1;

  return result;
}

/* Remove the binding for DECL which should be the innermost binding
   for ID.  */

static void 
pop_binding (id, decl) 
     tree id;
     tree decl;
{
  tree binding;
    
  if (id == NULL_TREE)
    /* It's easiest to write the loops that call this function without
       checking whether or not the entities involved have names.  We
       get here for such an entity.  */
    return;

  /* Get the innermost binding for ID.  */
  binding = IDENTIFIER_BINDING (id);

  /* The name should be bound.  */
  my_friendly_assert (binding != NULL_TREE, 0);

  /* The DECL will be either the ordinary binding or the type
     binding for this identifier.  Remove that binding.  */
  if (BINDING_VALUE (binding) == decl)
    BINDING_VALUE (binding) = NULL_TREE;
  else if (BINDING_TYPE (binding) == decl)
    BINDING_TYPE (binding) = NULL_TREE;
  else
    my_friendly_abort (0);

  if (!BINDING_VALUE (binding) && !BINDING_TYPE (binding))
    {
      /* We're completely done with the innermost binding for this
	 identifier.  Unhook it from the list of bindings.  */
      IDENTIFIER_BINDING (id) = TREE_CHAIN (binding);

      /* And place it on the free list.  */
      TREE_CHAIN (binding) = free_binding_nodes;
      free_binding_nodes = binding;
    }
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP == 1, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  register tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  int tmp = functionbody;
  int real_functionbody = current_binding_level->keep == 2
    ? ((functionbody = 0), tmp) : functionbody;
  tree tags = functionbody >= 0 ? current_binding_level->tags : 0;
  tree subblocks = functionbody >= 0 ? current_binding_level->blocks : 0;
  tree block = NULL_TREE;
  tree decl;
  int block_previously_created;
  int leaving_for_scope;

  if (current_binding_level->parm_flag == 2)
    return poplevel_class ();

  my_friendly_assert (!current_binding_level->class_shadowed,
		      19990414);

  /* We used to use KEEP == 2 to indicate that the new block should go
     at the beginning of the list of blocks at this binding level,
     rather than the end.  This hack is no longer used.  */
  my_friendly_assert (keep == 0 || keep == 1, 0);

  GNU_xref_end_scope ((HOST_WIDE_INT) current_binding_level,
		      (HOST_WIDE_INT) current_binding_level->level_chain,
		      current_binding_level->parm_flag,
		      current_binding_level->keep);

  if (current_binding_level->keep == 1)
    keep = 1;

  /* Get the decls in the order they were written.
     Usually current_binding_level->names is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_binding_level->names
      = decls = nreverse (current_binding_level->names);
  else
    decls = current_binding_level->names;

  /* Output any nested inline functions within this block
     if they weren't already output.  */

  for (decl = decls; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	&& ! TREE_ASM_WRITTEN (decl)
	&& DECL_INITIAL (decl) != NULL_TREE
	&& TREE_ADDRESSABLE (decl)
	&& decl_function_context (decl) == current_function_decl)
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != NULL_TREE)
	  TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
	else
	  {
	    push_function_context ();
	    output_inline_function (decl);
	    pop_function_context ();
	  }
      }

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a BLOCK to record them for the life of this function.  */

  block = NULL_TREE;
  block_previously_created = (current_binding_level->this_block != NULL_TREE);
  if (block_previously_created)
    block = current_binding_level->this_block;
  else if (keep == 1 || functionbody)
    block = make_node (BLOCK);
  if (block != NULL_TREE)
    {
      if (block_previously_created)
	{
	  if (decls || tags || subblocks)
	    {
	      if (BLOCK_VARS (block) || BLOCK_TYPE_TAGS (block))
		warning ("internal compiler error: debugging info corrupted");

	      BLOCK_VARS (block) = decls;
	      BLOCK_TYPE_TAGS (block) = tags;

	      /* We can have previous subblocks and new subblocks when
		 doing fixup_gotos with complex cleanups.  We chain the new
		 subblocks onto the end of any pre-existing subblocks.  */
	      BLOCK_SUBBLOCKS (block) = chainon (BLOCK_SUBBLOCKS (block),
						 subblocks);
	    }
	  /* If we created the block earlier on, and we are just
	     diddling it now, then it already should have a proper
	     BLOCK_END_NOTE value associated with it.  */
	}
      else
	{
	  BLOCK_VARS (block) = decls;
	  BLOCK_TYPE_TAGS (block) = tags;
	  BLOCK_SUBBLOCKS (block) = subblocks;
	  /* Otherwise, for a new block, install a new BLOCK_END_NOTE
	     value.  */ 
	  remember_end_note (block);
	}
    }

  /* In each subblock, record that this is its superior.  */

  if (keep >= 0)
    for (link = subblocks; link; link = TREE_CHAIN (link))
      BLOCK_SUPERCONTEXT (link) = block;

  /* We still support the old for-scope rules, whereby the variables
     in a for-init statement were in scope after the for-statement
     ended.  We only use the new rules in flag_new_for_scope is
     nonzero.  */
  leaving_for_scope 
    = current_binding_level->is_for_scope && flag_new_for_scope == 1;

  /* Remove declarations for all the DECLs in this level.  */
  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (leaving_for_scope && TREE_CODE (link) == VAR_DECL)
	{
	  tree outer_binding 
	    = TREE_CHAIN (IDENTIFIER_BINDING (DECL_NAME (link)));
	  tree ns_binding;

	  if (!outer_binding)
	    ns_binding = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (link));
	  else
	    ns_binding = NULL_TREE;

	  if (outer_binding 
	      && (BINDING_LEVEL (outer_binding) 
		  == current_binding_level->level_chain))
	    /* We have something like:
	       
	         int i;
	         for (int i; ;);
		 
	       and we are leaving the `for' scope.  There's no reason to
	       keep the binding of the inner `i' in this case.  */
	    pop_binding (DECL_NAME (link), link);
	  else if ((outer_binding 
		    && (TREE_CODE (BINDING_VALUE (outer_binding)) 
			== TYPE_DECL))
		   || (ns_binding 
		       && TREE_CODE (ns_binding) == TYPE_DECL))
	    /* Here, we have something like:

		 typedef int I;

		 void f () {
		   for (int I; ;);
		 }

	       We must pop the for-scope binding so we know what's a
	       type and what isn't.  */
	    pop_binding (DECL_NAME (link), link);
	  else
	    {
	      /* Mark this VAR_DECL as dead so that we can tell we left it
		 there only for backward compatibility.  */
	      DECL_DEAD_FOR_LOCAL (link) = 1;
	      
	      /* Keep track of what should of have happenned when we
		 popped the binding.  */
	      if (outer_binding && BINDING_VALUE (outer_binding))
		DECL_SHADOWED_FOR_VAR (link) 
		  = BINDING_VALUE (outer_binding);

	      /* Add it to the list of dead variables in the next
		 outermost binding to that we can remove these when we
		 leave that binding.  */
	      current_binding_level->level_chain->dead_vars_from_for
		= tree_cons (NULL_TREE, link,
			     current_binding_level->level_chain->
			     dead_vars_from_for);

	      /* Although we don't pop the CPLUS_BINDING, we do clear
		 its BINDING_LEVEL since the level is going away now.  */
	      BINDING_LEVEL (IDENTIFIER_BINDING (DECL_NAME (link)))
		= 0;
	    }
	}
      else 
	{
	  /* Remove the binding.  */
	  decl = link;
	  if (TREE_CODE (decl) == TREE_LIST)
	    decl = TREE_VALUE (decl);
	  if (TREE_CODE_CLASS (TREE_CODE (decl)) == 'd')
	    pop_binding (DECL_NAME (decl), decl);
	  else if (TREE_CODE (decl) == OVERLOAD)
	    pop_binding (DECL_NAME (OVL_FUNCTION (decl)), decl);
	  else 
	    my_friendly_abort (0);
	}
    }

  /* Remove declarations for any `for' variables from inner scopes
     that we kept around.  */
  for (link = current_binding_level->dead_vars_from_for;
       link; link = TREE_CHAIN (link))
    pop_binding (DECL_NAME (TREE_VALUE (link)), TREE_VALUE (link));

  /* Restore the IDENTIFIER_TYPE_VALUEs.  */
  for (link = current_binding_level->type_shadowed;
       link; link = TREE_CHAIN (link))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (link), TREE_VALUE (link));
  
  /* There may be OVERLOADs (wrapped in TREE_LISTs) on the BLOCK_VARs
     list if a `using' declaration put them there.  The debugging
     back-ends won't understand OVERLOAD, so we remove them here.
     Because the BLOCK_VARS are (temporarily) shared with
     CURRENT_BINDING_LEVEL->NAMES we must do this fixup after we have
     popped all the bindings.  */
  if (block)
    {
      tree* d;

      for (d = &BLOCK_VARS (block); *d; )
	{
	  if (TREE_CODE (*d) == TREE_LIST)
	    *d = TREE_CHAIN (*d);
	  else
	    d = &TREE_CHAIN (*d);
	}
    }

  /* If the level being exited is the top level of a function,
     check over all the labels.  */

  if (functionbody)
    {
      /* If this is the top level block of a function,
         the vars are the function's parameters.
         Don't leave them in the BLOCK because they are
         found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;

      /* Clear out the definitions of all label names,
	 since their scopes end here.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  register tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == NULL_TREE)
	    {
	      cp_error_at ("label `%D' used but not defined", label);
	      /* Avoid crashing later.  */
	      define_label (input_filename, 1, DECL_NAME (label));
	    }
	  else if (warn_unused && !TREE_USED (label))
	    cp_warning_at ("label `%D' defined but not used", label);
	  SET_IDENTIFIER_LABEL_VALUE (DECL_NAME (label), NULL_TREE);

          /* Put the labels into the "variables" of the
             top-level block, so debugger can see them.  */
          TREE_CHAIN (label) = BLOCK_VARS (block);
          BLOCK_VARS (block) = label;
	}

      named_labels = NULL_TREE;
    }

  /* Any uses of undefined labels now operate under constraints
     of next binding contour.  */
  {
    struct binding_level *level_chain;
    level_chain = current_binding_level->level_chain;
    if (level_chain)
      {
	struct named_label_list *labels;
	for (labels = named_label_uses; labels; labels = labels->next)
	  if (labels->binding_level == current_binding_level)
	    {
	      labels->binding_level = level_chain;
	      labels->names_in_scope = level_chain->names;
	    }
      }
  }

  tmp = current_binding_level->keep;

  pop_binding_level ();
  if (functionbody)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    {
      if (!block_previously_created)
        current_binding_level->blocks
          = chainon (current_binding_level->blocks, block);
    }
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblocks);

  /* Take care of compiler's internal binding structures.  */
  if (tmp == 2)
    {
      expand_end_bindings (getdecls (), keep, 1);
      /* Each and every BLOCK node created here in `poplevel' is important
	 (e.g. for proper debugging information) so if we created one
	 earlier, mark it as "used".  */
      if (block)
	TREE_USED (block) = 1;
      block = poplevel (keep, reverse, real_functionbody);
    }

  /* Each and every BLOCK node created here in `poplevel' is important
     (e.g. for proper debugging information) so if we created one
     earlier, mark it as "used".  */
  if (block)
    TREE_USED (block) = 1;
  return block;
}

/* Delete the node BLOCK from the current binding level.
   This is used for the block inside a stmt expr ({...})
   so that the block can be reinserted where appropriate.  */

void
delete_block (block)
     tree block;
{
  tree t;
  if (current_binding_level->blocks == block)
    current_binding_level->blocks = TREE_CHAIN (block);
  for (t = current_binding_level->blocks; t;)
    {
      if (TREE_CHAIN (t) == block)
	TREE_CHAIN (t) = TREE_CHAIN (block);
      else
	t = TREE_CHAIN (t);
    }
  TREE_CHAIN (block) = NULL_TREE;
  /* Clear TREE_USED which is always set by poplevel.
     The flag is set again if insert_block is called.  */
  TREE_USED (block) = 0;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
    register tree block;
{
  current_binding_level->this_block = block;
}

/* Do a pushlevel for class declarations.  */

void
pushlevel_class ()
{
  register struct binding_level *newlevel;

  /* Reuse or create a struct for this binding level.  */
#if defined(DEBUG_CP_BINDING_LEVELS)
  if (0)
#else /* !defined(DEBUG_CP_BINDING_LEVELS) */
  if (free_binding_level)
#endif /* !defined(DEBUG_CP_BINDING_LEVELS) */
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    newlevel = make_binding_level ();

#if defined(DEBUG_CP_BINDING_LEVELS)
  is_class_level = 1;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */

  push_binding_level (newlevel, 0, 0);

  decl_stack = push_decl_level (decl_stack, &decl_obstack);
  class_binding_level = current_binding_level;
  class_binding_level->parm_flag = 2;
}

/* ...and a poplevel for class declarations.  */

static tree
poplevel_class ()
{
  register struct binding_level *level = class_binding_level;
  tree shadowed;

  my_friendly_assert (level != 0, 354);
  
  decl_stack = pop_stack_level (decl_stack);
  /* If we're leaving a toplevel class, don't bother to do the setting
     of IDENTIFIER_CLASS_VALUE to NULL_TREE, since first of all this slot
     shouldn't even be used when current_class_type isn't set, and second,
     if we don't touch it here, we're able to use the cache effect if the
     next time we're entering a class scope, it is the same class.  */
  if (current_class_depth != 1)
    {
      struct binding_level* b;

      /* Clear out our IDENTIFIER_CLASS_VALUEs.  */
      for (shadowed = level->class_shadowed;
	   shadowed;
	   shadowed = TREE_CHAIN (shadowed))
	IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (shadowed)) = NULL_TREE;
	
      /* Find the next enclosing class, and recreate
	 IDENTIFIER_CLASS_VALUEs appropriate for that class.  */
      b = level->level_chain;
      while (b && b->parm_flag != 2)
	b = b->level_chain;

      if (b)
	for (shadowed = b->class_shadowed; 
	     shadowed; 
	     shadowed = TREE_CHAIN (shadowed))
	  {
	    tree t;

	    t = IDENTIFIER_BINDING (TREE_PURPOSE (shadowed));
	    while (t && BINDING_LEVEL (t) != b)
	      t = TREE_CHAIN (t);
      
	    if (t)
	      IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (shadowed)) 
		= BINDING_VALUE (t);
	  }
    }
  else
    /* Remember to save what IDENTIFIER's were bound in this scope so we
       can recover from cache misses.  */
    {
      previous_class_type = current_class_type;
      previous_class_values = class_binding_level->class_shadowed;
    }
  for (shadowed = level->type_shadowed;
       shadowed;
       shadowed = TREE_CHAIN (shadowed))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (shadowed), TREE_VALUE (shadowed));

  /* Remove the bindings for all of the class-level declarations.  */
  for (shadowed = level->class_shadowed; 
       shadowed; 
       shadowed = TREE_CHAIN (shadowed))
    pop_binding (TREE_PURPOSE (shadowed), TREE_TYPE (shadowed));

  GNU_xref_end_scope ((HOST_WIDE_INT) class_binding_level,
		      (HOST_WIDE_INT) class_binding_level->level_chain,
		      class_binding_level->parm_flag,
		      class_binding_level->keep);

  /* Now, pop out of the binding level which we created up in the
     `pushlevel_class' routine.  */
#if defined(DEBUG_CP_BINDING_LEVELS)
  is_class_level = 1;
#endif /* defined(DEBUG_CP_BINDING_LEVELS) */

  pop_binding_level ();

  return NULL_TREE;
}

/* We are entering the scope of a class.  Clear IDENTIFIER_CLASS_VALUE
   for any names in enclosing classes.  */

void
clear_identifier_class_values ()
{
  tree t;

  if (!class_binding_level)
    return;

  for (t = class_binding_level->class_shadowed;
       t;
       t = TREE_CHAIN (t))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (t)) = NULL_TREE;
}

/* Returns non-zero if T is a virtual function table.  */

int
vtable_decl_p (t, data)
     tree t;
     void *data ATTRIBUTE_UNUSED;
{
  return (TREE_CODE (t) == VAR_DECL && DECL_VIRTUAL_P (t));
}

/* Returns non-zero if T is a TYPE_DECL for a type with virtual
   functions.  */

int
vtype_decl_p (t, data)
     tree t;
     void *data ATTRIBUTE_UNUSED;
{
  return (TREE_CODE (t) == TYPE_DECL
	  && TREE_TYPE (t) != error_mark_node
	  && TYPE_LANG_SPECIFIC (TREE_TYPE (t))
	  && CLASSTYPE_VSIZE (TREE_TYPE (t)));
}

/* Returns non-zero if T is a signature table.  */

int 
sigtable_decl_p (t, data)
     tree t;
     void *data ATTRIBUTE_UNUSED;
{
  return (TREE_CODE (t) == VAR_DECL
	  && TREE_TYPE (t) != error_mark_node
	  && IS_SIGNATURE (TREE_TYPE (t)));
}

/* Walk all the namespaces contained NAMESPACE, including NAMESPACE
   itself, calling F for each.  The DATA is passed to F as well.  */

static int
walk_namespaces_r (namespace, f, data)
     tree namespace;
     walk_namespaces_fn f;
     void *data;
{
  tree current;
  int result = 0;

  result |= (*f) (namespace, data);

  for (current = NAMESPACE_LEVEL (namespace)->names;
       current;
       current = TREE_CHAIN (current))
    {
      if (TREE_CODE (current) != NAMESPACE_DECL
	  || DECL_NAMESPACE_ALIAS (current))
	continue;
      if (!DECL_LANG_SPECIFIC (current))
	{
	  /* Hmm. std. */
	  my_friendly_assert (current == std_node, 393);
	  continue;
	}

      /* We found a namespace.  */
      result |= walk_namespaces_r (current, f, data);
    }

  return result;
}

/* Walk all the namespaces, calling F for each.  The DATA is passed to
   F as well.  */

int
walk_namespaces (f, data)
     walk_namespaces_fn f;
     void *data;
{
  return walk_namespaces_r (global_namespace, f, data);
}

struct walk_globals_data {
  walk_globals_pred p;
  walk_globals_fn f;
  void *data;
};

/* Walk the global declarations in NAMESPACE.  Whenever one is found
   for which P returns non-zero, call F with its address.  If any call
   to F returns a non-zero value, return a non-zero value.  */

static int 
walk_globals_r (namespace, data)
     tree namespace;
     void *data;
{
  struct walk_globals_data* wgd = (struct walk_globals_data *) data;
  walk_globals_pred p = wgd->p;
  walk_globals_fn f = wgd->f;
  void *d = wgd->data;
  tree *t;
  int result = 0;

  t = &NAMESPACE_LEVEL (namespace)->names;

  while (*t)
    {
      tree glbl = *t;

      if ((*p) (glbl, d))
	result |= (*f) (t, d);

      /* If F changed *T, then *T still points at the next item to
	 examine.  */
      if (*t == glbl)
	t = &TREE_CHAIN (*t);
    }

  return result;
}

/* Walk the global declarations.  Whenever one is found for which P
   returns non-zero, call F with its address.  If any call to F
   returns a non-zero value, return a non-zero value.  */

int
walk_globals (p, f, data)
     walk_globals_pred p;
     walk_globals_fn f;
     void *data;
{
  struct walk_globals_data wgd;
  wgd.p = p;
  wgd.f = f;
  wgd.data = data;

  return walk_namespaces (walk_globals_r, &wgd);
}

/* Call wrapup_globals_declarations for the globals in NAMESPACE.  If
   DATA is non-NULL, this is the last time we will call
   wrapup_global_declarations for this NAMESPACE.  */

int
wrapup_globals_for_namespace (namespace, data)
     tree namespace;
     void *data;
{
  tree globals = NAMESPACE_LEVEL (namespace)->names;
  int len = list_length (globals);
  tree *vec = (tree *) alloca (sizeof (tree) * len);
  int i;
  int result;
  tree decl;
  int last_time = (data != 0);

  if (last_time && namespace == global_namespace)
    /* Let compile_file handle the global namespace.  */
    return 0;

  /* Process the decls in reverse order--earliest first.
     Put them into VEC from back to front, then take out from front.  */
  
  for (i = 0, decl = globals; i < len; i++, decl = TREE_CHAIN (decl))
    vec[len - i - 1] = decl;
  
  if (last_time)
    {
      check_global_declarations (vec, len);
      return 0;
    }

  /* Temporarily mark vtables as external.  That prevents
     wrapup_global_declarations from writing them out; we must process
     them ourselves in finish_vtable_vardecl.  */
  for (i = 0; i < len; ++i)
    if (vtable_decl_p (vec[i], /*data=*/0) && !DECL_EXTERNAL (vec[i]))
      {
	DECL_NOT_REALLY_EXTERN (vec[i]) = 1;
	DECL_EXTERNAL (vec[i]) = 1;
      }

  /* Write out any globals that need to be output.  */
  result = wrapup_global_declarations (vec, len);

  /* Undo the hack to DECL_EXTERNAL above.  */
  for (i = 0; i < len; ++i)
    if (vtable_decl_p (vec[i], /*data=*/0)
	&& DECL_NOT_REALLY_EXTERN (vec[i]))
      {
	DECL_NOT_REALLY_EXTERN (vec[i]) = 0;
	DECL_EXTERNAL (vec[i]) = 0;
      }

  return result;
}


/* For debugging.  */
static int no_print_functions = 0;
static int no_print_builtins = 0;

void
print_binding_level (lvl)
     struct binding_level *lvl;
{
  tree t;
  int i = 0, len;
  fprintf (stderr, " blocks=");
  fprintf (stderr, HOST_PTR_PRINTF, lvl->blocks);
  fprintf (stderr, " n_incomplete=%d parm_flag=%d keep=%d",
	   list_length (lvl->incomplete), lvl->parm_flag, lvl->keep);
  if (lvl->tag_transparent)
    fprintf (stderr, " tag-transparent");
  if (lvl->more_cleanups_ok)
    fprintf (stderr, " more-cleanups-ok");
  if (lvl->have_cleanups)
    fprintf (stderr, " have-cleanups");
  fprintf (stderr, "\n");
  if (lvl->names)
    {
      fprintf (stderr, " names:\t");
      /* We can probably fit 3 names to a line?  */
      for (t = lvl->names; t; t = TREE_CHAIN (t))
	{
	  if (no_print_functions && (TREE_CODE (t) == FUNCTION_DECL)) 
	    continue;
	  if (no_print_builtins
	      && (TREE_CODE (t) == TYPE_DECL)
	      && (!strcmp (DECL_SOURCE_FILE (t),"<built-in>")))
	    continue;

	  /* Function decls tend to have longer names.  */
	  if (TREE_CODE (t) == FUNCTION_DECL)
	    len = 3;
	  else
	    len = 2;
	  i += len;
	  if (i > 6)
	    {
	      fprintf (stderr, "\n\t");
	      i = len;
	    }
	  print_node_brief (stderr, "", t, 0);
	  if (t == error_mark_node)
	    break;
	}
      if (i)
        fprintf (stderr, "\n");
    }
  if (lvl->tags)
    {
      fprintf (stderr, " tags:\t");
      i = 0;
      for (t = lvl->tags; t; t = TREE_CHAIN (t))
	{
	  if (TREE_PURPOSE (t) == NULL_TREE)
	    len = 3;
	  else if (TREE_PURPOSE (t) == TYPE_IDENTIFIER (TREE_VALUE (t)))
	    len = 2;
	  else
	    len = 4;
	  i += len;
	  if (i > 5)
	    {
	      fprintf (stderr, "\n\t");
	      i = len;
	    }
	  if (TREE_PURPOSE (t) == NULL_TREE)
	    {
	      print_node_brief (stderr, "<unnamed-typedef", TREE_VALUE (t), 0);
	      fprintf (stderr, ">");
	    }
	  else if (TREE_PURPOSE (t) == TYPE_IDENTIFIER (TREE_VALUE (t)))
	    print_node_brief (stderr, "", TREE_VALUE (t), 0);
	  else
	    {
	      print_node_brief (stderr, "<typedef", TREE_PURPOSE (t), 0);
	      print_node_brief (stderr, "", TREE_VALUE (t), 0);
	      fprintf (stderr, ">");
	    }
	}
      if (i)
	fprintf (stderr, "\n");
    }
  if (lvl->class_shadowed)
    {
      fprintf (stderr, " class-shadowed:");
      for (t = lvl->class_shadowed; t; t = TREE_CHAIN (t))
	{
	  fprintf (stderr, " %s ", IDENTIFIER_POINTER (TREE_PURPOSE (t)));
	}
      fprintf (stderr, "\n");
    }
  if (lvl->type_shadowed)
    {
      fprintf (stderr, " type-shadowed:");
      for (t = lvl->type_shadowed; t; t = TREE_CHAIN (t))
        {
	  fprintf (stderr, " %s ", IDENTIFIER_POINTER (TREE_PURPOSE (t)));
        }
      fprintf (stderr, "\n");
    }
}

void
print_other_binding_stack (stack)
     struct binding_level *stack;
{
  struct binding_level *level;
  for (level = stack; level != global_binding_level; level = level->level_chain)
    {
      fprintf (stderr, "binding level ");
      fprintf (stderr, HOST_PTR_PRINTF, level);
      fprintf (stderr, "\n");
      print_binding_level (level);
    }
}

void
print_binding_stack ()
{
  struct binding_level *b;
  fprintf (stderr, "current_binding_level=");
  fprintf (stderr, HOST_PTR_PRINTF, current_binding_level);
  fprintf (stderr, "\nclass_binding_level=");
  fprintf (stderr, HOST_PTR_PRINTF, class_binding_level);
  fprintf (stderr, "\nglobal_binding_level=");
  fprintf (stderr, HOST_PTR_PRINTF, global_binding_level);
  fprintf (stderr, "\n");
  if (class_binding_level)
    {
      for (b = class_binding_level; b; b = b->level_chain)
	if (b == current_binding_level)
	  break;
      if (b)
	b = class_binding_level;
      else
	b = current_binding_level;
    }
  else
    b = current_binding_level;
  print_other_binding_stack (b);
  fprintf (stderr, "global:\n");
  print_binding_level (global_binding_level);
}

/* Namespace binding access routines: The namespace_bindings field of
   the identifier is polymorphic, with three possible values:
   NULL_TREE, a list of CPLUS_BINDINGS, or any other tree_node
   indicating the BINDING_VALUE of global_namespace. */

/* Check whether the a binding for the name to scope is known.
   Assumes that the bindings of the name are already a list
   of bindings. Returns the binding found, or NULL_TREE. */

static tree
find_binding (name, scope)
     tree name;
     tree scope;
{
  tree iter, prev = NULL_TREE;

  scope = ORIGINAL_NAMESPACE (scope);
  
  for (iter = IDENTIFIER_NAMESPACE_BINDINGS (name); iter;
       iter = TREE_CHAIN (iter))
    {
      my_friendly_assert (TREE_CODE (iter) == CPLUS_BINDING, 374);
      if (BINDING_SCOPE (iter) == scope)
	{
	  /* Move binding found to the fron of the list, so
             subsequent lookups will find it faster. */
	  if (prev)
	    {
	      TREE_CHAIN (prev) = TREE_CHAIN (iter);
	      TREE_CHAIN (iter) = IDENTIFIER_NAMESPACE_BINDINGS (name);
	      IDENTIFIER_NAMESPACE_BINDINGS (name) = iter;
	    }
	  return iter;
	}
      prev = iter;
    }
  return NULL_TREE;
}

/* Always returns a binding for name in scope. If the
   namespace_bindings is not a list, convert it to one first.
   If no binding is found, make a new one. */

tree
binding_for_name (name, scope)
     tree name;
     tree scope;
{
  tree b = IDENTIFIER_NAMESPACE_BINDINGS (name);
  tree result;

  scope = ORIGINAL_NAMESPACE (scope);
  
  if (b && TREE_CODE (b) != CPLUS_BINDING)
    {
      /* Get rid of optimization for global scope. */
      IDENTIFIER_NAMESPACE_BINDINGS (name) = NULL_TREE;
      BINDING_VALUE (binding_for_name (name, global_namespace)) = b;
      b = IDENTIFIER_NAMESPACE_BINDINGS (name);
    }
  if (b && (result = find_binding (name, scope)))
    return result;
  /* Not found, make a new permanent one. */
  push_obstacks (&permanent_obstack, &permanent_obstack);
  result = make_node (CPLUS_BINDING);
  TREE_CHAIN (result) = b;
  IDENTIFIER_NAMESPACE_BINDINGS (name) = result;
  BINDING_SCOPE (result) = scope;
  BINDING_TYPE (result) = NULL_TREE;
  BINDING_VALUE (result) = NULL_TREE;
  pop_obstacks ();
  return result;
}

/* Return the binding value for name in scope, considering that
   namespace_binding may or may not be a list of CPLUS_BINDINGS. */

tree
namespace_binding (name, scope)
     tree name;
     tree scope;
{
  tree b = IDENTIFIER_NAMESPACE_BINDINGS (name);
  if (b == NULL_TREE)
    return NULL_TREE;
  if (scope == NULL_TREE)
    scope = global_namespace;
  if (TREE_CODE (b) != CPLUS_BINDING)
    return (scope == global_namespace) ? b : NULL_TREE;
  name = find_binding (name,scope);
  if (name == NULL_TREE)
    return name;
  return BINDING_VALUE (name);
}

/* Set the binding value for name in scope. If modifying the binding
   of global_namespace is attempted, try to optimize it. */

void
set_namespace_binding (name, scope, val)
     tree name;
     tree scope;
     tree val;
{
  tree b;

  if (scope == NULL_TREE)
    scope = global_namespace;
  
  if (scope == global_namespace)
    {
      b = IDENTIFIER_NAMESPACE_BINDINGS (name);
      if (b == NULL_TREE || TREE_CODE (b) != CPLUS_BINDING)
	{
	  IDENTIFIER_NAMESPACE_BINDINGS (name) = val;
	  return;
	}
    }
  b = binding_for_name (name, scope);
  BINDING_VALUE (b) = val;
}

/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE, then we
   select a name that is unique to this compilation unit.  */

void
push_namespace (name)
     tree name;
{
  tree d = NULL_TREE;
  int need_new = 1;
  int implicit_use = 0;
  int global = 0;
  if (!global_namespace)
    {
      /* This must be ::. */
      my_friendly_assert (name == get_identifier ("::"), 377);
      global = 1;
    }
  else if (!name)
    {
      /* The name of anonymous namespace is unique for the translation
         unit.  */
      if (!anonymous_namespace_name)
        anonymous_namespace_name = get_file_function_name ('N');
      name = anonymous_namespace_name;
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d)
        /* Reopening anonymous namespace.  */
        need_new = 0;
      implicit_use = 1;
    }
  else if (current_namespace == global_namespace
	   && name == DECL_NAME (std_node))
    {
      in_std++;
      return;
    }
  else
    {
      /* Check whether this is an extended namespace definition. */
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d != NULL_TREE && TREE_CODE (d) == NAMESPACE_DECL)
        {
          need_new = 0;
          if (DECL_NAMESPACE_ALIAS (d))
            {
              cp_error ("namespace alias `%D' not allowed here, assuming `%D'",
                        d, DECL_NAMESPACE_ALIAS (d));
              d = DECL_NAMESPACE_ALIAS (d);
            }
        }
    }
  
  if (need_new)
    {
      /* Make a new namespace, binding the name to it. */
      d = build_lang_decl (NAMESPACE_DECL, name, void_type_node);
      /* The global namespace is not pushed, and the global binding
	 level is set elsewhere.  */
      if (!global)
	{
	  d = pushdecl (d);
	  pushlevel (0);
	  declare_namespace_level ();
	  NAMESPACE_LEVEL (d) = current_binding_level;
	}
    }
  else
    resume_binding_level (NAMESPACE_LEVEL (d));

  if (implicit_use)
    do_using_directive (d);
  /* Enter the name space. */
  current_namespace = d;
}

/* Pop from the scope of the current namespace.  */

void
pop_namespace ()
{
  if (current_namespace == global_namespace)
    {
      my_friendly_assert (in_std>0, 980421);
      in_std--;
      return;
    }
  current_namespace = CP_DECL_CONTEXT (current_namespace);
  /* The binding level is not popped, as it might be re-opened later.  */
  suspend_binding_level ();
}


/* Subroutines for reverting temporarily to top-level for instantiation
   of templates and such.  We actually need to clear out the class- and
   local-value slots of all identifiers, so that only the global values
   are at all visible.  Simply setting current_binding_level to the global
   scope isn't enough, because more binding levels may be pushed.  */
struct saved_scope {
  struct binding_level *old_binding_level;
  tree old_bindings;
  tree old_namespace;
  struct saved_scope *prev;
  tree class_name, class_type;
  tree access_specifier;
  tree function_decl;
  struct binding_level *class_bindings;
  tree *lang_base, *lang_stack, lang_name;
  int lang_stacksize;
  int minimal_parse_mode;
  tree last_function_parms;
  tree template_parms;
  HOST_WIDE_INT processing_template_decl;
  tree previous_class_type, previous_class_values;
  int processing_specialization;
  int processing_explicit_instantiation;
  char *class_cache_firstobj;
};
static struct saved_scope *current_saved_scope;

/* A chain of the binding vecs created by store_bindings.  We create a
   whole bunch of these during compilation, on permanent_obstack, so we
   can't just throw them away.  */
static tree free_binding_vecs;

static tree
store_bindings (names, old_bindings)
     tree names, old_bindings;
{
  tree t;
  for (t = names; t; t = TREE_CHAIN (t))
    {
      tree binding, t1, id;

      if (TREE_CODE (t) == TREE_LIST)
	id = TREE_PURPOSE (t);
      else
	id = DECL_NAME (t);

      if (!id 
	  /* Note that we may have an IDENTIFIER_CLASS_VALUE even when
	     we have no IDENTIFIER_BINDING if we have left the class
	     scope, but cached the class-level declarations.  */
	  || !(IDENTIFIER_BINDING (id) || IDENTIFIER_CLASS_VALUE (id)))
	continue;

      for (t1 = old_bindings; t1; t1 = TREE_CHAIN (t1))
	if (TREE_VEC_ELT (t1, 0) == id)
	  goto skip_it;

      if (free_binding_vecs)
	{
	  binding = free_binding_vecs;
	  free_binding_vecs = TREE_CHAIN (free_binding_vecs);
	}
      else
	binding = make_tree_vec (4);

      if (id)
	{
	  my_friendly_assert (TREE_CODE (id) == IDENTIFIER_NODE, 135);
	  TREE_VEC_ELT (binding, 0) = id;
	  TREE_VEC_ELT (binding, 1) = REAL_IDENTIFIER_TYPE_VALUE (id);
	  TREE_VEC_ELT (binding, 2) = IDENTIFIER_BINDING (id);
	  TREE_VEC_ELT (binding, 3) = IDENTIFIER_CLASS_VALUE (id);
	  IDENTIFIER_BINDING (id) = NULL_TREE;
	  IDENTIFIER_CLASS_VALUE (id) = NULL_TREE;
	}
      TREE_CHAIN (binding) = old_bindings;
      old_bindings = binding;
    skip_it:
      ;
    }
  return old_bindings;
}

void
maybe_push_to_top_level (pseudo)
     int pseudo;
{
  extern int current_lang_stacksize;
  struct saved_scope *s
    = (struct saved_scope *) xmalloc (sizeof (struct saved_scope));
  struct binding_level *b = current_binding_level;
  tree old_bindings = NULL_TREE;

  push_cp_function_context (NULL_TREE);

  if (previous_class_type)
    old_bindings = store_bindings (previous_class_values, old_bindings);

  /* Have to include global_binding_level, because class-level decls
     aren't listed anywhere useful.  */
  for (; b; b = b->level_chain)
    {
      tree t;

      /* Template IDs are inserted into the global level. If they were
	 inserted into namespace level, finish_file wouldn't find them
	 when doing pending instantiations. Therefore, don't stop at
	 namespace level, but continue until :: .  */
      if (b == global_binding_level || (pseudo && b->pseudo_global))
	break;

      old_bindings = store_bindings (b->names, old_bindings);
      /* We also need to check class_shadowed to save class-level type
	 bindings, since pushclass doesn't fill in b->names.  */
      if (b->parm_flag == 2)
	old_bindings = store_bindings (b->class_shadowed, old_bindings);

      /* Unwind type-value slots back to top level.  */
      for (t = b->type_shadowed; t; t = TREE_CHAIN (t))
	SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (t), TREE_VALUE (t));
    }

  s->old_binding_level = current_binding_level;
  current_binding_level = b;

  s->old_namespace = current_namespace;
  s->class_name = current_class_name;
  s->class_type = current_class_type;
  s->access_specifier = current_access_specifier;
  s->function_decl = current_function_decl;
  s->class_bindings = class_binding_level;
  s->lang_stack = current_lang_stack;
  s->lang_base = current_lang_base;
  s->lang_stacksize = current_lang_stacksize;
  s->lang_name = current_lang_name;
  s->minimal_parse_mode = minimal_parse_mode;
  s->last_function_parms = last_function_parms;
  s->template_parms = current_template_parms;
  s->processing_template_decl = processing_template_decl;
  s->previous_class_type = previous_class_type;
  s->previous_class_values = previous_class_values;
  s->class_cache_firstobj = class_cache_firstobj;
  s->processing_specialization = processing_specialization;
  s->processing_explicit_instantiation = processing_explicit_instantiation;

  current_class_name = current_class_type = NULL_TREE;
  current_function_decl = NULL_TREE;
  class_binding_level = (struct binding_level *)0;
  current_lang_stacksize = 10;
  current_lang_stack = current_lang_base
    = (tree *) xmalloc (current_lang_stacksize * sizeof (tree));
  current_lang_name = lang_name_cplusplus;
  strict_prototype = strict_prototypes_lang_cplusplus;
  named_labels = NULL_TREE;
  shadowed_labels = NULL_TREE;
  minimal_parse_mode = 0;
  previous_class_type = previous_class_values = NULL_TREE;
  class_cache_firstobj = 0;
  processing_specialization = 0;
  processing_explicit_instantiation = 0;
  current_template_parms = NULL_TREE;
  processing_template_decl = 0;
  current_namespace = global_namespace;

  s->prev = current_saved_scope;
  s->old_bindings = old_bindings;
  current_saved_scope = s;

  push_obstacks (&permanent_obstack, &permanent_obstack);
}

void
push_to_top_level ()
{
  maybe_push_to_top_level (0);
}

void
pop_from_top_level ()
{
  extern int current_lang_stacksize;
  struct saved_scope *s = current_saved_scope;
  tree t;

  /* Clear out class-level bindings cache.  */
  if (previous_class_type)
    invalidate_class_lookup_cache ();

  pop_obstacks ();

  current_binding_level = s->old_binding_level;
  current_saved_scope = s->prev;
  for (t = s->old_bindings; t; )
    {
      tree save = t;
      tree id = TREE_VEC_ELT (t, 0);
      if (id)
	{
	  SET_IDENTIFIER_TYPE_VALUE (id, TREE_VEC_ELT (t, 1));
	  IDENTIFIER_BINDING (id) = TREE_VEC_ELT (t, 2);
	  IDENTIFIER_CLASS_VALUE (id) = TREE_VEC_ELT (t, 3);
	}
      t = TREE_CHAIN (t);
      TREE_CHAIN (save) = free_binding_vecs;
      free_binding_vecs = save;
    }
  current_namespace = s->old_namespace;
  current_class_name = s->class_name;
  current_class_type = s->class_type;
  current_access_specifier = s->access_specifier;
  current_function_decl = s->function_decl;
  class_binding_level = s->class_bindings;
  free (current_lang_base);
  current_lang_base = s->lang_base;
  current_lang_stack = s->lang_stack;
  current_lang_name = s->lang_name;
  current_lang_stacksize = s->lang_stacksize;
  if (current_lang_name == lang_name_cplusplus)
    strict_prototype = strict_prototypes_lang_cplusplus;
  else if (current_lang_name == lang_name_c)
    strict_prototype = strict_prototypes_lang_c;
  minimal_parse_mode = s->minimal_parse_mode;
  last_function_parms = s->last_function_parms;
  current_template_parms = s->template_parms;
  processing_template_decl = s->processing_template_decl;
  previous_class_type = s->previous_class_type;
  previous_class_values = s->previous_class_values;
  processing_specialization = s->processing_specialization;
  processing_explicit_instantiation = s->processing_explicit_instantiation;
  class_cache_firstobj = s->class_cache_firstobj;

  free (s);

  pop_cp_function_context (NULL_TREE);
}

/* Push a definition of struct, union or enum tag "name".
   into binding_level "b".   "type" should be the type node, 
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be a NULL_TREE.

   C++ gratuitously puts all these tags in the name space.  */

/* When setting the IDENTIFIER_TYPE_VALUE field of an identifier ID,
   record the shadowed value for this binding contour.  TYPE is
   the type that ID maps to.  */

static void
set_identifier_type_value_with_scope (id, type, b)
     tree id;
     tree type;
     struct binding_level *b;
{
  if (!b->namespace_p)
    {
      /* Shadow the marker, not the real thing, so that the marker
	 gets restored later. */
      tree old_type_value = REAL_IDENTIFIER_TYPE_VALUE (id);
      b->type_shadowed
	= tree_cons (id, old_type_value, b->type_shadowed);
    }
  else
    {
      tree binding = binding_for_name (id, current_namespace);
      BINDING_TYPE (binding) = type;
      /* Store marker instead of real type. */
      type = global_type_node;
    }
  SET_IDENTIFIER_TYPE_VALUE (id, type);
}

/* As set_identifier_type_value_with_scope, but using current_binding_level.  */

void
set_identifier_type_value (id, type)
     tree id;
     tree type;
{
  set_identifier_type_value_with_scope (id, type, current_binding_level);
}

/* Return the type associated with id. */

tree
identifier_type_value (id)
     tree id;
{
  /* There is no type with that name, anywhere. */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) == NULL_TREE)
    return NULL_TREE;
  /* This is not the type marker, but the real thing. */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) != global_type_node)
    return REAL_IDENTIFIER_TYPE_VALUE (id);
  /* Have to search for it. It must be on the global level, now.
     Ask lookup_name not to return non-types. */
  id = lookup_name_real (id, 2, 1, 0);
  if (id)
    return TREE_TYPE (id);
  return NULL_TREE;
}

/* Pop off extraneous binding levels left over due to syntax errors.

   We don't pop past namespaces, as they might be valid.  */

void
pop_everything ()
{
#ifdef DEBUG_CP_BINDING_LEVELS
  fprintf (stderr, "XXX entering pop_everything ()\n");
#endif
  while (!toplevel_bindings_p ())
    {
      if (current_binding_level->parm_flag == 2)
	pop_nested_class ();
      else
	poplevel (0, 0, 0);
    }
#ifdef DEBUG_CP_BINDING_LEVELS
  fprintf (stderr, "XXX leaving pop_everything ()\n");
#endif
}

/* The type TYPE is being declared.  If it is a class template, or a
   specialization of a class template, do any processing required and
   perform error-checking.  If IS_FRIEND is non-zero, this TYPE is
   being declared a friend.  B is the binding level at which this TYPE
   should be bound.

   Returns the TYPE_DECL for TYPE, which may have been altered by this
   processing.  */

static tree 
maybe_process_template_type_declaration (type, globalize, b)
     tree type;
     int globalize;
     struct binding_level* b;
{
  tree decl = TYPE_NAME (type);
 
  if (processing_template_parmlist)
    /* You can't declare a new template type in a template parameter
       list.  But, you can declare a non-template type:
       
         template <class A*> struct S;
       
       is a forward-declaration of `A'.  */
    ;
  else 
    {
      maybe_check_template_type (type);

      my_friendly_assert (IS_AGGR_TYPE (type) 
			  || TREE_CODE (type) == ENUMERAL_TYPE, 0);
			  
			  
      if (/* If !GLOBALIZE then we are looking at a definition.
	     It may not be a primary template.  (For example, in:
		  
	       template <class T>
	       struct S1 { class S2 {}; }
		  
	     we have to push_template_decl for S2.)  */
	  (processing_template_decl && !globalize)
	  /* If we are declaring a friend template class, we will
	     have GLOBALIZE set, since something like:

	       template <class T>
	       struct S1 {
		 template <class U>
		 friend class S2; 
	       };

	     declares S2 to be at global scope.  */
	  || PROCESSING_REAL_TEMPLATE_DECL_P ())
	{
	  /* This may change after the call to
	     push_template_decl_real, but we want the original value.  */
	  tree name = DECL_NAME (decl);

	  decl = push_template_decl_real (decl, globalize);
	  /* If the current binding level is the binding level for the
	     template parameters (see the comment in
	     begin_template_parm_list) and the enclosing level is a class
	     scope, and we're not looking at a friend, push the
	     declaration of the member class into the class scope.  In the
	     friend case, push_template_decl will already have put the
	     friend into global scope, if appropriate.  */
	  if (TREE_CODE (type) != ENUMERAL_TYPE
	      && !globalize && b->pseudo_global
	      && b->level_chain->parm_flag == 2)
	    {
	      finish_member_declaration (CLASSTYPE_TI_TEMPLATE (type));
	      /* Put this tag on the list of tags for the class, since
		 that won't happen below because B is not the class
		 binding level, but is instead the pseudo-global level.  */
	      b->level_chain->tags = 
		saveable_tree_cons (name, type, b->level_chain->tags);
	      if (TYPE_SIZE (current_class_type) == NULL_TREE)
		CLASSTYPE_TAGS (current_class_type) = b->level_chain->tags;
	    }
	}
    }

  return decl;
}

/* Push a tag name NAME for struct/class/union/enum type TYPE.
   Normally put it into the inner-most non-tag-transparent scope,
   but if GLOBALIZE is true, put it in the inner-most non-class scope.
   The latter is needed for implicit declarations.  */

void
pushtag (name, type, globalize)
     tree name, type;
     int globalize;
{
  register struct binding_level *b;

  b = current_binding_level;
  while (b->tag_transparent
	 || (globalize && b->parm_flag == 2))
    b = b->level_chain;

  if (toplevel_bindings_p ())
    b->tags = perm_tree_cons (name, type, b->tags);
  else
    b->tags = saveable_tree_cons (name, type, b->tags);

  if (name)
    {
      /* Do C++ gratuitous typedefing.  */
      if (IDENTIFIER_TYPE_VALUE (name) != type)
        {
          register tree d = NULL_TREE;
	  int newdecl = 0, in_class = 0;
	  tree context;
	  tree c_decl = NULL_TREE;

	  context = type ? TYPE_CONTEXT (type) : NULL_TREE;
	  if (! context)
	    {
	      tree cs = current_scope ();

	      if (! globalize)
		context = cs;
	      else if (cs != NULL_TREE 
		       && TREE_CODE_CLASS (TREE_CODE (cs)) == 't')
		/* When declaring a friend class of a local class, we want
		   to inject the newly named class into the scope
		   containing the local class, not the namespace scope.  */
		context = hack_decl_function_context (get_type_decl (cs));
	    }
	  if (context)
	    c_decl = TREE_CODE (context) == FUNCTION_DECL
	      ? context : TYPE_MAIN_DECL (context);

	  if (!context)
	    context = current_namespace;

	  if ((b->pseudo_global && b->level_chain->parm_flag == 2)
	      || b->parm_flag == 2)
	    in_class = 1;
	  else
	    d = lookup_nested_type (type, c_decl);

	  if (d == NULL_TREE)
	    {
	      newdecl = 1;
	      d = build_decl (TYPE_DECL, name, type);
	      if (current_lang_name == lang_name_java)
		TYPE_FOR_JAVA (type) = 1;
	      SET_DECL_ARTIFICIAL (d);
	      if (! in_class)
		set_identifier_type_value_with_scope (name, type, b);
	    }
	  else
	    d = TYPE_MAIN_DECL (d);

	  TYPE_NAME (type) = d;
	  DECL_CONTEXT (d) = FROB_CONTEXT (context);

	  d = maybe_process_template_type_declaration (type,
						       globalize, b);

	  if (b->parm_flag == 2)
	    {
	      if (newdecl && !PROCESSING_REAL_TEMPLATE_DECL_P ())
		/* Put this TYPE_DECL on the TYPE_FIELDS list for the
		   class.  But if it's a member template class, we
		   want the TEMPLATE_DECL, not the TYPE_DECL, so this
		   is done later.  */
		finish_member_declaration (d);
	      else
		pushdecl_class_level (d);
	    }
	  else
	    d = pushdecl_with_scope (d, b);

	  if (newdecl)
	    {
	      if (ANON_AGGRNAME_P (name))
		DECL_IGNORED_P (d) = 1;

	      TYPE_CONTEXT (type) = DECL_CONTEXT (d);
	      DECL_ASSEMBLER_NAME (d) = DECL_NAME (d);
	      if (!uses_template_parms (type))
		DECL_ASSEMBLER_NAME (d)
		  = get_identifier (build_overload_name (type, 1, 1));
	    }
        }
      if (b->parm_flag == 2)
	{
	  if (TYPE_SIZE (current_class_type) == NULL_TREE)
	    CLASSTYPE_TAGS (current_class_type) = b->tags;
	}
    }

  if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
    /* Use the canonical TYPE_DECL for this node.  */
    TYPE_STUB_DECL (type) = TYPE_NAME (type);
  else
    {
      /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE
	 will be the tagged type we just added to the current
	 binding level.  This fake NULL-named TYPE_DECL node helps
	 dwarfout.c to know when it needs to output a
	 representation of a tagged type, and it also gives us a
	 convenient place to record the "scope start" address for
	 the tagged type.  */

      tree d = build_decl (TYPE_DECL, NULL_TREE, type);
      TYPE_STUB_DECL (type) = pushdecl_with_scope (d, b);
    }
}

/* Counter used to create anonymous type names.  */

static int anon_cnt = 0;

/* Return an IDENTIFIER which can be used as a name for
   anonymous structs and unions.  */

tree
make_anon_name ()
{
  char buf[32];

  sprintf (buf, ANON_AGGRNAME_FORMAT, anon_cnt++);
  return get_identifier (buf);
}

/* Clear the TREE_PURPOSE slot of tags which have anonymous typenames.
   This keeps dbxout from getting confused.  */

void
clear_anon_tags ()
{
  register struct binding_level *b;
  register tree tags;
  static int last_cnt = 0;

  /* Fast out if no new anon names were declared.  */
  if (last_cnt == anon_cnt)
    return;

  b = current_binding_level;
  while (b->tag_transparent)
    b = b->level_chain;
  tags = b->tags;
  while (tags)
    {
      /* A NULL purpose means we have already processed all tags
	 from here to the end of the list.  */
      if (TREE_PURPOSE (tags) == NULL_TREE)
	break;
      if (ANON_AGGRNAME_P (TREE_PURPOSE (tags)))
	TREE_PURPOSE (tags) = NULL_TREE;
      tags = TREE_CHAIN (tags);
    }
  last_cnt = anon_cnt;
}

/* Subroutine of duplicate_decls: return truthvalue of whether
   or not types of these decls match.

   For C++, we must compare the parameter list so that `int' can match
   `int&' in a parameter position, but `int&' is not confused with
   `const int&'.  */

int
decls_match (newdecl, olddecl)
     tree newdecl, olddecl;
{
  int types_match;

  if (newdecl == olddecl)
    return 1;

  if (TREE_CODE (newdecl) != TREE_CODE (olddecl))
    /* If the two DECLs are not even the same kind of thing, we're not
       interested in their types.  */
    return 0;

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      tree f1 = TREE_TYPE (newdecl);
      tree f2 = TREE_TYPE (olddecl);
      tree p1 = TYPE_ARG_TYPES (f1);
      tree p2 = TYPE_ARG_TYPES (f2);

      if (DECL_REAL_CONTEXT (newdecl) != DECL_REAL_CONTEXT (olddecl)
	  && ! (DECL_LANGUAGE (newdecl) == lang_c
		&& DECL_LANGUAGE (olddecl) == lang_c))
	return 0;

      /* When we parse a static member function definition,
	 we put together a FUNCTION_DECL which thinks its type
	 is METHOD_TYPE.  Change that to FUNCTION_TYPE, and
	 proceed.  */
      if (TREE_CODE (f1) == METHOD_TYPE && DECL_STATIC_FUNCTION_P (olddecl))
	revert_static_member_fn (&newdecl, &f1, &p1);
      else if (TREE_CODE (f2) == METHOD_TYPE
	       && DECL_STATIC_FUNCTION_P (newdecl))
	revert_static_member_fn (&olddecl, &f2, &p2);

      /* Here we must take care of the case where new default
	 parameters are specified.  Also, warn if an old
	 declaration becomes ambiguous because default
	 parameters may cause the two to be ambiguous.  */
      if (TREE_CODE (f1) != TREE_CODE (f2))
	{
	  if (TREE_CODE (f1) == OFFSET_TYPE)
	    cp_compiler_error ("`%D' redeclared as member function", newdecl);
	  else
	    cp_compiler_error ("`%D' redeclared as non-member function", newdecl);
	  return 0;
	}

      if (same_type_p (TREE_TYPE (f1), TREE_TYPE (f2)))
	{
	  if (! strict_prototypes_lang_c && DECL_LANGUAGE (olddecl) == lang_c
	      && p2 == NULL_TREE)
	    {
	      types_match = self_promoting_args_p (p1);
	      if (p1 == void_list_node)
		TREE_TYPE (newdecl) = TREE_TYPE (olddecl);
	    }
	  else if (!strict_prototypes_lang_c && DECL_LANGUAGE (olddecl)==lang_c
		   && DECL_LANGUAGE (newdecl) == lang_c && p1 == NULL_TREE)
	    {
	      types_match = self_promoting_args_p (p2);
	      TREE_TYPE (newdecl) = TREE_TYPE (olddecl);
	    }
	  else
	    types_match = compparms (p1, p2);
	}
      else
	types_match = 0;
    }
  else if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      if (!comp_template_parms (DECL_TEMPLATE_PARMS (newdecl),
				DECL_TEMPLATE_PARMS (olddecl)))
	return 0;
      
      if (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL)
	types_match = 1;
      else
	types_match = decls_match (DECL_TEMPLATE_RESULT (olddecl),
				   DECL_TEMPLATE_RESULT (newdecl));
    }
  else
    {
      if (TREE_TYPE (newdecl) == error_mark_node)
	types_match = TREE_TYPE (olddecl) == error_mark_node;
      else if (TREE_TYPE (olddecl) == NULL_TREE)
	types_match = TREE_TYPE (newdecl) == NULL_TREE;
      else if (TREE_TYPE (newdecl) == NULL_TREE)
	types_match = 0;
      else
	types_match = comptypes (TREE_TYPE (newdecl),
				 TREE_TYPE (olddecl),
				 COMPARE_REDECLARATION);
    }

  return types_match;
}

/* If NEWDECL is `static' and an `extern' was seen previously,
   warn about it.  (OLDDECL may be NULL_TREE; NAME contains
   information about previous usage as an `extern'.)

   Note that this does not apply to the C++ case of declaring
   a variable `extern const' and then later `const'.

   Don't complain about built-in functions, since they are beyond
   the user's control.  */

static void
warn_extern_redeclared_static (newdecl, olddecl)
     tree newdecl, olddecl;
{
  tree name;

  static const char *explicit_extern_static_warning
    = "`%D' was declared `extern' and later `static'";
  static const char *implicit_extern_static_warning
    = "`%D' was declared implicitly `extern' and later `static'";

  if (TREE_CODE (newdecl) == TYPE_DECL)
    return;

  name = DECL_ASSEMBLER_NAME (newdecl);
  if (TREE_PUBLIC (name) && DECL_THIS_STATIC (newdecl))
    {
      /* It's okay to redeclare an ANSI built-in function as static,
	 or to declare a non-ANSI built-in function as anything.  */
      if (! (TREE_CODE (newdecl) == FUNCTION_DECL
	     && olddecl != NULL_TREE
	     && TREE_CODE (olddecl) == FUNCTION_DECL
	     && (DECL_BUILT_IN (olddecl)
		 || DECL_BUILT_IN_NONANSI (olddecl))))
	{
	  cp_pedwarn (IDENTIFIER_IMPLICIT_DECL (name)
		      ? implicit_extern_static_warning
		      : explicit_extern_static_warning, newdecl);
	  if (olddecl != NULL_TREE)
	    cp_pedwarn_at ("previous declaration of `%D'", olddecl);
	}
    }
}

/* Handle when a new declaration NEWDECL has the same name as an old
   one OLDDECL in the same binding contour.  Prints an error message
   if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.  */

int
duplicate_decls (newdecl, olddecl)
     tree newdecl, olddecl;
{
  extern struct obstack permanent_obstack;
  unsigned olddecl_uid = DECL_UID (olddecl);
  int olddecl_friend = 0, types_match = 0;
  int new_defines_function = 0;

  if (newdecl == olddecl)
    return 1;

  types_match = decls_match (newdecl, olddecl);

  /* If either the type of the new decl or the type of the old decl is an
     error_mark_node, then that implies that we have already issued an
     error (earlier) for some bogus type specification, and in that case,
     it is rather pointless to harass the user with yet more error message
     about the same declaration, so just pretend the types match here.  */
  if (TREE_TYPE (newdecl) == error_mark_node
      || TREE_TYPE (olddecl) == error_mark_node)
    types_match = 1;
 
  /* Check for redeclaration and other discrepancies. */
  if (TREE_CODE (olddecl) == FUNCTION_DECL
      && DECL_ARTIFICIAL (olddecl)
      && (DECL_BUILT_IN (olddecl) || DECL_BUILT_IN_NONANSI (olddecl)))
    {
      /* If you declare a built-in or predefined function name as static,
	 the old definition is overridden, but optionally warn this was a
	 bad choice of name.  Ditto for overloads.  */
      if (! TREE_PUBLIC (newdecl)
	  || (TREE_CODE (newdecl) == FUNCTION_DECL
	      && DECL_LANGUAGE (newdecl) != DECL_LANGUAGE (olddecl)))
	{
	  if (warn_shadow)
	    cp_warning ("shadowing %s function `%#D'",
			DECL_BUILT_IN (olddecl) ? "built-in" : "library",
			olddecl);
	  /* Discard the old built-in function.  */
	  return 0;
	}
      else if (! types_match)
	{
	  if (TREE_CODE (newdecl) != FUNCTION_DECL)
	    {
	      /* If the built-in is not ansi, then programs can override
		 it even globally without an error.  */
	      if (! DECL_BUILT_IN (olddecl))
		cp_warning ("library function `%#D' redeclared as non-function `%#D'",
			    olddecl, newdecl);
	      else
		{
		  cp_error ("declaration of `%#D'", newdecl);
		  cp_error ("conflicts with built-in declaration `%#D'",
			    olddecl);
		}
	      return 0;
	    }

	  cp_warning ("declaration of `%#D'", newdecl);
	  cp_warning ("conflicts with built-in declaration `%#D'",
		      olddecl);
	}
    }
  else if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if ((TREE_CODE (olddecl) == TYPE_DECL && DECL_ARTIFICIAL (olddecl)
	   && TREE_CODE (newdecl) != TYPE_DECL
	   && ! (TREE_CODE (newdecl) == TEMPLATE_DECL
		 && TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL))
	  || (TREE_CODE (newdecl) == TYPE_DECL && DECL_ARTIFICIAL (newdecl)
	      && TREE_CODE (olddecl) != TYPE_DECL
	      && ! (TREE_CODE (olddecl) == TEMPLATE_DECL
		    && (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl))
			== TYPE_DECL))))
	{
	  /* We do nothing special here, because C++ does such nasty
	     things with TYPE_DECLs.  Instead, just let the TYPE_DECL
	     get shadowed, and know that if we need to find a TYPE_DECL
	     for a given name, we can look in the IDENTIFIER_TYPE_VALUE
	     slot of the identifier.  */
	  return 0;
	}

      if ((TREE_CODE (newdecl) == FUNCTION_DECL
	   && DECL_FUNCTION_TEMPLATE_P (olddecl))
	  || (TREE_CODE (olddecl) == FUNCTION_DECL
	      && DECL_FUNCTION_TEMPLATE_P (newdecl)))
	return 0;

      cp_error ("`%#D' redeclared as different kind of symbol", newdecl);
      if (TREE_CODE (olddecl) == TREE_LIST)
	olddecl = TREE_VALUE (olddecl);
      cp_error_at ("previous declaration of `%#D'", olddecl);

      /* New decl is completely inconsistent with the old one =>
	 tell caller to replace the old one.  */

      return 0;
    }
  else if (!types_match)
    {
      if (DECL_REAL_CONTEXT (newdecl) != DECL_REAL_CONTEXT (olddecl))
	/* These are certainly not duplicate declarations; they're
	   from different scopes.  */
	return 0;

      if (TREE_CODE (newdecl) == TEMPLATE_DECL)
	{
	  /* The name of a class template may not be declared to refer to
	     any other template, class, function, object, namespace, value,
	     or type in the same scope.  */
	  if (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl)) == TYPE_DECL
	      || TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL)
	    {
	      cp_error ("declaration of template `%#D'", newdecl);
	      cp_error_at ("conflicts with previous declaration `%#D'",
			   olddecl);
	    }
	  else if (TREE_CODE (DECL_TEMPLATE_RESULT (olddecl)) == FUNCTION_DECL
		   && TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == FUNCTION_DECL
		   && compparms (TYPE_ARG_TYPES (TREE_TYPE (DECL_TEMPLATE_RESULT (olddecl))),
				 TYPE_ARG_TYPES (TREE_TYPE (DECL_TEMPLATE_RESULT (newdecl))))
		   && comp_template_parms (DECL_TEMPLATE_PARMS (newdecl),
					   DECL_TEMPLATE_PARMS (olddecl)))
	    {
	      cp_error ("new declaration `%#D'", newdecl);
	      cp_error_at ("ambiguates old declaration `%#D'", olddecl);
	    }
	  return 0;
	}
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  if (DECL_LANGUAGE (newdecl) == lang_c
	      && DECL_LANGUAGE (olddecl) == lang_c)
	    {
	      cp_error ("declaration of C function `%#D' conflicts with",
			newdecl);
	      cp_error_at ("previous declaration `%#D' here", olddecl);
	    }
	  else if (compparms (TYPE_ARG_TYPES (TREE_TYPE (newdecl)),
			      TYPE_ARG_TYPES (TREE_TYPE (olddecl))))
	    {
	      cp_error ("new declaration `%#D'", newdecl);
	      cp_error_at ("ambiguates old declaration `%#D'", olddecl);
	    }
	  else
	    return 0;
	}

      /* Already complained about this, so don't do so again.  */
      else if (current_class_type == NULL_TREE
	  || IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (newdecl)) != current_class_type)
	{
	  cp_error ("conflicting types for `%#D'", newdecl);
	  cp_error_at ("previous declaration as `%#D'", olddecl);
	}
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL 
	    && ((DECL_TEMPLATE_SPECIALIZATION (olddecl)
		 && (!DECL_TEMPLATE_INFO (newdecl)
		     || (DECL_TI_TEMPLATE (newdecl) 
			 != DECL_TI_TEMPLATE (olddecl))))
		|| (DECL_TEMPLATE_SPECIALIZATION (newdecl)
		    && (!DECL_TEMPLATE_INFO (olddecl)
			|| (DECL_TI_TEMPLATE (olddecl) 
			    != DECL_TI_TEMPLATE (newdecl))))))
    /* It's OK to have a template specialization and a non-template
       with the same type, or to have specializations of two
       different templates with the same type.  Note that if one is a
       specialization, and the other is an instantiation of the same
       template, that we do not exit at this point.  That situation
       can occur if we instantiate a template class, and then
       specialize one of its methods.  This situation is legal, but
       the declarations must be merged in the usual way.  */
    return 0;
  else if (TREE_CODE (newdecl) == FUNCTION_DECL 
	   && ((DECL_TEMPLATE_INSTANTIATION (olddecl) 
		&& !DECL_USE_TEMPLATE (newdecl))
	       || (DECL_TEMPLATE_INSTANTIATION (newdecl)
		   && !DECL_USE_TEMPLATE (olddecl))))
    /* One of the declarations is a template instantiation, and the
       other is not a template at all.  That's OK.  */
    return 0;
  else if (TREE_CODE (newdecl) == NAMESPACE_DECL
           && DECL_NAMESPACE_ALIAS (newdecl)
           && DECL_NAMESPACE_ALIAS (newdecl) == DECL_NAMESPACE_ALIAS (olddecl))
    /* Redeclaration of namespace alias, ignore it. */
    return 1;
  else
    {
      const char *errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  cp_error (errmsg, newdecl);
	  if (DECL_NAME (olddecl) != NULL_TREE)
	    cp_error_at ((DECL_INITIAL (olddecl)
			  && namespace_bindings_p ())
			 ? "`%#D' previously defined here"
			 : "`%#D' previously declared here", olddecl);
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != NULL_TREE
	       && TYPE_ARG_TYPES (TREE_TYPE (olddecl)) == NULL_TREE
	       && TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != NULL_TREE)
	{
	  /* Prototype decl follows defn w/o prototype.  */
	  cp_warning_at ("prototype for `%#D'", newdecl);
	  cp_warning_at ("follows non-prototype definition here", olddecl);
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_LANGUAGE (newdecl) != DECL_LANGUAGE (olddecl))
	{
	  /* extern "C" int foo ();
	     int foo () { bar (); }
	     is OK.  */
	  if (current_lang_stack == current_lang_base)
	    DECL_LANGUAGE (newdecl) = DECL_LANGUAGE (olddecl);
	  else
	    {
	      cp_error_at ("previous declaration of `%#D' with %L linkage",
			   olddecl, DECL_LANGUAGE (olddecl));
	      cp_error ("conflicts with new declaration with %L linkage",
			DECL_LANGUAGE (newdecl));
	    }
	}

      if (DECL_LANG_SPECIFIC (olddecl) && DECL_USE_TEMPLATE (olddecl))
	;
      else if (TREE_CODE (olddecl) == FUNCTION_DECL)
	{
	  tree t1 = TYPE_ARG_TYPES (TREE_TYPE (olddecl));
	  tree t2 = TYPE_ARG_TYPES (TREE_TYPE (newdecl));
	  int i = 1;

	  if (TREE_CODE (TREE_TYPE (newdecl)) == METHOD_TYPE)
	    t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2);
	
	  for (; t1 && t1 != void_list_node;
	       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2), i++)
	    if (TREE_PURPOSE (t1) && TREE_PURPOSE (t2))
	      {
		if (1 == simple_cst_equal (TREE_PURPOSE (t1),
					   TREE_PURPOSE (t2)))
		  {
		    if (pedantic)
		      {
			cp_pedwarn ("default argument given for parameter %d of `%#D'",
				    i, newdecl);
			cp_pedwarn_at ("after previous specification in `%#D'",
				       olddecl);
		      }
		  }
		else
		  {
		    cp_error ("default argument given for parameter %d of `%#D'",
			      i, newdecl);
		    cp_error_at ("after previous specification in `%#D'",
				 olddecl);
		  }
	      }

	  if (DECL_THIS_INLINE (newdecl) && ! DECL_THIS_INLINE (olddecl)
	      && TREE_ADDRESSABLE (olddecl) && warn_inline)
	    {
	      cp_warning ("`%#D' was used before it was declared inline",
			  newdecl);
	      cp_warning_at ("previous non-inline declaration here",
			     olddecl);
	    }
	}
    }

  /* If new decl is `static' and an `extern' was seen previously,
     warn about it.  */
  warn_extern_redeclared_static (newdecl, olddecl);

  /* We have committed to returning 1 at this point.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Now that functions must hold information normally held
	 by field decls, there is extra work to do so that
	 declaration information does not get destroyed during
	 definition.  */
      if (DECL_VINDEX (olddecl))
	DECL_VINDEX (newdecl) = DECL_VINDEX (olddecl);
      if (DECL_CONTEXT (olddecl))
	DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
      if (DECL_CLASS_CONTEXT (olddecl))
	DECL_CLASS_CONTEXT (newdecl) = DECL_CLASS_CONTEXT (olddecl);
      if (DECL_PENDING_INLINE_INFO (newdecl) == (struct pending_inline *)0)
	DECL_PENDING_INLINE_INFO (newdecl) = DECL_PENDING_INLINE_INFO (olddecl);
      DECL_STATIC_CONSTRUCTOR (newdecl) |= DECL_STATIC_CONSTRUCTOR (olddecl);
      DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
      DECL_ABSTRACT_VIRTUAL_P (newdecl) |= DECL_ABSTRACT_VIRTUAL_P (olddecl);
      DECL_VIRTUAL_P (newdecl) |= DECL_VIRTUAL_P (olddecl);
      DECL_NEEDS_FINAL_OVERRIDER_P (newdecl) |= DECL_NEEDS_FINAL_OVERRIDER_P (olddecl);
      new_defines_function = DECL_INITIAL (newdecl) != NULL_TREE;
      
      /* Optionally warn about more than one declaration for the same
         name, but don't warn about a function declaration followed by a
         definition.  */
      if (warn_redundant_decls && ! DECL_ARTIFICIAL (olddecl)
	  && !(new_defines_function && DECL_INITIAL (olddecl) == NULL_TREE)
	  /* Don't warn about extern decl followed by definition. */
	  && !(DECL_EXTERNAL (olddecl) && ! DECL_EXTERNAL (newdecl))
	  /* Don't warn about friends, let add_friend take care of it. */
	  && ! DECL_FRIEND_P (newdecl))
	{
	  cp_warning ("redundant redeclaration of `%D' in same scope", newdecl);
	  cp_warning_at ("previous declaration of `%D'", olddecl);
	}
    }

  /* Deal with C++: must preserve virtual function table size.  */
  if (TREE_CODE (olddecl) == TYPE_DECL)
    {
      register tree newtype = TREE_TYPE (newdecl);
      register tree oldtype = TREE_TYPE (olddecl);

      if (newtype != error_mark_node && oldtype != error_mark_node
	  && TYPE_LANG_SPECIFIC (newtype) && TYPE_LANG_SPECIFIC (oldtype))
	{
	  CLASSTYPE_VSIZE (newtype) = CLASSTYPE_VSIZE (oldtype);
	  CLASSTYPE_FRIEND_CLASSES (newtype)
	    = CLASSTYPE_FRIEND_CLASSES (oldtype);
	}
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.  */
  DECL_MACHINE_ATTRIBUTES (newdecl) 
    = merge_machine_decl_attributes (olddecl, newdecl);

  if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      if (! duplicate_decls (DECL_TEMPLATE_RESULT (newdecl),
			     DECL_TEMPLATE_RESULT (olddecl)))
	cp_error ("invalid redeclaration of %D", newdecl);
      TREE_TYPE (olddecl) = TREE_TYPE (DECL_TEMPLATE_RESULT (olddecl));
      DECL_TEMPLATE_SPECIALIZATIONS (olddecl) 
	= chainon (DECL_TEMPLATE_SPECIALIZATIONS (olddecl),
		   DECL_TEMPLATE_SPECIALIZATIONS (newdecl));
 
      return 1;
    }
    
  if (types_match)
    {
      /* Automatically handles default parameters.  */
      tree oldtype = TREE_TYPE (olddecl);
      tree newtype;

      /* Make sure we put the new type in the same obstack as the old one.  */
      if (oldtype)
	push_obstacks (TYPE_OBSTACK (oldtype), TYPE_OBSTACK (oldtype));
      else
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	}

      /* Merge the data types specified in the two decls.  */
      newtype = common_type (TREE_TYPE (newdecl), TREE_TYPE (olddecl));

      if (TREE_CODE (newdecl) == VAR_DECL)
	DECL_THIS_EXTERN (newdecl) |= DECL_THIS_EXTERN (olddecl);
      /* Do this after calling `common_type' so that default
	 parameters don't confuse us.  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL
	  && (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl))
	      != TYPE_RAISES_EXCEPTIONS (TREE_TYPE (olddecl))))
	{
	  TREE_TYPE (newdecl) = build_exception_variant (newtype,
							 TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl)));
	  TREE_TYPE (olddecl) = build_exception_variant (newtype,
							 TYPE_RAISES_EXCEPTIONS (oldtype));

	  if ((pedantic || (! DECL_IN_SYSTEM_HEADER (olddecl)
	  		    && ! DECL_IN_SYSTEM_HEADER (newdecl)))
	      && DECL_SOURCE_LINE (olddecl) != 0
	      && flag_exceptions
	      && ! compexcepttypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl)))
	    {
	      cp_pedwarn ("declaration of `%D' throws different exceptions",
			newdecl);
	      cp_pedwarn_at ("previous declaration here", olddecl);
	    }
	}
      TREE_TYPE (newdecl) = TREE_TYPE (olddecl) = newtype;

      /* Lay the type out, unless already done.  */
      if (! same_type_p (newtype, oldtype)
	  && TREE_TYPE (newdecl) != error_mark_node
	  && !(processing_template_decl && uses_template_parms (newdecl)))
	layout_type (TREE_TYPE (newdecl));

      if ((TREE_CODE (newdecl) == VAR_DECL
	   || TREE_CODE (newdecl) == PARM_DECL
	   || TREE_CODE (newdecl) == RESULT_DECL
	   || TREE_CODE (newdecl) == FIELD_DECL
	   || TREE_CODE (newdecl) == TYPE_DECL)
	  && !(processing_template_decl && uses_template_parms (newdecl)))
	layout_decl (newdecl, 0);

      /* Merge the type qualifiers.  */
      if (TREE_READONLY (newdecl))
	TREE_READONLY (olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (olddecl) = 1;

      /* Merge the initialization information.  */
      if (DECL_INITIAL (newdecl) == NULL_TREE
	  && DECL_INITIAL (olddecl) != NULL_TREE)
	{
	  DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_SOURCE_FILE (newdecl) = DECL_SOURCE_FILE (olddecl);
	  DECL_SOURCE_LINE (newdecl) = DECL_SOURCE_LINE (olddecl);
	  if (DECL_LANG_SPECIFIC (newdecl)
	      && DECL_LANG_SPECIFIC (olddecl))
	    DECL_SAVED_TREE (newdecl) = DECL_SAVED_TREE (olddecl);
	}

      /* Merge the section attribute.
         We want to issue an error if the sections conflict but that must be
	 done later in decl_attributes since we are called before attributes
	 are assigned.  */
      if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
	DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

      /* Keep the old rtl since we can safely use it, unless it's the
	 call to abort() used for abstract virtuals.  */
      if ((DECL_LANG_SPECIFIC (olddecl)
	   && !DECL_ABSTRACT_VIRTUAL_P (olddecl))
	  || DECL_RTL (olddecl) != DECL_RTL (abort_fndecl))
	DECL_RTL (newdecl) = DECL_RTL (olddecl);

      pop_obstacks ();
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else
    {
      /* Clean out any memory we had of the old declaration.  */
      tree oldstatic = value_member (olddecl, static_aggregates);
      if (oldstatic)
	TREE_VALUE (oldstatic) = error_mark_node;

      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  DECL_WEAK (newdecl) |= DECL_WEAK (olddecl);
  DECL_ONE_ONLY (newdecl) |= DECL_ONE_ONLY (olddecl);
  TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
  TREE_STATIC (olddecl) = TREE_STATIC (newdecl) |= TREE_STATIC (olddecl);
  if (! DECL_EXTERNAL (olddecl))
    DECL_EXTERNAL (newdecl) = 0;
  
  if (DECL_LANG_SPECIFIC (newdecl) && DECL_LANG_SPECIFIC (olddecl))
    {
      DECL_INTERFACE_KNOWN (newdecl) |= DECL_INTERFACE_KNOWN (olddecl);
      DECL_NOT_REALLY_EXTERN (newdecl) |= DECL_NOT_REALLY_EXTERN (olddecl);
      DECL_COMDAT (newdecl) |= DECL_COMDAT (olddecl);
      DECL_TEMPLATE_INSTANTIATED (newdecl) 
	|= DECL_TEMPLATE_INSTANTIATED (olddecl);
      /* Don't really know how much of the language-specific
	 values we should copy from old to new.  */
      DECL_IN_AGGR_P (newdecl) = DECL_IN_AGGR_P (olddecl);
      DECL_ACCESS (newdecl) = DECL_ACCESS (olddecl);
      DECL_NONCONVERTING_P (newdecl) = DECL_NONCONVERTING_P (olddecl);
      DECL_TEMPLATE_INFO (newdecl) = DECL_TEMPLATE_INFO (olddecl);
      olddecl_friend = DECL_FRIEND_P (olddecl);

      /* Only functions have DECL_BEFRIENDING_CLASSES.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL
	  || DECL_FUNCTION_TEMPLATE_P (newdecl))
	DECL_BEFRIENDING_CLASSES (newdecl)
	  = chainon (DECL_BEFRIENDING_CLASSES (newdecl),
		     DECL_BEFRIENDING_CLASSES (olddecl));
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      if (DECL_TEMPLATE_INSTANTIATION (olddecl) 
	  && !DECL_TEMPLATE_INSTANTIATION (newdecl)) 
	{
	  /* If newdecl is not a specialization, then it is not a
	     template-related function at all.  And that means that we
	     shoud have exited above, returning 0.  */
	  my_friendly_assert (DECL_TEMPLATE_SPECIALIZATION (newdecl),
			      0);

	  if (TREE_USED (olddecl)) 
	    /* From [temp.expl.spec]:
	       
	       If a template, a member template or the member of a class
	       template is explicitly specialized then that
	       specialization shall be declared before the first use of
	       that specialization that would cause an implicit
	       instantiation to take place, in every translation unit in
	       which such a use occurs.  */
	    cp_error ("explicit specialization of %D after first use", 
		      olddecl);

	  SET_DECL_TEMPLATE_SPECIALIZATION (olddecl);
	}
      DECL_THIS_INLINE (newdecl) |= DECL_THIS_INLINE (olddecl);

      /* If either decl says `inline', this fn is inline, unless its
         definition was passed already.  */
      if (DECL_INLINE (newdecl) && DECL_INITIAL (olddecl) == NULL_TREE)
	DECL_INLINE (olddecl) = 1;
      DECL_INLINE (newdecl) = DECL_INLINE (olddecl);

      if (! types_match)
	{
	  DECL_LANGUAGE (olddecl) = DECL_LANGUAGE (newdecl);
	  DECL_ASSEMBLER_NAME (olddecl) = DECL_ASSEMBLER_NAME (newdecl);
	  DECL_RTL (olddecl) = DECL_RTL (newdecl);
	}
      if (! types_match || new_defines_function)
	{
	  /* These need to be copied so that the names are available.  */
	  DECL_ARGUMENTS (olddecl) = DECL_ARGUMENTS (newdecl);
	  DECL_RESULT (olddecl) = DECL_RESULT (newdecl);
	}
      if (new_defines_function)
	/* If defining a function declared with other language
	   linkage, use the previously declared language linkage.  */
	DECL_LANGUAGE (newdecl) = DECL_LANGUAGE (olddecl);
      else
	{
	  /* If redeclaring a builtin function, and not a definition,
	     it stays built in.  */
	  if (DECL_BUILT_IN (olddecl))
	    {
	      DECL_BUILT_IN (newdecl) = 1;
	      DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	      /* If we're keeping the built-in definition, keep the rtl,
		 regardless of declaration matches.  */
	      DECL_RTL (newdecl) = DECL_RTL (olddecl);
	    }
	  else
	    DECL_FRAME_SIZE (newdecl) = DECL_FRAME_SIZE (olddecl);

	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  if ((DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl)))
	    /* Previously saved insns go together with
	       the function's previous definition.  */
	    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  /* Don't clear out the arguments if we're redefining a function.  */
	  if (DECL_ARGUMENTS (olddecl))
	    DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
	}
      if (DECL_LANG_SPECIFIC (olddecl))
	DECL_MAIN_VARIANT (newdecl) = DECL_MAIN_VARIANT (olddecl);
    }

  if (TREE_CODE (newdecl) == NAMESPACE_DECL)
    {
      NAMESPACE_LEVEL (newdecl) = NAMESPACE_LEVEL (olddecl);
    }

  /* Now preserve various other info from the definition.  */
  TREE_ADDRESSABLE (newdecl) = TREE_ADDRESSABLE (olddecl);
  TREE_ASM_WRITTEN (newdecl) = TREE_ASM_WRITTEN (olddecl);
  DECL_COMMON (newdecl) = DECL_COMMON (olddecl);
  DECL_ASSEMBLER_NAME (newdecl) = DECL_ASSEMBLER_NAME (olddecl);

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      int function_size;
      struct lang_decl *ol = DECL_LANG_SPECIFIC (olddecl);
      struct lang_decl *nl = DECL_LANG_SPECIFIC (newdecl);

      function_size = sizeof (struct tree_decl);

      bcopy ((char *) newdecl + sizeof (struct tree_common),
	     (char *) olddecl + sizeof (struct tree_common),
	     function_size - sizeof (struct tree_common));

      /* Can we safely free the storage used by newdecl?  */

#define ROUND(x) ((x + obstack_alignment_mask (&permanent_obstack)) \
		  & ~ obstack_alignment_mask (&permanent_obstack))

      if (DECL_TEMPLATE_INSTANTIATION (newdecl))
	{
	  /* If newdecl is a template instantiation, it is possible that
	     the following sequence of events has occurred:

	     o A friend function was declared in a class template.  The
	     class template was instantiated.  

	     o The instantiation of the friend declaration was 
	     recorded on the instantiation list, and is newdecl.  

	     o Later, however, instantiate_class_template called pushdecl
	     on the newdecl to perform name injection.  But, pushdecl in
	     turn called duplicate_decls when it discovered that another
	     declaration of a global function with the same name already
	     existed. 

	     o Here, in duplicate_decls, we decided to clobber newdecl.

	     If we're going to do that, we'd better make sure that
	     olddecl, and not newdecl, is on the list of
	     instantiations so that if we try to do the instantiation
	     again we won't get the clobbered declaration.  */

	  tree tmpl = DECL_TI_TEMPLATE (newdecl); 
	  tree decls = DECL_TEMPLATE_SPECIALIZATIONS (tmpl); 

	  for (; decls; decls = TREE_CHAIN (decls))
	    if (TREE_VALUE (decls) == newdecl)
	      TREE_VALUE (decls) = olddecl;
	}

      if (((char *)newdecl + ROUND (function_size) == (char *)nl
	   && ((char *)newdecl + ROUND (function_size)
	       + ROUND (sizeof (struct lang_decl))
	       == obstack_next_free (&permanent_obstack)))
	  || ((char *)newdecl + ROUND (function_size)
	      == obstack_next_free (&permanent_obstack)))
	{
	  DECL_MAIN_VARIANT (newdecl) = olddecl;
	  DECL_LANG_SPECIFIC (olddecl) = ol;
	  bcopy ((char *)nl, (char *)ol, sizeof (struct lang_decl));

	  obstack_free (&permanent_obstack, newdecl);
	}
      else if (LANG_DECL_PERMANENT (ol) && ol != nl)
	{
	  if (DECL_MAIN_VARIANT (olddecl) == olddecl)
	    {
	      /* Save these lang_decls that would otherwise be lost.  */
	      extern tree free_lang_decl_chain;
	      tree free_lang_decl = (tree) ol;

	      if (DECL_LANG_SPECIFIC (olddecl) == ol)
		abort ();

	      TREE_CHAIN (free_lang_decl) = free_lang_decl_chain;
	      free_lang_decl_chain = free_lang_decl;
	    }
	  else
	    {
	      /* Storage leak.  */;
	    }
	}
    }
  else
    {
      bcopy ((char *) newdecl + sizeof (struct tree_common),
	     (char *) olddecl + sizeof (struct tree_common),
	     sizeof (struct tree_decl) - sizeof (struct tree_common)
	     + tree_code_length [(int)TREE_CODE (newdecl)] * sizeof (char *));
    }

  DECL_UID (olddecl) = olddecl_uid;
  if (olddecl_friend)
    DECL_FRIEND_P (olddecl) = 1;

  /* NEWDECL contains the merged attribute lists.
     Update OLDDECL to be the same.  */
  DECL_MACHINE_ATTRIBUTES (olddecl) = DECL_MACHINE_ATTRIBUTES (newdecl);

  return 1;
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (x)
     tree x;
{
  register tree t;
  register tree name = DECL_ASSEMBLER_NAME (x);
  int need_new_binding = 1;

  if (DECL_TEMPLATE_PARM_P (x))
    /* Template parameters have no context; they are not X::T even
       when declared within a class or namespace.  */
    ;
  else
    {
      if (current_function_decl && x != current_function_decl
	  /* A local declaration for a function doesn't constitute
             nesting.  */
	  && (TREE_CODE (x) != FUNCTION_DECL || DECL_INITIAL (x))
	  /* Don't change DECL_CONTEXT of virtual methods.  */
	  && (TREE_CODE (x) != FUNCTION_DECL || !DECL_VIRTUAL_P (x))
	  && !DECL_CONTEXT (x))
	DECL_CONTEXT (x) = current_function_decl;
      if (!DECL_CONTEXT (x))
	DECL_CONTEXT (x) = FROB_CONTEXT (current_namespace);
    }

  /* Type are looked up using the DECL_NAME, as that is what the rest of the
     compiler wants to use.  */
  if (TREE_CODE (x) == TYPE_DECL || TREE_CODE (x) == VAR_DECL
      || TREE_CODE (x) == NAMESPACE_DECL)
    name = DECL_NAME (x);

  if (name)
    {
#if 0
      /* Not needed...see below.  */
      char *file;
      int line;
#endif
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	name = TREE_OPERAND (name, 0);
      
      /* Namespace-scoped variables are not found in the current level. */
      if (TREE_CODE (x) == VAR_DECL && DECL_NAMESPACE_SCOPE_P (x))
	t = namespace_binding (name, DECL_CONTEXT (x));
      else
	t = lookup_name_current_level (name);
      if (t == error_mark_node)
	{
	  /* error_mark_node is 0 for a while during initialization!  */
	  t = NULL_TREE;
	  cp_error_at ("`%#D' used prior to declaration", x);
	}

      else if (t != NULL_TREE)
	{
#if 0
	  /* This is turned off until I have time to do it right (bpk).  */
	  /* With the code below that uses it...  */
	  file = DECL_SOURCE_FILE (t);
	  line = DECL_SOURCE_LINE (t);
#endif
	  if (TREE_CODE (t) == PARM_DECL)
	    {
	      if (DECL_CONTEXT (t) == NULL_TREE)
		fatal ("parse errors have confused me too much");

	      /* Check for duplicate params.  */
	      if (duplicate_decls (x, t))
		return t;
	    }
	  else if (((TREE_CODE (x) == FUNCTION_DECL && DECL_LANGUAGE (x) == lang_c)
		    || DECL_FUNCTION_TEMPLATE_P (x))
		   && is_overloaded_fn (t))
	    /* Don't do anything just yet. */;
	  else if (t == wchar_decl_node)
	    {
	      if (pedantic && ! DECL_IN_SYSTEM_HEADER (x))
		cp_pedwarn ("redeclaration of wchar_t as `%T'", TREE_TYPE (x));

	      /* Throw away the redeclaration.  */
	      return t;
	    }
	  else if (TREE_CODE (t) != TREE_CODE (x))
	    {
	      if (duplicate_decls (x, t))
		return t;
	    }
	  else if (duplicate_decls (x, t))
	    {
#if 0
	      /* This is turned off until I have time to do it right (bpk).  */

	      /* Also warn if they did a prototype with `static' on it, but
		 then later left the `static' off.  */
	      if (! TREE_PUBLIC (name) && TREE_PUBLIC (x))
		{
		  if (DECL_LANG_SPECIFIC (t) && DECL_FRIEND_P (t))
		    return t;

		  if (extra_warnings)
		    {
		      cp_warning ("`static' missing from declaration of `%D'",
				  t);
		      warning_with_file_and_line (file, line,
						  "previous declaration of `%s'",
						  decl_as_string (t, 0));
		    }

		  /* Now fix things so it'll do what they expect.  */
		  if (current_function_decl)
		    TREE_PUBLIC (current_function_decl) = 0;
		}
	      /* Due to interference in memory reclamation (X may be
		 obstack-deallocated at this point), we must guard against
		 one really special case.  [jason: This should be handled
		 by start_function]  */
	      if (current_function_decl == x)
		current_function_decl = t;
#endif
	      if (TREE_CODE (t) == TYPE_DECL)
		SET_IDENTIFIER_TYPE_VALUE (name, TREE_TYPE (t));
	      else if (TREE_CODE (t) == FUNCTION_DECL)
		check_default_args (t);

	      return t;
	    }
	  else if (DECL_MAIN_P (x))
	    {
	      /* A redeclaration of main, but not a duplicate of the
		 previous one. 

		 [basic.start.main]

	         This function shall not be overloaded.  */
	      cp_error_at ("invalid redeclaration of `%D'", t);
	      cp_error ("as `%D'", x);
	      /* We don't try to push this declaration since that
		 causes a crash.  */
	      return x;
	    }
	}

      check_template_shadow (x);

      /* If this is a function conjured up by the backend, massage it
	 so it looks friendly.  */
      if (TREE_CODE (x) == FUNCTION_DECL
	  && ! DECL_LANG_SPECIFIC (x))
	{
	  retrofit_lang_decl (x);
	  DECL_LANGUAGE (x) = lang_c;
	}

      if (TREE_CODE (x) == FUNCTION_DECL && ! DECL_FUNCTION_MEMBER_P (x))
	{
	  t = push_overloaded_decl (x, PUSH_LOCAL);
	  if (t != x || DECL_LANGUAGE (x) == lang_c)
	    return t;
	  if (!namespace_bindings_p ())
	    /* We do not need to create a binding for this name;
	       push_overloaded_decl will have already done so if
	       necessary.  */
	    need_new_binding = 0;
	}
      else if (DECL_FUNCTION_TEMPLATE_P (x) && DECL_NAMESPACE_SCOPE_P (x))
	return push_overloaded_decl (x, PUSH_GLOBAL);

      /* If declaring a type as a typedef, copy the type (unless we're
	 at line 0), and install this TYPE_DECL as the new type's typedef
	 name.  See the extensive comment in ../c-decl.c (pushdecl). */
      if (TREE_CODE (x) == TYPE_DECL)
	{
	  tree type = TREE_TYPE (x);
          if (DECL_SOURCE_LINE (x) == 0)
            {
	      if (TYPE_NAME (type) == 0)
	        TYPE_NAME (type) = x;
            }
          else if (type != error_mark_node && TYPE_NAME (type) != x
		   /* We don't want to copy the type when all we're
		      doing is making a TYPE_DECL for the purposes of
		      inlining.  */
		   && (!TYPE_NAME (type) 
		       || TYPE_NAME (type) != DECL_ABSTRACT_ORIGIN (x)))
            {
	      push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));

	      DECL_ORIGINAL_TYPE (x) = type;
              type = build_type_copy (type);
	      TYPE_STUB_DECL (type) = TYPE_STUB_DECL (DECL_ORIGINAL_TYPE (x));
              TYPE_NAME (type) = x;
              TREE_TYPE (x) = type;

	      pop_obstacks ();
            }

	  if (type != error_mark_node
	      && TYPE_NAME (type)
	      && TYPE_IDENTIFIER (type))
            set_identifier_type_value_with_scope (DECL_NAME (x), type, 
						  current_binding_level);

	}

      /* Multiple external decls of the same identifier ought to match.

	 We get warnings about inline functions where they are defined.
	 We get warnings about other functions from push_overloaded_decl.
	 
	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x) && TREE_CODE (x) != FUNCTION_DECL)
	{
	  tree decl;

	  if (IDENTIFIER_NAMESPACE_VALUE (name) != NULL_TREE
	      && (DECL_EXTERNAL (IDENTIFIER_NAMESPACE_VALUE (name))
		  || TREE_PUBLIC (IDENTIFIER_NAMESPACE_VALUE (name))))
	    decl = IDENTIFIER_NAMESPACE_VALUE (name);
	  else
	    decl = NULL_TREE;

	  if (decl
	      /* If different sort of thing, we already gave an error.  */
	      && TREE_CODE (decl) == TREE_CODE (x)
	      && !same_type_p (TREE_TYPE (x), TREE_TYPE (decl)))
	    {
	      cp_pedwarn ("type mismatch with previous external decl", x);
	      cp_pedwarn_at ("previous external decl of `%#D'", decl);
	    }
	}

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (namespace_bindings_p ())
	{
	  /* Install a global value.  */

	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == NULL_TREE && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  if (!(TREE_CODE (x) == TYPE_DECL && DECL_ARTIFICIAL (x)
		&& t != NULL_TREE))
	    {
	      if (TREE_CODE (x) == FUNCTION_DECL)
		my_friendly_assert 
		  ((IDENTIFIER_GLOBAL_VALUE (name) == NULL_TREE)
		  || (IDENTIFIER_GLOBAL_VALUE (name) == x), 378);
	      SET_IDENTIFIER_NAMESPACE_VALUE (name, x);
	    }

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != NULL_TREE
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && TREE_TYPE (TREE_TYPE (x)) == integer_type_node))
	    cp_warning
	      ("`%D' was previously implicitly declared to return `int'", x);

	  /* If new decl is `static' and an `extern' was seen previously,
	     warn about it.  */
	  if (x != NULL_TREE && t != NULL_TREE && decls_match (x, t))
	    warn_extern_redeclared_static (x, t);
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_VALUE (name);
	  tree oldglobal = IDENTIFIER_NAMESPACE_VALUE (name);

	  if (need_new_binding)
	    {
	      push_local_binding (name, x, 0);
	      /* Because push_local_binding will hook X on to the
		 current_binding_level's name list, we don't want to
		 do that again below.  */
	      need_new_binding = 0;
	    }

	  /* If this is a TYPE_DECL, push it into the type value slot.  */
	  if (TREE_CODE (x) == TYPE_DECL)
	    set_identifier_type_value_with_scope (name, TREE_TYPE (x), 
						  current_binding_level);

	  /* Clear out any TYPE_DECL shadowed by a namespace so that
	     we won't think this is a type.  The C struct hack doesn't
	     go through namespaces.  */
	  if (TREE_CODE (x) == NAMESPACE_DECL)
	    set_identifier_type_value_with_scope (name, NULL_TREE, 
						  current_binding_level);

	  /* If this is an extern function declaration, see if we
	     have a global definition or declaration for the function.  */
	  if (oldlocal == NULL_TREE
	      && DECL_EXTERNAL (x)
	      && oldglobal != NULL_TREE
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL)
	    {
	      /* We have one.  Their types must agree.  */
	      if (decls_match (x, oldglobal))
		/* OK */;
	      else
		{
		  cp_warning ("extern declaration of `%#D' doesn't match", x);
		  cp_warning_at ("global declaration `%#D'", oldglobal);
		}
	    }
	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == NULL_TREE
	      && oldglobal == NULL_TREE
	      && DECL_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  if (DECL_FROM_INLINE (x))
	    /* Inline decls shadow nothing.  */;

	  /* Warn if shadowing an argument at the top level of the body.  */
	  else if (oldlocal != NULL_TREE && !DECL_EXTERNAL (x)
		   && TREE_CODE (oldlocal) == PARM_DECL
		   /* Don't complain if it's from an enclosing function.  */
		   && DECL_CONTEXT (oldlocal) == current_function_decl
		   && TREE_CODE (x) != PARM_DECL)
	    {
	      /* Go to where the parms should be and see if we
		 find them there.  */
	      struct binding_level *b = current_binding_level->level_chain;

	      if (cleanup_label)
		b = b->level_chain;

	      /* ARM $8.3 */
	      if (b->parm_flag == 1)
		cp_error ("declaration of `%#D' shadows a parameter", name);
	    }
	  else if (warn_shadow && oldlocal != NULL_TREE
		   && current_binding_level->is_for_scope
		   && !DECL_DEAD_FOR_LOCAL (oldlocal))
	    {
	      warning ("variable `%s' shadows local",
		       IDENTIFIER_POINTER (name));
	      cp_warning_at ("  this is the shadowed declaration", oldlocal);
	    }		   
	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !DECL_EXTERNAL (x)
		   /* No shadow warnings for internally generated vars.  */
		   && ! DECL_ARTIFICIAL (x)
		   /* No shadow warnings for vars made for inlining.  */
		   && ! DECL_FROM_INLINE (x))
	    {
	      const char *warnstring = NULL;

	      if (oldlocal != NULL_TREE && TREE_CODE (oldlocal) == PARM_DECL)
		warnstring = "declaration of `%s' shadows a parameter";
	      else if (IDENTIFIER_CLASS_VALUE (name) != NULL_TREE
		       && current_class_ptr
		       && !TREE_STATIC (name))
		warnstring = "declaration of `%s' shadows a member of `this'";
	      else if (oldlocal != NULL_TREE)
		warnstring = "declaration of `%s' shadows previous local";
	      else if (oldglobal != NULL_TREE)
		/* XXX shadow warnings in outer-more namespaces */
		warnstring = "declaration of `%s' shadows global declaration";

	      if (warnstring)
		warning (warnstring, IDENTIFIER_POINTER (name));
	    }
	}

      if (TREE_CODE (x) == FUNCTION_DECL)
	check_default_args (x);

      /* Keep count of variables in this level with incomplete type.  */
      if (TREE_CODE (x) == VAR_DECL
	  && TREE_TYPE (x) != error_mark_node
	  && ((TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	       && PROMOTES_TO_AGGR_TYPE (TREE_TYPE (x), ARRAY_TYPE))
	      /* RTTI TD entries are created while defining the type_info.  */
	      || (TYPE_LANG_SPECIFIC (TREE_TYPE (x))
		  && TYPE_BEING_DEFINED (TREE_TYPE (x)))))
	current_binding_level->incomplete 
	  = tree_cons (NULL_TREE, x, current_binding_level->incomplete);
    }

  if (need_new_binding)
    {
      /* Put decls on list in reverse order.
	 We will reverse them later if necessary.  */
      TREE_CHAIN (x) = current_binding_level->names;
      current_binding_level->names = x;
      if (current_binding_level == global_binding_level
	  && !TREE_PERMANENT (x))
	my_friendly_abort (124);
    }

  return x;
}

/* Same as pushdecl, but define X in binding-level LEVEL.  We rely on the
   caller to set DECL_CONTEXT properly.  */

static tree
pushdecl_with_scope (x, level)
     tree x;
     struct binding_level *level;
{
  register struct binding_level *b;
  tree function_decl = current_function_decl;

  current_function_decl = NULL_TREE;
  if (level->parm_flag == 2)
    {
      b = class_binding_level;
      class_binding_level = level;
      pushdecl_class_level (x);
      class_binding_level = b;
    }
  else
    {
      b = current_binding_level;
      current_binding_level = level;
      x = pushdecl (x);
      current_binding_level = b;
    }
  current_function_decl = function_decl;
  return x;
}

/* Like pushdecl, only it places X in the current namespace,
   if appropriate.  */

tree
pushdecl_namespace_level (x)
     tree x;
{
  register struct binding_level *b = current_binding_level;
  register tree t;

  t = pushdecl_with_scope (x, NAMESPACE_LEVEL (current_namespace));

  /* Now, the type_shadowed stack may screw us.  Munge it so it does
     what we want.  */
  if (TREE_CODE (x) == TYPE_DECL)
    {
      tree name = DECL_NAME (x);
      tree newval;
      tree *ptr = (tree *)0;
      for (; b != global_binding_level; b = b->level_chain)
        {
          tree shadowed = b->type_shadowed;
          for (; shadowed; shadowed = TREE_CHAIN (shadowed))
            if (TREE_PURPOSE (shadowed) == name)
              {
		ptr = &TREE_VALUE (shadowed);
		/* Can't break out of the loop here because sometimes
		   a binding level will have duplicate bindings for
		   PT names.  It's gross, but I haven't time to fix it.  */
              }
        }
      newval = TREE_TYPE (x);
      if (ptr == (tree *)0)
        {
          /* @@ This shouldn't be needed.  My test case "zstring.cc" trips
             up here if this is changed to an assertion.  --KR  */
	  SET_IDENTIFIER_TYPE_VALUE (name, newval);
	}
      else
        {
	  *ptr = newval;
        }
    }
  return t;
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL,
   if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  tree cur_namespace = current_namespace;
  current_namespace = global_namespace;
  x = pushdecl_namespace_level (x);
  current_namespace = cur_namespace;
  return x;
}

/* Make the declaration of X appear in CLASS scope.  */

void
pushdecl_class_level (x)
     tree x;
{
  /* Don't use DECL_ASSEMBLER_NAME here!  Everything that looks in class
     scope looks for the pre-mangled name.  */
  register tree name;

  if (TREE_CODE (x) == OVERLOAD)
    x = OVL_CURRENT (x);
  name = DECL_NAME (x);

  if (name)
    {
      push_class_level_binding (name, x);
      if (TREE_CODE (x) == TYPE_DECL)
	set_identifier_type_value (name, TREE_TYPE (x));
    }
  else if (ANON_UNION_TYPE_P (TREE_TYPE (x)))
    {
      tree f;

      for (f = TYPE_FIELDS (TREE_TYPE (x));
	   f;
	   f = TREE_CHAIN (f))
	pushdecl_class_level (f);
    }
}

#if 0
/* This function is used to push the mangled decls for nested types into
   the appropriate scope.  Previously pushdecl_top_level was used, but that
   is incorrect for members of local classes.  */

void
pushdecl_nonclass_level (x)
     tree x;
{
  struct binding_level *b = current_binding_level;

  my_friendly_assert (b->parm_flag != 2, 180);

#if 0
  /* Get out of template binding levels */
  while (b->pseudo_global)
    b = b->level_chain;
#endif

  pushdecl_with_scope (x, b);
}
#endif

/* Make the declaration(s) of X appear in CLASS scope
   under the name NAME.  */

void
push_class_level_binding (name, x)
     tree name;
     tree x;
{
  tree binding;
  /* The class_binding_level will be NULL if x is a template 
     parameter name in a member template.  */
  if (!class_binding_level)
    return;

  /* Make sure that this new member does not have the same name
     as a template parameter.  */
  if (TYPE_BEING_DEFINED (current_class_type))
    check_template_shadow (x);

  /* If this declaration shadows a declaration from an enclosing
     class, then we will need to restore IDENTIFIER_CLASS_VALUE when
     we leave this class.  Record the shadowed declaration here.  */
  binding = IDENTIFIER_BINDING (name);
  if (binding 
      && ((TREE_CODE (x) == OVERLOAD
	   && BINDING_VALUE (binding)
	   && is_overloaded_fn (BINDING_VALUE (binding)))
	  || INHERITED_VALUE_BINDING_P (binding)))
    {
      tree shadow;
      tree old_decl;

      /* If the old binding was from a base class, and was for a tag
	 name, slide it over to make room for the new binding.  The
	 old binding is still visible if explicitly qualified with a
	 class-key.  */
      if (INHERITED_VALUE_BINDING_P (binding)
	  && BINDING_VALUE (binding)
	  && TREE_CODE (BINDING_VALUE (binding)) == TYPE_DECL
	  && DECL_ARTIFICIAL (BINDING_VALUE (binding))
	  && !(TREE_CODE (x) == TYPE_DECL && DECL_ARTIFICIAL (x)))
	{
	  old_decl = BINDING_TYPE (binding);
	  BINDING_TYPE (binding) = BINDING_VALUE (binding);
	  BINDING_VALUE (binding) = NULL_TREE;
	  INHERITED_VALUE_BINDING_P (binding) = 0;
	}
      else
	old_decl = BINDING_VALUE (binding);

      /* There was already a binding for X containing fewer
	 functions than are named in X.  Find the previous
	 declaration of X on the class-shadowed list, and update it.  */
      for (shadow = class_binding_level->class_shadowed;
	   shadow;
	   shadow = TREE_CHAIN (shadow))
	if (TREE_PURPOSE (shadow) == name
	    && TREE_TYPE (shadow) == old_decl)
	  {
	    BINDING_VALUE (binding) = x;
	    INHERITED_VALUE_BINDING_P (binding) = 0;
	    TREE_TYPE (shadow) = x;
	    return;
	  }
    }

  /* If we didn't replace an existing binding, put the binding on the
     stack of bindings for the identifier, and update
     IDENTIFIER_CLASS_VALUE.  */
  if (push_class_binding (name, x))
    {
      push_cache_obstack ();
      class_binding_level->class_shadowed
	= tree_cons (name, IDENTIFIER_CLASS_VALUE (name),
		     class_binding_level->class_shadowed);
      pop_obstacks ();
      /* Record the value we are binding NAME to so that we can know
	 what to pop later.  */
      TREE_TYPE (class_binding_level->class_shadowed) = x;
    }
}

/* Insert another USING_DECL into the current binding level,
   returning this declaration. If this is a redeclaration,
   do nothing and return NULL_TREE.  */

tree
push_using_decl (scope, name)
     tree scope;
     tree name;
{
  tree decl;
  
  my_friendly_assert (TREE_CODE (scope) == NAMESPACE_DECL, 383);
  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 384);
  for (decl = current_binding_level->usings; decl; decl = TREE_CHAIN (decl))
    if (DECL_INITIAL (decl) == scope && DECL_NAME (decl) == name)
      break;
  if (decl)
    return NULL_TREE;
  decl = build_lang_decl (USING_DECL, name, void_type_node);
  DECL_INITIAL (decl) = scope;
  TREE_CHAIN (decl) = current_binding_level->usings;
  current_binding_level->usings = decl;
  return decl;
}

/* Add namespace to using_directives. Return NULL_TREE if nothing was
   changed (i.e. there was already a directive), or the fresh
   TREE_LIST otherwise.  */

tree
push_using_directive (used)
     tree used;
{
  tree ud = current_binding_level->using_directives;
  tree iter, ancestor;
  
  /* Check if we already have this. */
  if (purpose_member (used, ud) != NULL_TREE)
    return NULL_TREE;

  /* Recursively add all namespaces used. */
  for (iter = DECL_NAMESPACE_USING (used); iter; iter = TREE_CHAIN (iter))
    push_using_directive (TREE_PURPOSE (iter));

  ancestor = namespace_ancestor (current_decl_namespace (), used);
  ud = current_binding_level->using_directives;
  ud = perm_tree_cons (used, ancestor, ud);
  current_binding_level->using_directives = ud;
  return ud;
}

/* DECL is a FUNCTION_DECL for a non-member function, which may have
   other definitions already in place.  We get around this by making
   the value of the identifier point to a list of all the things that
   want to be referenced by that name.  It is then up to the users of
   that name to decide what to do with that list.

   DECL may also be a TEMPLATE_DECL, with a FUNCTION_DECL in its DECL_RESULT
   slot.  It is dealt with the same way.

   FLAGS is a bitwise-or of the following values:
     PUSH_LOCAL: Bind DECL in the current scope, rather than at
                 namespace scope.
     PUSH_USING: DECL is being pushed as the result of a using
                 declaration. 

   The value returned may be a previous declaration if we guessed wrong
   about what language DECL should belong to (C or C++).  Otherwise,
   it's always DECL (and never something that's not a _DECL).  */

tree
push_overloaded_decl (decl, flags)
     tree decl;
     int flags;
{
  tree name = DECL_NAME (decl);
  tree old;
  tree new_binding;
  int doing_global = (namespace_bindings_p () || !(flags & PUSH_LOCAL));

  if (doing_global)
    {
      old = namespace_binding (name, DECL_CONTEXT (decl));
      if (old && TREE_CODE (old) == FUNCTION_DECL
	  && DECL_ARTIFICIAL (old)
	  && (DECL_BUILT_IN (old) || DECL_BUILT_IN_NONANSI (old)))
	{
	  if (duplicate_decls (decl, old))
	    return old;
	  old = NULL_TREE;
	}
    }
  else
    old = lookup_name_current_level (name);

  if (old)
    {
      if (TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
	{
	  tree t = TREE_TYPE (old);
	  if (IS_AGGR_TYPE (t) && warn_shadow
	      && (! DECL_IN_SYSTEM_HEADER (decl)
		  || ! DECL_IN_SYSTEM_HEADER (old)))
	    cp_warning ("`%#D' hides constructor for `%#T'", decl, t);
	  old = NULL_TREE;
	}
      else if (is_overloaded_fn (old))
        {
          tree tmp;
	  
	  for (tmp = old; tmp; tmp = OVL_NEXT (tmp))
	    {
	      tree fn = OVL_CURRENT (tmp);

	      if (TREE_CODE (tmp) == OVERLOAD && OVL_USED (tmp)
		  && !(flags & PUSH_USING)
		  && compparms (TYPE_ARG_TYPES (TREE_TYPE (fn)),
				TYPE_ARG_TYPES (TREE_TYPE (decl))))
		cp_error ("`%#D' conflicts with previous using declaration `%#D'",
			  decl, fn);
	      
	      if (duplicate_decls (decl, fn))
		return fn;
	    }
	}
      else
	{
	  cp_error_at ("previous non-function declaration `%#D'", old);
	  cp_error ("conflicts with function declaration `%#D'", decl);
	  return decl;
	}
    }

  if (old || TREE_CODE (decl) == TEMPLATE_DECL)
    {
      if (old && TREE_CODE (old) != OVERLOAD)
	new_binding = ovl_cons (decl, ovl_cons (old, NULL_TREE));
      else
	new_binding = ovl_cons (decl, old);
      if (flags & PUSH_USING)
	OVL_USED (new_binding) = 1;
    }
  else
    /* NAME is not ambiguous.  */
    new_binding = decl;

  if (doing_global)
    set_namespace_binding (name, current_namespace, new_binding);
  else
    {
      /* We only create an OVERLOAD if there was a previous binding at
	 this level, or if decl is a template. In the former case, we
	 need to remove the old binding and replace it with the new
	 binding.  We must also run through the NAMES on the binding
	 level where the name was bound to update the chain.  */

      if (TREE_CODE (new_binding) == OVERLOAD && old)
	{
	  tree *d;
	  
	  for (d = &BINDING_LEVEL (IDENTIFIER_BINDING (name))->names;
	       *d;
	       d = &TREE_CHAIN (*d))
	    if (*d == old
		|| (TREE_CODE (*d) == TREE_LIST
		    && TREE_VALUE (*d) == old))
	      {
		if (TREE_CODE (*d) == TREE_LIST)
		  /* Just replace the old binding with the new.  */
		  TREE_VALUE (*d) = new_binding;
		else
		  /* Build a TREE_LIST to wrap the OVERLOAD.  */
		  *d = build_tree_list (NULL_TREE, new_binding);

		/* And update the CPLUS_BINDING node.  */
		BINDING_VALUE (IDENTIFIER_BINDING (name))
		  = new_binding;
		return decl;
	      }

	  /* We should always find a previous binding in this case.  */
	  my_friendly_abort (0);
	}

      /* Install the new binding.  */
      push_local_binding (name, new_binding, flags);
    }

  return decl;
}

/* Generate an implicit declaration for identifier FUNCTIONID
   as a function of type int ().  Print a warning if appropriate.  */

tree
implicitly_declare (functionid)
     tree functionid;
{
  register tree decl;
  int temp = allocation_temporary_p ();

  push_obstacks_nochange ();

  /* Save the decl permanently so we can warn if definition follows.
     In ANSI C, warn_implicit is usually false, so the saves little space.
     But in C++, it's usually true, hence the extra code.  */
  if (temp && (! warn_implicit || toplevel_bindings_p ()))
    end_temporary_allocation ();

  /* We used to reuse an old implicit decl here,
     but this loses with inline functions because it can clobber
     the saved decl chains.  */
  decl = build_lang_decl (FUNCTION_DECL, functionid, default_function_type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* ANSI standard says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.  */
  pushdecl (decl);
  rest_of_decl_compilation (decl, NULL_PTR, 0, 0);

  if (warn_implicit
      /* Only one warning per identifier.  */
      && IDENTIFIER_IMPLICIT_DECL (functionid) == NULL_TREE)
    {
      cp_pedwarn ("implicit declaration of function `%#D'", decl);
    }

  SET_IDENTIFIER_IMPLICIT_DECL (functionid, decl);

  pop_obstacks ();

  return decl;
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return an error message format string with a %s
   where the identifier should go.  */

static const char *
redeclaration_error_message (newdecl, olddecl)
     tree newdecl, olddecl;
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      /* Because C++ can put things into name space for free,
	 constructs like "typedef struct foo { ... } foo"
	 would look like an erroneous redeclaration.  */
      if (same_type_p (TREE_TYPE (newdecl), TREE_TYPE (olddecl)))
	return 0;
      else
	return "redefinition of `%#D'";
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If this is a pure function, its olddecl will actually be
	 the original initialization to `0' (which we force to call
	 abort()).  Don't complain about redefinition in this case.  */
      if (DECL_LANG_SPECIFIC (olddecl) && DECL_ABSTRACT_VIRTUAL_P (olddecl))
	return 0;

      /* If both functions come from different namespaces, this is not
	 a redeclaration - this is a conflict with a used function. */
      if (DECL_NAMESPACE_SCOPE_P (olddecl)
	  && DECL_CONTEXT (olddecl) != DECL_CONTEXT (newdecl))
	return "`%D' conflicts with used function";

      /* We'll complain about linkage mismatches in
         warn_extern_redeclared_static.  */

      /* Defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != NULL_TREE
	  && DECL_INITIAL (newdecl) != NULL_TREE)
	{
	  if (DECL_NAME (olddecl) == NULL_TREE)
	    return "`%#D' not declared in class";
	  else
	    return "redefinition of `%#D'";
	}
      return 0;
    }
  else if (TREE_CODE (newdecl) == TEMPLATE_DECL)
    {
      if ((TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == FUNCTION_DECL
	   && DECL_INITIAL (DECL_TEMPLATE_RESULT (newdecl))
	   && DECL_INITIAL (DECL_TEMPLATE_RESULT (olddecl)))
	  || (TREE_CODE (DECL_TEMPLATE_RESULT (newdecl)) == TYPE_DECL
	      && TYPE_SIZE (TREE_TYPE (newdecl))
	      && TYPE_SIZE (TREE_TYPE (olddecl))))
	return "redefinition of `%#D'";
      return 0;
    }
  else if (toplevel_bindings_p ())
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (DECL_EXTERNAL (newdecl) || DECL_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      return "redefinition of `%#D'";
    }
  else
    {
      /* Objects declared with block scope:  */
      /* Reject two definitions, and reject a definition
	 together with an external reference.  */
      if (!(DECL_EXTERNAL (newdecl) && DECL_EXTERNAL (olddecl)))
	return "redeclaration of `%#D'";
      return 0;
    }
}

/* Get the LABEL_DECL corresponding to identifier ID as a label.
   Create one if none exists so far for the current function.
   This function is called for both label definitions and label references.  */

tree
lookup_label (id)
     tree id;
{
  register tree decl = IDENTIFIER_LABEL_VALUE (id);

  if (current_function_decl == NULL_TREE)
    {
      error ("label `%s' referenced outside of any function",
	     IDENTIFIER_POINTER (id));
      return NULL_TREE;
    }

  if ((decl == NULL_TREE
      || DECL_SOURCE_LINE (decl) == 0)
      && (named_label_uses == NULL
	  || named_label_uses->names_in_scope != current_binding_level->names
	  || named_label_uses->label_decl != decl))
    {
      struct named_label_list *new_ent;
      new_ent
	= (struct named_label_list*)oballoc (sizeof (struct named_label_list));
      new_ent->label_decl = decl;
      new_ent->names_in_scope = current_binding_level->names;
      new_ent->binding_level = current_binding_level;
      new_ent->lineno_o_goto = lineno;
      new_ent->filename_o_goto = input_filename;
      new_ent->next = named_label_uses;
      named_label_uses = new_ent;
    }

  /* Use a label already defined or ref'd with this name.  */
  if (decl != NULL_TREE)
    {
      /* But not if it is inherited and wasn't declared to be inheritable.  */
      if (DECL_CONTEXT (decl) != current_function_decl
	  && ! C_DECLARED_LABEL_FLAG (decl))
	return shadow_label (id);
      return decl;
    }

  decl = build_decl (LABEL_DECL, id, void_type_node);

  /* Make sure every label has an rtx.  */
  label_rtx (decl);

  /* A label not explicitly declared must be local to where it's ref'd.  */
  DECL_CONTEXT (decl) = current_function_decl;

  DECL_MODE (decl) = VOIDmode;

  /* Say where one reference is to the label,
     for the sake of the error if it is not defined.  */
  DECL_SOURCE_LINE (decl) = lineno;
  DECL_SOURCE_FILE (decl) = input_filename;

  SET_IDENTIFIER_LABEL_VALUE (id, decl);

  named_labels = tree_cons (NULL_TREE, decl, named_labels);
  named_label_uses->label_decl = decl;

  return decl;
}

/* Make a label named NAME in the current function,
   shadowing silently any that may be inherited from containing functions
   or containing scopes.

   Note that valid use, if the label being shadowed
   comes from another scope in the same function,
   requires calling declare_nonlocal_label right away.  */

tree
shadow_label (name)
     tree name;
{
  register tree decl = IDENTIFIER_LABEL_VALUE (name);

  if (decl != NULL_TREE)
    {
      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      SET_IDENTIFIER_LABEL_VALUE (name, NULL_TREE);
    }

  return lookup_label (name);
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     char *filename;
     int line;
     tree name;
{
  tree decl;

  if (minimal_parse_mode)
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      decl = build_decl (LABEL_DECL, name, void_type_node);
      pop_obstacks ();
      DECL_SOURCE_LINE (decl) = line;
      DECL_SOURCE_FILE (decl) = filename;
      add_tree (decl);
      return decl;
    }

  decl = lookup_label (name);

  /* After labels, make any new cleanups go into their
     own new (temporary) binding contour.  */
  current_binding_level->more_cleanups_ok = 0;

  /* If label with this name is known from an outer context, shadow it.  */
  if (decl != NULL_TREE && DECL_CONTEXT (decl) != current_function_decl)
    {
      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      SET_IDENTIFIER_LABEL_VALUE (name, NULL_TREE);
      decl = lookup_label (name);
    }

  if (name == get_identifier ("wchar_t"))
    cp_pedwarn ("label named wchar_t");

  if (DECL_INITIAL (decl) != NULL_TREE)
    {
      cp_error ("duplicate label `%D'", decl);
      return 0;
    }
  else
    {
      struct named_label_list *uses, *prev;
      int identified = 0;

      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;
      /* Say where in the source.  */
      DECL_SOURCE_FILE (decl) = filename;
      DECL_SOURCE_LINE (decl) = line;

      prev = NULL;
      uses = named_label_uses;
      while (uses != NULL)
	if (uses->label_decl == decl)
	  {
	    struct binding_level *b = current_binding_level;
	    while (b)
	      {
		tree new_decls = b->names;
		tree old_decls = (b == uses->binding_level)
				  ? uses->names_in_scope : NULL_TREE;
		while (new_decls != old_decls)
		  {
		    if (TREE_CODE (new_decls) == VAR_DECL
			/* Don't complain about crossing initialization
			   of internal entities.  They can't be accessed,
			   and they should be cleaned up
			   by the time we get to the label.  */
			&& ! DECL_ARTIFICIAL (new_decls)
			&& !(DECL_INITIAL (new_decls) == NULL_TREE
			     && pod_type_p (TREE_TYPE (new_decls))))
		      {
			/* This is really only important if we're crossing
			   an initialization.  The POD stuff is just
			   pedantry; why should it matter if the class
			   contains a field of pointer to member type?  */
			int problem = (DECL_INITIAL (new_decls)
				       || (TYPE_NEEDS_CONSTRUCTING
					   (TREE_TYPE (new_decls))));

			if (! identified)
			  {
			    if (problem)
			      {
				cp_error ("jump to label `%D'", decl);
				error_with_file_and_line
				  (uses->filename_o_goto,
				   uses->lineno_o_goto, "  from here");
			      }
			    else
			      {
				cp_pedwarn ("jump to label `%D'", decl);
				pedwarn_with_file_and_line
				  (uses->filename_o_goto,
				   uses->lineno_o_goto, "  from here");
			      }
			    identified = 1;
			}

			if (problem)
			  cp_error_at ("  crosses initialization of `%#D'",
				       new_decls);
			else
			  cp_pedwarn_at ("  enters scope of non-POD `%#D'",
					 new_decls);
		      }
		    new_decls = TREE_CHAIN (new_decls);
		  }
		if (b == uses->binding_level)
		  break;
		b = b->level_chain;
	      }

	    if (prev != NULL)
	      prev->next = uses->next;
	    else
	      named_label_uses = uses->next;

	    uses = uses->next;
	  }
	else
	  {
	    prev = uses;
	    uses = uses->next;
	  }
      current_function_return_value = NULL_TREE;
      return decl;
    }
}

struct cp_switch
{
  struct binding_level *level;
  struct cp_switch *next;
};

static struct cp_switch *switch_stack;

void
push_switch ()
{
  struct cp_switch *p
    = (struct cp_switch *) oballoc (sizeof (struct cp_switch));
  p->level = current_binding_level;
  p->next = switch_stack;
  switch_stack = p;
}

void
pop_switch ()
{
  switch_stack = switch_stack->next;
}

/* Same, but for CASE labels.  If DECL is NULL_TREE, it's the default.  */
/* XXX Note decl is never actually used. (bpk) */

void
define_case_label ()
{
  tree cleanup = last_cleanup_this_contour ();
  struct binding_level *b = current_binding_level;
  int identified = 0;

  if (cleanup)
    {
      static int explained = 0;
      cp_warning_at ("destructor needed for `%#D'", TREE_PURPOSE (cleanup));
      warning ("where case label appears here");
      if (!explained)
	{
	  warning ("(enclose actions of previous case statements requiring");
	  warning ("destructors in their own binding contours.)");
	  explained = 1;
	}
    }

  for (; b && b != switch_stack->level; b = b->level_chain)
    {
      tree new_decls = b->names;
      for (; new_decls; new_decls = TREE_CHAIN (new_decls))
	{
	  if (TREE_CODE (new_decls) == VAR_DECL
	      /* Don't complain about crossing initialization
		 of internal entities.  They can't be accessed,
		 and they should be cleaned up
		 by the time we get to the label.  */
	      && ! DECL_ARTIFICIAL (new_decls)
	      && ((DECL_INITIAL (new_decls) != NULL_TREE
		   && DECL_INITIAL (new_decls) != error_mark_node)
		  || TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (new_decls))))
	    {
	      if (! identified)
		error ("jump to case label");
	      identified = 1;
	      cp_error_at ("  crosses initialization of `%#D'",
			   new_decls);
	    }
	}
    }

  /* After labels, make any new cleanups go into their
     own new (temporary) binding contour.  */

  current_binding_level->more_cleanups_ok = 0;
  current_function_return_value = NULL_TREE;
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Return the list of type-tags (for structs, etc) of the current level.  */

tree
gettags ()
{
  return current_binding_level->tags;
}

/* Store the list of declarations of the current level.
   This is done for the parameter declarations of a function being defined,
   after they are modified in the light of any missing parameters.  */

static void
storedecls (decls)
     tree decls;
{
  current_binding_level->names = decls;
}

/* Similarly, store the list of tags of the current level.  */

void
storetags (tags)
     tree tags;
{
  current_binding_level->tags = tags;
}

/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   Searches binding levels from BINDING_LEVEL up to the global level.
   If THISLEVEL_ONLY is nonzero, searches only the specified context
   (but skips any tag-transparent contexts to find one that is
   meaningful for tags).
   FORM says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, and it's not a template, an error is
   reported.  */

static tree
lookup_tag (form, name, binding_level, thislevel_only)
     enum tree_code form;
     tree name;
     struct binding_level *binding_level;
     int thislevel_only;
{
  register struct binding_level *level;
  /* Non-zero if, we should look past a pseudo-global level, even if
     THISLEVEL_ONLY.  */
  int allow_pseudo_global = 1;

  for (level = binding_level; level; level = level->level_chain)
    {
      register tree tail;
      if (ANON_AGGRNAME_P (name))
	for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	  {
	    /* There's no need for error checking here, because
	       anon names are unique throughout the compilation.  */
	    if (TYPE_IDENTIFIER (TREE_VALUE (tail)) == name)
	      return TREE_VALUE (tail);
	  }
      else if (level->namespace_p)
	/* Do namespace lookup. */
	for (tail = current_namespace; 1; tail = CP_DECL_CONTEXT (tail))
	  {
	    tree old = binding_for_name (name, tail);

	    /* If we just skipped past a pseudo global level, even
	       though THISLEVEL_ONLY, and we find a template class
	       declaration, then we use the _TYPE node for the
	       template.  See the example below.  */
	    if (thislevel_only && !allow_pseudo_global
		&& old && BINDING_VALUE (old) 
		&& DECL_CLASS_TEMPLATE_P (BINDING_VALUE (old)))
	      old = TREE_TYPE (BINDING_VALUE (old));
	    else 
	      old = BINDING_TYPE (old);

	    /* If it has an original type, it is a typedef, and we
	       should not return it.  */
	    if (old && DECL_ORIGINAL_TYPE (TYPE_NAME (old)))
	      old = NULL_TREE;
	    if (old && TREE_CODE (old) != form
		&& !(form != ENUMERAL_TYPE && TREE_CODE (old) == TEMPLATE_DECL))
	      {
		cp_error ("`%#D' redeclared as %C", old, form);
		return NULL_TREE;
	      }
	    if (old)
	      return old;
	    if (thislevel_only || tail == global_namespace)
	      return NULL_TREE;
	  }
      else
	for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	  {
	    if (TREE_PURPOSE (tail) == name)
	      {
		enum tree_code code = TREE_CODE (TREE_VALUE (tail));
		/* Should tighten this up; it'll probably permit
		   UNION_TYPE and a struct template, for example.  */
		if (code != form
		    && !(form != ENUMERAL_TYPE && code == TEMPLATE_DECL))
		  {
		    /* Definition isn't the kind we were looking for.  */
		    cp_error ("`%#D' redeclared as %C", TREE_VALUE (tail),
			      form);
		    return NULL_TREE;
		  }
		return TREE_VALUE (tail);
	      }
	  }
      if (thislevel_only && ! level->tag_transparent)
	{
	  if (level->pseudo_global && allow_pseudo_global)
	    {
	      /* We must deal with cases like this:
		 
	           template <class T> struct S;
		   template <class T> struct S {};
		   
		 When looking up `S', for the second declaration, we
		 would like to find the first declaration.  But, we
		 are in the pseudo-global level created for the
		 template parameters, rather than the (surrounding)
		 namespace level.  Thus, we keep going one more level,
		 even though THISLEVEL_ONLY is non-zero.  */
	      allow_pseudo_global = 0;
	      continue;
	    }
	  else
	    return NULL_TREE;
	}
      if (current_class_type && level->level_chain->namespace_p)
	{
	  /* Try looking in this class's tags before heading into
	     global binding level.  */
	  tree context = current_class_type;
	  while (context)
	    {
	      switch (TREE_CODE_CLASS (TREE_CODE (context)))
		{
		tree these_tags;
		case 't':
		    these_tags = CLASSTYPE_TAGS (context);
		    if (ANON_AGGRNAME_P (name))
		      while (these_tags)
			{
			  if (TYPE_IDENTIFIER (TREE_VALUE (these_tags))
			      == name)
			    return TREE_VALUE (tail);
			  these_tags = TREE_CHAIN (these_tags);
			}
		    else
		      while (these_tags)
			{
			  if (TREE_PURPOSE (these_tags) == name)
			    {
			      if (TREE_CODE (TREE_VALUE (these_tags)) != form)
				{
				  cp_error ("`%#D' redeclared as %C in class scope",
					    TREE_VALUE (tail), form);
				  return NULL_TREE;
				}
			      return TREE_VALUE (tail);
			    }
			  these_tags = TREE_CHAIN (these_tags);
			}
		    /* If this type is not yet complete, then don't
		       look at its context.  */
		    if (TYPE_SIZE (context) == NULL_TREE)
		      goto no_context;
		    /* Go to next enclosing type, if any.  */
		    context = DECL_CONTEXT (TYPE_MAIN_DECL (context));
		    break;
	        case 'd':
		    context = DECL_CONTEXT (context);
		    break;
	        default:
		    my_friendly_abort (10);
		}
	      continue;
	      no_context:
	      break;
	    }
	}
    }
  return NULL_TREE;
}

#if 0
void
set_current_level_tags_transparency (tags_transparent)
     int tags_transparent;
{
  current_binding_level->tag_transparent = tags_transparent;
}
#endif

/* Given a type, find the tag that was defined for it and return the tag name.
   Otherwise return 0.  However, the value can never be 0
   in the cases in which this is used.

   C++: If NAME is non-zero, this is the new name to install.  This is
   done when replacing anonymous tags with real tag names.  */

static tree
lookup_tag_reverse (type, name)
     tree type;
     tree name;
{
  register struct binding_level *level;

  for (level = current_binding_level; level; level = level->level_chain)
    {
      register tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_VALUE (tail) == type)
	    {
	      if (name)
		TREE_PURPOSE (tail) = name;
	      return TREE_PURPOSE (tail);
	    }
	}
    }
  return NULL_TREE;
}

/* Lookup TYPE in CONTEXT (a chain of nested types or a FUNCTION_DECL).
   Return the type value, or NULL_TREE if not found.  */

static tree
lookup_nested_type (type, context)
     tree type;
     tree context;
{
  if (context == NULL_TREE)
    return NULL_TREE;
  while (context)
    {
      switch (TREE_CODE (context))
	{
	case TYPE_DECL:
	  {
	    tree ctype = TREE_TYPE (context);
	    tree match = value_member (type, CLASSTYPE_TAGS (ctype));
	    if (match)
	      return TREE_VALUE (match);
	    context = DECL_CONTEXT (context);

	    /* When we have a nested class whose member functions have
	       local types (e.g., a set of enums), we'll arrive here
	       with the DECL_CONTEXT as the actual RECORD_TYPE node for
	       the enclosing class.  Instead, we want to make sure we
	       come back in here with the TYPE_DECL, not the RECORD_TYPE.  */
	    if (context && TREE_CODE (context) == RECORD_TYPE)
	      context = TREE_CHAIN (context);
	  }
	  break;
	case FUNCTION_DECL:
	  if (TYPE_NAME (type) && TYPE_IDENTIFIER (type))
	    return lookup_name (TYPE_IDENTIFIER (type), 1);
	  return NULL_TREE;
	default:
	  my_friendly_abort (12);
	}
    }
  return NULL_TREE;
}

/* Look up NAME in the NAMESPACE.  */

tree
lookup_namespace_name (namespace, name)
     tree namespace, name;
{
  struct tree_binding _b;
  tree val;

  my_friendly_assert (TREE_CODE (namespace) == NAMESPACE_DECL, 370);

  if (TREE_CODE (name) == NAMESPACE_DECL)
    /* This happens for A::B<int> when B is a namespace. */
    return name;
  else if (TREE_CODE (name) == TEMPLATE_DECL)
    {
      /* This happens for A::B where B is a template, and there are no
	 template arguments.  */
      cp_error ("invalid use of `%D'", name);
      return error_mark_node;
    }

  namespace = ORIGINAL_NAMESPACE (namespace);

  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 373);
  
  val = binding_init (&_b);
  if (!qualified_lookup_using_namespace (name, namespace, val, 0))
    return error_mark_node;

  if (BINDING_VALUE (val))
    {
      val = BINDING_VALUE (val);

      /* If we have a single function from a using decl, pull it out.  */
      if (TREE_CODE (val) == OVERLOAD && ! really_overloaded_fn (val))
	val = OVL_FUNCTION (val);
      return val;
    }

  cp_error ("`%D' undeclared in namespace `%D'", name, namespace);
  return error_mark_node;
}

/* Hash a TYPENAME_TYPE.  K is really of type `tree'.  */

static unsigned long
typename_hash (k)
     hash_table_key k;
{
  unsigned long hash;
  tree t;

  t = (tree) k;
  hash = (((unsigned long) TYPE_CONTEXT (t))
	  ^ ((unsigned long) DECL_NAME (TYPE_NAME (t))));

  return hash;
}

/* Compare two TYPENAME_TYPEs.  K1 and K2 are really of type `tree'.  */

static boolean
typename_compare (k1, k2)
     hash_table_key k1;
     hash_table_key k2;
{
  tree t1;
  tree t2;
  tree d1;
  tree d2;

  t1 = (tree) k1;
  t2 = (tree) k2;
  d1 = TYPE_NAME (t1);
  d2 = TYPE_NAME (t2);
  
  return (DECL_NAME (d1) == DECL_NAME (d2)
	  && same_type_p (TYPE_CONTEXT (t1), TYPE_CONTEXT (t2))
	  && ((TREE_TYPE (t1) != NULL_TREE) 
	      == (TREE_TYPE (t2) != NULL_TREE))
	  && same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	  && TYPENAME_TYPE_FULLNAME (t1) == TYPENAME_TYPE_FULLNAME (t2));
}

/* Build a TYPENAME_TYPE.  If the type is `typename T::t', CONTEXT is
   the type of `T', NAME is the IDENTIFIER_NODE for `t'.  If BASE_TYPE
   is non-NULL, this type is being created by the implicit typename
   extension, and BASE_TYPE is a type named `t' in some base class of
   `T' which depends on template parameters.  

   Returns the new TYPENAME_TYPE.  */

tree
build_typename_type (context, name, fullname, base_type)
     tree context;
     tree name;
     tree fullname;
     tree base_type;
{
  tree t;
  tree d;
  struct hash_entry* e;

  static struct hash_table ht;

  push_obstacks (&permanent_obstack, &permanent_obstack);

  if (!ht.table
      && !hash_table_init (&ht, &hash_newfunc, &typename_hash, 
			   &typename_compare))
    fatal ("virtual memory exhausted");

  /* The FULLNAME needs to exist for the life of the hash table, i.e.,
     for the entire compilation.  */
  if (!TREE_PERMANENT (fullname))
    fullname = copy_to_permanent (fullname);

  /* Build the TYPENAME_TYPE.  */
  t = make_lang_type (TYPENAME_TYPE);
  TYPE_CONTEXT (t) = FROB_CONTEXT (context);
  TYPENAME_TYPE_FULLNAME (t) = fullname;
  TREE_TYPE (t) = base_type;

  /* Build the corresponding TYPE_DECL.  */
  d = build_decl (TYPE_DECL, name, t);
  TYPE_NAME (TREE_TYPE (d)) = d;
  TYPE_STUB_DECL (TREE_TYPE (d)) = d;
  DECL_CONTEXT (d) = FROB_CONTEXT (context);
  DECL_ARTIFICIAL (d) = 1;

  /* See if we already have this type.  */
  e = hash_lookup (&ht, t, /*create=*/false, /*copy=*/0);
  if (e)
    {
      /* This will free not only TREE_TYPE, but the lang-specific data
	 and the TYPE_DECL as well.  */
      obstack_free (&permanent_obstack, t);
      t = (tree) e->key;
    }
  else
    /* Insert the type into the table.  */
    hash_lookup (&ht, t, /*create=*/true, /*copy=*/0);

  pop_obstacks ();

  return t;
}

tree
make_typename_type (context, name)
     tree context, name;
{
  tree t;
  tree fullname;

  if (TREE_CODE_CLASS (TREE_CODE (name)) == 't')
    {
      if (!(TYPE_LANG_SPECIFIC (name) 
	    && (CLASSTYPE_IS_TEMPLATE (name) 
		|| CLASSTYPE_USE_TEMPLATE (name))))
	name = TYPE_IDENTIFIER (name);
      else
	/* Create a TEMPLATE_ID_EXPR for the type.  */
	name = build_nt (TEMPLATE_ID_EXPR,
			 CLASSTYPE_TI_TEMPLATE (name),
			 CLASSTYPE_TI_ARGS (name));
    }
  else if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);

  fullname = name;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      name = TREE_OPERAND (name, 0);
      if (TREE_CODE (name) == TEMPLATE_DECL)
	name = TREE_OPERAND (fullname, 0) = DECL_NAME (name);
    }
  if (TREE_CODE (name) != IDENTIFIER_NODE)
    my_friendly_abort (2000);

  if (TREE_CODE (context) == NAMESPACE_DECL)
    {
      /* We can get here from typename_sub0 in the explicit_template_type
	 expansion.  Just fail.  */
      cp_error ("no class template named `%#T' in `%#T'",
		name, context);
      return error_mark_node;
    }

  if (! uses_template_parms (context)
      || currently_open_class (context))
    {
      if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR)
	{
	  tree tmpl = NULL_TREE;
	  if (IS_AGGR_TYPE (context))
	    tmpl = lookup_field (context, name, 0, 0);
	  if (!tmpl || !DECL_CLASS_TEMPLATE_P (tmpl))
	    {
	      cp_error ("no class template named `%#T' in `%#T'",
			name, context);
	      return error_mark_node;
	    }

	  return lookup_template_class (tmpl, 
					TREE_OPERAND (fullname, 1),
					NULL_TREE, context, 
					/*entering_scope=*/0);
	}
      else
	{
	  if (IS_AGGR_TYPE (context))
	    t = lookup_field (context, name, 0, 1);
	  else
	    {
	      cp_error ("no type named `%#T' in `%#T'", name, context);
	      return error_mark_node;
	    }

	  if (t)
	    return TREE_TYPE (t);
	}
    }

  /* If the CONTEXT is not a template type, then either the field is
     there now or its never going to be.  */
  if (!uses_template_parms (context) && !t)
    {
      cp_error ("no type named `%#T' in `%#T'", name, context);
      return error_mark_node;
    }
    
  
  return build_typename_type (context, name, fullname,  NULL_TREE);
}

/* Select the right _DECL from multiple choices. */

static tree
select_decl (binding, flags)
     tree binding;
     int flags;
{
  tree val;
  val = BINDING_VALUE (binding);
  if (LOOKUP_NAMESPACES_ONLY (flags))
    {
      /* We are not interested in types. */
      if (val && TREE_CODE (val) == NAMESPACE_DECL)
        return val;
      return NULL_TREE;
    }
  
  /* If we could have a type and
     we have nothing or we need a type and have none.  */
  if (BINDING_TYPE (binding)
      && (!val || ((flags & LOOKUP_PREFER_TYPES)
                   && TREE_CODE (val) != TYPE_DECL)))
    val = TYPE_STUB_DECL (BINDING_TYPE (binding));
  /* Don't return non-types if we really prefer types. */
  else if (val && LOOKUP_TYPES_ONLY (flags)  && TREE_CODE (val) != TYPE_DECL
	   && (TREE_CODE (val) != TEMPLATE_DECL
	       || !DECL_CLASS_TEMPLATE_P (val)))
    val = NULL_TREE;

  return val;
}

/* Unscoped lookup of a global, iterate over namespaces, considering
   using namespace statements. */

static tree
unqualified_namespace_lookup (name, flags)
     tree name;
     int flags;
{
  struct tree_binding _binding;
  tree b = binding_init (&_binding);
  tree initial = current_decl_namespace();
  tree scope = initial;
  tree siter;
  struct binding_level *level;
  tree val = NULL_TREE;

  while (!val)
    {
      val = binding_for_name (name, scope);

      /* Initialize binding for this context. */
      BINDING_VALUE (b) = BINDING_VALUE (val);
      BINDING_TYPE (b) = BINDING_TYPE (val);

      /* Add all _DECLs seen through local using-directives. */
      for (level = current_binding_level; 
	   !level->namespace_p;
	   level = level->level_chain)
	if (!lookup_using_namespace (name, b, level->using_directives,
                                     scope, flags))
	  /* Give up because of error. */
	  return error_mark_node;

      /* Add all _DECLs seen through global using-directives. */
      /* XXX local and global using lists should work equally. */
      siter = initial;
      while (1)
	{
	  if (!lookup_using_namespace (name, b, DECL_NAMESPACE_USING (siter), 
				       scope, flags))
	    /* Give up because of error. */
	    return error_mark_node;
	  if (siter == scope) break;
	  siter = CP_DECL_CONTEXT (siter);
	}

      val = select_decl (b, flags);
      if (scope == global_namespace)
	break;
      scope = CP_DECL_CONTEXT (scope);
    }
  return val;
}

/* Combine prefer_type and namespaces_only into flags.  */

static int
lookup_flags (prefer_type, namespaces_only)
  int prefer_type, namespaces_only;
{
  if (namespaces_only)
    return LOOKUP_PREFER_NAMESPACES;
  if (prefer_type > 1)
    return LOOKUP_PREFER_TYPES;
  if (prefer_type > 0)
    return LOOKUP_PREFER_BOTH;
  return 0;
}

/* Given a lookup that returned VAL, use FLAGS to decide if we want to
   ignore it or not.  Subroutine of lookup_name_real.  */

static tree
qualify_lookup (val, flags)
     tree val;
     int flags;
{
  if (val == NULL_TREE)
    return val;
  if ((flags & LOOKUP_PREFER_NAMESPACES) && TREE_CODE (val) == NAMESPACE_DECL)
    return val;
  if ((flags & LOOKUP_PREFER_TYPES)
      && (TREE_CODE (val) == TYPE_DECL
	  || ((flags & LOOKUP_TEMPLATES_EXPECTED)
	      && DECL_CLASS_TEMPLATE_P (val))))
    return val;
  if (flags & (LOOKUP_PREFER_NAMESPACES | LOOKUP_PREFER_TYPES))
    return NULL_TREE;
  return val;
}

/* Any other BINDING overrides an implicit TYPENAME.  Warn about
   that.  */

static void
warn_about_implicit_typename_lookup (typename, binding)
     tree typename;
     tree binding;
{
  tree subtype = TREE_TYPE (TREE_TYPE (typename));
  tree name = DECL_NAME (typename);

  if (! (TREE_CODE (binding) == TEMPLATE_DECL
	 && CLASSTYPE_TEMPLATE_INFO (subtype)
	 && CLASSTYPE_TI_TEMPLATE (subtype) == binding)
      && ! (TREE_CODE (binding) == TYPE_DECL
	    && same_type_p (TREE_TYPE (binding), subtype)))
    {
      cp_warning ("lookup of `%D' finds `%#D'", 
		  name, binding);
      cp_warning ("  instead of `%D' from dependent base class",
		  typename);
      cp_warning ("  (use `typename %T::%D' if that's what you meant)",
		  constructor_name (current_class_type), name);
    }
}

/* Look up NAME in the current binding level and its superiors in the
   namespace of variables, functions and typedefs.  Return a ..._DECL
   node of some kind representing its definition if there is only one
   such declaration, or return a TREE_LIST with all the overloaded
   definitions if there are many, or return 0 if it is undefined.

   If PREFER_TYPE is > 0, we prefer TYPE_DECLs or namespaces.
   If PREFER_TYPE is > 1, we reject non-type decls (e.g. namespaces).
   If PREFER_TYPE is -2, we're being called from yylex(). (UGLY)
   Otherwise we prefer non-TYPE_DECLs.  

   If NONCLASS is non-zero, we don't look for the NAME in class scope,
   using IDENTIFIER_CLASS_VALUE.  */

static tree
lookup_name_real (name, prefer_type, nonclass, namespaces_only)
     tree name;
     int prefer_type, nonclass, namespaces_only;
{
  tree t;
  tree val = NULL_TREE;
  int yylex = 0;
  tree from_obj = NULL_TREE;
  int flags;
  int val_is_implicit_typename = 0;

  /* Hack: copy flag set by parser, if set. */
  if (only_namespace_names)
    namespaces_only = 1;

  if (prefer_type == -2)
    {
      extern int looking_for_typename;
      tree type = NULL_TREE;

      yylex = 1;
      prefer_type = looking_for_typename;

      flags = lookup_flags (prefer_type, namespaces_only);
      /* If the next thing is '<', class templates are types. */
      if (looking_for_template)
        flags |= LOOKUP_TEMPLATES_EXPECTED;

      /* std:: becomes :: for now.  */
      if (got_scope == std_node)
	got_scope = void_type_node;

      if (got_scope)
	type = got_scope;
      else if (got_object != error_mark_node)
	type = got_object;
      
      if (type)
	{
	  if (type == error_mark_node)
	    return error_mark_node;
	  if (TREE_CODE (type) == TYPENAME_TYPE && TREE_TYPE (type))
	    type = TREE_TYPE (type);

	  if (TYPE_P (type))
	    type = complete_type (type);

	  if (TREE_CODE (type) == VOID_TYPE)
	    type = global_namespace;
	  if (TREE_CODE (type) == NAMESPACE_DECL)
	    {
	      struct tree_binding b;
	      val = binding_init (&b);
	      flags |= LOOKUP_COMPLAIN;
	      if (!qualified_lookup_using_namespace (name, type, val, flags))
		return NULL_TREE;
	      val = select_decl (val, flags);
	    }
	  else if (! IS_AGGR_TYPE (type)
		   || TREE_CODE (type) == TEMPLATE_TYPE_PARM
		   || TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM
		   || TREE_CODE (type) == TYPENAME_TYPE)
	    /* Someone else will give an error about this if needed.  */
	    val = NULL_TREE;
	  else if (type == current_class_type)
	    val = IDENTIFIER_CLASS_VALUE (name);
	  else
	    val = lookup_member (type, name, 0, prefer_type);
	}
      else
	val = NULL_TREE;

      if (got_scope)
	goto done;
      else if (got_object && val)
	from_obj = val;
    }
  else
    {
      flags = lookup_flags (prefer_type, namespaces_only);
      /* If we're not parsing, we need to complain. */
      flags |= LOOKUP_COMPLAIN;
    }

  /* First, look in non-namespace scopes.  */
  for (t = IDENTIFIER_BINDING (name); t; t = TREE_CHAIN (t))
    {
      tree binding;

      if (!LOCAL_BINDING_P (t) && nonclass)
	/* We're not looking for class-scoped bindings, so keep going.  */
	continue;
      
      /* If this is the kind of thing we're looking for, we're done.  */
      if (qualify_lookup (BINDING_VALUE (t), flags))
	binding = BINDING_VALUE (t);
      else if ((flags & LOOKUP_PREFER_TYPES) 
	       && qualify_lookup (BINDING_TYPE (t), flags))
	binding = BINDING_TYPE (t);
      else
	binding = NULL_TREE;

      if (binding
	  && (!val || !(TREE_CODE (binding) == TYPE_DECL
			&& IMPLICIT_TYPENAME_P (TREE_TYPE (binding)))))
	{
	  if (val_is_implicit_typename && !yylex)
	    warn_about_implicit_typename_lookup (val, binding);
	  val = binding;
	  val_is_implicit_typename 
	    = (TREE_CODE (val) == TYPE_DECL
	       && IMPLICIT_TYPENAME_P (TREE_TYPE (val)));
	  if (!val_is_implicit_typename)
	    break;
	}
    }

  /* Now lookup in namespace scopes.  */
  if (!val || val_is_implicit_typename)
    {
      t = unqualified_namespace_lookup (name, flags);
      if (t)
	{
	  if (val_is_implicit_typename && !yylex)
	    warn_about_implicit_typename_lookup (val, t);
	  val = t;
	}
    }

 done:
  if (val)
    {
      /* This should only warn about types used in qualified-ids.  */
      if (from_obj && from_obj != val)
	{
	  if (looking_for_typename && TREE_CODE (from_obj) == TYPE_DECL
	      && TREE_CODE (val) == TYPE_DECL
	      && TREE_TYPE (from_obj) != TREE_TYPE (val))
	    {
	      cp_pedwarn ("lookup of `%D' in the scope of `%#T' (`%#T')",
			  name, got_object, TREE_TYPE (from_obj));
	      cp_pedwarn ("  does not match lookup in the current scope (`%#T')",
			  TREE_TYPE (val));
	    }

	  /* We don't change val to from_obj if got_object depends on
	     template parms because that breaks implicit typename for
	     destructor calls.  */
	  if (! uses_template_parms (got_object))
	    val = from_obj;
	}

      /* If we have a single function from a using decl, pull it out.  */
      if (TREE_CODE (val) == OVERLOAD && ! really_overloaded_fn (val))
	val = OVL_FUNCTION (val);
    }
  else if (from_obj)
    val = from_obj;

  return val;
}

tree
lookup_name_nonclass (name)
     tree name;
{
  return lookup_name_real (name, 0, 1, 0);
}

tree
lookup_function_nonclass (name, args)
     tree name;
     tree args;
{
  return lookup_arg_dependent (name, lookup_name_nonclass (name), args);
}

tree
lookup_name_namespace_only (name)
     tree name;
{
  /* type-or-namespace, nonclass, namespace_only */
  return lookup_name_real (name, 1, 1, 1);
}

tree
lookup_name (name, prefer_type)
     tree name;
     int prefer_type;
{
  return lookup_name_real (name, prefer_type, 0, 0);
}

/* Similar to `lookup_name' but look only in the innermost non-class
   binding level.  */

tree
lookup_name_current_level (name)
     tree name;
{
  struct binding_level *b;
  tree t = NULL_TREE;

  b = current_binding_level;
  while (b->parm_flag == 2)
    b = b->level_chain;

  if (b->namespace_p)
    {
      t =  IDENTIFIER_NAMESPACE_VALUE (name);

      /* extern "C" function() */
      if (t != NULL_TREE && TREE_CODE (t) == TREE_LIST)
	t = TREE_VALUE (t);
    }
  else if (IDENTIFIER_BINDING (name) 
	   && LOCAL_BINDING_P (IDENTIFIER_BINDING (name)))
    {
      while (1)
	{
	  if (BINDING_LEVEL (IDENTIFIER_BINDING (name)) == b)
	    return IDENTIFIER_VALUE (name);
	  
	  if (b->keep == 2)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  return t;
}

/* Like lookup_name_current_level, but for types.  */

tree
lookup_type_current_level (name)
     tree name;
{
  register tree t = NULL_TREE;

  my_friendly_assert (! current_binding_level->namespace_p, 980716);

  if (REAL_IDENTIFIER_TYPE_VALUE (name) != NULL_TREE
      && REAL_IDENTIFIER_TYPE_VALUE (name) != global_type_node)
    {
      struct binding_level *b = current_binding_level;
      while (1)
	{
	  if (purpose_member (name, b->type_shadowed))
	    return REAL_IDENTIFIER_TYPE_VALUE (name);
	  if (b->keep == 2)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  return t;
}

void
begin_only_namespace_names ()
{
  only_namespace_names = 1;
}

void
end_only_namespace_names ()
{
  only_namespace_names = 0;
}

/* Arrange for the user to get a source line number, even when the
   compiler is going down in flames, so that she at least has a
   chance of working around problems in the compiler.  We used to
   call error(), but that let the segmentation fault continue
   through; now, it's much more passive by asking them to send the
   maintainers mail about the problem.  */

static void
signal_catch (sig)
     int sig ATTRIBUTE_UNUSED;
{
  signal (SIGSEGV, SIG_DFL);
#ifdef SIGIOT
  signal (SIGIOT, SIG_DFL);
#endif
#ifdef SIGILL
  signal (SIGILL, SIG_DFL);
#endif
#ifdef SIGABRT
  signal (SIGABRT, SIG_DFL);
#endif
#ifdef SIGBUS
  signal (SIGBUS, SIG_DFL);
#endif
  my_friendly_abort (0);
}

#if 0
/* Unused -- brendan 970107 */
/* Array for holding types considered "built-in".  These types
   are output in the module in which `main' is defined.  */
static tree *builtin_type_tdescs_arr;
static int builtin_type_tdescs_len, builtin_type_tdescs_max;
#endif

/* Push the declarations of builtin types into the namespace.
   RID_INDEX, if < RID_MAX is the index of the builtin type
   in the array RID_POINTERS.  NAME is the name used when looking
   up the builtin type.  TYPE is the _TYPE node for the builtin type.  */

static void
record_builtin_type (rid_index, name, type)
     enum rid rid_index;
     const char *name;
     tree type;
{
  tree rname = NULL_TREE, tname = NULL_TREE;
  tree tdecl = NULL_TREE;

  if ((int) rid_index < (int) RID_MAX)
    rname = ridpointers[(int) rid_index];
  if (name)
    tname = get_identifier (name);

  TYPE_BUILT_IN (type) = 1;
  
  if (tname)
    {
      tdecl = pushdecl (build_decl (TYPE_DECL, tname, type));
      set_identifier_type_value (tname, NULL_TREE);
      if ((int) rid_index < (int) RID_MAX)
	/* Built-in types live in the global namespace. */
	SET_IDENTIFIER_GLOBAL_VALUE (tname, tdecl);
    }
  if (rname != NULL_TREE)
    {
      if (tname != NULL_TREE)
	{
	  set_identifier_type_value (rname, NULL_TREE);
	  SET_IDENTIFIER_GLOBAL_VALUE (rname, tdecl);
	}
      else
	{
	  tdecl = pushdecl (build_decl (TYPE_DECL, rname, type));
	  set_identifier_type_value (rname, NULL_TREE);
	}
    }
}

/* Record one of the standard Java types.
 * Declare it as having the given NAME.
 * If SIZE > 0, it is the size of one of the integral types;
 * otherwise it is the negative of the size of one of the other types.  */

static tree
record_builtin_java_type (name, size)
     const char *name;
     int size;
{
  tree type, decl;
  if (size > 0)
    type = make_signed_type (size);
  else if (size > -32)
    { /* "__java_char" or ""__java_boolean". */
      type = make_unsigned_type (-size);
      /*if (size == -1)	TREE_SET_CODE (type, BOOLEAN_TYPE);*/
    }
  else
    { /* "__java_float" or ""__java_double". */
      type = make_node (REAL_TYPE);
      TYPE_PRECISION (type) = - size;
      layout_type (type);
    }
  record_builtin_type (RID_MAX, name, type);
  decl = TYPE_NAME (type);

  /* Suppress generate debug symbol entries for these types,
     since for normal C++ they are just clutter.
     However, push_lang_context undoes this if extern "Java" is seen. */
  DECL_IGNORED_P (decl) = 1;

  TYPE_FOR_JAVA (type) = 1;
  return type;
}

/* Push a type into the namespace so that the back-ends ignore it. */

static void
record_unknown_type (type, name)
     tree type;
     const char *name;
{
  tree decl = pushdecl (build_decl (TYPE_DECL, get_identifier (name), type));
  /* Make sure the "unknown type" typedecl gets ignored for debug info.  */
  DECL_IGNORED_P (decl) = 1;
  TYPE_DECL_SUPPRESS_DEBUG (decl) = 1;
  TYPE_SIZE (type) = TYPE_SIZE (void_type_node);
  TYPE_ALIGN (type) = 1;
  TYPE_MODE (type) = TYPE_MODE (void_type_node);
} 

/* Push overloaded decl, in global scope, with one argument so it
   can be used as a callback from define_function.  */

static void
push_overloaded_decl_1 (x)
     tree x;
{
  push_overloaded_decl (x, PUSH_GLOBAL);
}

#ifdef __GNUC__
__inline
#endif
tree
auto_function (name, type, code)
     tree name, type;
     enum built_in_function code;
{
  return define_function
    (IDENTIFIER_POINTER (name), type, code, push_overloaded_decl_1,
     IDENTIFIER_POINTER (build_decl_overload (name, TYPE_ARG_TYPES (type),
					      0)));
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *)0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
init_decl_processing ()
{
  register tree endlink, int_endlink, double_endlink, unsigned_endlink;
  tree fields[20];
  /* Data type of memcpy.  */
  tree memcpy_ftype, strlen_ftype;
  int wchar_type_size;
  tree temp;
  tree array_domain_type;
  tree vb_off_identifier = NULL_TREE;
  /* Function type `char *(char *, char *)' and similar ones */
  tree string_ftype_ptr_ptr, int_ftype_string_string;
  tree sizetype_endlink;
  tree ptr_ftype, ptr_ftype_unsigned, ptr_ftype_sizetype;
  tree void_ftype, void_ftype_int, void_ftype_ptr;

  /* Have to make these distinct before we try using them.  */
  lang_name_cplusplus = get_identifier ("C++");
  lang_name_c = get_identifier ("C");
  lang_name_java = get_identifier ("Java");

  /* Enter the global namespace. */
  my_friendly_assert (global_namespace == NULL_TREE, 375);
  my_friendly_assert (current_lang_name == NULL_TREE, 375);
  current_lang_name = lang_name_cplusplus;
  push_namespace (get_identifier ("::"));
  global_namespace = current_namespace;
  current_lang_name = NULL_TREE;

  if (flag_strict_prototype == 2)
    flag_strict_prototype = pedantic;
  if (! flag_permissive && ! pedantic)
    flag_pedantic_errors = 1;

  strict_prototypes_lang_c = flag_strict_prototype;

  /* Initially, C.  */
  current_lang_name = lang_name_c;

  current_function_decl = NULL_TREE;
  named_labels = NULL_TREE;
  named_label_uses = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;

  /* Because most segmentation signals can be traced back into user
     code, catch them and at least give the user a chance of working
     around compiler bugs.  */
  signal (SIGSEGV, signal_catch);

  /* We will also catch aborts in the back-end through signal_catch and
     give the user a chance to see where the error might be, and to defeat
     aborts in the back-end when there have been errors previously in their
     code.  */
#ifdef SIGIOT
  signal (SIGIOT, signal_catch);
#endif
#ifdef SIGILL
  signal (SIGILL, signal_catch);
#endif
#ifdef SIGABRT
  signal (SIGABRT, signal_catch);
#endif
#ifdef SIGBUS
  signal (SIGBUS, signal_catch);
#endif

  gcc_obstack_init (&decl_obstack);

  /* Must lay these out before anything else gets laid out.  */
  error_mark_node = make_node (ERROR_MARK);
  TREE_PERMANENT (error_mark_node) = 1;
  TREE_TYPE (error_mark_node) = error_mark_node;
  error_mark_list = build_tree_list (error_mark_node, error_mark_node);
  TREE_TYPE (error_mark_list) = error_mark_node;

  /* Make the binding_level structure for global names.  */
  pushlevel (0);
  global_binding_level = current_binding_level;
  /* The global level is the namespace level of ::.  */
  NAMESPACE_LEVEL (global_namespace) = global_binding_level;
  declare_namespace_level ();

  this_identifier = get_identifier (THIS_NAME);
  in_charge_identifier = get_identifier (IN_CHARGE_NAME);
  ctor_identifier = get_identifier (CTOR_NAME);
  dtor_identifier = get_identifier (DTOR_NAME);
  pfn_identifier = get_identifier (VTABLE_PFN_NAME);
  index_identifier = get_identifier (VTABLE_INDEX_NAME);
  delta_identifier = get_identifier (VTABLE_DELTA_NAME);
  delta2_identifier = get_identifier (VTABLE_DELTA2_NAME);
  pfn_or_delta2_identifier = get_identifier ("__pfn_or_delta2");
  if (flag_handle_signatures)
    {
      tag_identifier = get_identifier (SIGTABLE_TAG_NAME);
      vb_off_identifier = get_identifier (SIGTABLE_VB_OFF_NAME);
      vt_off_identifier = get_identifier (SIGTABLE_VT_OFF_NAME);
    }

  /* Define `int' and `char' first so that dbx will output them first.  */

  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  record_builtin_type (RID_INT, NULL_PTR, integer_type_node);

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */

  char_type_node
    = (flag_signed_char
       ? make_signed_type (CHAR_TYPE_SIZE)
       : make_unsigned_type (CHAR_TYPE_SIZE));
  record_builtin_type (RID_CHAR, "char", char_type_node);

  /* `signed' is the same as `int' */
  record_builtin_type (RID_SIGNED, NULL_PTR, integer_type_node);
  
  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  record_builtin_type (RID_LONG, "long int", long_integer_type_node);

  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  record_builtin_type (RID_UNSIGNED, "unsigned int", unsigned_type_node);

  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  record_builtin_type (RID_MAX, "long unsigned int", long_unsigned_type_node);
  record_builtin_type (RID_MAX, "unsigned long", long_unsigned_type_node);

  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  record_builtin_type (RID_MAX, "long long int", long_long_integer_type_node);

  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  record_builtin_type (RID_MAX, "long long unsigned int",
		       long_long_unsigned_type_node);
  record_builtin_type (RID_MAX, "long long unsigned",
		       long_long_unsigned_type_node);

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  record_builtin_type (RID_SHORT, "short int", short_integer_type_node);
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  record_builtin_type (RID_MAX, "short unsigned int", short_unsigned_type_node);
  record_builtin_type (RID_MAX, "unsigned short", short_unsigned_type_node);

  /* `unsigned long' is the standard type for sizeof.
     Note that stddef.h uses `unsigned long',
     and this must agree, even if long and int are the same size.  */
  set_sizetype
    (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (SIZE_TYPE))));

  ptrdiff_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (PTRDIFF_TYPE)));

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  record_builtin_type (RID_MAX, "signed char", signed_char_type_node);
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  record_builtin_type (RID_MAX, "unsigned char", unsigned_char_type_node);

  /* These are types that type_for_size and type_for_mode use.  */
  intQI_type_node = make_signed_type (GET_MODE_BITSIZE (QImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intQI_type_node));
  intHI_type_node = make_signed_type (GET_MODE_BITSIZE (HImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intHI_type_node));
  intSI_type_node = make_signed_type (GET_MODE_BITSIZE (SImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intSI_type_node));
  intDI_type_node = make_signed_type (GET_MODE_BITSIZE (DImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  intTI_type_node = make_signed_type (GET_MODE_BITSIZE (TImode));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("__int128_t"), intTI_type_node));
#endif
  unsigned_intQI_type_node = make_unsigned_type (GET_MODE_BITSIZE (QImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intQI_type_node));
  unsigned_intHI_type_node = make_unsigned_type (GET_MODE_BITSIZE (HImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intHI_type_node));
  unsigned_intSI_type_node = make_unsigned_type (GET_MODE_BITSIZE (SImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intSI_type_node));
  unsigned_intDI_type_node = make_unsigned_type (GET_MODE_BITSIZE (DImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  unsigned_intTI_type_node = make_unsigned_type (GET_MODE_BITSIZE (TImode));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("__uint128_t"), unsigned_intTI_type_node));
#endif

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  record_builtin_type (RID_FLOAT, NULL_PTR, float_type_node);
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  if (flag_short_double)
    TYPE_PRECISION (double_type_node) = FLOAT_TYPE_SIZE;
  else
    TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  record_builtin_type (RID_DOUBLE, NULL_PTR, double_type_node);
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  record_builtin_type (RID_MAX, "long double", long_double_type_node);
  layout_type (long_double_type_node);

  complex_integer_type_node = make_node (COMPLEX_TYPE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex int"),
			complex_integer_type_node));
  TREE_TYPE (complex_integer_type_node) = integer_type_node;
  layout_type (complex_integer_type_node);

  complex_float_type_node = make_node (COMPLEX_TYPE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex float"),
			complex_float_type_node));
  TREE_TYPE (complex_float_type_node) = float_type_node;
  layout_type (complex_float_type_node);

  complex_double_type_node = make_node (COMPLEX_TYPE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex double"),
			complex_double_type_node));
  TREE_TYPE (complex_double_type_node) = double_type_node;
  layout_type (complex_double_type_node);

  complex_long_double_type_node = make_node (COMPLEX_TYPE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("complex long double"),
			complex_long_double_type_node));
  TREE_TYPE (complex_long_double_type_node) = long_double_type_node;
  layout_type (complex_long_double_type_node);

  java_byte_type_node = record_builtin_java_type ("__java_byte", 8);
  java_short_type_node = record_builtin_java_type ("__java_short", 16);
  java_int_type_node = record_builtin_java_type ("__java_int", 32);
  java_long_type_node = record_builtin_java_type ("__java_long", 64);
  java_float_type_node = record_builtin_java_type ("__java_float", -32);
  java_double_type_node = record_builtin_java_type ("__java_double", -64);
  java_char_type_node = record_builtin_java_type ("__java_char", -16);
  java_boolean_type_node = record_builtin_java_type ("__java_boolean", -1);

  integer_zero_node = build_int_2 (0, 0);
  TREE_TYPE (integer_zero_node) = integer_type_node;
  integer_one_node = build_int_2 (1, 0);
  TREE_TYPE (integer_one_node) = integer_type_node;
  integer_two_node = build_int_2 (2, 0);
  TREE_TYPE (integer_two_node) = integer_type_node;
  integer_three_node = build_int_2 (3, 0);
  TREE_TYPE (integer_three_node) = integer_type_node;

  boolean_type_node = make_unsigned_type (BOOL_TYPE_SIZE);
  TREE_SET_CODE (boolean_type_node, BOOLEAN_TYPE);
  TYPE_MAX_VALUE (boolean_type_node) = build_int_2 (1, 0);
  TREE_TYPE (TYPE_MAX_VALUE (boolean_type_node)) = boolean_type_node;
  TYPE_PRECISION (boolean_type_node) = 1;
  record_builtin_type (RID_BOOL, "bool", boolean_type_node);
  boolean_false_node = build_int_2 (0, 0);
  TREE_TYPE (boolean_false_node) = boolean_type_node;
  boolean_true_node = build_int_2 (1, 0);
  TREE_TYPE (boolean_true_node) = boolean_type_node;

  /* These are needed by stor-layout.c.  */
  size_zero_node = size_int (0);
  size_one_node = size_int (1);

  signed_size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (signed_size_zero_node) = make_signed_type (TYPE_PRECISION (sizetype));

  void_type_node = make_node (VOID_TYPE);
  record_builtin_type (RID_VOID, NULL_PTR, void_type_node);
  layout_type (void_type_node); /* Uses integer_zero_node.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);
  TREE_PARMLIST (void_list_node) = 1;

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));
     
  /* Used for expressions that do nothing, but are not errors.  */
  void_zero_node = build_int_2 (0, 0);
  TREE_TYPE (void_zero_node) = void_type_node;

  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type (char_type_node, 
						TYPE_QUAL_CONST));
#if 0
  record_builtin_type (RID_MAX, NULL_PTR, string_type_node);
#endif

  /* Make a type to be the domain of a few array types
     whose domains don't really matter.
     200 is small enough that it always fits in size_t
     and large enough that it can hold most function names for the
     initializations of __FUNCTION__ and __PRETTY_FUNCTION__.  */
  array_domain_type = build_index_type (build_int_2 (200, 0));

  /* Make a type for arrays of characters.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, array_domain_type);
  /* Likewise for arrays of ints.  */
  int_array_type_node
    = build_array_type (integer_type_node, array_domain_type);

  /* This is just some anonymous class type.  Nobody should ever
     need to look inside this envelope.  */
  class_star_type_node = build_pointer_type (make_lang_type (RECORD_TYPE));

  default_function_type
    = build_function_type (integer_type_node, NULL_TREE);

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_CONST)); 
#if 0
  record_builtin_type (RID_MAX, NULL_PTR, ptr_type_node);
#endif
  endlink = void_list_node;
  int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  double_endlink = tree_cons (NULL_TREE, double_type_node, endlink);
  unsigned_endlink = tree_cons (NULL_TREE, unsigned_type_node, endlink);

  ptr_ftype = build_function_type (ptr_type_node, NULL_TREE);
  ptr_ftype_unsigned = build_function_type (ptr_type_node, unsigned_endlink);
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  /* We realloc here because sizetype could be int or unsigned.  S'ok.  */
  ptr_ftype_sizetype = build_function_type (ptr_type_node, sizetype_endlink);

  void_ftype = build_function_type (void_type_node, endlink);
  void_ftype_int = build_function_type (void_type_node, int_endlink);
  void_ftype_ptr
    = build_function_type (void_type_node,
 			   tree_cons (NULL_TREE, ptr_type_node, endlink));
  void_ftype_ptr
    = build_exception_variant (void_ftype_ptr,
			       tree_cons (NULL_TREE, NULL_TREE, NULL_TREE));

  float_ftype_float
    = build_function_type (float_type_node,
			   tree_cons (NULL_TREE, float_type_node, endlink));

  double_ftype_double
    = build_function_type (double_type_node, double_endlink);

  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));

  double_ftype_double_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node,
				      double_endlink));

  int_ftype_int
    = build_function_type (integer_type_node, int_endlink);

  long_ftype_long
    = build_function_type (long_integer_type_node,
			   tree_cons (NULL_TREE, long_integer_type_node,
				      endlink));

  int_ftype_cptr_cptr_sizet
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  string_ftype_ptr_ptr		/* strcpy prototype */
    = build_function_type (string_type_node,
			   tree_cons (NULL_TREE, string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  int_ftype_string_string	/* strcmp prototype */
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  strlen_ftype		/* strlen prototype */
    = build_function_type (sizetype,
			   tree_cons (NULL_TREE, const_string_type_node,
				      endlink));

  memcpy_ftype	/* memcpy prototype */
    = build_function_type (ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 sizetype_endlink)));

  if (flag_huge_objects)
    delta_type_node = long_integer_type_node;
  else
    delta_type_node = short_integer_type_node;

  builtin_function ("__builtin_constant_p", default_function_type,
		    BUILT_IN_CONSTANT_P, NULL_PTR);

  builtin_return_address_fndecl
    = builtin_function ("__builtin_return_address", ptr_ftype_unsigned,
			BUILT_IN_RETURN_ADDRESS, NULL_PTR);

  builtin_function ("__builtin_frame_address", ptr_ftype_unsigned,
		    BUILT_IN_FRAME_ADDRESS, NULL_PTR);

  builtin_function ("__builtin_alloca", ptr_ftype_sizetype,
		    BUILT_IN_ALLOCA, "alloca");
  builtin_function ("__builtin_ffs", int_ftype_int, BUILT_IN_FFS, NULL_PTR);
  /* Define alloca, ffs as builtins.
     Declare _exit just to mark it as volatile.  */
  if (! flag_no_builtin && !flag_no_nonansi_builtin)
    {
      temp = builtin_function ("alloca", ptr_ftype_sizetype,
			       BUILT_IN_ALLOCA, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("ffs", int_ftype_int, BUILT_IN_FFS, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("_exit", void_ftype_int,
			       NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
    }

  builtin_function ("__builtin_abs", int_ftype_int, BUILT_IN_ABS, NULL_PTR);
  builtin_function ("__builtin_fabsf", float_ftype_float, BUILT_IN_FABS,
		    NULL_PTR);
  builtin_function ("__builtin_fabs", double_ftype_double, BUILT_IN_FABS,
		    NULL_PTR);
  builtin_function ("__builtin_fabsl", ldouble_ftype_ldouble, BUILT_IN_FABS,
		    NULL_PTR);
  builtin_function ("__builtin_labs", long_ftype_long,
		    BUILT_IN_LABS, NULL_PTR);
  builtin_function ("__builtin_saveregs", ptr_ftype,
		    BUILT_IN_SAVEREGS, NULL_PTR);
  builtin_function ("__builtin_classify_type", default_function_type,
		    BUILT_IN_CLASSIFY_TYPE, NULL_PTR);
  builtin_function ("__builtin_next_arg", ptr_ftype,
		    BUILT_IN_NEXT_ARG, NULL_PTR);
  builtin_function ("__builtin_args_info", int_ftype_int,
		    BUILT_IN_ARGS_INFO, NULL_PTR);
  builtin_function ("__builtin_setjmp",
		    build_function_type (integer_type_node,
					 tree_cons (NULL_TREE, ptr_type_node,
						    endlink)),
		    BUILT_IN_SETJMP, NULL_PTR);
  builtin_function ("__builtin_longjmp",
		    build_function_type (integer_type_node,
					 tree_cons (NULL_TREE, ptr_type_node,
						    tree_cons (NULL_TREE,
							       integer_type_node,
							       endlink))),
		    BUILT_IN_LONGJMP, NULL_PTR);

  /* Untyped call and return.  */
  builtin_function ("__builtin_apply_args", ptr_ftype,
		    BUILT_IN_APPLY_ARGS, NULL_PTR);

  temp = tree_cons (NULL_TREE,
		    build_pointer_type (build_function_type (void_type_node,
							     NULL_TREE)),
		    tree_cons (NULL_TREE, ptr_ftype_sizetype, NULL_TREE));
  builtin_function ("__builtin_apply",
		    build_function_type (ptr_type_node, temp),
		    BUILT_IN_APPLY, NULL_PTR);
  builtin_function ("__builtin_return", void_ftype_ptr,
		    BUILT_IN_RETURN, NULL_PTR);

  /* Currently under experimentation.  */
  builtin_function ("__builtin_memcpy", memcpy_ftype,
		    BUILT_IN_MEMCPY, "memcpy");
  builtin_function ("__builtin_memcmp", int_ftype_cptr_cptr_sizet,
		    BUILT_IN_MEMCMP, "memcmp");
  builtin_function ("__builtin_strcmp", int_ftype_string_string,
		    BUILT_IN_STRCMP, "strcmp");
  builtin_function ("__builtin_strcpy", string_ftype_ptr_ptr,
		    BUILT_IN_STRCPY, "strcpy");
  builtin_function ("__builtin_strlen", strlen_ftype,
		    BUILT_IN_STRLEN, "strlen");
  builtin_function ("__builtin_sqrtf", float_ftype_float, 
		    BUILT_IN_FSQRT, "sqrtf");
  builtin_function ("__builtin_fsqrt", double_ftype_double,
		    BUILT_IN_FSQRT, NULL_PTR);
  builtin_function ("__builtin_sqrtl", ldouble_ftype_ldouble, 
		    BUILT_IN_FSQRT, "sqrtl");
  builtin_function ("__builtin_sinf", float_ftype_float, 
		    BUILT_IN_SIN, "sinf");
  builtin_function ("__builtin_sin", double_ftype_double, 
		    BUILT_IN_SIN, "sin");
  builtin_function ("__builtin_sinl", ldouble_ftype_ldouble, 
		    BUILT_IN_SIN, "sinl");
  builtin_function ("__builtin_cosf", float_ftype_float, 
		    BUILT_IN_COS, "cosf");
  builtin_function ("__builtin_cos", double_ftype_double, 
		    BUILT_IN_COS, "cos");
  builtin_function ("__builtin_cosl", ldouble_ftype_ldouble, 
		    BUILT_IN_COS, "cosl");

  if (!flag_no_builtin)
    {
      builtin_function ("abs", int_ftype_int, BUILT_IN_ABS, NULL_PTR);
      builtin_function ("fabs", double_ftype_double, BUILT_IN_FABS, NULL_PTR);
      builtin_function ("labs", long_ftype_long, BUILT_IN_LABS, NULL_PTR);
      builtin_function ("fabsf", float_ftype_float, BUILT_IN_FABS, NULL_PTR);
      builtin_function ("fabsl", ldouble_ftype_ldouble, BUILT_IN_FABS,
			NULL_PTR);
      builtin_function ("memcpy", memcpy_ftype, BUILT_IN_MEMCPY, NULL_PTR);
      builtin_function ("memcmp", int_ftype_cptr_cptr_sizet, BUILT_IN_MEMCMP,
			NULL_PTR);
      builtin_function ("strcmp", int_ftype_string_string, BUILT_IN_STRCMP,
			NULL_PTR);
      builtin_function ("strcpy", string_ftype_ptr_ptr, BUILT_IN_STRCPY,
			NULL_PTR);
      builtin_function ("strlen", strlen_ftype, BUILT_IN_STRLEN, NULL_PTR);
      builtin_function ("sqrtf", float_ftype_float, BUILT_IN_FSQRT, NULL_PTR);
      builtin_function ("sqrt", double_ftype_double, BUILT_IN_FSQRT, NULL_PTR);
      builtin_function ("sqrtl", ldouble_ftype_ldouble, BUILT_IN_FSQRT,
			NULL_PTR);
      builtin_function ("sinf", float_ftype_float, BUILT_IN_SIN, NULL_PTR);
      builtin_function ("sin", double_ftype_double, BUILT_IN_SIN, NULL_PTR);
      builtin_function ("sinl", ldouble_ftype_ldouble, BUILT_IN_SIN, NULL_PTR);
      builtin_function ("cosf", float_ftype_float, BUILT_IN_COS, NULL_PTR);
      builtin_function ("cos", double_ftype_double, BUILT_IN_COS, NULL_PTR);
      builtin_function ("cosl", ldouble_ftype_ldouble, BUILT_IN_COS, NULL_PTR);

      /* Declare these functions volatile
	 to avoid spurious "control drops through" warnings.  */
      temp = builtin_function ("abort", void_ftype,
			       NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
      /* Well, these are actually ANSI, but we can't set DECL_BUILT_IN on
         them...  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("exit", void_ftype_int,
			       NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
      DECL_BUILT_IN_NONANSI (temp) = 1;
    }

#if 0
  /* Support for these has not been written in either expand_builtin
     or build_function_call.  */
  builtin_function ("__builtin_div", default_ftype, BUILT_IN_DIV, NULL_PTR);
  builtin_function ("__builtin_ldiv", default_ftype, BUILT_IN_LDIV, NULL_PTR);
  builtin_function ("__builtin_ffloor", double_ftype_double, BUILT_IN_FFLOOR,
		    NULL_PTR);
  builtin_function ("__builtin_fceil", double_ftype_double, BUILT_IN_FCEIL,
		    NULL_PTR);
  builtin_function ("__builtin_fmod", double_ftype_double_double,
		    BUILT_IN_FMOD, NULL_PTR);
  builtin_function ("__builtin_frem", double_ftype_double_double,
		    BUILT_IN_FREM, NULL_PTR);
  builtin_function ("__builtin_memset", ptr_ftype_ptr_int_int,
		    BUILT_IN_MEMSET, NULL_PTR);
  builtin_function ("__builtin_getexp", double_ftype_double, BUILT_IN_GETEXP,
		    NULL_PTR);
  builtin_function ("__builtin_getman", double_ftype_double, BUILT_IN_GETMAN,
		    NULL_PTR);
#endif

  /* C++ extensions */

  unknown_type_node = make_node (UNKNOWN_TYPE);
  record_unknown_type (unknown_type_node, "unknown type");

  /* Indirecting an UNKNOWN_TYPE node yields an UNKNOWN_TYPE node.  */
  TREE_TYPE (unknown_type_node) = unknown_type_node;

  TREE_TYPE (null_node) = type_for_size (POINTER_SIZE, 0);

  /* Looking up TYPE_POINTER_TO and TYPE_REFERENCE_TO yield the same
     result.  */
  TYPE_POINTER_TO (unknown_type_node) = unknown_type_node;
  TYPE_REFERENCE_TO (unknown_type_node) = unknown_type_node;

  /* This is for handling opaque types in signatures.  */
  opaque_type_node = copy_node (ptr_type_node);
  TYPE_MAIN_VARIANT (opaque_type_node) = opaque_type_node;
  record_builtin_type (RID_MAX, 0, opaque_type_node);

  /* This is special for C++ so functions can be overloaded.  */
  wchar_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (WCHAR_TYPE)));
  wchar_type_size = TYPE_PRECISION (wchar_type_node);
  signed_wchar_type_node = make_signed_type (wchar_type_size);
  unsigned_wchar_type_node = make_unsigned_type (wchar_type_size);
  wchar_type_node
    = TREE_UNSIGNED (wchar_type_node)
      ? unsigned_wchar_type_node
      : signed_wchar_type_node;
  record_builtin_type (RID_WCHAR, "__wchar_t", wchar_type_node);

  /* Artificial declaration of wchar_t -- can be bashed */
  wchar_decl_node = build_decl (TYPE_DECL, get_identifier ("wchar_t"),
				wchar_type_node);
  pushdecl (wchar_decl_node);

  /* This is for wide string constants.  */
  wchar_array_type_node
    = build_array_type (wchar_type_node, array_domain_type);

  if (flag_vtable_thunks)
    {
      /* Make sure we get a unique function type, so we can give
	 its pointer type a name.  (This wins for gdb.) */
      tree vfunc_type = make_node (FUNCTION_TYPE);
      TREE_TYPE (vfunc_type) = integer_type_node;
      TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
      layout_type (vfunc_type);

      vtable_entry_type = build_pointer_type (vfunc_type);
    }
  else
    {
      vtable_entry_type = make_lang_type (RECORD_TYPE);
      fields[0] = build_lang_field_decl (FIELD_DECL, delta_identifier,
					 delta_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, index_identifier,
					 delta_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, pfn_identifier,
					 ptr_type_node);
      finish_builtin_type (vtable_entry_type, VTBL_PTR_TYPE, fields, 2,
			   double_type_node);

      /* Make this part of an invisible union.  */
      fields[3] = copy_node (fields[2]);
      TREE_TYPE (fields[3]) = delta_type_node;
      DECL_NAME (fields[3]) = delta2_identifier;
      DECL_MODE (fields[3]) = TYPE_MODE (delta_type_node);
      DECL_SIZE (fields[3]) = TYPE_SIZE (delta_type_node);
      TREE_UNSIGNED (fields[3]) = 0;
      TREE_CHAIN (fields[2]) = fields[3];
      vtable_entry_type = build_qualified_type (vtable_entry_type,
						TYPE_QUAL_CONST);
    }
  record_builtin_type (RID_MAX, VTBL_PTR_TYPE, vtable_entry_type);

  vtbl_type_node
    = build_cplus_array_type (vtable_entry_type, NULL_TREE);
  layout_type (vtbl_type_node);
  vtbl_type_node = build_qualified_type (vtbl_type_node, TYPE_QUAL_CONST);
  record_builtin_type (RID_MAX, NULL_PTR, vtbl_type_node);
  vtbl_ptr_type_node = build_pointer_type (vtable_entry_type);
  layout_type (vtbl_ptr_type_node);
  record_builtin_type (RID_MAX, NULL_PTR, vtbl_ptr_type_node);

  /* Simplify life by making a "sigtable_entry_type".  Give its
     fields names so that the debugger can use them.  */

  if (flag_handle_signatures)
    {
      sigtable_entry_type = make_lang_type (RECORD_TYPE);
      fields[0] = build_lang_field_decl (FIELD_DECL, tag_identifier,
					 delta_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, vb_off_identifier,
					 delta_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, delta_identifier,
					 delta_type_node);
      fields[3] = build_lang_field_decl (FIELD_DECL, index_identifier,
					 delta_type_node);
      fields[4] = build_lang_field_decl (FIELD_DECL, pfn_identifier,
					 ptr_type_node);

      /* Set the alignment to the max of the alignment of ptr_type_node and
	 delta_type_node.  Double alignment wastes a word on the Sparc.  */
      finish_builtin_type (sigtable_entry_type, SIGTABLE_PTR_TYPE, fields, 4,
			   (TYPE_ALIGN (ptr_type_node) > TYPE_ALIGN (delta_type_node))
			   ? ptr_type_node
			   : delta_type_node);

      /* Make this part of an invisible union.  */
      fields[5] = copy_node (fields[4]);
      TREE_TYPE (fields[5]) = delta_type_node;
      DECL_NAME (fields[5]) = vt_off_identifier;
      DECL_MODE (fields[5]) = TYPE_MODE (delta_type_node);
      DECL_SIZE (fields[5]) = TYPE_SIZE (delta_type_node);
      TREE_UNSIGNED (fields[5]) = 0;
      TREE_CHAIN (fields[4]) = fields[5];

      sigtable_entry_type = build_qualified_type (sigtable_entry_type, 
						  TYPE_QUAL_CONST);
      record_builtin_type (RID_MAX, SIGTABLE_PTR_TYPE, sigtable_entry_type);
    }

  std_node = build_decl (NAMESPACE_DECL, 
			 get_identifier (flag_honor_std ? "fake std":"std"),
			 void_type_node);
  pushdecl (std_node);

  global_type_node = make_node (LANG_TYPE);
  record_unknown_type (global_type_node, "global type");

  /* Now, C++.  */
  current_lang_name = lang_name_cplusplus;

  {
    tree bad_alloc_type_node, newtype, deltype;
    if (flag_honor_std)
      push_namespace (get_identifier ("std"));
    bad_alloc_type_node = xref_tag
      (class_type_node, get_identifier ("bad_alloc"), 1);
    if (flag_honor_std)
      pop_namespace ();
    newtype = build_exception_variant
      (ptr_ftype_sizetype, build_tree_list (NULL_TREE, bad_alloc_type_node));
    deltype = build_exception_variant
      (void_ftype_ptr, build_tree_list (NULL_TREE, NULL_TREE));
    auto_function (ansi_opname[(int) NEW_EXPR], newtype, NOT_BUILT_IN);
    auto_function (ansi_opname[(int) VEC_NEW_EXPR], newtype, NOT_BUILT_IN);
    global_delete_fndecl
      = auto_function (ansi_opname[(int) DELETE_EXPR], deltype, NOT_BUILT_IN);
    auto_function (ansi_opname[(int) VEC_DELETE_EXPR], deltype, NOT_BUILT_IN);
  }

  abort_fndecl
    = define_function ("__pure_virtual", void_ftype,
		       NOT_BUILT_IN, 0, 0);

  /* Perform other language dependent initializations.  */
  init_class_processing ();
  init_init_processing ();
  init_search_processing ();
  if (flag_rtti)
    init_rtti_processing ();

  if (flag_exceptions)
    init_exception_processing ();
  if (flag_no_inline)
    {
      flag_inline_functions = 0;
    }

  if (! supports_one_only ())
    flag_weak = 0;

  /* Create the global bindings for __FUNCTION__ and __PRETTY_FUNCTION__.  */
  declare_function_name ();

  /* Prepare to check format strings against argument lists.  */
  init_function_format_info ();

  /* Show we use EH for cleanups.  */
  using_eh_for_cleanups ();

  print_error_function = lang_print_error_function;
  lang_get_alias_set = &c_get_alias_set;
  valid_lang_attribute = cp_valid_lang_attribute;

  /* Maintain consistency.  Perhaps we should just complain if they
     say -fwritable-strings?  */
  if (flag_writable_strings)
    flag_const_strings = 0;
}

/* Function to print any language-specific context for an error message.  */

static void
lang_print_error_function (file)
     char *file;
{
  default_print_error_function (file);
  maybe_print_template_context ();
}

/* Make a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
define_function (name, type, function_code, pfn, library_name)
     const char *name;
     tree type;
     enum built_in_function function_code;
     void (*pfn) PROTO((tree));
     const char *library_name;
{
  tree decl = build_lang_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;

  my_friendly_assert (DECL_CONTEXT (decl) == NULL_TREE, 392);
  DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

  /* Since `pushdecl' relies on DECL_ASSEMBLER_NAME instead of DECL_NAME,
     we cannot change DECL_ASSEMBLER_NAME until we have installed this
     function in the namespace.  */
  if (pfn) (*pfn) (decl);
  if (library_name)
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_function_rtl (decl);
  if (function_code != NOT_BUILT_IN)
    {
      DECL_BUILT_IN (decl) = 1;
      DECL_FUNCTION_CODE (decl) = function_code;
    }
  return decl;
}

/* When we call finish_struct for an anonymous union, we create
   default copy constructors and such.  But, an anonymous union
   shouldn't have such things; this function undoes the damage to the
   anonymous union type T.

   (The reason that we create the synthesized methods is that we don't
   distinguish `union { int i; }' from `typedef union { int i; } U'.
   The first is an anonymous union; the second is just an ordinary
   union type.)  */

void
fixup_anonymous_union (t)
     tree t;
{
  tree *q;

  /* Wipe out memory of synthesized methods */
  TYPE_HAS_CONSTRUCTOR (t) = 0;
  TYPE_HAS_DEFAULT_CONSTRUCTOR (t) = 0;
  TYPE_HAS_INIT_REF (t) = 0;
  TYPE_HAS_CONST_INIT_REF (t) = 0;
  TYPE_HAS_ASSIGN_REF (t) = 0;
  TYPE_HAS_CONST_ASSIGN_REF (t) = 0;

  /* Splice the implicitly generated functions out of the TYPE_METHODS
     list.  */
  q = &TYPE_METHODS (t);
  while (*q)
    {
      if (DECL_ARTIFICIAL (*q))
	*q = TREE_CHAIN (*q);
      else
	q = &TREE_CHAIN (*q);
    }

  /* ANSI C++ June 5 1992 WP 9.5.3.  Anonymous unions may not have
     function members.  */
  if (TYPE_METHODS (t))
    error ("an anonymous union cannot have function members");
}

/* Make sure that a declaration with no declarator is well-formed, i.e.
   just defines a tagged type or anonymous union.

   Returns the type defined, if any.  */

tree
check_tag_decl (declspecs)
     tree declspecs;
{
  int found_type = 0;
  tree ob_modifier = NULL_TREE;
  register tree link;
  register tree t = NULL_TREE;

  for (link = declspecs; link; link = TREE_CHAIN (link))
    {
      register tree value = TREE_VALUE (link);

      if (TYPE_P (value))
	{
	  ++found_type;

	  if (IS_AGGR_TYPE (value) || TREE_CODE (value) == ENUMERAL_TYPE)
	    {
	      my_friendly_assert (TYPE_MAIN_DECL (value) != NULL_TREE, 261);
	      t = value;
	    }
	}
      else if (value == ridpointers[(int) RID_FRIEND])
	{
	  if (current_class_type == NULL_TREE
	      || current_scope () != current_class_type)
	    ob_modifier = value;
	}
      else if (value == ridpointers[(int) RID_STATIC]
	       || value == ridpointers[(int) RID_EXTERN]
	       || value == ridpointers[(int) RID_AUTO]
	       || value == ridpointers[(int) RID_REGISTER]
	       || value == ridpointers[(int) RID_INLINE]
	       || value == ridpointers[(int) RID_VIRTUAL]
	       || value == ridpointers[(int) RID_CONST]
	       || value == ridpointers[(int) RID_VOLATILE]
	       || value == ridpointers[(int) RID_EXPLICIT])
	ob_modifier = value;
    }

  if (found_type > 1)
    error ("multiple types in one declaration");

  /* Inside a class, we might be in a friend or access declaration.
     Until we have a good way of detecting the latter, don't warn.  */
  if (t == NULL_TREE && ! current_class_type)
    pedwarn ("declaration does not declare anything");

  /* Check for an anonymous union.  We're careful
     accessing TYPE_IDENTIFIER because some built-in types, like
     pointer-to-member types, do not have TYPE_NAME.  */
  else if (t && TREE_CODE (t) == UNION_TYPE
	   && TYPE_NAME (t)
	   && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
    {
      /* Anonymous unions are objects, so they can have specifiers.  */;
      SET_ANON_UNION_TYPE_P (t);
    }

  else if (ob_modifier)
    {
      if (ob_modifier == ridpointers[(int) RID_INLINE]
	  || ob_modifier == ridpointers[(int) RID_VIRTUAL])
	cp_error ("`%D' can only be specified for functions", ob_modifier);
      else if (ob_modifier == ridpointers[(int) RID_FRIEND])
	cp_error ("`%D' can only be specified inside a class", ob_modifier);
      else if (ob_modifier == ridpointers[(int) RID_EXPLICIT])
	cp_error ("`%D' can only be specified for constructors",
		  ob_modifier);
      else
	cp_error ("`%D' can only be specified for objects and functions",
		  ob_modifier);
    }

  return t;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.

   C++: may have to grok the declspecs to learn about static,
   complain for anonymous unions.  */

void
shadow_tag (declspecs)
     tree declspecs;
{
  tree t = check_tag_decl (declspecs);

  if (t)
    maybe_process_partial_specialization (t);

  /* This is where the variables in an anonymous union are
     declared.  An anonymous union declaration looks like:
     union { ... } ;
     because there is no declarator after the union, the parser
     sends that declaration here.  */
  if (t && ANON_UNION_TYPE_P (t))
    {
      fixup_anonymous_union (t);

      if (TYPE_FIELDS (t))
	{
	  tree decl = grokdeclarator (NULL_TREE, declspecs, NORMAL, 0,
				      NULL_TREE);
	  finish_anon_union (decl);
	}
    }
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.  */

tree
groktypename (typename)
     tree typename;
{
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  return grokdeclarator (TREE_VALUE (typename),
			 TREE_PURPOSE (typename),
			 TYPENAME, 0, NULL_TREE);
}

/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `cp_finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

/* Set this to zero to debug not using the temporary obstack
   to parse initializers.  */
int debug_temp_inits = 1;

tree
start_decl (declarator, declspecs, initialized, attributes, prefix_attributes)
     tree declarator, declspecs;
     int initialized;
     tree attributes, prefix_attributes;
{
  register tree decl;
  register tree type, tem;
  tree context;
  extern int have_extern_spec;
  extern int used_extern_spec;
  tree attrlist;

#if 0
  /* See code below that used this.  */
  int init_written = initialized;
#endif

  /* This should only be done once on the top most decl.  */
  if (have_extern_spec && !used_extern_spec)
    {
      declspecs = decl_tree_cons (NULL_TREE, get_identifier ("extern"),
				  declspecs);
      used_extern_spec = 1;
    }

  if (attributes || prefix_attributes)
    attrlist = build_scratch_list (attributes, prefix_attributes);
  else
    attrlist = NULL_TREE;

  decl = grokdeclarator (declarator, declspecs, NORMAL, initialized,
			 attrlist);
			 
  if (decl == NULL_TREE || TREE_CODE (decl) == VOID_TYPE)
    return NULL_TREE;

  type = TREE_TYPE (decl);

  if (type == error_mark_node)
    return NULL_TREE;

  /* Don't lose if destructors must be executed at file-level.  */
  if (! processing_template_decl && TREE_STATIC (decl)
      && TYPE_NEEDS_DESTRUCTOR (complete_type (type))
      && !TREE_PERMANENT (decl))
    {
      push_obstacks (&permanent_obstack, &permanent_obstack);
      decl = copy_node (decl);
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree itype = TYPE_DOMAIN (type);
	  if (itype && ! TREE_PERMANENT (itype))
	    {
	      itype = build_index_type (copy_to_permanent (TYPE_MAX_VALUE (itype)));
	      type = build_cplus_array_type (TREE_TYPE (type), itype);
	      TREE_TYPE (decl) = type;
	    }
	}
      pop_obstacks ();
    }

  context
    = (TREE_CODE (decl) == FUNCTION_DECL && DECL_VIRTUAL_P (decl))
      ? DECL_CLASS_CONTEXT (decl)
      : DECL_CONTEXT (decl);

  if (initialized && context && TREE_CODE (context) == NAMESPACE_DECL
      && context != current_namespace && TREE_CODE (decl) == VAR_DECL)
    {
      /* When parsing the initializer, lookup should use the object's
	 namespace. */
      push_decl_namespace (context);
    }

  /* We are only interested in class contexts, later. */
  if (context && TREE_CODE (context) == NAMESPACE_DECL)
    context = NULL_TREE;

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `cp_finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	/* typedef foo = bar  means give foo the same type as bar.
	   We haven't parsed bar yet, so `cp_finish_decl' will fix that up.
	   Any other case of an initialization in a TYPE_DECL is an error.  */
	if (pedantic || list_length (declspecs) > 1)
	  {
	    cp_error ("typedef `%D' is initialized", decl);
	    initialized = 0;
	  }
	break;

      case FUNCTION_DECL:
	cp_error ("function `%#D' is initialized like a variable", decl);
	initialized = 0;
	break;

      default:
	if (! processing_template_decl)
	  {
	    if (type != error_mark_node)
	      {
		if (TYPE_SIZE (type) != NULL_TREE
		    && ! TREE_CONSTANT (TYPE_SIZE (type)))
		  {
		    cp_error
		      ("variable-sized object `%D' may not be initialized",
		       decl);
		    initialized = 0;
		  }

		if (TREE_CODE (type) == ARRAY_TYPE
		    && TYPE_SIZE (complete_type (TREE_TYPE (type))) == NULL_TREE)
		  {
		    cp_error
		      ("elements of array `%#D' have incomplete type", decl);
		    initialized = 0;
		  }
	      }
	  }
      }

  if (initialized)
    {
      if (! toplevel_bindings_p ()
	  && DECL_EXTERNAL (decl))
	cp_warning ("declaration of `%#D' has `extern' and is initialized",
		    decl);
      DECL_EXTERNAL (decl) = 0;
      if (toplevel_bindings_p ())
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `cp_finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

#ifdef SET_DEFAULT_DECL_ATTRIBUTES
  SET_DEFAULT_DECL_ATTRIBUTES (decl, attributes);
#endif
  
  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  cplus_decl_attributes (decl, attributes, prefix_attributes);

  if (context && TYPE_SIZE (complete_type (context)) != NULL_TREE)
    {
      push_nested_class (context, 2);

      if (TREE_CODE (decl) == VAR_DECL)
	{
	  tree field = lookup_field (context, DECL_NAME (decl), 0, 0);
	  if (field == NULL_TREE || TREE_CODE (field) != VAR_DECL)
	    cp_error ("`%#D' is not a static member of `%#T'", decl, context);
	  else
	    {
	      if (DECL_CONTEXT (field) != context)
		{
		  cp_pedwarn ("ANSI C++ does not permit `%T::%D' to be defined as `%T::%D'",
			      DECL_CONTEXT (field), DECL_NAME (decl),
			      context, DECL_NAME (decl));
		  DECL_CONTEXT (decl) = DECL_CONTEXT (field);
		}
	      /* Static data member are tricky; an in-class initialization
		 still doesn't provide a definition, so the in-class
		 declaration will have DECL_EXTERNAL set, but will have an
		 initialization.  Thus, duplicate_decls won't warn
		 about this situation, and so we check here.  */
	      if (DECL_INITIAL (decl) && DECL_INITIAL (field))
		cp_error ("duplicate initialization of %D", decl);
	      if (duplicate_decls (decl, field))
		decl = field;
	    }
	}
      else
	{
	  tree field = check_classfn (context, decl);
	  if (field && duplicate_decls (decl, field))
	    decl = field;
	}

      /* cp_finish_decl sets DECL_EXTERNAL if DECL_IN_AGGR_P is set.  */
      DECL_IN_AGGR_P (decl) = 0;
      if ((DECL_LANG_SPECIFIC (decl) && DECL_USE_TEMPLATE (decl)) 
	  || CLASSTYPE_USE_TEMPLATE (context))
	{
	  SET_DECL_TEMPLATE_SPECIALIZATION (decl);
	  /* [temp.expl.spec] An explicit specialization of a static data
	     member of a template is a definition if the declaration
	     includes an initializer; otherwise, it is a declaration.

	     We check for processing_specialization so this only applies
	     to the new specialization syntax.  */
	  if (DECL_INITIAL (decl) == NULL_TREE && processing_specialization)
	    DECL_EXTERNAL (decl) = 1;
	}

      if (DECL_EXTERNAL (decl) && ! DECL_TEMPLATE_SPECIALIZATION (decl))
	cp_pedwarn ("declaration of `%#D' outside of class is not definition",
		    decl);
    }

  /* Add this decl to the current binding level, but not if it
     comes from another scope, e.g. a static member variable.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  
  if ((TREE_CODE (decl) != PARM_DECL && DECL_CONTEXT (decl) != NULL_TREE 
       /* Definitions of namespace members outside their namespace are
	  possible. */
       && TREE_CODE (DECL_CONTEXT (decl)) != NAMESPACE_DECL)
      || (TREE_CODE (decl) == TEMPLATE_DECL && !namespace_bindings_p ())
      || TREE_CODE (type) == LANG_TYPE
      /* The declaration of template specializations does not affect
	 the functions available for overload resolution, so we do not
	 call pushdecl.  */
      || (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_TEMPLATE_SPECIALIZATION (decl)))
    tem = decl;
  else
    tem = pushdecl (decl);

  if (processing_template_decl)
    {
      if (! current_function_decl)
	tem = push_template_decl (tem);
      else if (minimal_parse_mode)
	DECL_VINDEX (tem)
	    = build_min_nt (DECL_STMT, copy_to_permanent (declarator),
			    copy_to_permanent (declspecs),
			    NULL_TREE);
    }


#if ! defined (ASM_OUTPUT_BSS) && ! defined (ASM_OUTPUT_ALIGNED_BSS)
  /* Tell the back-end to use or not use .common as appropriate.  If we say
     -fconserve-space, we want this to save .data space, at the expense of
     wrong semantics.  If we say -fno-conserve-space, we want this to
     produce errors about redefs; to do this we force variables into the
     data segment.  */
  DECL_COMMON (tem) = flag_conserve_space || ! TREE_PUBLIC (tem);
#endif
  
  if (! processing_template_decl)
    start_decl_1 (tem);

  /* Corresponding pop_obstacks is done in `cp_finish_decl'.  */
  push_obstacks_nochange ();

#if 0
  /* We have no way of knowing whether the initializer will need to be
     evaluated at run-time or not until we've parsed it, so let's just put
     it in the permanent obstack.  (jason) */
  if (init_written
      && ! (TREE_CODE (tem) == PARM_DECL
	    || (TREE_READONLY (tem)
		&& (TREE_CODE (tem) == VAR_DECL
		    || TREE_CODE (tem) == FIELD_DECL))))
    {
      /* When parsing and digesting the initializer,
	 use temporary storage.  Do this even if we will ignore the value.  */
      if (toplevel_bindings_p () && debug_temp_inits)
	{
	  if (processing_template_decl
	      || TYPE_NEEDS_CONSTRUCTING (type)
	      || TREE_CODE (type) == REFERENCE_TYPE)
	    /* In this case, the initializer must lay down in permanent
	       storage, since it will be saved until `finish_file' is run.   */
	    ;
	  else
	    temporary_allocation ();
	}
    }
#endif

  return tem;
}

void
start_decl_1 (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);
  int initialized = (DECL_INITIAL (decl) != NULL_TREE);

  if (type == error_mark_node)
    return;

  /* If this type of object needs a cleanup, and control may
     jump past it, make a new binding level so that it is cleaned
     up only when it is initialized first.  */
  if (TYPE_NEEDS_DESTRUCTOR (type)
      && current_binding_level->more_cleanups_ok == 0)
    pushlevel_temporary (1);

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `cp_finish_decl' to ignore the initializer once it is parsed.  */
    {
      /* Don't allow initializations for incomplete types except for
	 arrays which might be completed by the initialization.  */
      if (TYPE_SIZE (complete_type (type)) != NULL_TREE)
	;			/* A complete type is ok.  */
      else if (TREE_CODE (type) != ARRAY_TYPE)
	{
	  cp_error ("variable `%#D' has initializer but incomplete type",
		    decl);
	  initialized = 0;
	  type = TREE_TYPE (decl) = error_mark_node;
	}
      else if (TYPE_SIZE (complete_type (TREE_TYPE (type))) == NULL_TREE)
	{
	  if (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl))
	    cp_error ("elements of array `%#D' have incomplete type", decl);
	  /* else we already gave an error in start_decl.  */
	  initialized = 0;
	}
    }

  if (!initialized
      && TREE_CODE (decl) != TYPE_DECL
      && TREE_CODE (decl) != TEMPLATE_DECL
      && IS_AGGR_TYPE (type) && ! DECL_EXTERNAL (decl))
    {
      if ((! processing_template_decl || ! uses_template_parms (type))
	  && TYPE_SIZE (complete_type (type)) == NULL_TREE)
	{
	  cp_error ("aggregate `%#D' has incomplete type and cannot be initialized",
		 decl);
	  /* Change the type so that assemble_variable will give
	     DECL an rtl we can live with: (mem (const_int 0)).  */
	  type = TREE_TYPE (decl) = error_mark_node;
	}
      else
	{
	  /* If any base type in the hierarchy of TYPE needs a constructor,
	     then we set initialized to 1.  This way any nodes which are
	     created for the purposes of initializing this aggregate
	     will live as long as it does.  This is necessary for global
	     aggregates which do not have their initializers processed until
	     the end of the file.  */
	  initialized = TYPE_NEEDS_CONSTRUCTING (type);
	}
    }

#if 0
  /* We don't do this yet for GNU C++.  */
  /* For a local variable, define the RTL now.  */
  if (! toplevel_bindings_p ()
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && DECL_RTL (tem) == NULL_RTX)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != NULL_TREE)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != NULL_TREE)
	expand_decl (tem);
    }
#endif

  if (! initialized)
    DECL_INITIAL (decl) = NULL_TREE;
}

/* Handle initialization of references.
   These three arguments are from `cp_finish_decl', and have the
   same meaning here that they do there.

   Quotes on semantics can be found in ARM 8.4.3.  */

static void
grok_reference_init (decl, type, init)
     tree decl, type, init;
{
  tree tmp;

  if (init == NULL_TREE)
    {
      if ((DECL_LANG_SPECIFIC (decl) == 0
	   || DECL_IN_AGGR_P (decl) == 0)
	  && ! DECL_THIS_EXTERN (decl))
	{
	  cp_error ("`%D' declared as reference but not initialized", decl);
	  if (TREE_CODE (decl) == VAR_DECL)
	    SET_DECL_REFERENCE_SLOT (decl, error_mark_node);
	}
      return;
    }

  if (init == error_mark_node)
    return;

  if (TREE_CODE (type) == REFERENCE_TYPE
      && TREE_CODE (init) == CONSTRUCTOR)
    {
      cp_error ("ANSI C++ forbids use of initializer list to initialize reference `%D'", decl);
      return;
    }

  if (TREE_CODE (init) == TREE_LIST)
    init = build_compound_expr (init);

  if (TREE_CODE (TREE_TYPE (init)) == REFERENCE_TYPE)
    init = convert_from_reference (init);

  if (TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE)
    {
      /* Note: default conversion is only called in very special cases.  */
      init = default_conversion (init);
    }

  tmp = convert_to_reference
    (type, init, CONV_IMPLICIT,
     LOOKUP_SPECULATIVELY|LOOKUP_NORMAL|DIRECT_BIND, decl);

  if (tmp == error_mark_node)
    goto fail;
  else if (tmp != NULL_TREE)
    {
      init = tmp;
      DECL_INITIAL (decl) = save_expr (init);
    }
  else
    {
      cp_error ("cannot initialize `%T' from `%T'", type, TREE_TYPE (init));
      goto fail;
    }

  /* ?? Can this be optimized in some cases to
     hand back the DECL_INITIAL slot??  */
  if (TYPE_SIZE (TREE_TYPE (type)))
    {
      init = convert_from_reference (decl);
      if (TREE_PERMANENT (decl))
	init = copy_to_permanent (init);
      SET_DECL_REFERENCE_SLOT (decl, init);
    }

  if (TREE_STATIC (decl) && ! TREE_CONSTANT (DECL_INITIAL (decl)))
    {
      expand_static_init (decl, DECL_INITIAL (decl));
      DECL_INITIAL (decl) = NULL_TREE;
    }
  return;

 fail:
  if (TREE_CODE (decl) == VAR_DECL)
    SET_DECL_REFERENCE_SLOT (decl, error_mark_node);
  return;
}

/* Fill in DECL_INITIAL with some magical value to prevent expand_decl from
   mucking with forces it does not comprehend (i.e. initialization with a
   constructor).  If we are at global scope and won't go into COMMON, fill
   it in with a dummy CONSTRUCTOR to force the variable into .data;
   otherwise we can use error_mark_node.  */

static tree
obscure_complex_init (decl, init)
     tree decl, init;
{
  if (! flag_no_inline && TREE_STATIC (decl))
    {
      if (extract_init (decl, init))
	return NULL_TREE;
    }

#if ! defined (ASM_OUTPUT_BSS) && ! defined (ASM_OUTPUT_ALIGNED_BSS)
  if (toplevel_bindings_p () && ! DECL_COMMON (decl))
    DECL_INITIAL (decl) = build (CONSTRUCTOR, TREE_TYPE (decl), NULL_TREE,
				 NULL_TREE);
  else
#endif
    DECL_INITIAL (decl) = error_mark_node;

  return init;
}

/* Issue an error message if DECL is an uninitialized const variable.  */

static void
check_for_uninitialized_const_var (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);

  /* ``Unless explicitly declared extern, a const object does not have
     external linkage and must be initialized. ($8.4; $12.1)'' ARM
     7.1.6 */
  if (TREE_CODE (decl) == VAR_DECL
      && TREE_CODE (type) != REFERENCE_TYPE
      && CP_TYPE_CONST_P (type)
      && !TYPE_NEEDS_CONSTRUCTING (type)
      && !DECL_INITIAL (decl))
    cp_error ("uninitialized const `%D'", decl);
}

/* Finish processing of a declaration;
   install its line number and initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.

   Call `pop_obstacks' iff NEED_POP is nonzero.

   For C++, `cp_finish_decl' must be fairly evasive:  it must keep initializers
   for aggregates that have constructors alive on the permanent obstack,
   so that the global initializing functions can be written at the end.

   INIT0 holds the value of an initializer that should be allowed to escape
   the normal rules.

   FLAGS is LOOKUP_ONLYCONVERTING is the = init syntax was used, else 0
   if the (init) syntax was used.

   For functions that take default parameters, DECL points to its
   "maximal" instantiation.  `cp_finish_decl' must then also declared its
   subsequently lower and lower forms of instantiation, checking for
   ambiguity as it goes.  This can be sped up later.  */

void
cp_finish_decl (decl, init, asmspec_tree, need_pop, flags)
     tree decl, init;
     tree asmspec_tree;
     int need_pop;
     int flags;
{
  register tree type;
  tree cleanup = NULL_TREE, ttype = NULL_TREE;
  int was_incomplete;
  int temporary = allocation_temporary_p ();
  char *asmspec = NULL;
  int was_readonly = 0;
  int already_used = 0;
  tree core_type;

  /* If this is 0, then we did not change obstacks.  */
  if (! decl)
    {
      if (init)
	error ("assignment (not initialization) in declaration");
      return;
    }

  /* If a name was specified, get the string.  */
  if (asmspec_tree)
      asmspec = TREE_STRING_POINTER (asmspec_tree);

  if (init && TREE_CODE (init) == NAMESPACE_DECL)
    {
      cp_error ("Cannot initialize `%D' to namespace `%D'",
		decl, init);
      init = NULL_TREE;
    }

  if (current_class_type
      && DECL_REAL_CONTEXT (decl) == current_class_type
      && TYPE_BEING_DEFINED (current_class_type)
      && (DECL_INITIAL (decl) || init))
    DECL_DEFINED_IN_CLASS_P (decl) = 1;

  if (TREE_CODE (decl) == VAR_DECL 
      && DECL_CONTEXT (decl)
      && TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL
      && DECL_CONTEXT (decl) != current_namespace
      && init)
    {
      /* Leave the namespace of the object. */
      pop_decl_namespace ();
    }

  /* If the type of the thing we are declaring either has
     a constructor, or has a virtual function table pointer,
     AND its initialization was accepted by `start_decl',
     then we stayed on the permanent obstack through the
     declaration, otherwise, changed obstacks as GCC would.  */

  type = TREE_TYPE (decl);

  if (type == error_mark_node)
    {
      if (toplevel_bindings_p () && temporary)
	end_temporary_allocation ();

      return;
    }

  if (TYPE_HAS_MUTABLE_P (type))
    TREE_READONLY (decl) = 0;
  
  if (processing_template_decl)
    {
      if (init && DECL_INITIAL (decl))
	DECL_INITIAL (decl) = init;
      if (minimal_parse_mode && ! DECL_ARTIFICIAL (decl))
	{
	  tree stmt = DECL_VINDEX (decl);
	  /* If the decl is declaring a member of a local class (in a
	     template function), the DECL_VINDEX will either be NULL,
	     or it will be an actual virtual function index, not a
	     DECL_STMT.  */
	  if (stmt != NULL_TREE && TREE_CODE (stmt) == DECL_STMT)
	    {
	      DECL_VINDEX (decl) = NULL_TREE;
	      TREE_OPERAND (stmt, 2) = copy_to_permanent (init);
	      add_tree (stmt);
	    }
	}

      goto finish_end0;
    }
  /* Take care of TYPE_DECLs up front.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (init && DECL_INITIAL (decl))
	{
	  /* typedef foo = bar; store the type of bar as the type of foo.  */
	  TREE_TYPE (decl) = type = TREE_TYPE (init);
	  DECL_INITIAL (decl) = init = NULL_TREE;
	}
      if (type != error_mark_node
	  && IS_AGGR_TYPE (type) && DECL_NAME (decl))
	{
	  if (TREE_TYPE (DECL_NAME (decl)) && TREE_TYPE (decl) != type)
	    cp_warning ("shadowing previous type declaration of `%#D'", decl);
	  set_identifier_type_value (DECL_NAME (decl), type);
	  CLASSTYPE_GOT_SEMICOLON (type) = 1;
	}
      GNU_xref_decl (current_function_decl, decl);

      /* If we have installed this as the canonical typedef for this
	 type, and that type has not been defined yet, delay emitting
	 the debug information for it, as we will emit it later.  */
      if (TYPE_MAIN_DECL (TREE_TYPE (decl)) == decl
	  && TYPE_SIZE (TREE_TYPE (decl)) == NULL_TREE)
	TYPE_DECL_SUPPRESS_DEBUG (decl) = 1;

      rest_of_decl_compilation (decl, NULL_PTR,
				DECL_CONTEXT (decl) == NULL_TREE, at_eof);
      goto finish_end;
    }
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      ttype = target_type (type);
    }

  if (! DECL_EXTERNAL (decl) && TREE_READONLY (decl)
      && TYPE_NEEDS_CONSTRUCTING (type))
    {

      /* Currently, GNU C++ puts constants in text space, making them
	 impossible to initialize.  In the future, one would hope for
	 an operating system which understood the difference between
	 initialization and the running of a program.  */
      was_readonly = 1;
      TREE_READONLY (decl) = 0;
    }

  if (TREE_CODE (decl) == FIELD_DECL)
    {
      if (init && init != error_mark_node)
	my_friendly_assert (TREE_PERMANENT (init), 147);

      if (asmspec)
	{
	  /* This must override the asm specifier which was placed
	     by grokclassfn.  Lay this out fresh.  */
	  DECL_RTL (TREE_TYPE (decl)) = NULL_RTX;
	  DECL_ASSEMBLER_NAME (decl) = get_identifier (asmspec);
	  make_decl_rtl (decl, asmspec, 0);
	}
    }
  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  else if (init != NULL_TREE && DECL_INITIAL (decl) == NULL_TREE)
    init = NULL_TREE;
  else if (DECL_EXTERNAL (decl))
    ;
  else if (TREE_CODE (type) == REFERENCE_TYPE
	   || (TYPE_LANG_SPECIFIC (type) && IS_SIGNATURE_REFERENCE (type)))
    {
      if (TREE_STATIC (decl))
	make_decl_rtl (decl, NULL_PTR,
		       toplevel_bindings_p ()
		       || pseudo_global_level_p ());
      grok_reference_init (decl, type, init);
      init = NULL_TREE;
    }

  GNU_xref_decl (current_function_decl, decl);

  core_type = type;
  while (TREE_CODE (core_type) == ARRAY_TYPE)
    core_type = TREE_TYPE (core_type);
  
  if (TREE_CODE (decl) == FIELD_DECL)
    ;
  else if (TREE_CODE (decl) == CONST_DECL)
    {
      my_friendly_assert (TREE_CODE (decl) != REFERENCE_TYPE, 148);

      DECL_INITIAL (decl) = init;

      /* This will keep us from needing to worry about our obstacks.  */
      my_friendly_assert (init != NULL_TREE, 149);
      init = NULL_TREE;
    }
  else if (init)
    {
      if (TYPE_HAS_CONSTRUCTOR (type) || TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    init = digest_init (type, init, (tree *) 0);
	  else if (TREE_CODE (init) == CONSTRUCTOR
		   && TREE_HAS_CONSTRUCTOR (init))
	    {
	      if (TYPE_NON_AGGREGATE_CLASS (type))
		{
		  cp_error ("`%D' must be initialized by constructor, not by `{...}'",
			    decl);
		  init = error_mark_node;
		}
	      else
		goto dont_use_constructor;
	    }
	}
      else
	{
	dont_use_constructor:
	  if (TREE_CODE (init) != TREE_VEC)
	    init = store_init_value (decl, init);
	}

      if (init)
	/* We must hide the initializer so that expand_decl
	   won't try to do something it does not understand.  */
	init = obscure_complex_init (decl, init);
    }
  else if (DECL_EXTERNAL (decl))
    ;
  else if (TREE_CODE_CLASS (TREE_CODE (type)) == 't'
	   && (IS_AGGR_TYPE (type) || TYPE_NEEDS_CONSTRUCTING (type)))
    {
      if (! TYPE_NEEDS_CONSTRUCTING (core_type))
	{
	  if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (core_type))
	    cp_error ("structure `%D' with uninitialized const members", decl);
	  if (CLASSTYPE_REF_FIELDS_NEED_INIT (core_type))
	    cp_error ("structure `%D' with uninitialized reference members",
		      decl);
	}

      check_for_uninitialized_const_var (decl);

      if (TYPE_SIZE (type) != NULL_TREE
	  && TYPE_NEEDS_CONSTRUCTING (type))
	init = obscure_complex_init (decl, NULL_TREE);

    }
  else
    check_for_uninitialized_const_var (decl);
  
  /* For top-level declaration, the initial value was read in
     the temporary obstack.  MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack; but don't discard the
     temporary data yet.  */

  if (toplevel_bindings_p () && temporary)
    end_temporary_allocation ();

  /* Deduce size of array from initialization, if not already known.  */

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE
      && TREE_CODE (decl) != TYPE_DECL)
    {
      int do_default
	= (TREE_STATIC (decl)
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && ! DECL_EXTERNAL (decl)
	   : !DECL_EXTERNAL (decl));
      tree initializer = init ? init : DECL_INITIAL (decl);
      int failure = complete_array_type (type, initializer, do_default);

      if (failure == 1)
	cp_error ("initializer fails to determine size of `%D'", decl);

      if (failure == 2)
	{
	  if (do_default)
	    cp_error ("array size missing in `%D'", decl);
	  /* If a `static' var's size isn't known, make it extern as
	     well as static, so it does not get allocated.  If it's not
	     `static', then don't mark it extern; finish_incomplete_decl
	     will give it a default size and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && !TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	}

      if (pedantic && TYPE_DOMAIN (type) != NULL_TREE
	  && tree_int_cst_lt (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			      integer_zero_node))
	cp_error ("zero-size array `%D'", decl);

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == NULL_TREE
	  && TYPE_SIZE (complete_type (TREE_TYPE (decl))) != NULL_TREE)
	layout_decl (decl, 0);

      if (TREE_STATIC (decl) && DECL_SIZE (decl) == NULL_TREE)
	{
	  /* A static variable with an incomplete type:
	     that is an error if it is initialized.
	     Otherwise, let it through, but if it is not `extern'
	     then it may cause an error message later.  */
	  if (DECL_INITIAL (decl) != NULL_TREE)
	    cp_error ("storage size of `%D' isn't known", decl);
	  init = NULL_TREE;
	}
      else if (!DECL_EXTERNAL (decl) && DECL_SIZE (decl) == NULL_TREE)
	{
	  /* An automatic variable with an incomplete type: that is an error.
	     Don't talk about array types here, since we took care of that
	     message in grokdeclarator.  */
	  cp_error ("storage size of `%D' isn't known", decl);
	  TREE_TYPE (decl) = error_mark_node;
	}
      else if (!DECL_EXTERNAL (decl) && IS_AGGR_TYPE (ttype))
	/* Let debugger know it should output info for this type.  */
	note_debug_info_needed (ttype);

      if (TREE_STATIC (decl) && DECL_CLASS_SCOPE_P (decl))
	note_debug_info_needed (DECL_CONTEXT (decl));

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != NULL_TREE
	  && ! TREE_CONSTANT (DECL_SIZE (decl)))
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    cp_error ("storage size of `%D' isn't constant", decl);
	}

      if (! DECL_EXTERNAL (decl) && TYPE_NEEDS_DESTRUCTOR (type)
	  /* Cleanups for static variables are handled by `finish_file'.  */
	  && ! TREE_STATIC (decl))
	{
	  int yes = suspend_momentary ();
	  cleanup = maybe_build_cleanup (decl);
	  resume_momentary (yes);
	}
    }
  /* PARM_DECLs get cleanups, too.  */
  else if (TREE_CODE (decl) == PARM_DECL && TYPE_NEEDS_DESTRUCTOR (type))
    {
      if (temporary)
	end_temporary_allocation ();
      cleanup = maybe_build_cleanup (decl);
      if (temporary)
	resume_temporary_allocation ();
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  was_incomplete = (DECL_SIZE (decl) == NULL_TREE);

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL
      || TREE_CODE (decl) == RESULT_DECL)
    {
      /* ??? FIXME: What about nested classes?  */
      int toplev = toplevel_bindings_p () || pseudo_global_level_p ();
      int was_temp
	= (TREE_STATIC (decl) && TYPE_NEEDS_DESTRUCTOR (type)
	   && allocation_temporary_p ());

      if (was_temp)
	end_temporary_allocation ();

      /* Static data in a function with comdat linkage also has comdat
         linkage.  */
      if (TREE_CODE (decl) == VAR_DECL
	  && TREE_STATIC (decl)
	  /* Don't mess with __FUNCTION__.  */
	  && ! TREE_ASM_WRITTEN (decl)
	  && current_function_decl
	  && DECL_CONTEXT (decl) == current_function_decl
	  && (DECL_THIS_INLINE (current_function_decl)
	      || DECL_TEMPLATE_INSTANTIATION (current_function_decl))
	  && TREE_PUBLIC (current_function_decl))
	{
	  /* Rather than try to get this right with inlining, we suppress
	     inlining of such functions.  */
	  current_function_cannot_inline
	    = "function with static variable cannot be inline";

	  /* If flag_weak, we don't need to mess with this, as we can just
	     make the function weak, and let it refer to its unique local
	     copy.  This works because we don't allow the function to be
	     inlined.  */
	  if (! flag_weak)
	    {
	      if (DECL_INTERFACE_KNOWN (current_function_decl))
		{
		  TREE_PUBLIC (decl) = 1;
		  DECL_EXTERNAL (decl) = DECL_EXTERNAL (current_function_decl);
		}
	      else if (DECL_INITIAL (decl) == NULL_TREE
		       || DECL_INITIAL (decl) == error_mark_node)
		{
		  TREE_PUBLIC (decl) = 1;
		  DECL_COMMON (decl) = 1;
		}
	      /* else we lose. We can only do this if we can use common,
                 which we can't if it has been initialized.  */

	      if (TREE_PUBLIC (decl))
		DECL_ASSEMBLER_NAME (decl)
		  = build_static_name (current_function_decl, DECL_NAME (decl));
	      else if (! DECL_ARTIFICIAL (decl))
		{
		  cp_warning_at ("sorry: semantics of inline function static data `%#D' are wrong (you'll wind up with multiple copies)", decl);
		  cp_warning_at ("  you can work around this by removing the initializer", decl);
		}
	    }
	}

      else if (TREE_CODE (decl) == VAR_DECL
	       && DECL_LANG_SPECIFIC (decl)
	       && DECL_COMDAT (decl))
	/* Set it up again; we might have set DECL_INITIAL since the
	   last time.  */
	comdat_linkage (decl);

      if (TREE_CODE (decl) == VAR_DECL && DECL_VIRTUAL_P (decl))
	make_decl_rtl (decl, NULL_PTR, toplev);
      else if (TREE_CODE (decl) == VAR_DECL
	       && TREE_READONLY (decl)
	       && DECL_INITIAL (decl) != NULL_TREE
	       && DECL_INITIAL (decl) != error_mark_node
	       && ! EMPTY_CONSTRUCTOR_P (DECL_INITIAL (decl)))
	{
	  DECL_INITIAL (decl) = save_expr (DECL_INITIAL (decl));

	  if (asmspec)
	    DECL_ASSEMBLER_NAME (decl) = get_identifier (asmspec);

	  if (! toplev
	      && TREE_STATIC (decl)
	      && ! TREE_SIDE_EFFECTS (decl)
	      && ! TREE_PUBLIC (decl)
	      && ! DECL_EXTERNAL (decl)
	      && ! TYPE_NEEDS_DESTRUCTOR (type)
	      && DECL_MODE (decl) != BLKmode)
	    {
	      /* If this variable is really a constant, then fill its DECL_RTL
		 slot with something which won't take up storage.
		 If something later should take its address, we can always give
		 it legitimate RTL at that time.  */
	      DECL_RTL (decl) = gen_reg_rtx (DECL_MODE (decl));
	      store_expr (DECL_INITIAL (decl), DECL_RTL (decl), 0);
	      TREE_ASM_WRITTEN (decl) = 1;
	    }
	  else if (toplev && ! TREE_PUBLIC (decl))
	    {
	      /* If this is a static const, change its apparent linkage
	         if it belongs to a #pragma interface.  */
	      if (!interface_unknown)
		{
		  TREE_PUBLIC (decl) = 1;
		  DECL_EXTERNAL (decl) = interface_only;
		}
	      make_decl_rtl (decl, asmspec, toplev);
	    }
	  else
	    rest_of_decl_compilation (decl, asmspec, toplev, at_eof);
	}
      else if (TREE_CODE (decl) == VAR_DECL
	       && DECL_LANG_SPECIFIC (decl)
	       && DECL_IN_AGGR_P (decl))
	{
	  if (TREE_STATIC (decl))
	    {
	      if (init == NULL_TREE
#ifdef DEFAULT_STATIC_DEFS
		  /* If this code is dead, then users must
		     explicitly declare static member variables
		     outside the class def'n as well.  */
		  && TYPE_NEEDS_CONSTRUCTING (type)
#endif
		  )
		{
		  DECL_EXTERNAL (decl) = 1;
		  make_decl_rtl (decl, asmspec, 1);
		}
	      else
		rest_of_decl_compilation (decl, asmspec, toplev, at_eof);
	    }
	  else
	    /* Just a constant field.  Should not need any rtl.  */
	    goto finish_end0;
	}
      else
	rest_of_decl_compilation (decl, asmspec, toplev, at_eof);

      if (was_temp)
	resume_temporary_allocation ();

      if (type != error_mark_node
	  && TYPE_LANG_SPECIFIC (core_type)
	  && CLASSTYPE_ABSTRACT_VIRTUALS (core_type))
	abstract_virtuals_error (decl, core_type);
      else if ((TREE_CODE (type) == FUNCTION_TYPE
		|| TREE_CODE (type) == METHOD_TYPE)
	       && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
	       && CLASSTYPE_ABSTRACT_VIRTUALS (TREE_TYPE (type)))
	abstract_virtuals_error (decl, TREE_TYPE (type));

      if (TYPE_LANG_SPECIFIC (core_type) && IS_SIGNATURE (core_type))
	signature_error (decl, core_type);
      else if ((TREE_CODE (type) == FUNCTION_TYPE
		|| TREE_CODE (type) == METHOD_TYPE)
	       && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
	       && IS_SIGNATURE (TREE_TYPE (type)))
	signature_error (decl, TREE_TYPE (type));

      if (TREE_CODE (decl) == FUNCTION_DECL)
	;
      else if (DECL_EXTERNAL (decl)
	       && ! (DECL_LANG_SPECIFIC (decl)
		     && DECL_NOT_REALLY_EXTERN (decl)))
	{
	  if (init)
	    DECL_INITIAL (decl) = init;
	}
      else if (TREE_STATIC (decl) && type != error_mark_node)
	{
	  /* Cleanups for static variables are handled by `finish_file'.  */
	  if (TYPE_NEEDS_CONSTRUCTING (type) || init != NULL_TREE
	      || TYPE_NEEDS_DESTRUCTOR (type))
	    expand_static_init (decl, init);
	}
      else if (! toplev)
	{
	  /* This is a declared decl which must live until the
	     end of the binding contour.  It may need a cleanup.  */

	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete && ! TREE_STATIC (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == NULL_TREE)
		DECL_INITIAL (decl) = NULL_TREE;
	      expand_decl (decl);
	    }
	  else if (! TREE_ASM_WRITTEN (decl)
		   && (TYPE_SIZE (type) != NULL_TREE
		       || TREE_CODE (type) == ARRAY_TYPE))
	    {
	      /* Do this here, because we did not expand this decl's
		 rtl in start_decl.  */
	      if (DECL_RTL (decl) == NULL_RTX)
		expand_decl (decl);
	      else if (cleanup)
		{
		  /* XXX: Why don't we use decl here?  */
		  /* Ans: Because it was already expanded? */
		  if (! expand_decl_cleanup (NULL_TREE, cleanup))
		    cp_error ("parser lost in parsing declaration of `%D'",
			      decl);
		  /* Cleanup used up here.  */
		  cleanup = NULL_TREE;
		}
	    }

	  if (current_binding_level->is_for_scope)
	    {
	      struct binding_level *outer 
		= current_binding_level->level_chain;

	      /* Check to see if the same name is already bound at
		 the outer level, either because it was directly declared,
		 or because a dead for-decl got preserved.  In either case,
		 the code would not have been valid under the ARM
		 scope rules, so clear is_for_scope for the
		 current_binding_level.

		 Otherwise, we need to preserve the temp slot for decl
		 to last into the outer binding level.  */

	      tree outer_binding 
		= TREE_CHAIN (IDENTIFIER_BINDING (DECL_NAME (decl)));
	      
	      if (outer_binding && BINDING_LEVEL (outer_binding) == outer
		  && (TREE_CODE (BINDING_VALUE (outer_binding)) 
		      == VAR_DECL)
		  && DECL_DEAD_FOR_LOCAL (BINDING_VALUE (outer_binding)))
		{
		  BINDING_VALUE (outer_binding)
		    = DECL_SHADOWED_FOR_VAR (BINDING_VALUE (outer_binding));
		  current_binding_level->is_for_scope = 0;
		}
	      else if (DECL_IN_MEMORY_P (decl))
		preserve_temp_slots (DECL_RTL (decl));
	    }

	  expand_start_target_temps ();

	  if (DECL_SIZE (decl) && type != error_mark_node)
	    {
	      /* Compute and store the initial value.  */
	      expand_decl_init (decl);
       	      already_used = TREE_USED (decl) || TREE_USED (type);

	      if (init || TYPE_NEEDS_CONSTRUCTING (type))
		{
		  emit_line_note (DECL_SOURCE_FILE (decl),
				  DECL_SOURCE_LINE (decl));
		  expand_aggr_init (decl, init, flags);
		}

	      /* Set this to 0 so we can tell whether an aggregate which
		 was initialized was ever used.  Don't do this if it has a
		 destructor, so we don't complain about the 'resource
		 allocation is initialization' idiom.  */
	      /* Now set attribute((unused)) on types so decls of
		 that type will be marked used. (see TREE_USED, above.) 
		 This avoids the warning problems this particular code
		 tried to work around. */

	      if (TYPE_NEEDS_CONSTRUCTING (type)
		  && ! already_used
		  && cleanup == NULL_TREE
		  && DECL_NAME (decl))
		TREE_USED (decl) = 0;

	      if (already_used)
		TREE_USED (decl) = 1;
	    }

	  /* Cleanup any temporaries needed for the initial value.  */
	  expand_end_target_temps ();

	  if (DECL_SIZE (decl) && type != error_mark_node)
	    {
	      /* Store the cleanup, if there was one.  */
	      if (cleanup)
		{
		  if (! expand_decl_cleanup (decl, cleanup))
		    cp_error ("parser lost in parsing declaration of `%D'",
			      decl);
		}
	    }
	}
    finish_end0:

      /* Undo call to `pushclass' that was done in `start_decl'
	 due to initialization of qualified member variable.
	 I.e., Foo::x = 10;  */
      {
	tree context = DECL_REAL_CONTEXT (decl);
	if (context
	    && TREE_CODE_CLASS (TREE_CODE (context)) == 't'
	    && (TREE_CODE (decl) == VAR_DECL
		/* We also have a pushclass done that we need to undo here
		   if we're at top level and declare a method.  */
		|| TREE_CODE (decl) == FUNCTION_DECL)
	    /* If size hasn't been set, we're still defining it,
	       and therefore inside the class body; don't pop
	       the binding level..  */
	    && TYPE_SIZE (context) != NULL_TREE
	    && context == current_class_type)
	  pop_nested_class ();
      }
    }

 finish_end:

  /* If requested, warn about definitions of large data objects.  */

  if (warn_larger_than
      && ! processing_template_decl
      && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
      && !DECL_EXTERNAL (decl))
    {
      register tree decl_size = DECL_SIZE (decl);

      if (decl_size && TREE_CODE (decl_size) == INTEGER_CST)
	{
	  unsigned units = TREE_INT_CST_LOW (decl_size) / BITS_PER_UNIT;

	  if (units > larger_than_size)
	    warning_with_decl (decl, "size of `%s' is %u bytes", units);
	}
    }

  if (need_pop)
    {
      /* Resume permanent allocation, if not within a function.  */
      /* The corresponding push_obstacks_nochange is in start_decl,
	 start_method, groktypename, and in grokfield.  */
      pop_obstacks ();
    }

  if (was_readonly)
    TREE_READONLY (decl) = 1;
}

/* This is here for a midend callback from c-common.c */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  cp_finish_decl (decl, init, asmspec_tree, 1, 0);
}

void
expand_static_init (decl, init)
     tree decl;
     tree init;
{
  tree oldstatic = value_member (decl, static_aggregates);

  if (oldstatic)
    {
      if (TREE_PURPOSE (oldstatic) && init != NULL_TREE)
	cp_error ("multiple initializations given for `%D'", decl);
    }
  else if (! toplevel_bindings_p ())
    {
      /* Emit code to perform this initialization but once.  */
      tree temp;

      /* Remember this information until end of file.  */
      push_obstacks (&permanent_obstack, &permanent_obstack);

      /* Emit code to perform this initialization but once.  This code
	 looks like:

           static int temp = 0;
           if (!temp) {
             // Do initialization.
	     temp = 1;
	     // Register variable for destruction at end of program.
	   }

	 Note that the `temp' variable is only set to 1 *after* the
	 initialization is complete.  This ensures that an exception,
	 thrown during the construction, will cause the variable to
	 reinitialized when we pass through this code again, as per:
	 
	   [stmt.dcl]

	   If the initialization exits by throwing an exception, the
	   initialization is not complete, so it will be tried again
	   the next time control enters the declaration.

         In theory, this process should be thread-safe, too; multiple
	 threads should not be able to initialize the variable more
	 than once.  We don't yet attempt to ensure thread-safety.  */
      temp = get_temp_name (integer_type_node, 1);
      rest_of_decl_compilation (temp, NULL_PTR, 0, 0);

      /* Begin the conditional initialization.  */
      expand_start_cond (build_binary_op (EQ_EXPR, temp,
					  integer_zero_node), 0);
      expand_start_target_temps ();

      /* Do the initialization itself.  */
      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl))
	  || (init && TREE_CODE (init) == TREE_LIST))
	{
	  expand_aggr_init (decl, init, 0);
	  do_pending_stack_adjust ();
	}
      else if (init)
	expand_assignment (decl, init, 0, 0);

      /* Set TEMP to 1.  */
      expand_assignment (temp, integer_one_node, 0, 0);

      /* Cleanup any temporaries needed for the initial value.  If
	 destroying one of the temporaries causes an exception to be
	 thrown, then the object itself has still been fully
	 constructed.  */
      expand_end_target_temps ();

      /* Use atexit to register a function for destroying this static
	 variable.  */
      if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (decl)))
	{
	  tree cleanup, fcall;
	  static tree Atexit = 0;
	  int saved_flag_access_control;

	  if (Atexit == 0)
	    {
	      tree atexit_fndecl, PFV, pfvlist;
	      /* Remember this information until end of file.  */
	      push_obstacks (&permanent_obstack, &permanent_obstack);
	      PFV = build_pointer_type (build_function_type
					(void_type_node, void_list_node));

	      pfvlist = tree_cons (NULL_TREE, PFV, void_list_node);

	      push_lang_context (lang_name_c);
	      atexit_fndecl
		= builtin_function ("atexit",
				    build_function_type (void_type_node,
							 pfvlist),
				    NOT_BUILT_IN, NULL_PTR);
	      mark_used (atexit_fndecl);
	      Atexit = default_conversion (atexit_fndecl);
	      pop_lang_context ();
	      pop_obstacks ();
	    }
	      
	  /* Call build_cleanup before we enter the anonymous function
	     so that any access checks will be done relative to the
	     current scope, rather than the scope of the anonymous
	     function.  */
	  build_cleanup (decl);

	  /* Now start the function.  */
	  cleanup = start_anon_func ();

	  /* Now, recompute the cleanup.  It may contain SAVE_EXPRs
	     that refer to the original function, rather than the
	     anonymous one.  That will make the back-end think that
	     nested functions are in use, which causes confusion.  */
	  saved_flag_access_control = flag_access_control;
	  flag_access_control = 0;
	  fcall = build_cleanup (decl);
	  flag_access_control = saved_flag_access_control;

	  /* Finish off the function.  */
	  expand_expr_stmt (fcall);
	  end_anon_func ();

	  /* Call atexit with the cleanup function.  */
	  mark_addressable (cleanup);
	  cleanup = build_unary_op (ADDR_EXPR, cleanup, 0);
	  fcall = build_function_call (Atexit, 
				       expr_tree_cons (NULL_TREE, 
						       cleanup, 
						       NULL_TREE));
	  expand_expr_stmt (fcall);
	}

      expand_end_cond ();
      /* Resume old (possibly temporary) allocation.  */
      pop_obstacks ();
    }
  else
    {
      /* This code takes into account memory allocation policy of
	 `start_decl'.  Namely, if TYPE_NEEDS_CONSTRUCTING does not
	 hold for this object, then we must make permanent the storage
	 currently in the temporary obstack.  */
      if (!TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
	preserve_initializer ();
      static_aggregates = perm_tree_cons (init, decl, static_aggregates);
    }
}

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 0 if DO_DEFAULT).  */

int
complete_array_type (type, initial_value, do_default)
     tree type, initial_value;
     int do_default;
{
  register tree maxindex = NULL_TREE;
  int value = 0;
  
  /* Allocate on the same obstack as TYPE.  */
  push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));
  
  if (initial_value)
    {
      /* Note MAXINDEX  is really the maximum index,
	 one less than the size.  */
      if (TREE_CODE (initial_value) == STRING_CST)
	{
	  int eltsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (initial_value)));
	  maxindex = build_int_2 ((TREE_STRING_LENGTH (initial_value)
				   / eltsize) - 1, 0);
	}
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  tree elts = CONSTRUCTOR_ELTS (initial_value);
	  maxindex = size_binop (MINUS_EXPR, integer_zero_node, size_one_node);
	  for (; elts; elts = TREE_CHAIN (elts))
	    {
	      if (TREE_PURPOSE (elts))
		maxindex = TREE_PURPOSE (elts);
	      else
		maxindex = size_binop (PLUS_EXPR, maxindex, size_one_node);
	    }
	  maxindex = copy_node (maxindex);
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    value = 1;
	  else
	    initial_value = NULL_TREE;

	  /* Prevent further error messages.  */
	  maxindex = build_int_2 (0, 0);
	}
    }

  if (!maxindex)
    {
      if (do_default)
	maxindex = build_int_2 (0, 0);
      value = 2;
    }

  if (maxindex)
    {
      tree itype;
      tree domain;

      domain = build_index_type (maxindex);
      TYPE_DOMAIN (type) = domain;

      if (! TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = domain;
      if (initial_value)
        itype = TREE_TYPE (initial_value);
      else
	itype = NULL;
      if (itype && !TYPE_DOMAIN (itype))
	TYPE_DOMAIN (itype) = domain;
      /* The type of the main variant should never be used for arrays
	 of different sizes.  It should only ever be completed with the
	 size of the array.  */
      if (! TYPE_DOMAIN (TYPE_MAIN_VARIANT (type)))
	TYPE_DOMAIN (TYPE_MAIN_VARIANT (type)) = domain;
    }

  pop_obstacks();
  
  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  return value;
}

/* Return zero if something is declared to be a member of type
   CTYPE when in the context of CUR_TYPE.  STRING is the error
   message to print in that case.  Otherwise, quietly return 1.  */

static int
member_function_or_else (ctype, cur_type, string)
     tree ctype, cur_type;
     const char *string;
{
  if (ctype && ctype != cur_type)
    {
      error (string, TYPE_NAME_STRING (ctype));
      return 0;
    }
  return 1;
}

/* Subroutine of `grokdeclarator'.  */

/* Generate errors possibly applicable for a given set of specifiers.
   This is for ARM $7.1.2.  */

static void
bad_specifiers (object, type, virtualp, quals, inlinep, friendp, raises)
     tree object;
     const char *type;
     int virtualp, quals, friendp, raises, inlinep;
{
  if (virtualp)
    cp_error ("`%D' declared as a `virtual' %s", object, type);
  if (inlinep)
    cp_error ("`%D' declared as an `inline' %s", object, type);
  if (quals)
    cp_error ("`const' and `volatile' function specifiers on `%D' invalid in %s declaration",
	      object, type);
  if (friendp)
    cp_error_at ("invalid friend declaration", object);
  if (raises)
    cp_error_at ("invalid exception specifications", object);
}

/* CTYPE is class type, or null if non-class.
   TYPE is type this FUNCTION_DECL should have, either FUNCTION_TYPE
   or METHOD_TYPE.
   DECLARATOR is the function's name.
   VIRTUALP is truthvalue of whether the function is virtual or not.
   FLAGS are to be passed through to `grokclassfn'.
   QUALS are qualifiers indicating whether the function is `const'
   or `volatile'.
   RAISES is a list of exceptions that this function can raise.
   CHECK is 1 if we must find this method in CTYPE, 0 if we should
   not look, and -1 if we should not call `grokclassfn' at all.  

   Returns `NULL_TREE' if something goes wrong, after issuing
   applicable error messages.  */

static tree
grokfndecl (ctype, type, declarator, orig_declarator, virtualp, flags, quals,
	    raises, check, friendp, publicp, inlinep, funcdef_flag,
	    template_count, in_namespace)
     tree ctype, type;
     tree declarator;
     tree orig_declarator;
     int virtualp;
     enum overload_flags flags;
     tree quals, raises;
     int check, friendp, publicp, inlinep, funcdef_flag, template_count;
     tree in_namespace;
{
  tree cname, decl;
  int staticp = ctype && TREE_CODE (type) == FUNCTION_TYPE;
  int has_default_arg = 0;
  tree t;

  if (ctype)
    cname = TREE_CODE (TYPE_NAME (ctype)) == TYPE_DECL
      ? TYPE_IDENTIFIER (ctype) : TYPE_NAME (ctype);
  else
    cname = NULL_TREE;

  if (raises)
    {
      type = build_exception_variant (type, raises);
    }

  decl = build_lang_decl (FUNCTION_DECL, declarator, type);
  /* Propagate volatile out from type to decl. */
  if (TYPE_VOLATILE (type))
    TREE_THIS_VOLATILE (decl) = 1;

  /* If this decl has namespace scope, set that up.  */
  if (in_namespace)
    set_decl_namespace (decl, in_namespace, friendp);
  else if (publicp && ! ctype)
    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

  /* `main' and builtins have implicit 'C' linkage.  */
  if ((MAIN_NAME_P (declarator)
       || (IDENTIFIER_LENGTH (declarator) > 10
	   && IDENTIFIER_POINTER (declarator)[0] == '_'
	   && IDENTIFIER_POINTER (declarator)[1] == '_'
	   && strncmp (IDENTIFIER_POINTER (declarator)+2, "builtin_", 8) == 0))
      && current_lang_name == lang_name_cplusplus
      && ctype == NULL_TREE
      /* NULL_TREE means global namespace.  */
      && DECL_CONTEXT (decl) == NULL_TREE)
    DECL_LANGUAGE (decl) = lang_c;

  /* Should probably propagate const out from type to decl I bet (mrs).  */
  if (staticp)
    {
      DECL_STATIC_FUNCTION_P (decl) = 1;
      DECL_CONTEXT (decl) = ctype;
    }

  if (ctype)
    DECL_CLASS_CONTEXT (decl) = ctype;

  if (ctype == NULL_TREE && DECL_MAIN_P (decl))
    {
      if (processing_template_decl)
	error ("cannot declare `main' to be a template");
      if (inlinep)
	error ("cannot declare `main' to be inline");
      else if (! publicp)
	error ("cannot declare `main' to be static");
      inlinep = 0;
      publicp = 1;
    }

  /* Members of anonymous types and local classes have no linkage; make
     them internal.  */
  if (ctype && (ANON_AGGRNAME_P (TYPE_IDENTIFIER (ctype))
		|| hack_decl_function_context (TYPE_MAIN_DECL (ctype))))
    publicp = 0;

  if (publicp)
    {
      /* [basic.link]: A name with no linkage (notably, the name of a class
	 or enumeration declared in a local scope) shall not be used to
	 declare an entity with linkage.

	 Only check this for public decls for now.  */
      t = no_linkage_check (TREE_TYPE (decl));
      if (t)
	{
	  if (ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
	    {
	      if (DECL_LANGUAGE (decl) == lang_c)
		/* Allow this; it's pretty common in C.  */;
	      else
		cp_pedwarn ("non-local function `%#D' uses anonymous type",
			    decl);
	    }
	  else
	    cp_pedwarn ("non-local function `%#D' uses local type `%T'",
			decl, t);
	}
    }

  TREE_PUBLIC (decl) = publicp;
  if (! publicp)
    {
      DECL_INTERFACE_KNOWN (decl) = 1;
      DECL_NOT_REALLY_EXTERN (decl) = 1;
    }

  if (inlinep)
    DECL_THIS_INLINE (decl) = DECL_INLINE (decl) = 1;

  DECL_EXTERNAL (decl) = 1;
  if (quals != NULL_TREE && TREE_CODE (type) == FUNCTION_TYPE)
    {
      cp_error ("%smember function `%D' cannot have `%T' method qualifier",
		(ctype ? "static " : "non-"), decl, TREE_VALUE (quals));
      quals = NULL_TREE;
    }

  if (IDENTIFIER_OPNAME_P (DECL_NAME (decl)))
    grok_op_properties (decl, virtualp, check < 0);

  if (ctype && hack_decl_function_context (decl))
    DECL_NO_STATIC_CHAIN (decl) = 1;

  for (t = TYPE_ARG_TYPES (TREE_TYPE (decl)); t; t = TREE_CHAIN (t))
    if (TREE_PURPOSE (t)
	&& TREE_CODE (TREE_PURPOSE (t)) == DEFAULT_ARG)
      {
	has_default_arg = 1;
	break;
      }

  if (friendp
      && TREE_CODE (orig_declarator) == TEMPLATE_ID_EXPR)
    {
      if (funcdef_flag)
	cp_error
	  ("defining explicit specialization `%D' in friend declaration",
	   orig_declarator);
      else
	{
	  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
	    {
	      /* Something like `template <class T> friend void f<T>()'.  */
	      cp_error ("template-id `%D' in declaration of primary template", 
			orig_declarator);
	      return NULL_TREE;
	    }


	  /* A friend declaration of the form friend void f<>().  Record
	     the information in the TEMPLATE_ID_EXPR.  */
	  SET_DECL_IMPLICIT_INSTANTIATION (decl);
	  DECL_TEMPLATE_INFO (decl)
	    = perm_tree_cons (TREE_OPERAND (orig_declarator, 0),
			      TREE_OPERAND (orig_declarator, 1),
			      NULL_TREE);

	  if (has_default_arg)
	    {
	      cp_error ("default arguments are not allowed in declaration of friend template specialization `%D'",
			decl);
	      return NULL_TREE;
	    }

	  if (inlinep)
	    {
	      cp_error ("`inline' is not allowed in declaration of friend template specialization `%D'", 
			decl);
	      return NULL_TREE;
	    }
	}
    }

  if (has_default_arg)
    add_defarg_fn (decl);

  /* Plain overloading: will not be grok'd by grokclassfn.  */
  if (! ctype && ! processing_template_decl
      && DECL_LANGUAGE (decl) != lang_c
      && (! DECL_USE_TEMPLATE (decl) || name_mangling_version < 1))
    set_mangled_name_for_decl (decl);

  if (funcdef_flag)
    /* Make the init_value nonzero so pushdecl knows this is not
       tentative.  error_mark_node is replaced later with the BLOCK.  */
    DECL_INITIAL (decl) = error_mark_node;

  /* Caller will do the rest of this.  */
  if (check < 0)
    return decl;

  if (check && funcdef_flag)
    DECL_INITIAL (decl) = error_mark_node;

  if (flags == NO_SPECIAL && ctype && constructor_name (cname) == declarator)
    {
      tree tmp;
      /* Just handle constructors here.  We could do this
	 inside the following if stmt, but I think
	 that the code is more legible by breaking this
	 case out.  See comments below for what each of
	 the following calls is supposed to do.  */
      DECL_CONSTRUCTOR_P (decl) = 1;

      grokclassfn (ctype, decl, flags, quals);

      decl = check_explicit_specialization (orig_declarator, decl,
					    template_count, 
					    2 * (funcdef_flag != 0) + 
					    4 * (friendp != 0));
      if (decl == error_mark_node)
	return NULL_TREE;

      if ((! TYPE_FOR_JAVA (ctype) || check_java_method (decl))
	  && check)
	{
	  tmp = check_classfn (ctype, decl);

	  if (tmp && TREE_CODE (tmp) == TEMPLATE_DECL)
	    tmp = DECL_TEMPLATE_RESULT(tmp);

	  if (tmp && DECL_ARTIFICIAL (tmp))
	    cp_error ("definition of implicitly-declared `%D'", tmp);
	  if (tmp && duplicate_decls (decl, tmp))
	    return tmp;
	}
      if (! grok_ctor_properties (ctype, decl))
	return NULL_TREE;
    }
  else
    {
      tree tmp;

      /* Function gets the ugly name, field gets the nice one.
	 This call may change the type of the function (because
	 of default parameters)!  */
      if (ctype != NULL_TREE)
	grokclassfn (ctype, decl, flags, quals);

      decl = check_explicit_specialization (orig_declarator, decl,
					    template_count, 
					    2 * (funcdef_flag != 0) + 
					    4 * (friendp != 0));
      if (decl == error_mark_node)
	return NULL_TREE;

      if (ctype != NULL_TREE
	  && (! TYPE_FOR_JAVA (ctype) || check_java_method (decl))
	  && check)
	{
	  tmp = check_classfn (ctype, decl);

	  if (tmp && TREE_CODE (tmp) == TEMPLATE_DECL)
	    tmp = DECL_TEMPLATE_RESULT (tmp);
	      
	  if (tmp && DECL_STATIC_FUNCTION_P (tmp)
	      && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	    {
	      /* Remove the `this' parm added by grokclassfn.
	         XXX Isn't this done in start_function, too?  */
	      revert_static_member_fn (&decl, NULL, NULL);
	      last_function_parms = TREE_CHAIN (last_function_parms);
	    }
	  if (tmp && DECL_ARTIFICIAL (tmp))
	    cp_error ("definition of implicitly-declared `%D'", tmp);
	  if (tmp)
	    {
	      /* Attempt to merge the declarations.  This can fail, in
		 the case of some illegal specialization declarations.  */
	      if (!duplicate_decls (decl, tmp))
		cp_error ("no `%#D' member function declared in class `%T'",
			  decl, ctype);
	      return tmp;
	    }
	}

      if (ctype == NULL_TREE || check)
	return decl;

      if (virtualp)
	{
	  DECL_VIRTUAL_P (decl) = 1;
	  if (DECL_VINDEX (decl) == NULL_TREE)
	    DECL_VINDEX (decl) = error_mark_node;
	  IDENTIFIER_VIRTUAL_P (DECL_NAME (decl)) = 1;
	}
    }
  return decl;
}

static tree
grokvardecl (type, declarator, specbits_in, initialized, constp, in_namespace)
     tree type;
     tree declarator;
     RID_BIT_TYPE *specbits_in;
     int initialized;
     int constp;
     tree in_namespace;
{
  tree decl;
  RID_BIT_TYPE specbits;

  specbits = *specbits_in;

  if (TREE_CODE (type) == OFFSET_TYPE)
    {
      /* If you declare a static member so that it
	 can be initialized, the code will reach here.  */
      tree basetype = TYPE_OFFSET_BASETYPE (type);
      type = TREE_TYPE (type);
      decl = build_lang_field_decl (VAR_DECL, declarator, type);
      DECL_CONTEXT (decl) = basetype;
      DECL_CLASS_CONTEXT (decl) = basetype;
      DECL_ASSEMBLER_NAME (decl) = build_static_name (basetype, declarator);
    }
  else
    {
      tree context;

      if (in_namespace)
	context = in_namespace;
      else if (namespace_bindings_p () || RIDBIT_SETP (RID_EXTERN, specbits))
	context = current_namespace;
      else
	context = NULL_TREE;

      decl = build_decl (VAR_DECL, declarator, complete_type (type));

      if (context)
	set_decl_namespace (decl, context, 0);

      context = DECL_CONTEXT (decl);
      if (declarator && context && current_lang_name != lang_name_c)
	DECL_ASSEMBLER_NAME (decl) = build_static_name (context, declarator);
    }

  if (in_namespace)
    set_decl_namespace (decl, in_namespace, 0);

  if (RIDBIT_SETP (RID_EXTERN, specbits))
    {
      DECL_THIS_EXTERN (decl) = 1;
      DECL_EXTERNAL (decl) = !initialized;
    }

  /* In class context, static means one per class,
     public access, and static storage.  */
  if (DECL_CLASS_SCOPE_P (decl))
    {
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
    }
  /* At top level, either `static' or no s.c. makes a definition
     (perhaps tentative), and absence of `static' makes it public.  */
  else if (toplevel_bindings_p ())
    {
      TREE_PUBLIC (decl) = (RIDBIT_NOTSETP (RID_STATIC, specbits)
			    && (DECL_THIS_EXTERN (decl) || ! constp));
      TREE_STATIC (decl) = ! DECL_EXTERNAL (decl);
    }
  /* Not at top level, only `static' makes a static definition.  */
  else
    {
      TREE_STATIC (decl) = !! RIDBIT_SETP (RID_STATIC, specbits);
      TREE_PUBLIC (decl) = DECL_EXTERNAL (decl);
    }

  if (TREE_PUBLIC (decl))
    {
      /* [basic.link]: A name with no linkage (notably, the name of a class
	 or enumeration declared in a local scope) shall not be used to
	 declare an entity with linkage.

	 Only check this for public decls for now.  */
      tree t = no_linkage_check (TREE_TYPE (decl));
      if (t)
	{
	  if (ANON_AGGRNAME_P (TYPE_IDENTIFIER (t)))
	    /* Ignore for now; `enum { foo } e' is pretty common.  */;
	  else
	    cp_pedwarn ("non-local variable `%#D' uses local type `%T'",
			decl, t);
	}
    }

  return decl;
}

/* Create and return a canonical pointer to member function type, for
   TYPE, which is a POINTER_TYPE to a METHOD_TYPE.  */

tree
build_ptrmemfunc_type (type)
     tree type;
{
  tree fields[4];
  tree t;
  tree u;

  /* If a canonical type already exists for this type, use it.  We use
     this method instead of type_hash_canon, because it only does a
     simple equality check on the list of field members.  */

  if ((t = TYPE_GET_PTRMEMFUNC_TYPE (type)))
    return t;

  push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));

  u = make_lang_type (UNION_TYPE);
  SET_IS_AGGR_TYPE (u, 0);
  fields[0] = build_lang_field_decl (FIELD_DECL, pfn_identifier, type);
  fields[1] = build_lang_field_decl (FIELD_DECL, delta2_identifier,
				     delta_type_node);
  finish_builtin_type (u, "__ptrmemfunc_type", fields, 1, ptr_type_node);
  TYPE_NAME (u) = NULL_TREE;

  t = make_lang_type (RECORD_TYPE);

  /* Let the front-end know this is a pointer to member function...  */
  TYPE_PTRMEMFUNC_FLAG (t) = 1;
  /* ... and not really an aggregate.  */
  SET_IS_AGGR_TYPE (t, 0);

  fields[0] = build_lang_field_decl (FIELD_DECL, delta_identifier,
				     delta_type_node);
  fields[1] = build_lang_field_decl (FIELD_DECL, index_identifier,
				     delta_type_node);
  fields[2] = build_lang_field_decl (FIELD_DECL, pfn_or_delta2_identifier, u);
  finish_builtin_type (t, "__ptrmemfunc_type", fields, 2, ptr_type_node);

  pop_obstacks ();

  /* Zap out the name so that the back-end will give us the debugging
     information for this anonymous RECORD_TYPE.  */
  TYPE_NAME (t) = NULL_TREE;

  TYPE_SET_PTRMEMFUNC_TYPE (type, t);

  /* Seems to be wanted.  */
  CLASSTYPE_GOT_SEMICOLON (t) = 1;
  return t;
}

/* DECL is a VAR_DECL defined in-class, whose TYPE is also given.
   Check to see that the definition is valid.  Issue appropriate error
   messages.  Return 1 if the definition is particularly bad, or 0
   otherwise.  */

int
check_static_variable_definition (decl, type)
     tree decl;
     tree type;
{
  /* Motion 10 at San Diego: If a static const integral data member is
     initialized with an integral constant expression, the initializer
     may appear either in the declaration (within the class), or in
     the definition, but not both.  If it appears in the class, the
     member is a member constant.  The file-scope definition is always
     required.  */
  if (CLASS_TYPE_P (type) || TREE_CODE (type) == REFERENCE_TYPE)
    {
      cp_error ("in-class initialization of static data member of non-integral type `%T'", 
		type);
      /* If we just return the declaration, crashes will sometimes
	 occur.  We therefore return void_type_node, as if this was a
	 friend declaration, to cause callers to completely ignore
	 this declaration.  */
      return 1;
    }
  else if (!CP_TYPE_CONST_P (type))
    cp_error ("ANSI C++ forbids in-class initialization of non-const static member `%D'",
	      decl);
  else if (pedantic && !INTEGRAL_TYPE_P (type))
    cp_pedwarn ("ANSI C++ forbids initialization of member constant `%D' of non-integral type `%T'", decl, type);

  return 0;
}

/* Given declspecs and a declarator,
   determine the name and type of the object declared
   and construct a ..._DECL node for it.
   (In one case we can return a ..._TYPE node instead.
    For invalid input we sometimes return 0.)

   DECLSPECS is a chain of tree_list nodes whose value fields
    are the storage classes and type specifiers.

   DECL_CONTEXT says which syntactic context this declaration is in:
     NORMAL for most contexts.  Make a VAR_DECL or FUNCTION_DECL or TYPE_DECL.
     FUNCDEF for a function definition.  Like NORMAL but a few different
      error messages in each case.  Return value may be zero meaning
      this definition is too screwy to try to parse.
     MEMFUNCDEF for a function definition.  Like FUNCDEF but prepares to
      handle member functions (which have FIELD context).
      Return value may be zero meaning this definition is too screwy to
      try to parse.
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     CATCHPARM for a parameter declaration before a catch clause.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
     BITFIELD for a field with specified width.
   INITIALIZED is 1 if the decl has an initializer.

   ATTRLIST is a TREE_LIST node with prefix attributes in TREE_VALUE and
   normal attributes in TREE_PURPOSE, or NULL_TREE.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are interpreted.

   For C++, if there is any monkey business to do, the function which
   calls this one must do it, i.e., prepending instance variables,
   renaming overloaded function names, etc.

   Note that for this C++, it is an error to define a method within a class
   which does not belong to that class.

   Except in the case where SCOPE_REFs are implicitly known (such as
   methods within a class being redundantly qualified),
   declarations which involve SCOPE_REFs are returned as SCOPE_REFs
   (class_name::decl_name).  The caller must also deal with this.

   If a constructor or destructor is seen, and the context is FIELD,
   then the type gains the attribute TREE_HAS_x.  If such a declaration
   is erroneous, NULL_TREE is returned.

   QUALS is used only for FUNCDEF and MEMFUNCDEF cases.  For a member
   function, these are the qualifiers to give to the `this' pointer.

   May return void_type_node if the declarator turned out to be a friend.
   See grokfield for details.  */

enum return_types { return_normal, return_ctor, return_dtor, return_conversion };

tree
grokdeclarator (declarator, declspecs, decl_context, initialized, attrlist)
     tree declspecs;
     tree declarator;
     enum decl_context decl_context;
     int initialized;
     tree attrlist;
{
  RID_BIT_TYPE specbits;
  int nclasses = 0;
  tree spec;
  tree type = NULL_TREE;
  int longlong = 0;
  int constp;
  int restrictp;
  int volatilep;
  int type_quals;
  int virtualp, explicitp, friendp, inlinep, staticp;
  int explicit_int = 0;
  int explicit_char = 0;
  int defaulted_int = 0;
  int opaque_typedef = 0;
  tree typedef_decl = NULL_TREE;
  char *name;
  tree typedef_type = NULL_TREE;
  int funcdef_flag = 0;
  enum tree_code innermost_code = ERROR_MARK;
  int bitfield = 0;
#if 0
  /* See the code below that used this.  */
  tree decl_machine_attr = NULL_TREE;
#endif
  /* Set this to error_mark_node for FIELD_DECLs we could not handle properly.
     All FIELD_DECLs we build here have `init' put into their DECL_INITIAL.  */
  tree init = NULL_TREE;

  /* Keep track of what sort of function is being processed
     so that we can warn about default return values, or explicit
     return values which do not match prescribed defaults.  */
  enum return_types return_type = return_normal;

  tree dname = NULL_TREE;
  tree ctype = current_class_type;
  tree ctor_return_type = NULL_TREE;
  enum overload_flags flags = NO_SPECIAL;
  tree quals = NULL_TREE;
  tree raises = NULL_TREE;
  int template_count = 0;
  tree in_namespace = NULL_TREE;
  tree inner_attrs;
  int ignore_attrs;

  RIDBIT_RESET_ALL (specbits);
  if (decl_context == FUNCDEF)
    funcdef_flag = 1, decl_context = NORMAL;
  else if (decl_context == MEMFUNCDEF)
    funcdef_flag = -1, decl_context = FIELD;
  else if (decl_context == BITFIELD)
    bitfield = 1, decl_context = FIELD;

  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  {
    tree *next = &declarator;
    register tree decl;
    name = NULL;

    while (next && *next)
      {
	decl = *next;
	switch (TREE_CODE (decl))
	  {
	  case TREE_LIST:
	    /* For attributes.  */
	    next = &TREE_VALUE (decl);
	    break;

	  case COND_EXPR:
	    ctype = NULL_TREE;
	    next = &TREE_OPERAND (decl, 0);
	    break;

	  case BIT_NOT_EXPR:	/* For C++ destructors!  */
	    {
	      tree name = TREE_OPERAND (decl, 0);
	      tree rename = NULL_TREE;

	      my_friendly_assert (flags == NO_SPECIAL, 152);
	      flags = DTOR_FLAG;
	      return_type = return_dtor;
	      if (TREE_CODE (name) == TYPE_DECL)
		TREE_OPERAND (decl, 0) = name = constructor_name (name);
	      my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 153);
	      if (ctype == NULL_TREE)
		{
		  if (current_class_type == NULL_TREE)
		    {
		      error ("destructors must be member functions");
		      flags = NO_SPECIAL;
		    }
		  else
		    {
		      tree t = constructor_name (current_class_name);
		      if (t != name)
			rename = t;
		    }
		}
	      else
		{
		  tree t = constructor_name (ctype);
		  if (t != name)
		    rename = t;
		}

	      if (rename)
		{
		  cp_error ("destructor `%T' must match class name `%T'",
			    name, rename);
		  TREE_OPERAND (decl, 0) = rename;
		}
	      next = &name;
	    }
	    break;

	  case ADDR_EXPR:	/* C++ reference declaration */
	    /* Fall through. */
	  case ARRAY_REF:
	  case INDIRECT_REF:
	    ctype = NULL_TREE;
	    innermost_code = TREE_CODE (decl);
	    next = &TREE_OPERAND (decl, 0);
	    break;

	  case CALL_EXPR:
	    if (parmlist_is_exprlist (TREE_OPERAND (decl, 1)))
	      {
		/* This is actually a variable declaration using
		   constructor syntax.  We need to call start_decl and
		   cp_finish_decl so we can get the variable
		   initialized...  */

		tree attributes, prefix_attributes;

		*next = TREE_OPERAND (decl, 0);
		init = TREE_OPERAND (decl, 1);

		if (attrlist)
		  {
		    attributes = TREE_PURPOSE (attrlist);
		    prefix_attributes = TREE_VALUE (attrlist);
		  }
		else
		  {
		    attributes = NULL_TREE;
		    prefix_attributes = NULL_TREE;
		  }

		decl = start_decl (declarator, declspecs, 1,
				   attributes, prefix_attributes);
		if (decl)
		  {
		    /* Look for __unused__ attribute */
		    if (TREE_USED (TREE_TYPE (decl)))
		      TREE_USED (decl) = 1;
		    finish_decl (decl, init, NULL_TREE);
		  }
		else
		  cp_error ("invalid declarator");
		return 0;
	      }
	    innermost_code = TREE_CODE (decl);
	    if (decl_context == FIELD && ctype == NULL_TREE)
	      ctype = current_class_type;
	    if (ctype
		&& TREE_OPERAND (decl, 0)
		&& (TREE_CODE (TREE_OPERAND (decl, 0)) == TYPE_DECL
		    && ((DECL_NAME (TREE_OPERAND (decl, 0))
			 == constructor_name_full (ctype))
			|| (DECL_NAME (TREE_OPERAND (decl, 0))
			    == constructor_name (ctype)))))
	      TREE_OPERAND (decl, 0) = constructor_name (ctype);
	    next = &TREE_OPERAND (decl, 0);
	    decl = *next;
	    if (ctype != NULL_TREE
		&& decl != NULL_TREE && flags != DTOR_FLAG
		&& decl == constructor_name (ctype))
	      {
		return_type = return_ctor;
		ctor_return_type = ctype;
	      }
	    ctype = NULL_TREE;
	    break;
	    
	  case TEMPLATE_ID_EXPR:
	      {
		tree fns = TREE_OPERAND (decl, 0);

		if (TREE_CODE (fns) == LOOKUP_EXPR)
		  fns = TREE_OPERAND (fns, 0);

		dname = fns;
		if (TREE_CODE (dname) == COMPONENT_REF)
		  dname = TREE_OPERAND (dname, 1);
		if (TREE_CODE (dname) != IDENTIFIER_NODE)
		  {
		    my_friendly_assert (is_overloaded_fn (dname),
					19990331);
		    dname = DECL_NAME (get_first_fn (dname));
		  }
	      }
	  /* Fall through. */

	  case IDENTIFIER_NODE:
	    if (TREE_CODE (decl) == IDENTIFIER_NODE)
	      dname = decl;

	    next = 0;

	    if (is_rid (dname))
	      {
		cp_error ("declarator-id missing; using reserved word `%D'",
			  dname);
		name = IDENTIFIER_POINTER (dname);
	      }
	    if (! IDENTIFIER_OPNAME_P (dname)
		/* GNU/Linux headers use '__op'.  Arrgh.  */
		|| (IDENTIFIER_TYPENAME_P (dname) && ! TREE_TYPE (dname)))
	      name = IDENTIFIER_POINTER (dname);
	    else
	      {
		if (IDENTIFIER_TYPENAME_P (dname))
		  {
		    my_friendly_assert (flags == NO_SPECIAL, 154);
		    flags = TYPENAME_FLAG;
		    ctor_return_type = TREE_TYPE (dname);
		    return_type = return_conversion;
		  }
		name = operator_name_string (dname);
	      }
	    break;

	    /* C++ extension */
	  case SCOPE_REF:
	    {
	      /* Perform error checking, and decide on a ctype.  */
	      tree cname = TREE_OPERAND (decl, 0);
	      if (cname == NULL_TREE)
		ctype = NULL_TREE;
	      else if (TREE_CODE (cname) == NAMESPACE_DECL)
		{
		  ctype = NULL_TREE;
		  in_namespace = TREE_OPERAND (decl, 0);
		  TREE_OPERAND (decl, 0) = NULL_TREE;
		}
	      else if (! is_aggr_type (cname, 1))
		TREE_OPERAND (decl, 0) = NULL_TREE;
	      /* Must test TREE_OPERAND (decl, 1), in case user gives
		 us `typedef (class::memfunc)(int); memfunc *memfuncptr;'  */
	      else if (TREE_OPERAND (decl, 1)
		       && TREE_CODE (TREE_OPERAND (decl, 1)) == INDIRECT_REF)
		ctype = cname;
	      else if (TREE_CODE (cname) == TEMPLATE_TYPE_PARM
		       || TREE_CODE (cname) == TEMPLATE_TEMPLATE_PARM)
		{
		  cp_error ("`%T::%D' is not a valid declarator", cname,
			    TREE_OPERAND (decl, 1));
		  cp_error ("  perhaps you want `typename %T::%D' to make it a type",
			    cname, TREE_OPERAND (decl, 1));
		  return void_type_node;
		}
	      else if (ctype == NULL_TREE)
		ctype = cname;
	      else if (TREE_COMPLEXITY (decl) == current_class_depth)
		TREE_OPERAND (decl, 0) = ctype;
	      else
		{
		  if (! UNIQUELY_DERIVED_FROM_P (cname, ctype))
		    {
		      cp_error ("type `%T' is not derived from type `%T'",
				cname, ctype);
		      TREE_OPERAND (decl, 0) = NULL_TREE;
		    }
		  else
		    ctype = cname;
		}

	      if (ctype && TREE_CODE (TREE_OPERAND (decl, 1)) == TYPE_DECL
		  && ((DECL_NAME (TREE_OPERAND (decl, 1))
		       == constructor_name_full (ctype))
		      || (DECL_NAME (TREE_OPERAND (decl, 1))
			  == constructor_name (ctype))))
		TREE_OPERAND (decl, 1) = constructor_name (ctype);
	      next = &TREE_OPERAND (decl, 1);
	      decl = *next;
	      if (ctype)
		{
		  if (TREE_CODE (decl) == IDENTIFIER_NODE
		      && constructor_name (ctype) == decl)
		    {
		      return_type = return_ctor;
		      ctor_return_type = ctype;
		    }
		  else if (TREE_CODE (decl) == BIT_NOT_EXPR
			   && TREE_CODE (TREE_OPERAND (decl, 0)) == IDENTIFIER_NODE
			   && (constructor_name (ctype) == TREE_OPERAND (decl, 0)
			       || constructor_name_full (ctype) == TREE_OPERAND (decl, 0)))
		    {
		      return_type = return_dtor;
		      ctor_return_type = ctype;
		      flags = DTOR_FLAG;
		      TREE_OPERAND (decl, 0) = constructor_name (ctype);
		      next = &TREE_OPERAND (decl, 0);
		    }
		}
	    }
	    break;

	  case ERROR_MARK:
	    next = 0;
	    break;

	  case TYPE_DECL:
	    /* Parse error puts this typespec where
	       a declarator should go.  */
	    cp_error ("`%T' specified as declarator-id", DECL_NAME (decl));
	    if (TREE_TYPE (decl) == current_class_type)
	      cp_error ("  perhaps you want `%T' for a constructor",
			current_class_name);
	    dname = DECL_NAME (decl);
	    name = IDENTIFIER_POINTER (dname);

	    /* Avoid giving two errors for this.  */
	    IDENTIFIER_CLASS_VALUE (dname) = NULL_TREE;

	    declspecs = temp_tree_cons (NULL_TREE, integer_type_node,
					declspecs);
	    *next = dname;
	    next = 0;
	    break;

	  default:
	    cp_compiler_error ("`%D' as declarator", decl);
	    return 0; /* We used to do a 155 abort here.  */
	  }
      }
    if (name == NULL)
      name = "type name";
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  if (((dname && IDENTIFIER_OPNAME_P (dname)) || flags == TYPENAME_FLAG)
      && innermost_code != CALL_EXPR
      && ! (ctype && declspecs == NULL_TREE))
    {
      cp_error ("declaration of `%D' as non-function", dname);
      return void_type_node;
    }

  /* Anything declared one level down from the top level
     must be one of the parameters of a function
     (because the body is at least two levels down).  */

  /* This heuristic cannot be applied to C++ nodes! Fixed, however,
     by not allowing C++ class definitions to specify their parameters
     with xdecls (must be spec.d in the parmlist).

     Since we now wait to push a class scope until we are sure that
     we are in a legitimate method context, we must set oldcname
     explicitly (since current_class_name is not yet alive).

     We also want to avoid calling this a PARM if it is in a namespace.  */

  if (decl_context == NORMAL && ! namespace_bindings_p ()
      && ! pseudo_global_level_p ())
    {
      struct binding_level *b = current_binding_level;
      current_binding_level = b->level_chain;
      if (current_binding_level != 0 && toplevel_bindings_p ())
	decl_context = PARM;
      current_binding_level = b;
    }

  /* Look through the decl specs and record which ones appear.
     Some typespecs are defined as built-in typenames.
     Others, the ones that are modifiers of other types,
     are represented by bits in SPECBITS: set the bits for
     the modifiers that appear.  Storage class keywords are also in SPECBITS.

     If there is a typedef name or a type, store the type in TYPE.
     This includes builtin typedefs such as `int'.

     Set EXPLICIT_INT if the type is `int' or `char' and did not
     come from a user typedef.

     Set LONGLONG if `long' is mentioned twice.

     For C++, constructors and destructors have their own fast treatment.  */

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      register int i;
      register tree id;

      /* Certain parse errors slip through.  For example,
	 `int class;' is not caught by the parser. Try
	 weakly to recover here.  */
      if (TREE_CODE (spec) != TREE_LIST)
	return 0;

      id = TREE_VALUE (spec);

      if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  if (id == ridpointers[(int) RID_INT]
	      || id == ridpointers[(int) RID_CHAR]
	      || id == ridpointers[(int) RID_BOOL]
	      || id == ridpointers[(int) RID_WCHAR])
	    {
	      if (type)
		{
		  if (id == ridpointers[(int) RID_BOOL])
		    error ("`bool' is now a keyword");
		  else
		    cp_error ("extraneous `%T' ignored", id);
		}
	      else
		{
		  if (id == ridpointers[(int) RID_INT])
		    explicit_int = 1;
		  else if (id == ridpointers[(int) RID_CHAR])
		    explicit_char = 1;
		  type = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (id));
		}
	      goto found;
	    }
	  /* C++ aggregate types.  */
	  if (IDENTIFIER_HAS_TYPE_VALUE (id))
	    {
	      if (type)
		cp_error ("multiple declarations `%T' and `%T'", type, id);
	      else
		type = IDENTIFIER_TYPE_VALUE (id);
	      goto found;
	    }

	  for (i = (int) RID_FIRST_MODIFIER; i <= (int) RID_LAST_MODIFIER; i++)
	    {
	      if (ridpointers[i] == id)
		{
		  if (i == (int) RID_LONG && RIDBIT_SETP (i, specbits))
		    {
		      if (pedantic && ! in_system_header && warn_long_long)
			pedwarn ("ANSI C++ does not support `long long'");
		      if (longlong)
			error ("`long long long' is too long for GCC");
		      else
			longlong = 1;
		    }
		  else if (RIDBIT_SETP (i, specbits))
		    pedwarn ("duplicate `%s'", IDENTIFIER_POINTER (id));
		  RIDBIT_SET (i, specbits);
		  goto found;
		}
	    }
	}
      /* C++ aggregate types.  */
      else if (TREE_CODE (id) == TYPE_DECL || TREE_CODE (id) == TEMPLATE_DECL)
	{
	  if (type)
	    cp_error ("multiple declarations `%T' and `%T'", type,
		      TREE_TYPE (id));
	  else
	    {
	      type = TREE_TYPE (id);
	      TREE_VALUE (spec) = type;
	    }
	  goto found;
	}
      if (type)
	error ("two or more data types in declaration of `%s'", name);
      else if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  register tree t = lookup_name (id, 1);
	  if (!t || TREE_CODE (t) != TYPE_DECL)
	    error ("`%s' fails to be a typedef or built in type",
		   IDENTIFIER_POINTER (id));
	  else
	    {
	      type = TREE_TYPE (t);
#if 0
	      /* See the code below that used this.  */
	      decl_machine_attr = DECL_MACHINE_ATTRIBUTES (id);
#endif
	      typedef_decl = t;
	    }
	}
      else if (id != error_mark_node)
	/* Can't change CLASS nodes into RECORD nodes here!  */
	type = id;

    found: ;
    }

  typedef_type = type;

  /* No type at all: default to `int', and set DEFAULTED_INT
     because it was not a user-defined typedef.
     Except when we have a `typedef' inside a signature, in
     which case the type defaults to `unknown type' and is
     instantiated when assigning to a signature pointer or ref.  */

  if (type == NULL_TREE
      && (RIDBIT_SETP (RID_SIGNED, specbits)
	  || RIDBIT_SETP (RID_UNSIGNED, specbits)
	  || RIDBIT_SETP (RID_LONG, specbits)
	  || RIDBIT_SETP (RID_SHORT, specbits)))
    {
      /* These imply 'int'.  */
      type = integer_type_node;
      defaulted_int = 1;
    }

  if (type == NULL_TREE)
    {
      explicit_int = -1;
      if (return_type == return_dtor)
	type = void_type_node;
      else if (return_type == return_ctor)
	type = build_pointer_type (ctor_return_type);
      else if (return_type == return_conversion)
	type = ctor_return_type;
      else if (current_class_type
	       && IS_SIGNATURE (current_class_type)
	       && RIDBIT_SETP (RID_TYPEDEF, specbits)
	       && (decl_context == FIELD || decl_context == NORMAL))
	{
	  explicit_int = 0;
	  opaque_typedef = 1;
	  type = copy_node (opaque_type_node);
	}
      else
	{
	  /* We handle `main' specially here, because 'main () { }' is so
	     common.  With no options, it is allowed.  With -Wreturn-type,
	     it is a warning.  It is only an error with -pedantic-errors.  */
	  int is_main = (funcdef_flag
			 && MAIN_NAME_P (dname)
			 && ctype == NULL_TREE
			 && in_namespace == NULL_TREE
			 && current_namespace == global_namespace);

	  if (in_system_header)
	    /* Allow it, sigh.  */;
	  else if (pedantic || ! is_main)
	    cp_pedwarn ("ANSI C++ forbids declaration `%D' with no type",
			dname);
	  else if (warn_return_type)
	    cp_warning ("ANSI C++ forbids declaration `%D' with no type",
			dname);

	  type = integer_type_node;
	}
    }
  else if (return_type == return_dtor)
    {
      error ("return type specification for destructor invalid");
      type = void_type_node;
    }
  else if (return_type == return_ctor)
    {
      error ("return type specification for constructor invalid");
      type = build_pointer_type (ctor_return_type);
    }
  else if (return_type == return_conversion)
    {
      if (!same_type_p (type, ctor_return_type))
	cp_error ("operator `%T' declared to return `%T'",
		  ctor_return_type, type);
      else
	cp_pedwarn ("return type specified for `operator %T'",
		    ctor_return_type);

      type = ctor_return_type;
    }

  ctype = NULL_TREE;

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Long double is a special combination.  */

  if (RIDBIT_SETP (RID_LONG, specbits)
      && TYPE_MAIN_VARIANT (type) == double_type_node)
    {
      RIDBIT_RESET (RID_LONG, specbits);
      type = build_qualified_type (long_double_type_node, 
				   CP_TYPE_QUALS (type));
    }

  /* Check all other uses of type modifiers.  */

  if (RIDBIT_SETP (RID_UNSIGNED, specbits)
      || RIDBIT_SETP (RID_SIGNED, specbits)
      || RIDBIT_SETP (RID_LONG, specbits)
      || RIDBIT_SETP (RID_SHORT, specbits))
    {
      int ok = 0;

      if (TREE_CODE (type) == REAL_TYPE)
	error ("short, signed or unsigned invalid for `%s'", name);
      else if (TREE_CODE (type) != INTEGER_TYPE)
	error ("long, short, signed or unsigned invalid for `%s'", name);
      else if (RIDBIT_SETP (RID_LONG, specbits)
	       && RIDBIT_SETP (RID_SHORT, specbits))
	error ("long and short specified together for `%s'", name);
      else if ((RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits))
	       && explicit_char)
	error ("long or short specified with char for `%s'", name);
      else if ((RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits))
	       && TREE_CODE (type) == REAL_TYPE)
	error ("long or short specified with floating type for `%s'", name);
      else if (RIDBIT_SETP (RID_SIGNED, specbits)
	       && RIDBIT_SETP (RID_UNSIGNED, specbits))
	error ("signed and unsigned given together for `%s'", name);
      else
	{
	  ok = 1;
	  if (!explicit_int && !defaulted_int && !explicit_char && pedantic)
	    {
	      pedwarn ("long, short, signed or unsigned used invalidly for `%s'",
		       name);
	      if (flag_pedantic_errors)
		ok = 0;
	    }
	}

      /* Discard the type modifiers if they are invalid.  */
      if (! ok)
	{
	  RIDBIT_RESET (RID_UNSIGNED, specbits);
	  RIDBIT_RESET (RID_SIGNED, specbits);
	  RIDBIT_RESET (RID_LONG, specbits);
	  RIDBIT_RESET (RID_SHORT, specbits);
	  longlong = 0;
	}
    }

  if (RIDBIT_SETP (RID_COMPLEX, specbits)
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    {
      error ("complex invalid for `%s'", name);
      RIDBIT_RESET (RID_COMPLEX, specbits);
    }

  /* Decide whether an integer type is signed or not.
     Optionally treat bitfields as signed by default.  */
  if (RIDBIT_SETP (RID_UNSIGNED, specbits)
      || (bitfield && ! flag_signed_bitfields
	  && (explicit_int || defaulted_int || explicit_char
	      /* A typedef for plain `int' without `signed'
		 can be controlled just like plain `int'.  */
	      || ! (typedef_decl != NULL_TREE
		    && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	  && TREE_CODE (type) != ENUMERAL_TYPE
	  && RIDBIT_NOTSETP (RID_SIGNED, specbits)))
    {
      if (longlong)
	type = long_long_unsigned_type_node;
      else if (RIDBIT_SETP (RID_LONG, specbits))
	type = long_unsigned_type_node;
      else if (RIDBIT_SETP (RID_SHORT, specbits))
	type = short_unsigned_type_node;
      else if (type == char_type_node)
	type = unsigned_char_type_node;
      else if (typedef_decl)
	type = unsigned_type (type);
      else
	type = unsigned_type_node;
    }
  else if (RIDBIT_SETP (RID_SIGNED, specbits)
	   && type == char_type_node)
    type = signed_char_type_node;
  else if (longlong)
    type = long_long_integer_type_node;
  else if (RIDBIT_SETP (RID_LONG, specbits))
    type = long_integer_type_node;
  else if (RIDBIT_SETP (RID_SHORT, specbits))
    type = short_integer_type_node;

  if (RIDBIT_SETP (RID_COMPLEX, specbits))
    {
      /* If we just have "complex", it is equivalent to
	 "complex double", but if any modifiers at all are specified it is
	 the complex form of TYPE.  E.g, "complex short" is
	 "complex short int".  */

      if (defaulted_int && ! longlong
	  && ! (RIDBIT_SETP (RID_LONG, specbits)
		|| RIDBIT_SETP (RID_SHORT, specbits)
		|| RIDBIT_SETP (RID_SIGNED, specbits)
		|| RIDBIT_SETP (RID_UNSIGNED, specbits)))
	type = complex_double_type_node;
      else if (type == integer_type_node)
	type = complex_integer_type_node;
      else if (type == float_type_node)
	type = complex_float_type_node;
      else if (type == double_type_node)
	type = complex_double_type_node;
      else if (type == long_double_type_node)
	type = complex_long_double_type_node;
      else
	type = build_complex_type (type);
    }

  if (return_type == return_conversion 
      && (RIDBIT_SETP (RID_CONST, specbits)
	  || RIDBIT_SETP (RID_VOLATILE, specbits)
	  || RIDBIT_SETP (RID_RESTRICT, specbits)))
    cp_error ("qualifiers are not allowed on declaration of `operator %T'",
	      ctor_return_type);

  /* Set CONSTP if this declaration is `const', whether by
     explicit specification or via a typedef.
     Likewise for VOLATILEP.  */

  constp = !! RIDBIT_SETP (RID_CONST, specbits) + CP_TYPE_CONST_P (type);
  restrictp = 
    !! RIDBIT_SETP (RID_RESTRICT, specbits) + CP_TYPE_RESTRICT_P (type);
  volatilep = 
    !! RIDBIT_SETP (RID_VOLATILE, specbits) + CP_TYPE_VOLATILE_P (type);
  type_quals = ((constp ? TYPE_QUAL_CONST : 0)
		| (restrictp ? TYPE_QUAL_RESTRICT : 0)
		| (volatilep ? TYPE_QUAL_VOLATILE : 0));
  type = cp_build_qualified_type (type, type_quals);
  staticp = 0;
  inlinep = !! RIDBIT_SETP (RID_INLINE, specbits);
  virtualp = RIDBIT_SETP (RID_VIRTUAL, specbits);
  RIDBIT_RESET (RID_VIRTUAL, specbits);
  explicitp = RIDBIT_SETP (RID_EXPLICIT, specbits) != 0;
  RIDBIT_RESET (RID_EXPLICIT, specbits);

  if (RIDBIT_SETP (RID_STATIC, specbits))
    staticp = 1 + (decl_context == FIELD);

  if (virtualp && staticp == 2)
    {
      cp_error ("member `%D' cannot be declared both virtual and static",
		dname);
      staticp = 0;
    }
  friendp = RIDBIT_SETP (RID_FRIEND, specbits);
  RIDBIT_RESET (RID_FRIEND, specbits);

  /* $7.1.2, Function specifiers */
  if (friendp && explicitp)
    error ("only declarations of constructors can be `explicit'");

  if (RIDBIT_SETP (RID_MUTABLE, specbits))
    {
      if (decl_context == PARM)
	{
	  error ("non-member `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
      else if (friendp || decl_context == TYPENAME)
	{
	  error ("non-object member `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
    }

  /* Warn if two storage classes are given. Default to `auto'.  */

  if (RIDBIT_ANY_SET (specbits))
    {
      if (RIDBIT_SETP (RID_STATIC, specbits)) nclasses++;
      if (RIDBIT_SETP (RID_EXTERN, specbits)) nclasses++;
      if (decl_context == PARM && nclasses > 0)
	error ("storage class specifiers invalid in parameter declarations");
      if (RIDBIT_SETP (RID_TYPEDEF, specbits))
	{
	  if (decl_context == PARM)
	    error ("typedef declaration invalid in parameter declaration");
	  nclasses++;
	}
      if (RIDBIT_SETP (RID_AUTO, specbits)) nclasses++;
      if (RIDBIT_SETP (RID_REGISTER, specbits)) nclasses++;
    }

  /* Give error if `virtual' is used outside of class declaration.  */
  if (virtualp
      && (current_class_name == NULL_TREE || decl_context != FIELD))
    {
      error ("virtual outside class declaration");
      virtualp = 0;
    }
  if (current_class_name == NULL_TREE && RIDBIT_SETP (RID_MUTABLE, specbits))
    {
      error ("only members can be declared mutable");
      RIDBIT_RESET (RID_MUTABLE, specbits);
    }

  /* Static anonymous unions are dealt with here.  */
  if (staticp && decl_context == TYPENAME
      && TREE_CODE (declspecs) == TREE_LIST
      && ANON_UNION_TYPE_P (TREE_VALUE (declspecs)))
    decl_context = FIELD;

  /* Give error if `const,' `volatile,' `inline,' `friend,' or `virtual'
     is used in a signature member function declaration.  */
  if (decl_context == FIELD
      && IS_SIGNATURE (current_class_type)
      && RIDBIT_NOTSETP (RID_TYPEDEF, specbits))
    {
      if (type_quals != TYPE_UNQUALIFIED)
	{
	  error ("type qualifiers specified for signature member function `%s'", name);
	  type_quals = TYPE_UNQUALIFIED;
	}
      if (inlinep)
	{
	  error ("`inline' specified for signature member function `%s'", name);
	  /* Later, we'll make signature member functions inline.  */
	  inlinep = 0;
	}
      if (friendp)
	{
	  error ("`friend' declaration in signature definition");
	  friendp = 0;
	}
      if (virtualp)
	{
	  error ("`virtual' specified for signature member function `%s'",
		 name);
	  /* Later, we'll make signature member functions virtual.  */
	  virtualp = 0;
	}
    }

  /* Warn about storage classes that are invalid for certain
     kinds of declarations (parameters, typenames, etc.).  */

  if (nclasses > 1)
    error ("multiple storage classes in declaration of `%s'", name);
  else if (decl_context != NORMAL && nclasses > 0)
    {
      if ((decl_context == PARM || decl_context == CATCHPARM)
	  && (RIDBIT_SETP (RID_REGISTER, specbits)
	      || RIDBIT_SETP (RID_AUTO, specbits)))
	;
      else if (RIDBIT_SETP (RID_TYPEDEF, specbits))
	;
      else if (decl_context == FIELD
	       && ! IS_SIGNATURE (current_class_type)
 	       /* C++ allows static class elements  */
 	       && RIDBIT_SETP (RID_STATIC, specbits))
 	/* C++ also allows inlines and signed and unsigned elements,
 	   but in those cases we don't come in here.  */
	;
      else
	{
	  if (decl_context == FIELD)
	    {
	      tree tmp = NULL_TREE;
	      register int op = 0;

	      if (declarator)
		{
		  /* Avoid trying to get an operand off an identifier node.  */ 
		  if (TREE_CODE (declarator) == IDENTIFIER_NODE)
		    tmp = declarator;
		  else
		    tmp = TREE_OPERAND (declarator, 0);
		  op = IDENTIFIER_OPNAME_P (tmp);
		}
	      error ("storage class specified for %s `%s'",
		     IS_SIGNATURE (current_class_type)
		     ? (op
			? "signature member operator"
			: "signature member function")
		     : (op ? "member operator" : "field"),
		     op ? operator_name_string (tmp) : name);
	    }
	  else
	    error (((decl_context == PARM || decl_context == CATCHPARM)
		    ? "storage class specified for parameter `%s'"
		    : "storage class specified for typename"), name);
	  RIDBIT_RESET (RID_REGISTER, specbits);
	  RIDBIT_RESET (RID_AUTO, specbits);
	  RIDBIT_RESET (RID_EXTERN, specbits);

	  if (decl_context == FIELD && IS_SIGNATURE (current_class_type))
	    {
	      RIDBIT_RESET (RID_STATIC, specbits);
	      staticp = 0;
	    }
	}
    }
  else if (RIDBIT_SETP (RID_EXTERN, specbits) && initialized && !funcdef_flag)
    {
      if (toplevel_bindings_p ())
	{
	  /* It's common practice (and completely valid) to have a const
	     be initialized and declared extern.  */
	  if (!(type_quals & TYPE_QUAL_CONST))
	    warning ("`%s' initialized and declared `extern'", name);
	}
      else
	error ("`%s' has both `extern' and initializer", name);
    }
  else if (RIDBIT_SETP (RID_EXTERN, specbits) && funcdef_flag
	   && ! toplevel_bindings_p ())
    error ("nested function `%s' declared `extern'", name);
  else if (toplevel_bindings_p ())
    {
      if (RIDBIT_SETP (RID_AUTO, specbits))
	error ("top-level declaration of `%s' specifies `auto'", name);
    }

  if (nclasses > 0 && friendp)
    error ("storage class specifiers invalid in friend function declarations");

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an absolute declarator).  */

  inner_attrs = NULL_TREE;
  ignore_attrs = 0;  

  while (declarator && TREE_CODE (declarator) != IDENTIFIER_NODE
	 && TREE_CODE (declarator) != TEMPLATE_ID_EXPR)
    {
      /* Each level of DECLARATOR is either an ARRAY_REF (for ...[..]),
	 an INDIRECT_REF (for *...),
	 a CALL_EXPR (for ...(...)),
	 an identifier (for the name being declared)
	 or a null pointer (for the place in an absolute declarator
	 where the name was omitted).
	 For the last two cases, we have just exited the loop.

	 For C++ it could also be
	 a SCOPE_REF (for class :: ...).  In this case, we have converted
	 sensible names to types, and those are the values we use to
	 qualify the member name.
	 an ADDR_EXPR (for &...),
	 a BIT_NOT_EXPR (for destructors)

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (type == error_mark_node)
	{
	  if (TREE_CODE (declarator) == SCOPE_REF)
	    declarator = TREE_OPERAND (declarator, 1);
	  else
	    declarator = TREE_OPERAND (declarator, 0);
	  continue;
	}
      if (quals != NULL_TREE
	  && (declarator == NULL_TREE
	      || TREE_CODE (declarator) != SCOPE_REF))
	{
	  if (ctype == NULL_TREE && TREE_CODE (type) == METHOD_TYPE)
	    ctype = TYPE_METHOD_BASETYPE (type);
	  if (ctype != NULL_TREE)
	    {
	      tree dummy = build_decl (TYPE_DECL, NULL_TREE, type);
	      ctype = grok_method_quals (ctype, dummy, quals);
	      type = TREE_TYPE (dummy);
	      quals = NULL_TREE;
	    }
	}

      /* See the comment for the TREE_LIST case, below.  */
      if (ignore_attrs)
	ignore_attrs = 0;
      else if (inner_attrs)
	{
	  decl_attributes (type, inner_attrs, NULL_TREE);
	  inner_attrs = NULL_TREE;
	}

      switch (TREE_CODE (declarator))
	{
	case TREE_LIST:
	  {
	    /* We encode a declarator with embedded attributes using
	       a TREE_LIST.  The attributes apply to the declarator
	       directly inside them, so we have to skip an iteration
	       before applying them to the type.  If the declarator just
	       inside is the declarator-id, we apply the attrs to the
	       decl itself.  */
	    inner_attrs = TREE_PURPOSE (declarator);
	    ignore_attrs = 1;
	    declarator = TREE_VALUE (declarator);
	  }
	  break;

	case ARRAY_REF:
	  {
	    register tree itype = NULL_TREE;
	    register tree size = TREE_OPERAND (declarator, 1);
	    /* The index is a signed object `sizetype' bits wide.  */
	    tree index_type = signed_type (sizetype);

	    declarator = TREE_OPERAND (declarator, 0);

	    /* Check for some types that there cannot be arrays of.  */

	    if (TREE_CODE (type) == VOID_TYPE)
	      {
		cp_error ("declaration of `%D' as array of voids", dname);
		type = error_mark_node;
	      }

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		cp_error ("declaration of `%D' as array of functions", dname);
		type = error_mark_node;
	      }

	    /* ARM $8.4.3: Since you can't have a pointer to a reference,
	       you can't have arrays of references.  If we allowed them,
	       then we'd be saying x[i] is valid for an array x, but
	       then you'd have to ask: what does `*(x + i)' mean?  */
	    if (TREE_CODE (type) == REFERENCE_TYPE)
	      {
		if (decl_context == TYPENAME)
		  cp_error ("cannot make arrays of references");
		else
		  cp_error ("declaration of `%D' as array of references",
			    dname);
		type = error_mark_node;
	      }

	    if (TREE_CODE (type) == OFFSET_TYPE)
	      {
		  cp_error ("declaration of `%D' as array of data members",
			    dname);
		type = error_mark_node;
	      }

	    if (TREE_CODE (type) == METHOD_TYPE)
	      {
		cp_error ("declaration of `%D' as array of function members",
			  dname);
		type = error_mark_node;
	      }

	    if (size == error_mark_node)
	      type = error_mark_node;
	    else if (TREE_CODE (type) == ARRAY_TYPE && !TYPE_DOMAIN (type))
	      {
		/* [dcl.array]

		   the constant expressions that specify the bounds of
		   the arrays can be omitted only for the first member
		   of the sequence.  */
		cp_error ("declaration of `%D' as multidimensional array",
			  dname);
		cp_error ("must have bounds for all dimensions except the first");
		type = error_mark_node;
	      }

	    if (type == error_mark_node)
	      continue;

	    /* VC++ spells a zero-sized array with [].  */
	    if (size == NULL_TREE && decl_context == FIELD && !	staticp
		&& ! RIDBIT_SETP (RID_TYPEDEF, specbits))
	      size = integer_zero_node;

	    if (size)
	      {
		/* Must suspend_momentary here because the index
		   type may need to live until the end of the function.
		   For example, it is used in the declaration of a
		   variable which requires destructing at the end of
		   the function; then build_vec_delete will need this
		   value.  */
		int yes = suspend_momentary ();
		/* Might be a cast. */
		if (TREE_CODE (size) == NOP_EXPR
		    && TREE_TYPE (size) == TREE_TYPE (TREE_OPERAND (size, 0)))
		  size = TREE_OPERAND (size, 0);

		/* If this involves a template parameter, it will be a
		   constant at instantiation time, but we don't know
		   what the value is yet.  Even if no template
		   parameters are involved, we may an expression that
		   is not a constant; we don't even simplify `1 + 2'
		   when processing a template.  */
		if (processing_template_decl)
		  {
		    /* Resolve a qualified reference to an enumerator or
		       static const data member of ours.  */
		    if (TREE_CODE (size) == SCOPE_REF
			&& TREE_OPERAND (size, 0) == current_class_type)
		      {
			tree t = lookup_field (current_class_type,
					       TREE_OPERAND (size, 1), 0, 0);
			if (t)
			  size = t;
		      }

		    itype = build_index_type (build_min
		      (MINUS_EXPR, sizetype, size, integer_one_node));
		    goto dont_grok_size;
		  }

		if (TREE_CODE (TREE_TYPE (size)) != INTEGER_TYPE
		    && TREE_CODE (TREE_TYPE (size)) != ENUMERAL_TYPE
		    && TREE_CODE (TREE_TYPE (size)) != BOOLEAN_TYPE)
		  {
		    cp_error ("size of array `%D' has non-integer type",
			      dname);
		    size = integer_one_node;
		  }
		if (TREE_READONLY_DECL_P (size))
		  size = decl_constant_value (size);
		if (pedantic && integer_zerop (size))
		  cp_pedwarn ("ANSI C++ forbids zero-size array `%D'", dname);
		if (TREE_CONSTANT (size))
		  {
		    int old_flag_pedantic_errors = flag_pedantic_errors;
		    int old_pedantic = pedantic;
		    pedantic = flag_pedantic_errors = 1;
		    /* Always give overflow errors on array subscripts.  */
		    constant_expression_warning (size);
		    pedantic = old_pedantic;
		    flag_pedantic_errors = old_flag_pedantic_errors;
		    if (INT_CST_LT (size, integer_zero_node))
		      {
			cp_error ("size of array `%D' is negative", dname);
			size = integer_one_node;
		      }
		  }
		else
		  {
		    if (pedantic)
		      {
			if (dname)
			  cp_pedwarn ("ANSI C++ forbids variable-size array `%D'",
				      dname);
			else
			  cp_pedwarn ("ANSI C++ forbids variable-size array");
		      }
		  }

		itype
		  = fold (build_binary_op (MINUS_EXPR,
					   cp_convert (index_type, size),
					   cp_convert (index_type,
						       integer_one_node)));
		if (! TREE_CONSTANT (itype))
		  itype = variable_size (itype);
		else if (TREE_OVERFLOW (itype))
		  {
		    error ("overflow in array dimension");
		    TREE_OVERFLOW (itype) = 0;
		  }

		/* If we're a parm, we need to have a permanent type so
                   mangling checks for re-use will work right.  If both the
                   element and index types are permanent, the array type
                   will be, too.  */
		if (decl_context == PARM
		    && allocation_temporary_p () && TREE_PERMANENT (type))
		  {
		    push_obstacks (&permanent_obstack, &permanent_obstack);
		    itype = build_index_type (itype);
		    pop_obstacks ();
		  }
		else
		  itype = build_index_type (itype);

	      dont_grok_size:
		resume_momentary (yes);
	      }

	    type = build_cplus_array_type (type, itype);
	    ctype = NULL_TREE;
	  }
	  break;

	case CALL_EXPR:
	  {
	    tree arg_types;
	    int funcdecl_p;
	    tree inner_parms = TREE_OPERAND (declarator, 1);
	    tree inner_decl = TREE_OPERAND (declarator, 0);

	    /* Declaring a function type.
	       Make sure we have a valid type for the function to return.  */

	    /* We now know that the TYPE_QUALS don't apply to the
               decl, but to its return type.  */
	    type_quals = TYPE_UNQUALIFIED;

	    /* Warn about some types functions can't return.  */

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error ("`%s' declared as function returning a function", name);
		type = integer_type_node;
	      }
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      {
		error ("`%s' declared as function returning an array", name);
		type = integer_type_node;
	      }

	    if (inner_decl && TREE_CODE (inner_decl) == SCOPE_REF)
	      inner_decl = TREE_OPERAND (inner_decl, 1);

	    if (inner_decl && TREE_CODE (inner_decl) == TEMPLATE_ID_EXPR) 
	      inner_decl = dname;

	    /* Pick up type qualifiers which should be applied to `this'.  */
	    quals = TREE_OPERAND (declarator, 2);

	    /* Pick up the exception specifications.  */
	    raises = TREE_TYPE (declarator);

	    /* Say it's a definition only for the CALL_EXPR
	       closest to the identifier.  */
	    funcdecl_p
	      = inner_decl 
	      && (TREE_CODE (inner_decl) == IDENTIFIER_NODE
		  || TREE_CODE (inner_decl) == TEMPLATE_ID_EXPR 
		  || TREE_CODE (inner_decl) == BIT_NOT_EXPR);
	    
	    if (ctype == NULL_TREE
		&& decl_context == FIELD
		&& funcdecl_p
		&& (friendp == 0 || dname == current_class_name))
	      ctype = current_class_type;

	    if (ctype && return_type == return_conversion)
	      TYPE_HAS_CONVERSION (ctype) = 1;
	    if (ctype && constructor_name (ctype) == dname)
	      {
		/* We are within a class's scope. If our declarator name
		   is the same as the class name, and we are defining
		   a function, then it is a constructor/destructor, and
		   therefore returns a void type.  */

		if (flags == DTOR_FLAG)
		  {
		    /* ANSI C++ June 5 1992 WP 12.4.1.  A destructor may
		       not be declared const or volatile.  A destructor
		       may not be static.  */
		    if (staticp == 2)
		      error ("destructor cannot be static member function");
		    if (quals)
		      {
			cp_error ("destructors may not be `%s'",
				  IDENTIFIER_POINTER (TREE_VALUE (quals)));
			quals = NULL_TREE;
		      }
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "destructor for alien class `%s' cannot be a member"))
			  return void_type_node;
		      }
		  }
		else            /* It's a constructor.  */
		  {
		    if (explicitp == 1)
		      explicitp = 2;
		    /* ANSI C++ June 5 1992 WP 12.1.2.  A constructor may
		       not be declared const or volatile.  A constructor may
		       not be virtual.  A constructor may not be static.  */
		    if (staticp == 2)
		      error ("constructor cannot be static member function");
		    if (virtualp)
		      {
			pedwarn ("constructors cannot be declared virtual");
			virtualp = 0;
		      }
		    if (quals)
		      {
			cp_error ("constructors may not be `%s'",
				  IDENTIFIER_POINTER (TREE_VALUE (quals)));
			quals = NULL_TREE;
 		      }
		    {
		      RID_BIT_TYPE tmp_bits;
		      bcopy ((void*)&specbits, (void*)&tmp_bits, sizeof (RID_BIT_TYPE));
		      RIDBIT_RESET (RID_INLINE, tmp_bits);
		      RIDBIT_RESET (RID_STATIC, tmp_bits);
		      if (RIDBIT_ANY_SET (tmp_bits))
			error ("return value type specifier for constructor ignored");
		    }
		    type = build_pointer_type (ctype);
		    if (decl_context == FIELD
			&& IS_SIGNATURE (current_class_type))
		      {
			error ("constructor not allowed in signature");
			return void_type_node;
		      }			  
		    else if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "constructor for alien class `%s' cannot be member"))
			  return void_type_node;
			TYPE_HAS_CONSTRUCTOR (ctype) = 1;
			if (return_type != return_ctor)
			  return NULL_TREE;
		      }
		  }
		if (decl_context == FIELD)
		  staticp = 0;
	      }
	    else if (friendp)
	      {
		if (initialized)
		  error ("can't initialize friend function `%s'", name);
		if (virtualp)
		  {
		    /* Cannot be both friend and virtual.  */
		    error ("virtual functions cannot be friends");
		    RIDBIT_RESET (RID_FRIEND, specbits);
		    friendp = 0;
		  }
		if (decl_context == NORMAL)
		  error ("friend declaration not in class definition");
		if (current_function_decl && funcdef_flag)
		  cp_error ("can't define friend function `%s' in a local class definition",
			    name);
	      }

	    /* Construct the function type and go to the next
	       inner layer of declarator.  */

	    declarator = TREE_OPERAND (declarator, 0);

	    /* FIXME: This is where default args should be fully
	       processed.  */

	    arg_types = grokparms (inner_parms, funcdecl_p ? funcdef_flag : 0);

	    if (declarator && flags == DTOR_FLAG)
	      {
		/* A destructor declared in the body of a class will
		   be represented as a BIT_NOT_EXPR.  But, we just
		   want the underlying IDENTIFIER.  */
		if (TREE_CODE (declarator) == BIT_NOT_EXPR)
		  declarator = TREE_OPERAND (declarator, 0);
		
		if (strict_prototype == 0 && arg_types == NULL_TREE)
		  arg_types = void_list_node;
		else if (arg_types == NULL_TREE
			 || arg_types != void_list_node)
		  {
		    cp_error ("destructors may not have parameters");
		    arg_types = void_list_node;
		    last_function_parms = NULL_TREE;
		  }
	      }

	    /* ANSI says that `const int foo ();'
	       does not make the function foo const.  */
	    type = build_function_type (type, arg_types);

	    {
	      tree t;
	      for (t = arg_types; t; t = TREE_CHAIN (t))
		if (TREE_PURPOSE (t)
		    && TREE_CODE (TREE_PURPOSE (t)) == DEFAULT_ARG)
		  {
		    add_defarg_fn (type);
		    break;
		  }
	    }
	  }
	  break;

	case ADDR_EXPR:
	case INDIRECT_REF:
	  /* Filter out pointers-to-references and references-to-references.
	     We can get these if a TYPE_DECL is used.  */

	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    {
	      error ("cannot declare %s to references",
		     TREE_CODE (declarator) == ADDR_EXPR
		     ? "references" : "pointers");
	      declarator = TREE_OPERAND (declarator, 0);
	      continue;
	    }

	  if (TREE_CODE (type) == OFFSET_TYPE
	      && (TREE_CODE (TREE_TYPE (type)) == VOID_TYPE
		  || TREE_CODE (TREE_TYPE (type)) == REFERENCE_TYPE))
	    {
	      cp_error ("cannot declare pointer to `%#T' member",
			TREE_TYPE (type));
	      type = TREE_TYPE (type);
	    }

	  /* Merge any constancy or volatility into the target type
	     for the pointer.  */

	  /* We now know that the TYPE_QUALS don't apply to the decl,
	     but to the target of the pointer.  */
	  type_quals = TYPE_UNQUALIFIED;

	  if (IS_SIGNATURE (type))
	    {
	      if (TREE_CODE (declarator) == ADDR_EXPR)
		{
		  if (CLASSTYPE_METHOD_VEC (type) == NULL_TREE
		      && TYPE_SIZE (type))
		    cp_warning ("empty signature `%T' used in signature reference declaration",
				type);
#if 0
		  type = build_signature_reference_type (type);
#else
		  sorry ("signature reference");
		  return NULL_TREE;
#endif
		}
	      else
		{
		  if (CLASSTYPE_METHOD_VEC (type) == NULL_TREE
		      && TYPE_SIZE (type))
		    cp_warning ("empty signature `%T' used in signature pointer declaration",
				type);
		  type = build_signature_pointer_type (type);
		}
	    }
	  else if (TREE_CODE (declarator) == ADDR_EXPR)
	    {
	      if (TREE_CODE (type) == VOID_TYPE)
		error ("invalid type: `void &'");
	      else
		type = build_reference_type (type);
	    }
	  else if (TREE_CODE (type) == METHOD_TYPE)
	    type = build_ptrmemfunc_type (build_pointer_type (type));
	  else
	    type = build_pointer_type (type);

	  /* Process a list of type modifier keywords (such as
	     const or volatile) that were given inside the `*' or `&'.  */

	  if (TREE_TYPE (declarator))
	    {
	      register tree typemodlist;
	      int erred = 0;

	      constp = 0;
	      volatilep = 0;
	      restrictp = 0;
	      for (typemodlist = TREE_TYPE (declarator); typemodlist;
		   typemodlist = TREE_CHAIN (typemodlist))
		{
		  tree qualifier = TREE_VALUE (typemodlist);

		  if (qualifier == ridpointers[(int) RID_CONST])
		    constp++;
		  else if (qualifier == ridpointers[(int) RID_VOLATILE])
		    volatilep++;
		  else if (qualifier == ridpointers[(int) RID_RESTRICT])
		    restrictp++;
		  else if (!erred)
		    {
		      erred = 1;
		      error ("invalid type modifier within pointer declarator");
		    }
		}
	      if (constp > 1)
		pedwarn ("duplicate `const'");
	      if (volatilep > 1)
		pedwarn ("duplicate `volatile'");
	      if (restrictp > 1)
		pedwarn ("duplicate `restrict'");

	      type_quals = ((constp ? TYPE_QUAL_CONST : 0)
			    | (restrictp ? TYPE_QUAL_RESTRICT : 0)
			    | (volatilep ? TYPE_QUAL_VOLATILE : 0));
	      if (TREE_CODE (declarator) == ADDR_EXPR
		  && (constp || volatilep))
		{
		  if (constp)
		    pedwarn ("discarding `const' applied to a reference");
		  if (volatilep)
		    pedwarn ("discarding `volatile' applied to a reference");
		  type_quals &= ~(TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
		}
	      type = cp_build_qualified_type (type, type_quals);
	    }
	  declarator = TREE_OPERAND (declarator, 0);
	  ctype = NULL_TREE;
	  break;

	case SCOPE_REF:
	  {
	    /* We have converted type names to NULL_TREE if the
	       name was bogus, or to a _TYPE node, if not.

	       The variable CTYPE holds the type we will ultimately
	       resolve to.  The code here just needs to build
	       up appropriate member types.  */
	    tree sname = TREE_OPERAND (declarator, 1);
	    tree t;

	    /* Destructors can have their visibilities changed as well.  */
	    if (TREE_CODE (sname) == BIT_NOT_EXPR)
	      sname = TREE_OPERAND (sname, 0);

	    if (TREE_COMPLEXITY (declarator) == 0)
	      /* This needs to be here, in case we are called
		 multiple times.  */ ;
	    else if (TREE_COMPLEXITY (declarator) == -1)
	      /* Namespace member. */
	      pop_decl_namespace ();
	    else if (friendp && (TREE_COMPLEXITY (declarator) < 2))
	      /* Don't fall out into global scope. Hides real bug? --eichin */ ;
	    else if (! IS_AGGR_TYPE_CODE
		     (TREE_CODE (TREE_OPERAND (declarator, 0))))
	      ;
	    else if (TREE_COMPLEXITY (declarator) == current_class_depth)
	      {
		/* Resolve any TYPENAME_TYPEs from the decl-specifier-seq
		   that refer to ctype.  They couldn't be resolved earlier
		   because we hadn't pushed into the class yet.
		   Example: resolve 'B<T>::type' in
		   'B<typename B<T>::type> B<T>::f () { }'.  */
		if (current_template_parms
		    && uses_template_parms (type)
		    && uses_template_parms (current_class_type))
		  {
		    tree args = current_template_args ();
		    type = tsubst (type, args, /*complain=*/1, NULL_TREE);
		  }

		/* This pop_nested_class corresponds to the
                   push_nested_class used to push into class scope for
                   parsing the argument list of a function decl, in
                   qualified_id.  */
		pop_nested_class ();
		TREE_COMPLEXITY (declarator) = current_class_depth;
	      }
	    else
	      my_friendly_abort (16);

	    if (TREE_OPERAND (declarator, 0) == NULL_TREE)
	      {
		/* We had a reference to a global decl, or
		   perhaps we were given a non-aggregate typedef,
		   in which case we cleared this out, and should just
		   keep going as though it wasn't there.  */
		declarator = sname;
		continue;
	      }
	    ctype = TREE_OPERAND (declarator, 0);

	    t = ctype;
	    while (t != NULL_TREE && CLASS_TYPE_P (t)) 
	      {
		if (CLASSTYPE_TEMPLATE_INFO (t) &&
		    !CLASSTYPE_TEMPLATE_SPECIALIZATION (t))
		  template_count += 1;
		t = TYPE_MAIN_DECL (t);
		if (DECL_LANG_SPECIFIC (t))
		  t = DECL_CLASS_CONTEXT (t);
		else
		  t = NULL_TREE;
	      }

	    if (sname == NULL_TREE)
	      goto done_scoping;

	    if (TREE_CODE (sname) == IDENTIFIER_NODE)
	      {
		/* This is the `standard' use of the scoping operator:
		   basetype :: member .  */

		if (ctype == current_class_type)
		  {
		    /* class A {
		         void A::f ();
		       };

		       Is this ill-formed?  */

		    if (pedantic)
		      cp_pedwarn ("extra qualification `%T::' on member `%s' ignored",
				  ctype, name);
		  }
		else if (TREE_CODE (type) == FUNCTION_TYPE)
		  {
		    if (current_class_type == NULL_TREE
			|| friendp)
		      type = build_cplus_method_type (ctype, TREE_TYPE (type),
						      TYPE_ARG_TYPES (type));
		    else
		      {
			cp_error ("cannot declare member function `%T::%s' within `%T'",
				  ctype, name, current_class_type);
			return void_type_node;
		      }
		  }
		else if (RIDBIT_SETP (RID_TYPEDEF, specbits)
			 || TYPE_SIZE (complete_type (ctype)) != NULL_TREE)
		  {
		    /* Have to move this code elsewhere in this function.
		       this code is used for i.e., typedef int A::M; M *pm;

		       It is?  How? jason 10/2/94 */

		    if (current_class_type)
		      {
			cp_error ("cannot declare member `%T::%s' within `%T'",
				  ctype, name, current_class_type);
			return void_type_node;
		      }
		    type = build_offset_type (ctype, type);
		  }
		else if (uses_template_parms (ctype))
		  {
                    if (TREE_CODE (type) == FUNCTION_TYPE)
		      type
			= build_cplus_method_type (ctype, TREE_TYPE (type),
						   TYPE_ARG_TYPES (type));
  		  }
		else
		  {
		    cp_error ("structure `%T' not yet defined", ctype);
		    return error_mark_node;
		  }

		declarator = sname;
	      }
	    else if (TREE_CODE (sname) == SCOPE_REF)
	      my_friendly_abort (17);
	    else
	      {
	      done_scoping:
		declarator = TREE_OPERAND (declarator, 1);
		if (declarator && TREE_CODE (declarator) == CALL_EXPR)
		  /* In this case, we will deal with it later.  */
		  ;
		else
		  {
		    if (TREE_CODE (type) == FUNCTION_TYPE)
		      type = build_cplus_method_type (ctype, TREE_TYPE (type),
						      TYPE_ARG_TYPES (type));
		    else
		      type = build_offset_type (ctype, type);
		  }
	      }
	  }
	  break;

	case BIT_NOT_EXPR:
	  declarator = TREE_OPERAND (declarator, 0);
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  declarator = NULL_TREE;
	  break;

	case ERROR_MARK:
	  declarator = NULL_TREE;
	  break;

	default:
	  my_friendly_abort (158);
	}
    }

  /* See the comment for the TREE_LIST case, above.  */
  if (inner_attrs)
    {
      if (! ignore_attrs)
	decl_attributes (type, inner_attrs, NULL_TREE);
      else if (attrlist)
	TREE_VALUE (attrlist) = chainon (inner_attrs, TREE_VALUE (attrlist));
      else
	attrlist = build_decl_list (NULL_TREE, inner_attrs);
    }

  /* Now TYPE has the actual type.  */

  if (explicitp == 1)
    {
      error ("only constructors can be declared `explicit'");
      explicitp = 0;
    }

  if (RIDBIT_SETP (RID_MUTABLE, specbits))
    {
      if (type_quals & TYPE_QUAL_CONST)
	{
	  error ("const `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
      else if (staticp)
	{
	  error ("static `%s' cannot be declared `mutable'", name);
	  RIDBIT_RESET (RID_MUTABLE, specbits);
	}
    }

  if (declarator == NULL_TREE
      || TREE_CODE (declarator) == IDENTIFIER_NODE
      || (TREE_CODE (declarator) == TEMPLATE_ID_EXPR
	  && (TREE_CODE (type) == FUNCTION_TYPE
	      || TREE_CODE (type) == METHOD_TYPE)))
    /* OK */;
  else if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
    {
      cp_error ("template-id `%D' used as a declarator", declarator);
      declarator = dname;
    }
  else
    /* Unexpected declarator format.  */
    my_friendly_abort (990210);

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (RIDBIT_SETP (RID_TYPEDEF, specbits) && decl_context != TYPENAME)
    {
      tree decl;

      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (current_lang_name == lang_name_java)
	TYPE_FOR_JAVA (type) = 1;

      if (decl_context == FIELD)
	{
	  if (declarator == constructor_name (current_class_type))
	    cp_pedwarn ("ANSI C++ forbids nested type `%D' with same name as enclosing class",
			declarator);
	  decl = build_lang_decl (TYPE_DECL, declarator, type);
	  if (IS_SIGNATURE (current_class_type) && opaque_typedef)
	    SIGNATURE_HAS_OPAQUE_TYPEDECLS (current_class_type) = 1;
	}
      else
	{
	  /* Make sure this typedef lives as long as its type,
	     since it might be used as a template parameter. */
	  if (type != error_mark_node)
	    push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));
	  decl = build_decl (TYPE_DECL, declarator, type);
	  if (type != error_mark_node)
	    pop_obstacks ();
	}

      /* If the user declares "struct {...} foo" then `foo' will have
	 an anonymous name.  Fill that name in now.  Nothing can
	 refer to it, so nothing needs know about the name change.
	 The TYPE_NAME field was filled in by build_struct_xref.  */
      if (type != error_mark_node
	  && TYPE_NAME (type)
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && ANON_AGGRNAME_P (TYPE_IDENTIFIER (type)))
	{
	  tree oldname = TYPE_NAME (type);
	  tree t;

	  /* FIXME: This is bogus; we should not be doing this for
	            cv-qualified types.  */

	  /* Replace the anonymous name with the real name everywhere.  */
	  lookup_tag_reverse (type, declarator);
	  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	    if (TYPE_NAME (t) == oldname)
	      TYPE_NAME (t) = decl;

	  if (TYPE_LANG_SPECIFIC (type))
	    TYPE_WAS_ANONYMOUS (type) = 1;

	  /* If this is a typedef within a template class, the nested
	     type is a (non-primary) template.  The name for the
	     template needs updating as well.  */
	  if (TYPE_LANG_SPECIFIC (type) && CLASSTYPE_TEMPLATE_INFO (type))
	    DECL_NAME (CLASSTYPE_TI_TEMPLATE (type)) 
	      = TYPE_IDENTIFIER (type);

	  /* XXX Temporarily set the scope. 
	     When returning, start_decl expects it as NULL_TREE,
	     and will then then set it using pushdecl. */
	  my_friendly_assert (DECL_CONTEXT (decl) == NULL_TREE, 980404);
	  if (current_class_type)
	    DECL_CONTEXT (decl) = current_class_type;
	  else
	    DECL_CONTEXT (decl) = FROB_CONTEXT (current_namespace);

	  DECL_ASSEMBLER_NAME (decl) = DECL_NAME (decl);
	  DECL_ASSEMBLER_NAME (decl)
	    = get_identifier (build_overload_name (type, 1, 1));
	  DECL_CONTEXT (decl) = NULL_TREE;

	  /* FIXME remangle member functions; member functions of a
	     type with external linkage have external linkage.  */
	}

      if (TREE_CODE (type) == OFFSET_TYPE || TREE_CODE (type) == METHOD_TYPE)
	{
	  cp_error_at ("typedef name may not be class-qualified", decl);
	  return NULL_TREE;
	}
      else if (quals)
	{
	  if (ctype == NULL_TREE)
	    {
	      if (TREE_CODE (type) != METHOD_TYPE)
		cp_error_at ("invalid type qualifier for non-method type", decl);
	      else
		ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  if (ctype != NULL_TREE)
	    grok_method_quals (ctype, decl, quals);
	}

      if (RIDBIT_SETP (RID_SIGNED, specbits)
	  || (typedef_decl && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	C_TYPEDEF_EXPLICITLY_SIGNED (decl) = 1;

      if (RIDBIT_SETP (RID_MUTABLE, specbits))
	error ("non-object member `%s' cannot be declared mutable", name);

      bad_specifiers (decl, "type", virtualp, quals != NULL_TREE,
		      inlinep, friendp, raises != NULL_TREE);

      if (initialized)
	error ("typedef declaration includes an initializer");

      return decl;
    }

  /* Detect the case of an array type of unspecified size
     which came, as such, direct from a typedef name.
     We must copy the type, so that each identifier gets
     a distinct type, so that each identifier's size can be
     controlled separately by its own initializer.  */

  if (type == typedef_type && TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE)
    {
      type = build_cplus_array_type (TREE_TYPE (type), TYPE_DOMAIN (type));
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (type_quals != TYPE_UNQUALIFIED)
	{
	  if (IS_SIGNATURE (type))
	    error ("type qualifiers specified for signature type");
	  type_quals = TYPE_UNQUALIFIED;
	}

      /* Special case: "friend class foo" looks like a TYPENAME context.  */
      if (friendp)
	{
	  if (type_quals != TYPE_UNQUALIFIED)
	    {
	      cp_error ("type qualifiers specified for friend class declaration");
	      type_quals = TYPE_UNQUALIFIED;
	    }
	  if (inlinep)
	    {
	      cp_error ("`inline' specified for friend class declaration");
	      inlinep = 0;
	    }

	  /* Only try to do this stuff if we didn't already give up.  */
	  if (type != integer_type_node)
	    {
	      /* A friendly class?  */
	      if (current_class_type)
		make_friend_class (current_class_type, TYPE_MAIN_VARIANT (type));
	      else
		error ("trying to make class `%s' a friend of global scope",
		       TYPE_NAME_STRING (type));
	      type = void_type_node;
	    }
	}
      else if (quals)
	{
	  tree dummy = build_decl (TYPE_DECL, declarator, type);
	  if (ctype == NULL_TREE)
	    {
	      my_friendly_assert (TREE_CODE (type) == METHOD_TYPE, 159);
	      ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  grok_method_quals (ctype, dummy, quals);
	  type = TREE_TYPE (dummy);
	}

      return type;
    }
  else if (declarator == NULL_TREE && decl_context != PARM
	   && decl_context != CATCHPARM
	   && TREE_CODE (type) != UNION_TYPE
	   && ! bitfield)
    {
      cp_error ("abstract declarator `%T' used as declaration", type);
      declarator = make_anon_name ();
    }

  /* `void' at top level (not within pointer)
     is allowed only in typedefs or type names.
     We don't complain about parms either, but that is because
     a better error message can be made later.  */

  if (TREE_CODE (type) == VOID_TYPE && decl_context != PARM)
    {
      if (! declarator)
	error ("unnamed variable or field declared void");
      else if (TREE_CODE (declarator) == IDENTIFIER_NODE)
	{
	  if (IDENTIFIER_OPNAME_P (declarator))
	    my_friendly_abort (356);
	  else
	    error ("variable or field `%s' declared void", name);
	}
      else
	error ("variable or field declared void");
      type = integer_type_node;
    }

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  if (decl_context == PARM || decl_context == CATCHPARM)
    {
      if (ctype || in_namespace)
	error ("cannot use `::' in parameter declaration");

      /* A parameter declared as an array of T is really a pointer to T.
	 One declared as a function is really a pointer to a function.
	 One declared as a member is really a pointer to member.  */

      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  /* Transfer const-ness of array into that of type pointed to.  */
	  type = build_pointer_type (TREE_TYPE (type));
	  type_quals = TYPE_UNQUALIFIED;
	}
      else if (TREE_CODE (type) == FUNCTION_TYPE)
	type = build_pointer_type (type);
      else if (TREE_CODE (type) == OFFSET_TYPE)
	type = build_pointer_type (type);
      else if (TREE_CODE (type) == VOID_TYPE && declarator)
	{
	  error ("declaration of `%s' as void", name);
	  return NULL_TREE;
	}
    }
  
  {
    register tree decl;

    if (decl_context == PARM)
      {
	decl = build_decl (PARM_DECL, declarator, type);

	bad_specifiers (decl, "parameter", virtualp, quals != NULL_TREE,
			inlinep, friendp, raises != NULL_TREE);
	if (current_class_type
	    && IS_SIGNATURE (current_class_type))
	  {
	    if (inlinep)
	      error ("parameter of signature member function declared `inline'");
	    if (RIDBIT_SETP (RID_AUTO, specbits))
	      error ("parameter of signature member function declared `auto'");
	    if (RIDBIT_SETP (RID_REGISTER, specbits))
	      error ("parameter of signature member function declared `register'");
	  }

	/* Compute the type actually passed in the parmlist,
	   for the case where there is no prototype.
	   (For example, shorts and chars are passed as ints.)
	   When there is a prototype, this is overridden later.  */

	DECL_ARG_TYPE (decl) = type_promotes_to (type);
      }
    else if (decl_context == FIELD)
      {
	if (type == error_mark_node)
	  {
	    /* Happens when declaring arrays of sizes which
	       are error_mark_node, for example.  */
	    decl = NULL_TREE;
	  }
	else if (in_namespace && !friendp)
	  {
	    /* Something like struct S { int N::j; };  */
	    cp_error ("invalid use of `::'");
	    decl = NULL_TREE;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    int publicp = 0;
	    tree function_context;

	    /* We catch the others as conflicts with the builtin
	       typedefs.  */
	    if (friendp && declarator == ridpointers[(int) RID_SIGNED])
	      {
		cp_error ("function `%D' cannot be declared friend",
			  declarator);
		friendp = 0;
	      }

	    if (friendp == 0)
	      {
		if (ctype == NULL_TREE)
		  ctype = current_class_type;

		if (ctype == NULL_TREE)
		  {
		    cp_error ("can't make `%D' into a method -- not in a class",
			      declarator);
		    return void_type_node;
		  }

		/* ``A union may [ ... ] not [ have ] virtual functions.''
		   ARM 9.5 */
		if (virtualp && TREE_CODE (ctype) == UNION_TYPE)
		  {
		    cp_error ("function `%D' declared virtual inside a union",
			      declarator);
		    return void_type_node;
		  }

		if (declarator == ansi_opname[(int) NEW_EXPR]
		    || declarator == ansi_opname[(int) VEC_NEW_EXPR]
		    || declarator == ansi_opname[(int) DELETE_EXPR]
		    || declarator == ansi_opname[(int) VEC_DELETE_EXPR])
		  {
		    if (virtualp)
		      {
			cp_error ("`%D' cannot be declared virtual, since it is always static",
				  declarator);
			virtualp = 0;
		      }
		  }
		else if (staticp < 2)
		  type = build_cplus_method_type (ctype, TREE_TYPE (type),
						  TYPE_ARG_TYPES (type));
	      }

	    /* Tell grokfndecl if it needs to set TREE_PUBLIC on the node.  */
	    function_context = (ctype != NULL_TREE) ? 
	      hack_decl_function_context (TYPE_MAIN_DECL (ctype)) : NULL_TREE;
	    publicp = (! friendp || ! staticp)
	      && function_context == NULL_TREE;
	    decl = grokfndecl (ctype, type, 
			       TREE_CODE (declarator) != TEMPLATE_ID_EXPR
			       ? declarator : dname,
			       declarator,
			       virtualp, flags, quals, raises,
			       friendp ? -1 : 0, friendp, publicp, inlinep,
			       funcdef_flag, template_count, in_namespace);
	    if (decl == NULL_TREE)
	      return decl;
#if 0
	    /* This clobbers the attrs stored in `decl' from `attrlist'.  */
	    /* The decl and setting of decl_machine_attr is also turned off.  */
	    decl = build_decl_attribute_variant (decl, decl_machine_attr);
#endif

	    /* [class.conv.ctor]

	       A constructor declared without the function-specifier
	       explicit that can be called with a single parameter
	       specifies a conversion from the type of its first
	       parameter to the type of its class.  Such a constructor
	       is called a converting constructor.  */
	    if (explicitp == 2)
	      DECL_NONCONVERTING_P (decl) = 1;
	    else if (DECL_CONSTRUCTOR_P (decl))
	      {
		/* The constructor can be called with exactly one
		   parameter if there is at least one parameter, and
		   any subsequent parameters have default arguments.
		   We don't look at the first parameter, which is
		   really just the `this' parameter for the new
		   object.  */
		tree arg_types = 
		  TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)));

		/* Skip the `in_chrg' argument too, if present.  */
		if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (decl)))
		  arg_types = TREE_CHAIN (arg_types);

		if (arg_types == void_list_node
		    || (arg_types 
			&& TREE_CHAIN (arg_types) 
			&& TREE_CHAIN (arg_types) != void_list_node
			&& !TREE_PURPOSE (TREE_CHAIN (arg_types))))
		  DECL_NONCONVERTING_P (decl) = 1;
	      }
	  }
	else if (TREE_CODE (type) == METHOD_TYPE)
	  {
	    /* We only get here for friend declarations of
	       members of other classes.  */
	    /* All method decls are public, so tell grokfndecl to set
	       TREE_PUBLIC, also.  */
	    decl = grokfndecl (ctype, type, declarator, declarator,
			       virtualp, flags, quals, raises,
			       friendp ? -1 : 0, friendp, 1, 0, funcdef_flag,
			       template_count, in_namespace);
	    if (decl == NULL_TREE)
	      return NULL_TREE;
	  }
	else if (!staticp && ! processing_template_decl
		 && TYPE_SIZE (complete_type (type)) == NULL_TREE
		 && (TREE_CODE (type) != ARRAY_TYPE || initialized == 0))
	  {
	    if (declarator)
	      cp_error ("field `%D' has incomplete type", declarator);
	    else
	      cp_error ("name `%T' has incomplete type", type);

	    /* If we're instantiating a template, tell them which
	       instantiation made the field's type be incomplete.  */
	    if (current_class_type
		&& TYPE_NAME (current_class_type)
		&& IDENTIFIER_TEMPLATE (TYPE_IDENTIFIER (current_class_type))
		&& declspecs && TREE_VALUE (declspecs)
		&& TREE_TYPE (TREE_VALUE (declspecs)) == type)
	      cp_error ("  in instantiation of template `%T'",
			current_class_type);

	    type = error_mark_node;
	    decl = NULL_TREE;
	  }
	else
	  {
	    if (friendp)
	      {
		error ("`%s' is neither function nor method; cannot be declared friend",
		       IDENTIFIER_POINTER (declarator));
		friendp = 0;
	      }
	    decl = NULL_TREE;
	  }

	if (friendp)
	  {
	    /* Friends are treated specially.  */
	    if (ctype == current_class_type)
	      warning ("member functions are implicitly friends of their class");
	    else
	      {
		tree t = NULL_TREE;
		if (decl && DECL_NAME (decl))
		  {
		    if (template_class_depth (current_class_type) == 0)
		      {
			decl 
			  = check_explicit_specialization 
			  (declarator, decl,
			   template_count, 2 * (funcdef_flag != 0) + 4);
			if (decl == error_mark_node)
			  return error_mark_node;
		      }

		    t = do_friend (ctype, declarator, decl,
				   last_function_parms, attrlist, flags, quals,
				   funcdef_flag);
		  }
		if (t && funcdef_flag)
		  return t;
		
		return void_type_node;
	      }
	  }

	/* Structure field.  It may not be a function, except for C++ */

	if (decl == NULL_TREE)
	  {
	    if (initialized)
	      {
		if (!staticp)
		  {
		    /* An attempt is being made to initialize a non-static
		       member.  But, from [class.mem]:
		       
		       4 A member-declarator can contain a
		       constant-initializer only if it declares a static
		       member (_class.static_) of integral or enumeration
		       type, see _class.static.data_.  

		       This used to be relatively common practice, but
		       the rest of the compiler does not correctly
		       handle the initialization unless the member is
		       static so we make it static below.  */
		    cp_pedwarn ("ANSI C++ forbids initialization of member `%D'",
				declarator);
		    cp_pedwarn ("making `%D' static", declarator);
		    staticp = 1;
		  }

		if (uses_template_parms (type))
		  /* We'll check at instantiation time.  */
		  ;
		else if (check_static_variable_definition (declarator,
							   type))
		  /* If we just return the declaration, crashes
		     will sometimes occur.  We therefore return
		     void_type_node, as if this was a friend
		     declaration, to cause callers to completely
		     ignore this declaration.  */
		  return void_type_node;
	      }

	    /* 9.2p13 [class.mem] */
	    if (declarator == constructor_name (current_class_type)
		/* Divergence from the standard:  In extern "C", we
		   allow non-static data members here, because C does
		   and /usr/include/netinet/in.h uses that.  */
		&& (staticp || ! in_system_header))
	      cp_pedwarn ("ANSI C++ forbids data member `%D' with same name as enclosing class",
			  declarator);

	    if (staticp)
	      {
		/* C++ allows static class members.
		   All other work for this is done by grokfield.
		   This VAR_DCL is built by build_lang_field_decl.
		   All other VAR_DECLs are built by build_decl.  */
		decl = build_lang_field_decl (VAR_DECL, declarator, type);
		TREE_STATIC (decl) = 1;
		/* In class context, 'static' means public access.  */
		TREE_PUBLIC (decl) = DECL_EXTERNAL (decl) = 1;
	      }
	    else
	      {
		decl = build_lang_field_decl (FIELD_DECL, declarator, type);
		if (RIDBIT_SETP (RID_MUTABLE, specbits))
		  {
		    DECL_MUTABLE_P (decl) = 1;
		    RIDBIT_RESET (RID_MUTABLE, specbits);
		  }
	      }

	    bad_specifiers (decl, "field", virtualp, quals != NULL_TREE,
			    inlinep, friendp, raises != NULL_TREE);
	  }
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
      {
	tree original_name;
	int publicp = 0;

	if (! declarator)
	  return NULL_TREE;

	if (TREE_CODE (declarator) == TEMPLATE_ID_EXPR)
	  original_name = dname;
	else
	  original_name = declarator;

	if (RIDBIT_SETP (RID_AUTO, specbits))
	  error ("storage class `auto' invalid for function `%s'", name);
	else if (RIDBIT_SETP (RID_REGISTER, specbits))
	  error ("storage class `register' invalid for function `%s'", name);

	/* Function declaration not at top level.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (! toplevel_bindings_p ()
	    && (RIDBIT_SETP (RID_STATIC, specbits)
		|| RIDBIT_SETP (RID_INLINE, specbits))
	    && pedantic)
	  {
	    if (RIDBIT_SETP (RID_STATIC, specbits))
	      pedwarn ("storage class `static' invalid for function `%s' declared out of global scope", name);
	    else
	      pedwarn ("storage class `inline' invalid for function `%s' declared out of global scope", name);
	  }
	
	if (ctype == NULL_TREE)
	  {
	    if (virtualp)
	      {
		error ("virtual non-class function `%s'", name);
		virtualp = 0;
	      }
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE && staticp < 2)
	  type = build_cplus_method_type (ctype, TREE_TYPE (type),
					  TYPE_ARG_TYPES (type));

	/* Record presence of `static'.  */
	publicp = (ctype != NULL_TREE
		   || RIDBIT_SETP (RID_EXTERN, specbits)
		   || !RIDBIT_SETP (RID_STATIC, specbits));

	decl = grokfndecl (ctype, type, original_name, declarator,
			   virtualp, flags, quals, raises,
			   1, friendp,
			   publicp, inlinep, funcdef_flag, 
			   template_count, in_namespace);
	if (decl == NULL_TREE)
	  return NULL_TREE;

	if (staticp == 1)
	  {
	    int illegal_static = 0;

	    /* Don't allow a static member function in a class, and forbid
	       declaring main to be static.  */
	    if (TREE_CODE (type) == METHOD_TYPE)
	      {
		cp_pedwarn ("cannot declare member function `%D' to have static linkage", decl);
		illegal_static = 1;
	      }
	    else if (current_function_decl)
	      {
		/* FIXME need arm citation */
		error ("cannot declare static function inside another function");
		illegal_static = 1;
	      }

	    if (illegal_static)
	      {
		staticp = 0;
		RIDBIT_RESET (RID_STATIC, specbits);
	      }
	  }
      }
    else
      {
	/* It's a variable.  */

	/* An uninitialized decl with `extern' is a reference.  */
	decl = grokvardecl (type, declarator, &specbits, 
			    initialized, 
			    (type_quals & TYPE_QUAL_CONST) != 0, 
			    in_namespace);
	bad_specifiers (decl, "variable", virtualp, quals != NULL_TREE,
			inlinep, friendp, raises != NULL_TREE);

	if (ctype)
	  {
	    DECL_CONTEXT (decl) = ctype;
	    if (staticp == 1)
	      {
	        cp_pedwarn ("static member `%D' re-declared as static", decl);
	        staticp = 0;
		RIDBIT_RESET (RID_STATIC, specbits);
	      }
	    if (RIDBIT_SETP (RID_REGISTER, specbits) && TREE_STATIC (decl))
	      {
		cp_error ("static member `%D' declared `register'", decl);
		RIDBIT_RESET (RID_REGISTER, specbits);
	      }
	    if (RIDBIT_SETP (RID_EXTERN, specbits) && pedantic)
	      {
	        cp_pedwarn ("cannot explicitly declare member `%#D' to have extern linkage",
			    decl);
		RIDBIT_RESET (RID_EXTERN, specbits);
	      }
	  }
      }

    if (RIDBIT_SETP (RID_MUTABLE, specbits))
      {
	error ("`%s' cannot be declared mutable", name);
      }

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (RIDBIT_SETP (RID_REGISTER, specbits))
      DECL_REGISTER (decl) = 1;

    if (RIDBIT_SETP (RID_EXTERN, specbits))
      DECL_THIS_EXTERN (decl) = 1;

    if (RIDBIT_SETP (RID_STATIC, specbits))
      DECL_THIS_STATIC (decl) = 1;

    /* Record constancy and volatility.  */
    /* FIXME: Disallow `restrict' pointer-to-member declarations.  */
    c_apply_type_quals_to_decl (type_quals, decl);

    return decl;
  }
}

/* Tell if a parmlist/exprlist looks like an exprlist or a parmlist.
   An empty exprlist is a parmlist.  An exprlist which
   contains only identifiers at the global level
   is a parmlist.  Otherwise, it is an exprlist.  */

int
parmlist_is_exprlist (exprs)
     tree exprs;
{
  if (exprs == NULL_TREE || TREE_PARMLIST (exprs))
    return 0;

  if (toplevel_bindings_p ())
    {
      /* At the global level, if these are all identifiers,
	 then it is a parmlist.  */
      while (exprs)
	{
	  if (TREE_CODE (TREE_VALUE (exprs)) != IDENTIFIER_NODE)
	    return 1;
	  exprs = TREE_CHAIN (exprs);
	}
      return 0;
    }
  return 1;
}

/* Subroutine of start_function.  Ensure that each of the parameter
   types (as listed in PARMS) is complete, as is required for a
   function definition.  */

static void
require_complete_types_for_parms (parms)
     tree parms;
{
  while (parms)
    {
      tree type = TREE_TYPE (parms);
      if (TYPE_SIZE (complete_type (type)) == NULL_TREE)
	{
	  if (DECL_NAME (parms))
	    error ("parameter `%s' has incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (parms)));
	  else
	    error ("parameter has incomplete type");
	  TREE_TYPE (parms) = error_mark_node;
	}
      else
	layout_decl (parms, 0);

      parms = TREE_CHAIN (parms);
    }
}

/* Returns DECL if DECL is a local variable (or parameter).  Returns
   NULL_TREE otherwise.  */

static tree
local_variable_p (t)
     tree t;
{
  if ((TREE_CODE (t) == VAR_DECL 
       /* A VAR_DECL with a context that is a _TYPE is a static data
	  member.  */
       && !TYPE_P (CP_DECL_CONTEXT (t))
       /* Any other non-local variable must be at namespace scope.  */
       && TREE_CODE (CP_DECL_CONTEXT (t)) != NAMESPACE_DECL)
      || (TREE_CODE (t) == PARM_DECL))
    return t;

  return NULL_TREE;
}

/* Check that ARG, which is a default-argument expression for a
   parameter DECL, is legal.  Returns ARG, or ERROR_MARK_NODE, if
   something goes wrong.  DECL may also be a _TYPE node, rather than a
   DECL, if there is no DECL available.  */

tree
check_default_argument (decl, arg)
     tree decl;
     tree arg;
{
  tree var;
  tree decl_type;

  if (TREE_CODE (arg) == DEFAULT_ARG)
    /* We get a DEFAULT_ARG when looking at an in-class declaration
       with a default argument.  Ignore the argument for now; we'll
       deal with it after the class is complete.  */
    return arg;

  if (processing_template_decl || uses_template_parms (arg))
    /* We don't do anything checking until instantiation-time.  Note
       that there may be uninstantiated arguments even for an
       instantiated function, since default arguments are not
       instantiated until they are needed.  */
    return arg;

  if (TYPE_P (decl))
    {
      decl_type = decl;
      decl = NULL_TREE;
    }
  else
    decl_type = TREE_TYPE (decl);

  if (arg == error_mark_node 
      || decl == error_mark_node
      || TREE_TYPE (arg) == error_mark_node
      || decl_type == error_mark_node)
    /* Something already went wrong.  There's no need to check
       further.  */
    return error_mark_node;

  /* [dcl.fct.default]
     
     A default argument expression is implicitly converted to the
     parameter type.  */
  if (!TREE_TYPE (arg)
      || !can_convert_arg (decl_type, TREE_TYPE (arg), arg))
    {
      if (decl)
	cp_error ("default argument for `%#D' has type `%T'", 
		  decl, TREE_TYPE (arg));
      else
	cp_error ("default argument for paramter of type `%T' has type `%T'",
		  decl_type, TREE_TYPE (arg));

      return error_mark_node;
    }

  /* [dcl.fct.default]

     Local variables shall not be used in default argument
     expressions. 

     The keyword `this' shall not be used in a default argument of a
     member function.  */
  var = search_tree (arg, local_variable_p);
  if (var)
    {
      cp_error ("default argument `%E' uses local variable `%D'",
		arg, var);
      return error_mark_node;
    }

  /* All is well.  */
  return arg;
}

/* Decode the list of parameter types for a function type.
   Given the list of things declared inside the parens,
   return a list of types.

   The list we receive can have three kinds of elements:
   an IDENTIFIER_NODE for names given without types,
   a TREE_LIST node for arguments given as typespecs or names with typespecs,
   or void_type_node, to mark the end of an argument list
   when additional arguments are not permitted (... was not used).

   FUNCDEF_FLAG is nonzero for a function definition, 0 for
   a mere declaration.  A nonempty identifier-list gets an error message
   when FUNCDEF_FLAG is zero.
   If FUNCDEF_FLAG is 1, then parameter types must be complete.
   If FUNCDEF_FLAG is -1, then parameter types may be incomplete.

   If all elements of the input list contain types,
   we return a list of the types.
   If all elements contain no type (except perhaps a void_type_node
   at the end), we return a null list.
   If some have types and some do not, it is an error, and we
   return a null list.

   Also set last_function_parms to either
   a list of names (IDENTIFIER_NODEs) or a chain of PARM_DECLs.
   A list of names is converted to a chain of PARM_DECLs
   by store_parm_decls so that ultimately it is always a chain of decls.

   Note that in C++, parameters can take default values.  These default
   values are in the TREE_PURPOSE field of the TREE_LIST.  It is
   an error to specify default values which are followed by parameters
   that have no default values, or an ELLIPSES.  For simplicities sake,
   only parameters which are specified with their types can take on
   default values.  */

static tree
grokparms (first_parm, funcdef_flag)
     tree first_parm;
     int funcdef_flag;
{
  tree result = NULL_TREE;
  tree decls = NULL_TREE;

  if (first_parm != NULL_TREE
      && TREE_CODE (TREE_VALUE (first_parm)) == IDENTIFIER_NODE)
    {
      if (! funcdef_flag)
	pedwarn ("parameter names (without types) in function declaration");
      last_function_parms = first_parm;
      return NULL_TREE;
    }
  else if (first_parm != NULL_TREE
	   && TREE_CODE (TREE_VALUE (first_parm)) != TREE_LIST
	   && TREE_CODE (TREE_VALUE (first_parm)) != VOID_TYPE)
    my_friendly_abort (145);
  else
    {
      /* Types were specified.  This is a list of declarators
	 each represented as a TREE_LIST node.  */
      register tree parm, chain;
      int any_init = 0, any_error = 0;

      if (first_parm != NULL_TREE)
	{
	  tree last_result = NULL_TREE;
	  tree last_decl = NULL_TREE;

	  for (parm = first_parm; parm != NULL_TREE; parm = chain)
	    {
	      tree type = NULL_TREE, list_node = parm;
	      register tree decl = TREE_VALUE (parm);
	      tree init = TREE_PURPOSE (parm);

	      chain = TREE_CHAIN (parm);
	      /* @@ weak defense against parse errors.  */
	      if (TREE_CODE (decl) != VOID_TYPE 
		  && TREE_CODE (decl) != TREE_LIST)
		{
		  /* Give various messages as the need arises.  */
		  if (TREE_CODE (decl) == STRING_CST)
		    cp_error ("invalid string constant `%E'", decl);
		  else if (TREE_CODE (decl) == INTEGER_CST)
		    error ("invalid integer constant in parameter list, did you forget to give parameter name?");
		  continue;
		}

	      if (TREE_CODE (decl) != VOID_TYPE)
		{
		  decl = grokdeclarator (TREE_VALUE (decl),
					 TREE_PURPOSE (decl),
					 PARM, init != NULL_TREE,
					 NULL_TREE);
		  if (! decl || TREE_TYPE (decl) == error_mark_node)
		    continue;

		  /* Top-level qualifiers on the parameters are
		     ignored for function types.  */
		  type = TYPE_MAIN_VARIANT (TREE_TYPE (decl));

		  if (TREE_CODE (type) == VOID_TYPE)
		    decl = void_type_node;
		  else if (TREE_CODE (type) == METHOD_TYPE)
		    {
		      if (DECL_NAME (decl))
			/* Cannot use the decl here because
			   we don't have DECL_CONTEXT set up yet.  */
			cp_error ("parameter `%D' invalidly declared method type",
				  DECL_NAME (decl));
		      else
			error ("parameter invalidly declared method type");
		      type = build_pointer_type (type);
		      TREE_TYPE (decl) = type;
		    }
		  else if (TREE_CODE (type) == OFFSET_TYPE)
		    {
		      if (DECL_NAME (decl))
			cp_error ("parameter `%D' invalidly declared offset type",
				  DECL_NAME (decl));
		      else
			error ("parameter invalidly declared offset type");
		      type = build_pointer_type (type);
		      TREE_TYPE (decl) = type;
		    }
                  else if (TREE_CODE (type) == RECORD_TYPE
                           && TYPE_LANG_SPECIFIC (type)
                           && CLASSTYPE_ABSTRACT_VIRTUALS (type))
                    {
                      abstract_virtuals_error (decl, type);
                      any_error = 1;  /* Seems like a good idea. */
                    }
                  else if (TREE_CODE (type) == RECORD_TYPE
                           && TYPE_LANG_SPECIFIC (type)
                           && IS_SIGNATURE (type))
                    {
                      signature_error (decl, type);
                      any_error = 1;  /* Seems like a good idea. */
                    }
		  else if (POINTER_TYPE_P (type))
		    {
		      tree t = type;
		      while (POINTER_TYPE_P (t)
			     || (TREE_CODE (t) == ARRAY_TYPE
				 && TYPE_DOMAIN (t) != NULL_TREE))
			t = TREE_TYPE (t);
		      if (TREE_CODE (t) == ARRAY_TYPE)
			cp_error ("parameter type `%T' includes %s to array of unknown bound",
				  type,
				  TYPE_PTR_P (type) ? "pointer" : "reference");
		    }
		}

	      if (TREE_CODE (decl) == VOID_TYPE)
		{
		  if (result == NULL_TREE)
		    {
		      result = void_list_node;
		      last_result = result;
		    }
		  else
		    {
		      TREE_CHAIN (last_result) = void_list_node;
		      last_result = void_list_node;
		    }
		  if (chain
		      && (chain != void_list_node || TREE_CHAIN (chain)))
		    error ("`void' in parameter list must be entire list");
		  break;
		}

	      /* Since there is a prototype, args are passed in their own types.  */
	      DECL_ARG_TYPE (decl) = TREE_TYPE (decl);
#ifdef PROMOTE_PROTOTYPES
	      if ((TREE_CODE (type) == INTEGER_TYPE
		   || TREE_CODE (type) == ENUMERAL_TYPE)
		  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
		DECL_ARG_TYPE (decl) = integer_type_node;
#endif
	      if (!any_error && init)
		{
		  any_init++;
		  init = check_default_argument (decl, init);
		}
	      else
		init = NULL_TREE;

	      if (decls == NULL_TREE)
		{
		  decls = decl;
		  last_decl = decls;
		}
	      else
		{
		  TREE_CHAIN (last_decl) = decl;
		  last_decl = decl;
		}
	      if (! current_function_decl && TREE_PERMANENT (list_node))
		{
		  TREE_PURPOSE (list_node) = init;
		  TREE_VALUE (list_node) = type;
		  TREE_CHAIN (list_node) = NULL_TREE;
		}
	      else
		list_node = saveable_tree_cons (init, type, NULL_TREE);
	      if (result == NULL_TREE)
		{
		  result = list_node;
		  last_result = result;
		}
	      else
		{
		  TREE_CHAIN (last_result) = list_node;
		  last_result = list_node;
		}
	    }
	  if (last_result)
	    TREE_CHAIN (last_result) = NULL_TREE;
	  /* If there are no parameters, and the function does not end
	     with `...', then last_decl will be NULL_TREE.  */
	  if (last_decl != NULL_TREE)
	    TREE_CHAIN (last_decl) = NULL_TREE;
	}
    }

  last_function_parms = decls;

  return result;
}

/* Called from the parser to update an element of TYPE_ARG_TYPES for some
   FUNCTION_TYPE with the newly parsed version of its default argument, which
   was previously digested as text.  See snarf_defarg et al in lex.c.  */

void
replace_defarg (arg, init)
     tree arg, init;
{
  if (! processing_template_decl
      && ! can_convert_arg (TREE_VALUE (arg), TREE_TYPE (init), init))
    cp_pedwarn ("invalid type `%T' for default argument to `%T'",
		TREE_TYPE (init), TREE_VALUE (arg));
  TREE_PURPOSE (arg) = init;
}

int
copy_args_p (d)
     tree d;
{
  tree t = FUNCTION_ARG_CHAIN (d);
  if (DECL_CONSTRUCTOR_P (d)
      && TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (d)))
    t = TREE_CHAIN (t);
  if (t && TREE_CODE (TREE_VALUE (t)) == REFERENCE_TYPE
      && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (t)))
	  == DECL_CLASS_CONTEXT (d))
      && (TREE_CHAIN (t) == NULL_TREE
	  || TREE_CHAIN (t) == void_list_node
	  || TREE_PURPOSE (TREE_CHAIN (t))))
    return 1;
  return 0;
}

/* These memoizing functions keep track of special properties which
   a class may have.  `grok_ctor_properties' notices whether a class
   has a constructor of the form X(X&), and also complains
   if the class has a constructor of the form X(X).
   `grok_op_properties' takes notice of the various forms of
   operator= which are defined, as well as what sorts of type conversion
   may apply.  Both functions take a FUNCTION_DECL as an argument.  */

int
grok_ctor_properties (ctype, decl)
     tree ctype, decl;
{
  tree parmtypes = FUNCTION_ARG_CHAIN (decl);
  tree parmtype = parmtypes ? TREE_VALUE (parmtypes) : void_type_node;

  /* When a type has virtual baseclasses, a magical first int argument is
     added to any ctor so we can tell if the class has been initialized
     yet.  This could screw things up in this function, so we deliberately
     ignore the leading int if we're in that situation.  */
  if (TYPE_USES_VIRTUAL_BASECLASSES (ctype))
    {
      my_friendly_assert (parmtypes
			  && TREE_VALUE (parmtypes) == integer_type_node,
			  980529);
      parmtypes = TREE_CHAIN (parmtypes);
      parmtype = TREE_VALUE (parmtypes);
    }

  /* [class.copy]

     A non-template constructor for class X is a copy constructor if
     its first parameter is of type X&, const X&, volatile X& or const
     volatile X&, and either there are no other parameters or else all
     other parameters have default arguments.  */
  if (TREE_CODE (parmtype) == REFERENCE_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (parmtype)) == ctype
      && (TREE_CHAIN (parmtypes) == NULL_TREE
	  || TREE_CHAIN (parmtypes) == void_list_node
	  || TREE_PURPOSE (TREE_CHAIN (parmtypes)))
      && !(DECL_TEMPLATE_INSTANTIATION (decl)
	   && is_member_template (DECL_TI_TEMPLATE (decl))))
    {
      TYPE_HAS_INIT_REF (ctype) = 1;
      if (CP_TYPE_CONST_P (TREE_TYPE (parmtype)))
	TYPE_HAS_CONST_INIT_REF (ctype) = 1;
    }
  /* [class.copy]

     A declaration of a constructor for a class X is ill-formed if its
     first parameter is of type (optionally cv-qualified) X and either
     there are no other parameters or else all other parameters have
     default arguments.  

     We *don't* complain about member template instantiations that
     have this form, though; they can occur as we try to decide what
     constructor to use during overload resolution.  Since overload
     resolution will never prefer such a constructor to the
     non-template copy constructor (which is either explicitly or
     implicitly defined), there's no need to worry about their
     existence.  Theoretically, they should never even be
     instantiated, but that's hard to forestall.  */
  else if (TYPE_MAIN_VARIANT (parmtype) == ctype
	   && (TREE_CHAIN (parmtypes) == NULL_TREE
	       || TREE_CHAIN (parmtypes) == void_list_node
	       || TREE_PURPOSE (TREE_CHAIN (parmtypes)))
	   && !(DECL_TEMPLATE_INSTANTIATION (decl)
		&& is_member_template (DECL_TI_TEMPLATE (decl))))
    {
      cp_error ("invalid constructor; you probably meant `%T (const %T&)'",
		ctype, ctype);
      SET_IDENTIFIER_ERROR_LOCUS (DECL_NAME (decl), ctype);
      return 0;
    }
  else if (TREE_CODE (parmtype) == VOID_TYPE
	   || TREE_PURPOSE (parmtypes) != NULL_TREE)
    TYPE_HAS_DEFAULT_CONSTRUCTOR (ctype) = 1;

  return 1;
}

/* An operator with this name can be either unary or binary.  */

static int
ambi_op_p (name)
     tree name;
{
  return (name == ansi_opname [(int) INDIRECT_REF]
	  || name == ansi_opname [(int) ADDR_EXPR]
	  || name == ansi_opname [(int) NEGATE_EXPR]
	  || name == ansi_opname[(int) POSTINCREMENT_EXPR]
	  || name == ansi_opname[(int) POSTDECREMENT_EXPR]
	  || name == ansi_opname [(int) CONVERT_EXPR]);
}

/* An operator with this name can only be unary.  */

static int
unary_op_p (name)
     tree name;
{
  return (name == ansi_opname [(int) TRUTH_NOT_EXPR]
	  || name == ansi_opname [(int) BIT_NOT_EXPR]
	  || name == ansi_opname [(int) COMPONENT_REF]
	  || IDENTIFIER_TYPENAME_P (name));
}

/* Do a little sanity-checking on how they declared their operator.  */

void
grok_op_properties (decl, virtualp, friendp)
     tree decl;
     int virtualp, friendp;
{
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
  int methodp = (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE);
  tree name = DECL_NAME (decl);

  if (current_class_type == NULL_TREE)
    friendp = 1;

  if (! friendp)
    {
      /* [class.copy]

	 A user-declared copy assignment operator X::operator= is a
	 non-static non-template member function of class X with
	 exactly one parameter of type X, X&, const X&, volatile X& or
	 const volatile X&.  */
      if (name == ansi_opname[(int) MODIFY_EXPR]
	  && !(DECL_TEMPLATE_INSTANTIATION (decl)
	       && is_member_template (DECL_TI_TEMPLATE (decl))))
	;
      else if (name == ansi_opname[(int) CALL_EXPR])
	TYPE_OVERLOADS_CALL_EXPR (current_class_type) = 1;
      else if (name == ansi_opname[(int) ARRAY_REF])
	TYPE_OVERLOADS_ARRAY_REF (current_class_type) = 1;
      else if (name == ansi_opname[(int) COMPONENT_REF]
	       || name == ansi_opname[(int) MEMBER_REF])
	TYPE_OVERLOADS_ARROW (current_class_type) = 1;
      else if (name == ansi_opname[(int) NEW_EXPR])
	TYPE_GETS_NEW (current_class_type) |= 1;
      else if (name == ansi_opname[(int) DELETE_EXPR])
	TYPE_GETS_DELETE (current_class_type) |= 1;
      else if (name == ansi_opname[(int) VEC_NEW_EXPR])
	TYPE_GETS_NEW (current_class_type) |= 2;
      else if (name == ansi_opname[(int) VEC_DELETE_EXPR])
	TYPE_GETS_DELETE (current_class_type) |= 2;
    }

  if (name == ansi_opname[(int) NEW_EXPR]
      || name == ansi_opname[(int) VEC_NEW_EXPR])
    {
      /* When the compiler encounters the definition of A::operator new, it
	 doesn't look at the class declaration to find out if it's static.  */
      if (methodp)
	revert_static_member_fn (&decl, NULL, NULL);
     
      /* Take care of function decl if we had syntax errors.  */
      if (argtypes == NULL_TREE)
	TREE_TYPE (decl)
	  = build_function_type (ptr_type_node,
				 hash_tree_chain (integer_type_node,
						  void_list_node));
      else
	TREE_TYPE (decl) = coerce_new_type (TREE_TYPE (decl));
    }
  else if (name == ansi_opname[(int) DELETE_EXPR]
	   || name == ansi_opname[(int) VEC_DELETE_EXPR])
    {
      if (methodp)
	revert_static_member_fn (&decl, NULL, NULL);
     
      if (argtypes == NULL_TREE)
	TREE_TYPE (decl)
	  = build_function_type (void_type_node,
				 hash_tree_chain (ptr_type_node,
						  void_list_node));
      else
	{
	  TREE_TYPE (decl) = coerce_delete_type (TREE_TYPE (decl));

	  if (! friendp && name == ansi_opname[(int) VEC_DELETE_EXPR]
	      && (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)))
		  != void_list_node))
	    TYPE_VEC_DELETE_TAKES_SIZE (current_class_type) = 1;
	}
    }
  else
    {
      /* An operator function must either be a non-static member function
	 or have at least one parameter of a class, a reference to a class,
	 an enumeration, or a reference to an enumeration.  13.4.0.6 */
      if (! methodp || DECL_STATIC_FUNCTION_P (decl))
	{
	  if (IDENTIFIER_TYPENAME_P (name)
	      || name == ansi_opname[(int) CALL_EXPR]
	      || name == ansi_opname[(int) MODIFY_EXPR]
	      || name == ansi_opname[(int) COMPONENT_REF]
	      || name == ansi_opname[(int) ARRAY_REF])
	    cp_error ("`%D' must be a nonstatic member function", decl);
	  else
	    {
	      tree p = argtypes;

	      if (DECL_STATIC_FUNCTION_P (decl))
		cp_error ("`%D' must be either a non-static member function or a non-member function", decl);

	      if (p)
		for (; TREE_CODE (TREE_VALUE (p)) != VOID_TYPE ; p = TREE_CHAIN (p))
		  {
		    tree arg = TREE_VALUE (p);
		    if (TREE_CODE (arg) == REFERENCE_TYPE)
		      arg = TREE_TYPE (arg);

		    /* This lets bad template code slip through.  */
		    if (IS_AGGR_TYPE (arg)
			|| TREE_CODE (arg) == ENUMERAL_TYPE
			|| TREE_CODE (arg) == TEMPLATE_TYPE_PARM
			|| TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
		      goto foundaggr;
		  }
	      cp_error
		("`%D' must have an argument of class or enumerated type",
		 decl);
	    foundaggr:
	      ;
	    }
	}
      
      if (name == ansi_opname[(int) CALL_EXPR])
	return;			/* No restrictions on args. */

      if (IDENTIFIER_TYPENAME_P (name) && ! DECL_TEMPLATE_INFO (decl))
	{
	  tree t = TREE_TYPE (name);
	  if (TREE_CODE (t) == VOID_TYPE)
	    pedwarn ("void is not a valid type conversion operator");
	  else if (! friendp)
	    {
	      int ref = (TREE_CODE (t) == REFERENCE_TYPE);
	      const char *what = 0;
	      if (ref)
		t = TYPE_MAIN_VARIANT (TREE_TYPE (t));

	      if (t == current_class_type)
		what = "the same type";
	      /* Don't force t to be complete here.  */
	      else if (IS_AGGR_TYPE (t)
		       && TYPE_SIZE (t)
		       && DERIVED_FROM_P (t, current_class_type))
		what = "a base class";

	      if (what)
		warning ("conversion to %s%s will never use a type conversion operator",
			 ref ? "a reference to " : "", what);
	    }
	}

      if (name == ansi_opname[(int) MODIFY_EXPR])
	{
	  tree parmtype;

	  if (list_length (argtypes) != 3 && methodp)
	    {
	      cp_error ("`%D' must take exactly one argument", decl);
	      return;
	    }
	  parmtype = TREE_VALUE (TREE_CHAIN (argtypes));

	  if (copy_assignment_arg_p (parmtype, virtualp)
	      && ! friendp)
	    {
	      TYPE_HAS_ASSIGN_REF (current_class_type) = 1;
	      if (TREE_CODE (parmtype) != REFERENCE_TYPE
		  || CP_TYPE_CONST_P (TREE_TYPE (parmtype)))
		TYPE_HAS_CONST_ASSIGN_REF (current_class_type) = 1;
	    }
	}
      else if (name == ansi_opname[(int) COND_EXPR])
	{
	  /* 13.4.0.3 */
	  pedwarn ("ANSI C++ prohibits overloading operator ?:");
	  if (list_length (argtypes) != 4)
	    cp_error ("`%D' must take exactly three arguments", decl);
	}	  
      else if (ambi_op_p (name))
	{
	  if (list_length (argtypes) == 2)
	    /* prefix */;
	  else if (list_length (argtypes) == 3)
	    {
	      if ((name == ansi_opname[(int) POSTINCREMENT_EXPR]
		   || name == ansi_opname[(int) POSTDECREMENT_EXPR])
		  && ! processing_template_decl
		  && ! same_type_p (TREE_VALUE (TREE_CHAIN (argtypes)), integer_type_node))
		{
		  if (methodp)
		    cp_error ("postfix `%D' must take `int' as its argument",
			      decl);
		  else
		    cp_error
		      ("postfix `%D' must take `int' as its second argument",
		       decl);
		}
	    }
	  else
	    {
	      if (methodp)
		cp_error ("`%D' must take either zero or one argument", decl);
	      else
		cp_error ("`%D' must take either one or two arguments", decl);
	    }

	  /* More Effective C++ rule 6.  */
	  if (warn_ecpp
	      && (name == ansi_opname[(int) POSTINCREMENT_EXPR]
		  || name == ansi_opname[(int) POSTDECREMENT_EXPR]))
	    {
	      tree arg = TREE_VALUE (argtypes);
	      tree ret = TREE_TYPE (TREE_TYPE (decl));
	      if (methodp || TREE_CODE (arg) == REFERENCE_TYPE)
		arg = TREE_TYPE (arg);
	      arg = TYPE_MAIN_VARIANT (arg);
	      if (list_length (argtypes) == 2)
		{
		  if (TREE_CODE (ret) != REFERENCE_TYPE
		      || !same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (ret)),
				       arg))
		    cp_warning ("prefix `%D' should return `%T'", decl,
				build_reference_type (arg));
		}
	      else
		{
		  if (!same_type_p (TYPE_MAIN_VARIANT (ret), arg))
		    cp_warning ("postfix `%D' should return `%T'", decl, arg);
		}
	    }
	}
      else if (unary_op_p (name))
	{
	  if (list_length (argtypes) != 2)
	    {
	      if (methodp)
		cp_error ("`%D' must take `void'", decl);
	      else
		cp_error ("`%D' must take exactly one argument", decl);
	    }
	}
      else /* if (binary_op_p (name)) */
	{
	  if (list_length (argtypes) != 3)
	    {
	      if (methodp)
		cp_error ("`%D' must take exactly one argument", decl);
	      else
		cp_error ("`%D' must take exactly two arguments", decl);
	    }

	  /* More Effective C++ rule 7.  */
	  if (warn_ecpp
	      && (name == ansi_opname [TRUTH_ANDIF_EXPR]
		  || name == ansi_opname [TRUTH_ORIF_EXPR]
		  || name == ansi_opname [COMPOUND_EXPR]))
	    cp_warning ("user-defined `%D' always evaluates both arguments",
			decl);
	}

      /* Effective C++ rule 23.  */
      if (warn_ecpp
	  && list_length (argtypes) == 3
	  && (name == ansi_opname [PLUS_EXPR]
	      || name == ansi_opname [MINUS_EXPR]
	      || name == ansi_opname [TRUNC_DIV_EXPR]
	      || name == ansi_opname [MULT_EXPR])
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == REFERENCE_TYPE)
	cp_warning ("`%D' should return by value", decl);

      /* 13.4.0.8 */
      if (argtypes)
	for (; argtypes != void_list_node ; argtypes = TREE_CHAIN (argtypes))
	  if (TREE_PURPOSE (argtypes))
	    {
	      TREE_PURPOSE (argtypes) = NULL_TREE;
	      if (name == ansi_opname[(int) POSTINCREMENT_EXPR]
		  || name == ansi_opname[(int) POSTDECREMENT_EXPR])
		{
		  if (pedantic)
		    cp_pedwarn ("`%D' cannot have default arguments", decl);
		}
	      else
		cp_error ("`%D' cannot have default arguments", decl);
	    }
    }
}

static const char *
tag_name (code)
     enum tag_types code;
{
  switch (code)
    {
    case record_type:
      return "struct";
    case class_type:
      return "class";
    case union_type:
      return "union ";
    case enum_type:
      return "enum";
    case signature_type:
      return "signature";
    default:
      my_friendly_abort (981122);
    }
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.

   C++: If a class derivation is given, process it here, and report
   an error if multiple derivation declarations are not identical.

   If this is a definition, come in through xref_tag and only look in
   the current frame for the name (since C++ allows new names in any
   scope.)  */

tree
xref_tag (code_type_node, name, globalize)
     tree code_type_node;
     tree name;
     int globalize;
{
  enum tag_types tag_code;
  enum tree_code code;
  int temp = 0;
  register tree ref, t;
  struct binding_level *b = current_binding_level;
  int got_type = 0;
  tree attributes = NULL_TREE;
  tree context = NULL_TREE;

  /* If we are called from the parser, code_type_node will sometimes be a
     TREE_LIST.  This indicates that the user wrote
     "class __attribute__ ((foo)) bar".  Extract the attributes so we can
     use them later.  */
  if (TREE_CODE (code_type_node) == TREE_LIST)
    {
      attributes = TREE_PURPOSE (code_type_node);
      code_type_node = TREE_VALUE (code_type_node);
    }

  tag_code = (enum tag_types) TREE_INT_CST_LOW (code_type_node);
  switch (tag_code)
    {
    case record_type:
    case class_type:
    case signature_type:
      code = RECORD_TYPE;
      break;
    case union_type:
      code = UNION_TYPE;
      break;
    case enum_type:
      code = ENUMERAL_TYPE;
      break;
    default:
      my_friendly_abort (18);
    }

  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */
  if (TREE_CODE_CLASS (TREE_CODE (name)) == 't')
    {
      t = name;
      name = TYPE_IDENTIFIER (t);
      got_type = 1;
    }
  else
    t = IDENTIFIER_TYPE_VALUE (name);

  if (t && TREE_CODE (t) != code && TREE_CODE (t) != TEMPLATE_TYPE_PARM
      && TREE_CODE (t) != TEMPLATE_TEMPLATE_PARM)
    t = NULL_TREE;

  if (! globalize)
    {
      /* If we know we are defining this tag, only look it up in
	 this scope and don't try to find it as a type.  */
      ref = lookup_tag (code, name, b, 1);
    }
  else
    {
      if (t)
	{
	  /* [dcl.type.elab] If the identifier resolves to a
	     typedef-name or a template type-parameter, the
	     elaborated-type-specifier is ill-formed.  */
	  if (t != TYPE_MAIN_VARIANT (t)
	      || (CLASS_TYPE_P (t) && TYPE_WAS_ANONYMOUS (t)))
	    cp_pedwarn ("using typedef-name `%D' after `%s'",
			TYPE_NAME (t), tag_name (tag_code));
	  else if (TREE_CODE (t) == TEMPLATE_TYPE_PARM)
	    cp_error ("using template type parameter `%T' after `%s'",
		      t, tag_name (tag_code));

	  ref = t;
	}
      else
	ref = lookup_tag (code, name, b, 0);
	  
      if (! ref)
	{
	  /* Try finding it as a type declaration.  If that wins,
	     use it.  */ 
	  ref = lookup_name (name, 1);

	  if (ref != NULL_TREE
	      && processing_template_decl
	      && DECL_CLASS_TEMPLATE_P (ref)
	      && template_class_depth (current_class_type) == 0)
	    /* Since GLOBALIZE is true, we're declaring a global
	       template, so we want this type.  */
	    ref = DECL_RESULT (ref);

	  if (ref && TREE_CODE (ref) == TYPE_DECL
	      && TREE_CODE (TREE_TYPE (ref)) == code)
	    ref = TREE_TYPE (ref);
	  else
	    ref = NULL_TREE;
	}

      if (ref && current_class_type 
	  && template_class_depth (current_class_type) 
	  && PROCESSING_REAL_TEMPLATE_DECL_P ()) 
	{
	  /* Since GLOBALIZE is non-zero, we are not looking at a
	     definition of this tag.  Since, in addition, we are currently
	     processing a (member) template declaration of a template
	     class, we must be very careful; consider:

	       template <class X>
	       struct S1

	       template <class U>
	       struct S2
	       { template <class V>
	       friend struct S1; };

	     Here, the S2::S1 declaration should not be confused with the
	     outer declaration.  In particular, the inner version should
	     have a template parameter of level 2, not level 1.  This
	     would be particularly important if the member declaration
	     were instead:

	       template <class V = U> friend struct S1;

	     say, when we should tsubst into `U' when instantiating
	     S2.  On the other hand, when presented with:

	         template <class T>
	         struct S1 {
		   template <class U>
	           struct S2 {};
		   template <class U>
		   friend struct S2;
		 };

              we must find the inner binding eventually.  We
	      accomplish this by making sure that the new type we
	      create to represent this declaration has the right
	      TYPE_CONTEXT.  */
	  context = TYPE_CONTEXT (ref);
	  ref = NULL_TREE;
	}
    }

  push_obstacks_nochange ();

  if (! ref)
    {
      /* If no such tag is yet defined, create a forward-reference node
	 and record it as the "definition".
	 When a real declaration of this type is found,
	 the forward-reference will be altered into a real type.  */

      /* In C++, since these migrate into the global scope, we must
	 build them on the permanent obstack.  */

      temp = allocation_temporary_p ();
      if (temp)
	end_temporary_allocation ();

      if (code == ENUMERAL_TYPE)
	{
	  cp_error ("use of enum `%#D' without previous declaration", name);

	  ref = make_node (ENUMERAL_TYPE);

	  /* Give the type a default layout like unsigned int
	     to avoid crashing if it does not get defined.  */
	  TYPE_MODE (ref) = TYPE_MODE (unsigned_type_node);
	  TYPE_ALIGN (ref) = TYPE_ALIGN (unsigned_type_node);
	  TREE_UNSIGNED (ref) = 1;
	  TYPE_PRECISION (ref) = TYPE_PRECISION (unsigned_type_node);
	  TYPE_MIN_VALUE (ref) = TYPE_MIN_VALUE (unsigned_type_node);
	  TYPE_MAX_VALUE (ref) = TYPE_MAX_VALUE (unsigned_type_node);

	  /* Enable us to recognize when a type is created in class context.
	     To do nested classes correctly, this should probably be cleared
	     out when we leave this classes scope.  Currently this in only
	     done in `start_enum'.  */

	  pushtag (name, ref, globalize);
	}
      else
	{
	  struct binding_level *old_b = class_binding_level;

	  ref = make_lang_type (code);
	  TYPE_CONTEXT (ref) = context;

	  if (tag_code == signature_type)
	    {
	      SET_SIGNATURE (ref);
	      /* Since a signature type will be turned into the type
		 of signature tables, it's not only an interface.  */
	      CLASSTYPE_INTERFACE_ONLY (ref) = 0;
	      SET_CLASSTYPE_INTERFACE_KNOWN (ref);
	      /* A signature doesn't have a vtable.  */
	      CLASSTYPE_VTABLE_NEEDS_WRITING (ref) = 0;
	    }

#ifdef NONNESTED_CLASSES
	  /* Class types don't nest the way enums do.  */
	  class_binding_level = (struct binding_level *)0;
#endif
	  pushtag (name, ref, globalize);
	  class_binding_level = old_b;
	}
    }
  else
    {
      /* If it no longer looks like a nested type, make sure it's
	 in global scope.  
         If it is not an IDENTIFIER, this is not a declaration */
      if (b->namespace_p && !class_binding_level
	  && TREE_CODE (name) == IDENTIFIER_NODE)
	{
	  if (IDENTIFIER_NAMESPACE_VALUE (name) == NULL_TREE)
	    SET_IDENTIFIER_NAMESPACE_VALUE (name, TYPE_NAME (ref));
	}

      if (!globalize && processing_template_decl && IS_AGGR_TYPE (ref))
	redeclare_class_template (ref, current_template_parms);
    }

  /* Until the type is defined, tentatively accept whatever
     structure tag the user hands us.  */
  if (TYPE_SIZE (ref) == NULL_TREE
      && ref != current_class_type
      /* Have to check this, in case we have contradictory tag info.  */
      && IS_AGGR_TYPE_CODE (TREE_CODE (ref)))
    {
      if (tag_code == class_type)
	CLASSTYPE_DECLARED_CLASS (ref) = 1;
      else if (tag_code == record_type || tag_code == signature_type)
	CLASSTYPE_DECLARED_CLASS (ref) = 0;
    }

  pop_obstacks ();

  TREE_TYPE (ref) = attributes;

  return ref;
}

tree
xref_tag_from_type (old, id, globalize)
     tree old, id;
     int globalize;
{
  tree code_type_node;

  if (TREE_CODE (old) == RECORD_TYPE)
    code_type_node = (CLASSTYPE_DECLARED_CLASS (old)
		      ? class_type_node : record_type_node);
  else
    code_type_node = union_type_node;

  if (id == NULL_TREE)
    id = TYPE_IDENTIFIER (old);

  return xref_tag (code_type_node, id, globalize);
}

/* REF is a type (named NAME), for which we have just seen some
   baseclasses.  BINFO is a list of those baseclasses; the
   TREE_PURPOSE is an access_* node, and the TREE_VALUE is the type of
   the base-class.  CODE_TYPE_NODE indicates whether REF is a class,
   struct, or union.  */

void
xref_basetypes (code_type_node, name, ref, binfo)
     tree code_type_node;
     tree name, ref;
     tree binfo;
{
  /* In the declaration `A : X, Y, ... Z' we mark all the types
     (A, X, Y, ..., Z) so we can check for duplicates.  */
  tree binfos;
  tree base;

  int i, len;
  enum tag_types tag_code = (enum tag_types) TREE_INT_CST_LOW (code_type_node);

  if (tag_code == union_type)
    {
      cp_error ("derived union `%T' invalid", ref);
      return;
    }

  len = list_length (binfo);
  push_obstacks (TYPE_OBSTACK (ref), TYPE_OBSTACK (ref));

  /* First, make sure that any templates in base-classes are
     instantiated.  This ensures that if we call ourselves recursively
     we do not get confused about which classes are marked and which
     are not.  */
  for (base = binfo; base; base = TREE_CHAIN (base))
    complete_type (TREE_VALUE (base));

  SET_CLASSTYPE_MARKED (ref);
  BINFO_BASETYPES (TYPE_BINFO (ref)) = binfos = make_tree_vec (len);

  for (i = 0; binfo; binfo = TREE_CHAIN (binfo))
    {
      /* The base of a derived struct is public by default.  */
      int via_public
	= (TREE_PURPOSE (binfo) == access_public_node
	   || TREE_PURPOSE (binfo) == access_public_virtual_node
	   || (tag_code != class_type
	       && (TREE_PURPOSE (binfo) == access_default_node
		   || TREE_PURPOSE (binfo) == access_default_virtual_node)));
      int via_protected
	= (TREE_PURPOSE (binfo) == access_protected_node
	   || TREE_PURPOSE (binfo) == access_protected_virtual_node);
      int via_virtual
	= (TREE_PURPOSE (binfo) == access_private_virtual_node
	   || TREE_PURPOSE (binfo) == access_protected_virtual_node
	   || TREE_PURPOSE (binfo) == access_public_virtual_node
	   || TREE_PURPOSE (binfo) == access_default_virtual_node);
      tree basetype = TREE_VALUE (binfo);
      tree base_binfo;

      if (basetype && TREE_CODE (basetype) == TYPE_DECL)
	basetype = TREE_TYPE (basetype);
      if (!basetype
	  || (TREE_CODE (basetype) != RECORD_TYPE
	      && TREE_CODE (basetype) != TYPENAME_TYPE
	      && TREE_CODE (basetype) != TEMPLATE_TYPE_PARM
	      && TREE_CODE (basetype) != TEMPLATE_TEMPLATE_PARM))
	{
	  cp_error ("base type `%T' fails to be a struct or class type",
		    TREE_VALUE (binfo));
	  continue;
	}

      GNU_xref_hier (name, basetype, via_public, via_virtual, 0);

      /* This code replaces similar code in layout_basetypes.
         We put the complete_type first for implicit `typename'.  */
      if (TYPE_SIZE (basetype) == NULL_TREE
	  && ! (current_template_parms && uses_template_parms (basetype)))
	{
	  cp_error ("base class `%T' has incomplete type", basetype);
	  continue;
	}
      else
	{
	  if (CLASSTYPE_MARKED (basetype))
	    {
	      if (basetype == ref)
		cp_error ("recursive type `%T' undefined", basetype);
	      else
		cp_error ("duplicate base type `%T' invalid", basetype);
	      continue;
	    }

	  if (TYPE_FOR_JAVA (basetype)
	      && current_lang_stack == current_lang_base)
	    TYPE_FOR_JAVA (ref) = 1;

	  /* Note that the BINFO records which describe individual
	     inheritances are *not* shared in the lattice!  They
	     cannot be shared because a given baseclass may be
	     inherited with different `accessibility' by different
	     derived classes.  (Each BINFO record describing an
	     individual inheritance contains flags which say what
	     the `accessibility' of that particular inheritance is.)  */
  
	  base_binfo 
	    = make_binfo (integer_zero_node, basetype,
			  CLASS_TYPE_P (basetype)
			  ? TYPE_BINFO_VTABLE (basetype) : NULL_TREE,
			  CLASS_TYPE_P (basetype)
			  ? TYPE_BINFO_VIRTUALS (basetype) : NULL_TREE);
 
	  TREE_VEC_ELT (binfos, i) = base_binfo;
	  TREE_VIA_PUBLIC (base_binfo) = via_public;
	  TREE_VIA_PROTECTED (base_binfo) = via_protected;
	  TREE_VIA_VIRTUAL (base_binfo) = via_virtual;
	  BINFO_INHERITANCE_CHAIN (base_binfo) = TYPE_BINFO (ref);

	  /* We need to unshare the binfos now so that lookups during class
	     definition work.  */
	  unshare_base_binfos (base_binfo);

	  SET_CLASSTYPE_MARKED (basetype);

	  /* We are free to modify these bits because they are meaningless
	     at top level, and BASETYPE is a top-level type.  */
	  if (via_virtual || TYPE_USES_VIRTUAL_BASECLASSES (basetype))
	    {
	      TYPE_USES_VIRTUAL_BASECLASSES (ref) = 1;
	      TYPE_USES_COMPLEX_INHERITANCE (ref) = 1;
	    }

	  if (CLASS_TYPE_P (basetype))
	    {
	      TYPE_GETS_NEW (ref) |= TYPE_GETS_NEW (basetype);
	      TYPE_GETS_DELETE (ref) |= TYPE_GETS_DELETE (basetype);
	    }

	  i += 1;
	}
    }
  if (i)
    TREE_VEC_LENGTH (binfos) = i;
  else
    BINFO_BASETYPES (TYPE_BINFO (ref)) = NULL_TREE;

  if (i > 1)
    TYPE_USES_MULTIPLE_INHERITANCE (ref) = 1;
  else if (i == 1)
    {
      tree basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, 0));
      
      if (CLASS_TYPE_P (basetype))
	TYPE_USES_MULTIPLE_INHERITANCE (ref)
	  = TYPE_USES_MULTIPLE_INHERITANCE (basetype);
    }

  if (TYPE_USES_MULTIPLE_INHERITANCE (ref))
    TYPE_USES_COMPLEX_INHERITANCE (ref) = 1;

  /* Unmark all the types.  */
  while (--i >= 0)
    CLEAR_CLASSTYPE_MARKED (BINFO_TYPE (TREE_VEC_ELT (binfos, i)));
  CLEAR_CLASSTYPE_MARKED (ref);

  /* Now that we know all the base-classes, set up the list of virtual
     bases.  */
  CLASSTYPE_VBASECLASSES (ref) = get_vbase_types (ref);

  pop_obstacks ();
}
  

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (name)
     tree name;
{
  register tree enumtype = NULL_TREE;
  struct binding_level *b = current_binding_level;

  /* We are wasting space here and putting these on the permanent_obstack so
     that typeid(local enum) will work correctly. */
  push_obstacks (&permanent_obstack, &permanent_obstack);

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != NULL_TREE)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, b, 1);

  if (enumtype != NULL_TREE && TREE_CODE (enumtype) == ENUMERAL_TYPE)
    cp_error ("multiple definition of `%#T'", enumtype);
  else
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype, 0);
    }

  if (current_class_type)
    TREE_ADDRESSABLE (b->tags) = 1;

  /* We don't copy this value because build_enumerator needs to do it.  */
  enum_next_value = integer_zero_node;
  enum_overflow = 0;

  GNU_xref_decl (current_function_decl, enumtype);
  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object and VALUES a list of name-value pairs.
   Returns ENUMTYPE.  */

tree
finish_enum (enumtype)
     tree enumtype;
{
  register tree minnode = NULL_TREE, maxnode = NULL_TREE;
  /* Calculate the maximum value of any enumerator in this type.  */

  tree values = TYPE_VALUES (enumtype);
  if (values)
    {
      tree pair;

      for (pair = values; pair; pair = TREE_CHAIN (pair))
	{
	  tree decl;
	  tree value;

	  /* The TREE_VALUE is a CONST_DECL for this enumeration
	     constant.  */
	  decl = TREE_VALUE (pair);

	  /* The DECL_INITIAL will be NULL if we are processing a
	     template declaration and this enumeration constant had no
	     explicit initializer.  */
	  value = DECL_INITIAL (decl);
	  if (value && !processing_template_decl)
	    {
	      /* Set the TREE_TYPE for the VALUE as well.  That's so
		 that when we call decl_constant_value we get an
		 entity of the right type (but with the constant
		 value).  Since we shouldn't ever call
		 decl_constant_value on a template type, there's no
		 reason to do that when processing_template_decl.
		 And, if the expression is something like a
		 TEMPLATE_PARM_INDEX or a CAST_EXPR doing so will
		 wreak havoc on the intended type of the expression.  

	         Of course, there's also no point in trying to compute
		 minimum or maximum values if we're in a template.  */
	      TREE_TYPE (value) = enumtype;

	      if (!minnode)
		minnode = maxnode = value;
	      else if (tree_int_cst_lt (maxnode, value))
		maxnode = value;
	      else if (tree_int_cst_lt (value, minnode))
		minnode = value;
	    }

	  if (processing_template_decl) 
	    /* If this is just a template, leave the CONST_DECL
	       alone.  That way tsubst_copy will find CONST_DECLs for
	       CONST_DECLs, and not INTEGER_CSTs.  */
	    ;
	  else
	    /* In the list we're building up, we want the enumeration
	       values, not the CONST_DECLs.  */
	    TREE_VALUE (pair) = value;
	}
    }
  else
    maxnode = minnode = integer_zero_node;

  TYPE_VALUES (enumtype) = nreverse (values);

  if (processing_template_decl)
    {
      tree scope = current_scope ();
      if (scope && TREE_CODE (scope) == FUNCTION_DECL)
	add_tree (build_min (TAG_DEFN, enumtype));
    }
  else
    {
      int unsignedp = tree_int_cst_sgn (minnode) >= 0;
      int lowprec = min_precision (minnode, unsignedp);
      int highprec = min_precision (maxnode, unsignedp);
      int precision = MAX (lowprec, highprec);
      tree tem;

      TYPE_SIZE (enumtype) = NULL_TREE;

      /* Set TYPE_MIN_VALUE and TYPE_MAX_VALUE according to `precision'.  */

      TYPE_PRECISION (enumtype) = precision;
      if (unsignedp)
	fixup_unsigned_type (enumtype);
      else
	fixup_signed_type (enumtype);

      if (flag_short_enums || (precision > TYPE_PRECISION (integer_type_node)))
	/* Use the width of the narrowest normal C type which is wide
	   enough.  */ 
	TYPE_PRECISION (enumtype) = TYPE_PRECISION (type_for_size
						    (precision, 1));
      else
	TYPE_PRECISION (enumtype) = TYPE_PRECISION (integer_type_node);

      TYPE_SIZE (enumtype) = 0;
      layout_type (enumtype);
    
      /* Fix up all variant types of this enum type.  */
      for (tem = TYPE_MAIN_VARIANT (enumtype); tem;
	   tem = TYPE_NEXT_VARIANT (tem))
	{
	  TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
	  TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
	  TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
	  TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
	  TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
	  TYPE_MODE (tem) = TYPE_MODE (enumtype);
	  TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
	  TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
	  TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
	}

      /* Finish debugging output for this type.  */
      rest_of_type_compilation (enumtype, namespace_bindings_p ());
    }

  /* In start_enum we pushed obstacks.  Here, we must pop them.  */
  pop_obstacks ();

  return enumtype;
}

/* Build and install a CONST_DECL for an enumeration constant of the
   enumeration type TYPE whose NAME and VALUE (if any) are provided.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (name, value, type)
     tree name;
     tree value;
     tree type;
{
  tree decl, result;
  tree context;

  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

 if (! processing_template_decl)
   {
     /* Validate and default VALUE.  */
     if (value != NULL_TREE)
       {
	 if (TREE_READONLY_DECL_P (value))
	   value = decl_constant_value (value);

	 if (TREE_CODE (value) == INTEGER_CST)
	   {
	     value = default_conversion (value);
	     constant_expression_warning (value);
	   }
	 else
	   {
	     cp_error ("enumerator value for `%D' not integer constant", name);
	     value = NULL_TREE;
	   }
       }

     /* Default based on previous value.  */
     if (value == NULL_TREE && ! processing_template_decl)
       {
	 value = enum_next_value;
	 if (enum_overflow)
	   cp_error ("overflow in enumeration values at `%D'", name);
       }

     /* Remove no-op casts from the value.  */
     if (value)
       STRIP_TYPE_NOPS (value);
#if 0
     /* To fix MAX_VAL enum consts. (bkoz)  */
     TREE_TYPE (value) = integer_type_node;
#endif
   }

 /* We always have to copy here; not all INTEGER_CSTs are unshared.
    Even in other cases, we will later (in finish_enum) be setting the
    type of VALUE.  */
 if (value != NULL_TREE)
   value = copy_node (value);

  /* C++ associates enums with global, function, or class declarations.  */
 
 context = current_scope ();
 if (context && context == current_class_type)
   /* This enum declaration is local to the class.  */
   decl = build_lang_field_decl (CONST_DECL, name, type);
 else
   /* It's a global enum, or it's local to a function.  (Note local to
      a function could mean local to a class method.  */
   decl = build_decl (CONST_DECL, name, type);

 DECL_CONTEXT (decl) = FROB_CONTEXT (context);
 DECL_INITIAL (decl) = value;
 TREE_READONLY (decl) = 1;

 if (context && context == current_class_type)
   /* In something like `struct S { enum E { i = 7 }; };' we put `i'
      on the TYPE_FIELDS list for `S'.  (That's so that you can say
      things like `S::i' later.)  */
   finish_member_declaration (decl);
 else
   {
     pushdecl (decl);
     GNU_xref_decl (current_function_decl, decl);
   }

 if (! processing_template_decl)
   {
     /* Set basis for default for next value.  */
     enum_next_value = build_binary_op_nodefault (PLUS_EXPR, value,
						  integer_one_node, PLUS_EXPR);
     enum_overflow = tree_int_cst_lt (enum_next_value, value);
   }

  result = saveable_tree_cons (name, decl, NULL_TREE);
  return result;
}


static int function_depth;

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   If PRE_PARSED_P is non-zero then DECLARATOR is really the DECL for
   the function we are about to process; DECLSPECS are ignored.  For
   example, we set PRE_PARSED_P when processing the definition of
   inline function that was defined in-class; the definition is
   actually processed when the class is complete.  In this case,
   PRE_PARSED_P is 2.  We also set PRE_PARSED_P when instanting the
   body of a template function, and when constructing thunk functions
   and such; in these cases PRE_PARSED_P is 1.
   
   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.

   For C++, we must first check whether that datum makes any sense.
   For example, "class A local_a(1,2);" means that variable local_a
   is an aggregate of type A, which should have a constructor
   applied to it with the argument list [1, 2].

   @@ There is currently no way to retrieve the storage
   @@ allocated to FUNCTION (or all of its parms) if we return
   @@ something we had previously.  */

int
start_function (declspecs, declarator, attrs, pre_parsed_p)
     tree declspecs, declarator, attrs;
     int pre_parsed_p;
{
  tree decl1;
  tree ctype = NULL_TREE;
  tree fntype;
  tree restype;
  extern int have_extern_spec;
  extern int used_extern_spec;
  int doing_friend = 0;

  /* Sanity check.  */
  my_friendly_assert (TREE_CODE (TREE_VALUE (void_list_node)) == VOID_TYPE, 160);
  my_friendly_assert (TREE_CHAIN (void_list_node) == NULL_TREE, 161);

  /* Assume, until we see it does.  */
  current_function_returns_value = 0;
  current_function_returns_null = 0;
  named_labels = 0;
  shadowed_labels = 0;
  current_function_assigns_this = 0;
  current_function_just_assigned_this = 0;
  current_function_parms_stored = 0;
  original_result_rtx = NULL_RTX;
  base_init_expr = NULL_TREE;
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;
  ctor_label = dtor_label = NULL_TREE;
  static_labelno = 0;

  clear_temp_name ();

  /* This should only be done once on the top most decl.  */
  if (have_extern_spec && !used_extern_spec)
    {
      declspecs = decl_tree_cons (NULL_TREE, get_identifier ("extern"), declspecs);
      used_extern_spec = 1;
    }

  if (pre_parsed_p)
    {
      decl1 = declarator;

#if 0
      /* What was this testing for, exactly?  */
      if (! DECL_ARGUMENTS (decl1)
	  && !DECL_STATIC_FUNCTION_P (decl1)
	  && !DECL_ARTIFICIAL (decl1)
	  && DECL_CLASS_SCOPE_P (decl1)
	  && TYPE_IDENTIFIER (DECL_CONTEXT (decl1))
	  && IDENTIFIER_TEMPLATE (TYPE_IDENTIFIER (DECL_CONTEXT (decl1))))
	{
	  tree binding = binding_for_name (DECL_NAME (decl1), 
					   current_namespace);
	  cp_error ("redeclaration of `%#D'", decl1);
	  if (IDENTIFIER_CLASS_VALUE (DECL_NAME (decl1)))
	    cp_error_at ("previous declaration here", IDENTIFIER_CLASS_VALUE (DECL_NAME (decl1)));
	  else if (BINDING_VALUE (binding))
	    cp_error_at ("previous declaration here", BINDING_VALUE (binding));
	}
#endif

      fntype = TREE_TYPE (decl1);
      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);

      /* ANSI C++ June 5 1992 WP 11.4.5.  A friend function defined in a
	 class is in the (lexical) scope of the class in which it is
	 defined.  */
      if (!ctype && DECL_FRIEND_P (decl1))
	{
	  ctype = DECL_CLASS_CONTEXT (decl1);

	  /* CTYPE could be null here if we're dealing with a template;
	     for example, `inline friend float foo()' inside a template
	     will have no CTYPE set.  */
	  if (ctype && TREE_CODE (ctype) != RECORD_TYPE)
	    ctype = NULL_TREE;
	  else
	    doing_friend = 1;
	}

      last_function_parms = DECL_ARGUMENTS (decl1);
      last_function_parm_tags = NULL_TREE;
    }
  else
    {
      decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1, NULL_TREE);
      /* If the declarator is not suitable for a function definition,
	 cause a syntax error.  */
      if (decl1 == NULL_TREE || TREE_CODE (decl1) != FUNCTION_DECL) return 0;

      fntype = TREE_TYPE (decl1);

      restype = TREE_TYPE (fntype);
      if (CLASS_TYPE_P (restype) && !CLASSTYPE_GOT_SEMICOLON (restype))
	{
	  cp_error ("semicolon missing after declaration of `%#T'", restype);
	  shadow_tag (build_expr_list (NULL_TREE, restype));
	  CLASSTYPE_GOT_SEMICOLON (restype) = 1;
	  if (TREE_CODE (fntype) == FUNCTION_TYPE)
	    fntype = build_function_type (integer_type_node,
					  TYPE_ARG_TYPES (fntype));
	  else
	    fntype = build_cplus_method_type (build_type_variant (TYPE_METHOD_BASETYPE (fntype), TREE_READONLY (decl1), TREE_SIDE_EFFECTS (decl1)),
					      integer_type_node,
					      TYPE_ARG_TYPES (fntype));
	  TREE_TYPE (decl1) = fntype;
	}

      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);
      else if (DECL_MAIN_P (decl1))
	{
	  /* If this doesn't return integer_type, complain.  */
	  if (TREE_TYPE (TREE_TYPE (decl1)) != integer_type_node)
	    {
	      if (pedantic || warn_return_type)
		pedwarn ("return type for `main' changed to `int'");
	      TREE_TYPE (decl1) = fntype = default_function_type;
	    }
	}
    }

  /* Warn if function was previously implicitly declared
     (but not if we warned then).  */
  if (! warn_implicit
      && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)) != NULL_TREE)
    cp_warning_at ("`%D' implicitly declared before its definition", IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)));

  announce_function (decl1);

  /* Set up current_class_type, and enter the scope of the class, if
     appropriate.  */
  if (ctype)
    push_nested_class (ctype, 1);
  else if (DECL_STATIC_FUNCTION_P (decl1))
    push_nested_class (DECL_CONTEXT (decl1), 2);

  /* Now that we have entered the scope of the class, we must restore
     the bindings for any template parameters surrounding DECL1, if it
     is an inline member template.  (Order is important; consider the
     case where a template parameter has the same name as a field of
     the class.)  It is not until after this point that
     PROCESSING_TEMPLATE_DECL is guaranteed to be set up correctly.  */
  if (pre_parsed_p == 2)
    maybe_begin_member_template_processing (decl1);

  /* We are now in the scope of the function being defined.  */
  current_function_decl = decl1;

  /* Save the parm names or decls from this function's declarator
     where store_parm_decls will find them.  */
  current_function_parms = last_function_parms;
  current_function_parm_tags = last_function_parm_tags;

  if (! processing_template_decl)
    {
      /* In a function definition, arg types must be complete.  */
      require_complete_types_for_parms (current_function_parms);

      if (TYPE_SIZE (complete_type (TREE_TYPE (fntype))) == NULL_TREE)
	{
	  cp_error ("return-type `%#T' is an incomplete type",
		    TREE_TYPE (fntype));

	  /* Make it return void instead, but don't change the
	     type of the DECL_RESULT, in case we have a named return value.  */
	  if (ctype)
	    TREE_TYPE (decl1)
	      = build_cplus_method_type (build_type_variant (ctype,
							     TREE_READONLY (decl1),
							     TREE_SIDE_EFFECTS (decl1)),
					 void_type_node,
					 FUNCTION_ARG_CHAIN (decl1));
	  else
	    TREE_TYPE (decl1)
	      = build_function_type (void_type_node,
				     TYPE_ARG_TYPES (TREE_TYPE (decl1)));
	  DECL_RESULT (decl1)
	    = build_decl (RESULT_DECL, 0, TYPE_MAIN_VARIANT (TREE_TYPE (fntype)));
	  TREE_READONLY (DECL_RESULT (decl1))
	    = CP_TYPE_CONST_P (TREE_TYPE (fntype));
	  TREE_THIS_VOLATILE (DECL_RESULT (decl1))
	    = CP_TYPE_VOLATILE_P (TREE_TYPE (fntype));
	}

      if (TYPE_LANG_SPECIFIC (TREE_TYPE (fntype))
	  && CLASSTYPE_ABSTRACT_VIRTUALS (TREE_TYPE (fntype)))
	abstract_virtuals_error (decl1, TREE_TYPE (fntype));
    }

  /* Effective C++ rule 15.  See also c_expand_return.  */
  if (warn_ecpp
      && DECL_NAME (decl1) == ansi_opname[(int) MODIFY_EXPR]
      && TREE_CODE (TREE_TYPE (fntype)) == VOID_TYPE)
    cp_warning ("`operator=' should return a reference to `*this'");

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  DECL_INITIAL (decl1) = error_mark_node;

#ifdef SET_DEFAULT_DECL_ATTRIBUTES
  SET_DEFAULT_DECL_ATTRIBUTES (decl1, attrs);
#endif
  
  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* We must call push_template_decl after current_class_type is set
     up.  (If we are processing inline definitions after exiting a
     class scope, current_class_type will be NULL_TREE until set above
     by push_nested_class.)  */
  if (processing_template_decl)
    decl1 = push_template_decl (decl1);

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */
  if (!processing_template_decl && pre_parsed_p == 0)
    {
      /* A specialization is not used to guide overload resolution.  */
      if ((flag_guiding_decls 
	   || !DECL_TEMPLATE_SPECIALIZATION (decl1))
	  && ! DECL_FUNCTION_MEMBER_P (decl1))
	decl1 = pushdecl (decl1);
      else
	{
	  /* We need to set the DECL_CONTEXT. */
	  if (!DECL_CONTEXT (decl1) && DECL_TEMPLATE_INFO (decl1))
	    DECL_CONTEXT (decl1) = DECL_CONTEXT (DECL_TI_TEMPLATE (decl1));
	  /* And make sure we have enough default args.  */
	  check_default_args (decl1);
	}
      DECL_MAIN_VARIANT (decl1) = decl1;
      fntype = TREE_TYPE (decl1);
    }

  current_function_decl = decl1;

  if (DECL_INTERFACE_KNOWN (decl1))
    {
      tree ctx = hack_decl_function_context (decl1);

      if (DECL_NOT_REALLY_EXTERN (decl1))
	DECL_EXTERNAL (decl1) = 0;

      if (ctx != NULL_TREE && DECL_THIS_INLINE (ctx) 
	  && TREE_PUBLIC (ctx))
	/* This is a function in a local class in an extern inline
	   function.  */
	comdat_linkage (decl1);
    }
  /* If this function belongs to an interface, it is public.
     If it belongs to someone else's interface, it is also external.
     This only affects inlines and template instantiations.  */
  else if (interface_unknown == 0
	   && (! DECL_TEMPLATE_INSTANTIATION (decl1)
	       || flag_alt_external_templates))
    {
      if (DECL_THIS_INLINE (decl1) || DECL_TEMPLATE_INSTANTIATION (decl1)
	  || processing_template_decl)
	{
	  DECL_EXTERNAL (decl1)
	    = (interface_only
	       || (DECL_THIS_INLINE (decl1) && ! flag_implement_inlines
		   && !DECL_VINDEX (decl1)));

	  /* For WIN32 we also want to put these in linkonce sections.  */
	  maybe_make_one_only (decl1);
	}
      else
	DECL_EXTERNAL (decl1) = 0;
      DECL_NOT_REALLY_EXTERN (decl1) = 0;
      DECL_INTERFACE_KNOWN (decl1) = 1;
    }
  else if (interface_unknown && interface_only
	   && (! DECL_TEMPLATE_INSTANTIATION (decl1)
	       || flag_alt_external_templates))
    {
      /* If MULTIPLE_SYMBOL_SPACES is defined and we saw a #pragma
	 interface, we will have interface_only set but not
	 interface_known.  In that case, we don't want to use the normal
	 heuristics because someone will supply a #pragma implementation
	 elsewhere, and deducing it here would produce a conflict.  */
      comdat_linkage (decl1);
      DECL_EXTERNAL (decl1) = 0;
      DECL_INTERFACE_KNOWN (decl1) = 1;
      DECL_DEFER_OUTPUT (decl1) = 1;
    }
  else
    {
      /* This is a definition, not a reference.
	 So clear DECL_EXTERNAL.  */
      DECL_EXTERNAL (decl1) = 0;

      if ((DECL_THIS_INLINE (decl1) || DECL_TEMPLATE_INSTANTIATION (decl1))
	  && ! DECL_INTERFACE_KNOWN (decl1)
	  /* Don't try to defer nested functions for now.  */
	  && ! hack_decl_function_context (decl1))
	DECL_DEFER_OUTPUT (decl1) = 1;
      else
	DECL_INTERFACE_KNOWN (decl1) = 1;
    }

  if (ctype != NULL_TREE && DECL_STATIC_FUNCTION_P (decl1))
    {
      if (TREE_CODE (fntype) == METHOD_TYPE)
	TREE_TYPE (decl1) = fntype
	  = build_function_type (TREE_TYPE (fntype),
				 TREE_CHAIN (TYPE_ARG_TYPES (fntype)));
      current_function_parms = TREE_CHAIN (current_function_parms);
      DECL_ARGUMENTS (decl1) = current_function_parms;
      ctype = NULL_TREE;
    }
  restype = TREE_TYPE (fntype);

  if (ctype)
    {
      /* If we're compiling a friend function, neither of the variables
	 current_class_ptr nor current_class_type will have values.  */
      if (! doing_friend)
	{
	  /* We know that this was set up by `grokclassfn'.
	     We do not wait until `store_parm_decls', since evil
	     parse errors may never get us to that point.  Here
	     we keep the consistency between `current_class_type'
	     and `current_class_ptr'.  */
	  tree t = current_function_parms;

	  my_friendly_assert (t != NULL_TREE
			      && TREE_CODE (t) == PARM_DECL, 162);

	  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
	    {
	      int i;

	      if (! hack_decl_function_context (decl1))
		temporary_allocation ();
	      i = suspend_momentary ();

	      /* Normally, build_indirect_ref returns
		 current_class_ref whenever current_class_ptr is
		 dereferenced.  This time, however, we want it to
		 *create* current_class_ref, so we temporarily clear
		 current_class_ptr to fool it.  */
	      current_class_ptr = NULL_TREE;
	      current_class_ref = build_indirect_ref (t, NULL_PTR);
	      current_class_ptr = t;

	      resume_momentary (i);
	      if (! hack_decl_function_context (decl1))
		end_temporary_allocation ();
	    }
	  else
	    /* We're having a signature pointer here.  */
	    current_class_ref = current_class_ptr = t;

	}
    }
  else
    current_class_ptr = current_class_ref = NULL_TREE;

  pushlevel (0);
  current_binding_level->parm_flag = 1;

  GNU_xref_function (decl1, current_function_parms);

  if (attrs)
    cplus_decl_attributes (decl1, NULL_TREE, attrs);
  
  make_function_rtl (decl1);

  /* Promote the value to int before returning it.  */
  if (C_PROMOTING_INTEGER_TYPE_P (restype))
    restype = type_promotes_to (restype);

  /* If this fcn was already referenced via a block-scope `extern' decl
     (or an implicit decl), propagate certain information about the usage.  */
  if (TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (decl1)))
    TREE_ADDRESSABLE (decl1) = 1;

  if (DECL_RESULT (decl1) == NULL_TREE)
    {
      DECL_RESULT (decl1)
	= build_decl (RESULT_DECL, 0, TYPE_MAIN_VARIANT (restype));
      TREE_READONLY (DECL_RESULT (decl1)) = CP_TYPE_CONST_P (restype);
      TREE_THIS_VOLATILE (DECL_RESULT (decl1)) = CP_TYPE_VOLATILE_P (restype);
    }

  /* Allocate further tree nodes temporarily during compilation
     of this function only.  Tiemann moved up here from bottom of fn.  */
  /* If this is a nested function, then we must continue to allocate RTL
     on the permanent obstack in case we need to inline it later.  */
  if (! hack_decl_function_context (decl1))
    temporary_allocation ();

  if (processing_template_decl)
    {
      ++minimal_parse_mode;
      last_tree = DECL_SAVED_TREE (decl1)
	= build_nt (EXPR_STMT, void_zero_node);
    }

  ++function_depth;

  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl1))
      && DECL_LANGUAGE (decl1) == lang_cplusplus)
    {
      dtor_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      ctor_label = NULL_TREE;
    }
  else
    {
      dtor_label = NULL_TREE;
      if (DECL_CONSTRUCTOR_P (decl1))
	ctor_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
    }

  return 1;
}

/* Called after store_parm_decls for a function-try-block.  We need to update
   last_parm_cleanup_insn so that the base initializers for a constructor
   are run within this block, not before it.  */

void
expand_start_early_try_stmts ()
{
  expand_start_try_stmts ();
  last_parm_cleanup_insn = get_last_insn ();
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   Also install to binding contour return value identifier, if any.  */

void
store_parm_decls ()
{
  register tree fndecl = current_function_decl;
  register tree parm;
  int parms_have_cleanups = 0;
  tree cleanups = NULL_TREE;

  /* This is either a chain of PARM_DECLs (when a prototype is used).  */
  tree specparms = current_function_parms;

  /* This is a list of types declared among parms in a prototype.  */
  tree parmtags = current_function_parm_tags;

  /* This is a chain of any other decls that came in among the parm
     declarations.  If a parm is declared with  enum {foo, bar} x;
     then CONST_DECLs for foo and bar are put here.  */
  tree nonparms = NULL_TREE;

  if (toplevel_bindings_p ())
    fatal ("parse errors have confused me too much");

  /* Initialize RTL machinery.  */
  init_function_start (fndecl, input_filename, lineno);

  /* Create a binding level for the parms.  */
  expand_start_bindings (0);

  if (specparms != NULL_TREE)
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */

      register tree next;

      /* Must clear this because it might contain TYPE_DECLs declared
	 at class level.  */
      storedecls (NULL_TREE);

      for (parm = nreverse (specparms); parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      tree cleanup;
	      if (DECL_NAME (parm) == NULL_TREE)
		{
		  pushdecl (parm);
		}
	      else if (TREE_CODE (TREE_TYPE (parm)) == VOID_TYPE)
		cp_error ("parameter `%D' declared void", parm);
	      else
		{
		  /* Now fill in DECL_REFERENCE_SLOT for any of the parm decls.
		     A parameter is assumed not to have any side effects.
		     If this should change for any reason, then this
		     will have to wrap the bashed reference type in a save_expr.
		     
		     Also, if the parameter type is declared to be an X
		     and there is an X(X&) constructor, we cannot lay it
		     into the stack (any more), so we make this parameter
		     look like it is really of reference type.  Functions
		     which pass parameters to this function will know to
		     create a temporary in their frame, and pass a reference
		     to that.  */

		  if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE
		      && TYPE_SIZE (TREE_TYPE (TREE_TYPE (parm))))
		    SET_DECL_REFERENCE_SLOT (parm, convert_from_reference (parm));

		  pushdecl (parm);
		}
	      if (! processing_template_decl
		  && (cleanup = maybe_build_cleanup (parm), cleanup))
		{
		  expand_decl (parm);
		  parms_have_cleanups = 1;

		  /* Keep track of the cleanups.  */
		  cleanups = tree_cons (parm, cleanup, cleanups);
		}
	    }
	  else
	    {
	      /* If we find an enum constant or a type tag,
		 put it aside for the moment.  */
	      TREE_CHAIN (parm) = NULL_TREE;
	      nonparms = chainon (nonparms, parm);
	    }
	}

      /* Get the decls in their original chain order
	 and record in the function.  This is all and only the
	 PARM_DECLs that were pushed into scope by the loop above.  */
      DECL_ARGUMENTS (fndecl) = getdecls ();

      storetags (chainon (parmtags, gettags ()));
    }
  else
    DECL_ARGUMENTS (fndecl) = NULL_TREE;

  /* Now store the final chain of decls for the arguments
     as the decl-chain of the current lexical scope.
     Put the enumerators in as well, at the front so that
     DECL_ARGUMENTS is not modified.  */

  storedecls (chainon (nonparms, DECL_ARGUMENTS (fndecl)));

  /* Declare __FUNCTION__ and __PRETTY_FUNCTION__ for this function.  */
  declare_function_name ();

  /* Initialize the RTL code for the function.  */
  DECL_SAVED_INSNS (fndecl) = NULL_RTX;
  if (! processing_template_decl)
    expand_function_start (fndecl, parms_have_cleanups);

  current_function_parms_stored = 1;

  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (DECL_MAIN_P (fndecl))
    expand_main_function ();

  /* Now that we have initialized the parms, we can start their
     cleanups.  We cannot do this before, since expand_decl_cleanup
     should not be called before the parm can be used.  */
  if (cleanups
      && ! processing_template_decl)      
    {
      for (cleanups = nreverse (cleanups); cleanups; cleanups = TREE_CHAIN (cleanups))
	{
	  if (! expand_decl_cleanup (TREE_PURPOSE (cleanups), TREE_VALUE (cleanups)))
	    cp_error ("parser lost in parsing declaration of `%D'",
		      TREE_PURPOSE (cleanups));
	}
    }

  /* Create a binding contour which can be used to catch
     cleanup-generated temporaries.  Also, if the return value needs or
     has initialization, deal with that now.  */
  if (parms_have_cleanups)
    {
      pushlevel (0);
      expand_start_bindings (0);
    }

  if (! processing_template_decl && flag_exceptions)
    {
      /* Do the starting of the exception specifications, if we have any.  */
      if (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl)))
	expand_start_eh_spec ();
    }

  last_parm_cleanup_insn = get_last_insn ();
  last_dtor_insn = get_last_insn ();
}

/* Bind a name and initialization to the return value of
   the current function.  */

void
store_return_init (return_id, init)
     tree return_id, init;
{
  tree decl = DECL_RESULT (current_function_decl);

  if (pedantic)
    /* Give this error as many times as there are occurrences,
       so that users can use Emacs compilation buffers to find
       and fix all such places.  */
    pedwarn ("ANSI C++ does not permit named return values");

  if (return_id != NULL_TREE)
    {
      if (DECL_NAME (decl) == NULL_TREE)
	{
	  DECL_NAME (decl) = return_id;
	  DECL_ASSEMBLER_NAME (decl) = return_id;
	}
      else
	cp_error ("return identifier `%D' already in place", decl);
    }

  /* Can't let this happen for constructors.  */
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      error ("can't redefine default return value for constructors");
      return;
    }

  /* If we have a named return value, put that in our scope as well.  */
  if (DECL_NAME (decl) != NULL_TREE)
    {
      /* If this named return value comes in a register,
	 put it in a pseudo-register.  */
      if (DECL_REGISTER (decl))
	{
	  original_result_rtx = DECL_RTL (decl);
	  DECL_RTL (decl) = gen_reg_rtx (DECL_MODE (decl));
	}

      /* Let `cp_finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      pushdecl (decl);

      if (minimal_parse_mode)
	add_tree (build_min_nt (RETURN_INIT, return_id,
				copy_to_permanent (init)));
      else
	cp_finish_decl (decl, init, NULL_TREE, 0, 0);
    }
}


/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.
   LINENO is the current line number.

   FLAGS is a bitwise or of the following values: 
     1 - CALL_POPLEVEL 
       An extra call to poplevel (and expand_end_bindings) must be
       made to take care of the binding contour for the base
       initializers.  This is only relevant for constructors.
     2 - INCLASS_INLINE
       We just finished processing the body of an in-class inline
       function definition.  (This processing will have taken place
       after the class definition is complete.)

   NESTED is nonzero if we were in the middle of compiling another function
   when we started on this one.  */

void
finish_function (lineno, flags, nested)
     int lineno;
     int flags;
     int nested;
{
  register tree fndecl = current_function_decl;
  tree fntype, ctype = NULL_TREE;
  rtx last_parm_insn, insns;
  /* Label to use if this function is supposed to return a value.  */
  tree no_return_label = NULL_TREE;
  tree decls = NULL_TREE;
  int call_poplevel = (flags & 1) != 0;
  int inclass_inline = (flags & 2) != 0;
  int in_template;

  /* When we get some parse errors, we can end up without a
     current_function_decl, so cope.  */
  if (fndecl == NULL_TREE)
    return;

  if (function_depth > 1)
    nested = 1;

  fntype = TREE_TYPE (fndecl);

/*  TREE_READONLY (fndecl) = 1;
    This caused &foo to be of type ptr-to-const-function
    which then got a warning when stored in a ptr-to-function variable.  */

  /* This happens on strange parse errors.  */
  if (! current_function_parms_stored)
    {
      call_poplevel = 0;
      store_parm_decls ();
    }

  if (processing_template_decl)
    {
      if (DECL_CONSTRUCTOR_P (fndecl) && call_poplevel)
	{
	  decls = getdecls ();
	  expand_end_bindings (decls, decls != NULL_TREE, 0);
	  poplevel (decls != NULL_TREE, 0, 0);
	}
    }
  else
    {
      if (write_symbols != NO_DEBUG /*&& TREE_CODE (fntype) != METHOD_TYPE*/)
	{
	  tree ttype = target_type (fntype);
	  tree parmdecl;

	  if (IS_AGGR_TYPE (ttype))
	    /* Let debugger know it should output info for this type.  */
	    note_debug_info_needed (ttype);

	  for (parmdecl = DECL_ARGUMENTS (fndecl); parmdecl; parmdecl = TREE_CHAIN (parmdecl))
	    {
	      ttype = target_type (TREE_TYPE (parmdecl));
	      if (IS_AGGR_TYPE (ttype))
		/* Let debugger know it should output info for this type.  */
		note_debug_info_needed (ttype);
	    }
	}

      /* Clean house because we will need to reorder insns here.  */
      do_pending_stack_adjust ();

      if (dtor_label)
	{
	  tree binfo = TYPE_BINFO (current_class_type);
	  tree cond = integer_one_node;
	  tree exprstmt;
	  tree in_charge_node = lookup_name (in_charge_identifier, 0);
	  tree virtual_size;
	  int ok_to_optimize_dtor = 0;
	  int empty_dtor = get_last_insn () == last_dtor_insn;

	  if (current_function_assigns_this)
	    cond = build (NE_EXPR, boolean_type_node,
			  current_class_ptr, integer_zero_node);
	  else
	    {
	      int n_baseclasses = CLASSTYPE_N_BASECLASSES (current_class_type);

	      /* If this destructor is empty, then we don't need to check
		 whether `this' is NULL in some cases.  */
	      if ((flag_this_is_variable & 1) == 0)
		ok_to_optimize_dtor = 1;
	      else if (empty_dtor)
		ok_to_optimize_dtor
		  = (n_baseclasses == 0
		     || (n_baseclasses == 1
			 && TYPE_HAS_DESTRUCTOR (TYPE_BINFO_BASETYPE (current_class_type, 0))));
	    }

	  /* These initializations might go inline.  Protect
	     the binding level of the parms.  */
	  pushlevel (0);
	  expand_start_bindings (0);

	  if (current_function_assigns_this)
	    {
	      current_function_assigns_this = 0;
	      current_function_just_assigned_this = 0;
	    }

	  /* Generate the code to call destructor on base class.
	     If this destructor belongs to a class with virtual
	     functions, then set the virtual function table
	     pointer to represent the type of our base class.  */

	  /* This side-effect makes call to `build_delete' generate the
	     code we have to have at the end of this destructor.
	     `build_delete' will set the flag again.  */
	  TYPE_HAS_DESTRUCTOR (current_class_type) = 0;

	  /* These are two cases where we cannot delegate deletion.  */
	  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type)
	      || TYPE_GETS_REG_DELETE (current_class_type))
	    exprstmt = build_delete (current_class_type, current_class_ref, integer_zero_node,
				     LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_NORMAL, 0);
	  else
	    exprstmt = build_delete (current_class_type, current_class_ref, in_charge_node,
				     LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_NORMAL, 0);

	  /* If we did not assign to this, then `this' is non-zero at
	     the end of a destructor.  As a special optimization, don't
	     emit test if this is an empty destructor.  If it does nothing,
	     it does nothing.  If it calls a base destructor, the base
	     destructor will perform the test.  */

	  if (exprstmt != error_mark_node
	      && (TREE_CODE (exprstmt) != NOP_EXPR
		  || TREE_OPERAND (exprstmt, 0) != integer_zero_node
		  || TYPE_USES_VIRTUAL_BASECLASSES (current_class_type)))
	    {
	      expand_label (dtor_label);
	      if (cond != integer_one_node)
		expand_start_cond (cond, 0);
	      if (exprstmt != void_zero_node)
		/* Don't call `expand_expr_stmt' if we're not going to do
		   anything, since -Wall will give a diagnostic.  */
		expand_expr_stmt (exprstmt);

	      /* Run destructor on all virtual baseclasses.  */
	      if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
		{
		  tree vbases = nreverse (copy_list (CLASSTYPE_VBASECLASSES (current_class_type)));
		  expand_start_cond (build (BIT_AND_EXPR, integer_type_node,
					    in_charge_node, integer_two_node), 0);
		  while (vbases)
		    {
		      if (TYPE_NEEDS_DESTRUCTOR (BINFO_TYPE (vbases)))
			{
			  tree vb = get_vbase
			    (BINFO_TYPE (vbases),
			     TYPE_BINFO (current_class_type));
			  expand_expr_stmt
			    (build_scoped_method_call
			     (current_class_ref, vb, dtor_identifier,
			      build_expr_list (NULL_TREE, integer_zero_node)));
			}
		      vbases = TREE_CHAIN (vbases);
		    }
		  expand_end_cond ();
		}

	      do_pending_stack_adjust ();
	      if (cond != integer_one_node)
		expand_end_cond ();
	    }

	  virtual_size = c_sizeof (current_class_type);

	  /* At the end, call delete if that's what's requested.  */

	  /* FDIS sez: At the point of definition of a virtual destructor
	       (including an implicit definition), non-placement operator
	       delete shall be looked up in the scope of the destructor's
	       class and if found shall be accessible and unambiguous.

	     This is somewhat unclear, but I take it to mean that if the
	     class only defines placement deletes we don't do anything here.
	     So we pass LOOKUP_SPECULATIVELY; delete_sanity will complain
	     for us if they ever try to delete one of these.  */

	  if (TYPE_GETS_REG_DELETE (current_class_type)
	      || TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
	    exprstmt = build_op_delete_call
	      (DELETE_EXPR, current_class_ptr, virtual_size,
	       LOOKUP_NORMAL | LOOKUP_SPECULATIVELY, NULL_TREE);
	  else
	    exprstmt = NULL_TREE;

	  if (exprstmt)
	    {
	      cond = build (BIT_AND_EXPR, integer_type_node,
			    in_charge_node, integer_one_node);
	      expand_start_cond (cond, 0);
	      expand_expr_stmt (exprstmt);
	      expand_end_cond ();
	    }

	  /* End of destructor.  */
	  expand_end_bindings (NULL_TREE, getdecls () != NULL_TREE, 0);
	  poplevel (getdecls () != NULL_TREE, 0, 0);

	  /* Back to the top of destructor.  */
	  /* Don't execute destructor code if `this' is NULL.  */

	  start_sequence ();

	  /* If the dtor is empty, and we know there is not possible way we
	     could use any vtable entries, before they are possibly set by
	     a base class dtor, we don't have to setup the vtables, as we
	     know that any base class dtoring will set up any vtables it
	     needs.  We avoid MI, because one base class dtor can do a
	     virtual dispatch to an overridden function that would need to
	     have a non-related vtable set up, we cannot avoid setting up
	     vtables in that case.  We could change this to see if there is
	     just one vtable.  */
	  if (! empty_dtor || TYPE_USES_COMPLEX_INHERITANCE (current_class_type))
	    {
	      /* Make all virtual function table pointers in non-virtual base
		 classes point to CURRENT_CLASS_TYPE's virtual function
		 tables.  */
	      expand_direct_vtbls_init (binfo, binfo, 1, 0, current_class_ptr);

	      if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
		expand_indirect_vtbls_init (binfo, current_class_ref, current_class_ptr);
	    }
	  
	  if (! ok_to_optimize_dtor)
	    {
	      cond = build_binary_op (NE_EXPR,
				      current_class_ptr, integer_zero_node);
	      expand_start_cond (cond, 0);
	    }

	  insns = get_insns ();
	  end_sequence ();

	  last_parm_insn = get_first_nonparm_insn ();
	  if (last_parm_insn == NULL_RTX)
	    last_parm_insn = get_last_insn ();
	  else
	    last_parm_insn = previous_insn (last_parm_insn);

	  emit_insns_after (insns, last_parm_insn);

	  if (! ok_to_optimize_dtor)
	    expand_end_cond ();
	}
      else if (current_function_assigns_this)
	{
	  /* Does not need to call emit_base_init, because
	     that is done (if needed) just after assignment to this
	     is seen.  */

	  if (DECL_CONSTRUCTOR_P (current_function_decl))
	    {
	      end_protect_partials ();
	      expand_label (ctor_label);
	      ctor_label = NULL_TREE;

	      if (call_poplevel)
		{
		  decls = getdecls ();
		  expand_end_bindings (decls, decls != NULL_TREE, 0);
		  poplevel (decls != NULL_TREE, 0, 0);
		}
	      /* c_expand_return knows to return 'this' from a constructor.  */
	      c_expand_return (NULL_TREE);
	    }
	  else if (TREE_CODE (TREE_TYPE (DECL_RESULT (current_function_decl))) != VOID_TYPE
		   && return_label != NULL_RTX)
	    no_return_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	  current_function_assigns_this = 0;
	  current_function_just_assigned_this = 0;
	  base_init_expr = NULL_TREE;
	}
      else if (DECL_CONSTRUCTOR_P (fndecl))
	{
	  tree cond = NULL_TREE, thenclause = NULL_TREE;
	  /* Allow constructor for a type to get a new instance of the object
	     using `build_new'.  */
	  tree abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type);
	  CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type) = NULL_TREE;

	  if (flag_this_is_variable > 0)
	    {
	      cond = build_binary_op (EQ_EXPR,
				      current_class_ptr, integer_zero_node);
	      thenclause = build_modify_expr (current_class_ptr, NOP_EXPR,
					      build_new (NULL_TREE, current_class_type, void_type_node, 0));
	    }

	  CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type) = abstract_virtuals;

	  start_sequence ();

	  if (flag_this_is_variable > 0)
	    {
	      expand_start_cond (cond, 0);
	      expand_expr_stmt (thenclause);
	      expand_end_cond ();
	    }

	  /* Emit insns from `emit_base_init' which sets up virtual
	     function table pointer(s).  */
	  if (base_init_expr)
	    {
	      expand_expr_stmt (base_init_expr);
	      base_init_expr = NULL_TREE;
	    }

	  insns = get_insns ();
	  end_sequence ();

	  /* This is where the body of the constructor begins.  */

	  emit_insns_after (insns, last_parm_cleanup_insn);

	  end_protect_partials ();

	  /* This is where the body of the constructor ends.  */
	  expand_label (ctor_label);
	  ctor_label = NULL_TREE;

	  if (call_poplevel)
	    {
	      decls = getdecls ();
	      expand_end_bindings (decls, decls != NULL_TREE, 0);
	      poplevel (decls != NULL_TREE, 1, 0);
	    }

	  /* c_expand_return knows to return 'this' from a constructor.  */
	  c_expand_return (NULL_TREE);

	  current_function_assigns_this = 0;
	  current_function_just_assigned_this = 0;
	}
      else if (DECL_MAIN_P (fndecl))
	{
	  /* Make it so that `main' always returns 0 by default.  */
#ifdef VMS
	  c_expand_return (integer_one_node);
#else
	  c_expand_return (integer_zero_node);
#endif
	}
      else if (return_label != NULL_RTX
	       && current_function_return_value == NULL_TREE
	       && ! DECL_NAME (DECL_RESULT (current_function_decl)))
	no_return_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

      if (flag_exceptions)
	expand_exception_blocks ();

      /* If this function is supposed to return a value, ensure that
	 we do not fall into the cleanups by mistake.  The end of our
	 function will look like this:
	 
	 user code (may have return stmt somewhere)
	 goto no_return_label
	 cleanup_label:
	 cleanups
	 goto return_label
	 no_return_label:
	 NOTE_INSN_FUNCTION_END
	 return_label:
	 things for return
	 
	 If the user omits a return stmt in the USER CODE section, we
	 will have a control path which reaches NOTE_INSN_FUNCTION_END.
	 Otherwise, we won't.  */
      if (no_return_label)
	{
	  DECL_CONTEXT (no_return_label) = fndecl;
	  DECL_INITIAL (no_return_label) = error_mark_node;
	  DECL_SOURCE_FILE (no_return_label) = input_filename;
	  DECL_SOURCE_LINE (no_return_label) = lineno;
	  expand_goto (no_return_label);
	}

      if (cleanup_label)
	{
	  /* Remove the binding contour which is used
	     to catch cleanup-generated temporaries.  */
	  expand_end_bindings (0, 0, 0);
	  poplevel (0, 0, 0);

	  /* Emit label at beginning of cleanup code for parameters.  */
	  emit_label (cleanup_label);
	}

      /* Get return value into register if that's where it's supposed to be.  */
      if (original_result_rtx)
	fixup_result_decl (DECL_RESULT (fndecl), original_result_rtx);

      /* Finish building code that will trigger warnings if users forget
	 to make their functions return values.  */
      if (no_return_label || cleanup_label)
	emit_jump (return_label);
      if (no_return_label)
	{
	  /* We don't need to call `expand_*_return' here because we
	     don't need any cleanups here--this path of code is only
	     for error checking purposes.  */
	  expand_label (no_return_label);
	}

      /* Generate rtl for function exit.  */
      expand_function_end (input_filename, lineno, 1);
    }
  
  /* If we're processing a template, squirrel away the definition
     until we do an instantiation.  */
  if (processing_template_decl)
    {
      --minimal_parse_mode;
      DECL_SAVED_TREE (fndecl) = TREE_CHAIN (DECL_SAVED_TREE (fndecl));
      /* We have to save this value here in case
	 maybe_end_member_template_processing decides to pop all the
	 template parameters.  */
      in_template = 1;
    }
  else
    in_template = 0;

  /* This must come after expand_function_end because cleanups might
     have declarations (from inline functions) that need to go into
     this function's blocks.  */
  if (current_binding_level->parm_flag != 1)
    my_friendly_abort (122);
  poplevel (1, 0, 1);

  /* If this is a in-class inline definition, we may have to pop the
     bindings for the template parameters that we added in
     maybe_begin_member_template_processing when start_function was
     called.  */
  if (inclass_inline)
    maybe_end_member_template_processing ();

  /* Reset scope for C++: if we were in the scope of a class,
     then when we finish this function, we are not longer so.
     This cannot be done until we know for sure that no more
     class members will ever be referenced in this function
     (i.e., calls to destructors).  */
  if (current_class_name)
    {
      ctype = current_class_type;
      pop_nested_class ();
    }

  /* Must mark the RESULT_DECL as being in this function.  */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* Set the BLOCK_SUPERCONTEXT of the outermost function scope to point
     to the FUNCTION_DECL node itself.  */
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  if (!in_template)
    {
      int saved_flag_keep_inline_functions =
	flag_keep_inline_functions;

      /* So we can tell if jump_optimize sets it to 1.  */
      can_reach_end = 0;

      if (DECL_CONTEXT (fndecl) != NULL_TREE
	  && hack_decl_function_context (fndecl))
	/* Trick rest_of_compilation into not deferring output of this
	   function, even if it is inline, since the rtl_obstack for
	   this function is the function_obstack of the enclosing
	   function and will be deallocated when the enclosing
	   function is gone.  See save_tree_status.  */
	flag_keep_inline_functions = 1;

      /* Run the optimizers and output the assembler code for this
         function.  */

      if (DECL_ARTIFICIAL (fndecl))
	{
	  /* Do we really *want* to inline this synthesized method?  */

	  int save_fif = flag_inline_functions;
	  flag_inline_functions = 1;

	  /* Turn off DECL_INLINE for the moment so function_cannot_inline_p
	     will check our size.  */
	  DECL_INLINE (fndecl) = 0;

	  rest_of_compilation (fndecl);
	  flag_inline_functions = save_fif;
	}
      else
	rest_of_compilation (fndecl);

      flag_keep_inline_functions = saved_flag_keep_inline_functions;

      if (DECL_SAVED_INSNS (fndecl) && ! TREE_ASM_WRITTEN (fndecl))
	{
	  /* Set DECL_EXTERNAL so that assemble_external will be called as
	     necessary.  We'll clear it again in finish_file.  */
	  if (! DECL_EXTERNAL (fndecl))
	    DECL_NOT_REALLY_EXTERN (fndecl) = 1;
	  DECL_EXTERNAL (fndecl) = 1;
	  mark_inline_for_output (fndecl);
	}

      if (ctype && TREE_ASM_WRITTEN (fndecl))
	note_debug_info_needed (ctype);

      current_function_returns_null |= can_reach_end;

      /* Since we don't normally go through c_expand_return for constructors,
	 this normally gets the wrong value.
	 Also, named return values have their return codes emitted after
	 NOTE_INSN_FUNCTION_END, confusing jump.c.  */
      if (DECL_CONSTRUCTOR_P (fndecl)
	  || DECL_NAME (DECL_RESULT (fndecl)) != NULL_TREE)
	current_function_returns_null = 0;

      if (TREE_THIS_VOLATILE (fndecl) && current_function_returns_null)
	cp_warning ("`noreturn' function `%D' does return", fndecl);
      else if ((warn_return_type || pedantic)
	       && current_function_returns_null
	       && TREE_CODE (TREE_TYPE (fntype)) != VOID_TYPE)
	{
	  /* If this function returns non-void and control can drop through,
	     complain.  */
	  cp_warning ("control reaches end of non-void function `%D'", fndecl);
	}
      /* With just -W, complain only if function returns both with
	 and without a value.  */
      else if (extra_warnings
	       && current_function_returns_value && current_function_returns_null)
	warning ("this function may return with or without a value");
    }

  --function_depth;

  /* Free all the tree nodes making up this function.  */
  /* Switch back to allocating nodes permanently
     until we start another function.  */
  if (! nested)
    permanent_allocation (1);

  if (DECL_SAVED_INSNS (fndecl) == NULL_RTX)
    {
      tree t;

      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (fndecl) = error_mark_node;
      for (t = DECL_ARGUMENTS (fndecl); t; t = TREE_CHAIN (t))
	DECL_RTL (t) = DECL_INCOMING_RTL (t) = NULL_RTX;
    }

  if (DECL_STATIC_CONSTRUCTOR (fndecl))
    static_ctors = perm_tree_cons (NULL_TREE, fndecl, static_ctors);
  if (DECL_STATIC_DESTRUCTOR (fndecl))
    static_dtors = perm_tree_cons (NULL_TREE, fndecl, static_dtors);

  if (! nested)
    {
      /* Let the error reporting routines know that we're outside a
         function.  For a nested function, this value is used in
         pop_cp_function_context and then reset via pop_function_context.  */
      current_function_decl = NULL_TREE;
    }

  named_label_uses = NULL;
  current_class_ptr = NULL_TREE;
  current_class_ref = NULL_TREE;
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the return type and the name of the function,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns a FUNCTION_DECL on success.

   If the DECLARATOR is not suitable for a function (it defines a datum
   instead), we return 0, which tells yyparse to report a parse error.

   May return void_type_node indicating that this method is actually
   a friend.  See grokfield for more details.

   Came here with a `.pushlevel' .

   DO NOT MAKE ANY CHANGES TO THIS CODE WITHOUT MAKING CORRESPONDING
   CHANGES TO CODE IN `grokfield'.  */

tree
start_method (declspecs, declarator, attrlist)
     tree declarator, declspecs, attrlist;
{
  tree fndecl = grokdeclarator (declarator, declspecs, MEMFUNCDEF, 0,
				attrlist);

  /* Something too ugly to handle.  */
  if (fndecl == NULL_TREE)
    return NULL_TREE;

  /* Pass friends other than inline friend functions back.  */
  if (fndecl == void_type_node)
    return fndecl;

  if (TREE_CODE (fndecl) != FUNCTION_DECL)
    /* Not a function, tell parser to report parse error.  */
    return NULL_TREE;

  if (IS_SIGNATURE (current_class_type))
    IS_DEFAULT_IMPLEMENTATION (fndecl) = 1;

  if (DECL_IN_AGGR_P (fndecl))
    {
      if (IDENTIFIER_ERROR_LOCUS (DECL_ASSEMBLER_NAME (fndecl)) != current_class_type)
	{
	  if (DECL_CONTEXT (fndecl) 
	      && TREE_CODE( DECL_CONTEXT (fndecl)) != NAMESPACE_DECL)
	    cp_error ("`%D' is already defined in class %s", fndecl,
			     TYPE_NAME_STRING (DECL_CONTEXT (fndecl)));
	}
      return void_type_node;
    }

  check_template_shadow (fndecl);

  DECL_THIS_INLINE (fndecl) = 1;

  if (flag_default_inline)
    DECL_INLINE (fndecl) = 1;

  /* We process method specializations in finish_struct_1.  */
  if (processing_template_decl && !DECL_TEMPLATE_SPECIALIZATION (fndecl))
    fndecl = push_template_decl (fndecl);

  /* We read in the parameters on the maybepermanent_obstack,
     but we won't be getting back to them until after we
     may have clobbered them.  So the call to preserve_data
     will keep them safe.  */
  preserve_data ();

  if (! DECL_FRIEND_P (fndecl))
    {
      if (TREE_CHAIN (fndecl))
	{
	  fndecl = copy_node (fndecl);
	  TREE_CHAIN (fndecl) = NULL_TREE;
	}

      if (DECL_CONSTRUCTOR_P (fndecl))
	{
	  if (! grok_ctor_properties (current_class_type, fndecl))
	    return void_type_node;
	}
      else if (IDENTIFIER_OPNAME_P (DECL_NAME (fndecl)))
	grok_op_properties (fndecl, DECL_VIRTUAL_P (fndecl), 0);
    }

  cp_finish_decl (fndecl, NULL_TREE, NULL_TREE, 0, 0);

  /* Make a place for the parms */
  pushlevel (0);
  current_binding_level->parm_flag = 1;
  
  DECL_IN_AGGR_P (fndecl) = 1;
  return fndecl;
}

/* Go through the motions of finishing a function definition.
   We don't compile this method until after the whole class has
   been processed.

   FINISH_METHOD must return something that looks as though it
   came from GROKFIELD (since we are defining a method, after all).

   This is called after parsing the body of the function definition.
   STMTS is the chain of statements that makes up the function body.

   DECL is the ..._DECL that `start_method' provided.  */

tree
finish_method (decl)
     tree decl;
{
  register tree fndecl = decl;
  tree old_initial;

  register tree link;

  if (decl == void_type_node)
    return decl;

  old_initial = DECL_INITIAL (fndecl);

  /* Undo the level for the parms (from start_method).
     This is like poplevel, but it causes nothing to be
     saved.  Saving information here confuses symbol-table
     output routines.  Besides, this information will
     be correctly output when this method is actually
     compiled.  */

  /* Clear out the meanings of the local variables of this level;
     also record in each decl which block it belongs to.  */

  for (link = current_binding_level->names; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != NULL_TREE)
	pop_binding (DECL_NAME (link), link);
      my_friendly_assert (TREE_CODE (link) != FUNCTION_DECL, 163);
      DECL_CONTEXT (link) = NULL_TREE;
    }

  GNU_xref_end_scope ((HOST_WIDE_INT) current_binding_level,
		      (HOST_WIDE_INT) current_binding_level->level_chain,
		      current_binding_level->parm_flag,
		      current_binding_level->keep);

  poplevel (0, 0, 0);

  DECL_INITIAL (fndecl) = old_initial;

  /* We used to check if the context of FNDECL was different from
     current_class_type as another way to get inside here.  This didn't work
     for String.cc in libg++.  */
  if (DECL_FRIEND_P (fndecl))
    {
      CLASSTYPE_INLINE_FRIENDS (current_class_type)
	= tree_cons (NULL_TREE, fndecl, CLASSTYPE_INLINE_FRIENDS (current_class_type));
      decl = void_type_node;
    }

  return decl;
}

/* Called when a new struct TYPE is defined.
   If this structure or union completes the type of any previous
   variable declaration, lay it out and output its rtl.  */

void
hack_incomplete_structures (type)
     tree type;
{
  tree *list;

  if (current_binding_level->incomplete == NULL_TREE)
    return;

  if (!type) /* Don't do this for class templates.  */
    return;

  for (list = &current_binding_level->incomplete; *list; )
    {
      tree decl = TREE_VALUE (*list);
      if ((decl && TREE_TYPE (decl) == type)
	  || (TREE_TYPE (decl)
	      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	      && TREE_TYPE (TREE_TYPE (decl)) == type))
	{
	  int toplevel = toplevel_bindings_p ();
	  if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	      && TREE_TYPE (TREE_TYPE (decl)) == type)
	    layout_type (TREE_TYPE (decl));
	  layout_decl (decl, 0);
	  rest_of_decl_compilation (decl, NULL_PTR, toplevel, 0);
	  if (! toplevel)
	    {
	      tree cleanup;
	      expand_decl (decl);
	      cleanup = maybe_build_cleanup (decl);
	      expand_decl_init (decl);
	      if (! expand_decl_cleanup (decl, cleanup))
		cp_error ("parser lost in parsing declaration of `%D'",
			  decl);
	    }
	  *list = TREE_CHAIN (*list);
	}
      else
	list = &TREE_CHAIN (*list);
    }
}

/* If DECL is of a type which needs a cleanup, build that cleanup here.
   See build_delete for information about AUTO_DELETE.

   Don't build these on the momentary obstack; they must live
   the life of the binding contour.  */

static tree
maybe_build_cleanup_1 (decl, auto_delete)
     tree decl, auto_delete;
{
  tree type = TREE_TYPE (decl);
  if (type != error_mark_node && TYPE_NEEDS_DESTRUCTOR (type))
    {
      int temp = 0, flags = LOOKUP_NORMAL|LOOKUP_DESTRUCTOR;
      tree rval;

      if (TREE_CODE (decl) != PARM_DECL)
	temp = suspend_momentary ();

      if (TREE_CODE (type) == ARRAY_TYPE)
	rval = decl;
      else
	{
	  mark_addressable (decl);
	  rval = build_unary_op (ADDR_EXPR, decl, 0);
	}

      /* Optimize for space over speed here.  */
      if (! TYPE_USES_VIRTUAL_BASECLASSES (type)
	  || flag_expensive_optimizations)
	flags |= LOOKUP_NONVIRTUAL;

      rval = build_delete (TREE_TYPE (rval), rval, auto_delete, flags, 0);

      if (TYPE_USES_VIRTUAL_BASECLASSES (type)
	  && ! TYPE_HAS_DESTRUCTOR (type))
	rval = build_compound_expr (expr_tree_cons (NULL_TREE, rval,
					       build_expr_list (NULL_TREE, build_vbase_delete (type, decl))));

      if (TREE_CODE (decl) != PARM_DECL)
	resume_momentary (temp);

      return rval;
    }
  return 0;
}

/* If DECL is of a type which needs a cleanup, build that cleanup
   here.  The cleanup does free the storage with a call to delete.  */

tree
maybe_build_cleanup_and_delete (decl)
     tree decl;
{
  return maybe_build_cleanup_1 (decl, integer_three_node);
}

/* If DECL is of a type which needs a cleanup, build that cleanup
   here.  The cleanup does not free the storage with a call a delete.  */

tree
maybe_build_cleanup (decl)
     tree decl;
{
  return maybe_build_cleanup_1 (decl, integer_two_node);
}

/* Expand a C++ expression at the statement level.
   This is needed to ferret out nodes which have UNKNOWN_TYPE.
   The C++ type checker should get all of these out when
   expressions are combined with other, type-providing, expressions,
   leaving only orphan expressions, such as:

   &class::bar;		/ / takes its address, but does nothing with it.  */

void
cplus_expand_expr_stmt (exp)
     tree exp;
{
  if (processing_template_decl)
    {
      add_tree (build_min_nt (EXPR_STMT, exp));
      return;
    }

  /* Arrange for all temps to disappear.  */
  expand_start_target_temps ();

  exp = require_complete_type_in_void (exp);
  
  if (TREE_CODE (exp) == FUNCTION_DECL)
    {
      cp_warning ("reference, not call, to function `%D'", exp);
      warning ("at this point in file");
    }

#if 0
  /* We should do this eventually, but right now this causes regex.o from
     libg++ to miscompile, and tString to core dump.  */
  exp = build1 (CLEANUP_POINT_EXPR, TREE_TYPE (exp), exp);
#endif

  /* Strip unused implicit INDIRECT_REFs of references.  */
  if (TREE_CODE (exp) == INDIRECT_REF
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == REFERENCE_TYPE)
    exp = TREE_OPERAND (exp, 0);

  /* If we don't do this, we end up down inside expand_expr
     trying to do TYPE_MODE on the ERROR_MARK, and really
     go outside the bounds of the type.  */
  if (exp != error_mark_node)
    expand_expr_stmt (break_out_cleanups (exp));

  /* Clean up any pending cleanups.  This happens when a function call
     returns a cleanup-needing value that nobody uses.  */
  expand_end_target_temps ();
}

/* When a stmt has been parsed, this function is called.

   Currently, this function only does something within a
   constructor's scope: if a stmt has just assigned to this,
   and we are in a derived class, we call `emit_base_init'.  */

void
finish_stmt ()
{
  extern struct nesting *cond_stack, *loop_stack, *case_stack;

  
  if (current_function_assigns_this
      || ! current_function_just_assigned_this)
    return;
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      /* Constructors must wait until we are out of control
	 zones before calling base constructors.  */
      if (cond_stack || loop_stack || case_stack)
	return;
      expand_expr_stmt (base_init_expr);
      check_base_init (current_class_type);
    }
  current_function_assigns_this = 1;
}

/* Change a static member function definition into a FUNCTION_TYPE, instead
   of the METHOD_TYPE that we create when it's originally parsed.

   WARNING: DO NOT pass &TREE_TYPE (decl) to FN or &TYPE_ARG_TYPES
   (TREE_TYPE (decl)) to ARGTYPES, as doing so will corrupt the types of
   other decls.  Either pass the addresses of local variables or NULL.  */

void
revert_static_member_fn (decl, fn, argtypes)
     tree *decl, *fn, *argtypes;
{
  tree tmp;
  tree function = fn ? *fn : TREE_TYPE (*decl);
  tree args = argtypes ? *argtypes : TYPE_ARG_TYPES (function);

  if (CP_TYPE_QUALS (TREE_TYPE (TREE_VALUE (args))) 
      != TYPE_UNQUALIFIED)
    cp_error ("static member function `%#D' declared with type qualifiers", 
	      *decl);

  args = TREE_CHAIN (args);
  tmp = build_function_type (TREE_TYPE (function), args);
  tmp = build_qualified_type (tmp, CP_TYPE_QUALS (function));
  tmp = build_exception_variant (tmp,
				 TYPE_RAISES_EXCEPTIONS (function));
  TREE_TYPE (*decl) = tmp;
  if (DECL_ARGUMENTS (*decl))
    DECL_ARGUMENTS (*decl) = TREE_CHAIN (DECL_ARGUMENTS (*decl));
  DECL_STATIC_FUNCTION_P (*decl) = 1;
  if (fn)
    *fn = tmp;
  if (argtypes)
    *argtypes = args;
}

struct cp_function
{
  int returns_value;
  int returns_null;
  int assigns_this;
  int just_assigned_this;
  int parms_stored;
  int temp_name_counter;
  tree named_labels;
  struct named_label_list *named_label_uses;
  tree shadowed_labels;
  tree ctor_label;
  tree dtor_label;
  rtx last_dtor_insn;
  rtx last_parm_cleanup_insn;
  tree base_init_list;
  tree member_init_list;
  tree base_init_expr;
  tree current_class_ptr;
  tree current_class_ref;
  rtx result_rtx;
  struct cp_function *next;
  struct binding_level *binding_level;
  int static_labelno;
};

static struct cp_function *cp_function_chain;

extern int temp_name_counter;

/* Save and reinitialize the variables
   used during compilation of a C++ function.  */

void
push_cp_function_context (context)
     tree context;
{
  struct cp_function *p
    = (struct cp_function *) xmalloc (sizeof (struct cp_function));

  push_function_context_to (context);

  p->next = cp_function_chain;
  cp_function_chain = p;

  p->named_labels = named_labels;
  p->named_label_uses = named_label_uses;
  p->shadowed_labels = shadowed_labels;
  p->returns_value = current_function_returns_value;
  p->returns_null = current_function_returns_null;
  p->binding_level = current_binding_level;
  p->ctor_label = ctor_label;
  p->dtor_label = dtor_label;
  p->last_dtor_insn = last_dtor_insn;
  p->last_parm_cleanup_insn = last_parm_cleanup_insn;
  p->assigns_this = current_function_assigns_this;
  p->just_assigned_this = current_function_just_assigned_this;
  p->parms_stored = current_function_parms_stored;
  p->result_rtx = original_result_rtx;
  p->base_init_expr = base_init_expr;
  p->temp_name_counter = temp_name_counter;
  p->base_init_list = current_base_init_list;
  p->member_init_list = current_member_init_list;
  p->current_class_ptr = current_class_ptr;
  p->current_class_ref = current_class_ref;
  p->static_labelno = static_labelno;
}

/* Restore the variables used during compilation of a C++ function.  */

void
pop_cp_function_context (context)
     tree context;
{
  struct cp_function *p = cp_function_chain;
  tree link;

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      SET_IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)),
				  TREE_VALUE (link));

  pop_function_context_from (context);

  cp_function_chain = p->next;

  named_labels = p->named_labels;
  named_label_uses = p->named_label_uses;
  shadowed_labels = p->shadowed_labels;
  current_function_returns_value = p->returns_value;
  current_function_returns_null = p->returns_null;
  current_binding_level = p->binding_level;
  ctor_label = p->ctor_label;
  dtor_label = p->dtor_label;
  last_dtor_insn = p->last_dtor_insn;
  last_parm_cleanup_insn = p->last_parm_cleanup_insn;
  current_function_assigns_this = p->assigns_this;
  current_function_just_assigned_this = p->just_assigned_this;
  current_function_parms_stored = p->parms_stored;
  original_result_rtx = p->result_rtx;
  base_init_expr = p->base_init_expr;
  temp_name_counter = p->temp_name_counter;
  current_base_init_list = p->base_init_list;
  current_member_init_list = p->member_init_list;
  current_class_ptr = p->current_class_ptr;
  current_class_ref = p->current_class_ref;
  static_labelno = p->static_labelno;

  free (p);
}

int
in_function_p ()
{
  return function_depth != 0;
}
