/* Process declarations and variables for C compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "c-tree.h"
#include "c-lex.h"
#include <stdio.h>

/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  BITFIELD,			/* Likewise but with specified width */
  TYPENAME};			/* Typename (inside cast or sizeof)  */

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

/* a node which has tree code ERROR_MARK, and whose type is itself.
   All erroneous expressions are replaced with this node.  All functions
   that accept nodes as arguments should avoid generating error messages
   if this node is one of the arguments, since it is undesirable to get
   multiple error messages from one error in the input.  */

tree error_mark_node;

/* INTEGER_TYPE and REAL_TYPE nodes for the standard data types */

tree short_integer_type_node;
tree integer_type_node;
tree long_integer_type_node;
tree long_long_integer_type_node;

tree short_unsigned_type_node;
tree unsigned_type_node;
tree long_unsigned_type_node;
tree long_long_unsigned_type_node;

tree boolean_type_node;
tree boolean_false_node;
tree boolean_true_node;

tree ptrdiff_type_node;

tree unsigned_char_type_node;
tree signed_char_type_node;
tree char_type_node;
tree wchar_type_node;
tree signed_wchar_type_node;
tree unsigned_wchar_type_node;

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

tree unsigned_intQI_type_node;
tree unsigned_intHI_type_node;
tree unsigned_intSI_type_node;
tree unsigned_intDI_type_node;

/* a VOID_TYPE node.  */

tree void_type_node;

/* Nodes for types `void *' and `const void *'.  */

tree ptr_type_node, const_ptr_type_node;

/* Nodes for types `char *' and `const char *'.  */

tree string_type_node, const_string_type_node;

/* Type `char[SOMENUMBER]'.
   Used when an array of char is needed and the size is irrelevant.  */

tree char_array_type_node;

/* Type `int[SOMENUMBER]' or something like it.
   Used when an array of int needed and the size is irrelevant.  */

tree int_array_type_node;

/* Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.  */

tree wchar_array_type_node;

/* type `int ()' -- used for implicit declaration of functions.  */

tree default_function_type;

/* function types `double (double)' and `double (double, double)', etc.  */

tree double_ftype_double, double_ftype_double_double;
tree int_ftype_int, long_ftype_long;
tree float_ftype_float;
tree ldouble_ftype_ldouble;

/* Function type `void (void *, void *, int)' and similar ones */

tree void_ftype_ptr_ptr_int, int_ftype_ptr_ptr_int, void_ftype_ptr_int_int;

/* Function type `char *(char *, char *)' and similar ones */
tree string_ftype_ptr_ptr, int_ftype_string_string;

/* Function type `int (const void *, const void *, size_t)' */
tree int_ftype_cptr_cptr_sizet;

/* Two expressions that are constants with value zero.
   The first is of type `int', the second of type `void *'.  */

tree integer_zero_node;
tree null_pointer_node;

/* A node for the integer constant 1.  */

tree integer_one_node;

/* Nonzero if we have seen an invalid cross reference
   to a struct, union, or enum, but not yet printed the message.  */

tree pending_invalid_xref;
/* File and line to appear in the eventual error message.  */
char *pending_invalid_xref_file;
int pending_invalid_xref_line;

/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  Note that will do not have to save this or `enum_overflow'
   around nested function definition since such a definition could only
   occur in an enum value expression and we don't use these variables in
   that case.  */

static tree enum_next_value;

/* Nonzero means that there was overflow computing enum_next_value.  */

static int enum_overflow;

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */

static tree last_function_parms;

/* Parsing a function declarator leaves here a chain of structure
   and enum types declared in the parmlist.  */

static tree last_function_parm_tags;

/* After parsing the declarator that starts a function definition,
   `start_function' puts here the list of parameter names or chain of decls.
   `store_parm_decls' finds it here.  */

static tree current_function_parms;

/* Similar, for last_function_parm_tags.  */
static tree current_function_parm_tags;

/* Similar, for the file and line that the prototype came from if this is
   an old-style definition.  */
static char *current_function_prototype_file;
static int current_function_prototype_line;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static tree shadowed_labels;

/* Nonzero when store_parm_decls is called indicates a varargs function.
   Value not meaningful after store_parm_decls.  */

static int c_function_varargs;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

int current_function_returns_null;

/* Set to nonzero by `grokdeclarator' for a function
   whose return type is defaulted, if warnings for this are desired.  */

static int warn_about_return_type;

/* Nonzero when starting a function declared `extern inline'.  */

static int current_extern_inline;

/* For each binding contour we allocate a binding_level structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *  2) one for each compound statement,
 *     to record its declarations.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* A list of structure, union and enum definitions,
     * for looking up tag names.
     * It is a chain of TREE_LIST nodes, each of whose TREE_PURPOSE is a name,
     * or NULL_TREE; and whose TREE_VALUE is a RECORD_TYPE, UNION_TYPE,
     * or ENUMERAL_TYPE node.
     */
    tree tags;

    /* For each level, a list of shadowed outer-level local definitions
       to be restored when this level is popped.
       Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
       whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
    tree shadowed;

    /* For each level (except not the global one),
       a chain of BLOCK nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The BLOCK node for this level, if one has been preallocated.
       If 0, the BLOCK is allocated (if needed) when the level is popped.  */
    tree this_block;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* Nonzero for the level that holds the parameters of a function.  */
    char parm_flag;

    /* Nonzero if this level "doesn't exist" for tags.  */
    char tag_transparent;

    /* Nonzero if sublevels of this level "don't exist" for tags.
       This is set in the parm level of a function definition
       while reading the function body, so that the outermost block
       of the function body will be tag-transparent.  */
    char subblocks_tag_transparent;

    /* Nonzero means make a BLOCK for this level regardless of all else.  */
    char keep;

    /* Nonzero means make a BLOCK if this level has any subblocks.  */
    char keep_if_subblocks;

    /* Number of decls in `names' that have incomplete 
       structure or union types.  */
    int n_incomplete;

    /* A list of decls giving the (reversed) specified order of parms,
       not including any forward-decls in the parmlist.
       This is so we can put the parms in proper order for assign_parms.  */
    tree parm_order;
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL
  
/* The binding level currently in effect.  */

static struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
  = {NULL, NULL, NULL, NULL, NULL, NULL_BINDING_LEVEL, 0, 0, 0, 0, 0, 0,
     NULL};

/* Nonzero means unconditionally make a BLOCK for the next level pushed.  */

static int keep_next_level_flag;

/* Nonzero means make a BLOCK for the next level pushed
   if it has subblocks.  */

static int keep_next_if_subblocks;
  
/* The chain of outer levels of label scopes.
   This uses the same data structure used for binding levels,
   but it works differently: each link in the chain records
   saved values of named_labels and shadowed_labels for
   a label binding level outside the current one.  */

static struct binding_level *label_level_chain;

/* Functions called automatically at the beginning and end of execution.  */

tree static_ctors, static_dtors;

/* Forward declarations.  */

static tree grokparms (), grokdeclarator ();
tree pushdecl ();
tree builtin_function ();
void shadow_tag_warned ();

static tree lookup_tag ();
static tree lookup_tag_reverse ();
tree lookup_name_current_level ();
static char *redeclaration_error_message ();
static void layout_array_type ();

/* C-specific option variables.  */

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

int flag_cond_mismatch;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double;

/* Nonzero means don't recognize the keyword `asm'.  */

int flag_no_asm;

/* Nonzero means don't recognize any builtin functions.  */

int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

int flag_no_nonansi_builtin;

/* Nonzero means do some things the same way PCC does.  */

int flag_traditional;

/* Nonzero means to allow single precision math even if we're generally
   being traditional. */
int flag_allow_single_precision = 0;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */

int flag_signed_bitfields = 1;
int explicit_flag_signed_bitfields = 0;

/* Nonzero means handle `#ident' directives.  0 means ignore them.  */

int flag_no_ident = 0;

/* Nonzero means warn about implicit declarations.  */

int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

int warn_write_strings;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

int warn_cast_qual;

/* Nonzero means warn when casting a function call to a type that does
   not match the return type (e.g. (float)sqrt() or (anything*)malloc()
   when there is no previous declaration of sqrt or malloc.  */

int warn_bad_function_cast;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */

int warn_traditional;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

int warn_pointer_arith;

/* Nonzero means warn for non-prototype function decls
   or non-prototyped defs without previous prototype.  */

int warn_strict_prototypes;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */

int warn_missing_prototypes;

/* Nonzero means warn for any global function def
   without separate previous decl.  */

int warn_missing_declarations;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

int warn_redundant_decls = 0;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */

int warn_nested_externs = 0;

/* Warn about *printf or *scanf format/argument anomalies. */

int warn_format;

/* Warn about a subscript that has type char.  */

int warn_char_subscripts = 0;

/* Warn if a type conversion is done that might have confusing results.  */

int warn_conversion;

/* Warn if adding () is suggested.  */

int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */

int warn_missing_braces;

/* Nonzero means `$' can be in an identifier.
   See cccp.c for reasons why this breaks some obscure ANSI C programs.  */

#ifndef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 1
#endif
int dollars_in_ident = DOLLARS_IN_IDENTIFIERS > 1;

/* Decode the string P as a language-specific option for C.
   Return 1 if it is recognized (and handle it);
   return 0 if not recognized.  */
   
int
c_decode_option (p)
     char *p;
{
  if (!strcmp (p, "-ftraditional") || !strcmp (p, "-traditional"))
    {
      flag_traditional = 1;
      flag_writable_strings = 1;
#if DOLLARS_IN_IDENTIFIERS > 0
      dollars_in_ident = 1;
#endif
    }
  else if (!strcmp (p, "-fallow-single-precision"))
    flag_allow_single_precision = 1;
  else if (!strcmp (p, "-fnotraditional") || !strcmp (p, "-fno-traditional"))
    {
      flag_traditional = 0;
      flag_writable_strings = 0;
      dollars_in_ident = DOLLARS_IN_IDENTIFIERS > 1;
    }
  else if (!strcmp (p, "-fdollars-in-identifiers"))
    {
#if DOLLARS_IN_IDENTIFIERS > 0
      dollars_in_ident = 1;
#endif
    }
  else if (!strcmp (p, "-fno-dollars-in-identifiers"))
    dollars_in_ident = 0;
  else if (!strcmp (p, "-fsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-funsigned-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-signed-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-unsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-fsigned-bitfields")
	   || !strcmp (p, "-fno-unsigned-bitfields"))
    {
      flag_signed_bitfields = 1;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-funsigned-bitfields")
	   || !strcmp (p, "-fno-signed-bitfields"))
    {
      flag_signed_bitfields = 0;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-fshort-enums"))
    flag_short_enums = 1;
  else if (!strcmp (p, "-fno-short-enums"))
    flag_short_enums = 0;
  else if (!strcmp (p, "-fcond-mismatch"))
    flag_cond_mismatch = 1;
  else if (!strcmp (p, "-fno-cond-mismatch"))
    flag_cond_mismatch = 0;
  else if (!strcmp (p, "-fshort-double"))
    flag_short_double = 1;
  else if (!strcmp (p, "-fno-short-double"))
    flag_short_double = 0;
  else if (!strcmp (p, "-fasm"))
    flag_no_asm = 0;
  else if (!strcmp (p, "-fno-asm"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-fbuiltin"))
    flag_no_builtin = 0;
  else if (!strcmp (p, "-fno-builtin"))
    flag_no_builtin = 1;
  else if (!strcmp (p, "-fno-ident"))
    flag_no_ident = 1;
  else if (!strcmp (p, "-fident"))
    flag_no_ident = 0;
  else if (!strcmp (p, "-ansi"))
    flag_no_asm = 1, flag_no_nonansi_builtin = 1, dollars_in_ident = 0;
  else if (!strcmp (p, "-Wimplicit"))
    warn_implicit = 1;
  else if (!strcmp (p, "-Wno-implicit"))
    warn_implicit = 0;
  else if (!strcmp (p, "-Wwrite-strings"))
    warn_write_strings = 1;
  else if (!strcmp (p, "-Wno-write-strings"))
    warn_write_strings = 0;
  else if (!strcmp (p, "-Wcast-qual"))
    warn_cast_qual = 1;
  else if (!strcmp (p, "-Wno-cast-qual"))
    warn_cast_qual = 0;
  else if (!strcmp (p, "-Wbad-function-cast"))
    warn_bad_function_cast = 1;
  else if (!strcmp (p, "-Wno-bad-function-cast"))
    warn_bad_function_cast = 0;
  else if (!strcmp (p, "-Wpointer-arith"))
    warn_pointer_arith = 1;
  else if (!strcmp (p, "-Wno-pointer-arith"))
    warn_pointer_arith = 0;
  else if (!strcmp (p, "-Wstrict-prototypes"))
    warn_strict_prototypes = 1;
  else if (!strcmp (p, "-Wno-strict-prototypes"))
    warn_strict_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-prototypes"))
    warn_missing_prototypes = 1;
  else if (!strcmp (p, "-Wno-missing-prototypes"))
    warn_missing_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-declarations"))
    warn_missing_declarations = 1;
  else if (!strcmp (p, "-Wno-missing-declarations"))
    warn_missing_declarations = 0;
  else if (!strcmp (p, "-Wredundant-decls"))
    warn_redundant_decls = 1;
  else if (!strcmp (p, "-Wno-redundant-decls"))
    warn_redundant_decls = 0;
  else if (!strcmp (p, "-Wnested-externs"))
    warn_nested_externs = 1;
  else if (!strcmp (p, "-Wno-nested-externs"))
    warn_nested_externs = 0;
  else if (!strcmp (p, "-Wtraditional"))
    warn_traditional = 1;
  else if (!strcmp (p, "-Wno-traditional"))
    warn_traditional = 0;
  else if (!strcmp (p, "-Wformat"))
    warn_format = 1;
  else if (!strcmp (p, "-Wno-format"))
    warn_format = 0;
  else if (!strcmp (p, "-Wchar-subscripts"))
    warn_char_subscripts = 1;
  else if (!strcmp (p, "-Wno-char-subscripts"))
    warn_char_subscripts = 0;
  else if (!strcmp (p, "-Wconversion"))
    warn_conversion = 1;
  else if (!strcmp (p, "-Wno-conversion"))
    warn_conversion = 0;
  else if (!strcmp (p, "-Wparentheses"))
    warn_parentheses = 1;
  else if (!strcmp (p, "-Wno-parentheses"))
    warn_parentheses = 0;
  else if (!strcmp (p, "-Wreturn-type"))
    warn_return_type = 1;
  else if (!strcmp (p, "-Wno-return-type"))
    warn_return_type = 0;
  else if (!strcmp (p, "-Wcomment"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-comment"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wcomments"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-comments"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wtrigraphs"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-trigraphs"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wimport"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wno-import"))
    ; /* cpp handles this one.  */
  else if (!strcmp (p, "-Wmissing-braces"))
    warn_missing_braces = 1;
  else if (!strcmp (p, "-Wno-missing-braces"))
    warn_missing_braces = 0;
  else if (!strcmp (p, "-Wall"))
    {
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = 2;
      warn_implicit = 1;
      warn_return_type = 1;
      warn_unused = 1;
      warn_switch = 1;
      warn_format = 1;
      warn_char_subscripts = 1;
      warn_parentheses = 1;
      warn_missing_braces = 1;
    }
  else
    return 0;

  return 1;
}

/* Hooks for print_node.  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  print_node (file, "global", IDENTIFIER_GLOBAL_VALUE (node), indent + 4);
  print_node (file, "local", IDENTIFIER_LOCAL_VALUE (node), indent + 4);
  print_node (file, "label", IDENTIFIER_LABEL_VALUE (node), indent + 4);
  print_node (file, "implicit", IDENTIFIER_IMPLICIT_DECL (node), indent + 4);
  print_node (file, "error locus", IDENTIFIER_ERROR_LOCUS (node), indent + 4);
  print_node (file, "limbo value", IDENTIFIER_LIMBO_VALUE (node), indent + 4);
}

/* Hook called at end of compilation to assume 1 elt
   for a top-level array decl that wasn't complete before.  */
   
void
finish_incomplete_decl (decl)
     tree decl;
{
  if (TREE_CODE (decl) == VAR_DECL && TREE_TYPE (decl) != error_mark_node)
    {
      tree type = TREE_TYPE (decl);
      if (TREE_CODE (type) == ARRAY_TYPE
	  && TYPE_DOMAIN (type) == 0
	  && TREE_CODE (decl) != TYPE_DECL)
	{
	  complete_array_type (type, NULL_TREE, 1);

	  layout_decl (decl, 0);
	}
    }
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

void
keep_next_level ()
{
  keep_next_level_flag = 1;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return ((current_binding_level->keep_if_subblocks
	   && current_binding_level->blocks != 0)
	  || current_binding_level->keep
	  || current_binding_level->names != 0
	  || (current_binding_level->tags != 0
	      && !current_binding_level->tag_transparent));
}

/* Identify this binding level as a level of parameters.
   DEFINITION_FLAG is 1 for a definition, 0 for a declaration.
   But it turns out there is no way to pass the right value for
   DEFINITION_FLAG, so we ignore it.  */

void
declare_parm_level (definition_flag)
     int definition_flag;
{
  current_binding_level->parm_flag = 1;
}

/* Nonzero if currently making parm declarations.  */

int
in_parm_level_p ()
{
  return current_binding_level->parm_flag;
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
     just make sure that NAMED_LABELS is 0.  */

  if (current_binding_level == global_binding_level)
    {
      named_labels = 0;
    }

  /* Reuse or create a struct for this binding level.  */

  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }

  /* Add this level to the front of the chain (stack) of levels that
     are active.  */

  *newlevel = clear_binding_level;
  newlevel->tag_transparent
    = (tag_transparent
       || (current_binding_level
	   ? current_binding_level->subblocks_tag_transparent
	   : 0));
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
  newlevel->keep = keep_next_level_flag;
  keep_next_level_flag = 0;
  newlevel->keep_if_subblocks = keep_next_if_subblocks;
  keep_next_if_subblocks = 0;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
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
  tree tags = current_binding_level->tags;
  tree subblocks = current_binding_level->blocks;
  tree block = 0;
  tree decl;
  int block_previously_created;

  keep |= current_binding_level->keep;

  /* This warning is turned off because it causes warnings for
     declarations like `extern struct foo *x'.  */
#if 0
  /* Warn about incomplete structure types in this level.  */
  for (link = tags; link; link = TREE_CHAIN (link))
    if (TYPE_SIZE (TREE_VALUE (link)) == 0)
      {
	tree type = TREE_VALUE (link);
	char *errmsg;
	switch (TREE_CODE (type))
	  {
	  case RECORD_TYPE:
	    errmsg = "`struct %s' incomplete in scope ending here";
	    break;
	  case UNION_TYPE:
	    errmsg = "`union %s' incomplete in scope ending here";
	    break;
	  case ENUMERAL_TYPE:
	    errmsg = "`enum %s' incomplete in scope ending here";
	    break;
	  }
	if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	  error (errmsg, IDENTIFIER_POINTER (TYPE_NAME (type)));
	else
	  /* If this type has a typedef-name, the TYPE_NAME is a TYPE_DECL.  */
	  error (errmsg, IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
      }
#endif /* 0 */

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
	&& DECL_INITIAL (decl) != 0
	&& TREE_ADDRESSABLE (decl))
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.

	   DECL_ABSTRACT_ORIGIN can be set to itself if warn_return_type is
	   true, since then the decl goes through save_for_inline_copying.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != 0
	    && DECL_ABSTRACT_ORIGIN (decl) != decl)
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

  block = 0;
  block_previously_created = (current_binding_level->this_block != 0);
  if (block_previously_created)
    block = current_binding_level->this_block;
  else if (keep || functionbody
	   || (current_binding_level->keep_if_subblocks && subblocks != 0))
    block = make_node (BLOCK);
  if (block != 0)
    {
      BLOCK_VARS (block) = decls;
      BLOCK_TYPE_TAGS (block) = tags;
      BLOCK_SUBBLOCKS (block) = subblocks;
      remember_end_note (block);
    }

  /* In each subblock, record that this is its superior.  */

  for (link = subblocks; link; link = TREE_CHAIN (link))
    BLOCK_SUPERCONTEXT (link) = block;

  /* Clear out the meanings of the local variables of this level.  */

  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	{
	  /* If the ident. was used or addressed via a local extern decl,
	     don't forget that fact.  */
	  if (DECL_EXTERNAL (link))
	    {
	      if (TREE_USED (link))
		TREE_USED (DECL_NAME (link)) = 1;
	      if (TREE_ADDRESSABLE (link))
		TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (link)) = 1;
	    }
	  IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
	}
    }

  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */

  for (link = current_binding_level->shadowed; link; link = TREE_CHAIN (link))
    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);

  /* If the level being exited is the top level of a function,
     check over all the labels, and clear out the current
     (function local) meanings of their names.  */

  if (functionbody)
    {
      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the BLOCK because they are
	 found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;

      /* Clear out the definitions of all label names,
	 since their scopes end here,
	 and add them to BLOCK_VARS.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  register tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == 0)
	    {
	      error_with_decl (label, "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (label));
	    }
	  else if (warn_unused && !TREE_USED (label))
	    warning_with_decl (label, "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (label)) = 0;

	  /* Put the labels into the "variables" of the
	     top-level block, so debugger can see them.  */
	  TREE_CHAIN (label) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = label;
	}
    }

  /* Pop the current level, and free the structure for reuse.  */

  {
    register struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;

    level->level_chain = free_binding_level;
    free_binding_level = level;
  }

  /* Dispose of the block that we just made inside some higher level.  */
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

  /* Set the TYPE_CONTEXTs for all of the tagged types belonging to this
     binding contour so that they point to the appropriate construct, i.e.
     either to the current FUNCTION_DECL node, or else to the BLOCK node
     we just constructed.

     Note that for tagged types whose scope is just the formal parameter
     list for some function type specification, we can't properly set
     their TYPE_CONTEXTs here, because we don't have a pointer to the
     appropriate FUNCTION_TYPE node readily available to us.  For those
     cases, the TYPE_CONTEXTs of the relevant tagged type nodes get set
     in `grokdeclarator' as soon as we have created the FUNCTION_TYPE
     node which will represent the "scope" for these "parameter list local"
     tagged types.
  */

  if (functionbody)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = current_function_decl;
  else if (block)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = block;

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
  TREE_CHAIN (block) = NULL;
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

void
push_label_level ()
{
  register struct binding_level *newlevel;

  /* Reuse or create a struct for this binding level.  */

  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }

  /* Add this level to the front of the chain (stack) of label levels.  */

  newlevel->level_chain = label_level_chain;
  label_level_chain = newlevel;

  newlevel->names = named_labels;
  newlevel->shadowed = shadowed_labels;
  named_labels = 0;
  shadowed_labels = 0;
}

void
pop_label_level ()
{
  register struct binding_level *level = label_level_chain;
  tree link, prev;

  /* Clear out the definitions of the declared labels in this level.
     Leave in the list any ordinary, non-declared labels.  */
  for (link = named_labels, prev = 0; link;)
    {
      if (C_DECLARED_LABEL_FLAG (TREE_VALUE (link)))
	{
	  if (DECL_SOURCE_LINE (TREE_VALUE (link)) == 0)
	    {
	      error_with_decl (TREE_VALUE (link),
			       "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (TREE_VALUE (link)));
	    }
	  else if (warn_unused && !TREE_USED (TREE_VALUE (link)))
	    warning_with_decl (TREE_VALUE (link), 
			       "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link))) = 0;

	  /* Delete this element from the list.  */
	  link = TREE_CHAIN (link);
	  if (prev)
	    TREE_CHAIN (prev) = link;
	  else
	    named_labels = link;
	}
      else
	{
	  prev = link;
	  link = TREE_CHAIN (link);
	}
    }

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);

  named_labels = chainon (named_labels, level->names);
  shadowed_labels = level->shadowed;

  /* Pop the current level, and free the structure for reuse.  */
  label_level_chain = label_level_chain->level_chain;
  level->level_chain = free_binding_level;
  free_binding_level = level;
}

/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

void
pushtag (name, type)
     tree name, type;
{
  register struct binding_level *b;

  /* Find the proper binding level for this type tag.  */

  for (b = current_binding_level; b->tag_transparent; b = b->level_chain)
    continue;

  if (name)
    {
      /* Record the identifier as the type's name if it has none.  */

      if (TYPE_NAME (type) == 0)
	TYPE_NAME (type) = name;
    }

  if (b == global_binding_level)
    b->tags = perm_tree_cons (name, type, b->tags);
  else
    b->tags = saveable_tree_cons (name, type, b->tags);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current binding level.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));
}

/* Handle when a new declaration NEWDECL
   has the same name as an old one OLDDECL
   in the same binding contour.
   Prints an error message if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.  */

static int
duplicate_decls (newdecl, olddecl)
     register tree newdecl, olddecl;
{
  int types_match = comptypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl));
  int new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && DECL_INITIAL (newdecl) != 0);
  tree oldtype = TREE_TYPE (olddecl);
  tree newtype = TREE_TYPE (newdecl);
  char *errmsg = 0;

  if (TREE_CODE_CLASS (TREE_CODE (olddecl)) == 'd')
    DECL_MACHINE_ATTRIBUTES (newdecl) = DECL_MACHINE_ATTRIBUTES (olddecl);

  if (TREE_CODE (newtype) == ERROR_MARK
      || TREE_CODE (oldtype) == ERROR_MARK)
    types_match = 0;

  /* New decl is completely inconsistent with the old one =>
     tell caller to replace the old one.
     This is always an error except in the case of shadowing a builtin.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && (DECL_BUILT_IN (olddecl)
	      || DECL_BUILT_IN_NONANSI (olddecl)))
	{
	  /* If you declare a built-in or predefined function name as static,
	     the old definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (!TREE_PUBLIC (newdecl))
	    {
	      if (!warn_shadow)
		;
	      else if (DECL_BUILT_IN (olddecl))
		warning_with_decl (newdecl, "shadowing built-in function `%s'");
	      else
		warning_with_decl (newdecl, "shadowing library function `%s'");
	    }
	  /* Likewise, if the built-in is not ansi, then programs can
	     override it even globally without an error.  */
	  else if (! DECL_BUILT_IN (olddecl))
	    warning_with_decl (newdecl,
			       "library function `%s' declared as non-function");

	  else if (DECL_BUILT_IN_NONANSI (olddecl))
	    warning_with_decl (newdecl,
			       "built-in function `%s' declared as non-function");
	  else
	    warning_with_decl (newdecl,
			     "built-in function `%s' declared as non-function");
	}
      else
	{
	  error_with_decl (newdecl, "`%s' redeclared as different kind of symbol");
	  error_with_decl (olddecl, "previous declaration of `%s'");
	}

      return 0;
    }

  /* For real parm decl following a forward decl,
     return 1 so old decl will be reused.  */
  if (types_match && TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && ! TREE_ASM_WRITTEN (newdecl))
    return 1;

  /* The new declaration is the same kind of object as the old one.
     The declarations may partially match.  Print warnings if they don't
     match enough.  Ultimately, copy most of the information from the new
     decl to the old one, and keep using the old one.  */

  if (flag_traditional && TREE_CODE (newdecl) == FUNCTION_DECL
      && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (newdecl)) == olddecl
      && DECL_INITIAL (olddecl) == 0)
    /* If -traditional, avoid error for redeclaring fcn
       after implicit decl.  */
    ;
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_BUILT_IN (olddecl))
    {
      /* A function declaration for a built-in function.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare a built-in function name as static, the
	     built-in definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (warn_shadow)
	    warning_with_decl (newdecl, "shadowing built-in function `%s'");
	  /* Discard the old built-in function.  */
	  return 0;
	}
      else if (!types_match)
	{
          /* Accept the return type of the new declaration if same modes.  */
	  tree oldreturntype = TREE_TYPE (TREE_TYPE (olddecl));
	  tree newreturntype = TREE_TYPE (TREE_TYPE (newdecl));

	  /* Make sure we put the new type in the same obstack as the old ones.
	     If the old types are not both in the same obstack, use the
	     permanent one.  */
	  if (TYPE_OBSTACK (oldtype) == TYPE_OBSTACK (newtype))
	    push_obstacks (TYPE_OBSTACK (oldtype), TYPE_OBSTACK (oldtype));
	  else
	    {
	      push_obstacks_nochange ();
	      end_temporary_allocation ();
	    }

          if (TYPE_MODE (oldreturntype) == TYPE_MODE (newreturntype))
            {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree newtype
		= build_function_type (newreturntype,
				       TYPE_ARG_TYPES (TREE_TYPE (olddecl)));
	      
              types_match = comptypes (TREE_TYPE (newdecl), newtype);
	      if (types_match)
		TREE_TYPE (olddecl) = newtype;
	    }
	  /* Accept harmless mismatch in first argument type also.
	     This is for ffs.  */
	  if (TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != 0
	      && TYPE_ARG_TYPES (TREE_TYPE (olddecl)) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (newdecl))) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (olddecl))) != 0
	      && (TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (newdecl))))
		  ==
		  TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (olddecl))))))
	    {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree newtype
		= build_function_type (TREE_TYPE (TREE_TYPE (olddecl)),
				       tree_cons (NULL_TREE, 
						  TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (newdecl))),
						  TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (olddecl)))));
	      
              types_match = comptypes (TREE_TYPE (newdecl), newtype);
	      if (types_match)
		TREE_TYPE (olddecl) = newtype;
	    }

	  pop_obstacks ();
	}
      if (!types_match)
	{
	  /* If types don't match for a built-in, throw away the built-in.  */
	  warning_with_decl (newdecl, "conflicting types for built-in function `%s'");
	  return 0;
	}
    }
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_SOURCE_LINE (olddecl) == 0)
    {
      /* A function declaration for a predeclared function
	 that isn't actually built in.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare it as static, the
	     default definition is overridden.  */
	  return 0;
	}
      else if (!types_match)
	{
	  /* If the types don't match, preserve volatility indication.
	     Later on, we will discard everything else about the
	     default declaration.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	}
    }
  /* Permit char *foo () to match void *foo (...) if not pedantic,
     if one of them came from a system header file.  */
  else if (!types_match
	   && TREE_CODE (olddecl) == FUNCTION_DECL
	   && TREE_CODE (newdecl) == FUNCTION_DECL
	   && TREE_CODE (TREE_TYPE (oldtype)) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (newtype)) == POINTER_TYPE
	   && (DECL_IN_SYSTEM_HEADER (olddecl)
	       || DECL_IN_SYSTEM_HEADER (newdecl))
	   && ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (newtype))) == void_type_node
		&& TYPE_ARG_TYPES (oldtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (newtype))
		&& TREE_TYPE (TREE_TYPE (oldtype)) == char_type_node)
	       ||
	       (TREE_TYPE (TREE_TYPE (newtype)) == char_type_node
		&& TYPE_ARG_TYPES (newtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (oldtype))
		&& TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)))
    {
      if (pedantic)
	pedwarn_with_decl (newdecl, "conflicting types for `%s'");
      /* Make sure we keep void * as ret type, not char *.  */
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)
	TREE_TYPE (newdecl) = newtype = oldtype;

      /* Set DECL_IN_SYSTEM_HEADER, so that if we see another declaration
	 we will come back here again.  */
      DECL_IN_SYSTEM_HEADER (newdecl) = 1;
    }
  else if (!types_match
	   /* Permit char *foo (int, ...); followed by char *foo ();
	      if not pedantic.  */
	   && ! (TREE_CODE (olddecl) == FUNCTION_DECL
		 && ! pedantic
		 /* Return types must still match.  */
		 && comptypes (TREE_TYPE (oldtype),
			       TREE_TYPE (newtype))
		 && TYPE_ARG_TYPES (newtype) == 0))
    {
      error_with_decl (newdecl, "conflicting types for `%s'");
      /* Check for function type mismatch
	 involving an empty arglist vs a nonempty one.  */
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && comptypes (TREE_TYPE (oldtype),
			TREE_TYPE (newtype))
	  && ((TYPE_ARG_TYPES (oldtype) == 0
	       && DECL_INITIAL (olddecl) == 0)
	      ||
	      (TYPE_ARG_TYPES (newtype) == 0
	       && DECL_INITIAL (newdecl) == 0)))
	{
	  /* Classify the problem further.  */
	  register tree t = TYPE_ARG_TYPES (oldtype);
	  if (t == 0)
	    t = TYPE_ARG_TYPES (newtype);
	  for (; t; t = TREE_CHAIN (t))
	    {
	      register tree type = TREE_VALUE (t);

	      if (TREE_CHAIN (t) == 0
		  && TYPE_MAIN_VARIANT (type) != void_type_node)
		{
		  error ("A parameter list with an ellipsis can't match");
		  error ("an empty parameter name list declaration.");
		  break;
		}

	      if (TYPE_MAIN_VARIANT (type) == float_type_node
		  || C_PROMOTING_INTEGER_TYPE_P (type))
		{
		  error ("An argument type that has a default promotion");
		  error ("can't match an empty parameter name list declaration.");
		  break;
		}
	    }
	}
      error_with_decl (olddecl, "previous declaration of `%s'");
    }
  else
    {
      errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  error_with_decl (newdecl, errmsg);
	  error_with_decl (olddecl,
			   ((DECL_INITIAL (olddecl)
			     && current_binding_level == global_binding_level)
			    ? "`%s' previously defined here"
			    : "`%s' previously declared here"));
	}
      else if (TREE_CODE (newdecl) == TYPE_DECL
               && (DECL_IN_SYSTEM_HEADER (olddecl) 
                   || DECL_IN_SYSTEM_HEADER (newdecl)))
	{
	  warning_with_decl (newdecl, "redefinition of `%s'");
	  warning_with_decl 
	    (olddecl,
	     ((DECL_INITIAL (olddecl)
	       && current_binding_level == global_binding_level)
	      ? "`%s' previously defined here"
	      : "`%s' previously declared here"));
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != 0
	       && TYPE_ARG_TYPES (oldtype) == 0
	       && TYPE_ARG_TYPES (newtype) != 0
	       && TYPE_ACTUAL_ARG_TYPES (oldtype) != 0)
	{
	  register tree type, parm;
	  register int nargs;
	  /* Prototype decl follows defn w/o prototype.  */

	  for (parm = TYPE_ACTUAL_ARG_TYPES (oldtype),
	       type = TYPE_ARG_TYPES (newtype),
	       nargs = 1;
	       (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) != void_type_node
		|| TYPE_MAIN_VARIANT (TREE_VALUE (type)) != void_type_node);
	       parm = TREE_CHAIN (parm), type = TREE_CHAIN (type), nargs++)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == void_type_node
		  || TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  errmsg = "prototype for `%s' follows and number of arguments";
		  break;
		}
	      /* Type for passing arg must be consistent
		 with that declared for the arg.  */
	      if (! comptypes (TREE_VALUE (parm), TREE_VALUE (type))
		  /* If -traditional, allow `unsigned int' instead of `int'
		     in the prototype.  */
		  && (! (flag_traditional
			 && TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == integer_type_node
			 && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == unsigned_type_node)))
		{
		  errmsg = "prototype for `%s' follows and argument %d";
		  break;
		}
	    }
	  if (errmsg)
	    {
	      error_with_decl (newdecl, errmsg, nargs);
	      error_with_decl (olddecl,
			       "doesn't match non-prototype definition here");
	    }
	  else
	    {
	      warning_with_decl (newdecl, "prototype for `%s' follows");
	      warning_with_decl (olddecl, "non-prototype definition here");
	    }
	}
      /* Warn about mismatches in various flags.  */
      else
	{
	  /* Warn if function is now inline
	     but was previously declared not inline and has been called.  */
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_INLINE (olddecl) && DECL_INLINE (newdecl)
	      && TREE_USED (olddecl))
	    warning_with_decl (newdecl,
			       "`%s' declared inline after being called");
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_INLINE (olddecl) && DECL_INLINE (newdecl)
	      && DECL_INITIAL (olddecl) != 0)
	    warning_with_decl (newdecl,
			       "`%s' declared inline after its definition");

	  /* If pedantic, warn when static declaration follows a non-static
	     declaration.  Otherwise, do so only for functions.  */
	  if ((pedantic || TREE_CODE (olddecl) == FUNCTION_DECL)
	      && TREE_PUBLIC (olddecl)
	      && !TREE_PUBLIC (newdecl))
	    warning_with_decl (newdecl, "static declaration for `%s' follows non-static");

	  /* Warn when const declaration follows a non-const
	     declaration, but not for functions.  */
	  if (TREE_CODE (olddecl) != FUNCTION_DECL
	      && !TREE_READONLY (olddecl)
	      && TREE_READONLY (newdecl))
	    warning_with_decl (newdecl, "const declaration for `%s' follows non-const");
	  /* These bits are logically part of the type, for variables.
	     But not for functions
	     (where qualifiers are not valid ANSI anyway).  */
	  else if (pedantic && TREE_CODE (olddecl) != FUNCTION_DECL
	      && (TREE_READONLY (newdecl) != TREE_READONLY (olddecl)
		  || TREE_THIS_VOLATILE (newdecl) != TREE_THIS_VOLATILE (olddecl)))
	    pedwarn_with_decl (newdecl, "type qualifiers for `%s' conflict with previous decl");
	}
    }

  /* Optionally warn about more than one declaration for the same name.  */
  if (errmsg == 0 && warn_redundant_decls && DECL_SOURCE_LINE (olddecl) != 0
      /* Dont warn about a function declaration
	 followed by a definition.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL && DECL_INITIAL (newdecl) != 0
	   && DECL_INITIAL (olddecl) == 0)
      /* Don't warn about extern decl followed by (tentative) definition.  */
      && !(DECL_EXTERNAL (olddecl) && ! DECL_EXTERNAL (newdecl)))
    {
      warning_with_decl (newdecl, "redundant redeclaration of `%s' in same scope");
      warning_with_decl (olddecl, "previous declaration of `%s'");
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.

     Past this point, we don't change OLDTYPE and NEWTYPE
     even if we change the types of NEWDECL and OLDDECL.  */

  if (types_match)
    {
      /* Make sure we put the new type in the same obstack as the old ones.
	 If the old types are not both in the same obstack, use the permanent
	 one.  */
      if (TYPE_OBSTACK (oldtype) == TYPE_OBSTACK (newtype))
	push_obstacks (TYPE_OBSTACK (oldtype), TYPE_OBSTACK (oldtype));
      else
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	}
		       
      /* Merge the data types specified in the two decls.  */
      if (TREE_CODE (newdecl) != FUNCTION_DECL || !DECL_BUILT_IN (olddecl))
	TREE_TYPE (newdecl)
	  = TREE_TYPE (olddecl)
	    = common_type (newtype, oldtype);

      /* Lay the type out, unless already done.  */
      if (oldtype != TREE_TYPE (newdecl))
	{
	  if (TREE_TYPE (newdecl) != error_mark_node)
	    layout_type (TREE_TYPE (newdecl));
	  if (TREE_CODE (newdecl) != FUNCTION_DECL
	      && TREE_CODE (newdecl) != TYPE_DECL
	      && TREE_CODE (newdecl) != CONST_DECL)
	    layout_decl (newdecl, 0);
	}
      else
	{
	  /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
	  DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
	  if (TREE_CODE (olddecl) != FUNCTION_DECL)
	    if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	      DECL_ALIGN (newdecl) = DECL_ALIGN (olddecl);
	}

      /* Keep the old rtl since we can safely use it.  */
      DECL_RTL (newdecl) = DECL_RTL (olddecl);

      /* Merge the type qualifiers.  */
      if (DECL_BUILT_IN_NONANSI (olddecl) && TREE_THIS_VOLATILE (olddecl)
	  && !TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (olddecl) = 0;
      if (TREE_READONLY (newdecl))
	TREE_READONLY (olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	{
	  TREE_THIS_VOLATILE (olddecl) = 1;
	  if (TREE_CODE (newdecl) == VAR_DECL)
	    make_var_volatile (newdecl);
	}

      /* Keep source location of definition rather than declaration.
	 Likewise, keep decl at outer scope.  */
      if ((DECL_INITIAL (newdecl) == 0 && DECL_INITIAL (olddecl) != 0)
	  || (DECL_CONTEXT (newdecl) != 0 && DECL_CONTEXT (olddecl) == 0))
	{
	  DECL_SOURCE_LINE (newdecl) = DECL_SOURCE_LINE (olddecl);
	  DECL_SOURCE_FILE (newdecl) = DECL_SOURCE_FILE (olddecl);

	  if (DECL_CONTEXT (olddecl) == 0
	      && TREE_CODE (newdecl) != FUNCTION_DECL)
	    DECL_CONTEXT (newdecl) = 0;
	}

      /* Merge the unused-warning information.  */
      if (DECL_IN_SYSTEM_HEADER (olddecl))
	DECL_IN_SYSTEM_HEADER (newdecl) = 1;
      else if (DECL_IN_SYSTEM_HEADER (newdecl))
	DECL_IN_SYSTEM_HEADER (olddecl) = 1;

      /* Merge the initialization information.  */
      if (DECL_INITIAL (newdecl) == 0)
	DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);

      /* Merge the section attribute.
         We want to issue an error if the sections conflict but that must be
	 done later in decl_attributes since we are called before attributes
	 are assigned.  */
      if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
	DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
	  DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
	}

      pop_obstacks ();
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  DECL_WEAK (newdecl) |= DECL_WEAK (olddecl);	  
  /* For functions, static overrides non-static.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
      /* This is since we don't automatically
	 copy the attributes of NEWDECL into OLDDECL.  */
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
      /* If this clears `static', clear it in the identifier too.  */
      if (! TREE_PUBLIC (olddecl))
	TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
    }
  if (DECL_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);
      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }

  /* If either decl says `inline', this fn is inline,
     unless its definition was passed already.  */
  if (DECL_INLINE (newdecl) && DECL_INITIAL (olddecl) == 0)
    DECL_INLINE (olddecl) = 1;
  DECL_INLINE (newdecl) = DECL_INLINE (olddecl);

  /* Get rid of any built-in function if new arg types don't match it
     or if we have a function definition.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL
      && DECL_BUILT_IN (olddecl)
      && (!types_match || new_is_definition))
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      DECL_BUILT_IN (olddecl) = 0;
    }

  /* If redeclaring a builtin function, and not a definition,
     it stays built in.
     Also preserve various other info from the definition.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL && !new_is_definition)
    {
      if (DECL_BUILT_IN (olddecl))
	{
	  DECL_BUILT_IN (newdecl) = 1;
	  DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	}
      else
	DECL_FRAME_SIZE (newdecl) = DECL_FRAME_SIZE (olddecl);

      DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
      DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
      DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
      DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
    }

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve OLDdECL's DECL_UID.  */
  {
    register unsigned olddecl_uid = DECL_UID (olddecl);

    bcopy ((char *) newdecl + sizeof (struct tree_common),
	   (char *) olddecl + sizeof (struct tree_common),
	   sizeof (struct tree_decl) - sizeof (struct tree_common));
    DECL_UID (olddecl) = olddecl_uid;
  }

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
  register tree name = DECL_NAME (x);
  register struct binding_level *b = current_binding_level;

  DECL_CONTEXT (x) = current_function_decl;
  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if (TREE_CODE (x) == FUNCTION_DECL && DECL_INITIAL (x) == 0
      && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = 0;

  if (warn_nested_externs && DECL_EXTERNAL (x) && b != global_binding_level
      && x != IDENTIFIER_IMPLICIT_DECL (name)
      /* Don't print error messages for __FUNCTION__ and __PRETTY_FUNCTION__ */
      && !DECL_IN_SYSTEM_HEADER (x))
    warning ("nested extern declaration of `%s'", IDENTIFIER_POINTER (name));

  if (name)
    {
      char *file;
      int line;
      int declared_global;

      /* Don't type check externs here when -traditional.  This is so that
	 code with conflicting declarations inside blocks will get warnings
	 not errors.  X11 for instance depends on this.  */
      if (DECL_EXTERNAL (x) && TREE_PUBLIC (x) && ! flag_traditional)
	t = lookup_name_current_level_global (name);
      else
	t = lookup_name_current_level (name);
      if (t != 0 && t == error_mark_node)
	/* error_mark_node is 0 for a while during initialization!  */
	{
	  t = 0;
	  error_with_decl (x, "`%s' used prior to declaration");
	}

      if (t != 0)
	{
	  file = DECL_SOURCE_FILE (t);
	  line = DECL_SOURCE_LINE (t);
	}

      /* duplicate_decls might write to TREE_PUBLIC (x) and DECL_EXTERNAL (x)
	 to make it identical to the initial declaration.  */
      declared_global = TREE_PUBLIC (x) || DECL_EXTERNAL (x);
      if (t != 0 && duplicate_decls (x, t))
	{
	  if (TREE_CODE (t) == PARM_DECL)
	    {
	      /* Don't allow more than one "real" duplicate
		 of a forward parm decl.  */
	      TREE_ASM_WRITTEN (t) = TREE_ASM_WRITTEN (x);
	      return t;
	    }
	  /* If this decl is `static' and an implicit decl was seen previously,
	     warn.  But don't complain if -traditional,
	     since traditional compilers don't complain.  */
	  if (!flag_traditional && TREE_PUBLIC (name)

	      /* should this be '&& ! declared_global' ?  */
	      && ! TREE_PUBLIC (x) && ! DECL_EXTERNAL (x)

	      /* We used to warn also for explicit extern followed by static,
		 but sometimes you need to do it that way.  */
	      && IDENTIFIER_IMPLICIT_DECL (name) != 0)
	    {
	      pedwarn ("`%s' was declared implicitly `extern' and later `static'",
		       IDENTIFIER_POINTER (name));
	      pedwarn_with_file_and_line (file, line,
					  "previous declaration of `%s'",
					  IDENTIFIER_POINTER (name));
	    }

	  /* If this is a global decl, and there exists a conflicting local
	     decl in a parent block, then we can't return as yet, because we
	     need to register this decl in the current binding block.  */
	  /* A test for TREE_PUBLIC (x) will fail for variables that have
	     been declared static first, and extern now.  */
	  if (! declared_global || lookup_name (name) == t)
	    return t;
	}

      /* If we are processing a typedef statement, generate a whole new
	 ..._TYPE node (which will be just an variant of the existing
	 ..._TYPE node with identical properties) and then install the
	 TYPE_DECL node generated to represent the typedef name as the
	 TYPE_NAME of this brand new (duplicate) ..._TYPE node.

	 The whole point here is to end up with a situation where each
	 and every ..._TYPE node the compiler creates will be uniquely
	 associated with AT MOST one node representing a typedef name.
	 This way, even though the compiler substitutes corresponding
	 ..._TYPE nodes for TYPE_DECL (i.e. "typedef name") nodes very
	 early on, later parts of the compiler can always do the reverse
	 translation and get back the corresponding typedef name.  For
	 example, given:

		typedef struct S MY_TYPE;
		MY_TYPE object;

	 Later parts of the compiler might only know that `object' was of
	 type `struct S' if if were not for code just below.  With this
	 code however, later parts of the compiler see something like:

		struct S' == struct S
		typedef struct S' MY_TYPE;
		struct S' object;

	 And they can then deduce (from the node for type struct S') that
	 the original object declaration was:

		MY_TYPE object;

	 Being able to do this is important for proper support of protoize,
	 and also for generating precise symbolic debugging information
	 which takes full account of the programmer's (typedef) vocabulary.

         Obviously, we don't want to generate a duplicate ..._TYPE node if
	 the TYPE_DECL node that we are now processing really represents a
	 standard built-in type.

         Since all standard types are effectively declared at line zero
         in the source file, we can easily check to see if we are working
         on a standard type by checking the current value of lineno.  */

      if (TREE_CODE (x) == TYPE_DECL)
        {
          if (DECL_SOURCE_LINE (x) == 0)
            {
	      if (TYPE_NAME (TREE_TYPE (x)) == 0)
	        TYPE_NAME (TREE_TYPE (x)) = x;
            }
          else if (TREE_TYPE (x) != error_mark_node)
            {
              tree tt = TREE_TYPE (x);

              tt = build_type_copy (tt);
              TYPE_NAME (tt) = x;
              TREE_TYPE (x) = tt;
            }
        }

      /* Multiple external decls of the same identifier ought to match.
	 Check against both global declarations (when traditional) and out of
	 scope (limbo) block level declarations.

	 We get warnings about inline functions where they are defined.
	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x) && ! DECL_INLINE (x))
	{
	  tree decl;

	  if (flag_traditional && IDENTIFIER_GLOBAL_VALUE (name) != 0
	      && (DECL_EXTERNAL (IDENTIFIER_GLOBAL_VALUE (name))
		  || TREE_PUBLIC (IDENTIFIER_GLOBAL_VALUE (name))))
	    decl = IDENTIFIER_GLOBAL_VALUE (name);
	  else if (IDENTIFIER_LIMBO_VALUE (name) != 0)
	    /* Decls in limbo are always extern, so no need to check that.  */
	    decl = IDENTIFIER_LIMBO_VALUE (name);
	  else
	    decl = 0;

	  if (decl && ! comptypes (TREE_TYPE (x), TREE_TYPE (decl))
	      /* If old decl is built-in, we already warned if we should.  */
	      && !DECL_BUILT_IN (decl))
	    {
	      pedwarn_with_decl (x,
				 "type mismatch with previous external decl");
	      pedwarn_with_decl (decl, "previous external decl of `%s'");
	    }
	}

      /* If a function has had an implicit declaration, and then is defined,
	 make sure they are compatible.  */

      if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	  && IDENTIFIER_GLOBAL_VALUE (name) == 0
	  && TREE_CODE (x) == FUNCTION_DECL
	  && ! comptypes (TREE_TYPE (x),
			  TREE_TYPE (IDENTIFIER_IMPLICIT_DECL (name))))
	{
	  warning_with_decl (x, "type mismatch with previous implicit declaration");
	  warning_with_decl (IDENTIFIER_IMPLICIT_DECL (name),
			     "previous implicit declaration of `%s'");
	}

      /* In PCC-compatibility mode, extern decls of vars with no current decl
	 take effect at top level no matter where they are.  */
      if (flag_traditional && DECL_EXTERNAL (x)
	  && lookup_name (name) == 0)
	{
	  tree type = TREE_TYPE (x);

	  /* But don't do this if the type contains temporary nodes.  */
	  while (type)
	    {
	      if (type == error_mark_node)
		break;
	      if (! TREE_PERMANENT (type))
		{
		  warning_with_decl (x, "type of external `%s' is not global");
		  /* By exiting the loop early, we leave TYPE nonzero,
		     and thus prevent globalization of the decl.  */
		  break;
		}
	      else if (TREE_CODE (type) == FUNCTION_TYPE
		       && TYPE_ARG_TYPES (type) != 0)
		/* The types might not be truly local,
		   but the list of arg types certainly is temporary.
		   Since prototypes are nontraditional,
		   ok not to do the traditional thing.  */
		break;
	      type = TREE_TYPE (type);
	    }

	  if (type == 0)
	    b = global_binding_level;
	}

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (b == global_binding_level)
	{
	  /* Install a global value.  */
	  
	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == 0 && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  IDENTIFIER_GLOBAL_VALUE (name) = x;

	  /* We no longer care about any previous block level declarations.  */
	  IDENTIFIER_LIMBO_VALUE (name) = 0;

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1, TREE_USED (name) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (x)))
			== integer_type_node)))
	    pedwarn ("`%s' was previously implicitly declared to return `int'",
		     IDENTIFIER_POINTER (name));

	  /* If this decl is `static' and an `extern' was seen previously,
	     that is erroneous.  */
	  if (TREE_PUBLIC (name)
	      && ! TREE_PUBLIC (x) && ! DECL_EXTERNAL (x))
	    {
	      /* Okay to redeclare an ANSI built-in as static.  */
	      if (t != 0 && DECL_BUILT_IN (t))
		;
	      /* Okay to declare a non-ANSI built-in as anything.  */
	      else if (t != 0 && DECL_BUILT_IN_NONANSI (t))
		;
	      else if (IDENTIFIER_IMPLICIT_DECL (name))
		pedwarn ("`%s' was declared implicitly `extern' and later `static'",
			 IDENTIFIER_POINTER (name));
	      else
		pedwarn ("`%s' was declared `extern' and later `static'",
			 IDENTIFIER_POINTER (name));
	    }
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_LOCAL_VALUE (name);
	  tree oldglobal = IDENTIFIER_GLOBAL_VALUE (name);
	  IDENTIFIER_LOCAL_VALUE (name) = x;

	  /* If this is an extern function declaration, see if we
	     have a global definition or declaration for the function.  */
	  if (oldlocal == 0
	      && DECL_EXTERNAL (x) && !DECL_INLINE (x)
	      && oldglobal != 0
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL)
	    {
	      /* We have one.  Their types must agree.  */
	      if (! comptypes (TREE_TYPE (x),
			       TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name))))
		pedwarn_with_decl (x, "extern declaration of `%s' doesn't match global one");
	      else
		{
		  /* Inner extern decl is inline if global one is.
		     Copy enough to really inline it.  */
		  if (DECL_INLINE (oldglobal))
		    {
		      DECL_INLINE (x) = DECL_INLINE (oldglobal);
		      DECL_INITIAL (x) = (current_function_decl == oldglobal
					  ? 0 : DECL_INITIAL (oldglobal));
		      DECL_SAVED_INSNS (x) = DECL_SAVED_INSNS (oldglobal);
		      DECL_FRAME_SIZE (x) = DECL_FRAME_SIZE (oldglobal);
		      DECL_ARGUMENTS (x) = DECL_ARGUMENTS (oldglobal);
		      DECL_RESULT (x) = DECL_RESULT (oldglobal);
		      TREE_ASM_WRITTEN (x) = TREE_ASM_WRITTEN (oldglobal);
		      DECL_ABSTRACT_ORIGIN (x) = oldglobal;
		    }
		  /* Inner extern decl is built-in if global one is.  */
		  if (DECL_BUILT_IN (oldglobal))
		    {
		      DECL_BUILT_IN (x) = DECL_BUILT_IN (oldglobal);
		      DECL_FUNCTION_CODE (x) = DECL_FUNCTION_CODE (oldglobal);
		    }
		  /* Keep the arg types from a file-scope fcn defn.  */
		  if (TYPE_ARG_TYPES (TREE_TYPE (oldglobal)) != 0
		      && DECL_INITIAL (oldglobal)
		      && TYPE_ARG_TYPES (TREE_TYPE (x)) == 0)
		    TREE_TYPE (x) = TREE_TYPE (oldglobal);
		}
	    }

#if 0 /* This case is probably sometimes the right thing to do.  */
	  /* If we have a local external declaration,
	     then any file-scope declaration should not
	     have been static.  */
	  if (oldlocal == 0 && oldglobal != 0
	      && !TREE_PUBLIC (oldglobal)
	      && DECL_EXTERNAL (x) && TREE_PUBLIC (x))
	    warning ("`%s' locally external but globally static",
		     IDENTIFIER_POINTER (name));
#endif

	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == 0
	      && oldglobal == 0
	      && DECL_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    {
	      TREE_PUBLIC (name) = 1;

	      /* Save this decl, so that we can do type checking against
		 other decls after it falls out of scope.

		 Only save it once.  This prevents temporary decls created in
		 expand_inline_function from being used here, since this
		 will have been set when the inline function was parsed.
		 It also helps give slightly better warnings.  */
	      if (IDENTIFIER_LIMBO_VALUE (name) == 0)
		IDENTIFIER_LIMBO_VALUE (name) = x;
	    }

	  /* Warn if shadowing an argument at the top level of the body.  */
	  if (oldlocal != 0 && !DECL_EXTERNAL (x)
	      /* This warning doesn't apply to the parms of a nested fcn.  */
	      && ! current_binding_level->parm_flag
	      /* Check that this is one level down from the parms.  */
	      && current_binding_level->level_chain->parm_flag
	      /* Check that the decl being shadowed
		 comes from the parm level, one level up.  */
	      && chain_member (oldlocal, current_binding_level->level_chain->names))
	    {
	      if (TREE_CODE (oldlocal) == PARM_DECL)
		pedwarn ("declaration of `%s' shadows a parameter",
			 IDENTIFIER_POINTER (name));
	      else
		pedwarn ("declaration of `%s' shadows a symbol from the parameter list",
			 IDENTIFIER_POINTER (name));
	    }

	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !DECL_EXTERNAL (x)
		   /* No shadow warnings for internally generated vars.  */
		   && DECL_SOURCE_LINE (x) != 0
		   /* No shadow warnings for vars made for inlining.  */
		   && ! DECL_FROM_INLINE (x))
	    {
	      char *warnstring = 0;

	      if (TREE_CODE (x) == PARM_DECL
		  && current_binding_level->level_chain->parm_flag)
		/* Don't warn about the parm names in function declarator
		   within a function declarator.
		   It would be nice to avoid warning in any function
		   declarator in a declaration, as opposed to a definition,
		   but there is no way to tell it's not a definition.  */
		;
	      else if (oldlocal != 0 && TREE_CODE (oldlocal) == PARM_DECL)
		warnstring = "declaration of `%s' shadows a parameter";
	      else if (oldlocal != 0)
		warnstring = "declaration of `%s' shadows previous local";
	      else if (IDENTIFIER_GLOBAL_VALUE (name) != 0
		       && IDENTIFIER_GLOBAL_VALUE (name) != error_mark_node)
		warnstring = "declaration of `%s' shadows global declaration";

	      if (warnstring)
		warning (warnstring, IDENTIFIER_POINTER (name));
	    }

	  /* If storing a local value, there may already be one (inherited).
	     If so, record it for restoration when this binding level ends.  */
	  if (oldlocal != 0)
	    b->shadowed = tree_cons (name, oldlocal, b->shadowed);
	}

      /* Keep count of variables in this level with incomplete type.  */
      if (TYPE_SIZE (TREE_TYPE (x)) == 0)
	++b->n_incomplete;
    }

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL, if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  register tree t;
  register struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}

/* Generate an implicit declaration for identifier FUNCTIONID
   as a function of type int ().  Print a warning if appropriate.  */

tree
implicitly_declare (functionid)
     tree functionid;
{
  register tree decl;
  int traditional_warning = 0;
  /* Only one "implicit declaration" warning per identifier.  */
  int implicit_warning;

  /* Save the decl permanently so we can warn if definition follows.  */
  push_obstacks_nochange ();
  end_temporary_allocation ();

  /* We used to reuse an old implicit decl here,
     but this loses with inline functions because it can clobber
     the saved decl chains.  */
/*  if (IDENTIFIER_IMPLICIT_DECL (functionid) != 0)
    decl = IDENTIFIER_IMPLICIT_DECL (functionid);
  else  */
    decl = build_decl (FUNCTION_DECL, functionid, default_function_type);

  /* Warn of implicit decl following explicit local extern decl.
     This is probably a program designed for traditional C.  */
  if (TREE_PUBLIC (functionid) && IDENTIFIER_GLOBAL_VALUE (functionid) == 0)
    traditional_warning = 1;

  /* Warn once of an implicit declaration.  */
  implicit_warning = (IDENTIFIER_IMPLICIT_DECL (functionid) == 0);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* Record that we have an implicit decl and this is it.  */
  IDENTIFIER_IMPLICIT_DECL (functionid) = decl;

  /* ANSI standard says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.
     If flag_traditional is set, pushdecl does it top-level.  */
  pushdecl (decl);

  /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
  maybe_objc_check_decl (decl);

  rest_of_decl_compilation (decl, NULL_PTR, 0, 0);

  if (warn_implicit && implicit_warning)
    warning ("implicit declaration of function `%s'",
	     IDENTIFIER_POINTER (functionid));
  else if (warn_traditional && traditional_warning)
    warning ("function `%s' was previously declared within a block",
	     IDENTIFIER_POINTER (functionid));

  /* Write a record describing this implicit function declaration to the
     prototypes file (if requested).  */

  gen_aux_info_record (decl, 0, 1, 0);

  pop_obstacks ();

  return decl;
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return an error message format string with a %s
   where the identifier should go.  */

static char *
redeclaration_error_message (newdecl, olddecl)
     tree newdecl, olddecl;
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      if (flag_traditional && TREE_TYPE (newdecl) == TREE_TYPE (olddecl))
	return 0;
      /* pushdecl creates distinct types for TYPE_DECLs by calling
	 build_type_copy, so the above comparison generally fails.  We do
	 another test against the TYPE_MAIN_VARIANT of the olddecl, which
	 is equivalent to what this code used to do before the build_type_copy
	 call.  The variant type distinction should not matter for traditional
	 code, because it doesn't have type qualifiers.  */
      if (flag_traditional 
	  && TYPE_MAIN_VARIANT (TREE_TYPE (olddecl)) == TREE_TYPE (newdecl))
	return 0;
      if (DECL_IN_SYSTEM_HEADER (olddecl) || DECL_IN_SYSTEM_HEADER (newdecl))
	return 0;
      return "redefinition of `%s'";
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Declarations of functions can insist on internal linkage
	 but they can't be inconsistent with internal linkage,
	 so there can be no error on that account.
	 However defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0
	  /* However, defining once as extern inline and a second
	     time in another way is ok.  */
	  && !(DECL_INLINE (olddecl) && DECL_EXTERNAL (olddecl)
	       && !(DECL_INLINE (newdecl) && DECL_EXTERNAL (newdecl))))
	return "redefinition of `%s'";
      return 0;
    }
  else if (current_binding_level == global_binding_level)
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (DECL_EXTERNAL (newdecl) || DECL_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0)
	return "redefinition of `%s'";
      /* Now we have two tentative defs, or one tentative and one real def.  */
      /* Insist that the linkage match.  */
      if (TREE_PUBLIC (olddecl) != TREE_PUBLIC (newdecl))
	return "conflicting declarations of `%s'";
      return 0;
    }
  else if (current_binding_level->parm_flag
	   && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl))
    return 0;
  else
    {
      /* Newdecl has block scope.  If olddecl has block scope also, then
	 reject two definitions, and reject a definition together with an
	 external reference.  Otherwise, it is OK, because newdecl must
	 be an extern reference to olddecl.  */
      if (!(DECL_EXTERNAL (newdecl) && DECL_EXTERNAL (olddecl))
	  && DECL_CONTEXT (newdecl) == DECL_CONTEXT (olddecl))
	return "redeclaration of `%s'";
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

  if (current_function_decl == 0)
    {
      error ("label %s referenced outside of any function",
	     IDENTIFIER_POINTER (id));
      return 0;
    }

  /* Use a label already defined or ref'd with this name.  */
  if (decl != 0)
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

  IDENTIFIER_LABEL_VALUE (id) = decl;

  named_labels = tree_cons (NULL_TREE, decl, named_labels);

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

  if (decl != 0)
    {
      register tree dup;

      /* Check to make sure that the label hasn't already been declared
	 at this label scope */
      for (dup = named_labels; dup; dup = TREE_CHAIN (dup))
	if (TREE_VALUE (dup) == decl)
	  {
	    error ("duplicate label declaration `%s'", 
		   IDENTIFIER_POINTER (name));
	    error_with_decl (TREE_VALUE (dup),
			     "this is a previous declaration");
	    /* Just use the previous declaration.  */
	    return lookup_label (name);
	  }

      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = decl = 0;
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
  tree decl = lookup_label (name);

  /* If label with this name is known from an outer context, shadow it.  */
  if (decl != 0 && DECL_CONTEXT (decl) != current_function_decl)
    {
      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = 0;
      decl = lookup_label (name);
    }

  if (DECL_INITIAL (decl) != 0)
    {
      error ("duplicate label `%s'", IDENTIFIER_POINTER (name));
      return 0;
    }
  else
    {
      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;
      /* Say where in the source.  */
      DECL_SOURCE_FILE (decl) = filename;
      DECL_SOURCE_LINE (decl) = line;
      return decl;
    }
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

static void
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
   CODE says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, an error is reported.  */

static tree
lookup_tag (code, name, binding_level, thislevel_only)
     enum tree_code code;
     struct binding_level *binding_level;
     tree name;
     int thislevel_only;
{
  register struct binding_level *level;

  for (level = binding_level; level; level = level->level_chain)
    {
      register tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_PURPOSE (tail) == name)
	    {
	      if (TREE_CODE (TREE_VALUE (tail)) != code)
		{
		  /* Definition isn't the kind we were looking for.  */
		  pending_invalid_xref = name;
		  pending_invalid_xref_file = input_filename;
		  pending_invalid_xref_line = lineno;
		}
	      return TREE_VALUE (tail);
	    }
	}
      if (thislevel_only && ! level->tag_transparent)
	return NULL_TREE;
    }
  return NULL_TREE;
}

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */

void
pending_xref_error ()
{
  if (pending_invalid_xref != 0)
    error_with_file_and_line (pending_invalid_xref_file,
			      pending_invalid_xref_line,
			      "`%s' defined as wrong kind of tag",
			      IDENTIFIER_POINTER (pending_invalid_xref));
  pending_invalid_xref = 0;
}

/* Given a type, find the tag that was defined for it and return the tag name.
   Otherwise return 0.  */

static tree
lookup_tag_reverse (type)
     tree type;
{
  register struct binding_level *level;

  for (level = current_binding_level; level; level = level->level_chain)
    {
      register tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_VALUE (tail) == type)
	    return TREE_PURPOSE (tail);
	}
    }
  return NULL_TREE;
}

/* Look up NAME in the current binding level and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (name)
     tree name;
{
  register tree val;
  if (current_binding_level != global_binding_level
      && IDENTIFIER_LOCAL_VALUE (name))
    val = IDENTIFIER_LOCAL_VALUE (name);
  else
    val = IDENTIFIER_GLOBAL_VALUE (name);
  return val;
}

/* Similar to `lookup_name' but look only at current binding level.  */

tree
lookup_name_current_level (name)
     tree name;
{
  register tree t;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) == 0)
    return 0;

  for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
    if (DECL_NAME (t) == name)
      break;

  return t;
}

/* Similar to `lookup_name_current_level' but also look at the global binding
   level.  */

tree
lookup_name_current_level_global (name)
     tree name;
{
  register tree t = 0;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) != 0)
    for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
      if (DECL_NAME (t) == name)
	break;

  if (t == 0)
    t = IDENTIFIER_GLOBAL_VALUE (name);

  return t;
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *)0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

void
init_decl_processing ()
{
  register tree endlink;
  /* Either char* or void*.  */
  tree traditional_ptr_type_node;
  /* Data types of memcpy and strlen.  */
  tree memcpy_ftype, strlen_ftype;
  tree void_ftype_any;
  int wchar_type_size;
  tree temp;
  tree array_domain_type;

  current_function_decl = NULL;
  named_labels = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;
  pushlevel (0);	/* make the binding_level structure for global names */
  global_binding_level = current_binding_level;

  /* Define `int' and `char' first so that dbx will output them first.  */

  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_INT],
			integer_type_node));

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */

  char_type_node
    = (flag_signed_char
       ? make_signed_type (CHAR_TYPE_SIZE)
       : make_unsigned_type (CHAR_TYPE_SIZE));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
			char_type_node));

  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long int"),
			long_integer_type_node));

  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			unsigned_type_node));

  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long unsigned int"),
			long_unsigned_type_node));

  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long int"),
			long_long_integer_type_node));

  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long unsigned int"),
			long_long_unsigned_type_node));

  /* `unsigned long' is the standard type for sizeof.
     Traditionally, use a signed type.
     Note that stddef.h uses `unsigned long',
     and this must agree, even of long and int are the same size.  */
  sizetype
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (SIZE_TYPE)));
  if (flag_traditional && TREE_UNSIGNED (sizetype))
    sizetype = signed_type (sizetype);

  ptrdiff_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (PTRDIFF_TYPE)));

  TREE_TYPE (TYPE_SIZE (integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (char_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_unsigned_type_node)) = sizetype;

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short int"),
			short_integer_type_node));

  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short unsigned int"),
			short_unsigned_type_node));

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("signed char"),
			signed_char_type_node));

  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			unsigned_char_type_node));

  intQI_type_node = make_signed_type (GET_MODE_BITSIZE (QImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intQI_type_node));

  intHI_type_node = make_signed_type (GET_MODE_BITSIZE (HImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intHI_type_node));

  intSI_type_node = make_signed_type (GET_MODE_BITSIZE (SImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intSI_type_node));

  intDI_type_node = make_signed_type (GET_MODE_BITSIZE (DImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intDI_type_node));

  unsigned_intQI_type_node = make_unsigned_type (GET_MODE_BITSIZE (QImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intQI_type_node));

  unsigned_intHI_type_node = make_unsigned_type (GET_MODE_BITSIZE (HImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intHI_type_node));

  unsigned_intSI_type_node = make_unsigned_type (GET_MODE_BITSIZE (SImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intSI_type_node));

  unsigned_intDI_type_node = make_unsigned_type (GET_MODE_BITSIZE (DImode));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intDI_type_node));

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_FLOAT],
			float_type_node));
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  if (flag_short_double)
    TYPE_PRECISION (double_type_node) = FLOAT_TYPE_SIZE;
  else
    TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_DOUBLE],
			double_type_node));
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long double"),
			long_double_type_node));
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

  wchar_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (WCHAR_TYPE)));
  wchar_type_size = TYPE_PRECISION (wchar_type_node);
  signed_wchar_type_node = signed_type (wchar_type_node);
  unsigned_wchar_type_node = unsigned_type (wchar_type_node);

  integer_zero_node = build_int_2 (0, 0);
  TREE_TYPE (integer_zero_node) = integer_type_node;
  integer_one_node = build_int_2 (1, 0);
  TREE_TYPE (integer_one_node) = integer_type_node;

  boolean_type_node = integer_type_node;
  boolean_true_node = integer_one_node;
  boolean_false_node = integer_zero_node;

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  void_type_node = make_node (VOID_TYPE);
  pushdecl (build_decl (TYPE_DECL,
			ridpointers[(int) RID_VOID], void_type_node));
  layout_type (void_type_node);	/* Uses integer_zero_node */
  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));

  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_type_variant (char_type_node, 1, 0));

  /* Make a type to be the domain of a few array types
     whose domains don't really matter.
     200 is small enough that it always fits in size_t
     and large enough that it can hold most function names for the
     initializations of __FUNCTION__ and __PRETTY_FUNCTION__.  */
  array_domain_type = build_index_type (build_int_2 (200, 0));

  /* make a type for arrays of characters.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, array_domain_type);
  /* Likewise for arrays of ints.  */
  int_array_type_node
    = build_array_type (integer_type_node, array_domain_type);
  /* This is for wide string constants.  */
  wchar_array_type_node
    = build_array_type (wchar_type_node, array_domain_type);

  default_function_type
    = build_function_type (integer_type_node, NULL_TREE);

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));

  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  void_ftype_any
    = build_function_type (void_type_node, NULL_TREE);

  float_ftype_float
    = build_function_type (float_type_node,
			   tree_cons (NULL_TREE, float_type_node, endlink));

  double_ftype_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node, endlink));

  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));

  double_ftype_double_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node,
				      tree_cons (NULL_TREE,
						 double_type_node, endlink)));

  int_ftype_int
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node, endlink));

  long_ftype_long
    = build_function_type (long_integer_type_node,
			   tree_cons (NULL_TREE,
				      long_integer_type_node, endlink));

  void_ftype_ptr_ptr_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, ptr_type_node,
						 tree_cons (NULL_TREE,
							    integer_type_node,
							    endlink))));

  int_ftype_cptr_cptr_sizet
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  void_ftype_ptr_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    integer_type_node,
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
    = build_function_type (flag_traditional ? integer_type_node : sizetype,
			   tree_cons (NULL_TREE, const_string_type_node,
				      endlink));

  traditional_ptr_type_node
    = (flag_traditional ? string_type_node : ptr_type_node);

  memcpy_ftype	/* memcpy prototype */
    = build_function_type (traditional_ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  builtin_function ("__builtin_constant_p", default_function_type,
		    BUILT_IN_CONSTANT_P, NULL_PTR);

  builtin_function ("__builtin_return_address",
		    build_function_type (ptr_type_node, 
					 tree_cons (NULL_TREE,
						    unsigned_type_node,
						    endlink)),
		    BUILT_IN_RETURN_ADDRESS, NULL_PTR);

  builtin_function ("__builtin_frame_address",
		    build_function_type (ptr_type_node, 
					 tree_cons (NULL_TREE,
						    unsigned_type_node,
						    endlink)),
		    BUILT_IN_FRAME_ADDRESS, NULL_PTR);

  builtin_function ("__builtin_alloca",
		    build_function_type (ptr_type_node,
					 tree_cons (NULL_TREE,
						    sizetype,
						    endlink)),
		    BUILT_IN_ALLOCA, "alloca");
  builtin_function ("__builtin_ffs", int_ftype_int, BUILT_IN_FFS, NULL_PTR);
  /* Define alloca, ffs as builtins.
     Declare _exit just to mark it as volatile.  */
  if (! flag_no_builtin && !flag_no_nonansi_builtin)
    {
      temp = builtin_function ("alloca",
			       build_function_type (ptr_type_node,
						    tree_cons (NULL_TREE,
							       sizetype,
							       endlink)),
			       BUILT_IN_ALLOCA, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("ffs", int_ftype_int, BUILT_IN_FFS, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("_exit", void_ftype_any, NOT_BUILT_IN,
			       NULL_PTR);
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
  builtin_function ("__builtin_labs", long_ftype_long, BUILT_IN_LABS,
		    NULL_PTR);
  builtin_function ("__builtin_saveregs",
		    build_function_type (ptr_type_node, NULL_TREE),
		    BUILT_IN_SAVEREGS, NULL_PTR);
/* EXPAND_BUILTIN_VARARGS is obsolete.  */
#if 0
  builtin_function ("__builtin_varargs",
		    build_function_type (ptr_type_node,
					 tree_cons (NULL_TREE,
						    integer_type_node,
						    endlink)),
		    BUILT_IN_VARARGS, NULL_PTR);
#endif
  builtin_function ("__builtin_classify_type", default_function_type,
		    BUILT_IN_CLASSIFY_TYPE, NULL_PTR);
  builtin_function ("__builtin_next_arg",
		    build_function_type (ptr_type_node, NULL_TREE),
		    BUILT_IN_NEXT_ARG, NULL_PTR);
  builtin_function ("__builtin_args_info",
		    build_function_type (integer_type_node,
					 tree_cons (NULL_TREE,
						    integer_type_node,
						    endlink)),
		    BUILT_IN_ARGS_INFO, NULL_PTR);

  /* Untyped call and return.  */
  builtin_function ("__builtin_apply_args",
		    build_function_type (ptr_type_node, NULL_TREE),
		    BUILT_IN_APPLY_ARGS, NULL_PTR);

  temp = tree_cons (NULL_TREE,
		    build_pointer_type (build_function_type (void_type_node,
							     NULL_TREE)),
		    tree_cons (NULL_TREE,
			       ptr_type_node,
			       tree_cons (NULL_TREE,
					  sizetype,
					  endlink)));
  builtin_function ("__builtin_apply",
		    build_function_type (ptr_type_node, temp),
		    BUILT_IN_APPLY, NULL_PTR);
  builtin_function ("__builtin_return",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE,
						    ptr_type_node,
						    endlink)),
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
		    BUILT_IN_FSQRT, "sqrt");
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

  /* In an ANSI C program, it is okay to supply built-in meanings
     for these functions, since applications cannot validly use them
     with any other meaning.
     However, honor the -fno-builtin option.  */
  if (!flag_no_builtin)
    {
      builtin_function ("abs", int_ftype_int, BUILT_IN_ABS, NULL_PTR);
      builtin_function ("fabsf", float_ftype_float, BUILT_IN_FABS, NULL_PTR);
      builtin_function ("fabs", double_ftype_double, BUILT_IN_FABS, NULL_PTR);
      builtin_function ("fabsl", ldouble_ftype_ldouble, BUILT_IN_FABS,
			NULL_PTR);
      builtin_function ("labs", long_ftype_long, BUILT_IN_LABS, NULL_PTR);
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
      /* Don't specify the argument types, to avoid errors
	 from certain code which isn't valid in ANSI but which exists.  */
      temp = builtin_function ("abort", void_ftype_any, NOT_BUILT_IN,
			       NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
      temp = builtin_function ("exit", void_ftype_any, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
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

  pedantic_lvalues = pedantic;

  /* Create the global bindings for __FUNCTION__ and __PRETTY_FUNCTION__.  */
  declare_function_name ();

  start_identifier_warnings ();

  /* Prepare to check format strings against argument lists.  */
  init_function_format_info ();

  init_iterators ();

  incomplete_decl_finalize_hook = finish_incomplete_decl;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (name, type, function_code, library_name)
     char *name;
     tree type;
     enum built_in_function function_code;
     char *library_name;
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  /* If -traditional, permit redefining a builtin function any way you like.
     (Though really, if the program redefines these functions,
     it probably won't work right unless compiled with -fno-builtin.)  */
  if (flag_traditional && name[0] != '_')
    DECL_BUILT_IN_NONANSI (decl) = 1;
  if (library_name)
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_decl_rtl (decl, NULL_PTR, 1);
  pushdecl (decl);
  if (function_code != NOT_BUILT_IN)
    {
      DECL_BUILT_IN (decl) = 1;
      DECL_FUNCTION_CODE (decl) = function_code;
    }
  /* Warn if a function in the namespace for users
     is used without an occasion to consider it declared.  */
  if (name[0] != '_' || name[1] != '_')
    C_DECL_ANTICIPATED (decl) = 1;

  return decl;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.  */

void
shadow_tag (declspecs)
     tree declspecs;
{
  shadow_tag_warned (declspecs, 0);
}

void
shadow_tag_warned (declspecs, warned)
     tree declspecs;
     int warned;
     /* 1 => we have done a pedwarn.  2 => we have done a warning, but
	no pedwarn.  */
{
  int found_tag = 0;
  register tree link;

  pending_invalid_xref = 0;

  for (link = declspecs; link; link = TREE_CHAIN (link))
    {
      register tree value = TREE_VALUE (link);
      register enum tree_code code = TREE_CODE (value);

      if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
	/* Used to test also that TYPE_SIZE (value) != 0.
	   That caused warning for `struct foo;' at top level in the file.  */
	{
	  register tree name = lookup_tag_reverse (value);
	  register tree t;

	  found_tag++;

	  if (name == 0)
	    {
	      if (warned != 1 && code != ENUMERAL_TYPE)
		/* Empty unnamed enum OK */
		{
		  pedwarn ("unnamed struct/union that defines no instances");
		  warned = 1;
		}
	    }
	  else
	    {
	      t = lookup_tag (code, name, current_binding_level, 1);

	      if (t == 0)
		{
		  t = make_node (code);
		  pushtag (name, t);
		}
	    }
	}
      else
	{
	  if (!warned && ! in_system_header)
	    {
	      warning ("useless keyword or type name in empty declaration");
	      warned = 2;
	    }
	}
    }

  if (found_tag > 1)
    error ("two types specified in one empty declaration");

  if (warned != 1)
    {
      if (found_tag == 0)
	pedwarn ("empty declaration");
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
			 TYPENAME, 0);
}

/* Return a PARM_DECL node for a given pair of specs and declarator.  */

tree
groktypename_in_parm_context (typename)
     tree typename;
{
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  return grokdeclarator (TREE_VALUE (typename),
			 TREE_PURPOSE (typename),
			 PARM, 0);
}

/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `finish_decl'.

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
  register tree decl = grokdeclarator (declarator, declspecs,
				       NORMAL, initialized);
  register tree tem;
  int init_written = initialized;

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	/* typedef foo = bar  means give foo the same type as bar.
	   We haven't parsed bar yet, so `finish_decl' will fix that up.
	   Any other case of an initialization in a TYPE_DECL is an error.  */
	if (pedantic || list_length (declspecs) > 1)
	  {
	    error ("typedef `%s' is initialized",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	break;

      case FUNCTION_DECL:
	error ("function `%s' is initialized like a variable",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      case PARM_DECL:
	/* DECL_INITIAL in a PARM_DECL is really DECL_ARG_TYPE.  */
	error ("parameter `%s' is initialized",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      default:
	/* Don't allow initializations for incomplete types
	   except for arrays which might be completed by the initialization.  */
	if (TYPE_SIZE (TREE_TYPE (decl)) != 0)
	  {
	    /* A complete type is ok if size is fixed.  */

	    if (TREE_CODE (TYPE_SIZE (TREE_TYPE (decl))) != INTEGER_CST
		|| C_DECL_VARIABLE_SIZE (decl))
	      {
		error ("variable-sized object may not be initialized");
		initialized = 0;
	      }
	  }
	else if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	  {
	    error ("variable `%s' has initializer but incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	else if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl))) == 0)
	  {
	    error ("elements of array `%s' have incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
      }

  if (initialized)
    {
#if 0  /* Seems redundant with grokdeclarator.  */
      if (current_binding_level != global_binding_level
	  && DECL_EXTERNAL (decl)
	  && TREE_CODE (decl) != FUNCTION_DECL)
	warning ("declaration of `%s' has `extern' and is initialized",
		 IDENTIFIER_POINTER (DECL_NAME (decl)));
#endif
      DECL_EXTERNAL (decl) = 0;
      if (current_binding_level == global_binding_level)
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* If this is a function declaration, write a record describing it to the
     prototypes file (if requested).  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    gen_aux_info_record (decl, 0, 0, TYPE_ARG_TYPES (TREE_TYPE (decl)) != 0);

  /* For C and Objective-C, we by default put things in .common when
     possible.  */
  DECL_COMMON (decl) = 1;

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  decl_attributes (decl, attributes, prefix_attributes);

  /* Add this decl to the current binding level.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  tem = pushdecl (decl);

  /* For a local variable, define the RTL now.  */
  if (current_binding_level != global_binding_level
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && DECL_RTL (tem) == 0)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }

  if (init_written)
    {
      /* When parsing and digesting the initializer,
	 use temporary storage.  Do this even if we will ignore the value.  */
      if (current_binding_level == global_binding_level && debug_temp_inits)
	temporary_allocation ();
    }

  return tem;
}

/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  register tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  int temporary = allocation_temporary_p ();
  char *asmspec = 0;

  /* If a name was specified, get the string.   */
  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  /* If `start_decl' didn't like having an initialization, ignore it now.  */

  if (init != 0 && DECL_INITIAL (decl) == 0)
    init = 0;
  /* Don't crash if parm is initialized.  */
  if (TREE_CODE (decl) == PARM_DECL)
    init = 0;

  if (ITERATOR_P (decl))
    {
      if (init == 0)
	error_with_decl (decl, "iterator has no initial value");
      else
	init = save_expr (init);
    }

  if (init)
    {
      if (TREE_CODE (decl) != TYPE_DECL)
	store_init_value (decl, init);
      else
	{
	  /* typedef foo = bar; store the type of bar as the type of foo.  */
	  TREE_TYPE (decl) = TREE_TYPE (init);
	  DECL_INITIAL (decl) = init = 0;
	}
    }

  /* Pop back to the obstack that is current for this binding level.
     This is because MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack.  But don't discard the
     temporary data yet.  */
  pop_obstacks ();
#if 0 /* pop_obstacks was near the end; this is what was here.  */
  if (current_binding_level == global_binding_level && temporary)
    end_temporary_allocation ();
#endif

  /* Deduce size of array from initialization, if not already known */

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
      int do_default
	= (TREE_STATIC (decl)
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && !TREE_PUBLIC (decl)
	   : !DECL_EXTERNAL (decl));
      int failure
	= complete_array_type (type, DECL_INITIAL (decl), do_default);

      /* Get the completed type made by complete_array_type.  */
      type = TREE_TYPE (decl);

      if (failure == 1)
	error_with_decl (decl, "initializer fails to determine size of `%s'");

      if (failure == 2)
	{
	  if (do_default)
	    error_with_decl (decl, "array size missing in `%s'");
	  /* If a `static' var's size isn't known,
	     make it extern as well as static, so it does not get
	     allocated.
	     If it is not `static', then do not mark extern;
	     finish_incomplete_decl will give it a default size
	     and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && ! TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	}

      /* TYPE_MAX_VALUE is always one less than the number of elements
	 in the array, because we start counting at zero.  Therefore,
	 warn only if the value is less than zero.  */
      if (pedantic && TYPE_DOMAIN (type) != 0
	  && tree_int_cst_sgn (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) < 0)
	error_with_decl (decl, "zero or negative size array `%s'");

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0
	  && TYPE_SIZE (TREE_TYPE (decl)) != 0)
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
	  && (TREE_STATIC (decl)
	      ?
		/* A static variable with an incomplete type
		   is an error if it is initialized.
		   Also if it is not file scope.
		   Otherwise, let it through, but if it is not `extern'
		   then it may cause an error message later.  */
	      /* We must use DECL_CONTEXT instead of current_binding_level,
		 because a duplicate_decls call could have changed the binding
		 level of this decl.  */
		(DECL_INITIAL (decl) != 0 || DECL_CONTEXT (decl) != 0)
	      :
		/* An automatic variable with an incomplete type
		   is an error.  */
		!DECL_EXTERNAL (decl)))
	{
	  error_with_decl (decl, "storage size of `%s' isn't known");
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    error_with_decl (decl, "storage size of `%s' isn't constant");
	}
    }

  /* If this is a function and an assembler name is specified, it isn't
     builtin any more.  Also reset DECL_RTL so we can give it its new
     name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
      {
	DECL_BUILT_IN (decl) = 0;
	DECL_RTL (decl) = 0;
      }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      if ((flag_traditional || TREE_PERMANENT (decl))
	  && allocation_temporary_p ())
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	  /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
	  maybe_objc_check_decl (decl);
	  rest_of_decl_compilation (decl, asmspec, DECL_CONTEXT (decl) == 0,
				    0);
	  pop_obstacks ();
	}
      else
	{
	  /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
	  maybe_objc_check_decl (decl);
	  rest_of_decl_compilation (decl, asmspec, DECL_CONTEXT (decl) == 0,
				    0);
	}
      if (DECL_CONTEXT (decl) != 0)
	{
	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete
	      && ! TREE_STATIC (decl) && ! DECL_EXTERNAL (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	      expand_decl (decl);
	    }
	  /* Compute and store the initial value.  */
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    expand_decl_init (decl);
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
      maybe_objc_check_decl (decl);
      rest_of_decl_compilation (decl, NULL_PTR, DECL_CONTEXT (decl) == 0,
				0);
    }

  /* ??? After 2.3, test (init != 0) instead of TREE_CODE.  */
  /* This test used to include TREE_PERMANENT, however, we have the same
     problem with initializers at the function level.  Such initializers get
     saved until the end of the function on the momentary_obstack.  */
  if (!(TREE_CODE (decl) == FUNCTION_DECL && DECL_INLINE (decl))
      && temporary
      /* DECL_INITIAL is not defined in PARM_DECLs, since it shares
	 space with DECL_ARG_TYPE.  */
      && TREE_CODE (decl) != PARM_DECL)
    {
      /* We need to remember that this array HAD an initialization,
	 but discard the actual temporary nodes,
	 since we can't have a permanent node keep pointing to them.  */
      /* We make an exception for inline functions, since it's
	 normal for a local extern redeclaration of an inline function
	 to have a copy of the top-level decl's DECL_INLINE.  */
      if (DECL_INITIAL (decl) != 0 && DECL_INITIAL (decl) != error_mark_node)
	{
	  /* If this is a const variable, then preserve the
	     initializer instead of discarding it so that we can optimize
	     references to it.  */
	  /* This test used to include TREE_STATIC, but this won't be set
	     for function level initializers.  */
	  if (TREE_READONLY (decl) || ITERATOR_P (decl))
	    {
	      preserve_initializer ();
	      /* Hack?  Set the permanent bit for something that is permanent,
		 but not on the permanent obstack, so as to convince
		 output_constant_def to make its rtl on the permanent
		 obstack.  */
	      TREE_PERMANENT (DECL_INITIAL (decl)) = 1;

	      /* The initializer and DECL must have the same (or equivalent
		 types), but if the initializer is a STRING_CST, its type
		 might not be on the right obstack, so copy the type
		 of DECL.  */
	      TREE_TYPE (DECL_INITIAL (decl)) = type;
	    }
	  else
	    DECL_INITIAL (decl) = error_mark_node;
	}
    }

  /* If requested, warn about definitions of large data objects.  */

  if (warn_larger_than
      && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
      && !DECL_EXTERNAL (decl))
    {
      register tree decl_size = DECL_SIZE (decl);

      if (decl_size && TREE_CODE (decl_size) == INTEGER_CST)
	{
	   unsigned units = TREE_INT_CST_LOW(decl_size) / BITS_PER_UNIT;

	  if (units > larger_than_size)
	    warning_with_decl (decl, "size of `%s' is %u bytes", units);
	}
    }

#if 0
  /* Resume permanent allocation, if not within a function.  */
  /* The corresponding push_obstacks_nochange is in start_decl,
     and in push_parm_decl and in grokfield.  */
  pop_obstacks ();
#endif

  /* If we have gone back from temporary to permanent allocation,
     actually free the temporary space that we no longer need.  */
  if (temporary && !allocation_temporary_p ())
    permanent_allocation (0);

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_binding_level == global_binding_level)
    get_pending_sizes ();
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl;
{
  /* There are no cleanups in C.  */
  return NULL_TREE;
}

/* Given a parsed parameter declaration,
   decode it into a PARM_DECL and push that on the current binding level.
   Also, for the sake of forward parm decls,
   record the given order of parms in `parm_order'.  */

void
push_parm_decl (parm)
     tree parm;
{
  tree decl;
  int old_immediate_size_expand = immediate_size_expand;
  /* Don't try computing parm sizes now -- wait till fn is called.  */
  immediate_size_expand = 0;

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();

  decl = grokdeclarator (TREE_VALUE (TREE_PURPOSE (parm)),
			 TREE_PURPOSE (TREE_PURPOSE (parm)), PARM, 0);
  decl_attributes (decl, TREE_VALUE (TREE_VALUE (parm)),
		   TREE_PURPOSE (TREE_VALUE (parm)));

#if 0
  if (DECL_NAME (decl))
    {
      tree olddecl;
      olddecl = lookup_name (DECL_NAME (decl));
      if (pedantic && olddecl != 0 && TREE_CODE (olddecl) == TYPE_DECL)
	pedwarn_with_decl (decl, "ANSI C forbids parameter `%s' shadowing typedef");
    }
#endif

  decl = pushdecl (decl);

  immediate_size_expand = old_immediate_size_expand;

  current_binding_level->parm_order
    = tree_cons (NULL_TREE, decl, current_binding_level->parm_order);

  /* Add this decl to the current binding level.  */
  finish_decl (decl, NULL_TREE, NULL_TREE);
}

/* Clear the given order of parms in `parm_order'.
   Used at start of parm list,
   and also at semicolon terminating forward decls.  */

void
clear_parm_order ()
{
  current_binding_level->parm_order = NULL_TREE;
}

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 1 if DO_DEFAULT).  */

int
complete_array_type (type, initial_value, do_default)
     tree type;
     tree initial_value;
     int do_default;
{
  register tree maxindex = NULL_TREE;
  int value = 0;

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
      TYPE_DOMAIN (type) = build_index_type (maxindex);
      if (!TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = TYPE_DOMAIN (type);
#if 0 /* I took out this change
	 together with the change in build_array_type. --rms  */
      change_main_variant (type,
			   build_array_type (TREE_TYPE (type),
					     TYPE_DOMAIN (type)));
#endif
    }

  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  return value;
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
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
     BITFIELD for a field with specified width.
   INITIALIZED is 1 if the decl has an initializer.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are interpreted.  */

static tree
grokdeclarator (declarator, declspecs, decl_context, initialized)
     tree declspecs;
     tree declarator;
     enum decl_context decl_context;
     int initialized;
{
  int specbits = 0;
  tree spec;
  tree type = NULL_TREE;
  int longlong = 0;
  int constp;
  int volatilep;
  int inlinep;
  int explicit_int = 0;
  int explicit_char = 0;
  int defaulted_int = 0;
  tree typedef_decl = 0;
  char *name;
  tree typedef_type = 0;
  int funcdef_flag = 0;
  enum tree_code innermost_code = ERROR_MARK;
  int bitfield = 0;
  int size_varies = 0;
  tree decl_machine_attr = NULL_TREE;

  if (decl_context == BITFIELD)
    bitfield = 1, decl_context = FIELD;

  if (decl_context == FUNCDEF)
    funcdef_flag = 1, decl_context = NORMAL;

  push_obstacks_nochange ();

  if (flag_traditional && allocation_temporary_p ())
    end_temporary_allocation ();

  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  {
    register tree decl = declarator;
    name = 0;

    while (decl)
      switch (TREE_CODE (decl))
	{
	case ARRAY_REF:
	case INDIRECT_REF:
	case CALL_EXPR:
	  innermost_code = TREE_CODE (decl);
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case IDENTIFIER_NODE:
	  name = IDENTIFIER_POINTER (decl);
	  decl = 0;
	  break;

	default:
	  abort ();
	}
    if (name == 0)
      name = "type name";
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  /* Anything declared one level down from the top level
     must be one of the parameters of a function
     (because the body is at least two levels down).  */

  /* If this looks like a function definition, make it one,
     even if it occurs where parms are expected.
     Then store_parm_decls will reject it and not use it as a parm.  */
  if (decl_context == NORMAL && !funcdef_flag
      && current_binding_level->parm_flag)
    decl_context = PARM;

  /* Look through the decl specs and record which ones appear.
     Some typespecs are defined as built-in typenames.
     Others, the ones that are modifiers of other types,
     are represented by bits in SPECBITS: set the bits for
     the modifiers that appear.  Storage class keywords are also in SPECBITS.

     If there is a typedef name or a type, store the type in TYPE.
     This includes builtin typedefs such as `int'.

     Set EXPLICIT_INT or EXPLICIT_CHAR if the type is `int' or `char'
     and did not come from a user typedef.

     Set LONGLONG if `long' is mentioned twice.  */

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      register int i;
      register tree id = TREE_VALUE (spec);

      if (id == ridpointers[(int) RID_INT])
	explicit_int = 1;
      if (id == ridpointers[(int) RID_CHAR])
	explicit_char = 1;

      if (TREE_CODE (id) == IDENTIFIER_NODE)
	for (i = (int) RID_FIRST_MODIFIER; i < (int) RID_MAX; i++)
	  {
	    if (ridpointers[i] == id)
	      {
		if (i == (int) RID_LONG && specbits & (1<<i))
		  {
		    if (longlong)
		      error ("`long long long' is too long for GCC");
		    else
		      {
			if (pedantic && ! in_system_header)
			  pedwarn ("ANSI C does not support `long long'");
			longlong = 1;
		      }
		  }
		else if (specbits & (1 << i))
		  pedwarn ("duplicate `%s'", IDENTIFIER_POINTER (id));
		specbits |= 1 << i;
		goto found;
	      }
	  }
      if (type)
	error ("two or more data types in declaration of `%s'", name);
      /* Actual typedefs come to us as TYPE_DECL nodes.  */
      else if (TREE_CODE (id) == TYPE_DECL)
	{
	  type = TREE_TYPE (id);
          decl_machine_attr = DECL_MACHINE_ATTRIBUTES (id);
	  typedef_decl = id;
	}
      /* Built-in types come as identifiers.  */
      else if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  register tree t = lookup_name (id);
	  if (TREE_TYPE (t) == error_mark_node)
	    ;
	  else if (!t || TREE_CODE (t) != TYPE_DECL)
	    error ("`%s' fails to be a typedef or built in type",
		   IDENTIFIER_POINTER (id));
	  else
	    {
	      type = TREE_TYPE (t);
	      typedef_decl = t;
	    }
	}
      else if (TREE_CODE (id) != ERROR_MARK)
	type = id;

    found: {}
    }

  typedef_type = type;
  if (type)
    size_varies = C_TYPE_VARIABLE_SIZE (type);

  /* No type at all: default to `int', and set DEFAULTED_INT
     because it was not a user-defined typedef.  */

  if (type == 0)
    {
      if (funcdef_flag && warn_return_type
	  && ! (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			    | (1 << (int) RID_SIGNED) | (1 << (int) RID_UNSIGNED))))
	warn_about_return_type = 1;
      defaulted_int = 1;
      type = integer_type_node;
    }

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Long double is a special combination.  */

  if ((specbits & 1 << (int) RID_LONG)
      && TYPE_MAIN_VARIANT (type) == double_type_node)
    {
      specbits &= ~ (1 << (int) RID_LONG);
      type = long_double_type_node;
    }

  /* Check all other uses of type modifiers.  */

  if (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
		  | (1 << (int) RID_UNSIGNED) | (1 << (int) RID_SIGNED)))
    {
      int ok = 0;

      if (TREE_CODE (type) != INTEGER_TYPE)
	error ("long, short, signed or unsigned invalid for `%s'", name);
      else if ((specbits & 1 << (int) RID_LONG)
	       && (specbits & 1 << (int) RID_SHORT))
	error ("long and short specified together for `%s'", name);
      else if (((specbits & 1 << (int) RID_LONG)
		|| (specbits & 1 << (int) RID_SHORT))
	       && explicit_char)
	error ("long or short specified with char for `%s'", name);
      else if (((specbits & 1 << (int) RID_LONG)
		|| (specbits & 1 << (int) RID_SHORT))
	       && TREE_CODE (type) == REAL_TYPE)
	error ("long or short specified with floating type for `%s'", name);
      else if ((specbits & 1 << (int) RID_SIGNED)
	       && (specbits & 1 << (int) RID_UNSIGNED))
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
	  specbits &= ~((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			| (1 << (int) RID_UNSIGNED) | (1 << (int) RID_SIGNED));
	  longlong = 0;
	}
    }

  if ((specbits & (1 << (int) RID_COMPLEX))
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    {
      error ("complex invalid for `%s'", name);
      specbits &= ~ (1 << (int) RID_COMPLEX);
    }

  /* Decide whether an integer type is signed or not.
     Optionally treat bitfields as signed by default.  */
  if (specbits & 1 << (int) RID_UNSIGNED
      /* Traditionally, all bitfields are unsigned.  */
      || (bitfield && flag_traditional
	  && (! explicit_flag_signed_bitfields || !flag_signed_bitfields))
      || (bitfield && ! flag_signed_bitfields
	  && (explicit_int || defaulted_int || explicit_char
	      /* A typedef for plain `int' without `signed'
		 can be controlled just like plain `int'.  */
	      || ! (typedef_decl != 0
		    && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	  && TREE_CODE (type) != ENUMERAL_TYPE
	  && !(specbits & 1 << (int) RID_SIGNED)))
    {
      if (longlong)
	type = long_long_unsigned_type_node;
      else if (specbits & 1 << (int) RID_LONG)
	type = long_unsigned_type_node;
      else if (specbits & 1 << (int) RID_SHORT)
	type = short_unsigned_type_node;
      else if (type == char_type_node)
	type = unsigned_char_type_node;
      else if (typedef_decl)
	type = unsigned_type (type);
      else
	type = unsigned_type_node;
    }
  else if ((specbits & 1 << (int) RID_SIGNED)
	   && type == char_type_node)
    type = signed_char_type_node;
  else if (longlong)
    type = long_long_integer_type_node;
  else if (specbits & 1 << (int) RID_LONG)
    type = long_integer_type_node;
  else if (specbits & 1 << (int) RID_SHORT)
    type = short_integer_type_node;

  if (specbits & 1 << (int) RID_COMPLEX)
    {
      /* If we just have "complex", it is equivalent to
	 "complex double", but if any modifiers at all are specified it is
	 the complex form of TYPE.  E.g, "complex short" is
	 "complex short int".  */

      if (defaulted_int && ! longlong
	  && ! (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
			    | (1 << (int) RID_SIGNED)
			    | (1 << (int) RID_UNSIGNED))))
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

  /* Set CONSTP if this declaration is `const', whether by
     explicit specification or via a typedef.
     Likewise for VOLATILEP.  */

  constp = !! (specbits & 1 << (int) RID_CONST) + TYPE_READONLY (type);
  volatilep = !! (specbits & 1 << (int) RID_VOLATILE) + TYPE_VOLATILE (type);
  inlinep = !! (specbits & (1 << (int) RID_INLINE));
  if (constp > 1)
    pedwarn ("duplicate `const'");
  if (volatilep > 1)
    pedwarn ("duplicate `volatile'");
  if (! flag_gen_aux_info && (TYPE_READONLY (type) || TYPE_VOLATILE (type)))
    type = TYPE_MAIN_VARIANT (type);

  /* Warn if two storage classes are given. Default to `auto'.  */

  {
    int nclasses = 0;

    if (specbits & 1 << (int) RID_AUTO) nclasses++;
    if (specbits & 1 << (int) RID_STATIC) nclasses++;
    if (specbits & 1 << (int) RID_EXTERN) nclasses++;
    if (specbits & 1 << (int) RID_REGISTER) nclasses++;
    if (specbits & 1 << (int) RID_TYPEDEF) nclasses++;
    if (specbits & 1 << (int) RID_ITERATOR) nclasses++;

    /* Warn about storage classes that are invalid for certain
       kinds of declarations (parameters, typenames, etc.).  */

    if (nclasses > 1)
      error ("multiple storage classes in declaration of `%s'", name);
    else if (funcdef_flag
	     && (specbits
		 & ((1 << (int) RID_REGISTER)
		    | (1 << (int) RID_AUTO)
		    | (1 << (int) RID_TYPEDEF))))
      {
	if (specbits & 1 << (int) RID_AUTO
	    && (pedantic || current_binding_level == global_binding_level))
	  pedwarn ("function definition declared `auto'");
	if (specbits & 1 << (int) RID_REGISTER)
	  error ("function definition declared `register'");
	if (specbits & 1 << (int) RID_TYPEDEF)
	  error ("function definition declared `typedef'");
	specbits &= ~ ((1 << (int) RID_TYPEDEF) | (1 << (int) RID_REGISTER)
		       | (1 << (int) RID_AUTO));
      }
    else if (decl_context != NORMAL && nclasses > 0)
      {
	if (decl_context == PARM && specbits & 1 << (int) RID_REGISTER)
	  ;
	else
	  {
	    error ((decl_context == FIELD
		    ? "storage class specified for structure field `%s'"
		    : (decl_context == PARM
		       ? "storage class specified for parameter `%s'"
		       : "storage class specified for typename")),
		   name);
	    specbits &= ~ ((1 << (int) RID_TYPEDEF) | (1 << (int) RID_REGISTER)
			   | (1 << (int) RID_AUTO) | (1 << (int) RID_STATIC)
			   | (1 << (int) RID_EXTERN));
	  }
      }
    else if (specbits & 1 << (int) RID_EXTERN && initialized && ! funcdef_flag)
      {
	/* `extern' with initialization is invalid if not at top level.  */
	if (current_binding_level == global_binding_level)
	  warning ("`%s' initialized and declared `extern'", name);
	else
	  error ("`%s' has both `extern' and initializer", name);
      }
    else if (specbits & 1 << (int) RID_EXTERN && funcdef_flag
	     && current_binding_level != global_binding_level)
      error ("nested function `%s' declared `extern'", name);
    else if (current_binding_level == global_binding_level
	     && specbits & (1 << (int) RID_AUTO))
      error ("top-level declaration of `%s' specifies `auto'", name);
    else if ((specbits & 1 << (int) RID_ITERATOR)
	     && TREE_CODE (declarator) != IDENTIFIER_NODE)
      {
	error ("iterator `%s' has derived type", name);
	type = error_mark_node;
      }
    else if ((specbits & 1 << (int) RID_ITERATOR)
	     && TREE_CODE (type) != INTEGER_TYPE)
      {
	error ("iterator `%s' has noninteger type", name);
	type = error_mark_node;
      }
  }

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an absolute declarator).  */

  while (declarator && TREE_CODE (declarator) != IDENTIFIER_NODE)
    {
      if (type == error_mark_node)
	{
	  declarator = TREE_OPERAND (declarator, 0);
	  continue;
	}

      /* Each level of DECLARATOR is either an ARRAY_REF (for ...[..]),
	 an INDIRECT_REF (for *...),
	 a CALL_EXPR (for ...(...)),
	 an identifier (for the name being declared)
	 or a null pointer (for the place in an absolute declarator
	 where the name was omitted).
	 For the last two cases, we have just exited the loop.

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (TREE_CODE (declarator) == ARRAY_REF)
	{
	  register tree itype = NULL_TREE;
	  register tree size = TREE_OPERAND (declarator, 1);
	  /* An uninitialized decl with `extern' is a reference.  */
	  int extern_ref = !initialized && (specbits & (1 << (int) RID_EXTERN));
	  /* The index is a signed object `sizetype' bits wide.  */
	  tree index_type = signed_type (sizetype);

	  declarator = TREE_OPERAND (declarator, 0);

	  /* Check for some types that there cannot be arrays of.  */

	  if (TYPE_MAIN_VARIANT (type) == void_type_node)
	    {
	      error ("declaration of `%s' as array of voids", name);
	      type = error_mark_node;
	    }

	  if (TREE_CODE (type) == FUNCTION_TYPE)
	    {
	      error ("declaration of `%s' as array of functions", name);
	      type = error_mark_node;
	    }

	  if (size == error_mark_node)
	    type = error_mark_node;

	  if (type == error_mark_node)
	    continue;

	  /* If this is a block level extern, it must live past the end
	     of the function so that we can check it against other extern
	     declarations (IDENTIFIER_LIMBO_VALUE).  */
	  if (extern_ref && allocation_temporary_p ())
	    end_temporary_allocation ();

	  /* If size was specified, set ITYPE to a range-type for that size.
	     Otherwise, ITYPE remains null.  finish_decl may figure it out
	     from an initial value.  */

	  if (size)
	    {
	      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
	      STRIP_TYPE_NOPS (size);

	      if (TREE_CODE (TREE_TYPE (size)) != INTEGER_TYPE
		  && TREE_CODE (TREE_TYPE (size)) != ENUMERAL_TYPE)
		{
		  error ("size of array `%s' has non-integer type", name);
		  size = integer_one_node;
		}

	      if (pedantic && integer_zerop (size))
		pedwarn ("ANSI C forbids zero-size array `%s'", name);

	      if (TREE_CODE (size) == INTEGER_CST)
		{
		  constant_expression_warning (size);
		  if (tree_int_cst_sgn (size) < 0)
		    {
		      error ("size of array `%s' is negative", name);
		      size = integer_one_node;
		    }
		}
	      else
		{
		  /* Make sure the array size remains visibly nonconstant
		     even if it is (eg) a const variable with known value.  */
		  size_varies = 1;

		  if (pedantic)
		    {
		      if (TREE_CONSTANT (size))
			pedwarn ("ANSI C forbids array `%s' whose size can't be evaluated", name);
		      else
			pedwarn ("ANSI C forbids variable-size array `%s'", name);
		    }
		}

	      /* Convert size to index_type, so that if it is a variable
		 the computations will be done in the proper mode.  */
	      itype = fold (build (MINUS_EXPR, index_type,
				   convert (index_type, size),
				   convert (index_type, size_one_node)));

	      if (size_varies)
		itype = variable_size (itype);
	      itype = build_index_type (itype);
	    }

#if 0 /* This had bad results for pointers to arrays, as in
	 union incomplete (*foo)[4];  */
	  /* Complain about arrays of incomplete types, except in typedefs.  */

	  if (TYPE_SIZE (type) == 0
	      /* Avoid multiple warnings for nested array types.  */
	      && TREE_CODE (type) != ARRAY_TYPE
	      && !(specbits & (1 << (int) RID_TYPEDEF))
	      && !C_TYPE_BEING_DEFINED (type))
	    warning ("array type has incomplete element type");
#endif

#if 0  /* We shouldn't have a function type here at all!
	  Functions aren't allowed as array elements.  */
	  if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	      && (constp || volatilep))
	    pedwarn ("ANSI C forbids const or volatile function types");
#endif

	  /* Build the array type itself, then merge any constancy or
	     volatility into the target type.  We must do it in this order
	     to ensure that the TYPE_MAIN_VARIANT field of the array type
	     is set correctly.  */

	  type = build_array_type (type, itype);
	  if (constp || volatilep)
	    type = c_build_type_variant (type, constp, volatilep);

#if 0	/* don't clear these; leave them set so that the array type
	   or the variable is itself const or volatile.  */
	  constp = 0;
	  volatilep = 0;
#endif

	  if (size_varies)
	    C_TYPE_VARIABLE_SIZE (type) = 1;
	}
      else if (TREE_CODE (declarator) == CALL_EXPR)
	{
	  int extern_ref = (!(specbits & (1 << (int) RID_AUTO))
			    || current_binding_level == global_binding_level);
	  tree arg_types;

	  /* Declaring a function type.
	     Make sure we have a valid type for the function to return.  */
	  if (type == error_mark_node)
	    continue;

	  size_varies = 0;

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

#ifndef TRADITIONAL_RETURN_FLOAT
	  /* Traditionally, declaring return type float means double.  */

	  if (flag_traditional && TYPE_MAIN_VARIANT (type) == float_type_node)
	    type = double_type_node;
#endif /* TRADITIONAL_RETURN_FLOAT */

	  /* If this is a block level extern, it must live past the end
	     of the function so that we can check it against other extern
	     declarations (IDENTIFIER_LIMBO_VALUE).  */
	  if (extern_ref && allocation_temporary_p ())
	    end_temporary_allocation ();

	  /* Construct the function type and go to the next
	     inner layer of declarator.  */

	  arg_types = grokparms (TREE_OPERAND (declarator, 1),
				 funcdef_flag
				 /* Say it's a definition
				    only for the CALL_EXPR
				    closest to the identifier.  */
				 && TREE_CODE (TREE_OPERAND (declarator, 0)) == IDENTIFIER_NODE);
#if 0 /* This seems to be false.  We turn off temporary allocation
	 above in this function if -traditional.
	 And this code caused inconsistent results with prototypes:
	 callers would ignore them, and pass arguments wrong.  */

	  /* Omit the arg types if -traditional, since the arg types
	     and the list links might not be permanent.  */
	  type = build_function_type (type,
				      flag_traditional 
				      ? NULL_TREE : arg_types);
#endif
	  /* ANSI seems to say that `const int foo ();'
	     does not make the function foo const.  */
	  if (constp || volatilep)
	    type = c_build_type_variant (type, constp, volatilep);
	  constp = 0;
	  volatilep = 0;

	  type = build_function_type (type, arg_types);
	  declarator = TREE_OPERAND (declarator, 0);

	  /* Set the TYPE_CONTEXTs for each tagged type which is local to
	     the formal parameter list of this FUNCTION_TYPE to point to
	     the FUNCTION_TYPE node itself.  */

	  {
	    register tree link;

	    for (link = current_function_parm_tags;
		 link;
		 link = TREE_CHAIN (link))
	      TYPE_CONTEXT (TREE_VALUE (link)) = type;
	  }
	}
      else if (TREE_CODE (declarator) == INDIRECT_REF)
	{
	  /* Merge any constancy or volatility into the target type
	     for the pointer.  */

	  if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	      && (constp || volatilep))
	    pedwarn ("ANSI C forbids const or volatile function types");
	  if (constp || volatilep)
	    type = c_build_type_variant (type, constp, volatilep);
	  constp = 0;
	  volatilep = 0;
	  size_varies = 0;

	  type = build_pointer_type (type);

	  /* Process a list of type modifier keywords
	     (such as const or volatile) that were given inside the `*'.  */

	  if (TREE_TYPE (declarator))
	    {
	      register tree typemodlist;
	      int erred = 0;
	      for (typemodlist = TREE_TYPE (declarator); typemodlist;
		   typemodlist = TREE_CHAIN (typemodlist))
		{
		  if (TREE_VALUE (typemodlist) == ridpointers[(int) RID_CONST])
		    constp++;
		  else if (TREE_VALUE (typemodlist) == ridpointers[(int) RID_VOLATILE])
		    volatilep++;
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
	    }

	  declarator = TREE_OPERAND (declarator, 0);
	}
      else
	abort ();

    }

  /* Now TYPE has the actual type.  */

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (specbits & (1 << (int) RID_TYPEDEF))
    {
      tree decl;
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters */
      if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	  && (constp || volatilep))
	pedwarn ("ANSI C forbids const or volatile function types");
      if (constp || volatilep)
	type = c_build_type_variant (type, constp, volatilep);
      pop_obstacks ();
      decl = build_decl (TYPE_DECL, declarator, type);
      if ((specbits & (1 << (int) RID_SIGNED))
	  || (typedef_decl && C_TYPEDEF_EXPLICITLY_SIGNED (typedef_decl)))
	C_TYPEDEF_EXPLICITLY_SIGNED (decl) = 1;
      return decl;
    }

  /* Detect the case of an array type of unspecified size
     which came, as such, direct from a typedef name.
     We must copy the type, so that each identifier gets
     a distinct type, so that each identifier's size can be
     controlled separately by its own initializer.  */

  if (type != 0 && typedef_type != 0
      && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (typedef_type)
      && TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type) == 0)
    {
      type = build_array_type (TREE_TYPE (type), 0);
      if (size_varies)
	C_TYPE_VARIABLE_SIZE (type) = 1;
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters */
      if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	  && (constp || volatilep))
	pedwarn ("ANSI C forbids const or volatile function types");
      if (constp || volatilep)
	type = c_build_type_variant (type, constp, volatilep);
      pop_obstacks ();
      return type;
    }

  /* Aside from typedefs and type names (handle above),
     `void' at top level (not within pointer)
     is allowed only in public variables.
     We don't complain about parms either, but that is because
     a better error message can be made later.  */

  if (TYPE_MAIN_VARIANT (type) == void_type_node && decl_context != PARM
      && ! ((decl_context != FIELD && TREE_CODE (type) != FUNCTION_TYPE)
	    && ((specbits & (1 << (int) RID_EXTERN))
		|| (current_binding_level == global_binding_level
		    && !(specbits
			 & ((1 << (int) RID_STATIC) | (1 << (int) RID_REGISTER)))))))
    {
      error ("variable or field `%s' declared void", name);
      type = integer_type_node;
    }

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  {
    register tree decl;

    if (decl_context == PARM)
      {
	tree type_as_written = type;
	tree main_type;

	/* A parameter declared as an array of T is really a pointer to T.
	   One declared as a function is really a pointer to a function.  */

	if (TREE_CODE (type) == ARRAY_TYPE)
	  {
	    /* Transfer const-ness of array into that of type pointed to.  */
	    type = TREE_TYPE (type);
	    if (constp || volatilep)
	      type = c_build_type_variant (type, constp, volatilep);
	    type = build_pointer_type (type);
	    volatilep = constp = 0;
	    size_varies = 0;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    if (pedantic && (constp || volatilep))
	      pedwarn ("ANSI C forbids const or volatile function types");
	    if (constp || volatilep)
	      type = c_build_type_variant (type, constp, volatilep);
	    type = build_pointer_type (type);
	    volatilep = constp = 0;
	  }

	decl = build_decl (PARM_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	/* Compute the type actually passed in the parmlist,
	   for the case where there is no prototype.
	   (For example, shorts and chars are passed as ints.)
	   When there is a prototype, this is overridden later.  */

	DECL_ARG_TYPE (decl) = type;
	main_type = (type == error_mark_node
		     ? error_mark_node
		     : TYPE_MAIN_VARIANT (type));
	if (main_type == float_type_node)
	  DECL_ARG_TYPE (decl) = double_type_node;
	/* Don't use TYPE_PRECISION to decide whether to promote,
	   because we should convert short if it's the same size as int,
	   but we should not convert long if it's the same size as int.  */
	else if (TREE_CODE (main_type) != ERROR_MARK
		 && C_PROMOTING_INTEGER_TYPE_P (main_type))
	  {
	    if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)
		&& TREE_UNSIGNED (type))
	      DECL_ARG_TYPE (decl) = unsigned_type_node;
	    else
	      DECL_ARG_TYPE (decl) = integer_type_node;
	  }

	DECL_ARG_TYPE_AS_WRITTEN (decl) = type_as_written;
      }
    else if (decl_context == FIELD)
      {
	/* Structure field.  It may not be a function.  */

	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    error ("field `%s' declared as a function", name);
	    type = build_pointer_type (type);
	  }
	else if (TREE_CODE (type) != ERROR_MARK && TYPE_SIZE (type) == 0)
	  {
	    error ("field `%s' has incomplete type", name);
	    type = error_mark_node;
	  }
	/* Move type qualifiers down to element of an array.  */
	if (TREE_CODE (type) == ARRAY_TYPE && (constp || volatilep))
	  {
	    type = build_array_type (c_build_type_variant (TREE_TYPE (type),
							   constp, volatilep),
				     TYPE_DOMAIN (type));
#if 0 /* Leave the field const or volatile as well.  */
	    constp = volatilep = 0;
#endif
	  }
	decl = build_decl (FIELD_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE)
      {
	/* Every function declaration is "external"
	   except for those which are inside a function body
	   in which `auto' is used.
	   That is a case not specified by ANSI C,
	   and we use it for forward declarations for nested functions.  */
	int extern_ref = (!(specbits & (1 << (int) RID_AUTO))
			  || current_binding_level == global_binding_level);

	if (specbits & (1 << (int) RID_AUTO)
	    && (pedantic || current_binding_level == global_binding_level))
	  pedwarn ("invalid storage class for function `%s'", name);
	if (specbits & (1 << (int) RID_REGISTER))
	  error ("invalid storage class for function `%s'", name);
	/* Function declaration not at top level.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (current_binding_level != global_binding_level
	    && (specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_INLINE)))
	    && pedantic)
	  pedwarn ("invalid storage class for function `%s'", name);

	/* If this is a block level extern, it must live past the end
	   of the function so that we can check it against other
	   extern declarations (IDENTIFIER_LIMBO_VALUE).  */
	if (extern_ref && allocation_temporary_p ())
	  end_temporary_allocation ();

	decl = build_decl (FUNCTION_DECL, declarator, type);
	decl = build_decl_attribute_variant (decl, decl_machine_attr);

	if (pedantic && (constp || volatilep)
	    && ! DECL_IN_SYSTEM_HEADER (decl))
	  pedwarn ("ANSI C forbids const or volatile functions");

	if (volatilep
	    && TREE_TYPE (TREE_TYPE (decl)) != void_type_node)
	  warning ("`noreturn' function returns non-void value");

	if (extern_ref)
	  DECL_EXTERNAL (decl) = 1;
	/* Record absence of global scope for `static' or `auto'.  */
	TREE_PUBLIC (decl)
	  = !(specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_AUTO)));

	/* Record presence of `inline', if it is reasonable.  */
	if (inlinep)
	  {
	    tree last = tree_last (TYPE_ARG_TYPES (type));

	    if (! strcmp (IDENTIFIER_POINTER (declarator), "main"))
	      warning ("cannot inline function `main'");
	    else if (last && (TYPE_MAIN_VARIANT (TREE_VALUE (last))
			      != void_type_node))
	      warning ("inline declaration ignored for function with `...'");
	    else
	      /* Assume that otherwise the function can be inlined.  */
	      DECL_INLINE (decl) = 1;

	    if (specbits & (1 << (int) RID_EXTERN))
	      current_extern_inline = 1;
	  }
      }
    else
      {
	/* It's a variable.  */
	/* An uninitialized decl with `extern' is a reference.  */
	int extern_ref = !initialized && (specbits & (1 << (int) RID_EXTERN));

	/* Move type qualifiers down to element of an array.  */
	if (TREE_CODE (type) == ARRAY_TYPE && (constp || volatilep))
	  {
	    type = build_array_type (c_build_type_variant (TREE_TYPE (type),
							   constp, volatilep),
				     TYPE_DOMAIN (type));
#if 0 /* Leave the variable const or volatile as well.  */
	    constp = volatilep = 0;
#endif
	  }

	/* If this is a block level extern, it must live past the end
	   of the function so that we can check it against other
	   extern declarations (IDENTIFIER_LIMBO_VALUE).  */
	if (extern_ref && allocation_temporary_p ())
	  end_temporary_allocation ();

	decl = build_decl (VAR_DECL, declarator, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	if (inlinep)
	  pedwarn_with_decl (decl, "variable `%s' declared `inline'");

	DECL_EXTERNAL (decl) = extern_ref;
	/* At top level, the presence of a `static' or `register' storage
	   class specifier, or the absence of all storage class specifiers
	   makes this declaration a definition (perhaps tentative).  Also,
	   the absence of both `static' and `register' makes it public.  */
	if (current_binding_level == global_binding_level)
	  {
	    TREE_PUBLIC (decl)
	      = !(specbits
		  & ((1 << (int) RID_STATIC) | (1 << (int) RID_REGISTER)));
	    TREE_STATIC (decl) = ! DECL_EXTERNAL (decl);
	  }
	/* Not at top level, only `static' makes a static definition.  */
	else
	  {
	    TREE_STATIC (decl) = (specbits & (1 << (int) RID_STATIC)) != 0;
	    TREE_PUBLIC (decl) = DECL_EXTERNAL (decl);
	  }

	if (specbits & 1 << (int) RID_ITERATOR)
	  ITERATOR_P (decl) = 1;
      }

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (specbits & (1 << (int) RID_REGISTER))
      DECL_REGISTER (decl) = 1;

    /* Record constancy and volatility.  */

    if (constp)
      TREE_READONLY (decl) = 1;
    if (volatilep)
      {
	TREE_SIDE_EFFECTS (decl) = 1;
	TREE_THIS_VOLATILE (decl) = 1;
      }
    /* If a type has volatile components, it should be stored in memory.
       Otherwise, the fact that those components are volatile
       will be ignored, and would even crash the compiler.  */
    if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (decl)))
      mark_addressable (decl);

    pop_obstacks ();

    return decl;
  }
}

/* Decode the parameter-list info for a function type or function definition.
   The argument is the value returned by `get_parm_info' (or made in parse.y
   if there is an identifier list instead of a parameter decl list).
   These two functions are separate because when a function returns
   or receives functions then each is called multiple times but the order
   of calls is different.  The last call to `grokparms' is always the one
   that contains the formal parameter names of a function definition.

   Store in `last_function_parms' a chain of the decls of parms.
   Also store in `last_function_parm_tags' a chain of the struct, union,
   and enum tags declared among the parms.

   Return a list of arg types to use in the FUNCTION_TYPE for this function.

   FUNCDEF_FLAG is nonzero for a function definition, 0 for
   a mere declaration.  A nonempty identifier-list gets an error message
   when FUNCDEF_FLAG is zero.  */

static tree
grokparms (parms_info, funcdef_flag)
     tree parms_info;
     int funcdef_flag;
{
  tree first_parm = TREE_CHAIN (parms_info);

  last_function_parms = TREE_PURPOSE (parms_info);
  last_function_parm_tags = TREE_VALUE (parms_info);

  if (warn_strict_prototypes && first_parm == 0 && !funcdef_flag
      && !in_system_header)
    warning ("function declaration isn't a prototype");

  if (first_parm != 0
      && TREE_CODE (TREE_VALUE (first_parm)) == IDENTIFIER_NODE)
    {
      if (! funcdef_flag)
	pedwarn ("parameter names (without types) in function declaration");

      last_function_parms = first_parm;
      return 0;
    }
  else
    {
      tree parm;
      tree typelt;
      /* We no longer test FUNCDEF_FLAG.
	 If the arg types are incomplete in a declaration,
	 they must include undefined tags.
	 These tags can never be defined in the scope of the declaration,
	 so the types can never be completed,
	 and no call can be compiled successfully.  */
#if 0
      /* In a fcn definition, arg types must be complete.  */
      if (funcdef_flag)
#endif
	for (parm = last_function_parms, typelt = first_parm;
	     parm;
	     parm = TREE_CHAIN (parm))
	  /* Skip over any enumeration constants declared here.  */
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      /* Barf if the parameter itself has an incomplete type.  */
	      tree type = TREE_VALUE (typelt);
	      if (TYPE_SIZE (type) == 0)
		{
		  if (funcdef_flag && DECL_NAME (parm) != 0)
		    error ("parameter `%s' has incomplete type",
			   IDENTIFIER_POINTER (DECL_NAME (parm)));
		  else
		    warning ("parameter has incomplete type");
		  if (funcdef_flag)
		    {
		      TREE_VALUE (typelt) = error_mark_node;
		      TREE_TYPE (parm) = error_mark_node;
		    }
		}
#if 0  /* This has been replaced by parm_tags_warning
	  which uses a more accurate criterion for what to warn about.  */
	      else
		{
		  /* Now warn if is a pointer to an incomplete type.  */
		  while (TREE_CODE (type) == POINTER_TYPE
			 || TREE_CODE (type) == REFERENCE_TYPE)
		    type = TREE_TYPE (type);
		  type = TYPE_MAIN_VARIANT (type);
		  if (TYPE_SIZE (type) == 0)
		    {
		      if (DECL_NAME (parm) != 0)
			warning ("parameter `%s' points to incomplete type",
				 IDENTIFIER_POINTER (DECL_NAME (parm)));
		      else
			warning ("parameter points to incomplete type");
		    }
		}
#endif
	      typelt = TREE_CHAIN (typelt);
	    }

      /* Allocate the list of types the way we allocate a type.  */
      if (first_parm && ! TREE_PERMANENT (first_parm))
	{
	  /* Construct a copy of the list of types
	     on the saveable obstack.  */
	  tree result = NULL;
	  for (typelt = first_parm; typelt; typelt = TREE_CHAIN (typelt))
	    result = saveable_tree_cons (NULL_TREE, TREE_VALUE (typelt),
					 result);
	  return nreverse (result);
	}
      else
	/* The list we have is permanent already.  */
	return first_parm;
    }
}


/* Return a tree_list node with info on a parameter list just parsed.
   The TREE_PURPOSE is a chain of decls of those parms.
   The TREE_VALUE is a list of structure, union and enum tags defined.
   The TREE_CHAIN is a list of argument types to go in the FUNCTION_TYPE.
   This tree_list node is later fed to `grokparms'.

   VOID_AT_END nonzero means append `void' to the end of the type-list.
   Zero means the parmlist ended with an ellipsis so don't append `void'.  */

tree
get_parm_info (void_at_end)
     int void_at_end;
{
  register tree decl, t;
  register tree types = 0;
  int erred = 0;
  tree tags = gettags ();
  tree parms = getdecls ();
  tree new_parms = 0;
  tree order = current_binding_level->parm_order;

  /* Just `void' (and no ellipsis) is special.  There are really no parms.  */
  if (void_at_end && parms != 0
      && TREE_CHAIN (parms) == 0
      && TYPE_MAIN_VARIANT (TREE_TYPE (parms)) == void_type_node
      && DECL_NAME (parms) == 0)
    {
      parms = NULL_TREE;
      storedecls (NULL_TREE);
      return saveable_tree_cons (NULL_TREE, NULL_TREE,
				 saveable_tree_cons (NULL_TREE, void_type_node, NULL_TREE));
    }

  /* Extract enumerator values and other non-parms declared with the parms.
     Likewise any forward parm decls that didn't have real parm decls.  */
  for (decl = parms; decl; )
    {
      tree next = TREE_CHAIN (decl);

      if (TREE_CODE (decl) != PARM_DECL)
	{
	  TREE_CHAIN (decl) = new_parms;
	  new_parms = decl;
	}
      else if (TREE_ASM_WRITTEN (decl))
	{
	  error_with_decl (decl, "parameter `%s' has just a forward declaration");
	  TREE_CHAIN (decl) = new_parms;
	  new_parms = decl;
	}
      decl = next;
    }

  /* Put the parm decls back in the order they were in in the parm list.  */
  for (t = order; t; t = TREE_CHAIN (t))
    {
      if (TREE_CHAIN (t))
	TREE_CHAIN (TREE_VALUE (t)) = TREE_VALUE (TREE_CHAIN (t));
      else
	TREE_CHAIN (TREE_VALUE (t)) = 0;
    }

  new_parms = chainon (order ? nreverse (TREE_VALUE (order)) : 0,
		       new_parms);

  /* Store the parmlist in the binding level since the old one
     is no longer a valid list.  (We have changed the chain pointers.)  */
  storedecls (new_parms);

  for (decl = new_parms; decl; decl = TREE_CHAIN (decl))
    /* There may also be declarations for enumerators if an enumeration
       type is declared among the parms.  Ignore them here.  */
    if (TREE_CODE (decl) == PARM_DECL)
      {
	/* Since there is a prototype,
	   args are passed in their declared types.  */
	tree type = TREE_TYPE (decl);
	DECL_ARG_TYPE (decl) = type;
#ifdef PROMOTE_PROTOTYPES
	if ((TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == ENUMERAL_TYPE)
	    && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (decl) = integer_type_node;
#endif

	types = saveable_tree_cons (NULL_TREE, TREE_TYPE (decl), types);
	if (TYPE_MAIN_VARIANT (TREE_VALUE (types)) == void_type_node && ! erred
	    && DECL_NAME (decl) == 0)
	  {
	    error ("`void' in parameter list must be the entire list");
	    erred = 1;
	  }
      }

  if (void_at_end)
    return saveable_tree_cons (new_parms, tags,
			       nreverse (saveable_tree_cons (NULL_TREE, void_type_node, types)));

  return saveable_tree_cons (new_parms, tags, nreverse (types));
}

/* At end of parameter list, warn about any struct, union or enum tags
   defined within.  Do so because these types cannot ever become complete.  */

void
parmlist_tags_warning ()
{
  tree elt;
  static int already;

  for (elt = current_binding_level->tags; elt; elt = TREE_CHAIN (elt))
    {
      enum tree_code code = TREE_CODE (TREE_VALUE (elt));
      /* An anonymous union parm type is meaningful as a GNU extension.
	 So don't warn for that.  */
      if (code == UNION_TYPE && !pedantic)
	continue;
      if (TREE_PURPOSE (elt) != 0)
	warning ("`%s %s' declared inside parameter list",
		 (code == RECORD_TYPE ? "struct"
		  : code == UNION_TYPE ? "union"
		  : "enum"),
		 IDENTIFIER_POINTER (TREE_PURPOSE (elt)));
      else
	warning ("anonymous %s declared inside parameter list",
		 (code == RECORD_TYPE ? "struct"
		  : code == UNION_TYPE ? "union"
		  : "enum"));

      if (! already)
	{
	  warning ("its scope is only this definition or declaration,");
	  warning ("which is probably not what you want.");
	  already = 1;
	}
    }
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.  */

tree
xref_tag (code, name)
     enum tree_code code;
     tree name;
{
  int temporary = allocation_temporary_p ();

  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */

  register tree ref = lookup_tag (code, name, current_binding_level, 0);
  /* Even if this is the wrong type of tag, return what we found.
     There will be an error message anyway, from pending_xref_error.
     If we create an empty xref just for an invalid use of the type,
     the main result is to create lots of superfluous error messages.  */
  if (ref)
    return ref;

  push_obstacks_nochange ();

  if (current_binding_level == global_binding_level && temporary)
    end_temporary_allocation ();

  /* If no such tag is yet defined, create a forward-reference node
     and record it as the "definition".
     When a real declaration of this type is found,
     the forward-reference will be altered into a real type.  */

  ref = make_node (code);
  if (code == ENUMERAL_TYPE)
    {
      /* (In ANSI, Enums can be referred to only if already defined.)  */
      if (pedantic)
	pedwarn ("ANSI C forbids forward references to `enum' types");
      /* Give the type a default layout like unsigned int
	 to avoid crashing if it does not get defined.  */
      TYPE_MODE (ref) = TYPE_MODE (unsigned_type_node);
      TYPE_ALIGN (ref) = TYPE_ALIGN (unsigned_type_node);
      TREE_UNSIGNED (ref) = 1;
      TYPE_PRECISION (ref) = TYPE_PRECISION (unsigned_type_node);
      TYPE_MIN_VALUE (ref) = TYPE_MIN_VALUE (unsigned_type_node);
      TYPE_MAX_VALUE (ref) = TYPE_MAX_VALUE (unsigned_type_node);
    }

  pushtag (name, ref);

  pop_obstacks ();

  return ref;
}

/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.

   We also do a push_obstacks_nochange
   whose matching pop is in finish_struct.  */

tree
start_struct (code, name)
     enum tree_code code;
     tree name;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  register tree ref = 0;

  push_obstacks_nochange ();
  if (current_binding_level == global_binding_level)
    end_temporary_allocation ();

  if (name != 0)
    ref = lookup_tag (code, name, current_binding_level, 1);
  if (ref && TREE_CODE (ref) == code)
    {
      C_TYPE_BEING_DEFINED (ref) = 1;
      if (TYPE_FIELDS (ref))
	error ((code == UNION_TYPE ? "redefinition of `union %s'"
		: "redefinition of `struct %s'"),
	       IDENTIFIER_POINTER (name));

      return ref;
    }

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  ref = make_node (code);
  pushtag (name, ref);
  C_TYPE_BEING_DEFINED (ref) = 1;
  return ref;
}

/* Process the specs, declarator (NULL if omitted) and width (NULL if omitted)
   of a structure component, returning a FIELD_DECL node.
   WIDTH is non-NULL for bit fields only, and is an INTEGER_CST node.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
grokfield (filename, line, declarator, declspecs, width)
     char *filename;
     int line;
     tree declarator, declspecs, width;
{
  tree value;

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();

  value = grokdeclarator (declarator, declspecs, width ? BITFIELD : FIELD, 0);

  finish_decl (value, NULL_TREE, NULL_TREE);
  DECL_INITIAL (value) = width;

  maybe_objc_check_decl (value);
  return value;
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (x, y)
     tree *x, *y;
{
  if (DECL_NAME (*x) == DECL_NAME (*y))
    return 0;
  if (DECL_NAME (*x) == NULL)
    return -1;
  if (DECL_NAME (*y) == NULL)
    return 1;
  if (DECL_NAME (*x) < DECL_NAME (*y))
    return -1;
  return 1;
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.

   We also do a pop_obstacks to match the push in start_struct.  */

tree
finish_struct (t, fieldlist, attributes)
     tree t;
     tree fieldlist;
     tree attributes;
{
  register tree x;
  int old_momentary;
  int toplevel = global_binding_level == current_binding_level;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;

  decl_attributes (t, attributes, NULL_TREE);

  /* Nameless union parm types are useful as GCC extension.  */
  if (! (TREE_CODE (t) == UNION_TYPE && TYPE_NAME (t) == 0) && !pedantic)
    /* Otherwise, warn about any struct or union def. in parmlist.  */
    if (in_parm_level_p ())
      {
	if (pedantic)
	  pedwarn ((TREE_CODE (t) == UNION_TYPE ? "union defined inside parms"
		    : "structure defined inside parms"));
	else if (! flag_traditional)
	  warning ((TREE_CODE (t) == UNION_TYPE ? "union defined inside parms"
		    : "structure defined inside parms"));
      }

  old_momentary = suspend_momentary ();

  if (fieldlist == 0 && pedantic)
    pedwarn ((TREE_CODE (t) == UNION_TYPE ? "union has no members"
	      : "structure has no members"));

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes.
     Set DECL_FIELD_SIZE to the specified size, or 0 if none specified.
     The specified size is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;
      DECL_PACKED (x) |= TYPE_PACKED (t);
      DECL_FIELD_SIZE (x) = 0;

      /* If any field is const, the structure type is pseudo-const.  */
      if (TREE_READONLY (x))
	C_TYPE_FIELDS_READONLY (t) = 1;
      else
	{
	  /* A field that is pseudo-const makes the structure likewise.  */
	  tree t1 = TREE_TYPE (x);
	  while (TREE_CODE (t1) == ARRAY_TYPE)
	    t1 = TREE_TYPE (t1);
	  if ((TREE_CODE (t1) == RECORD_TYPE || TREE_CODE (t1) == UNION_TYPE)
	      && C_TYPE_FIELDS_READONLY (t1))
	    C_TYPE_FIELDS_READONLY (t) = 1;
	}

      /* Any field that is volatile means variables of this type must be
	 treated in some ways as volatile.  */
      if (TREE_THIS_VOLATILE (x))
	C_TYPE_FIELDS_VOLATILE (t) = 1;

      /* Any field of nominal variable size implies structure is too.  */
      if (C_DECL_VARIABLE_SIZE (x))
	C_TYPE_VARIABLE_SIZE (t) = 1;

      /* Detect invalid nested redefinition.  */
      if (TREE_TYPE (x) == t)
	error ("nested redefinition of `%s'",
	       IDENTIFIER_POINTER (TYPE_NAME (t)));

      /* Detect invalid bit-field size.  */
      if (DECL_INITIAL (x))
	STRIP_NOPS (DECL_INITIAL (x));
      if (DECL_INITIAL (x))
	{
	  if (TREE_CODE (DECL_INITIAL (x)) == INTEGER_CST)
	    constant_expression_warning (DECL_INITIAL (x));
	  else
	    {
	      error_with_decl (x, "bit-field `%s' width not an integer constant");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Detect invalid bit-field type.  */
      if (DECL_INITIAL (x)
	  && TREE_CODE (TREE_TYPE (x)) != INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
	{
	  error_with_decl (x, "bit-field `%s' has invalid type");
	  DECL_INITIAL (x) = NULL;
	}
      if (DECL_INITIAL (x) && pedantic
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != integer_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != unsigned_type_node
	  /* Accept an enum that's equivalent to int or unsigned int.  */
	  && !(TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
	       && (TYPE_PRECISION (TREE_TYPE (x))
		   == TYPE_PRECISION (integer_type_node))))
	pedwarn_with_decl (x, "bit-field `%s' type invalid in ANSI C");

      /* Detect and ignore out of range field width.  */
      if (DECL_INITIAL (x))
	{
	  unsigned HOST_WIDE_INT width = TREE_INT_CST_LOW (DECL_INITIAL (x));

	  if (tree_int_cst_sgn (DECL_INITIAL (x)) < 0)
	    {
	      DECL_INITIAL (x) = NULL;
	      error_with_decl (x, "negative width in bit-field `%s'");
	    }
	  else if (TREE_INT_CST_HIGH (DECL_INITIAL (x)) != 0
		   || width > TYPE_PRECISION (TREE_TYPE (x)))
	    {
	      DECL_INITIAL (x) = NULL;
	      pedwarn_with_decl (x, "width of `%s' exceeds its type");
	    }
	  else if (width == 0 && DECL_NAME (x) != 0)
	    {
	      error_with_decl (x, "zero width for bit-field `%s'");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Process valid field width.  */
      if (DECL_INITIAL (x))
	{
	  register int width = TREE_INT_CST_LOW (DECL_INITIAL (x));

	  DECL_FIELD_SIZE (x) = width;
	  DECL_BIT_FIELD (x) = 1;
	  DECL_INITIAL (x) = NULL;

	  if (width == 0)
	    {
	      /* field size 0 => force desired amount of alignment.  */
#ifdef EMPTY_FIELD_BOUNDARY
	      DECL_ALIGN (x) = MAX (DECL_ALIGN (x), EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
	      DECL_ALIGN (x) = MAX (DECL_ALIGN (x),
				    TYPE_ALIGN (TREE_TYPE (x)));
#endif
	    }
	}
      else if (TREE_TYPE (x) != error_mark_node)
	{
	  int min_align = (DECL_PACKED (x) ? BITS_PER_UNIT
			   : TYPE_ALIGN (TREE_TYPE (x)));
	  /* Non-bit-fields are aligned for their type, except packed
	     fields which require only BITS_PER_UNIT alignment.  */
	  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), min_align);
	}
    }

  /* Now DECL_INITIAL is null on all members.  */

  /* Delete all duplicate fields from the fieldlist */
  for (x = fieldlist; x && TREE_CHAIN (x);)
    /* Anonymous fields aren't duplicates.  */
    if (DECL_NAME (TREE_CHAIN (x)) == 0)
      x = TREE_CHAIN (x);
    else
      {
	register tree y = fieldlist;
	  
	while (1)
	  {
	    if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	      break;
	    if (y == x)
	      break;
	    y = TREE_CHAIN (y);
	  }
	if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	  {
	    error_with_decl (TREE_CHAIN (x), "duplicate member `%s'");
	    TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
	  }
	else x = TREE_CHAIN (x);
      }

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Delete all zero-width bit-fields from the front of the fieldlist */
  while (fieldlist
	 && DECL_INITIAL (fieldlist))
    fieldlist = TREE_CHAIN (fieldlist);
  /* Delete all such members from the rest of the fieldlist */
  for (x = fieldlist; x;)
    {
      if (TREE_CHAIN (x) && DECL_INITIAL (TREE_CHAIN (x)))
	TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
      else x = TREE_CHAIN (x);
    }

  /*  Now we have the truly final field list.
      Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */
  {
    int len = 0;

    for (x = fieldlist; x; x = TREE_CHAIN (x))
      {
	if (len > 15)
	  break;
	len += 1;
      }
    if (len > 15)
      {
	tree *field_array;
	char *space;

	len += list_length (x);
	/* Use the same allocation policy here that make_node uses, to
	   ensure that this lives as long as the rest of the struct decl.
	   All decls in an inline function need to be saved.  */
	if (allocation_temporary_p ())
	  space = savealloc (sizeof (struct lang_type) + len * sizeof (tree));
	else
	  space = oballoc (sizeof (struct lang_type) + len * sizeof (tree));

	TYPE_LANG_SPECIFIC (t) = (struct lang_type *) space;
	TYPE_LANG_SPECIFIC (t)->len = len;

	field_array = &TYPE_LANG_SPECIFIC (t)->elts[0];
	len = 0;
	for (x = fieldlist; x; x = TREE_CHAIN (x))
	  field_array[len++] = x;

	qsort (field_array, len, sizeof (tree), field_decl_cmp);
      }
  }

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
    }

  /* Promote each bit-field's type to int if it is narrower than that.  */
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    if (DECL_BIT_FIELD (x)
	&& (C_PROMOTING_INTEGER_TYPE_P (TREE_TYPE (x))
	    || DECL_FIELD_SIZE (x) < TYPE_PRECISION (integer_type_node)))
      {
	tree type = TREE_TYPE (x);

	/* Preserve unsignedness if traditional
	   or if not really getting any wider.  */
	if (TREE_UNSIGNED (type)
	    && (flag_traditional
		||
		(TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)
		 &&
		 DECL_FIELD_SIZE (x) == TYPE_PRECISION (integer_type_node))))
	  TREE_TYPE (x) = unsigned_type_node;
	else
	  TREE_TYPE (x) = integer_type_node;
      }

  /* If this was supposed to be a transparent union, but we can't
     make it one, warn and turn off the flag.  */
  if (TREE_CODE (t) == UNION_TYPE
      && TYPE_TRANSPARENT_UNION (t)
      && TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t)))
    {
      TYPE_TRANSPARENT_UNION (t) = 0;
      warning ("cannot make `%s' a transparent union");
    }

  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */

  if (current_binding_level->n_incomplete != 0)
    {
      tree decl;
      for (decl = current_binding_level->names; decl; decl = TREE_CHAIN (decl))
	{
	  if (TREE_TYPE (decl) == t
	      && TREE_CODE (decl) != TYPE_DECL)
	    {
	      layout_decl (decl, 0);
	      /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
	      maybe_objc_check_decl (decl);
	      rest_of_decl_compilation (decl, NULL_PTR, toplevel, 0);
	      if (! toplevel)
		expand_decl (decl);
	      --current_binding_level->n_incomplete;
	    }
	  else if (TYPE_SIZE (TREE_TYPE (decl)) == 0
		   && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    {
	      tree element = TREE_TYPE (decl);
	      while (TREE_CODE (element) == ARRAY_TYPE)
		element = TREE_TYPE (element);
	      if (element == t)
		layout_array_type (TREE_TYPE (decl));
	    }
	}
    }

  resume_momentary (old_momentary);

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  /* The matching push is in start_struct.  */
  pop_obstacks ();

  return t;
}

/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (t)
     tree t;
{
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
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
  register tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, current_binding_level, 1);

  /* The corresponding pop_obstacks is in finish_enum.  */
  push_obstacks_nochange ();
  /* If these symbols and types are global, make them permanent.  */
  if (current_binding_level == global_binding_level)
    end_temporary_allocation ();

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }

  C_TYPE_BEING_DEFINED (enumtype) = 1;

  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of `enum %s'", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  enum_next_value = integer_zero_node;
  enum_overflow = 0;

  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object, VALUES a list of decl-value pairs,
   and ATTRIBUTES are the specified attributes.
   Returns ENUMTYPE.  */

tree
finish_enum (enumtype, values, attributes)
     tree enumtype;
     tree values;
     tree attributes;
{
  register tree pair, tem;
  tree minnode = 0, maxnode = 0;
  int lowprec, highprec, precision;
  int toplevel = global_binding_level == current_binding_level;

  if (in_parm_level_p ())
    warning ("enum defined inside parms");

  decl_attributes (enumtype, attributes, NULL_TREE);

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    for (pair = values; pair; pair = TREE_CHAIN (pair))
      {
	tree value = TREE_VALUE (pair);
	if (pair == values)
	  minnode = maxnode = TREE_VALUE (pair);
	else
	  {
	    if (tree_int_cst_lt (maxnode, value))
	      maxnode = value;
	    if (tree_int_cst_lt (value, minnode))
	      minnode = value;
	  }
      }

  TYPE_MIN_VALUE (enumtype) = minnode;
  TYPE_MAX_VALUE (enumtype) = maxnode;

  /* An enum can have some negative values; then it is signed.  */
  TREE_UNSIGNED (enumtype) = tree_int_cst_sgn (minnode) >= 0;

  /* Determine the precision this type needs.  */

  lowprec = min_precision (minnode, TREE_UNSIGNED (enumtype));
  highprec = min_precision (maxnode, TREE_UNSIGNED (enumtype));
  precision = MAX (lowprec, highprec);

  if (flag_short_enums || TYPE_PACKED (enumtype)
      || precision > TYPE_PRECISION (integer_type_node))
    /* Use the width of the narrowest normal C type which is wide enough.  */
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (type_for_size (precision, 1));
  else
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (integer_type_node);

  TYPE_SIZE (enumtype) = 0;
  layout_type (enumtype);

  if (values != error_mark_node)
    {
      /* Change the type of the enumerators to be the enum type.
	 Formerly this was done only for enums that fit in an int,
	 but the comment said it was done only for enums wider than int.
	 It seems necessary to do this for wide enums,
	 and best not to change what's done for ordinary narrower ones.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	{
	  TREE_TYPE (TREE_PURPOSE (pair)) = enumtype;
	  DECL_SIZE (TREE_PURPOSE (pair)) = TYPE_SIZE (enumtype);
	  if (TREE_CODE (TREE_PURPOSE (pair)) != FUNCTION_DECL)
	    DECL_ALIGN (TREE_PURPOSE (pair)) = TYPE_ALIGN (enumtype);
	}

      /* Replace the decl nodes in VALUES with their names.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	TREE_PURPOSE (pair) = DECL_NAME (TREE_PURPOSE (pair));

      TYPE_VALUES (enumtype) = values;
    }

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_MODE (tem) = TYPE_MODE (enumtype);
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
      TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, toplevel);

  /* This matches a push in start_enum.  */
  pop_obstacks ();

  return enumtype;
}

/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   Return a tree-list containing the CONST_DECL and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (name, value)
     tree name, value;
{
  register tree decl, type;

  /* Validate and default VALUE.  */

  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

  if (value != 0)
    {
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  value = default_conversion (value);
	  constant_expression_warning (value);
	}
      else
	{
	  error ("enumerator value for `%s' not integer constant",
		 IDENTIFIER_POINTER (name));
	  value = 0;
	}
    }

  /* Default based on previous value.  */
  /* It should no longer be possible to have NON_LVALUE_EXPR
     in the default.  */
  if (value == 0)
    {
      value = enum_next_value;
      if (enum_overflow)
	error ("overflow in enumeration values");
    }

  if (pedantic && ! int_fits_type_p (value, integer_type_node))
    {
      pedwarn ("ANSI C restricts enumerator values to range of `int'");
      value = integer_zero_node;
    }

  /* Set basis for default for next value.  */
  enum_next_value = build_binary_op (PLUS_EXPR, value, integer_one_node, 0);
  enum_overflow = tree_int_cst_lt (enum_next_value, value);

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);
  type = type_for_size (MAX (TYPE_PRECISION (type),
			     TYPE_PRECISION (integer_type_node)),
			((flag_traditional
			  || TYPE_PRECISION (type) >= TYPE_PRECISION (integer_type_node))
			 && TREE_UNSIGNED (type)));

  decl = build_decl (CONST_DECL, name, type);
  DECL_INITIAL (decl) = value;
  TREE_TYPE (value) = type;
  pushdecl (decl);

  return saveable_tree_cons (decl, value, NULL_TREE);
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS, DECLARATOR, PREFIX_ATTRIBUTES and ATTRIBUTES are the parts of
   the declaration; they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.

   NESTED is nonzero for a function nested within another function.  */

int
start_function (declspecs, declarator, prefix_attributes, attributes, nested)
     tree declarator, declspecs, prefix_attributes, attributes;
     int nested;
{
  tree decl1, old_decl;
  tree restype;
  int old_immediate_size_expand = immediate_size_expand;

  current_function_returns_value = 0;  /* Assume, until we see it does. */
  current_function_returns_null = 0;
  warn_about_return_type = 0;
  current_extern_inline = 0;
  c_function_varargs = 0;
  named_labels = 0;
  shadowed_labels = 0;

  /* Don't expand any sizes in the return type of the function.  */
  immediate_size_expand = 0;

  decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1);

  /* If the declarator is not suitable for a function definition,
     cause a syntax error.  */
  if (decl1 == 0)
    return 0;

  decl_attributes (decl1, prefix_attributes, attributes);

  announce_function (decl1);

  if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl1))) == 0)
    {
      error ("return-type is an incomplete type");
      /* Make it return void instead.  */
      TREE_TYPE (decl1)
	= build_function_type (void_type_node,
			       TYPE_ARG_TYPES (TREE_TYPE (decl1)));
    }

  if (warn_about_return_type)
    warning ("return-type defaults to `int'");

  /* Save the parm names or decls from this function's declarator
     where store_parm_decls will find them.  */
  current_function_parms = last_function_parms;
  current_function_parm_tags = last_function_parm_tags;

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  DECL_INITIAL (decl1) = error_mark_node;

  /* If this definition isn't a prototype and we had a prototype declaration
     before, copy the arg type info from that prototype.
     But not if what we had before was a builtin function.  */
  old_decl = lookup_name_current_level (DECL_NAME (decl1));
  if (old_decl != 0 && TREE_CODE (TREE_TYPE (old_decl)) == FUNCTION_TYPE
      && !DECL_BUILT_IN (old_decl)
      && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl1)))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (old_decl))))
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0)
    {
      TREE_TYPE (decl1) = TREE_TYPE (old_decl);
      current_function_prototype_file = DECL_SOURCE_FILE (old_decl);
      current_function_prototype_line = DECL_SOURCE_LINE (old_decl);
    }

  /* If there is no explicit declaration, look for any out-of-scope implicit
     declarations.  */
  if (old_decl == 0)
    old_decl = IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1));

  /* Optionally warn of old-fashioned def with no previous prototype.  */
  if (warn_strict_prototypes
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0
      && !(old_decl != 0 && TYPE_ARG_TYPES (TREE_TYPE (old_decl)) != 0))
    warning ("function declaration isn't a prototype");
  /* Optionally warn of any global def with no previous prototype.  */
  else if (warn_missing_prototypes
	   && TREE_PUBLIC (decl1)
	   && !(old_decl != 0 && TYPE_ARG_TYPES (TREE_TYPE (old_decl)) != 0)
	   && strcmp ("main", IDENTIFIER_POINTER (DECL_NAME (decl1))))
    warning_with_decl (decl1, "no previous prototype for `%s'");
  /* Optionally warn of any def with no previous prototype
     if the function has already been used.  */
  else if (warn_missing_prototypes
	   && old_decl != 0 && TREE_USED (old_decl)
	   && TYPE_ARG_TYPES (TREE_TYPE (old_decl)) == 0)
    warning_with_decl (decl1,
		      "`%s' was used with no prototype before its definition");
  /* Optionally warn of any global def with no previous declaration.  */
  else if (warn_missing_declarations
	   && TREE_PUBLIC (decl1)
	   && old_decl == 0
	   && strcmp ("main", IDENTIFIER_POINTER (DECL_NAME (decl1))))
    warning_with_decl (decl1, "no previous declaration for `%s'");
  /* Optionally warn of any def with no previous declaration
     if the function has already been used.  */
  else if (warn_missing_declarations
	   && old_decl != 0 && TREE_USED (old_decl)
	   && old_decl == IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)))
    warning_with_decl (decl1,
		    "`%s' was used with no declaration before its definition");

  /* This is a definition, not a reference.
     So normally clear DECL_EXTERNAL.
     However, `extern inline' acts like a declaration
     except for defining how to inline.  So set DECL_EXTERNAL in that case.  */
  DECL_EXTERNAL (decl1) = current_extern_inline;

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* A nested function is not global.  */
  if (current_function_decl != 0)
    TREE_PUBLIC (decl1) = 0;

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */

  current_function_decl = pushdecl (decl1);

  pushlevel (0);
  declare_parm_level (1);
  current_binding_level->subblocks_tag_transparent = 1;

  make_function_rtl (current_function_decl);

  restype = TREE_TYPE (TREE_TYPE (current_function_decl));
  /* Promote the value to int before returning it.  */
  if (C_PROMOTING_INTEGER_TYPE_P (restype))
    {
      /* It retains unsignedness if traditional
	 or if not really getting wider.  */
      if (TREE_UNSIGNED (restype)
	  && (flag_traditional
	      || (TYPE_PRECISION (restype)
		  == TYPE_PRECISION (integer_type_node))))
	restype = unsigned_type_node;
      else
	restype = integer_type_node;
    }
  DECL_RESULT (current_function_decl)
    = build_decl (RESULT_DECL, NULL_TREE, restype);

  if (!nested)
    /* Allocate further tree nodes temporarily during compilation
       of this function only.  */
    temporary_allocation ();

  /* If this fcn was already referenced via a block-scope `extern' decl
     (or an implicit decl), propagate certain information about the usage.  */
  if (TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (current_function_decl)))
    TREE_ADDRESSABLE (current_function_decl) = 1;

  immediate_size_expand = old_immediate_size_expand;

  return 1;
}

/* Record that this function is going to be a varargs function.
   This is called before store_parm_decls, which is too early
   to call mark_varargs directly.  */

void
c_mark_varargs ()
{
  c_function_varargs = 1;
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   For an old-style definition, modify the function's type
   to specify at least the number of arguments.  */

void
store_parm_decls ()
{
  register tree fndecl = current_function_decl;
  register tree parm;

  /* This is either a chain of PARM_DECLs (if a prototype was used)
     or a list of IDENTIFIER_NODEs (for an old-fashioned C definition).  */
  tree specparms = current_function_parms;

  /* This is a list of types declared among parms in a prototype.  */
  tree parmtags = current_function_parm_tags;

  /* This is a chain of PARM_DECLs from old-style parm declarations.  */
  register tree parmdecls = getdecls ();

  /* This is a chain of any other decls that came in among the parm
     declarations.  If a parm is declared with  enum {foo, bar} x;
     then CONST_DECLs for foo and bar are put here.  */
  tree nonparms = 0;

  /* Nonzero if this definition is written with a prototype.  */
  int prototype = 0;

  if (specparms != 0 && TREE_CODE (specparms) != TREE_LIST)
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */

      register tree next;
      tree others = 0;

      prototype = 1;

      if (parmdecls != 0)
	{
	  tree decl, link;

	  error_with_decl (fndecl,
			   "parm types given both in parmlist and separately");
	  /* Get rid of the erroneous decls; don't keep them on
	     the list of parms, since they might not be PARM_DECLs.  */
	  for (decl = current_binding_level->names;
	       decl; decl = TREE_CHAIN (decl))
	    if (DECL_NAME (decl))
	      IDENTIFIER_LOCAL_VALUE (DECL_NAME (decl)) = 0;
	  for (link = current_binding_level->shadowed;
	       link; link = TREE_CHAIN (link))
	    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);
	  current_binding_level->names = 0;
	  current_binding_level->shadowed = 0;
	}

      specparms = nreverse (specparms);
      for (parm = specparms; parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      if (DECL_NAME (parm) == 0)
		error_with_decl (parm, "parameter name omitted");
	      else if (TYPE_MAIN_VARIANT (TREE_TYPE (parm)) == void_type_node)
		{
		  error_with_decl (parm, "parameter `%s' declared void");
		  /* Change the type to error_mark_node so this parameter
		     will be ignored by assign_parms.  */
		  TREE_TYPE (parm) = error_mark_node;
		}
	      pushdecl (parm);
	    }
	  else
	    {
	      /* If we find an enum constant or a type tag,
		 put it aside for the moment.  */
	      TREE_CHAIN (parm) = 0;
	      others = chainon (others, parm);
	    }
	}

      /* Get the decls in their original chain order
	 and record in the function.  */
      DECL_ARGUMENTS (fndecl) = getdecls ();

#if 0
      /* If this function takes a variable number of arguments,
	 add a phony parameter to the end of the parm list,
	 to represent the position of the first unnamed argument.  */
      if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (fndecl))))
	  != void_type_node)
	{
	  tree dummy = build_decl (PARM_DECL, NULL_TREE, void_type_node);
	  /* Let's hope the address of the unnamed parm
	     won't depend on its type.  */
	  TREE_TYPE (dummy) = integer_type_node;
	  DECL_ARG_TYPE (dummy) = integer_type_node;
	  DECL_ARGUMENTS (fndecl)
	    = chainon (DECL_ARGUMENTS (fndecl), dummy);
	}
#endif

      /* Now pushdecl the enum constants.  */
      for (parm = others; parm; parm = next)
	{
	  next = TREE_CHAIN (parm);
	  if (DECL_NAME (parm) == 0)
	    ;
	  else if (TYPE_MAIN_VARIANT (TREE_TYPE (parm)) == void_type_node)
	    ;
	  else if (TREE_CODE (parm) != PARM_DECL)
	    pushdecl (parm);
	}

      storetags (chainon (parmtags, gettags ()));
    }
  else
    {
      /* SPECPARMS is an identifier list--a chain of TREE_LIST nodes
	 each with a parm name as the TREE_VALUE.

	 PARMDECLS is a chain of declarations for parameters.
	 Warning! It can also contain CONST_DECLs which are not parameters
	 but are names of enumerators of any enum types
	 declared among the parameters.

	 First match each formal parameter name with its declaration.
	 Associate decls with the names and store the decls
	 into the TREE_PURPOSE slots.  */

      for (parm = parmdecls; parm; parm = TREE_CHAIN (parm))
	DECL_RESULT (parm) = 0;

      for (parm = specparms; parm; parm = TREE_CHAIN (parm))
	{
	  register tree tail, found = NULL;

	  if (TREE_VALUE (parm) == 0)
	    {
	      error_with_decl (fndecl, "parameter name missing from parameter list");
	      TREE_PURPOSE (parm) = 0;
	      continue;
	    }

	  /* See if any of the parmdecls specifies this parm by name.
	     Ignore any enumerator decls.  */
	  for (tail = parmdecls; tail; tail = TREE_CHAIN (tail))
	    if (DECL_NAME (tail) == TREE_VALUE (parm)
		&& TREE_CODE (tail) == PARM_DECL)
	      {
		found = tail;
		break;
	      }

	  /* If declaration already marked, we have a duplicate name.
	     Complain, and don't use this decl twice.   */
	  if (found && DECL_RESULT (found) != 0)
	    {
	      error_with_decl (found, "multiple parameters named `%s'");
	      found = 0;
	    }

	  /* If the declaration says "void", complain and ignore it.  */
	  if (found && TYPE_MAIN_VARIANT (TREE_TYPE (found)) == void_type_node)
	    {
	      error_with_decl (found, "parameter `%s' declared void");
	      TREE_TYPE (found) = integer_type_node;
	      DECL_ARG_TYPE (found) = integer_type_node;
	      layout_decl (found, 0);
	    }

	  /* Traditionally, a parm declared float is actually a double.  */
	  if (found && flag_traditional
	      && TYPE_MAIN_VARIANT (TREE_TYPE (found)) == float_type_node)
	    {
	      TREE_TYPE (found) = double_type_node;
	      DECL_ARG_TYPE (found) = double_type_node;
	      layout_decl (found, 0);
	    }

	  /* If no declaration found, default to int.  */
	  if (!found)
	    {
	      found = build_decl (PARM_DECL, TREE_VALUE (parm),
				  integer_type_node);
	      DECL_ARG_TYPE (found) = TREE_TYPE (found);
	      DECL_SOURCE_LINE (found) = DECL_SOURCE_LINE (fndecl);
	      DECL_SOURCE_FILE (found) = DECL_SOURCE_FILE (fndecl);
	      if (extra_warnings)
		warning_with_decl (found, "type of `%s' defaults to `int'");
	      pushdecl (found);
	    }

	  TREE_PURPOSE (parm) = found;

	  /* Mark this decl as "already found" -- see test, above.
	     It is safe to use DECL_RESULT for this
	     since it is not used in PARM_DECLs or CONST_DECLs.  */
	  DECL_RESULT (found) = error_mark_node;
	}

      /* Put anything which is on the parmdecls chain and which is
	 not a PARM_DECL onto the list NONPARMS.  (The types of
	 non-parm things which might appear on the list include
	 enumerators and NULL-named TYPE_DECL nodes.) Complain about
	 any actual PARM_DECLs not matched with any names.  */

      nonparms = 0;
      for (parm = parmdecls; parm; )
	{
	  tree next = TREE_CHAIN (parm);
	  TREE_CHAIN (parm) = 0;

	  if (TREE_CODE (parm) != PARM_DECL)
	    nonparms = chainon (nonparms, parm);
	  else
	    {
	      /* Complain about args with incomplete types.  */
	      if (TYPE_SIZE (TREE_TYPE (parm)) == 0)
	        {
	          error_with_decl (parm, "parameter `%s' has incomplete type");
	          TREE_TYPE (parm) = error_mark_node;
	        }

	      if (DECL_RESULT (parm) == 0)
	        {
	          error_with_decl (parm,
			           "declaration for parameter `%s' but no such parameter");
	          /* Pretend the parameter was not missing.
		     This gets us to a standard state and minimizes
		     further error messages.  */
	          specparms
		    = chainon (specparms,
			       tree_cons (parm, NULL_TREE, NULL_TREE));
		}
	    }

	  parm = next;
	}

      /* Chain the declarations together in the order of the list of names.  */
      /* Store that chain in the function decl, replacing the list of names.  */
      parm = specparms;
      DECL_ARGUMENTS (fndecl) = 0;
      {
	register tree last;
	for (last = 0; parm; parm = TREE_CHAIN (parm))
	  if (TREE_PURPOSE (parm))
	    {
	      if (last == 0)
		DECL_ARGUMENTS (fndecl) = TREE_PURPOSE (parm);
	      else
		TREE_CHAIN (last) = TREE_PURPOSE (parm);
	      last = TREE_PURPOSE (parm);
	      TREE_CHAIN (last) = 0;
	    }
      }

      /* If there was a previous prototype,
	 set the DECL_ARG_TYPE of each argument according to
	 the type previously specified, and report any mismatches.  */

      if (TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
	{
	  register tree type;
	  for (parm = DECL_ARGUMENTS (fndecl),
	       type = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
	       parm || (type && (TYPE_MAIN_VARIANT (TREE_VALUE (type))
				 != void_type_node));
	       parm = TREE_CHAIN (parm), type = TREE_CHAIN (type))
	    {
	      if (parm == 0 || type == 0
		  || TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  error ("number of arguments doesn't match prototype");
		  error_with_file_and_line (current_function_prototype_file,
					    current_function_prototype_line,
					    "prototype declaration");
		  break;
		}
	      /* Type for passing arg must be consistent
		 with that declared for the arg.  */
	      if (! comptypes (DECL_ARG_TYPE (parm), TREE_VALUE (type)))
		{
		  if (TYPE_MAIN_VARIANT (TREE_TYPE (parm))
		      == TYPE_MAIN_VARIANT (TREE_VALUE (type)))
		    {
		      /* Adjust argument to match prototype.  E.g. a previous
			 `int foo(float);' prototype causes
			 `int foo(x) float x; {...}' to be treated like
			 `int foo(float x) {...}'.  This is particularly
			 useful for argument types like uid_t.  */
		      DECL_ARG_TYPE (parm) = TREE_TYPE (parm);
#ifdef PROMOTE_PROTOTYPES
		      if ((TREE_CODE (TREE_TYPE (parm)) == INTEGER_TYPE
			   || TREE_CODE (TREE_TYPE (parm)) == ENUMERAL_TYPE)
			  && TYPE_PRECISION (TREE_TYPE (parm))
			  < TYPE_PRECISION (integer_type_node))
			DECL_ARG_TYPE (parm) = integer_type_node;
#endif
		      if (pedantic)
			{
			  pedwarn ("promoted argument `%s' doesn't match prototype",
				   IDENTIFIER_POINTER (DECL_NAME (parm)));
			  warning_with_file_and_line
			    (current_function_prototype_file,
			     current_function_prototype_line,
			     "prototype declaration");
			}
		    }
		  /* If -traditional, allow `int' argument to match
		     `unsigned' prototype.  */
		  else if (! (flag_traditional
			      && TYPE_MAIN_VARIANT (TREE_TYPE (parm)) == integer_type_node
			      && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == unsigned_type_node))
		    {
		      error ("argument `%s' doesn't match prototype",
			     IDENTIFIER_POINTER (DECL_NAME (parm)));
		      error_with_file_and_line (current_function_prototype_file,
						current_function_prototype_line,
						"prototype declaration");
		    }
		}
	    }
	  TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = 0;
	}

      /* Otherwise, create a prototype that would match.  */

      else
	{
	  tree actual = 0, last = 0, type;

	  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
	    {
	      type = perm_tree_cons (NULL_TREE, DECL_ARG_TYPE (parm),
				     NULL_TREE);
	      if (last)
		TREE_CHAIN (last) = type;
	      else
		actual = type;
	      last = type;
	    }
	  type = perm_tree_cons (NULL_TREE, void_type_node, NULL_TREE);
	  if (last)
	    TREE_CHAIN (last) = type;
	  else
	    actual = type;

	  /* We are going to assign a new value for the TYPE_ACTUAL_ARG_TYPES
	     of the type of this function, but we need to avoid having this
	     affect the types of other similarly-typed functions, so we must
	     first force the generation of an identical (but separate) type
	     node for the relevant function type.  The new node we create
	     will be a variant of the main variant of the original function
	     type.  */

	  TREE_TYPE (fndecl) = build_type_copy (TREE_TYPE (fndecl));

	  TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = actual;
	}

      /* Now store the final chain of decls for the arguments
	 as the decl-chain of the current lexical scope.
	 Put the enumerators in as well, at the front so that
	 DECL_ARGUMENTS is not modified.  */

      storedecls (chainon (nonparms, DECL_ARGUMENTS (fndecl)));
    }

  /* Make sure the binding level for the top of the function body
     gets a BLOCK if there are any in the function.
     Otherwise, the dbx output is wrong.  */

  keep_next_if_subblocks = 1;

  /* ??? This might be an improvement,
     but needs to be thought about some more.  */
#if 0
  keep_next_level_flag = 1;
#endif

  /* Write a record describing this function definition to the prototypes
     file (if requested).  */

  gen_aux_info_record (fndecl, 1, 0, prototype);

  /* Initialize the RTL code for the function.  */

  init_function_start (fndecl, input_filename, lineno);

  /* If this is a varargs function, inform function.c.  */

  if (c_function_varargs)
    mark_varargs ();

  /* Declare __FUNCTION__ and __PRETTY_FUNCTION__ for this function.  */

  declare_function_name ();

  /* Set up parameters and prepare for return, for the function.  */

  expand_function_start (fndecl, 0);

  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (DECL_NAME (fndecl)
      && strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "main") == 0
      && DECL_CONTEXT (fndecl) == NULL_TREE)
    expand_main_function ();
}

/* SPECPARMS is an identifier list--a chain of TREE_LIST nodes
   each with a parm name as the TREE_VALUE.  A null pointer as TREE_VALUE
   stands for an ellipsis in the identifier list.

   PARMLIST is the data returned by get_parm_info for the
   parmlist that follows the semicolon.

   We return a value of the same sort that get_parm_info returns,
   except that it describes the combination of identifiers and parmlist.  */

tree
combine_parm_decls (specparms, parmlist, void_at_end)
     tree specparms, parmlist;
     int void_at_end;
{
  register tree fndecl = current_function_decl;
  register tree parm;

  tree parmdecls = TREE_PURPOSE (parmlist);

  /* This is a chain of any other decls that came in among the parm
     declarations.  They were separated already by get_parm_info,
     so we just need to keep them separate.  */
  tree nonparms = TREE_VALUE (parmlist);

  tree types = 0;

  for (parm = parmdecls; parm; parm = TREE_CHAIN (parm))
    DECL_RESULT (parm) = 0;

  for (parm = specparms; parm; parm = TREE_CHAIN (parm))
    {
      register tree tail, found = NULL;

      /* See if any of the parmdecls specifies this parm by name.  */
      for (tail = parmdecls; tail; tail = TREE_CHAIN (tail))
	if (DECL_NAME (tail) == TREE_VALUE (parm))
	  {
	    found = tail;
	    break;
	  }

      /* If declaration already marked, we have a duplicate name.
	 Complain, and don't use this decl twice.   */
      if (found && DECL_RESULT (found) != 0)
	{
	  error_with_decl (found, "multiple parameters named `%s'");
	  found = 0;
	}

      /* If the declaration says "void", complain and ignore it.  */
      if (found && TYPE_MAIN_VARIANT (TREE_TYPE (found)) == void_type_node)
	{
	  error_with_decl (found, "parameter `%s' declared void");
	  TREE_TYPE (found) = integer_type_node;
	  DECL_ARG_TYPE (found) = integer_type_node;
	  layout_decl (found, 0);
	}

      /* Traditionally, a parm declared float is actually a double.  */
      if (found && flag_traditional
	  && TYPE_MAIN_VARIANT (TREE_TYPE (found)) == float_type_node)
	{
	  TREE_TYPE (found) = double_type_node;
	  DECL_ARG_TYPE (found) = double_type_node;
	  layout_decl (found, 0);
	}

      /* If no declaration found, default to int.  */
      if (!found)
	{
	  found = build_decl (PARM_DECL, TREE_VALUE (parm),
			      integer_type_node);
	  DECL_ARG_TYPE (found) = TREE_TYPE (found);
	  DECL_SOURCE_LINE (found) = DECL_SOURCE_LINE (fndecl);
	  DECL_SOURCE_FILE (found) = DECL_SOURCE_FILE (fndecl);
	  error_with_decl (found, "type of parameter `%s' is not declared");
	  pushdecl (found);
	}

      TREE_PURPOSE (parm) = found;

      /* Mark this decl as "already found" -- see test, above.
	 It is safe to use DECL_RESULT for this
	 since it is not used in PARM_DECLs or CONST_DECLs.  */
      DECL_RESULT (found) = error_mark_node;
    }

  /* Complain about any actual PARM_DECLs not matched with any names.  */

  for (parm = parmdecls; parm; )
    {
      tree next = TREE_CHAIN (parm);
      TREE_CHAIN (parm) = 0;

      /* Complain about args with incomplete types.  */
      if (TYPE_SIZE (TREE_TYPE (parm)) == 0)
	{
	  error_with_decl (parm, "parameter `%s' has incomplete type");
	  TREE_TYPE (parm) = error_mark_node;
	}

      if (DECL_RESULT (parm) == 0)
	{
	  error_with_decl (parm,
			   "declaration for parameter `%s' but no such parameter");
	  /* Pretend the parameter was not missing.
	     This gets us to a standard state and minimizes
	     further error messages.  */
	  specparms
	    = chainon (specparms,
		       tree_cons (parm, NULL_TREE, NULL_TREE));
	}

      parm = next;
    }

  /* Chain the declarations together in the order of the list of names.
     At the same time, build up a list of their types, in reverse order.  */

  parm = specparms;
  parmdecls = 0;
  {
    register tree last;
    for (last = 0; parm; parm = TREE_CHAIN (parm))
      if (TREE_PURPOSE (parm))
	{
	  if (last == 0)
	    parmdecls = TREE_PURPOSE (parm);
	  else
	    TREE_CHAIN (last) = TREE_PURPOSE (parm);
	  last = TREE_PURPOSE (parm);
	  TREE_CHAIN (last) = 0;

	  types = saveable_tree_cons (NULL_TREE, TREE_TYPE (parm), types);
	}
  }
  
  if (void_at_end)
    return saveable_tree_cons (parmdecls, nonparms,
			       nreverse (saveable_tree_cons (NULL_TREE, void_type_node, types)));

  return saveable_tree_cons (parmdecls, nonparms, nreverse (types));
}

/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.

   NESTED is nonzero if the function being finished is nested in another.  */

void
finish_function (nested)
     int nested;
{
  register tree fndecl = current_function_decl;

/*  TREE_READONLY (fndecl) = 1;
    This caused &foo to be of type ptr-to-const-function
    which then got a warning when stored in a ptr-to-function variable.  */

  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */

  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* Obey `register' declarations if `setjmp' is called in this fn.  */
  if (flag_traditional && current_function_calls_setjmp)
    {
      setjmp_protect (DECL_INITIAL (fndecl));
      setjmp_protect_args ();
    }

#ifdef DEFAULT_MAIN_RETURN
  if (! strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "main"))
    {
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl)))
	  != integer_type_node)
	warning_with_decl (fndecl, "return type of `%s' is not `int'");
      else
	{
	  /* Make it so that `main' always returns success by default.  */
	  DEFAULT_MAIN_RETURN;
	}
    }
#endif

  /* Generate rtl for function exit.  */
  expand_function_end (input_filename, lineno, 0);

  /* So we can tell if jump_optimize sets it to 1.  */
  can_reach_end = 0;

  /* Run the optimizers and output the assembler code for this function.  */
  rest_of_compilation (fndecl);

  current_function_returns_null |= can_reach_end;

  if (TREE_THIS_VOLATILE (fndecl) && current_function_returns_null)
    warning ("`noreturn' function does return");
  else if (warn_return_type && can_reach_end
	   && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl))) != void_type_node)
    /* If this function returns non-void and control can drop through,
       complain.  */
    warning ("control reaches end of non-void function");
  /* With just -W, complain only if function returns both with
     and without a value.  */
  else if (extra_warnings
	   && current_function_returns_value && current_function_returns_null)
    warning ("this function may return with or without a value");

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */

  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      register tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type)
	{
	  register tree ret_type_size = TYPE_SIZE (ret_type);

	  if (TREE_CODE (ret_type_size) == INTEGER_CST)
	    {
	      unsigned units
		= TREE_INT_CST_LOW (ret_type_size) / BITS_PER_UNIT;

	      if (units > larger_than_size)
		warning_with_decl (fndecl,
				   "size of return value of `%s' is %u bytes",
				   units);
	    }
	}
    }

  /* Free all the tree nodes making up this function.  */
  /* Switch back to allocating nodes permanently
     until we start another function.  */
  if (! nested)
    permanent_allocation (1);

  if (DECL_SAVED_INSNS (fndecl) == 0 && ! nested)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      /* For a nested function, this is done in pop_c_function_context.  */
      /* If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (fndecl) != 0)
	DECL_INITIAL (fndecl) = error_mark_node;
      DECL_ARGUMENTS (fndecl) = 0;
    }

  if (DECL_STATIC_CONSTRUCTOR (fndecl))
    {
#ifndef ASM_OUTPUT_CONSTRUCTOR
      if (! flag_gnu_linker)
	static_ctors = perm_tree_cons (NULL_TREE, fndecl, static_ctors);
      else
#endif
      assemble_constructor (IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }
  if (DECL_STATIC_DESTRUCTOR (fndecl))
    {
#ifndef ASM_OUTPUT_DESTRUCTOR
      if (! flag_gnu_linker)
	static_dtors = perm_tree_cons (NULL_TREE, fndecl, static_dtors);
      else
#endif
      assemble_destructor (IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  if (! nested)
    {
      /* Let the error reporting routines know that we're outside a
	 function.  For a nested function, this value is used in
	 pop_c_function_context and then reset via pop_function_context.  */
      current_function_decl = NULL;
    }
}

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct c_function
{
  struct c_function *next;
  tree named_labels;
  tree shadowed_labels;
  int returns_value;
  int returns_null;
  int warn_about_return_type;
  int extern_inline;
  struct binding_level *binding_level;
};

struct c_function *c_function_chain;

/* Save and reinitialize the variables
   used during compilation of a C function.  */

void
push_c_function_context ()
{
  struct c_function *p
    = (struct c_function *) xmalloc (sizeof (struct c_function));

  if (pedantic)
    pedwarn ("ANSI C forbids nested functions");

  push_function_context ();

  p->next = c_function_chain;
  c_function_chain = p;

  p->named_labels = named_labels;
  p->shadowed_labels = shadowed_labels;
  p->returns_value = current_function_returns_value;
  p->returns_null = current_function_returns_null;
  p->warn_about_return_type = warn_about_return_type;
  p->extern_inline = current_extern_inline;
  p->binding_level = current_binding_level;
}

/* Restore the variables used during compilation of a C function.  */

void
pop_c_function_context ()
{
  struct c_function *p = c_function_chain;
  tree link;

  /* Bring back all the labels that were shadowed.  */
  for (link = shadowed_labels; link; link = TREE_CHAIN (link))
    if (DECL_NAME (TREE_VALUE (link)) != 0)
      IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)))
	= TREE_VALUE (link);

  if (DECL_SAVED_INSNS (current_function_decl) == 0)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = 0;
    }

  pop_function_context ();

  c_function_chain = p->next;

  named_labels = p->named_labels;
  shadowed_labels = p->shadowed_labels;
  current_function_returns_value = p->returns_value;
  current_function_returns_null = p->returns_null;
  warn_about_return_type = p->warn_about_return_type;
  current_extern_inline = p->extern_inline;
  current_binding_level = p->binding_level;

  free (p);
}

/* integrate_decl_tree calls this function, but since we don't use the
   DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node;
{
}
