/* Definitions for CHILL parsing and type checking.
   Copyright (C) 1992, 93, 94, 98, 1999 Free Software Foundation, Inc.

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

#ifndef _CH_TREE_H
#define _CH_TREE_H

/* Usage of TREE_LANG_FLAG_?:
   1: TUPLE_NAMED_FIELD
   "  TYPE_FIELDS_READONLY (in ARRAY_TYPE, RECORD_TYPE or UNION_TYPE)
   "  C_DECLARED_LABEL_FLAG
   "  C_TYPE_VARIABLE_SIZE
   2: C_TYPE_FIELDS_VOLATILE (in RECORD_TYPE or UNION_TYPE)
   "  ELSE_LABEL_SPECIFIED (in CASE selector expression)
   3: UNSATISFIED_FLAG
   4: CH_USE_SEIZEFILE_RESTRICTED
   "  CH_ALREADY_GRANTED
   5: CH_DERIVED_FLAG   (in EXPR or DECL)
*/

/* Usage of TYPE_LANG_FLAG_?:
   0: CH_TYPE_NONVALUE_P
   1: C_TYPE_VARIABLE_SIZE
   2: CH_IS_ACCESS_MODE  
   3: CH_IS_BUFFER_MODE
   4: CH_IS_EVENT_MODE
   5: CH_ENUM_IS_NUMBERED
   6: CH_IS_TEXT_MODE
*/

/* Language-dependent contents of an identifier.  */

struct lang_identifier
{
  /* These match the fields in c-tree.h. */
  struct tree_identifier ignore;
  tree outer_value, local_value, implicit_decl;
  tree error_locus, limbo_value;

  /* These are Chill-specific. */
  tree forbid;
  tree signal_dest;
  int  signal_data;
};

/* Macros for access to language-specific slots in an identifier.  */

/* The outer_value is a chain of decls (normally a single decl),
   that have been granted into the scope surrounding all modules. */
#define IDENTIFIER_OUTER_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->outer_value)
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *)(NODE))->implicit_decl)
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
  (((struct lang_identifier *)(NODE))->error_locus)
#define IDENTIFIER_FORBID(NODE)         \
  (((struct lang_identifier *)(NODE))->forbid)

/* The nesting level increates by one for every nested 'group'.
   Predefined declarations have level -1; the global scope is level 0.
 */
#define DECL_NESTING_LEVEL(DECL) \
   ((DECL)->decl.vindex ? TREE_INT_CST_LOW((DECL)->decl.vindex) : -1)

/* Nesting of things that can have an ON-unit attached. */
extern int action_nesting_level;

/* The DECL_NAME of a FIELD_DECL that represents the ELSE part of a variant. */
#define ELSE_VARIANT_NAME ridpointers[(int) RID_ELSE]

/* For a LABEL_DECL:  action_nesting_level of its target. */
#define DECL_ACTION_NESTING_LEVEL(NODE) ((NODE)->decl.saved_insns.i)

#define DECL_OLD_PREFIX(DECL) ((DECL)->decl.initial)
#define DECL_NEW_PREFIX(DECL) ((DECL)->decl.result)
#define DECL_POSTFIX(DECL) ((DECL)->decl.arguments)
extern tree ALL_POSTFIX;
#define DECL_SEIZEFILE(DECL) ((DECL)->decl.size)
#define DECL_POSTFIX_ALL(DECL) (DECL_POSTFIX(DECL) == ALL_POSTFIX)
#define DECL_OLD_NAME(DECL) decl_old_name(DECL)
/* For a siezefile name this means restricted usage of this file.
   In this case, the USE_SEIZE_FILE directive will not be copied
   into the grant file */
#define CH_USE_SEIZEFILE_RESTRICTED(NODE) TREE_LANG_FLAG_4(NODE)
extern tree decl_old_name PROTO ((tree));

/* for selective granting, mark as already granted */
#define CH_ALREADY_GRANTED(NODE) TREE_LANG_FLAG_4(NODE)

/* to store the receiving process of that signal
   at definition time */
#define IDENTIFIER_SIGNAL_DEST(NODE)	\
  (((struct lang_identifier *)(NODE))->signal_dest)

/* indicates a signal with no data */
#define IDENTIFIER_SIGNAL_DATA(NODE)	\
  (((struct lang_identifier *)(NODE))->signal_data)

/* In identifiers, C uses the following fields in a special way:
   TREE_PUBLIC        to record that there was a previous local extern decl.
   TREE_USED          to record that such a decl was used.
   TREE_ADDRESSABLE   to record that the address of such a decl was used.  */

/* Nonzero means reject anything that Z.200 Recommendation forbids.  */
extern int pedantic;

/* the prototypical CHILL INSTANCE type */
extern tree instance_type_node;

/* Non-zero if type or expr depends on non-resolved identifier. */
#define UNSATISFIED(expr) \
  (UNSATISFIED_FLAG (expr) || TREE_CODE (expr) == IDENTIFIER_NODE)
#define UNSATISFIED_FLAG(expr) TREE_LANG_FLAG_3 (expr)

/* Non-zero in a TREE_LIST if part of a labelled structure tuple. */
#define TUPLE_NAMED_FIELD(LIST) TREE_LANG_FLAG_1(LIST)

/* In an ARRAY_TYPE, RECORD_TYPE or UNION_TYPE, nonzero if any component
   is read-only.  */
#define TYPE_FIELDS_READONLY(type) TREE_LANG_FLAG_1 (type)

/* True if TYPE has the "read-only property." */
#define TYPE_READONLY_PROPERTY(TYPE) \
  (TYPE_READONLY (TYPE) || TYPE_FIELDS_READONLY (TYPE))

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is volatile.  */
#define C_TYPE_FIELDS_VOLATILE(type) TREE_LANG_FLAG_2 (type)

/* In a CASE selector expression, nonzero if any alternative specifies (ELSE) for
   that selector. */
#define ELSE_LABEL_SPECIFIED(expr) TREE_LANG_FLAG_2 (expr)

/* CH_CHARS_TYPE_P(TYPE) is true iff TYPE is a character string type.

   There is no essential difference between a string and a (one-dimensional)
   character array, at least for non-varying types.  I don't know where
   the Chill designers got the idea that it was useful to make a distinction.
   (I suspect packing might be involved, but on a byte-adressable machine
   we don't care.)  Since we want the same code to be generated for
   char arrays as for char strings, we use the same representation for
   both.  But we still need to distinguish them for the sake a Chill
   type checking.  We do that using TYPE_STRING_FLAG. */

#define MARK_AS_STRING_TYPE(TYPE) (TYPE_STRING_FLAG (TYPE) = 1)

#define CH_CHARS_TYPE_P(type) \
  (TREE_CODE (type) == ARRAY_TYPE && TREE_CODE(TREE_TYPE(type)) == CHAR_TYPE \
   && TYPE_STRING_FLAG (type))

/* True if TYPE is CHARS(1). */
#define CH_CHARS_ONE_P(TYPE) (CH_CHARS_TYPE_P(TYPE) \
   && integer_zerop (TYPE_MAX_VALUE (TYPE_DOMAIN (TYPE))))

/* True if TYPE is a bitstring (BOOLS or BIT) type.
   The TYPE_STRING_FLAG is used to distinguish a bitstring from a powerset. */

#define CH_BOOLS_TYPE_P(type) \
  (TREE_CODE (type) == SET_TYPE && TYPE_STRING_FLAG (type))

/* True if TYPE is BOOLS(1). */
#define CH_BOOLS_ONE_P(TYPE) (CH_BOOLS_TYPE_P(TYPE) \
   && integer_zerop (TYPE_MAX_VALUE (TYPE_DOMAIN (TYPE))))

/* Value is nonzero if TYPE is a CHILL string type.
   See CH_CHARS_TYPE_P and CH_BOOLS_TYPE_P above. */

#define CH_STRING_TYPE_P(type) \
 ((TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == SET_TYPE) \
  && TYPE_STRING_FLAG (type))

/* In a RECORD_TYPE, a sorted array of the fields of the type.  */
struct lang_type_record
{
  int  len;
  tree tasking_code_decl;
  tree elts[1];
};

struct lang_type
{
  union
    {
      struct lang_type_record rec;
    } foo;
};

struct lang_decl
{
  union
    {
      tree stuff;
    } foo;
};
/* A tasking type's corresponding tasking_code_variable has its
   decl pointer in here. */
#define DECL_TASKING_CODE_DECL(DECL) (DECL_LANG_SPECIFIC(DECL))

/* A pointer to an as-yet undefined type.  */
extern tree unknown_type_node;

/* The CHILL type INT (either integer_type_node or 
   short_integer_type_node). */
extern tree chill_integer_type_node;
extern tree chill_unsigned_type_node;

/* Nonzero for FIELD_DECL node means that this FIELD_DECL is
   a member of a union construct.  */
#define TREE_UNION_ELEM(NODE) ((NODE)->decl.regdecl_flag) /* overloaded! */

/* Mark which labels are explicitly declared.
   These may be shadowed, and may be referenced from nested functions.  */
#define C_DECLARED_LABEL_FLAG(label) TREE_LANG_FLAG_1 (label)

/* Record whether a type was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(type) TYPE_LANG_FLAG_1 (type)

#define DECL_WEAK_NAME(DECL) DECL_LANG_FLAG_0(DECL)

/* These are for FUNCTION_DECLs.  */
#define CH_DECL_GENERAL(DECL) DECL_LANG_FLAG_1(DECL)
#define CH_DECL_SIMPLE(DECL) DECL_LANG_FLAG_2(DECL)
#define CH_DECL_RECURSIVE(DECL) DECL_LANG_FLAG_3(DECL)
#define CH_FUNCTION_SETS_RESULT(DECL) DECL_LANG_FLAG_6(DECL)

/* For a CONST_DECL, indicates that it was implicitly declared
   in a SET mode declaration, and it should not be explicitly granted. */
#define CH_DECL_ENUM(DECL)  DECL_LANG_FLAG_3(DECL)

/* in a FIELD_DECL use DECL_LANG_FLAG_4 to mark FORBID in a grant-statement */
#define CH_DECL_FORBID(DECL) DECL_LANG_FLAG_4(DECL)

/* in an ALIAS_DECL use DECL_LANG_FLAG_4 to mark decl was granted */
#define CH_DECL_GRANTED(DECL) DECL_LANG_FLAG_4(DECL)

/* (in a non-FIELD_DECL) note that this decl was hidden by push_module(). */
#define DECL_HIDDEN_BY_MODULE(decl) DECL_LANG_FLAG_4 (decl)

/* Record in each node resulting from a binary operator
   what operator was specified for it.  */
#define C_EXP_ORIGINAL_CODE(exp) ((enum tree_code) TREE_COMPLEXITY (exp))

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(exp, code) \
  (TREE_COMPLEXITY (exp) = (int)(code))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(exp) DECL_LANG_FLAG_1 ((exp))

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_NONCOPIED_PARTS (NODE)

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the
   (names of) exceptions that this type can raise.  */
#define TYPE_RAISES_EXCEPTIONS(NODE) ((NODE)->type.minval)

/* For UNION_TYPE, the list of tag fields that distinguishes the members.  */
#define TYPE_TAGFIELDS(NODE) ((NODE)->type.minval)

/* For RECORD_TYPE, the tag values that select it.  */
#define TYPE_TAG_VALUES(NODE) TYPE_BINFO(NODE)

/* For VAR_DECL, TYPE_DECL, FUNCTION_DECL, indicates that
   the DECL was read from a seizefile but not seized */
#define CH_DECL_NOTDECLARED(DECL) DECL_LANG_FLAG_5(DECL)

/* For FUNCTION_DECL's, mark as PROCESSEs. */
#define CH_DECL_PROCESS(DECL) DECL_LANG_FLAG_7(DECL)

/* For TYPE_DECL's, mark as SIGNALs. */
#define CH_DECL_SIGNAL(DECL) DECL_LANG_FLAG_7(DECL)

/* Macros using terminology of the CHILL Blue Book. */

/* A class is either Null, All, M-value, M-derived, or M-reference,
   where M is some mode (type). */

enum ch_class_kind {
  CH_ALL_CLASS, CH_NULL_CLASS,
  CH_VALUE_CLASS, CH_DERIVED_CLASS, CH_REFERENCE_CLASS
};

typedef struct ch_class {
  enum ch_class_kind kind;
  tree mode;  /* The 'M' in M-value, M-derived, or M-reference. */
} ch_class;

struct mode_chain;  /* Forward reference */

#define CH_IS_REFERENCE_MODE(MODE) (TREE_CODE (MODE) == POINTER_TYPE)
#define CH_IS_BOUND_REFERENCE_MODE(MODE) \
  (TREE_CODE (MODE) == POINTER_TYPE && TREE_TYPE(MODE) != void_type_node)
#define CH_IS_PROCEDURE_MODE(MODE) (TREE_CODE (MODE) == FUNCTION_TYPE)
#define CH_IS_INSTANCE_MODE(MODE)  (CH_SIMILAR (MODE, instance_type_node))
#define CH_IS_BUFFER_MODE(MODE)    (TYPE_LANG_FLAG_3(MODE))
#define CH_IS_EVENT_MODE(MODE)     (TYPE_LANG_FLAG_4(MODE))
/* This is TRUE if the set is numbered, which makes pred/succ
   unusable */
#define CH_ENUM_IS_NUMBERED(MODE)  (TYPE_LANG_FLAG_5(MODE))

/* for ACCESS, and TEXT mode */
#define CH_IS_ACCESS_MODE(MODE)        (TYPE_LANG_FLAG_2(MODE))
#define CH_IS_TEXT_MODE(MODE)          (TYPE_LANG_FLAG_6(MODE))
#define CH_IS_ASSOCIATION_MODE(MODE)   (CH_SIMILAR (MODE, association_type_node))
#define CH_IS_USAGE_MODE(MODE)         (CH_SIMILAR (MODE, usage_type_node))
#define CH_IS_WHERE_MODE(MODE)         (CH_SIMILAR (MODE, where_type_node))

/* for RECORD or ARRAY type */
#define CH_TYPE_NONVALUE_P(MODE)       (TYPE_LANG_FLAG_0(MODE))

/* CH_NOVELTY is the novelty of a mode:  NULL_TREE means the novelty is nil;
   otherwise a TYPE_DECL matching the defining occurrence of a newmode. */
#define CH_NOVELTY(MODE)           TYPE_CONTEXT(MODE)

/* Set the novelty of MODE to NOVELTY (which is assumed to be non-nil). */
#define SET_CH_NOVELTY(MODE, NOVELTY) (CH_NOVELTY (MODE) = (NOVELTY))
#define SET_CH_NOVELTY_NONNIL(MODE, NOVELTY) (CH_NOVELTY (MODE) = (NOVELTY))

/* CH_DERIVED_FLAG is true the class of EXPR is X-derived for some X. */
#define CH_DERIVED_FLAG(EXPR) TREE_LANG_FLAG_5(EXPR)

#define CH_HAS_REFERENCING_PROPERTY(MODE) \
  (TREE_CODE (MODE) == POINTER_TYPE) /* incomplete FIXME! */

/* CH_COMPATIBLE(EXPR, MODE) is true if the class of EXPR is
   "compatible" with the type MODE. */
#define CH_COMPATIBLE(EXPR, MODE)           chill_compatible(EXPR, MODE)
#define CH_COMPATIBLE_CLASSES(EXPR1, EXPR2) chill_compatible_classes(EXPR1, EXPR2)
#define CH_STATIC_MODE(MODE)                  1 /* for now */
#define CH_SIMILAR(MODE1, MODE2)              chill_similar(MODE1, MODE2, 0)
#define CH_ROOT_MODE(MODE) chill_root_mode(MODE)
#define CH_RESULTING_CLASS(C1, C2) chill_resulting_class(C1, C2)
#define CH_ROOT_RESULTING_CLASS(E1, E2) \
  CH_RESULTING_CLASS (chill_expr_class(E1), chill_expr_class(E2))
#define CH_RESULTING_MODE(MODE1, MODE2) chill_resulting_mode(MODE1, MODE2)
#define CH_V_EQUIVALENT(MODE1, MODE2) (CH_SIMILAR(MODE1, MODE2) \
   && CH_NOVELTY(MODE1) == CH_NOVELTY(MODE2))
#define CH_EQUIVALENT(MODE1, MODE2) \
  (!integer_zerop (chill_equivalent (MODE1, MODE2, 0)))
#define CH_RESTRICTABLE_TO(MODE1, MODE2)  \
    CH_EQUIVALENT(MODE1, MODE2) /* && some more stuff FIXME! */

/* pass an OFFSET_TYPE or REFERENCE_TYPE's underlying type to SCALAR_P */
#define CH_READ_COMPATIBLE(modeM, modeN) chill_read_compatible(modeM, modeN)

#define SCALAR_P(TYPE) (TYPE != NULL_TREE \
			&& (TREE_CODE (TYPE) == INTEGER_TYPE \
			    || TREE_CODE (TYPE) == REAL_TYPE \
			    || TREE_CODE (TYPE) == ENUMERAL_TYPE \
			    || TREE_CODE (TYPE) == BOOLEAN_TYPE \
			    || TREE_CODE (TYPE) == CHAR_TYPE \
			    || TREE_CODE (TYPE) == POINTER_TYPE \
			    || TREE_CODE (TYPE) == INSTANCE_TYPE))
#define CH_REFERABLE(EXPR) chill_referable(EXPR)
#define CH_LOCATION_P(EXPR) chill_location (EXPR)

extern int maybe_objc_comptypes ();
extern tree maybe_building_objc_message_expr ();

/* Standard named or nameless data types of the C compiler.  */

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

extern int warn_write_strings;

/* Nonzero means warn about sizeof (function) or addition/subtraction
   of function pointers.  */

extern int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */

extern int warn_strict_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

extern int warn_redundant_decls;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */

extern int warn_nested_externs;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */

extern int warn_traditional;

/* Warn about *printf or *scanf format/argument anomalies. */

extern int warn_format;

/* Warn about a subscript that has type char.  */

extern int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */

extern int warn_conversion;

/* Warn if switch labels aren't complete, or are duplicated */

extern int warn_switch;

/* Nonzero means do some things the same way PCC does.  */

extern int flag_traditional;

/* Nonzero means warn about suggesting putting in ()'s.  */

extern int warn_parentheses;

/* Nonzero means this is a function to call to perform comptypes
   on two record types.  */

extern int (*comptypes_record_hook) ();

/* Nonzero means we are reading code that came from a system header file.  */
extern int system_header_p;

/* One means range checking is on; <= 0 off; -1 permanently off. */
extern int range_checking;

/* 0 means empty checking is off, else it is on */
extern int empty_checking;

/* 1 means -fruntime-checking specified (default), o means -fno-runtime-checking */
extern int runtime_checking_flag;

/* Type node for boolean types.  */

extern tree boolean_type_node;
extern tree signed_boolean_type_node;

extern tree string_one_type_node;
extern tree bitstring_one_type_node, bit_zero_node, bit_one_node;

extern tree float_type_node, double_type_node;
extern tree void_type_node, ptr_type_node, const_ptr_type_node;

/* a VOID_TYPE node, packaged in a TREE_LIST.  */

extern tree void_list_node;

/* Chill language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum chill_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "ch-tree.def"
  LAST_CHILL_TREE_CODE
};
#undef DEFTREECODE

enum chill_built_in_function
{
  DUMMY_FIRST_CHILL_BUILT_IN = END_BUILTINS,

  BUILT_IN_CH_ABS,
  BUILT_IN_ABSTIME,
  BUILT_IN_ADDR,
  BUILT_IN_ALLOCATE,
  BUILT_IN_ALLOCATE_GLOBAL_MEMORY,
  BUILT_IN_ALLOCATE_MEMORY,
  BUILT_IN_ARCCOS,
  BUILT_IN_ARCSIN,
  BUILT_IN_ARCTAN,
  BUILT_IN_ASSOCIATE,
  BUILT_IN_CARD,
  BUILT_IN_CONNECT,
  BUILT_IN_COPY_NUMBER,
  BUILT_IN_CH_COS,
  BUILT_IN_CREATE,
  BUILT_IN_DAYS,
  BUILT_IN_CH_DELETE,
  BUILT_IN_DESCR,
  BUILT_IN_DISCONNECT,
  BUILT_IN_DISSOCIATE,
  BUILT_IN_EOLN,
  BUILT_IN_EXP,
  BUILT_IN_EXPIRED,
  BUILT_IN_EXISTING,
  BUILT_IN_GEN_CODE,
  BUILT_IN_GEN_INST,
  BUILT_IN_GEN_PTYPE,
  BUILT_IN_GETASSOCIATION,
  BUILT_IN_GETSTACK,
  BUILT_IN_GETTEXTACCESS,
  BUILT_IN_GETTEXTINDEX,
  BUILT_IN_GETTEXTRECORD,
  BUILT_IN_GETUSAGE,
  BUILT_IN_HOURS,
  BUILT_IN_INDEXABLE,
  BUILT_IN_INTTIME,
  BUILT_IN_ISASSOCIATED,
  BUILT_IN_LENGTH,
  BUILT_IN_LOG,
  BUILT_IN_LOWER,
  BUILT_IN_LN,
  BUILT_IN_MAX,
  BUILT_IN_MILLISECS,
  BUILT_IN_MIN,
  BUILT_IN_MINUTES,
  BUILT_IN_MODIFY,
  BUILT_IN_NUM,
  BUILT_IN_OUTOFFILE,
  BUILT_IN_PRED,
  BUILT_IN_PROC_TYPE,
  BUILT_IN_QUEUE_LENGTH,
  BUILT_IN_READABLE,
  BUILT_IN_READRECORD,
  BUILT_IN_READTEXT,
  BUILT_IN_RETURN_MEMORY,
  BUILT_IN_SECS,
  BUILT_IN_SETTEXTACCESS,
  BUILT_IN_SETTEXTINDEX,
  BUILT_IN_SETTEXTRECORD,
  BUILT_IN_SEQUENCIBLE,
  BUILT_IN_SIZE,
  BUILT_IN_SQRT,
  BUILT_IN_SUCC,
  BUILT_IN_CH_SIN,
  BUILT_IN_TAN,
  BUILT_IN_TRUNC,
  BUILT_IN_TERMINATE,
  BUILT_IN_UPPER,
  BUILT_IN_VARIABLE,
  BUILT_IN_WAIT,
  BUILT_IN_WRITEABLE,
  BUILT_IN_WRITERECORD,
  BUILT_IN_WRITETEXT,
};

/* name of additional (compiler generated) arguments for
   functions which may propagate exceptions. */
#define CALLER_FILE "__CALLER_FILE__"
#define CALLER_LINE "__CALLER_LINE__"

/* field-name strings for the fields of the structure which
   represents a CHILL VARYING array.  The angle brackets assure
   that no user-defined structure can match this one.
   This field holds, at runtime, the current length of the
   array, in UNITS, not including the length itself. It's an
   integer_type_node */
#define VAR_LENGTH "__var_length"

/* This field is statically allocated to the user-defined
   size, but contains valid array entries starting from the
   first allocated space, proceeding for VAR_LENGTH bytes.
   There are no holes in the data;  the user isn't allowed
   to store beyond the first available entry. */

#define VAR_DATA "__var_data"

/* This field is the name of the array, encapsulated in the CHILL
   structure used to represent an array type parameter. */
/*#define ARRAY_DATA "__array_data"*/

/* The CHILL INSTANCE type is composed of two CHILL integer
   fields, the process_type (set by the user with the 
   process_type compiler directive, and the proc_copy field,
   which is set by the start_process call's first parameter. */
#define INS_PTYPE "__proc_type"
#define INS_COPY  "__proc_copy"

/* This is the actual array type inside the VARYING struct */
#define CH_VARYING_ARRAY_TYPE(TYPE) TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (TYPE)))

/* Identifiers which hold the VAR_LENGTH and VAR_DATA strings.  */
extern tree var_length_id;
extern tree var_data_id;

/* A RANGE_EXPR representing an ELSE in a case label. */
extern tree case_else_node;

#if 0   /* changed to function */
/* return non-zero if type is a compiler-generated VARYING array record */
#define CH_VARYING_TYPE_P(type) (TREE_CODE (type) == RECORD_TYPE && \
                           DECL_NAME (TYPE_FIELDS (type)) == \
                                      get_identifier (VAR_LENGTH) && \
                           DECL_NAME (TREE_CHAIN (TYPE_FIELDS (type))) == \
                                      get_identifier (VAR_DATA) && \
                   TREE_CHAIN (CH_VARYING_ARRAY_TYPE (type)) == NULL_TREE)

#endif

/* in c-aux-info.c */
extern void gen_aux_info_record                 PROTO((tree, int, int, int));

/* in c-common.c */
extern tree combine_strings                     PROTO((tree));
extern void constant_expression_warning         PROTO((tree));
extern void decl_attributes                     PROTO((tree, tree));
extern void declare_function_name               PROTO((void));
#ifdef BUFSIZ
extern char *get_directive_line                 PROTO((FILE *));
#endif
extern tree shorten_compare                     PROTO((tree *, tree *, tree *, enum tree_code *));

/* in c-decl.c */
extern tree short_integer_type_node, integer_type_node;
extern tree long_integer_type_node, long_long_integer_type_node;
extern tree short_unsigned_type_node, unsigned_type_node;
extern tree long_unsigned_type_node, long_long_unsigned_type_node;
extern tree ptrdiff_type_node;
extern tree unsigned_char_type_node, signed_char_type_node, char_type_node;
extern tree wchar_type_node, signed_wchar_type_node, unsigned_wchar_type_node;
extern tree float_type_node, double_type_node, long_double_type_node;
extern tree void_type_node, ptr_type_node, const_ptr_type_node;
extern tree default_function_type;
extern tree double_ftype_double, double_ftype_double_double;
extern tree int_ftype_int, long_ftype_long;
extern tree void_ftype_ptr_ptr_int, int_ftype_ptr_ptr_int;
extern tree void_ftype_ptr_int_int, string_ftype_ptr_ptr;
extern tree int_ftype_string_string, int_ftype_cptr_cptr_sizet;
/* Nodes for boolean constants TRUE and FALSE */
extern tree boolean_true_node, boolean_false_node;

extern tree global_function_decl;

/* in except.c */
extern void except_init_pass_2                  PROTO((void));

/* in ch-loop.c */
extern int flag_local_loop_counter;
extern void push_loop_block                     PROTO((void));
extern void pop_loop_block                      PROTO((void));
extern void build_loop_start                    PROTO((tree));
extern void top_loop_end_check			PROTO((tree));
extern void build_loop_end                      PROTO((void));
extern void build_loop_iterator                 PROTO((tree, tree, tree, tree, int, int, int));
extern void begin_loop_scope                    PROTO((void));
extern void end_loop_scope                      PROTO((tree));
extern void nonvalue_begin_loop_scope           PROTO((void));
extern void nonvalue_end_loop_scope             PROTO((void));

extern tree build_enumerator                    PROTO((tree, tree));
extern tree builtin_function                    PROTO((char *, tree, enum built_in_function function_, char *));
extern tree c_build_type_variant                PROTO((tree, int, int));
extern int  c_decode_option                     PROTO((int, char **));
extern void c_mark_varargs                      PROTO((void));
extern void clear_parm_order                    PROTO((void));
extern tree combine_parm_decls                  PROTO((tree, tree, int));
extern int  complete_array_type                 PROTO((tree, tree, int));
extern void declare_parm_level                  PROTO((int));
extern tree define_label                        PROTO((char *, int, tree));
extern void delete_block                        PROTO((tree));
extern void finish_decl                         PROTO((tree));
extern tree finish_enum                         PROTO((tree, tree));
extern void finish_function                     PROTO((int));
extern tree finish_struct                       PROTO((tree, tree));
extern tree get_parm_decls                      PROTO((void));
extern tree get_parm_info                       PROTO((int));
extern tree getdecls                            PROTO((void));
extern tree gettags                             PROTO((void));
extern int  global_bindings_p                   PROTO((void));
extern tree grokfield                           PROTO((char *, int, tree, tree, tree));
extern tree groktypename                        PROTO((tree));
extern tree groktypename_in_parm_context        PROTO((tree));
extern tree implicitly_declare                  PROTO((tree));
extern void init_decl_processing                PROTO((void));
extern void insert_block                        PROTO((tree));
extern void keep_next_level                     PROTO((void));
extern int  kept_level_p                        PROTO((void));
extern tree lookup_label                        PROTO((tree));
extern tree lookup_name                         PROTO((tree));
extern tree maybe_build_cleanup                 PROTO((tree));
extern void parmlist_tags_warning               PROTO((void));
extern void pending_xref_error                  PROTO((void));
extern void pop_chill_function_context          PROTO((void));
extern tree poplevel                            PROTO((int, int, int));
#ifdef BUFSIZ
extern void print_lang_decl                     PROTO((FILE *,tree, int));
extern void print_lang_identifier               PROTO((FILE *,tree, int));
extern void print_lang_type                     PROTO((FILE *,tree, int));
#endif
extern void push_chill_function_context         PROTO((void));
extern void push_parm_decl                      PROTO((tree));
extern tree pushdecl                            PROTO((tree));
extern tree pushdecl_top_level                  PROTO((tree));
extern void pushlevel                           PROTO((int));
extern void set_block                           PROTO((tree));
extern tree shadow_label                        PROTO((tree));
extern void shadow_record_fields                PROTO((tree));
extern void shadow_tag                          PROTO((tree));
extern void shadow_tag_warned                   PROTO((tree, int));
extern tree start_enum                          PROTO((tree));
extern int  start_function                      PROTO((tree, tree, int));
extern tree start_decl                          PROTO((tree, tree, int));
extern tree start_struct                        PROTO((enum tree_code, tree));
extern void store_parm_decls                    PROTO((void));
extern tree xref_tag                            PROTO((enum tree_code, tree));

/* in c-typeck.c */
extern tree build_array_ref                     PROTO((tree, tree));
extern tree build_c_cast                        PROTO((tree, tree));
extern tree build_chill_modify_expr             PROTO((tree, tree));
extern tree build_chill_component_ref           PROTO((tree, tree));
extern tree build_component_ref                 PROTO((tree, tree));
extern tree build_compound_expr                 PROTO((tree));
extern tree build_conditional_expr              PROTO((tree, tree, tree));
extern tree build_function_call                 PROTO((tree, tree));
extern tree build_indirect_ref                  PROTO((tree, char *));
extern tree build_modify_expr                   PROTO((tree, enum tree_code, tree));
extern tree build_unary_op                      PROTO((enum tree_code, tree, int));
extern tree c_alignof                           PROTO((tree));
extern tree c_alignof_expr                      PROTO((tree));
extern void c_expand_asm_operands               PROTO((tree, tree, tree, tree, int, char *, int));
extern tree c_sizeof                            PROTO((tree));
extern void c_expand_return                     PROTO((tree));
extern tree c_expand_start_case                 PROTO((tree));
extern tree common_type                         PROTO((tree, tree));
extern tree copy_novelty                        PROTO((tree, tree));
extern tree default_conversion                  PROTO((tree));
extern void finish_init                         PROTO((void));
extern tree parser_build_binary_op              PROTO((enum tree_code, tree, tree));
extern tree pop_init_level                      PROTO((int));
extern void process_init_default                PROTO((tree));
extern void process_init_element                PROTO((tree));
extern void push_init_level                     PROTO((int));
extern void really_start_incremental_init       PROTO((tree));
extern void set_init_index                      PROTO((tree, tree));
extern void set_init_label                      PROTO((tree));
extern void start_init                          PROTO((tree, tree, int));
extern void store_init_value                    PROTO((tree, tree));
extern tree valid_array_index_p                 PROTO((tree, tree, char *, int));

/* in ch/actions.c */
extern int grant_only_flag;
extern void allocate_lang_decl                  PROTO((tree));
extern tree build_chill_abs                     PROTO((tree));
extern tree build_chill_array_ref_1             PROTO((tree, tree));
extern tree build_chill_array_ref               PROTO((tree, tree));
extern tree build_chill_bin_type                PROTO((tree));
extern tree build_chill_binary_op               PROTO((enum chill_tree_code, tree, tree));
extern tree build_chill_card                    PROTO((tree));
extern tree build_chill_case_expr               PROTO((tree, tree, tree)); 
extern tree build_cause_exception               PROTO((tree, int));
extern tree build_chill_exception_decl          PROTO((char *));
extern tree build_chill_function_call           PROTO((tree, tree));
extern tree build_chill_length                  PROTO((tree));
extern tree build_chill_indirect_ref            PROTO((tree, tree, int));
extern tree build_chill_lower                   PROTO((tree));
extern tree build_chill_max                     PROTO((tree));
extern tree build_chill_min                     PROTO((tree));
extern tree build_chill_num                     PROTO((tree));
extern tree build_chill_repetition_op           PROTO((tree, tree));
extern tree build_chill_sizeof                  PROTO((tree));
extern tree build_chill_slice		        PROTO((tree, tree, tree));
extern tree build_chill_slice_with_range        PROTO((tree, tree, tree));
extern tree build_chill_slice_with_length       PROTO((tree, tree, tree));
extern tree build_chill_struct_type             PROTO((tree));
extern tree build_chill_unary_op                PROTO((enum chill_tree_code, tree));
extern tree build_chill_upper                   PROTO((tree));
extern tree build_exception_variant             PROTO((tree, tree));
extern tree build_generalized_call              PROTO((tree, tree));
extern tree build_lang_decl                     PROTO((enum chill_tree_code, tree, tree));
extern tree build_rts_call                      PROTO((char *, tree, tree));
extern tree build_varying_struct                PROTO((tree));
extern void chill_check_decl                    PROTO((tree));
extern tree chill_convert_for_assignment        PROTO((tree, tree, char*));
extern void chill_expand_return                 PROTO((tree, int));
extern void chill_expand_result                 PROTO((tree, int));
extern void chill_handle_case_default           PROTO((void));
extern void chill_handle_case_label		PROTO((tree, tree));
extern int  chill_varying_string_type_p         PROTO((tree));
extern int  chill_varying_type_p                PROTO((tree));
extern int  ch_singleton_set                    PROTO((tree));
extern tree check_expression                    PROTO((tree, tree, tree));
extern void check_missing_cases                 PROTO((tree));
extern tree check_non_null                      PROTO((tree));
extern tree check_range                         PROTO((tree, tree, tree,tree));
extern void cond_type_range_exception           PROTO((tree));
extern void expand_cause_exception              PROTO((tree));
extern tree finish_chill_binary_op              PROTO((tree));
extern tree finish_chill_unary_op               PROTO((tree));
extern tree high_domain_value                   PROTO((tree));
extern tree low_domain_value                    PROTO((tree));
extern tree maybe_array_ref			PROTO((tree, tree));
extern void maybe_chill_check_decl              PROTO((tree));
extern tree powersetlen                         PROTO((tree));
extern tree test_range                          PROTO((tree, tree, tree));
/* in ch/convert.c */
extern tree build_array_type_for_scalar         PROTO((tree));
extern tree convert                             PROTO((tree, tree));
extern tree convert_from_reference              PROTO((tree));
extern tree convert_to_class                    PROTO((ch_class, tree));
extern char* display_int_cst			PROTO((tree));

/* in ch/decl.c */
extern tree build_enumerator		        PROTO((tree, tree));
extern tree chill_munge_params                  PROTO((tree, tree, tree));
extern tree build_chill_function_type           PROTO((tree, tree, tree, tree));
extern tree decl_temp1                          PROTO((tree, tree, int, tree, int, int));
extern void do_based_decls                      PROTO((tree, tree, tree));
extern void do_chill_outparms                   PROTO((void));
extern tree do_decl                             PROTO((tree, tree, int, int, tree, int));
extern void do_decls                            PROTO((tree, tree, int, int, tree, int));
extern void expand_chill_outparms               PROTO((void));
extern void find_granted_decls		        PROTO((void));
extern void finish_chill_function               PROTO(());
extern tree finish_enum		                PROTO((tree, tree));
extern void fixup_chill_parms                   PROTO((tree));
extern void finish_outer_function               PROTO((void));
extern unsigned get_type_precision              PROTO((tree, tree));
extern tree grok_chill_fixedfields              PROTO((tree, tree, tree));
extern tree grok_chill_variantdefs              PROTO((tree, tree, tree));
extern void layout_enum                         PROTO((tree));
/* extern tree lookup_remembered_decl PROTO((HOST_WIDE_INT, tree)); */
extern void lookup_and_expand_goto		PROTO((tree));
extern tree lookup_tag_fields		        PROTO((tree, tree));
extern void lookup_and_handle_exit              PROTO((tree));
extern tree massage_param_node			PROTO((tree, tree));
extern void pop_module                          PROTO((void));
extern void print_mode                          PROTO((tree));
extern tree push_extern_function                PROTO((tree, tree, tree, tree, int));
extern void push_extern_process                 PROTO((tree, tree, tree, int));
extern void push_extern_signal                  PROTO((tree, tree, tree));
extern void push_granted                        PROTO((tree, tree));
extern tree push_modedef                        PROTO((tree, tree, int));
extern tree push_module                         PROTO((tree, int));
extern void push_parms                          PROTO((tree, tree, tree));
extern void push_syndecl                        PROTO((tree, tree, tree));
extern int result_never_set;
extern void save_expr_under_name                PROTO((tree, tree));
extern tree set_module_name                     PROTO((tree));
extern int  start_chill_function                PROTO((tree, tree, tree, tree, tree));
extern void start_outer_function	        PROTO((void));
extern void switch_to_pass_2 	                PROTO((void));

/* in ch/except.c */
extern void chill_check_no_handlers             PROTO((void));
extern void chill_finish_on                     PROTO((void));
extern void chill_handle_on_labels              PROTO((tree));
extern void chill_reraise_exceptions            PROTO((tree));
extern void chill_start_default_handler         PROTO((void));
extern void chill_start_on                      PROTO((void));
extern void expand_goto_except_cleanup          PROTO((int));
extern int is_handled				PROTO((tree));

/* in ch/expr.c */
extern tree build_chill_addr_expr               PROTO((tree, char *));
extern tree build_chill_arrow_expr              PROTO((tree, int));
extern tree build_component_ref		        PROTO((tree, tree));
extern tree build_chill_compound_expr           PROTO((tree));
extern tree build_chill_descr                   PROTO((tree));
extern void build_chill_descr_type              PROTO((void));
extern void build_chill_inttime_type            PROTO((void));
extern tree build_compare_expr			PROTO((enum tree_code,
						       tree, tree));
extern tree build_compare_discrete_expr		PROTO((enum tree_code,
						       tree, tree));
extern tree check_case_selector                 PROTO((tree));
extern tree check_case_selector_list            PROTO((tree));
extern tree check_have_mode                     PROTO((tree, char*));
extern void init_chill_expand                   PROTO((void));
extern void chill_expand_assignment             PROTO((tree, enum chill_tree_code, tree));
extern void expand_assignment_action            PROTO((tree, enum chill_tree_code, tree));
extern int compare_int_csts			PROTO((enum chill_tree_code,
						       tree, tree));
extern void expand_varying_length_assignment    PROTO((tree, tree));
extern tree force_addr_of			PROTO((tree));
extern tree resolve_component_ref               PROTO((tree));
extern tree truthvalue_conversion               PROTO((tree));
extern tree varying_to_slice		        PROTO((tree));

/* in ch/grant.c */
extern void chill_finish_compile                PROTO((void));
extern void chill_seize                         PROTO((tree, tree, tree));
extern void start_outer_function	        PROTO((void));
extern void finish_chill_seize                  PROTO((tree));
extern void chill_grant                         PROTO((tree,tree, tree, tree));
extern void set_default_grant_file              PROTO((void));
extern void set_identifier_size                 PROTO((int));
extern void write_grant_file                    PROTO((void));
extern void write_spec_module                   PROTO((tree, tree));

/* in ch/lang.c */
extern tree string_index_type_dummy;
extern tree integer_minus_one_node;
extern int  flag_old_strings;
extern void GNU_xref_begin                      PROTO((void));
extern void GNU_xref_end                        PROTO((void));
extern tree build_chill_array_type              PROTO((tree, tree, int, tree));
extern tree build_chill_struct_type             PROTO((tree));
extern tree build_chill_pointer_type            PROTO((tree));
extern tree build_chill_range_type              PROTO((tree, tree, tree));
extern tree build_chill_reference_type          PROTO((tree));
extern tree build_simple_array_type             PROTO((tree, tree, tree));
extern tree const_expr                          PROTO((tree));
extern tree get_identifier3			PROTO((char*, char*, char*));
extern tree layout_chill_array_type             PROTO((tree));
extern tree layout_chill_range_type             PROTO((tree, int));
extern tree layout_chill_pointer_type           PROTO((tree));
extern tree layout_chill_struct_type            PROTO((tree));
extern tree layout_chill_variants               PROTO((tree));
extern tree layout_powerset_type                PROTO((tree));
extern tree lookup_interface                    PROTO((tree));
extern tree maybe_building_objc_message_expr    PROTO((void));
extern void maybe_objc_check_decl               PROTO((tree));
extern int  maybe_objc_comptypes                PROTO((tree, tree));
extern int  recognize_objc_keyword              PROTO((void));

/* in ch/lex.l */
extern tree use_seizefile_name;
extern tree current_seizefile_name;
extern tree build_chill_string                  PROTO((int, char *));
extern int  check_newline                       PROTO((void));
extern tree get_chill_filename                  PROTO((void)); 
extern tree get_chill_linenumber                PROTO((void));       
extern void register_seize_path                 PROTO((char *));
extern void reinit_parse_for_function           PROTO((void));
extern void mark_use_seizefile_written          PROTO((tree));

/* in ch/loop.c */
extern void begin_chill_loop                    PROTO((tree, tree));
extern tree build_chill_iterator                PROTO((tree, tree, tree, int, int, int));
extern void end_chill_loop                      PROTO((void));
extern tree get_unique_identifier               PROTO((char *));

/* in ch/inout.c */
extern tree access_recordmode                   PROTO((tree));
extern void invalidate_access_recordmode        PROTO((tree));
extern tree access_indexmode                    PROTO((tree));
extern tree access_dynamic                      PROTO((tree));
extern tree association_init_value;
extern tree association_type_node;
extern tree build_access_mode                   PROTO((tree, tree, int));
extern tree build_chill_associate               PROTO((tree, tree, tree));
extern tree build_chill_connect                 PROTO((tree, tree, tree, tree));
extern tree build_chill_create                  PROTO((tree));
extern tree build_chill_delete                  PROTO((tree));
extern tree build_chill_disconnect              PROTO((tree));
extern tree build_chill_dissociate              PROTO((tree));
extern tree build_chill_eoln                    PROTO((tree)); 
extern tree build_chill_existing                PROTO((tree));
extern tree build_chill_gettextaccess           PROTO((tree)); 
extern tree build_chill_getassociation          PROTO((tree)); 
extern tree build_chill_gettextindex            PROTO((tree)); 
extern tree build_chill_gettextrecord           PROTO((tree)); 
extern tree build_chill_getusage                PROTO((tree)); 
extern tree build_chill_indexable               PROTO((tree)); 
extern tree build_chill_isassociated            PROTO((tree)); 
extern tree build_chill_modify                  PROTO((tree, tree));
extern tree build_chill_outoffile               PROTO((tree)); 
extern tree build_chill_readable                PROTO((tree));
extern tree build_chill_readrecord              PROTO((tree, tree));
extern tree build_chill_readtext                PROTO((tree, tree));
extern tree build_chill_sequencible             PROTO((tree)); 
extern tree build_chill_settextaccess           PROTO((tree, tree));
extern tree build_chill_settextindex            PROTO((tree, tree));
extern tree build_chill_settextrecord           PROTO((tree, tree));
extern tree build_chill_variable                PROTO((tree)); 
extern tree build_chill_writeable               PROTO((tree));
extern tree build_chill_writerecord             PROTO((tree, tree));
extern tree build_chill_writetext               PROTO((tree, tree));
extern void build_enum_tables                   PROTO((void));
extern tree build_text_mode                     PROTO((tree, tree, int));
extern tree check_text_length                   PROTO((tree));
extern void init_access_location                PROTO((tree, tree));
extern void init_text_location                  PROTO((tree, tree));
extern void inout_init                          PROTO((void));
extern tree text_dynamic                        PROTO((tree));
extern tree text_indexmode                      PROTO((tree));
extern tree text_length                         PROTO((tree));
extern tree usage_type_node;
extern tree where_type_node;

/* in ch/parse.c */
extern tree get_type_of                         PROTO((tree));
extern void set_yydebug                         PROTO((int));
extern void yyerror                             PROTO((char *));
extern int  pass;
extern int ignoring;
extern int seen_action;
extern int build_constructor;
extern void possibly_define_exit_label          PROTO((tree));
extern void to_global_binding_level             PROTO((void));

/* in ch/satisfy.c */
extern tree satisfy_decl 	                PROTO((tree, int));

/* in ch/tasking.c */
extern void add_taskstuff_to_list               PROTO((tree, char *, tree, tree, tree));
extern void process_buffer_decls                PROTO((tree, tree, int));
extern tree buffer_element_mode                 PROTO((tree));
extern void invalidate_buffer_element_mode      PROTO((tree));
extern tree build_buffer_descriptor             PROTO((tree, tree, tree));
extern tree build_buffer_type                   PROTO((tree, tree));
extern void build_delay_action                  PROTO((tree, tree));
extern tree build_delay_case_start              PROTO((tree, tree));
extern void build_delay_case_end                PROTO((tree));
extern void build_delay_case_label              PROTO((tree, int));
extern tree build_event_type                    PROTO((tree));
extern void build_receive_case_end              PROTO((tree, tree));
extern int  build_receive_case_if_generated     PROTO((void));
extern tree build_receive_case_label            PROTO((tree, tree));
extern tree build_receive_case_start            PROTO((tree));
extern void expand_continue_event               PROTO((tree));
extern void expand_send_buffer                  PROTO((tree, tree, tree, tree, tree));
extern void expand_send_signal                  PROTO((tree, tree, tree, tree, tree));
extern void build_start_process                 PROTO((tree, tree, tree, tree));
extern tree build_copy_number                   PROTO((tree));
extern tree build_gen_code                      PROTO((tree));
extern tree build_gen_inst                      PROTO((tree, tree));
extern tree build_gen_ptype                     PROTO((tree));
extern void build_instance_type                 PROTO((void));
extern tree build_process_header                PROTO((tree, tree));
extern void build_process_wrapper               PROTO((tree, tree));
extern tree build_proc_type                     PROTO((tree));
extern tree build_queue_length                  PROTO((tree));
extern tree build_signal_descriptor             PROTO((tree, tree));
extern tree build_signal_struct_type            PROTO((tree, tree, tree));
extern tree build_tasking_struct                PROTO((void));
extern tree chill_taskingcode_type_node;
extern tree check_queue_size                    PROTO((tree));
extern tree generate_tasking_code_variable      PROTO((tree, tree *, int));
extern tree get_signal_type_name                PROTO((tree));
extern tree get_struct_type_name                PROTO((tree));
extern tree get_tasking_code_name               PROTO((tree));
extern tree make_process_struct                 PROTO((tree, tree));
extern tree make_signal_struct                  PROTO((tree));
extern tree max_queue_size                      PROTO((tree));
extern void tasking_init                        PROTO((void));
extern void tasking_registry                    PROTO((void));
extern void tasking_setup                       PROTO((void));

/* in ch/timing.c */
extern tree abs_timing_type_node;
extern tree after_stack;
extern void build_after_end                     PROTO((void));
extern void build_after_start                   PROTO((tree, int));
extern void build_after_timeout_start           PROTO((void));
extern void build_at_action                     PROTO((tree));
extern void build_cycle_end                     PROTO((tree));
extern tree build_cycle_start                   PROTO((tree));
extern tree build_timeout_preface               PROTO((void));
extern void build_timesupervised_call           PROTO((tree, tree));
extern tree duration_timing_type_node;
extern void timing_init                         PROTO((void));

/* in ch/tree.c */
extern tree build_alias_decl			PROTO((tree, tree, tree));
extern tree build_bitstring_type                PROTO((tree));
extern tree build_powerset_type                 PROTO((tree));
extern tree build_string_type                   PROTO((tree, tree));
extern tree decl_check_rename			PROTO((tree, tree));
extern tree discrete_count                      PROTO((tree));
extern int  list_length                         PROTO((tree));
extern tree munge_exit_label			PROTO((tree));
extern tree save_if_needed			PROTO((tree));

/* in ch/typeck.c */
extern tree build_array_from_set                PROTO((tree));
extern tree build_chill_array_ref               PROTO((tree, tree));
extern tree build_chill_bitref                  PROTO((tree, tree));
extern tree build_chill_cast                    PROTO((tree, tree));
extern tree chill_equivalent	                PROTO((tree, tree, struct mode_chain*));
extern tree build_init_struct                   PROTO((void));
extern tree build_readonly_type                 PROTO((tree));
extern int  chill_compatible                    PROTO((tree, tree));
extern int  chill_compatible_classes            PROTO((tree, tree));
extern ch_class chill_expr_class                PROTO((tree));
extern tree chill_give_type_to_expr             PROTO((tree, tree));
extern tree chill_expand_tuple                  PROTO((tree, tree));
extern ch_class chill_expr_class                PROTO((tree));
extern int  chill_location                      PROTO((tree));
extern tree chill_max_vary_array_index		PROTO((tree));
extern int  chill_read_compatible               PROTO((tree, tree));
extern int  chill_referable                     PROTO((tree));
extern tree chill_root_mode	                PROTO((tree));
extern ch_class chill_resulting_class           PROTO((ch_class, ch_class));
extern tree chill_resulting_mode                PROTO((tree, tree));
extern int  chill_similar	                PROTO((tree, tree, struct mode_chain*));
extern int  discrete_type_p			PROTO((tree));
extern tree initializer_constant_valid_p        PROTO((tree, tree));
extern tree convert_to_discrete                 PROTO((tree));
extern tree smash_dummy_type                    PROTO((tree));
extern tree string_assignment_condition         PROTO((tree, tree));
extern tree type_for_mode                       PROTO((enum machine_mode, int));
extern tree type_for_size                       PROTO((unsigned, int));
extern int  valid_array_index                   PROTO((tree, tree));
extern void validate_varying_array_ref          PROTO((tree, tree));

/* in function.c */
extern void expand_function_end                 PROTO((char *, int, int));
extern void expand_function_start               PROTO((tree, int));
extern void init_function_start                 PROTO((tree, char *, int));
extern void pop_function_context                PROTO((void));
extern void push_function_context               PROTO((void));

/* in integrate.c */
extern void output_inline_function              PROTO((tree));

/* in stmt.c */
extern void remember_end_note                   PROTO((tree));

/* in toplev.c */
extern void announce_function                   PROTO((tree));
extern int  floor_log2_wide                     PROTO((unsigned HOST_WIDE_INT));
extern void rest_of_compilation                 PROTO((tree));

/* in varasm.c */
extern void make_function_rtl                   PROTO((tree));

/* in ???? */
extern void init_iterators                      PROTO((void));
extern int  mark_addressable			PROTO((tree));
extern tree chill_result_decl;
#ifdef RTX_CODE
extern rtx label_rtx                            PROTO((tree));
#endif
extern void permanent_allocation                PROTO((int));

#ifndef SET_WORD_SIZE
#define SET_WORD_SIZE BITS_PER_WORD
#endif

struct module
{
  struct module *next_module;  /* Next module, in order of their beginning. */
  struct module *prev_module;  /* The surrounding module, if any. */
  tree name;
  tree prefix_name; /* Usually same as name, expect for nested modules.
		       Used to generate DECL_ASSEMBLER_NAMEs. */	       
  /* procedure_seen indicates a procedure or process was declared.
     After this, no SEIZE, DCL, SYN, NEWMODE, SYNMODE statement is allowed */
  int procedure_seen;
  int is_spec_module;

  /* The value of current_nesting_level inside the module. */
  int nesting_level;

  /* A chain contain one ALIAS_DECL for each 'GRANT foo->bar'.
     The DECL_NAME is get_identifier("bar"), and the DECL_INITIAL
     is get_identifier("bar").  Only used in pass 1. */
  tree granted_decls;
};

extern struct module *current_module;

/* fold a tree to constant as much as possible */
extern tree deep_fold PROTO((tree));
#endif
