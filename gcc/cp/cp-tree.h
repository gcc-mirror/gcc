/* Definitions for C++ parsing and type checking.
   Copyright (C) 1987, 93, 94, 95, 1996 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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

#ifndef _CP_TREE_H
#define _CP_TREE_H

#include "gansidecl.h"

/* Language-dependent contents of an identifier.  */

struct lang_identifier
{
  struct tree_identifier ignore;
  tree global_value, local_value;
  tree class_value;
  tree class_template_info;
  struct lang_id2 *x;
};

struct lang_id2
{
  tree label_value, implicit_decl;
  tree type_desc, as_list, error_locus;
};

typedef struct 
{
  tree t;
  int new_type_flag;
} flagged_type_tree;

/* To identify to the debug emitters if it should pay attention to the
   flag `-Wtemplate-debugging'.  */
#define HAVE_TEMPLATES 1

/* Macros for access to language-specific slots in an identifier.  */

#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->global_value)
#define IDENTIFIER_CLASS_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->class_value)
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
#define IDENTIFIER_TEMPLATE(NODE)	\
  (((struct lang_identifier *)(NODE))->class_template_info)

#define IDENTIFIER_TYPE_VALUE(NODE) (TREE_TYPE (NODE))
#define SET_IDENTIFIER_TYPE_VALUE(NODE,TYPE) (TREE_TYPE (NODE) = TYPE)
#define IDENTIFIER_HAS_TYPE_VALUE(NODE) (TREE_TYPE (NODE) ? 1 : 0)

#define LANG_ID_FIELD(NAME,NODE) \
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->NAME : 0)
#define SET_LANG_ID(NODE,VALUE,NAME) \
  (((struct lang_identifier *)(NODE))->x == 0				    \
   ? ((struct lang_identifier *)(NODE))->x				    \
      = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0,   \
   ((struct lang_identifier *)(NODE))->x->NAME = (VALUE))

#define IDENTIFIER_LABEL_VALUE(NODE)	    LANG_ID_FIELD(label_value, NODE)
#define SET_IDENTIFIER_LABEL_VALUE(NODE,VALUE)   \
	SET_LANG_ID(NODE, VALUE, label_value)

#define IDENTIFIER_IMPLICIT_DECL(NODE)	    LANG_ID_FIELD(implicit_decl, NODE)
#define SET_IDENTIFIER_IMPLICIT_DECL(NODE,VALUE) \
	SET_LANG_ID(NODE, VALUE, implicit_decl)

#define IDENTIFIER_AS_DESC(NODE)	    LANG_ID_FIELD(type_desc, NODE)
#define SET_IDENTIFIER_AS_DESC(NODE,DESC)	\
	SET_LANG_ID(NODE, DESC, type_desc)

#define IDENTIFIER_AS_LIST(NODE)	    LANG_ID_FIELD(as_list, NODE)
#define SET_IDENTIFIER_AS_LIST(NODE,LIST)	\
	SET_LANG_ID(NODE, LIST, as_list)

#define IDENTIFIER_ERROR_LOCUS(NODE)	    LANG_ID_FIELD(error_locus, NODE)
#define SET_IDENTIFIER_ERROR_LOCUS(NODE,VALUE)	\
	SET_LANG_ID(NODE, VALUE, error_locus)


#define IDENTIFIER_VIRTUAL_P(NODE) TREE_LANG_FLAG_1(NODE)

/* Nonzero if this identifier is the prefix for a mangled C++ operator name.  */
#define IDENTIFIER_OPNAME_P(NODE) TREE_LANG_FLAG_2(NODE)

#define IDENTIFIER_TYPENAME_P(NODE)	\
  (! strncmp (IDENTIFIER_POINTER (NODE),			\
	      IDENTIFIER_POINTER (ansi_opname[(int) TYPE_EXPR]),	\
	      IDENTIFIER_LENGTH (ansi_opname[(int) TYPE_EXPR])))

/* Nonzero means reject anything that ANSI standard C forbids.  */
extern int pedantic;

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(type) TYPE_LANG_FLAG_0 (type)

/* Record in each node resulting from a binary operator
   what operator was specified for it.  */
#define C_EXP_ORIGINAL_CODE(exp) ((enum tree_code) TREE_COMPLEXITY (exp))

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(exp, code) \
  (TREE_COMPLEXITY (exp) = (int)(code))

/* If non-zero, a VAR_DECL whose cleanup will cause a throw to the
   next exception handler.  */
extern tree exception_throw_decl;

extern tree double_type_node, long_double_type_node, float_type_node;
extern tree char_type_node, unsigned_char_type_node, signed_char_type_node;
extern tree ptrdiff_type_node;

extern tree short_integer_type_node, short_unsigned_type_node;
extern tree long_integer_type_node, long_unsigned_type_node;
extern tree long_long_integer_type_node, long_long_unsigned_type_node;
extern tree unsigned_type_node;
extern tree string_type_node, char_array_type_node, int_array_type_node;
extern tree wchar_array_type_node;
extern tree wchar_type_node, signed_wchar_type_node, unsigned_wchar_type_node;

extern tree complex_integer_type_node;
extern tree complex_float_type_node;
extern tree complex_double_type_node;
extern tree complex_long_double_type_node;

extern tree intQI_type_node, unsigned_intQI_type_node;
extern tree intHI_type_node, unsigned_intHI_type_node;
extern tree intSI_type_node, unsigned_intSI_type_node;
extern tree intDI_type_node, unsigned_intDI_type_node;

extern int current_function_returns_value;
extern int current_function_returns_null;
extern tree current_function_return_value;

extern tree ridpointers[];
extern tree ansi_opname[];
extern tree ansi_assopname[];

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* For cross referencing.  */

extern int flag_gnu_xref;

/* For environments where you can use GNU binutils (as, ld in particular).  */

extern int flag_gnu_binutils;

/* Nonzero means ignore `#ident' directives.  */

extern int flag_no_ident;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means warn when all ctors or dtors are private, and the class
   has no friends.  */

extern int warn_ctor_dtor_privacy;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

extern int warn_return_type;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

extern int warn_write_strings;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

extern int warn_pointer_arith;

/* Nonzero means warn about suggesting putting in ()'s.  */

extern int warn_parentheses;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

extern int warn_redundant_decls;

/* Warn if initializer is not completely bracketed.  */

extern int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */

extern int warn_sign_compare;

/* Warn about a subscript that has type char.  */

extern int warn_char_subscripts;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Warn about *printf or *scanf format/argument anomalies.  */

extern int warn_format;

/* Nonzero means warn about non virtual destructors in classes that have
   virtual functions.  */

extern int warn_nonvdtor;

/* Non-zero means warn when we convert a pointer to member function
   into a pointer to (void or function).  */

extern int warn_pmf2ptr;

/* Nonzero means warn about violation of some Effective C++ style rules.  */

extern int warn_ecpp;

/* Nonzero means warn where overload resolution chooses a promotion from
   unsigned to signed over a conversion to an unsigned of the same size.  */

extern int warn_sign_promo;

/* Non-zero means warn when a function is declared extern and later inline.  */

extern int warn_extern_inline;

/* Non-zero means warn when an old-style cast is used.  */

extern int warn_old_style_cast;

/* Nonzero means to treat bitfields as unsigned unless they say `signed'.  */

extern int flag_signed_bitfields;

/* 3 means write out only virtuals function tables `defined'
   in this implementation file.
   2 means write out only specific virtual function tables
   and give them (C) public access.
   1 means write out virtual function tables and give them
   (C) public access.
   0 means write out virtual function tables and give them
   (C) static access (default).
   -1 means declare virtual function tables extern.  */

extern int write_virtuals;

/* True for more efficient but incompatible (not not fully tested)
   vtable implementation (using thunks).
   0 is old behavior; 1 is new behavior.  */
extern int flag_vtable_thunks;

/* INTERFACE_ONLY nonzero means that we are in an "interface"
   section of the compiler.  INTERFACE_UNKNOWN nonzero means
   we cannot trust the value of INTERFACE_ONLY.  If INTERFACE_UNKNOWN
   is zero and INTERFACE_ONLY is zero, it means that we are responsible
   for exporting definitions that others might need.  */
extern int interface_only, interface_unknown;

/* Nonzero means we should attempt to elide constructors when possible.  */

extern int flag_elide_constructors;

/* Nonzero means enable obscure ANSI features and disable GNU extensions
   that might cause ANSI-compliant code to be miscompiled.  */

extern int flag_ansi;

/* Nonzero means do argument matching for overloading according to the
   ANSI rules, rather than what g++ used to believe to be correct.  */

extern int flag_ansi_overloading;

/* Nonzero means recognize and handle signature language constructs.  */

extern int flag_handle_signatures;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

extern int flag_default_inline;

/* The name-mangling scheme to use.  Versions of gcc before 2.8 use
   version 0.  */
extern int name_mangling_version;

/* Nonzero means that guiding declarations are allowed.  */
extern int flag_guiding_decls;


/* C++ language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum cplus_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "cp-tree.def"
  LAST_CPLUS_TREE_CODE
};
#undef DEFTREECODE

enum languages { lang_c, lang_cplusplus };

/* Macros to make error reporting functions' lives easier.  */
#define TYPE_IDENTIFIER(NODE) (DECL_NAME (TYPE_NAME (NODE)))
#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (TYPE_IDENTIFIER (NODE)))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (TYPE_IDENTIFIER (NODE)))

#define TYPE_ASSEMBLER_NAME_STRING(NODE) (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME  (NODE))))
#define TYPE_ASSEMBLER_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (TYPE_NAME (NODE))))

/* The _DECL for this _TYPE.  */
#define TYPE_MAIN_DECL(NODE) (TYPE_STUB_DECL (TYPE_MAIN_VARIANT (NODE)))

#define IS_AGGR_TYPE(t)		(TYPE_LANG_FLAG_5 (t))
#define IS_AGGR_TYPE_CODE(t)	(t == RECORD_TYPE || t == UNION_TYPE)
#define IS_AGGR_TYPE_2(TYPE1,TYPE2) \
  (TREE_CODE (TYPE1) == TREE_CODE (TYPE2)	\
   && IS_AGGR_TYPE (TYPE1)&IS_AGGR_TYPE (TYPE2))
#define IS_OVERLOAD_TYPE(t) \
  (IS_AGGR_TYPE (t) || TREE_CODE (t) == ENUMERAL_TYPE)

/* In a *_TYPE, nonzero means a built-in type.  */
#define TYPE_BUILT_IN(NODE) TYPE_LANG_FLAG_6(NODE)

#define DELTA_FROM_VTABLE_ENTRY(ENTRY) \
  (!flag_vtable_thunks ? \
     TREE_VALUE (CONSTRUCTOR_ELTS (ENTRY)) \
   : TREE_CODE (TREE_OPERAND ((ENTRY), 0)) != THUNK_DECL ? integer_zero_node \
   : build_int_2 (THUNK_DELTA (TREE_OPERAND ((ENTRY), 0)), 0))

/* Virtual function addresses can be gotten from a virtual function
   table entry using this macro.  */
#define FNADDR_FROM_VTABLE_ENTRY(ENTRY) \
  (!flag_vtable_thunks ? \
     TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (ENTRY)))) \
   : TREE_CODE (TREE_OPERAND ((ENTRY), 0)) != THUNK_DECL ? (ENTRY) \
   : DECL_INITIAL (TREE_OPERAND ((ENTRY), 0)))
#define SET_FNADDR_FROM_VTABLE_ENTRY(ENTRY,VALUE) \
  (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (ENTRY)))) = (VALUE))
#define FUNCTION_ARG_CHAIN(NODE) (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (NODE))))
#define PROMOTES_TO_AGGR_TYPE(NODE,CODE)	\
  (((CODE) == TREE_CODE (NODE)			\
       && IS_AGGR_TYPE (TREE_TYPE (NODE)))	\
   || IS_AGGR_TYPE (NODE))

/* Nonzero iff TYPE is uniquely derived from PARENT.  Under MI, PARENT can
   be an ambiguous base class of TYPE, and this macro will be false.  */
#define UNIQUELY_DERIVED_FROM_P(PARENT, TYPE) (get_base_distance (PARENT, TYPE, 0, (tree *)0) >= 0)
#define ACCESSIBLY_DERIVED_FROM_P(PARENT, TYPE) (get_base_distance (PARENT, TYPE, -1, (tree *)0) >= 0)
#define ACCESSIBLY_UNIQUELY_DERIVED_P(PARENT, TYPE) (get_base_distance (PARENT, TYPE, 1, (tree *)0) >= 0)
#define DERIVED_FROM_P(PARENT, TYPE) (get_base_distance (PARENT, TYPE, 0, (tree *)0) != -1)

/* Statistics show that while the GNU C++ compiler may generate
   thousands of different types during a compilation run, it
   generates relatively few (tens) of classtypes.  Because of this,
   it is not costly to store a generous amount of information
   in classtype nodes.  This struct must fill out to a multiple of 4 bytes.  */
struct lang_type
{
  struct
    {
      unsigned has_type_conversion : 1;
      unsigned has_int_conversion : 1;
      unsigned has_float_conversion : 1;
      unsigned has_init_ref : 1;
      unsigned gets_init_aggr : 1;
      unsigned has_assignment : 1;
      unsigned has_default_ctor : 1;
      unsigned uses_multiple_inheritance : 1;

      unsigned has_nonpublic_ctor : 2;
      unsigned has_nonpublic_assign_ref : 2;
      unsigned const_needs_init : 1;
      unsigned ref_needs_init : 1;
      unsigned has_const_assign_ref : 1;
      unsigned vtable_needs_writing : 1;

      unsigned has_assign_ref : 1;
      unsigned gets_new : 2;
      unsigned gets_delete : 2;
      unsigned has_call_overloaded : 1;
      unsigned has_array_ref_overloaded : 1;
      unsigned has_arrow_overloaded : 1;

      unsigned local_typedecls : 1;
      unsigned interface_only : 1;
      unsigned interface_unknown : 1;
      unsigned needs_virtual_reinit : 1;
      unsigned vec_delete_takes_size : 1;
      unsigned declared_class : 1;
      unsigned being_defined : 1;
      unsigned redefined : 1;

      unsigned no_globalize : 1;
      unsigned marked : 1;
      unsigned marked2 : 1;
      unsigned marked3 : 1;
      unsigned marked4 : 1;
      unsigned marked5 : 1;
      unsigned marked6 : 1;
      unsigned debug_requested : 1;

      unsigned use_template : 2;
      unsigned has_method_call_overloaded : 1;
      unsigned private_attr : 1;
      unsigned got_semicolon : 1;
      unsigned ptrmemfunc_flag : 1;
      unsigned is_signature : 1;
      unsigned is_signature_pointer : 1;

      unsigned is_signature_reference : 1;
      unsigned has_opaque_typedecls : 1;
      unsigned sigtable_has_been_generated : 1;
      unsigned was_anonymous : 1;
      unsigned has_real_assignment : 1;
      unsigned has_real_assign_ref : 1;
      unsigned has_const_init_ref : 1;
      unsigned has_complex_init_ref : 1;

      unsigned has_complex_assign_ref : 1;
      unsigned has_abstract_assign_ref : 1;
      unsigned non_aggregate : 1;

      /* The MIPS compiler gets it wrong if this struct also
	 does not fill out to a multiple of 4 bytes.  Add a
	 member `dummy' with new bits if you go over the edge.  */
      unsigned dummy : 21;

      unsigned n_vancestors : 16;
    } type_flags;

  int cid;
  int n_ancestors;
  int vsize;
  int max_depth;
  int vfield_parent;

  union tree_node *vbinfo[2];
  union tree_node *baselink_vec;
  union tree_node *vfields;
  union tree_node *vbases;
  union tree_node *vbase_size;

  union tree_node *tags;
  char *memoized_table_entry;

  union tree_node *search_slot;

#ifdef ONLY_INT_FIELDS
  unsigned int mode : 8;
#else
  enum machine_mode mode : 8;
#endif

  unsigned char size_unit;
  unsigned char align;
  unsigned char sep_unit;

  union tree_node *sep;
  union tree_node *size;

  union tree_node *base_init_list;
  union tree_node *abstract_virtuals;
  union tree_node *as_list;
  union tree_node *id_as_list;
  union tree_node *binfo_as_list;
  union tree_node *friend_classes;

  char *mi_matrix;

  union tree_node *rtti;

  union tree_node *methods;

  union tree_node *signature;
  union tree_node *signature_pointer_to;
  union tree_node *signature_reference_to;

  union tree_node *template_info;

  int linenum;
};

#define CLASSTYPE_SOURCE_LINE(NODE) (TYPE_LANG_SPECIFIC(NODE)->linenum)

/* Indicates whether or not (and how) a template was expanded for this class.
     0=no information yet/non-template class
     1=implicit template instantiation
     2=explicit template specialization
     3=explicit template instantiation  */
#define CLASSTYPE_USE_TEMPLATE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.use_template)

/* Fields used for storing information before the class is defined.
   After the class is defined, these fields hold other information.  */

/* List of friends which were defined inline in this class definition.  */
#define CLASSTYPE_INLINE_FRIENDS(NODE) (TYPE_NONCOPIED_PARTS (NODE))

/* Nonzero for _CLASSTYPE means that the _CLASSTYPE either has
   a special meaning for the assignment operator ("operator="),
   or one of its fields (or base members) has a special meaning
   defined.  */
#define TYPE_HAS_ASSIGNMENT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_assignment)
#define TYPE_HAS_REAL_ASSIGNMENT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_real_assignment)

/* Nonzero for _CLASSTYPE means that operator new and delete are defined,
   respectively.  */
#define TYPE_GETS_NEW(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_new)
#define TYPE_GETS_DELETE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_delete)
#define TYPE_GETS_REG_DELETE(NODE) (TYPE_GETS_DELETE (NODE) & 1)

/* Nonzero for _CLASSTYPE means that operator vec delete is defined and
   takes the optional size_t argument.  */
#define TYPE_VEC_DELETE_TAKES_SIZE(NODE) \
  (TYPE_LANG_SPECIFIC(NODE)->type_flags.vec_delete_takes_size)
#define TYPE_VEC_NEW_USES_COOKIE(NODE) \
  (TYPE_NEEDS_DESTRUCTOR (NODE) \
   || (TYPE_LANG_SPECIFIC (NODE) && TYPE_VEC_DELETE_TAKES_SIZE (NODE)))

/* Nonzero for TREE_LIST or _TYPE node means that this node is class-local.  */
#define TREE_NONLOCAL_FLAG(NODE) (TREE_LANG_FLAG_0 (NODE))

/* Nonzero for a _CLASSTYPE node which we know to be private.  */
#define TYPE_PRIVATE_P(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.private_attr)

/* Nonzero means that this _CLASSTYPE node defines ways of converting
   itself to other types.  */
#define TYPE_HAS_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_type_conversion)

/* Nonzero means that this _CLASSTYPE node can convert itself to an
   INTEGER_TYPE.  */
#define TYPE_HAS_INT_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_int_conversion)

/* Nonzero means that this _CLASSTYPE node can convert itself to an
   REAL_TYPE.  */
#define TYPE_HAS_REAL_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_float_conversion)

/* Nonzero means that this _CLASSTYPE node overloads operator=(X&).  */
#define TYPE_HAS_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_assign_ref)
#define TYPE_HAS_CONST_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_const_assign_ref)

/* Nonzero means that this _CLASSTYPE node has an X(X&) constructor.  */
#define TYPE_HAS_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_init_ref)
#define TYPE_HAS_CONST_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_const_init_ref)

/* Nonzero means that this _CLASSTYPE node has an X(X ...) constructor.
   Note that there must be other arguments, or this constructor is flagged
   as being erroneous.  */
#define TYPE_GETS_INIT_AGGR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_init_aggr)

/* Nonzero means that this type is being defined.  I.e., the left brace
   starting the definition of this type has been seen.  */
#define TYPE_BEING_DEFINED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.being_defined)
/* Nonzero means that this type has been redefined.  In this case, if
   convenient, don't reprocess any methods that appear in its redefinition.  */
#define TYPE_REDEFINED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.redefined)

/* Nonzero means that this _CLASSTYPE node overloads the method call
   operator.  In this case, all method calls go through `operator->()(...).  */
#define TYPE_OVERLOADS_METHOD_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_method_call_overloaded)

/* Nonzero means that this type is a signature.  */
# define IS_SIGNATURE(NODE) (TYPE_LANG_SPECIFIC(NODE)?TYPE_LANG_SPECIFIC(NODE)->type_flags.is_signature:0)
# define SET_SIGNATURE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.is_signature=1)
# define CLEAR_SIGNATURE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.is_signature=0)

/* Nonzero means that this type is a signature pointer type.  */
# define IS_SIGNATURE_POINTER(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.is_signature_pointer)

/* Nonzero means that this type is a signature reference type.  */
# define IS_SIGNATURE_REFERENCE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.is_signature_reference)

/* Nonzero means that this signature contains opaque type declarations.  */
#define SIGNATURE_HAS_OPAQUE_TYPEDECLS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_opaque_typedecls)

/* Nonzero means that a signature table has been generated
   for this signature.  */
#define SIGTABLE_HAS_BEEN_GENERATED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.sigtable_has_been_generated)

/* If NODE is a class, this is the signature type that contains NODE's
   signature after it has been computed using sigof().  */
#define CLASSTYPE_SIGNATURE(NODE) (TYPE_LANG_SPECIFIC(NODE)->signature)

/* If NODE is a signature pointer or signature reference, this is the
   signature type the pointer/reference points to.  */
#define SIGNATURE_TYPE(NODE) (TYPE_LANG_SPECIFIC(NODE)->signature)

/* If NODE is a signature, this is a vector of all methods defined
   in the signature or in its base types together with their default
   implementations.  */
#define SIGNATURE_METHOD_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->signature)

/* If NODE is a signature, this is the _TYPE node that contains NODE's
   signature pointer type.  */
#define SIGNATURE_POINTER_TO(NODE) (TYPE_LANG_SPECIFIC(NODE)->signature_pointer_to)

/* If NODE is a signature, this is the _TYPE node that contains NODE's
   signature reference type.  */
#define SIGNATURE_REFERENCE_TO(NODE) (TYPE_LANG_SPECIFIC(NODE)->signature_reference_to)

/* The is the VAR_DECL that contains NODE's rtti.  */
#define CLASSTYPE_RTTI(NODE) (TYPE_LANG_SPECIFIC(NODE)->rtti)

/* Nonzero means that this _CLASSTYPE node overloads operator().  */
#define TYPE_OVERLOADS_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_call_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator[].  */
#define TYPE_OVERLOADS_ARRAY_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_array_ref_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator->.  */
#define TYPE_OVERLOADS_ARROW(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_arrow_overloaded)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   multiple inheritance.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_MULTIPLE_INHERITANCE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.uses_multiple_inheritance)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   virtual base classes.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_VIRTUAL_BASECLASSES(NODE) (TREE_LANG_FLAG_3(NODE))

/* List of lists of member functions defined in this class.  */
#define CLASSTYPE_METHOD_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->methods)

/* The first type conversion operator in the class (the others can be
   searched with TREE_CHAIN), or the first non-constructor function if
   there are no type conversion operators.  */
#define CLASSTYPE_FIRST_CONVERSION(NODE) \
  TREE_VEC_LENGTH (CLASSTYPE_METHOD_VEC (NODE)) > 2 \
    ? TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (NODE), 2) \
    : NULL_TREE;

/* Pointer from any member function to the head of the list of
   member functions of the type that member function belongs to.  */
#define CLASSTYPE_BASELINK_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->baselink_vec)

/* Mark bits for depth-first and breath-first searches.  */
#define CLASSTYPE_MARKED(NODE)  (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked)
#define CLASSTYPE_MARKED2(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2)
#define CLASSTYPE_MARKED3(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3)
#define CLASSTYPE_MARKED4(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4)
#define CLASSTYPE_MARKED5(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5)
#define CLASSTYPE_MARKED6(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6)
/* Macros to modify the above flags */
#define SET_CLASSTYPE_MARKED(NODE)	(CLASSTYPE_MARKED(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED(NODE)	(CLASSTYPE_MARKED(NODE) = 0)
#define SET_CLASSTYPE_MARKED2(NODE)	(CLASSTYPE_MARKED2(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED2(NODE)	(CLASSTYPE_MARKED2(NODE) = 0)
#define SET_CLASSTYPE_MARKED3(NODE)	(CLASSTYPE_MARKED3(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED3(NODE)	(CLASSTYPE_MARKED3(NODE) = 0)
#define SET_CLASSTYPE_MARKED4(NODE)	(CLASSTYPE_MARKED4(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED4(NODE)	(CLASSTYPE_MARKED4(NODE) = 0)
#define SET_CLASSTYPE_MARKED5(NODE)	(CLASSTYPE_MARKED5(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED5(NODE)	(CLASSTYPE_MARKED5(NODE) = 0)
#define SET_CLASSTYPE_MARKED6(NODE)	(CLASSTYPE_MARKED6(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED6(NODE)	(CLASSTYPE_MARKED6(NODE) = 0)

#define CLASSTYPE_TAGS(NODE)		(TYPE_LANG_SPECIFIC(NODE)->tags)

/* If this class has any bases, this is the number of the base class from
   which our VFIELD is based, -1 otherwise.  If this class has no base
   classes, this is not used.
   In D : B1, B2, PARENT would be 0, if D's vtable came from B1,
   1, if D's vtable came from B2.  */
#define CLASSTYPE_VFIELD_PARENT(NODE)	(TYPE_LANG_SPECIFIC(NODE)->vfield_parent)

/* Remove when done merging.  */
#define CLASSTYPE_VFIELD(NODE) TYPE_VFIELD(NODE)

/* The number of virtual functions defined for this
   _CLASSTYPE node.  */
#define CLASSTYPE_VSIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vsize)
/* The virtual base classes that this type uses.  */
#define CLASSTYPE_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbases)
/* The virtual function pointer fields that this type contains.  */
#define CLASSTYPE_VFIELDS(NODE) (TYPE_LANG_SPECIFIC(NODE)->vfields)

/* Number of baseclasses defined for this type.
   0 means no base classes.  */
#define CLASSTYPE_N_BASECLASSES(NODE) \
  (TYPE_BINFO_BASETYPES (NODE) ? TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES(NODE)) : 0)

/* Memoize the number of super classes (base classes) tha this node
   has.  That way we can know immediately (albeit conservatively how
   large a multiple-inheritance matrix we need to build to find
   derivation information.  */
#define CLASSTYPE_N_SUPERCLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->n_ancestors)
#define CLASSTYPE_N_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.n_vancestors)

/* Record how deep the inheritance is for this class so `void*' conversions
   are less favorable than a conversion to the most base type.  */
#define CLASSTYPE_MAX_DEPTH(NODE) (TYPE_LANG_SPECIFIC(NODE)->max_depth)

/* Used for keeping search-specific information.  Any search routine
   which uses this must define what exactly this slot is used for.  */
#define CLASSTYPE_SEARCH_SLOT(NODE) (TYPE_LANG_SPECIFIC(NODE)->search_slot)

/* Entry for keeping memoization tables for this type to
   hopefully speed up search routines.  Since it is a pointer,
   it can mean almost anything.  */
#define CLASSTYPE_MTABLE_ENTRY(NODE) (TYPE_LANG_SPECIFIC(NODE)->memoized_table_entry)

/* This is the total size of the baseclasses defined for this type.
   Needed because it is desirable to layout such information
   before beginning to process the class itself, and we
   don't want to compute it second time when actually laying
   out the type for real.  */
#define CLASSTYPE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->size)
#define CLASSTYPE_SIZE_UNIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->size_unit)
#define CLASSTYPE_MODE(NODE) (TYPE_LANG_SPECIFIC(NODE)->mode)
#define CLASSTYPE_ALIGN(NODE) (TYPE_LANG_SPECIFIC(NODE)->align)

/* This is the space needed for virtual base classes.  NULL if
   there are no virtual basetypes.  */
#define CLASSTYPE_VBASE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbase_size)

/* A cons list of structure elements which either have constructors
   to be called, or virtual function table pointers which
   need initializing.  Depending on what is being initialized,
   the TREE_PURPOSE and TREE_VALUE fields have different meanings:

   Member initialization: <FIELD_DECL, TYPE>
   Base class construction: <NULL_TREE, BASETYPE>
   Base class initialization: <BASE_INITIALIZATION, THESE_INITIALIZATIONS>
   Whole type: <MEMBER_INIT, BASE_INIT>.  */
#define CLASSTYPE_BASE_INIT_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->base_init_list)

/* A cons list of virtual functions which cannot be inherited by
   derived classes.  When deriving from this type, the derived
   class must provide its own definition for each of these functions.  */
#define CLASSTYPE_ABSTRACT_VIRTUALS(NODE) (TYPE_LANG_SPECIFIC(NODE)->abstract_virtuals)

/* Nonzero means that this aggr type has been `closed' by a semicolon.  */
#define CLASSTYPE_GOT_SEMICOLON(NODE) (TYPE_LANG_SPECIFIC (NODE)->type_flags.got_semicolon)

/* Nonzero means that the main virtual function table pointer needs to be
   set because base constructors have placed the wrong value there.
   If this is zero, it means that they placed the right value there,
   and there is no need to change it.  */
#define CLASSTYPE_NEEDS_VIRTUAL_REINIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.needs_virtual_reinit)

/* Nonzero means that if this type has virtual functions, that
   the virtual function table will be written out.  */
#define CLASSTYPE_VTABLE_NEEDS_WRITING(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.vtable_needs_writing)

/* Nonzero means that this type defines its own local type declarations.  */
#define CLASSTYPE_LOCAL_TYPEDECLS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.local_typedecls)

/* Nonzero means that this type has an X() constructor.  */
#define TYPE_HAS_DEFAULT_CONSTRUCTOR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_default_ctor)

/* Nonzero means the type declared a ctor as private or protected.  We
   use this to make sure we don't try to generate a copy ctor for a 
   class that has a member of type NODE.  */
#define TYPE_HAS_NONPUBLIC_CTOR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_nonpublic_ctor)

/* Ditto, for operator=.  */
#define TYPE_HAS_NONPUBLIC_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_nonpublic_assign_ref)

/* Many routines need to cons up a list of basetypes for access
   checking.  This field contains a TREE_LIST node whose TREE_VALUE
   is the main variant of the type, and whose TREE_VIA_PUBLIC
   and TREE_VIA_VIRTUAL bits are correctly set.  */
#define CLASSTYPE_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->as_list)
/* Same, but cache a list whose value is the name of this type.  */
#define CLASSTYPE_ID_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->id_as_list)
/* Same, but cache a list whose value is the binfo of this type.  */
#define CLASSTYPE_BINFO_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->binfo_as_list)

/* A list of class types with which this type is a friend.  */
#define CLASSTYPE_FRIEND_CLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->friend_classes)

/* Keep an inheritance lattice around so we can quickly tell whether
   a type is derived from another or not.  */
#define CLASSTYPE_MI_MATRIX(NODE) (TYPE_LANG_SPECIFIC(NODE)->mi_matrix)

/* Say whether this node was declared as a "class" or a "struct".  */
#define CLASSTYPE_DECLARED_CLASS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.declared_class)
/* whether this can be globalized.  */
#define CLASSTYPE_NO_GLOBALIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.no_globalize)

/* Nonzero if this class has const members which have no specified initialization.  */
#define CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.const_needs_init)

/* Nonzero if this class has ref members which have no specified initialization.  */
#define CLASSTYPE_REF_FIELDS_NEED_INIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.ref_needs_init)

/* Nonzero if this class is included from a header file which employs
   `#pragma interface', and it is not included in its implementation file.  */
#define CLASSTYPE_INTERFACE_ONLY(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_only)

/* Same as above, but for classes whose purpose we do not know.  */
#define CLASSTYPE_INTERFACE_UNKNOWN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown)
#define CLASSTYPE_INTERFACE_KNOWN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown == 0)
#define SET_CLASSTYPE_INTERFACE_UNKNOWN_X(NODE,X) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown = !!(X))
#define SET_CLASSTYPE_INTERFACE_UNKNOWN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown = 1)
#define SET_CLASSTYPE_INTERFACE_KNOWN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown = 0)

/* Nonzero if a _DECL node requires us to output debug info for this class.  */
#define CLASSTYPE_DEBUG_REQUESTED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.debug_requested)

/* Additional macros for inheritance information.  */

#define CLASSTYPE_VBINFO(NODE,VIA_PUBLIC) \
  (TYPE_LANG_SPECIFIC (NODE)->vbinfo[VIA_PUBLIC])

/* When following an binfo-specific chain, this is the cumulative
   via-public flag.  */
#define BINFO_VIA_PUBLIC(NODE) TREE_LANG_FLAG_5 (NODE)

/* When building a matrix to determine by a single lookup
   whether one class is derived from another or not,
   this field is the index of the class in the table.  */
#define CLASSTYPE_CID(NODE) (TYPE_LANG_SPECIFIC(NODE)->cid)
#define BINFO_CID(NODE) CLASSTYPE_CID(BINFO_TYPE(NODE))

/* Nonzero means marked by DFS or BFS search, including searches
   by `get_binfo' and `get_base_distance'.  */
#define BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED(BINFO_TYPE(NODE)):TREE_LANG_FLAG_0(NODE))
/* Macros needed because of C compilers that don't allow conditional
   expressions to be lvalues.  Grr!  */
#define SET_BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_0(NODE)=1))
#define CLEAR_BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_0(NODE)=0))

/* Nonzero means marked in building initialization list.  */
#define BINFO_BASEINIT_MARKED(NODE) CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
/* Modifier macros */
#define SET_BINFO_BASEINIT_MARKED(NODE) SET_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
#define CLEAR_BINFO_BASEINIT_MARKED(NODE) CLEAR_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))

/* Nonzero means marked in search through virtual inheritance hierarchy.  */
#define BINFO_VBASE_MARKED(NODE) CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
/* Modifier macros */
#define SET_BINFO_VBASE_MARKED(NODE) SET_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
#define CLEAR_BINFO_VBASE_MARKED(NODE) CLEAR_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))

/* Nonzero means marked in search for members or member functions.  */
#define BINFO_FIELDS_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED2 (BINFO_TYPE (NODE)):TREE_LANG_FLAG_2(NODE))
#define SET_BINFO_FIELDS_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED2(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_2(NODE)=1))
#define CLEAR_BINFO_FIELDS_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED2(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_2(NODE)=0))

/* Nonzero means that this class is on a path leading to a new vtable.  */
#define BINFO_VTABLE_PATH_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):TREE_LANG_FLAG_3(NODE))
#define SET_BINFO_VTABLE_PATH_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_3(NODE)=1))
#define CLEAR_BINFO_VTABLE_PATH_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_3(NODE)=0))

/* Nonzero means that this class has a new vtable.  */
#define BINFO_NEW_VTABLE_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):TREE_LANG_FLAG_4(NODE))
#define SET_BINFO_NEW_VTABLE_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_4(NODE)=1))
#define CLEAR_BINFO_NEW_VTABLE_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_4(NODE)=0))

/* Nonzero means this class has initialized its virtual baseclasses.  */
#define BINFO_VBASE_INIT_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):TREE_LANG_FLAG_5(NODE))
#define SET_BINFO_VBASE_INIT_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_5(NODE)=1))
#define CLEAR_BINFO_VBASE_INIT_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_5(NODE)=0))

/* Accessor macros for the vfield slots in structures.  */

/* Get the assoc info that caused this vfield to exist.  */
#define VF_BINFO_VALUE(NODE) TREE_PURPOSE (NODE)

/* Get that same information as a _TYPE.  */
#define VF_BASETYPE_VALUE(NODE) TREE_VALUE (NODE)

/* Get the value of the top-most type dominating the non-`normal' vfields.  */
#define VF_DERIVED_VALUE(NODE) (VF_BINFO_VALUE (NODE) ? BINFO_TYPE (VF_BINFO_VALUE (NODE)) : NULL_TREE)

/* Get the value of the top-most type that's `normal' for the vfield.  */
#define VF_NORMAL_VALUE(NODE) TREE_TYPE (NODE)

/* Nonzero for TREE_LIST node means that this list of things
   is a list of parameters, as opposed to a list of expressions.  */
#define TREE_PARMLIST(NODE) ((NODE)->common.unsigned_flag) /* overloaded! */

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  */
#define TYPE_RAISES_EXCEPTIONS(NODE) TYPE_NONCOPIED_PARTS (NODE)

/* The binding level associated with the namespace.  */
#define NAMESPACE_LEVEL(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.level)

struct lang_decl_flags
{
#ifdef ONLY_INT_FIELDS
  int language : 8;
#else
  enum languages language : 8;
#endif

  unsigned operator_attr : 1;
  unsigned constructor_attr : 1;
  unsigned returns_first_arg : 1;
  unsigned preserves_first_arg : 1;
  unsigned friend_attr : 1;
  unsigned static_function : 1;
  unsigned const_memfunc : 1;
  unsigned volatile_memfunc : 1;

  unsigned abstract_virtual : 1;
  unsigned permanent_attr : 1 ;
  unsigned constructor_for_vbase_attr : 1;
  unsigned mutable_flag : 1;
  unsigned is_default_implementation : 1;
  unsigned saved_inline : 1;
  unsigned use_template : 2;

  unsigned nonconverting : 1;
  unsigned declared_inline : 1;
  unsigned not_really_extern : 1;
  unsigned dummy : 5;

  tree access;
  tree context;
  tree memfunc_pointer_to;
  tree template_info;
  struct binding_level *level;
};

struct lang_decl
{
  struct lang_decl_flags decl_flags;

  tree main_decl_variant;
  struct pending_inline *pending_inline_info;
  tree chain;
};

/* Non-zero if NODE is a _DECL with TREE_READONLY set.  */
#define TREE_READONLY_DECL_P(NODE) \
  (TREE_READONLY (NODE) && TREE_CODE_CLASS (TREE_CODE (NODE)) == 'd')

/* Non-zero iff DECL is memory-based.  The DECL_RTL of
   certain const variables might be a CONST_INT, or a REG
   in some cases.  We cannot use `memory_operand' as a test
   here because on most RISC machines, a variable's address
   is not, by itself, a legitimate address.  */
#define DECL_IN_MEMORY_P(NODE) \
  (DECL_RTL (NODE) != NULL_RTX && GET_CODE (DECL_RTL (NODE)) == MEM)

/* For FUNCTION_DECLs: return the language in which this decl
   was declared.  */
#define DECL_LANGUAGE(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.language)

/* For FUNCTION_DECLs: nonzero means that this function is a constructor.  */
#define DECL_CONSTRUCTOR_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.constructor_attr)
/* For FUNCTION_DECLs: nonzero means that this function is a constructor
   for an object with virtual baseclasses.  */
#define DECL_CONSTRUCTOR_FOR_VBASE_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.constructor_for_vbase_attr)

/* For FUNCTION_DECLs: nonzero means that this function is a default
   implementation of a signature method.  */
#define IS_DEFAULT_IMPLEMENTATION(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.is_default_implementation)

/* For FUNCTION_DECLs: nonzero means that the constructor
   is known to return a non-zero `this' unchanged.  */
#define DECL_RETURNS_FIRST_ARG(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.returns_first_arg)

/* Nonzero for FUNCTION_DECL means that this constructor is known to
   not make any assignment to `this', and therefore can be trusted
   to return it unchanged.  Otherwise, we must re-assign `current_class_ptr'
   after performing base initializations.  */
#define DECL_PRESERVES_THIS(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.preserves_first_arg)

/* Nonzero for _DECL means that this decl appears in (or will appear
   in) as a member in a RECORD_TYPE or UNION_TYPE node.  It is also for
   detecting circularity in case members are multiply defined.  In the
   case of a VAR_DECL, it is also used to determine how program storage
   should be allocated.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_FLAG_3(NODE))

/* Nonzero for FUNCTION_DECL means that this decl is just a
   friend declaration, and should not be added to the list of
   member functions for this class.  */
#define DECL_FRIEND_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.friend_attr)

/* Nonzero for FUNCTION_DECL means that this decl is a static
   member function.  */
#define DECL_STATIC_FUNCTION_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.static_function)

/* Nonzero for a class member means that it is shared between all objects
   of that class.  */
#define SHARED_MEMBER_P(NODE) \
  (TREE_CODE (NODE) == VAR_DECL || TREE_CODE (NODE) == TYPE_DECL \
   || TREE_CODE (NODE) == CONST_DECL)
				
/* Nonzero for FUNCTION_DECL means that this decl is a member function
   (static or non-static).  */
#define DECL_FUNCTION_MEMBER_P(NODE) \
 (TREE_CODE (TREE_TYPE (NODE)) == METHOD_TYPE || DECL_STATIC_FUNCTION_P (NODE))

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as const X *const.  */
#define DECL_CONST_MEMFUNC_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.const_memfunc)

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as volatile X *const.  */
#define DECL_VOLATILE_MEMFUNC_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.volatile_memfunc)

/* Nonzero for _DECL means that this member object type
   is mutable.  */
#define DECL_MUTABLE_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.mutable_flag)

/* Nonzero for _DECL means that this constructor is a non-converting
   constructor.  */
#define DECL_NONCONVERTING_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.nonconverting)

/* Nonzero for FUNCTION_DECL means that this member function
   exists as part of an abstract class's interface.  */
#define DECL_ABSTRACT_VIRTUAL_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.abstract_virtual)

/* Nonzero if allocated on permanent_obstack.  */
#define LANG_DECL_PERMANENT(LANGDECL) ((LANGDECL)->decl_flags.permanent_attr)

/* The _TYPE context in which this _DECL appears.  This field holds the
   class where a virtual function instance is actually defined, and the
   lexical scope of a friend function defined in a class body.  */
#define DECL_CLASS_CONTEXT(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.context)
#define DECL_REAL_CONTEXT(NODE) \
  ((TREE_CODE (NODE) == FUNCTION_DECL && DECL_FUNCTION_MEMBER_P (NODE)) \
   ? DECL_CLASS_CONTEXT (NODE) : DECL_CONTEXT (NODE))

/* For a FUNCTION_DECL: the chain through which the next method
   with the same name is found.  We now use TREE_CHAIN to
   walk through the methods in order of declaration.  */
#if 1
#define DECL_CHAIN(NODE) (DECL_LANG_SPECIFIC(NODE)->chain)
#else
#define DECL_CHAIN(NODE) (TREE_CHAIN (NODE))
#endif

/* In a VAR_DECL for a variable declared in a for statement,
   this is the shadowed (local) variable.  */
#define DECL_SHADOWED_FOR_VAR(NODE) DECL_RESULT(NODE)

/* Points back to the decl which caused this lang_decl to be allocated.  */
#define DECL_MAIN_VARIANT(NODE) (DECL_LANG_SPECIFIC(NODE)->main_decl_variant)

/* For a FUNCTION_DECL: if this function was declared inline inside of
   a class declaration, this is where the text for the function is
   squirreled away.  */
#define DECL_PENDING_INLINE_INFO(NODE) (DECL_LANG_SPECIFIC(NODE)->pending_inline_info)

/* True if on the saved_inlines (see decl2.c) list.  */
#define DECL_SAVED_INLINE(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->decl_flags.saved_inline)

/* For a FUNCTION_DECL: if this function was declared inside a signature
   declaration, this is the corresponding member function pointer that was
   created for it.  */
#define DECL_MEMFUNC_POINTER_TO(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.memfunc_pointer_to)

/* For a FIELD_DECL: this points to the signature member function from
   which this signature member function pointer was created.  */
#define DECL_MEMFUNC_POINTING_TO(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.memfunc_pointer_to)

/* For a VAR_DECL or FUNCTION_DECL: template-specific information.  */
#define DECL_TEMPLATE_INFO(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.template_info)
#define CLASSTYPE_TEMPLATE_INFO(NODE) (TYPE_LANG_SPECIFIC(NODE)->template_info)
#define TI_TEMPLATE(NODE) (TREE_PURPOSE (NODE))
#define TI_ARGS(NODE) (TREE_VALUE (NODE))
#define TI_SPEC_INFO(NODE) (TREE_CHAIN (NODE))
#define TI_USES_TEMPLATE_PARMS(NODE) TREE_LANG_FLAG_0 (NODE)
#define TI_PENDING_TEMPLATE_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)
#define DECL_TI_TEMPLATE(NODE)      TI_TEMPLATE (DECL_TEMPLATE_INFO (NODE))
#define DECL_TI_ARGS(NODE)          TI_ARGS (DECL_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_TEMPLATE(NODE) TI_TEMPLATE (CLASSTYPE_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_ARGS(NODE)     TI_ARGS (CLASSTYPE_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_SPEC_INFO(NODE) TI_SPEC_INFO (CLASSTYPE_TEMPLATE_INFO (NODE))
#define INNERMOST_TEMPLATE_PARMS(NODE)  TREE_VALUE(NODE)

#define DECL_SAVED_TREE(NODE)		DECL_MEMFUNC_POINTER_TO (NODE)
#define COMPOUND_STMT_NO_SCOPE(NODE)	TREE_LANG_FLAG_0 (NODE)
#define NEW_EXPR_USE_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)
#define DELETE_EXPR_USE_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)
#define DELETE_EXPR_USE_VEC(NODE)	TREE_LANG_FLAG_1 (NODE)
#define LOOKUP_EXPR_GLOBAL(NODE)	TREE_LANG_FLAG_0 (NODE)

/* Nonzero in INT_CST means that this int is negative by dint of
   using a twos-complement negated operand.  */
#define TREE_NEGATED_INT(NODE) (TREE_LANG_FLAG_0 (NODE))

/* Nonzero in any kind of _EXPR or _REF node means that it is a call
   to a storage allocation routine.  If, later, alternate storage
   is found to hold the object, this call can be ignored.  */
#define TREE_CALLS_NEW(NODE) (TREE_LANG_FLAG_1 (NODE))

/* Nonzero in any kind of _TYPE that uses multiple inheritance
   or virtual baseclasses.  */
#define TYPE_USES_COMPLEX_INHERITANCE(NODE) (TREE_LANG_FLAG_1 (NODE))

#if 0				/* UNUSED */
/* Nonzero in IDENTIFIER_NODE means that this name is not the name the user
   gave; it's a DECL_NESTED_TYPENAME.  Someone may want to set this on
   mangled function names, too, but it isn't currently.  */
#define TREE_MANGLED(NODE) (TREE_LANG_FLAG_0 (NODE))
#endif

#if 0				/* UNUSED */
/* Nonzero in IDENTIFIER_NODE means that this name is overloaded, and
   should be looked up in a non-standard way.  */
#define DECL_OVERLOADED(NODE) (FOO)
#endif

/* Nonzero if this (non-TYPE)_DECL has its virtual attribute set.
   For a FUNCTION_DECL, this is when the function is a virtual function.
   For a VAR_DECL, this is when the variable is a virtual function table.
   For a FIELD_DECL, when the field is the field for the virtual function table.
   For an IDENTIFIER_NODE, nonzero if any function with this name
   has been declared virtual.

   For a _TYPE if it uses virtual functions (or is derived from
   one that does).  */
#define TYPE_VIRTUAL_P(NODE) (TREE_LANG_FLAG_2 (NODE))

#if 0
/* Same, but tells if this field is private in current context.  */
#define DECL_PRIVATE(NODE) (FOO)

/* Same, but tells if this field is private in current context.  */
#define DECL_PROTECTED(NODE) (DECL_LANG_FLAG_6 (NODE))

#define DECL_PUBLIC(NODE) (DECL_LANG_FLAG_7 (NODE))
#endif

extern int flag_new_for_scope;

/* This flag is true of a local VAR_DECL if it was declared in a for
   statement, but we are no longer in the scope of the for.  */
#define DECL_DEAD_FOR_LOCAL(NODE) DECL_LANG_FLAG_7 (NODE)

/* This flag is set on a VAR_DECL that is a DECL_DEAD_FOR_LOCAL
   if we already emitted a warning about using it.  */
#define DECL_ERROR_REPORTED(NODE) DECL_LANG_FLAG_0 (NODE)

/* This _DECL represents a compiler-generated entity.  */
#define SET_DECL_ARTIFICIAL(NODE) (DECL_ARTIFICIAL (NODE) = 1)

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(exp) DECL_LANG_FLAG_1 ((exp))

/* Nonzero if the type T promotes to itself.
   ANSI C states explicitly the list of types that promote;
   in particular, short promotes to int even if they have the same width.  */
#define C_PROMOTING_INTEGER_TYPE_P(t)				\
  (TREE_CODE ((t)) == INTEGER_TYPE				\
   && (TYPE_MAIN_VARIANT (t) == char_type_node			\
       || TYPE_MAIN_VARIANT (t) == signed_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == unsigned_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_integer_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_unsigned_type_node))

#define INTEGRAL_CODE_P(CODE) \
  (CODE == INTEGER_TYPE || CODE == ENUMERAL_TYPE || CODE == BOOLEAN_TYPE)
#define ARITHMETIC_TYPE_P(TYPE) (INTEGRAL_TYPE_P (TYPE) || FLOAT_TYPE_P (TYPE))

/* Mark which labels are explicitly declared.
   These may be shadowed, and may be referenced from nested functions.  */
#define C_DECLARED_LABEL_FLAG(label) TREE_LANG_FLAG_1 (label)

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(type) TREE_LANG_FLAG_4 (type)
#define C_DECL_VARIABLE_SIZE(type) DECL_LANG_FLAG_8 (type)

/* Nonzero for _TYPE means that the _TYPE defines
   at least one constructor.  */
#define TYPE_HAS_CONSTRUCTOR(NODE) (TYPE_LANG_FLAG_1(NODE))

/* When appearing in an INDIRECT_REF, it means that the tree structure
   underneath is actually a call to a constructor.  This is needed
   when the constructor must initialize local storage (which can
   be automatically destroyed), rather than allowing it to allocate
   space from the heap.

   When appearing in a SAVE_EXPR, it means that underneath
   is a call to a constructor.

   When appearing in a CONSTRUCTOR, it means that it was
   a GNU C constructor expression.

   When appearing in a FIELD_DECL, it means that this field
   has been duly initialized in its constructor.  */
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_4(NODE))

#define EMPTY_CONSTRUCTOR_P(NODE) (TREE_CODE (NODE) == CONSTRUCTOR \
				   && CONSTRUCTOR_ELTS (NODE) == NULL_TREE)

#if 0
/* Indicates that a NON_LVALUE_EXPR came from a C++ reference.
   Used to generate more helpful error message in case somebody
   tries to take its address.  */
#define TREE_REFERENCE_EXPR(NODE) (TREE_LANG_FLAG_3(NODE))
#endif

/* Nonzero for _TYPE means that the _TYPE defines a destructor.  */
#define TYPE_HAS_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_2(NODE))

#if 0
/* Nonzero for _TYPE node means that creating an object of this type
   will involve a call to a constructor.  This can apply to objects
   of ARRAY_TYPE if the type of the elements needs a constructor.  */
#define TYPE_NEEDS_CONSTRUCTING(NODE) (TYPE_LANG_FLAG_3(NODE))
#endif

/* Nonzero means that an object of this type can not be initialized using
   an initializer list.  */
#define CLASSTYPE_NON_AGGREGATE(NODE) \
  (TYPE_LANG_SPECIFIC (NODE)->type_flags.non_aggregate)
#define TYPE_NON_AGGREGATE_CLASS(NODE) \
  (IS_AGGR_TYPE (NODE) && CLASSTYPE_NON_AGGREGATE (NODE))

/* Nonzero if there is a user-defined X::op=(x&) for this class.  */
#define TYPE_HAS_REAL_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_real_assign_ref)
#define TYPE_HAS_COMPLEX_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_complex_assign_ref)
#define TYPE_HAS_ABSTRACT_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_abstract_assign_ref)
#define TYPE_HAS_COMPLEX_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_complex_init_ref)

/* Nonzero for _TYPE node means that destroying an object of this type
   will involve a call to a destructor.  This can apply to objects
   of ARRAY_TYPE is the type of the elements needs a destructor.  */
#define TYPE_NEEDS_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_4(NODE))

/* Nonzero for class type means that initialization of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_INIT_REF(NODE) \
  (TYPE_HAS_INIT_REF (NODE) && ! TYPE_HAS_COMPLEX_INIT_REF (NODE))

/* Nonzero for class type means that assignment of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_ASSIGN_REF(NODE) \
  (TYPE_HAS_ASSIGN_REF (NODE) && ! TYPE_HAS_COMPLEX_ASSIGN_REF (NODE))

#define TYPE_PTRMEM_P(NODE)					\
  (TREE_CODE (NODE) == POINTER_TYPE				\
   && TREE_CODE (TREE_TYPE (NODE)) == OFFSET_TYPE)
#define TYPE_PTR_P(NODE)				\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) != OFFSET_TYPE)
#define TYPE_PTROB_P(NODE)						\
  (TYPE_PTR_P (NODE) && TREE_CODE (TREE_TYPE (NODE)) != FUNCTION_TYPE	\
   && TREE_CODE (TREE_TYPE (NODE)) != VOID_TYPE)
#define TYPE_PTROBV_P(NODE)						\
  (TYPE_PTR_P (NODE) && TREE_CODE (TREE_TYPE (NODE)) != FUNCTION_TYPE)
#define TYPE_PTRFN_P(NODE)				\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) == FUNCTION_TYPE)

/* Nonzero for _TYPE node means that this type is a pointer to member
   function type.  */
#define TYPE_PTRMEMFUNC_P(NODE) (TREE_CODE(NODE) == RECORD_TYPE && TYPE_LANG_SPECIFIC(NODE)->type_flags.ptrmemfunc_flag)
#define TYPE_PTRMEMFUNC_FLAG(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.ptrmemfunc_flag)
/* Get the POINTER_TYPE to the METHOD_TYPE associated with this
   pointer to member function.  TYPE_PTRMEMFUNC_P _must_ be true,
   before using this macro.  */
#define TYPE_PTRMEMFUNC_FN_TYPE(NODE) (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (NODE)))))))
/* These are use to manipulate the the canonical RECORD_TYPE from the
   hashed POINTER_TYPE, and can only be used on the POINTER_TYPE.  */
#define TYPE_GET_PTRMEMFUNC_TYPE(NODE) ((tree)TYPE_LANG_SPECIFIC(NODE))
#define TYPE_SET_PTRMEMFUNC_TYPE(NODE, VALUE) (TYPE_LANG_SPECIFIC(NODE) = ((struct lang_type *)(void*)(VALUE)))
/* These are to get the delta2 and pfn fields from a TYPE_PTRMEMFUNC_P.  */
#define DELTA2_FROM_PTRMEMFUNC(NODE) (build_component_ref (build_component_ref ((NODE), pfn_or_delta2_identifier, NULL_TREE, 0), delta2_identifier, NULL_TREE, 0))
#define PFN_FROM_PTRMEMFUNC(NODE) (build_component_ref (build_component_ref ((NODE), pfn_or_delta2_identifier, NULL_TREE, 0), pfn_identifier, NULL_TREE, 0))

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `extern' was
   specified in its declaration.  */
#define DECL_THIS_EXTERN(NODE) (DECL_LANG_FLAG_2(NODE))

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `static' was
   specified in its declaration.  */
#define DECL_THIS_STATIC(NODE) (DECL_LANG_FLAG_6(NODE))

/* Nonzero for SAVE_EXPR if used to initialize a PARM_DECL.  */
#define PARM_DECL_EXPR(NODE) (TREE_LANG_FLAG_2(NODE))

/* Nonzero in FUNCTION_DECL means it is really an operator.
   Just used to communicate formatting information to dbxout.c.  */
#define DECL_OPERATOR(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.operator_attr)

#define ANON_UNION_P(NODE) (DECL_NAME (NODE) == 0)

#define UNKNOWN_TYPE LANG_TYPE

/* Define fields and accessors for nodes representing declared names.  */

#if 0
/* C++: A derived class may be able to directly use the virtual
   function table of a base class.  When it does so, it may
   still have a decl node used to access the virtual function
   table (so that variables of this type can initialize their
   virtual function table pointers by name).  When such thievery
   is committed, know exactly which base class's virtual function
   table is the one being stolen.  This effectively computes the
   transitive closure.  */
#define DECL_VPARENT(NODE) ((NODE)->decl.arguments)
#endif

#define TYPE_WAS_ANONYMOUS(NODE) (TYPE_LANG_SPECIFIC (NODE)->type_flags.was_anonymous)

/* C++: all of these are overloaded!  These apply only to TYPE_DECLs.  */
#define DECL_FRIENDLIST(NODE)		(DECL_INITIAL (NODE))

/* The DECL_ACCESS is used to record under which context
   special access rules apply.  */
#define DECL_ACCESS(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.access)

/* C++: all of these are overloaded!
   These apply to PARM_DECLs and VAR_DECLs.  */
#define DECL_REFERENCE_SLOT(NODE) ((tree)(NODE)->decl.arguments)
#define SET_DECL_REFERENCE_SLOT(NODE,VAL) ((NODE)->decl.arguments=VAL)

/* Accessor macros for C++ template decl nodes.  */
#define DECL_TEMPLATE_PARMS(NODE)       DECL_ARGUMENTS(NODE)
#define DECL_INNERMOST_TEMPLATE_PARMS(NODE) \
   INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (NODE))
#define DECL_NTPARMS(NODE) \
   TREE_VEC_LENGTH (DECL_INNERMOST_TEMPLATE_PARMS (NODE))
/* For class templates.  */
#define DECL_TEMPLATE_SPECIALIZATIONS(NODE)     DECL_SIZE(NODE)
/* For function, method, class-data templates.  */
#define DECL_TEMPLATE_RESULT(NODE)      DECL_RESULT(NODE)
#define DECL_TEMPLATE_INSTANTIATIONS(NODE) DECL_VINDEX(NODE)
#define DECL_TEMPLATE_INJECT(NODE)	DECL_INITIAL(NODE)

#define DECL_FUNCTION_TEMPLATE_P(NODE)  \
  (TREE_CODE (NODE) == TEMPLATE_DECL \
   && TREE_CODE (DECL_TEMPLATE_RESULT (NODE)) == FUNCTION_DECL)

#define PRIMARY_TEMPLATE_P(NODE) \
  (TREE_TYPE (DECL_INNERMOST_TEMPLATE_PARMS (NODE)) == (NODE))

#define CLASSTYPE_TEMPLATE_LEVEL(NODE) \
  (TREE_INT_CST_HIGH (TREE_PURPOSE (CLASSTYPE_TI_TEMPLATE (NODE))))

/* Indicates whether or not (and how) a template was expanded for this
   FUNCTION_DECL or VAR_DECL.
     0=normal declaration, e.g. int min (int, int);
     1=implicit template instantiation
     2=explicit template specialization, e.g. int min<int> (int, int);
     3=explicit template instantiation, e.g. template int min<int> (int, int);  */
#define DECL_USE_TEMPLATE(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.use_template)

#define DECL_TEMPLATE_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) & 1)
#define CLASSTYPE_TEMPLATE_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) & 1)

#define DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) == 2)
#define SET_DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) = 2)
#define CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 2)
#define SET_CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 2)

#define DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 1)
#define SET_DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 1)
#define CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE(NODE) == 1)
#define SET_CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE(NODE) = 1)

#define DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 3)
#define SET_DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 3)
#define CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE(NODE) == 3)
#define SET_CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE(NODE) = 3)

/* This function may be a guiding decl for a template.  */
#define DECL_MAYBE_TEMPLATE(NODE) DECL_LANG_FLAG_4 (NODE)
/* We know what we're doing with this decl now.  */
#define DECL_INTERFACE_KNOWN(NODE) DECL_LANG_FLAG_5 (NODE)

/* This function was declared inline.  This flag controls the linkage
   semantics of 'inline'; whether or not the function is inlined is
   controlled by DECL_INLINE.  */
#define DECL_THIS_INLINE(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.declared_inline)

/* DECL_EXTERNAL must be set on a decl until the decl is actually emitted,
   so that assemble_external will work properly.  So we have this flag to
   tell us whether the decl is really not external.  */
#define DECL_NOT_REALLY_EXTERN(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->decl_flags.not_really_extern)

#define DECL_REALLY_EXTERN(NODE) \
  (DECL_EXTERNAL (NODE) && ! DECL_NOT_REALLY_EXTERN (NODE))

#define THUNK_DELTA(DECL) ((DECL)->decl.frame_size.i)

/* ...and for unexpanded-parameterized-type nodes.  */
#define UPT_TEMPLATE(NODE)      TREE_PURPOSE(TYPE_VALUES(NODE))
#define UPT_PARMS(NODE)         TREE_VALUE(TYPE_VALUES(NODE))

/* An un-parsed default argument looks like an identifier.  */
#define DEFARG_LENGTH(NODE)	IDENTIFIER_LENGTH(NODE)
#define DEFARG_POINTER(NODE)	IDENTIFIER_POINTER(NODE)

#define builtin_function(NAME, TYPE, CODE, LIBNAME) \
  define_function (NAME, TYPE, CODE, (void (*) PROTO((tree)))pushdecl, LIBNAME)

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types { record_type, class_type, union_type, enum_type,
		   signature_type };

/* Zero means prototype weakly, as in ANSI C (no args means nothing).
   Each language context defines how this variable should be set.  */
extern int strict_prototype;
extern int strict_prototypes_lang_c, strict_prototypes_lang_cplusplus;

/* Non-zero means that if a label exists, and no other identifier
   applies, use the value of the label.  */
extern int flag_labels_ok;

/* Non-zero means to collect statistics which might be expensive
   and to print them when we are done.  */
extern int flag_detailed_statistics;

/* Non-zero means warn in function declared in derived class has the
   same name as a virtual in the base class, but fails to match the
   type signature of any virtual function in the base class.  */
extern int warn_overloaded_virtual;

/* in c-common.c */
extern void declare_function_name               PROTO((void));
extern void decl_attributes                     PROTO((tree, tree, tree));
extern void init_function_format_info		PROTO((void));
extern void record_function_format		PROTO((tree, tree, int, int, int));
extern void check_function_format		PROTO((tree, tree, tree));
/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */
extern void binary_op_error                     PROTO((enum tree_code));
extern tree cp_build_type_variant                PROTO((tree, int, int));
extern void c_expand_expr_stmt                  PROTO((tree));
/* Validate the expression after `case' and apply default promotions.  */
extern tree check_case_value                    PROTO((tree));
/* Concatenate a list of STRING_CST nodes into one STRING_CST.  */
extern tree combine_strings                     PROTO((tree));
extern void constant_expression_warning         PROTO((tree));
extern tree convert_and_check			PROTO((tree, tree));
extern void overflow_warning			PROTO((tree));
extern void unsigned_conversion_warning		PROTO((tree, tree));
/* Read the rest of the current #-directive line.  */
extern char *get_directive_line                 PROTO((FILE *));
/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.  */
extern tree shorten_compare                     PROTO((tree *, tree *, tree *, enum tree_code *));
/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.  */
extern tree truthvalue_conversion               PROTO((tree));
extern tree type_for_mode                       PROTO((enum machine_mode, int));
extern tree type_for_size                       PROTO((unsigned, int));

/* in decl{2}.c */
extern tree void_list_node;
extern tree void_zero_node;
extern tree default_function_type;
extern tree vtable_entry_type;
extern tree sigtable_entry_type;
extern tree __t_desc_type_node;
#if 0
extern tree __tp_desc_type_node;
#endif
extern tree __access_mode_type_node;
extern tree __bltn_desc_type_node, __user_desc_type_node;
extern tree __class_desc_type_node, __attr_desc_type_node;
extern tree __ptr_desc_type_node, __func_desc_type_node;
extern tree __ptmf_desc_type_node, __ptmd_desc_type_node;
extern tree type_info_type_node;
extern tree class_star_type_node;
extern tree this_identifier;
extern tree ctor_identifier, dtor_identifier;
extern tree pfn_identifier;
extern tree index_identifier;
extern tree delta_identifier;
extern tree delta2_identifier;
extern tree pfn_or_delta2_identifier;
extern tree tag_identifier;
extern tree vt_off_identifier;

/* A node that is a list (length 1) of error_mark_nodes.  */
extern tree error_mark_list;

extern tree ptr_type_node;
extern tree class_type_node, record_type_node, union_type_node, enum_type_node;
extern tree unknown_type_node;
extern tree opaque_type_node, signature_type_node;

/* Node for "pointer to (virtual) function".
   This may be distinct from ptr_type_node so gdb can distinguish them.  */
#define vfunc_ptr_type_node \
  (flag_vtable_thunks ? vtable_entry_type : ptr_type_node)

/* Array type `(void *)[]' */
extern tree vtbl_type_node;
extern tree delta_type_node;
extern tree std_node;

extern tree long_long_integer_type_node, long_long_unsigned_type_node;
/* For building calls to `delete'.  */
extern tree integer_two_node, integer_three_node;
extern tree boolean_type_node, boolean_true_node, boolean_false_node;

extern tree null_node;

/* in pt.c  */

extern tree current_template_parms;
extern HOST_WIDE_INT processing_template_decl;

/* The template currently being instantiated, and where the instantiation
   was triggered.  */
struct tinst_level
{
  tree decl;
  int line;
  char *file;
  struct tinst_level *next;
};

extern int minimal_parse_mode;

/* in class.c */
extern tree current_class_name;
extern tree current_class_type;
extern tree current_class_ptr;
extern tree previous_class_type;
extern tree current_class_ref;

extern tree current_lang_name, lang_name_cplusplus, lang_name_c;

/* Points to the name of that function. May not be the DECL_NAME
   of CURRENT_FUNCTION_DECL due to overloading */
extern tree original_function_name;

/* in init.c  */
extern tree global_base_init_list;
extern tree current_base_init_list, current_member_init_list;

extern int current_function_just_assigned_this;
extern int current_function_parms_stored;

/* Here's where we control how name mangling takes place.  */

#define OPERATOR_ASSIGN_FORMAT "__a%s"
#define OPERATOR_FORMAT "__%s"
#define OPERATOR_TYPENAME_FORMAT "__op"
#define OPERATOR_TYPENAME_P(ID_NODE) \
  (IDENTIFIER_POINTER (ID_NODE)[0] == '_'	\
   && IDENTIFIER_POINTER (ID_NODE)[1] == '_'	\
   && IDENTIFIER_POINTER (ID_NODE)[2] == 'o'	\
   && IDENTIFIER_POINTER (ID_NODE)[3] == 'p')


/* Cannot use '$' up front, because this confuses gdb
   (names beginning with '$' are gdb-local identifiers).

   Note that all forms in which the '$' is significant are long enough
   for direct indexing (meaning that if we know there is a '$'
   at a particular location, we can index into the string at
   any other location that provides distinguishing characters).  */

/* Define NO_DOLLAR_IN_LABEL in your favorite tm file if your assembler
   doesn't allow '$' in symbol names.  */
#ifndef NO_DOLLAR_IN_LABEL

#define JOINER '$'

#define VPTR_NAME "$v"
#define THROW_NAME "$eh_throw"
#define DESTRUCTOR_DECL_PREFIX "_$_"
#define AUTO_VTABLE_NAME "__vtbl$me__"
#define AUTO_TEMP_NAME "_$tmp_"
#define AUTO_TEMP_FORMAT "_$tmp_%d"
#define VTABLE_BASE "$vb"
#define VTABLE_NAME_FORMAT (flag_vtable_thunks ? "__vt_%s" : "_vt$%s")
#define VFIELD_BASE "$vf"
#define VFIELD_NAME "_vptr$"
#define VFIELD_NAME_FORMAT "_vptr$%s"
#define VBASE_NAME "_vb$"
#define VBASE_NAME_FORMAT "_vb$%s"
#define STATIC_NAME_FORMAT "_%s$%s"
#define ANON_AGGRNAME_FORMAT "$_%d"

#else /* NO_DOLLAR_IN_LABEL */

#ifndef NO_DOT_IN_LABEL

#define JOINER '.'

#define VPTR_NAME ".v"
#define THROW_NAME ".eh_throw"
#define DESTRUCTOR_DECL_PREFIX "_._"
#define AUTO_VTABLE_NAME "__vtbl.me__"
#define AUTO_TEMP_NAME "_.tmp_"
#define AUTO_TEMP_FORMAT "_.tmp_%d"
#define VTABLE_BASE ".vb"
#define VTABLE_NAME_FORMAT (flag_vtable_thunks ? "__vt_%s" : "_vt.%s")
#define VFIELD_BASE ".vf"
#define VFIELD_NAME "_vptr."
#define VFIELD_NAME_FORMAT "_vptr.%s"
#define VBASE_NAME "_vb."
#define VBASE_NAME_FORMAT "_vb.%s"
#define STATIC_NAME_FORMAT "_%s.%s"

#define ANON_AGGRNAME_FORMAT "._%d"

#else /* NO_DOT_IN_LABEL */

#define VPTR_NAME "__vptr"
#define VPTR_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VPTR_NAME, sizeof (VPTR_NAME) - 1))
#define THROW_NAME "__eh_throw"
#define DESTRUCTOR_DECL_PREFIX "__destr_"
#define DESTRUCTOR_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), DESTRUCTOR_DECL_PREFIX, \
	     sizeof (DESTRUCTOR_DECL_PREFIX) - 1))
#define IN_CHARGE_NAME "__in_chrg"
#define AUTO_VTABLE_NAME "__vtbl_me__"
#define AUTO_TEMP_NAME "__tmp_"
#define TEMP_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, \
	     sizeof (AUTO_TEMP_NAME) - 1))
#define AUTO_TEMP_FORMAT "__tmp_%d"
#define VTABLE_BASE "__vtb"
#define VTABLE_NAME "__vt_"
#define VTABLE_NAME_FORMAT (flag_vtable_thunks ? "__vt_%s" : "_vt_%s")
#define VTABLE_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VTABLE_NAME, \
	     sizeof (VTABLE_NAME) - 1))
#define VFIELD_BASE "__vfb"
#define VFIELD_NAME "__vptr_"
#define VFIELD_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, \
	    sizeof (VFIELD_NAME) - 1))
#define VFIELD_NAME_FORMAT "_vptr_%s"
#define VBASE_NAME "__vb_"
#define VBASE_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VBASE_NAME, \
	     sizeof (VBASE_NAME) - 1))
#define VBASE_NAME_FORMAT "__vb_%s"
#define STATIC_NAME_FORMAT "__static_%s_%s"

#define ANON_AGGRNAME_PREFIX "__anon_"
#define ANON_AGGRNAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), ANON_AGGRNAME_PREFIX, \
	     sizeof (ANON_AGGRNAME_PREFIX) - 1))
#define ANON_AGGRNAME_FORMAT "__anon_%d"
#define ANON_PARMNAME_FORMAT "__%d"
#define ANON_PARMNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == '_' \
				  && IDENTIFIER_POINTER (ID_NODE)[1] == '_' \
				  && IDENTIFIER_POINTER (ID_NODE)[2] <= '9')

#endif	/* NO_DOT_IN_LABEL */
#endif	/* NO_DOLLAR_IN_LABEL */

#define THIS_NAME "this"
#define DESTRUCTOR_NAME_FORMAT "~%s"
#define FILE_FUNCTION_PREFIX_LEN 9
#define CTOR_NAME "__ct"
#define DTOR_NAME "__dt"

#define IN_CHARGE_NAME "__in_chrg"

#define VTBL_PTR_TYPE		"__vtbl_ptr_type"
#define VTABLE_DELTA_NAME	"__delta"
#define VTABLE_INDEX_NAME	"__index"
#define VTABLE_PFN_NAME		"__pfn"
#define VTABLE_DELTA2_NAME	"__delta2"

#define SIGNATURE_FIELD_NAME	"__s_"
#define SIGNATURE_FIELD_NAME_FORMAT "__s_%s"
#define SIGNATURE_OPTR_NAME	"__optr"
#define SIGNATURE_SPTR_NAME	"__sptr"
#define SIGNATURE_POINTER_NAME	"__sp_"
#define SIGNATURE_POINTER_NAME_FORMAT "__%s%ssp_%s"
#define SIGNATURE_REFERENCE_NAME "__sr_"
#define SIGNATURE_REFERENCE_NAME_FORMAT "__%s%ssr_%s"

#define SIGTABLE_PTR_TYPE	"__sigtbl_ptr_type"
#define SIGTABLE_NAME_FORMAT	"__st_%s_%s"
#define SIGTABLE_NAME_FORMAT_LONG "__st_%s_%s_%d"
#define SIGTABLE_TAG_NAME	"__tag"
#define SIGTABLE_VB_OFF_NAME	"__vb_off"
#define SIGTABLE_VT_OFF_NAME	"__vt_off"
#define EXCEPTION_CLEANUP_NAME 	"exception cleanup"

#define THIS_NAME_P(ID_NODE) (strcmp(IDENTIFIER_POINTER (ID_NODE), "this") == 0)

#if !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL)

#define VPTR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
			      && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')
#define DESTRUCTOR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == JOINER \
                                    && IDENTIFIER_POINTER (ID_NODE)[2] == '_') 

#define VTABLE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == 'v' \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 't' \
  && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)

#define VBASE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == 'v' \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 'b' \
  && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)

#define TEMP_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, sizeof (AUTO_TEMP_NAME)-1))
#define VFIELD_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, sizeof(VFIELD_NAME)-1))

/* For anonymous aggregate types, we need some sort of name to
   hold on to.  In practice, this should not appear, but it should
   not be harmful if it does.  */
#define ANON_AGGRNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
				  && IDENTIFIER_POINTER (ID_NODE)[1] == '_')
#define ANON_PARMNAME_FORMAT "_%d"
#define ANON_PARMNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == '_' \
				  && IDENTIFIER_POINTER (ID_NODE)[1] <= '9')
#endif /* !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL) */

/* Define the sets of attributes that member functions and baseclasses
   can have.  These are sensible combinations of {public,private,protected}
   cross {virtual,non-virtual}.  */

/* in class.c.  */
extern tree access_default_node; /* 0 */
extern tree access_public_node; /* 1 */
extern tree access_protected_node; /* 2 */
extern tree access_private_node; /* 3 */
extern tree access_default_virtual_node; /* 4 */
extern tree access_public_virtual_node; /* 5 */
extern tree access_private_virtual_node; /* 6 */

/* Things for handling inline functions.  */

struct pending_inline
{
  struct pending_inline *next;	/* pointer to next in chain */
  int lineno;			/* line number we got the text from */
  char *filename;		/* name of file we were processing */
  tree fndecl;			/* FUNCTION_DECL that brought us here */
  int token;			/* token we were scanning */
  int token_value;		/* value of token we were scanning (YYSTYPE) */

  char *buf;			/* pointer to character stream */
  int len;			/* length of stream */
  unsigned int can_free : 1;	/* free this after we're done with it? */
  unsigned int deja_vu : 1;	/* set iff we don't want to see it again.  */
  unsigned int interface : 2;	/* 0=interface 1=unknown 2=implementation */
};

/* in method.c */
extern struct pending_inline *pending_inlines;

/* 1 for -fall-virtual: make every member function (except
   constructors) lay down in the virtual function table.
   Calls can then either go through the virtual function table or not,
   depending on whether we know what function will actually be called.  */

extern int flag_all_virtual;

/* Positive values means that we cannot make optimizing assumptions about
   `this'.  Negative values means we know `this' to be of static type.  */

extern int flag_this_is_variable;

/* Controls whether enums and ints freely convert.
   1 means with complete freedom.
   0 means enums can convert to ints, but not vice-versa.  */

extern int flag_int_enum_equivalence;

/* Nonzero means generate 'rtti' that give run-time type information.  */

extern int flag_rtti;

/* Nonzero means do emit exported implementations of functions even if
   they can be inlined.  */

extern int flag_implement_inlines;

/* Nonzero means templates obey #pragma interface and implementation.  */

extern int flag_external_templates;

/* Nonzero means templates are emitted where they are instantiated.  */

extern int flag_alt_external_templates;

/* Nonzero means implicit template instantiations are emitted.  */

extern int flag_implicit_templates;

/* Nonzero if we want to emit defined symbols with common-like linkage as
   weak symbols where possible, in order to conform to C++ semantics.
   Otherwise, emit them as local symbols.  */

extern int flag_weak;

/* Nonzero if we're done parsing and into end-of-file activities.  */

extern int at_eof;

enum overload_flags { NO_SPECIAL = 0, DTOR_FLAG, OP_FLAG, TYPENAME_FLAG };

/* The following two can be derived from the previous one */
extern tree current_class_name;	/* IDENTIFIER_NODE: name of current class */
extern tree current_class_type;	/* _TYPE: the type of the current class */

/* Some macros for char-based bitfields.  */
#define B_SET(a,x) (a[x>>3] |= (1 << (x&7)))
#define B_CLR(a,x) (a[x>>3] &= ~(1 << (x&7)))
#define B_TST(a,x) (a[x>>3] & (1 << (x&7)))

/* These are uses as bits in flags passed to build_method_call
   to control its error reporting behavior.

   LOOKUP_PROTECT means flag access violations.
   LOOKUP_COMPLAIN mean complain if no suitable member function
     matching the arguments is found.
   LOOKUP_NORMAL is just a combination of these two.
   LOOKUP_NONVIRTUAL means make a direct call to the member function found
   LOOKUP_GLOBAL means search through the space of overloaded functions,
     as well as the space of member functions.
   LOOKUP_HAS_IN_CHARGE means that the "in charge" variable is already
     in the parameter list.
   LOOKUP_ONLYCONVERTING means that non-conversion constructors are not tried.
   DIRECT_BIND means that if a temporary is created, it should be created so
     that it lives as long as the current variable bindings; otherwise it
     only lives until the end of the complete-expression.
   LOOKUP_SPECULATIVELY means return NULL_TREE if we cannot find what we are
     after.  Note, LOOKUP_COMPLAIN is checked and error messages printed
     before LOOKUP_SPECULATIVELY is checked.
   LOOKUP_NO_CONVERSION means that user-defined conversions are not
     permitted.  Built-in conversions are permitted.
   LOOKUP_DESTRUCTOR means explicit call to destructor.
   LOOKUP_NO_TEMP_BIND means temporaries will not be bound to references.  */

#define LOOKUP_PROTECT (1)
#define LOOKUP_COMPLAIN (2)
#define LOOKUP_NORMAL (3)
/* #define LOOKUP_UNUSED (4) */
#define LOOKUP_NONVIRTUAL (8)
#define LOOKUP_GLOBAL (16)
#define LOOKUP_HAS_IN_CHARGE (32)
#define LOOKUP_SPECULATIVELY (64)
#define LOOKUP_ONLYCONVERTING (128)
#define DIRECT_BIND (256)
#define LOOKUP_NO_CONVERSION (512)
#define LOOKUP_DESTRUCTOR (512)
#define LOOKUP_NO_TEMP_BIND (1024)

/* These flags are used by the conversion code.
   CONV_IMPLICIT   :  Perform implicit conversions (standard and user-defined).
   CONV_STATIC     :  Perform the explicit conversions for static_cast.
   CONV_CONST      :  Perform the explicit conversions for const_cast.
   CONV_REINTERPRET:  Perform the explicit conversions for reinterpret_cast.
   CONV_PRIVATE    :  Perform upcasts to private bases.
   CONV_FORCE_TEMP :  Require a new temporary when converting to the same
   		      aggregate type.  */

#define CONV_IMPLICIT    1
#define CONV_STATIC      2
#define CONV_CONST       4
#define CONV_REINTERPRET 8
#define CONV_PRIVATE	 16
/* #define CONV_NONCONVERTING 32 */
#define CONV_FORCE_TEMP  64
#define CONV_STATIC_CAST (CONV_IMPLICIT | CONV_STATIC | CONV_FORCE_TEMP)
#define CONV_OLD_CONVERT (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET)
#define CONV_C_CAST      (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET | CONV_PRIVATE | CONV_FORCE_TEMP)

/* Used by build_expr_type_conversion to indicate which types are
   acceptable as arguments to the expression under consideration.  */

#define WANT_INT	1 /* integer types, including bool */
#define WANT_FLOAT	2 /* floating point types */
#define WANT_ENUM	4 /* enumerated types */
#define WANT_POINTER	8 /* pointer types */
#define WANT_NULL      16 /* null pointer constant */

#define WANT_ARITH	(WANT_INT | WANT_FLOAT)

/* Anatomy of a DECL_FRIENDLIST (which is a TREE_LIST):
   purpose = friend name (IDENTIFIER_NODE);
   value = TREE_LIST of FUNCTION_DECLS;
   chain, type = EMPTY;  */
#define FRIEND_NAME(LIST) (TREE_PURPOSE (LIST))
#define FRIEND_DECLS(LIST) (TREE_VALUE (LIST))

/* These macros are for accessing the fields of TEMPLATE...PARM nodes.  */
#define TEMPLATE_TYPE_IDX(NODE) TREE_INT_CST_LOW (TYPE_FIELDS (NODE))
#define TEMPLATE_TYPE_LEVEL(NODE) TREE_INT_CST_HIGH (TYPE_FIELDS (NODE))
#define TEMPLATE_TYPE_SET_INFO(NODE,I,L) \
  (TYPE_FIELDS (NODE) = build_int_2 (I, L))
#define TEMPLATE_CONST_IDX(NODE) (TREE_INT_CST_LOW(NODE))
#define TEMPLATE_CONST_LEVEL(NODE) (TREE_INT_CST_HIGH(NODE))
#define TEMPLATE_CONST_SET_INFO(NODE,I,L) \
  (TEMPLATE_CONST_IDX (NODE) = I, TEMPLATE_CONST_LEVEL (NODE) = L)

/* in lex.c  */
/* Indexed by TREE_CODE, these tables give C-looking names to
   operators represented by TREE_CODES.  For example,
   opname_tab[(int) MINUS_EXPR] == "-".  */
extern char **opname_tab, **assignop_tab;

/* in call.c */
extern struct candidate *ansi_c_filler;
extern int get_arglist_len_in_bytes		PROTO((tree));

extern int rank_for_overload			PROTO((struct candidate *, struct candidate *));
extern void compute_conversion_costs		PROTO((tree, tree, struct candidate *, int));
extern tree build_vfield_ref			PROTO((tree, tree));
extern tree resolve_scope_to_name		PROTO((tree, tree));
extern tree build_scoped_method_call		PROTO((tree, tree, tree, tree));
extern tree build_addr_func			PROTO((tree));
extern tree build_call				PROTO((tree, tree, tree));
extern tree build_method_call			PROTO((tree, tree, tree, tree, int));
extern tree build_overload_call_real		PROTO((tree, tree, int, struct candidate *, int));
extern tree build_overload_call			PROTO((tree, tree, int));
extern int null_ptr_cst_p			PROTO((tree));
extern tree type_decays_to			PROTO((tree));
extern tree build_user_type_conversion		PROTO((tree, tree, int));
extern tree build_new_function_call		PROTO((tree, tree, tree));
extern tree build_new_op			PROTO((enum tree_code, int, tree, tree, tree));
extern int can_convert				PROTO((tree, tree));
extern int can_convert_arg			PROTO((tree, tree, tree));

/* in class.c */
extern tree build_vbase_path			PROTO((enum tree_code, tree, tree, tree, int));
extern tree build_vtbl_ref			PROTO((tree, tree));
extern tree build_vfn_ref			PROTO((tree *, tree, tree));
extern void add_method				PROTO((tree, tree *, tree));
extern tree get_vfield_offset			PROTO((tree));
extern void duplicate_tag_error			PROTO((tree));
extern tree finish_struct			PROTO((tree, tree, tree, int));
extern tree finish_struct_1			PROTO((tree, int));
extern tree finish_struct_methods		PROTO((tree, tree, int));
extern int resolves_to_fixed_type_p		PROTO((tree, int *));
extern void init_class_processing		PROTO((void));
extern void pushclass				PROTO((tree, int));
extern void popclass				PROTO((int));
extern void push_nested_class			PROTO((tree, int));
extern void pop_nested_class			PROTO((int));
extern void push_lang_context			PROTO((tree));
extern void pop_lang_context			PROTO((void));
extern tree instantiate_type			PROTO((tree, tree, int));
extern void print_class_statistics		PROTO((void));
extern void maybe_push_cache_obstack		PROTO((void));
extern unsigned HOST_WIDE_INT skip_rtti_stuff	PROTO((tree *));
extern tree build_self_reference		PROTO((void));
extern void warn_hidden				PROTO((tree));

/* in cvt.c */
extern tree convert_to_reference		PROTO((tree, tree, int, int, tree));
extern tree convert_from_reference		PROTO((tree));
extern tree convert_to_aggr			PROTO((tree, tree, char **, int));
extern tree convert_pointer_to_real		PROTO((tree, tree));
extern tree convert_pointer_to			PROTO((tree, tree));
extern tree ocp_convert				PROTO((tree, tree, int, int));
extern tree cp_convert				PROTO((tree, tree));
extern tree convert				PROTO((tree, tree));
extern tree convert_force			PROTO((tree, tree, int));
extern tree build_type_conversion		PROTO((enum tree_code, tree, tree, int));
extern tree build_expr_type_conversion		PROTO((int, tree, int));
extern int build_default_binary_type_conversion	PROTO((enum tree_code, tree *, tree *));
extern tree type_promotes_to			PROTO((tree));

/* decl.c */
/* resume_binding_level */
extern int global_bindings_p			PROTO((void));
extern int toplevel_bindings_p			PROTO((void));
extern void keep_next_level			PROTO((void));
extern int kept_level_p				PROTO((void));
extern void declare_parm_level			PROTO((void));
extern void declare_pseudo_global_level		PROTO((void));
extern int pseudo_global_level_p		PROTO((void));
extern void set_class_shadows			PROTO((tree));
extern void pushlevel				PROTO((int));
extern void note_level_for_for			PROTO((void));
extern void pushlevel_temporary			PROTO((int));
extern tree poplevel				PROTO((int, int, int));
extern void resume_level			PROTO((struct binding_level *));
extern void delete_block			PROTO((tree));
extern void insert_block			PROTO((tree));
extern void add_block_current_level		PROTO((tree));
extern void set_block				PROTO((tree));
extern void pushlevel_class			PROTO((void));
extern tree poplevel_class			PROTO((int));
extern void print_binding_stack			PROTO((void));
extern void print_binding_level			PROTO((struct binding_level *));
extern void push_namespace			PROTO((tree));
extern void pop_namespace			PROTO((void));
extern void maybe_push_to_top_level		PROTO((int));
extern void push_to_top_level			PROTO((void));
extern void pop_from_top_level			PROTO((void));
extern void set_identifier_type_value		PROTO((tree, tree));
extern void pop_everything			PROTO((void));
extern void pushtag				PROTO((tree, tree, int));
extern tree make_anon_name			PROTO((void));
extern void clear_anon_tags			PROTO((void));
extern int decls_match				PROTO((tree, tree));
extern int duplicate_decls			PROTO((tree, tree));
extern tree pushdecl				PROTO((tree));
extern tree pushdecl_top_level			PROTO((tree));
extern tree pushdecl_class_level		PROTO((tree));
#if 0
extern void pushdecl_nonclass_level		PROTO((tree));
#endif
extern void push_class_level_binding		PROTO((tree, tree));
extern int overloaded_globals_p			PROTO((tree));
extern tree implicitly_declare			PROTO((tree));
extern tree lookup_label			PROTO((tree));
extern tree shadow_label			PROTO((tree));
extern tree define_label			PROTO((char *, int, tree));
extern void push_switch				PROTO((void));
extern void pop_switch				PROTO((void));
extern void define_case_label			PROTO((tree));
extern tree getdecls				PROTO((void));
extern tree gettags				PROTO((void));
#if 0
extern void set_current_level_tags_transparency	PROTO((int));
#endif
extern tree lookup_namespace_name		PROTO((tree, tree));
extern tree make_typename_type			PROTO((tree, tree));
extern tree lookup_name_nonclass		PROTO((tree));
extern tree lookup_name				PROTO((tree, int));
extern tree lookup_name_current_level		PROTO((tree));
extern tree auto_function			PROTO((tree, tree, enum built_in_function));
extern void init_decl_processing		PROTO((void));
extern int init_type_desc			PROTO((void));
extern tree define_function
	PROTO((char *, tree, enum built_in_function,
	       void (*) (tree), char *));
extern void shadow_tag				PROTO((tree));
extern tree groktypename			PROTO((tree));
extern tree start_decl				PROTO((tree, tree, int));
extern void start_decl_1			PROTO((tree));
extern void cp_finish_decl			PROTO((tree, tree, tree, int, int));
extern void finish_decl				PROTO((tree, tree, tree));
extern void expand_static_init			PROTO((tree, tree));
extern int complete_array_type			PROTO((tree, tree, int));
extern tree build_ptrmemfunc_type		PROTO((tree));
/* the grokdeclarator prototype is in decl.h */
extern int parmlist_is_exprlist			PROTO((tree));
extern int copy_args_p				PROTO((tree));
extern int grok_ctor_properties			PROTO((tree, tree));
extern void grok_op_properties			PROTO((tree, int, int));
extern tree xref_tag				PROTO((tree, tree, tree, int));
extern tree xref_tag_from_type			PROTO((tree, tree, int));
extern void xref_basetypes			PROTO((tree, tree, tree, tree));
extern tree start_enum				PROTO((tree));
extern tree finish_enum				PROTO((tree, tree));
extern tree build_enumerator			PROTO((tree, tree));
extern tree grok_enum_decls			PROTO((tree, tree));
extern int start_function			PROTO((tree, tree, tree, int));
extern void store_after_parms			PROTO((struct rtx_def *));
extern void expand_start_early_try_stmts	PROTO((void));
extern void store_parm_decls			PROTO((void));
extern void store_return_init			PROTO((tree, tree));
extern void finish_function			PROTO((int, int, int));
extern tree start_method			PROTO((tree, tree));
extern tree finish_method			PROTO((tree));
extern void hack_incomplete_structures		PROTO((tree));
extern tree maybe_build_cleanup_and_delete	PROTO((tree));
extern tree maybe_build_cleanup			PROTO((tree));
extern void cplus_expand_expr_stmt		PROTO((tree));
extern void finish_stmt				PROTO((void));
extern int id_in_current_class			PROTO((tree));
extern void push_cp_function_context		PROTO((tree));
extern void pop_cp_function_context		PROTO((tree));
extern int in_function_p			PROTO((void));
extern void replace_defarg			PROTO((tree, tree));
extern void print_other_binding_stack		PROTO((struct binding_level *));
extern tree strip_attrs				PROTO((tree));

/* in decl2.c */
extern int flag_assume_nonnull_objects;
extern int lang_decode_option			PROTO((char *));
extern tree grok_method_quals			PROTO((tree, tree, tree));
extern void warn_if_unknown_interface		PROTO((tree));
extern tree grok_x_components			PROTO((tree, tree));
extern void grokclassfn				PROTO((tree, tree, tree, enum overload_flags, tree));
extern tree grok_alignof			PROTO((tree));
extern tree grok_array_decl			PROTO((tree, tree));
extern tree delete_sanity			PROTO((tree, tree, int, int));
extern tree check_classfn			PROTO((tree, tree));
extern void check_member_template               PROTO((tree));
extern tree grokfield				PROTO((tree, tree, tree, tree, tree));
extern tree grokbitfield			PROTO((tree, tree, tree));
extern tree groktypefield			PROTO((tree, tree));
extern tree grokoptypename			PROTO((tree, tree));
extern int copy_assignment_arg_p		PROTO((tree, int));
extern void cplus_decl_attributes		PROTO((tree, tree, tree)); 
extern tree constructor_name_full		PROTO((tree));
extern tree constructor_name			PROTO((tree));
extern void setup_vtbl_ptr			PROTO((void));
extern void mark_inline_for_output		PROTO((tree));
extern void clear_temp_name			PROTO((void));
extern tree get_temp_name			PROTO((tree, int));
extern tree get_temp_regvar			PROTO((tree, tree));
extern void finish_anon_union			PROTO((tree));
extern tree finish_table			PROTO((tree, tree, tree, int));
extern void finish_builtin_type			PROTO((tree, char *, tree *, int, tree));
extern tree coerce_new_type			PROTO((tree));
extern tree coerce_delete_type			PROTO((tree));
extern void comdat_linkage			PROTO((tree));
extern void import_export_vtable		PROTO((tree, tree, int));
extern int finish_prevtable_vardecl		PROTO((tree, tree));
extern int walk_vtables				PROTO((void (*)(tree, tree),
						       int (*)(tree, tree)));
extern void walk_sigtables			PROTO((void (*)(tree, tree),
						       void (*)(tree, tree)));
extern void import_export_decl			PROTO((tree));
extern tree build_cleanup			PROTO((tree));
extern void finish_file				PROTO((void));
extern tree reparse_absdcl_as_expr		PROTO((tree, tree));
extern tree reparse_absdcl_as_casts		PROTO((tree, tree));
extern tree build_expr_from_tree		PROTO((tree));
extern tree reparse_decl_as_expr		PROTO((tree, tree));
extern tree finish_decl_parsing			PROTO((tree));
extern tree check_cp_case_value			PROTO((tree));
extern tree get_namespace_id			PROTO((void));
extern tree current_namespace_id		PROTO((tree));
extern void do_namespace_alias			PROTO((tree, tree));
extern void do_toplevel_using_decl		PROTO((tree));
extern tree do_class_using_decl			PROTO((tree));
extern void do_using_directive			PROTO((tree));
extern void check_default_args			PROTO((tree));
extern void mark_used				PROTO((tree));
extern tree handle_class_head			PROTO((tree, tree, tree));

/* in errfn.c */
extern void cp_error				();
extern void cp_error_at				();
extern void cp_warning				();
extern void cp_warning_at			();
extern void cp_pedwarn				();
extern void cp_pedwarn_at			();
extern void cp_compiler_error			();
extern void cp_sprintf				();

/* in error.c */
extern void init_error				PROTO((void));
extern char *fndecl_as_string			PROTO((tree, int));
extern char *type_as_string			PROTO((tree, int));
extern char *args_as_string			PROTO((tree, int));
extern char *decl_as_string			PROTO((tree, int));
extern char *expr_as_string			PROTO((tree, int));
extern char *code_as_string			PROTO((enum tree_code, int));
extern char *language_as_string			PROTO((enum languages, int));
extern char *parm_as_string			PROTO((int, int));
extern char *op_as_string			PROTO((enum tree_code, int));
extern char *assop_as_string			PROTO((enum tree_code, int));
extern char *cv_as_string			PROTO((tree, int));
extern char *lang_decl_name			PROTO((tree, int));
extern char *cp_file_of				PROTO((tree));
extern int cp_line_of				PROTO((tree));

/* in except.c */
extern void init_exception_processing		PROTO((void));
extern void expand_start_catch_block		PROTO((tree, tree));
extern void expand_end_catch_block		PROTO((void));
extern void expand_builtin_throw		PROTO((void));
extern void expand_start_eh_spec		PROTO((void));
extern void expand_exception_blocks		PROTO((void));
extern tree start_anon_func			PROTO((void));
extern void end_anon_func			PROTO((void));
extern void expand_throw			PROTO((tree));
extern tree build_throw				PROTO((tree));

/* in expr.c */
extern void init_cplus_expand			PROTO((void));
extern void fixup_result_decl			PROTO((tree, struct rtx_def *));
extern int extract_init				PROTO((tree, tree));
extern void do_case				PROTO((tree, tree));

/* friend.c */
extern int is_friend				PROTO((tree, tree));
extern void make_friend_class			PROTO((tree, tree));
extern tree do_friend				PROTO((tree, tree, tree, tree, enum overload_flags, tree, int));

/* in init.c */
extern void init_init_processing		PROTO((void));
extern void expand_direct_vtbls_init		PROTO((tree, tree, int, int, tree));
extern void emit_base_init			PROTO((tree, int));
extern void check_base_init			PROTO((tree));
extern void do_member_init			PROTO((tree, tree, tree));
extern void expand_member_init			PROTO((tree, tree, tree));
extern void expand_aggr_init			PROTO((tree, tree, int, int));
extern int is_aggr_typedef			PROTO((tree, int));
extern int is_aggr_type				PROTO((tree, int));
extern tree get_aggr_from_typedef		PROTO((tree, int));
extern tree get_type_value			PROTO((tree));
extern tree build_member_call			PROTO((tree, tree, tree));
extern tree build_offset_ref			PROTO((tree, tree));
extern tree resolve_offset_ref			PROTO((tree));
extern tree decl_constant_value			PROTO((tree));
extern tree build_new				PROTO((tree, tree, tree, int));
extern tree expand_vec_init			PROTO((tree, tree, tree, tree, int));
extern tree build_x_delete			PROTO((tree, tree, int, tree));
extern tree build_delete			PROTO((tree, tree, tree, int, int));
extern tree build_vbase_delete			PROTO((tree, tree));
extern tree build_vec_delete			PROTO((tree, tree, tree, tree, int));

/* in input.c */

/* in lex.c */
extern tree make_pointer_declarator		PROTO((tree, tree));
extern tree make_reference_declarator		PROTO((tree, tree));
extern tree make_call_declarator		PROTO((tree, tree, tree, tree));
extern void set_quals_and_spec			PROTO((tree, tree, tree));
extern char *operator_name_string		PROTO((tree));
extern void lang_init				PROTO((void));
extern void lang_finish				PROTO((void));
extern void init_filename_times			PROTO((void));
#if 0
extern void reinit_lang_specific		PROTO((void));
#endif
extern void init_lex				PROTO((void));
extern void reinit_parse_for_function		PROTO((void));
extern int *init_parse				PROTO((void));
extern void print_parse_statistics		PROTO((void));
extern void extract_interface_info		PROTO((void));
extern void do_pending_inlines			PROTO((void));
extern void process_next_inline			PROTO((tree));
extern struct pending_input *save_pending_input PROTO((void));
extern void restore_pending_input		PROTO((struct pending_input *));
extern void yyungetc				PROTO((int, int));
extern void reinit_parse_for_method		PROTO((int, tree));
extern void reinit_parse_for_block		PROTO((int, struct obstack *));
extern tree cons_up_default_function		PROTO((tree, tree, int));
extern void check_for_missing_semicolon		PROTO((tree));
extern void note_got_semicolon			PROTO((tree));
extern void note_list_got_semicolon		PROTO((tree));
extern void do_pending_lang_change		PROTO((void));
extern int identifier_type			PROTO((tree));
extern void see_typename			PROTO((void));
extern tree do_identifier			PROTO((tree, int));
extern tree do_scoped_id			PROTO((tree, int));
extern tree identifier_typedecl_value		PROTO((tree));
extern int real_yylex				PROTO((void));
extern int is_rid				PROTO((tree));
extern tree build_lang_decl			PROTO((enum tree_code, tree, tree));
extern tree build_lang_field_decl		PROTO((enum tree_code, tree, tree));
extern void copy_lang_decl			PROTO((tree));
extern tree make_lang_type			PROTO((enum tree_code));
extern void dump_time_statistics		PROTO((void));
/* extern void compiler_error			PROTO((char *, HOST_WIDE_INT, HOST_WIDE_INT)); */
extern void yyerror				PROTO((char *));
extern void clear_inline_text_obstack		PROTO((void));
extern void maybe_snarf_defarg			PROTO((void));
extern tree snarf_defarg			PROTO((void));
extern void add_defarg_fn			PROTO((tree));
extern void do_pending_defargs			PROTO((void));
extern int identifier_type			PROTO((tree));
extern void yyhook				PROTO((int));

/* in method.c */
extern void init_method				PROTO((void));
extern void do_inline_function_hair		PROTO((tree, tree));
extern void  report_type_mismatch		PROTO((struct candidate *, tree, char *));
extern char *build_overload_name		PROTO((tree, int, int));
extern tree build_static_name			PROTO((tree, tree));
extern tree build_decl_overload			PROTO((tree, tree, int));
extern tree build_template_decl_overload        PROTO((tree, tree, tree, tree, tree, int));
extern tree build_typename_overload		PROTO((tree));
extern tree build_overload_with_type		PROTO((tree, tree));
extern tree build_opfncall			PROTO((enum tree_code, int, tree, tree, tree));
extern tree hack_identifier			PROTO((tree, tree));
extern tree make_thunk				PROTO((tree, int));
extern void emit_thunk				PROTO((tree));
extern void synthesize_method			PROTO((tree));
extern tree get_id_2				PROTO((char *, tree));

/* in pt.c */
extern tree tsubst				PROTO ((tree, tree, int, tree));
extern tree tsubst_expr				PROTO ((tree, tree, int, tree));
extern tree tsubst_copy				PROTO ((tree, tree, int, tree));
extern tree tsubst_chain			PROTO((tree, tree));
extern void begin_member_template_processing    PROTO((tree));
extern void end_member_template_processing      PROTO((void));
extern void begin_template_parm_list		PROTO((void));
extern void begin_specialization                PROTO((void));
extern void reset_specialization                PROTO((void));
extern void end_specialization                  PROTO((void));
extern tree determine_explicit_specialization   PROTO((tree, tree, tree *, int, int));
extern int check_explicit_specialization       PROTO((tree, tree, int, int));
extern tree process_template_parm		PROTO((tree, tree));
extern tree end_template_parm_list		PROTO((tree));
extern void end_template_decl			PROTO((void));
extern tree current_template_args		PROTO((void));
extern void push_template_decl			PROTO((tree));
extern tree lookup_template_class		PROTO((tree, tree, tree));
extern tree lookup_template_function            PROTO((tree, tree));
extern int uses_template_parms			PROTO((tree));
extern tree instantiate_class_template		PROTO((tree));
extern tree instantiate_template		PROTO((tree, tree));
extern void overload_template_name		PROTO((tree));
extern int fn_type_unification                  PROTO((tree, tree, tree, tree, tree, int));
extern int type_unification			PROTO((tree, tree *, tree, tree, tree, int *, int, int));
struct tinst_level *tinst_for_decl		PROTO((void));
extern void mark_decl_instantiated		PROTO((tree, int));
extern int more_specialized			PROTO((tree, tree));
extern void mark_class_instantiated		PROTO((tree, int));
extern void do_decl_instantiation		PROTO((tree, tree, tree));
extern void do_type_instantiation		PROTO((tree, tree));
extern tree instantiate_decl			PROTO((tree));
extern tree lookup_nested_type_by_name		PROTO((tree, tree));
extern tree do_poplevel				PROTO((void));
extern tree get_bindings			PROTO((tree, tree));
/* CONT ... */
extern void add_tree				PROTO((tree));
extern void add_maybe_template			PROTO((tree, tree));
extern void pop_tinst_level			PROTO((void));
extern tree most_specialized			PROTO((tree, tree));
extern tree most_specialized_class		PROTO((tree, tree));
extern int more_specialized_class		PROTO((tree, tree));
extern void do_pushlevel			PROTO((void));
extern int is_member_template                   PROTO((tree));
extern int processing_specialization;

/* in repo.c */
extern void repo_template_used			PROTO((tree));
extern void repo_template_instantiated		PROTO((tree, int));
extern void init_repo				PROTO((char*));
extern void finish_repo				PROTO((void));

/* in rtti.c */
extern void init_rtti_processing		PROTO((void));
extern tree get_tinfo_fn_dynamic		PROTO((tree));
extern tree build_typeid			PROTO((tree));
extern tree build_x_typeid			PROTO((tree));
extern tree get_tinfo_fn			PROTO((tree));
extern tree get_typeid				PROTO((tree));
extern tree build_dynamic_cast			PROTO((tree, tree));
extern void synthesize_tinfo_fn			PROTO((tree));

/* in search.c */
extern void push_memoized_context		PROTO((tree, int));
extern void pop_memoized_context		PROTO((int));
extern tree get_vbase				PROTO((tree, tree));
extern tree get_binfo				PROTO((tree, tree, int));
extern int get_base_distance			PROTO((tree, tree, int, tree *));
extern tree compute_access			PROTO((tree, tree));
extern tree lookup_field			PROTO((tree, tree, int, int));
extern tree lookup_nested_field			PROTO((tree, int));
extern tree lookup_fnfields			PROTO((tree, tree, int));
extern tree lookup_nested_tag			PROTO((tree, tree));
extern tree get_matching_virtual		PROTO((tree, tree, int));
extern tree get_abstract_virtuals		PROTO((tree));
extern tree get_baselinks			PROTO((tree, tree, tree));
extern tree next_baselink			PROTO((tree));
extern tree init_vbase_pointers			PROTO((tree, tree));
extern void expand_indirect_vtbls_init		PROTO((tree, tree, tree));
extern void clear_search_slots			PROTO((tree));
extern tree get_vbase_types			PROTO((tree));
extern void build_mi_matrix			PROTO((tree));
extern void free_mi_matrix			PROTO((void));
extern void build_mi_virtuals			PROTO((int, int));
extern void add_mi_virtuals			PROTO((int, tree));
extern void report_ambiguous_mi_virtuals	PROTO((int, tree));
extern void note_debug_info_needed		PROTO((tree));
extern void push_class_decls			PROTO((tree));
extern void pop_class_decls			PROTO((void));
extern void unuse_fields			PROTO((tree));
extern void unmark_finished_struct		PROTO((tree));
extern void print_search_statistics		PROTO((void));
extern void init_search_processing		PROTO((void));
extern void reinit_search_statistics		PROTO((void));
extern tree current_scope			PROTO((void));
extern tree lookup_conversions			PROTO((tree));
extern tree get_template_base			PROTO((tree, tree));

/* in sig.c */
extern tree build_signature_pointer_type	PROTO((tree, int, int));
extern tree build_signature_reference_type	PROTO((tree, int, int));
extern tree build_signature_pointer_constructor	PROTO((tree, tree));
extern tree build_signature_method_call		PROTO((tree, tree));
extern tree build_optr_ref			PROTO((tree));
extern void append_signature_fields		PROTO((tree));

/* in spew.c */
extern void init_spew				PROTO((void));
extern int peekyylex				PROTO((void));
extern int yylex				PROTO((void));
extern tree arbitrate_lookup			PROTO((tree, tree, tree));

/* in tree.c */
extern int real_lvalue_p			PROTO((tree));
extern tree build_min				PVPROTO((enum tree_code, tree, ...));
extern tree build_min_nt			PVPROTO((enum tree_code, ...));
extern tree min_tree_cons			PROTO((tree, tree, tree));
extern int lvalue_p				PROTO((tree));
extern int lvalue_or_else			PROTO((tree, char *));
extern tree build_cplus_new			PROTO((tree, tree));
extern tree break_out_cleanups			PROTO((tree));
extern tree break_out_calls			PROTO((tree));
extern tree build_cplus_method_type		PROTO((tree, tree, tree));
extern tree build_cplus_staticfn_type		PROTO((tree, tree, tree));
extern tree build_cplus_array_type		PROTO((tree, tree));
extern void propagate_binfo_offsets		PROTO((tree, tree));
extern int layout_vbasetypes			PROTO((tree, int));
extern tree layout_basetypes			PROTO((tree, tree));
extern tree hash_tree_cons			PROTO((int, int, int, tree, tree, tree));
extern tree hash_tree_chain			PROTO((tree, tree));
extern tree hash_chainon			PROTO((tree, tree));
extern tree get_decl_list			PROTO((tree));
extern tree make_binfo				PROTO((tree, tree, tree, tree, tree));
extern tree binfo_value				PROTO((tree, tree));
extern tree reverse_path			PROTO((tree));
extern int decl_list_length			PROTO((tree));
extern int count_functions			PROTO((tree));
extern int is_overloaded_fn			PROTO((tree));
extern tree get_first_fn			PROTO((tree));
extern tree fnaddr_from_vtable_entry		PROTO((tree));
extern tree function_arg_chain			PROTO((tree));
extern int promotes_to_aggr_type		PROTO((tree, enum tree_code));
extern int is_aggr_type_2			PROTO((tree, tree));
extern char *lang_printable_name		PROTO((tree, int));
extern tree build_exception_variant		PROTO((tree, tree));
extern tree copy_to_permanent			PROTO((tree));
extern void print_lang_statistics		PROTO((void));
extern void __eprintf
	PROTO((const char *, const char *, unsigned, const char *));
extern tree array_type_nelts_total		PROTO((tree));
extern tree array_type_nelts_top		PROTO((tree));
extern tree break_out_target_exprs		PROTO((tree));
extern tree get_type_decl			PROTO((tree));
extern tree vec_binfo_member			PROTO((tree, tree));
extern tree hack_decl_function_context 		PROTO((tree));
extern tree lvalue_type				PROTO((tree));
extern tree error_type				PROTO((tree));
extern tree make_temp_vec			PROTO((int));
extern int varargs_function_p			PROTO((tree));
extern int really_overloaded_fn			PROTO((tree));
extern int cp_tree_equal			PROTO((tree, tree));
extern int can_free				PROTO((struct obstack *, tree));
extern tree mapcar				PROTO((tree, tree (*) (tree)));
extern void debug_binfo				PROTO((tree));
#define scratchalloc expralloc
#define scratch_tree_cons expr_tree_cons
#define build_scratch_list build_expr_list
#define make_scratch_vec make_temp_vec

/* in typeck.c */
extern tree condition_conversion		PROTO((tree));
extern tree target_type				PROTO((tree));
extern tree require_complete_type		PROTO((tree));
extern tree complete_type			PROTO((tree));
extern int type_unknown_p			PROTO((tree));
extern int fntype_p				PROTO((tree));
extern tree require_instantiated_type		PROTO((tree, tree, tree));
extern tree commonparms				PROTO((tree, tree));
extern tree common_type				PROTO((tree, tree));
extern int compexcepttypes			PROTO((tree, tree));
extern int comptypes				PROTO((tree, tree, int));
extern int comp_target_types			PROTO((tree, tree, int));
extern int compparms				PROTO((tree, tree, int));
extern int comp_target_types			PROTO((tree, tree, int));
extern int self_promoting_args_p		PROTO((tree));
extern tree unsigned_type			PROTO((tree));
extern tree signed_type				PROTO((tree));
extern tree signed_or_unsigned_type		PROTO((int, tree));
extern tree expr_sizeof				PROTO((tree));
extern tree c_sizeof				PROTO((tree));
extern tree c_sizeof_nowarn			PROTO((tree));
extern tree c_alignof				PROTO((tree));
extern tree inline_conversion			PROTO((tree));
extern tree decay_conversion			PROTO((tree));
extern tree default_conversion			PROTO((tree));
extern tree build_object_ref			PROTO((tree, tree, tree));
extern tree build_component_ref_1		PROTO((tree, tree, int));
extern tree build_component_ref			PROTO((tree, tree, tree, int));
extern tree build_x_component_ref		PROTO((tree, tree, tree, int));
extern tree build_x_indirect_ref		PROTO((tree, char *));
extern tree build_indirect_ref			PROTO((tree, char *));
extern tree build_array_ref			PROTO((tree, tree));
extern tree build_x_function_call		PROTO((tree, tree, tree));
extern tree get_member_function_from_ptrfunc	PROTO((tree *, tree));
extern tree build_function_call_real		PROTO((tree, tree, int, int));
extern tree build_function_call			PROTO((tree, tree));
extern tree build_function_call_maybe		PROTO((tree, tree));
extern tree convert_arguments			PROTO((tree, tree, tree, tree, int));
extern tree build_x_binary_op			PROTO((enum tree_code, tree, tree));
extern tree build_binary_op			PROTO((enum tree_code, tree, tree, int));
extern tree build_binary_op_nodefault		PROTO((enum tree_code, tree, tree, enum tree_code));
extern tree build_component_addr		PROTO((tree, tree, char *));
extern tree build_x_unary_op			PROTO((enum tree_code, tree));
extern tree build_unary_op			PROTO((enum tree_code, tree, int));
extern tree unary_complex_lvalue		PROTO((enum tree_code, tree));
extern int mark_addressable			PROTO((tree));
extern tree build_x_conditional_expr		PROTO((tree, tree, tree));
extern tree build_conditional_expr		PROTO((tree, tree, tree));
extern tree build_x_compound_expr		PROTO((tree));
extern tree build_compound_expr			PROTO((tree));
extern tree build_static_cast			PROTO((tree, tree));
extern tree build_reinterpret_cast		PROTO((tree, tree));
extern tree build_const_cast			PROTO((tree, tree));
extern tree build_c_cast			PROTO((tree, tree));
extern tree build_x_modify_expr			PROTO((tree, enum tree_code, tree));
extern tree build_modify_expr			PROTO((tree, enum tree_code, tree));
extern int language_lvalue_valid		PROTO((tree));
extern void warn_for_assignment			PROTO((char *, char *, char *, tree, int, int));
extern tree convert_for_initialization		PROTO((tree, tree, tree, int, char *, tree, int));
extern void c_expand_asm_operands		PROTO((tree, tree, tree, tree, int, char *, int));
extern void c_expand_return			PROTO((tree));
extern tree c_expand_start_case			PROTO((tree));
extern int comp_ptr_ttypes			PROTO((tree, tree));
extern int ptr_reasonably_similar		PROTO((tree, tree));
extern tree build_ptrmemfunc			PROTO((tree, tree, int));

/* in typeck2.c */
extern tree error_not_base_type			PROTO((tree, tree));
extern tree binfo_or_else			PROTO((tree, tree));
extern void readonly_error			PROTO((tree, char *, int));
extern void abstract_virtuals_error		PROTO((tree, tree));
extern void signature_error			PROTO((tree, tree));
extern void incomplete_type_error		PROTO((tree, tree));
extern void my_friendly_abort			PROTO((int));
extern void my_friendly_assert			PROTO((int, int));
extern tree store_init_value			PROTO((tree, tree));
extern tree digest_init				PROTO((tree, tree, tree *));
extern tree build_scoped_ref			PROTO((tree, tree));
extern tree build_x_arrow			PROTO((tree));
extern tree build_m_component_ref		PROTO((tree, tree));
extern tree build_functional_cast		PROTO((tree, tree));
extern char *enum_name_string			PROTO((tree, tree));
extern void report_case_error			PROTO((int, tree, tree, tree));
extern void check_for_new_type			PROTO((char *,flagged_type_tree));
extern tree initializer_constant_valid_p	PROTO((tree, tree));

/* in xref.c */
extern void GNU_xref_begin			PROTO((char *));
extern void GNU_xref_end			PROTO((int));
extern void GNU_xref_file			PROTO((char *));
extern void GNU_xref_start_scope		PROTO((HOST_WIDE_INT));
extern void GNU_xref_end_scope			PROTO((HOST_WIDE_INT, HOST_WIDE_INT, int, int));
extern void GNU_xref_ref			PROTO((tree, char *));
extern void GNU_xref_decl			PROTO((tree, tree));
extern void GNU_xref_call			PROTO((tree, char *));
extern void GNU_xref_function			PROTO((tree, tree));
extern void GNU_xref_assign			PROTO((tree));
extern void GNU_xref_hier			PROTO((char *, char *, int, int, int));
extern void GNU_xref_member			PROTO((tree, tree));

/* -- end of C++ */

#endif /* not _CP_TREE_H */
