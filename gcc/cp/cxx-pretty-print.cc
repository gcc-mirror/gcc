/* Implementation of subroutines for the GNU C++ pretty-printer.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "cxx-pretty-print.h"
#include "tree-pretty-print.h"
#include "make-unique.h"

static void pp_cxx_unqualified_id (cxx_pretty_printer *, tree);
static void pp_cxx_nested_name_specifier (cxx_pretty_printer *, tree);
static void pp_cxx_qualified_id (cxx_pretty_printer *, tree);
static void pp_cxx_template_argument_list (cxx_pretty_printer *, tree);
static void pp_cxx_type_specifier_seq (cxx_pretty_printer *, tree);
static void pp_cxx_ptr_operator (cxx_pretty_printer *, tree);
static void pp_cxx_parameter_declaration_clause (cxx_pretty_printer *, tree);
static void pp_cxx_template_parameter (cxx_pretty_printer *, tree);
static void pp_cxx_cast_expression (cxx_pretty_printer *, tree);
static void pp_cxx_typeid_expression (cxx_pretty_printer *, tree);
static void pp_cxx_unary_left_fold_expression (cxx_pretty_printer *, tree);
static void pp_cxx_unary_right_fold_expression (cxx_pretty_printer *, tree);
static void pp_cxx_binary_fold_expression (cxx_pretty_printer *, tree);
static void pp_cxx_concept_definition (cxx_pretty_printer *, tree);


static inline void
pp_cxx_nonconsecutive_character (cxx_pretty_printer *pp, int c)
{
  const char *p = pp_last_position_in_text (pp);

  if (p != NULL && *p == c)
    pp_cxx_whitespace (pp);
  pp_character (pp, c);
  pp->set_padding (pp_none);
}

#define pp_cxx_expression_list(PP, T)    \
   pp_c_expression_list (PP, T)
#define pp_cxx_space_for_pointer_operator(PP, T)  \
   pp_c_space_for_pointer_operator (PP, T)
#define pp_cxx_init_declarator(PP, T)    \
   pp_c_init_declarator (PP, T)
#define pp_cxx_call_argument_list(PP, T) \
   pp_c_call_argument_list (PP, T)

void
pp_cxx_colon_colon (cxx_pretty_printer *pp)
{
  pp_colon_colon (pp);
  pp->set_padding (pp_none);
}

void
pp_cxx_begin_template_argument_list (cxx_pretty_printer *pp)
{
  pp_cxx_nonconsecutive_character (pp, '<');
}

void
pp_cxx_end_template_argument_list (cxx_pretty_printer *pp)
{
  pp_cxx_nonconsecutive_character (pp, '>');
}

void
pp_cxx_separate_with (cxx_pretty_printer *pp, int c)
{
  pp_separate_with (pp, c);
  pp->set_padding (pp_none);
}

/* Expressions.  */

/* conversion-function-id:
      operator conversion-type-id

   conversion-type-id:
      type-specifier-seq conversion-declarator(opt)

   conversion-declarator:
      ptr-operator conversion-declarator(opt)  */

static inline void
pp_cxx_conversion_function_id (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "operator");
  pp_cxx_type_specifier_seq (pp, TREE_TYPE (t));
}

static inline void
pp_cxx_template_id (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 0));
  pp_cxx_begin_template_argument_list (pp);
  pp_cxx_template_argument_list (pp, TREE_OPERAND (t, 1));
  pp_cxx_end_template_argument_list (pp);
}

/* Prints the unqualified part of the id-expression T.

   unqualified-id:
     identifier
     operator-function-id
     conversion-function-id
     ~ class-name
     template-id  */

static void
pp_cxx_unqualified_id (cxx_pretty_printer *pp, tree t)
{
  enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case RESULT_DECL:
      pp->translate_string ("<return-value>");
      break;

    case OVERLOAD:
      t = OVL_FIRST (t);
      /* FALLTHRU */
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case TYPE_DECL:
    case FUNCTION_DECL:
    case NAMESPACE_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
    case USING_DECL:
    case TEMPLATE_DECL:
      t = DECL_NAME (t);
      /* FALLTHRU */

    case IDENTIFIER_NODE:
      if (t == NULL)
	pp->translate_string ("<unnamed>");
      else if (IDENTIFIER_CONV_OP_P (t))
	pp_cxx_conversion_function_id (pp, t);
      else
	pp_cxx_tree_identifier (pp, t);
      break;

    case TEMPLATE_ID_EXPR:
      pp_cxx_template_id (pp, t);
      break;

    case BASELINK:
      pp_cxx_unqualified_id (pp, BASELINK_FUNCTIONS (t));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case TYPENAME_TYPE:
    case UNBOUND_CLASS_TEMPLATE:
      pp_cxx_unqualified_id (pp, TYPE_NAME (t));
      if (tree ti = TYPE_TEMPLATE_INFO_MAYBE_ALIAS (t))
	if (PRIMARY_TEMPLATE_P (TI_TEMPLATE (ti)))
	  {
	    pp_cxx_begin_template_argument_list (pp);
	    tree args = INNERMOST_TEMPLATE_ARGS (TI_ARGS (ti));
	    pp_cxx_template_argument_list (pp, args);
	    pp_cxx_end_template_argument_list (pp);
	  }
      break;

    case BIT_NOT_EXPR:
      pp_cxx_complement (pp);
      pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 0));
      break;

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
      if (template_placeholder_p (t))
	{
	  t = TREE_TYPE (CLASS_PLACEHOLDER_TEMPLATE (t));
	  pp_cxx_unqualified_id (pp, TYPE_IDENTIFIER (t));
	  pp_string (pp, "<...auto...>");
	}
      else if (TYPE_IDENTIFIER (t))
	pp_cxx_unqualified_id (pp, TYPE_IDENTIFIER (t));
      else
	pp_cxx_canonical_template_parameter (pp, t);
      break;

    case TEMPLATE_PARM_INDEX:
      pp_cxx_unqualified_id (pp, TEMPLATE_PARM_DECL (t));
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      pp_cxx_cv_qualifier_seq (pp, t);
      pp_cxx_unqualified_id (pp, TYPE_IDENTIFIER (t));
      pp_cxx_begin_template_argument_list (pp);
      pp_cxx_template_argument_list (pp, TYPE_TI_ARGS (t));
      pp_cxx_end_template_argument_list (pp);
      break;

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* Pretty-print out the token sequence ":: template" in template codes
   where it is needed to "inline declare" the (following) member as
   a template.  This situation arises when SCOPE of T is dependent
   on template parameters.  */

static inline void
pp_cxx_template_keyword_if_needed (cxx_pretty_printer *pp, tree scope, tree t)
{
  if (TREE_CODE (t) == TEMPLATE_ID_EXPR
      && TYPE_P (scope) && dependent_type_p (scope))
    pp_cxx_ws_string (pp, "template");
}

/* nested-name-specifier:
      class-or-namespace-name :: nested-name-specifier(opt)
      class-or-namespace-name :: template nested-name-specifier   */

static void
pp_cxx_nested_name_specifier (cxx_pretty_printer *pp, tree t)
{
  /* FIXME: When diagnosing references to concepts (especially as types?)
     we end up adding too many '::' to the name. This is partially due
     to the fact that pp->enclosing_namespace is null.  */
  if (t == global_namespace)
    {
      pp_cxx_colon_colon (pp);
    }
  else if (!SCOPE_FILE_SCOPE_P (t) && t != pp->enclosing_scope)
    {
      tree scope = get_containing_scope (t);
      pp_cxx_nested_name_specifier (pp, scope);
      pp_cxx_template_keyword_if_needed (pp, scope, t);
      pp_cxx_unqualified_id (pp, t);
      pp_cxx_colon_colon (pp);
    }
}

/* qualified-id:
      nested-name-specifier template(opt) unqualified-id  */

static void
pp_cxx_qualified_id (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
      /* A pointer-to-member is always qualified.  */
    case PTRMEM_CST:
      pp_cxx_nested_name_specifier (pp, PTRMEM_CST_CLASS (t));
      pp_cxx_unqualified_id (pp, PTRMEM_CST_MEMBER (t));
      break;

      /* In Standard C++, functions cannot possibly be used as
	 nested-name-specifiers.  However, there are situations where
	 is "makes sense" to output the surrounding function name for the
	 purpose of emphasizing on the scope kind.  Just printing the
	 function name might not be sufficient as it may be overloaded; so,
	 we decorate the function with its signature too.
	 FIXME:  This is probably the wrong pretty-printing for conversion
	 functions and some function templates.  */
    case OVERLOAD:
      t = OVL_FIRST (t);
      /* FALLTHRU */
    case FUNCTION_DECL:
      if (DECL_FUNCTION_MEMBER_P (t))
	pp_cxx_nested_name_specifier (pp, DECL_CONTEXT (t));
      pp_cxx_unqualified_id
	(pp, DECL_CONSTRUCTOR_P (t) ? DECL_CONTEXT (t) : t);
      pp_cxx_parameter_declaration_clause (pp, TREE_TYPE (t));
      break;

    case OFFSET_REF:
    case SCOPE_REF:
      pp_cxx_nested_name_specifier (pp, TREE_OPERAND (t, 0));
      pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 1));
      break;

    default:
      {
	tree scope = get_containing_scope (t);
	if (scope != pp->enclosing_scope)
	  {
	    pp_cxx_nested_name_specifier (pp, scope);
	    pp_cxx_template_keyword_if_needed (pp, scope, t);
	  }
	pp_cxx_unqualified_id (pp, t);
      }
      break;
    }
}

/* Given a value e of ENUMERAL_TYPE:
   Print out the first ENUMERATOR id with value e, if one is found,
   (including nested names but excluding the enum name if unscoped)
   else print out the value as a C-style cast (type-id)value.  */

static void
pp_cxx_enumeration_constant (cxx_pretty_printer *pp, tree e)
{
  tree type = TREE_TYPE (e);
  tree value = NULL_TREE;

  /* Find the name of this constant.  */
  if ((pp->flags & pp_c_flag_gnu_v3) == 0)
    for (value = TYPE_VALUES (type); value != NULL_TREE;
	 value = TREE_CHAIN (value))
      if (tree_int_cst_equal (DECL_INITIAL (TREE_VALUE (value)), e))
	break;

  if (value != NULL_TREE)
    {
      if (!ENUM_IS_SCOPED (type))
	type = get_containing_scope (type);
      pp_cxx_nested_name_specifier (pp, type);
      pp->id_expression (TREE_PURPOSE (value));
    }
  else
    {
      /* Value must have been cast.  */
       pp_c_type_cast (pp, type);
       pp_c_integer_constant (pp, e);
    }
}


void
cxx_pretty_printer::constant (tree t)
{
  switch (TREE_CODE (t))
    {
    case STRING_CST:
      {
	const bool in_parens = PAREN_STRING_LITERAL_P (t);
	if (in_parens)
	  pp_cxx_left_paren (this);
	c_pretty_printer::constant (t);
	if (in_parens)
	  pp_cxx_right_paren (this);
      }
      break;

    case INTEGER_CST:
      if (NULLPTR_TYPE_P (TREE_TYPE (t)))
	{
	  pp_string (this, "nullptr");
	  break;
	}
      else if (TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
	{
	  pp_cxx_enumeration_constant (this, t);
	  break;
	}
      /* fall through.  */

    default:
      c_pretty_printer::constant (t);
      break;
    }
}

/* id-expression:
      unqualified-id
      qualified-id   */

void
cxx_pretty_printer::id_expression (tree t)
{
  if (TREE_CODE (t) == OVERLOAD)
    t = OVL_FIRST (t);
  if (DECL_P (t) && DECL_CONTEXT (t))
    pp_cxx_qualified_id (this, t);
  else
    pp_cxx_unqualified_id (this, t);
}

/* user-defined literal:
      literal ud-suffix  */

void
pp_cxx_userdef_literal (cxx_pretty_printer *pp, tree t)
{
  pp->constant (USERDEF_LITERAL_VALUE (t));
  pp->id_expression (USERDEF_LITERAL_SUFFIX_ID (t));
}


/* primary-expression:
     literal
     this
     :: identifier
     :: operator-function-id
     :: qualifier-id
     ( expression )
     id-expression

   GNU Extensions:
     __builtin_va_arg ( assignment-expression , type-id )
     __builtin_offsetof ( type-id, offsetof-expression )
     __builtin_addressof ( expression )

     __builtin_is_virtual_base_of ( type-id , type-id )

     __has_nothrow_assign ( type-id )
     __has_nothrow_constructor ( type-id )
     __has_nothrow_copy ( type-id )
     __has_trivial_assign ( type-id )
     __has_trivial_constructor ( type-id )
     __has_trivial_copy ( type-id )
     __has_unique_object_representations ( type-id )
     __has_trivial_destructor ( type-id )
     __has_virtual_destructor ( type-id )
     __is_abstract ( type-id )
     __is_base_of ( type-id , type-id )
     __is_class ( type-id )
     __is_empty ( type-id )
     __is_enum ( type-id )
     __is_literal_type ( type-id )
     __is_pod ( type-id )
     __is_polymorphic ( type-id )
     __is_std_layout ( type-id )
     __is_trivial ( type-id )
     __is_union ( type-id )  */

void
cxx_pretty_printer::primary_expression (tree t)
{
  switch (TREE_CODE (t))
    {
    case VOID_CST:
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
    case STRING_CST:
      constant (t);
      break;

    case USERDEF_LITERAL:
      pp_cxx_userdef_literal (this, t);
      break;

    case BASELINK:
      t = BASELINK_FUNCTIONS (t);
      /* FALLTHRU */
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case FUNCTION_DECL:
    case OVERLOAD:
    case CONST_DECL:
    case TEMPLATE_DECL:
      id_expression (t);
      break;

    case RESULT_DECL:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
      pp_cxx_unqualified_id (this, t);
      break;

    case STMT_EXPR:
      pp_cxx_left_paren (this);
      statement (STMT_EXPR_STMT (t));
      pp_cxx_right_paren (this);
      break;

    case TRAIT_EXPR:
      pp_cxx_trait (this, t);
      break;

    case VA_ARG_EXPR:
      pp_cxx_va_arg_expression (this, t);
      break;

    case OFFSETOF_EXPR:
      pp_cxx_offsetof_expression (this, t);
      break;

    case ADDRESSOF_EXPR:
      pp_cxx_addressof_expression (this, t);
      break;

    case REQUIRES_EXPR:
      pp_cxx_requires_expr (this, t);
      break;

    default:
      c_pretty_printer::primary_expression (t);
      break;
    }
}

/* postfix-expression:
     primary-expression
     postfix-expression [ expression ]
     postfix-expression ( expression-list(opt) )
     simple-type-specifier ( expression-list(opt) )
     typename ::(opt) nested-name-specifier identifier ( expression-list(opt) )
     typename ::(opt) nested-name-specifier template(opt)
				       template-id ( expression-list(opt) )
     postfix-expression . template(opt) ::(opt) id-expression
     postfix-expression -> template(opt) ::(opt) id-expression
     postfix-expression . pseudo-destructor-name
     postfix-expression -> pseudo-destructor-name
     postfix-expression ++
     postfix-expression --
     dynamic_cast < type-id > ( expression )
     static_cast < type-id > ( expression )
     reinterpret_cast < type-id > ( expression )
     const_cast < type-id > ( expression )
     typeid ( expression )
     typeid ( type-id )  */

void
cxx_pretty_printer::postfix_expression (tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      {
	tree fun = cp_get_callee (t);
	tree saved_scope = enclosing_scope;
	bool skipfirst = false;
	tree arg;

	if (TREE_CODE (fun) == ADDR_EXPR)
	  fun = TREE_OPERAND (fun, 0);

	/* In templates, where there is no way to tell whether a given
	   call uses an actual member function.  So the parser builds
	   FUN as a COMPONENT_REF or a plain IDENTIFIER_NODE until
	   instantiation time.  */
	if (TREE_CODE (fun) != FUNCTION_DECL)
	  ;
	else if (DECL_OBJECT_MEMBER_FUNCTION_P (fun))
	  {
	    tree object = (code == AGGR_INIT_EXPR
			   ? (AGGR_INIT_VIA_CTOR_P (t)
			      ? AGGR_INIT_EXPR_SLOT (t)
			      : AGGR_INIT_EXPR_ARG (t, 0))
			   : CALL_EXPR_ARG (t, 0));

	    while (TREE_CODE (object) == NOP_EXPR)
	      object = TREE_OPERAND (object, 0);

	    if (TREE_CODE (object) == ADDR_EXPR)
	      object = TREE_OPERAND (object, 0);

	    if (!TYPE_PTR_P (TREE_TYPE (object)))
	      {
		postfix_expression (object);
		pp_cxx_dot (this);
	      }
	    else
	      {
		postfix_expression (object);
		pp_cxx_arrow (this);
	      }
	    skipfirst = true;
	    enclosing_scope = strip_pointer_operator (TREE_TYPE (object));
	  }

	postfix_expression (fun);
	enclosing_scope = saved_scope;
	pp_cxx_left_paren (this);
	if (code == AGGR_INIT_EXPR)
	  {
	    aggr_init_expr_arg_iterator iter;
	    FOR_EACH_AGGR_INIT_EXPR_ARG (arg, iter, t)
	      {
		if (skipfirst)
		  skipfirst = false;
		else
		  {
		    expression (arg);
		    if (more_aggr_init_expr_args_p (&iter))
		      pp_cxx_separate_with (this, ',');
		  }
	      }
	  }
	else
	  {
	    call_expr_arg_iterator iter;
	    FOR_EACH_CALL_EXPR_ARG (arg, iter, t)
	      {
		if (skipfirst)
		  skipfirst = false;
		else
		  {
		    expression (arg);
		    if (more_call_expr_args_p (&iter))
		      pp_cxx_separate_with (this, ',');
		  }
	      }
	  }
	pp_cxx_right_paren (this);
      }
      if (code == AGGR_INIT_EXPR && AGGR_INIT_VIA_CTOR_P (t))
	{
	  pp_cxx_separate_with (this, ',');
	  postfix_expression (AGGR_INIT_EXPR_SLOT (t));
	}
      break;

    case BASELINK:
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case FUNCTION_DECL:
    case OVERLOAD:
    case CONST_DECL:
    case TEMPLATE_DECL:
    case RESULT_DECL:
      primary_expression (t);
      break;

    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
      if (code == DYNAMIC_CAST_EXPR)
	pp_cxx_ws_string (this, "dynamic_cast");
      else if (code == STATIC_CAST_EXPR)
	pp_cxx_ws_string (this, "static_cast");
      else if (code == REINTERPRET_CAST_EXPR)
	pp_cxx_ws_string (this, "reinterpret_cast");
      else
	pp_cxx_ws_string (this, "const_cast");
      pp_cxx_begin_template_argument_list (this);
      type_id (TREE_TYPE (t));
      pp_cxx_end_template_argument_list (this);
      pp_left_paren (this);
      expression (TREE_OPERAND (t, 0));
      pp_right_paren (this);
      break;

    case BIT_CAST_EXPR:
      pp_cxx_ws_string (this, "__builtin_bit_cast");
      pp_left_paren (this);
      type_id (TREE_TYPE (t));
      pp_comma (this);
      expression (TREE_OPERAND (t, 0));
      pp_right_paren (this);
      break;

    case EMPTY_CLASS_EXPR:
      type_id (TREE_TYPE (t));
      pp_left_paren (this);
      pp_right_paren (this);
      break;

    case TYPEID_EXPR:
      pp_cxx_typeid_expression (this, t);
      break;

    case PSEUDO_DTOR_EXPR:
      postfix_expression (TREE_OPERAND (t, 0));
      pp_cxx_dot (this);
      if (TREE_OPERAND (t, 1))
	{
	  pp_cxx_qualified_id (this, TREE_OPERAND (t, 1));
	  pp_cxx_colon_colon (this);
	}
      pp_complement (this);
      pp_cxx_unqualified_id (this, TREE_OPERAND (t, 2));
      break;

    case ARROW_EXPR:
      postfix_expression (TREE_OPERAND (t, 0));
      pp_cxx_arrow (this);
      break;

    default:
      c_pretty_printer::postfix_expression (t);
      break;
    }
}

/* new-expression:
      ::(opt) new new-placement(opt) new-type-id new-initializer(opt)
      ::(opt) new new-placement(opt) ( type-id ) new-initializer(opt)

   new-placement:
      ( expression-list )

   new-type-id:
      type-specifier-seq new-declarator(opt)

   new-declarator:
      ptr-operator new-declarator(opt)
      direct-new-declarator

   direct-new-declarator
      [ expression ]
      direct-new-declarator [ constant-expression ]

   new-initializer:
      ( expression-list(opt) )  */

static void
pp_cxx_new_expression (cxx_pretty_printer *pp, tree t)
{
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_OPERAND (t, 1);
  tree init = TREE_OPERAND (t, 2);
  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      if (NEW_EXPR_USE_GLOBAL (t))
	pp_cxx_colon_colon (pp);
      pp_cxx_ws_string (pp, "new");
      if (TREE_OPERAND (t, 0))
	{
	  pp_cxx_call_argument_list (pp, TREE_OPERAND (t, 0));
	  pp_space (pp);
	}
      if (TREE_CODE (type) == ARRAY_REF)
	type = build_cplus_array_type
	  (TREE_OPERAND (type, 0),
	   build_index_type (fold_build2_loc (input_location,
					  MINUS_EXPR, integer_type_node,
					  TREE_OPERAND (type, 1),
					  integer_one_node)));
      pp->type_id (type);
      if (init)
	{
	  pp_left_paren (pp);
	  if (TREE_CODE (init) == TREE_LIST)
	    pp_c_expression_list (pp, init);
	  else if (init == void_node)
	    ;			/* OK, empty initializer list.  */
	  else
	    pp->expression (init);
	  pp_right_paren (pp);
	}
      break;

    default:
      pp_unsupported_tree (pp, t);
    }
}

/* delete-expression:
      ::(opt) delete cast-expression
      ::(opt) delete [ ] cast-expression   */

static void
pp_cxx_delete_expression (cxx_pretty_printer *pp, tree t)
{
  enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      if (DELETE_EXPR_USE_GLOBAL (t))
	pp_cxx_colon_colon (pp);
      pp_cxx_ws_string (pp, "delete");
      pp_space (pp);
      if (code == VEC_DELETE_EXPR
	  || DELETE_EXPR_USE_VEC (t))
	{
	  pp_left_bracket (pp);
	  pp_right_bracket (pp);
	  pp_space (pp);
	}
      pp_c_cast_expression (pp, TREE_OPERAND (t, 0));
      break;

    default:
      pp_unsupported_tree (pp, t);
    }
}

/* unary-expression:
      postfix-expression
      ++ cast-expression
      -- cast-expression
      unary-operator cast-expression
      sizeof unary-expression
      sizeof ( type-id )
      sizeof ... ( identifier )
      new-expression
      delete-expression

   unary-operator: one of
      *   &   +   -  !

   GNU extensions:
      __alignof__ unary-expression
      __alignof__ ( type-id )  */

void
cxx_pretty_printer::unary_expression (tree t)
{
  enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      pp_cxx_new_expression (this, t);
      break;

    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      pp_cxx_delete_expression (this, t);
      break;

    case SIZEOF_EXPR:
      if (PACK_EXPANSION_P (TREE_OPERAND (t, 0)))
	{
	  pp_cxx_ws_string (this, "sizeof");
	  pp_cxx_ws_string (this, "...");
	  pp_cxx_whitespace (this);
	  pp_cxx_left_paren (this);
	  if (TYPE_P (TREE_OPERAND (t, 0)))
	    type_id (TREE_OPERAND (t, 0));
	  else
	    unary_expression (TREE_OPERAND (t, 0));
	  pp_cxx_right_paren (this);
	  break;
	}
      /* Fall through  */

    case ALIGNOF_EXPR:
      if (code == SIZEOF_EXPR)
	pp_cxx_ws_string (this, "sizeof");
      else if (ALIGNOF_EXPR_STD_P (t))
	pp_cxx_ws_string (this, "alignof");
      else
	pp_cxx_ws_string (this, "__alignof__");
      pp_cxx_whitespace (this);
      if (TREE_CODE (t) == SIZEOF_EXPR && SIZEOF_EXPR_TYPE_P (t))
	{
	  pp_cxx_left_paren (this);
	  type_id (TREE_TYPE (TREE_OPERAND (t, 0)));
	  pp_cxx_right_paren (this);
	}
      else if (TYPE_P (TREE_OPERAND (t, 0)))
	{
	  pp_cxx_left_paren (this);
	  type_id (TREE_OPERAND (t, 0));
	  pp_cxx_right_paren (this);
	}
      else
	unary_expression (TREE_OPERAND (t, 0));
      break;

    case AT_ENCODE_EXPR:
      pp_cxx_ws_string (this, "@encode");
      pp_cxx_whitespace (this);
      pp_cxx_left_paren (this);
      type_id (TREE_OPERAND (t, 0));
      pp_cxx_right_paren (this);
      break;

    case NOEXCEPT_EXPR:
      pp_cxx_ws_string (this, "noexcept");
      pp_cxx_whitespace (this);
      pp_cxx_left_paren (this);
      expression (TREE_OPERAND (t, 0));
      pp_cxx_right_paren (this);
      break;

    case UNARY_PLUS_EXPR:
      pp_plus (this);
      pp_cxx_cast_expression (this, TREE_OPERAND (t, 0));
      break;

    default:
      c_pretty_printer::unary_expression (t);
      break;
    }
}

/* cast-expression:
      unary-expression
      ( type-id ) cast-expression  */

static void
pp_cxx_cast_expression (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      pp->type_id (TREE_TYPE (t));
      pp_cxx_call_argument_list (pp, TREE_OPERAND (t, 0));
      break;

    default:
      pp_c_cast_expression (pp, t);
      break;
    }
}

/* pm-expression:
      cast-expression
      pm-expression .* cast-expression
      pm-expression ->* cast-expression  */

static void
pp_cxx_pm_expression (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
      /* Handle unfortunate OFFSET_REF overloading here.  */
    case OFFSET_REF:
      if (TYPE_P (TREE_OPERAND (t, 0)))
	{
	  pp_cxx_qualified_id (pp, t);
	  break;
	}
      /* Fall through.  */
    case MEMBER_REF:
    case DOTSTAR_EXPR:
      pp_cxx_pm_expression (pp, TREE_OPERAND (t, 0));
      if (TREE_CODE (t) == MEMBER_REF)
	pp_cxx_arrow (pp);
      else
	pp_cxx_dot (pp);
      pp_star(pp);
      pp_cxx_cast_expression (pp, TREE_OPERAND (t, 1));
      break;


    default:
      pp_cxx_cast_expression (pp, t);
      break;
    }
}

/* multiplicative-expression:
      pm-expression
      multiplicative-expression * pm-expression
      multiplicative-expression / pm-expression
      multiplicative-expression % pm-expression  */

void
cxx_pretty_printer::multiplicative_expression (tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      multiplicative_expression (TREE_OPERAND (e, 0));
      pp_space (this);
      if (code == MULT_EXPR)
	pp_star (this);
      else if (code != TRUNC_MOD_EXPR)
	pp_slash (this);
      else
	pp_modulo (this);
      pp_space (this);
      pp_cxx_pm_expression (this, TREE_OPERAND (e, 1));
      break;

    default:
      pp_cxx_pm_expression (this, e);
      break;
    }
}

/* conditional-expression:
      logical-or-expression
      logical-or-expression ?  expression  : assignment-expression  */

void
cxx_pretty_printer::conditional_expression (tree e)
{
  if (TREE_CODE (e) == COND_EXPR)
    {
      pp_c_logical_or_expression (this, TREE_OPERAND (e, 0));
      pp_space (this);
      pp_question (this);
      pp_space (this);
      expression (TREE_OPERAND (e, 1));
      pp_space (this);
      assignment_expression (TREE_OPERAND (e, 2));
    }
  else
    pp_c_logical_or_expression (this, e);
}

/* Pretty-print a compound assignment operator token as indicated by T.  */

static void
pp_cxx_assignment_operator (cxx_pretty_printer *pp, tree t)
{
  const char *op;

  switch (TREE_CODE (t))
    {
    case NOP_EXPR:
      op = "=";
      break;

    case PLUS_EXPR:
      op = "+=";
      break;

    case MINUS_EXPR:
      op = "-=";
      break;

    case TRUNC_DIV_EXPR:
      op = "/=";
      break;

    case TRUNC_MOD_EXPR:
      op = "%=";
      break;

    default:
      op = get_tree_code_name (TREE_CODE (t));
      break;
    }

  pp_cxx_ws_string (pp, op);
}


/* assignment-expression:
      conditional-expression
      logical-or-expression assignment-operator assignment-expression
      throw-expression

   throw-expression:
       throw assignment-expression(opt)

   assignment-operator: one of
      =    *=    /=    %=    +=    -=    >>=    <<=    &=    ^=    |=  */

void
cxx_pretty_printer::assignment_expression (tree e)
{
  switch (TREE_CODE (e))
    {
    case MODIFY_EXPR:
    case INIT_EXPR:
      pp_c_logical_or_expression (this, TREE_OPERAND (e, 0));
      pp_space (this);
      pp_equal (this);
      pp_space (this);
      assignment_expression (TREE_OPERAND (e, 1));
      break;

    case THROW_EXPR:
      pp_cxx_ws_string (this, "throw");
      if (TREE_OPERAND (e, 0))
	assignment_expression (TREE_OPERAND (e, 0));
      break;

    case MODOP_EXPR:
      pp_c_logical_or_expression (this, TREE_OPERAND (e, 0));
      pp_cxx_assignment_operator (this, TREE_OPERAND (e, 1));
      assignment_expression (TREE_OPERAND (e, 2));
      break;

    default:
      conditional_expression (e);
      break;
    }
}

void
cxx_pretty_printer::expression (tree t)
{
  switch (TREE_CODE (t))
    {
    case STRING_CST:
    case VOID_CST:
    case INTEGER_CST:
    case REAL_CST:
    case COMPLEX_CST:
      constant (t);
      break;

    case USERDEF_LITERAL:
      pp_cxx_userdef_literal (this, t);
      break;

    case RESULT_DECL:
      pp_cxx_unqualified_id (this, t);
      break;

#if 0
    case OFFSET_REF:
#endif
    case SCOPE_REF:
    case PTRMEM_CST:
      pp_cxx_qualified_id (this, t);
      break;

    case OVERLOAD:
      t = OVL_FIRST (t);
      /* FALLTHRU */
    case VAR_DECL:
      if (DECL_NTTP_OBJECT_P (t))
	{
	  /* Print the type followed by the CONSTRUCTOR value of the
	     NTTP object.  */
	  simple_type_specifier (cv_unqualified (TREE_TYPE (t)));
	  expression (DECL_INITIAL (t));
	  break;
	}
      /* FALLTHRU */
    case PARM_DECL:
    case FIELD_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case BASELINK:
    case TEMPLATE_DECL:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TEMPLATE_PARM:
    case STMT_EXPR:
    case REQUIRES_EXPR:
      primary_expression (t);
      break;

    case CALL_EXPR:
    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
#if 0
    case MEMBER_REF:
#endif
    case EMPTY_CLASS_EXPR:
    case TYPEID_EXPR:
    case PSEUDO_DTOR_EXPR:
    case AGGR_INIT_EXPR:
    case ARROW_EXPR:
      postfix_expression (t);
      break;

    case NEW_EXPR:
    case VEC_NEW_EXPR:
      pp_cxx_new_expression (this, t);
      break;

    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      pp_cxx_delete_expression (this, t);
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case NOEXCEPT_EXPR:
    case UNARY_PLUS_EXPR:
      unary_expression (t);
      break;

    case CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      pp_cxx_cast_expression (this, t);
      break;

    case OFFSET_REF:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
      pp_cxx_pm_expression (this, t);
      break;

    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      multiplicative_expression (t);
      break;

    case COND_EXPR:
      conditional_expression (t);
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
    case THROW_EXPR:
    case MODOP_EXPR:
      assignment_expression (t);
      break;

    case MUST_NOT_THROW_EXPR:
      expression (TREE_OPERAND (t, 0));
      break;

    case EXPR_PACK_EXPANSION:
      expression (PACK_EXPANSION_PATTERN (t));
      pp_cxx_ws_string (this, "...");
      break;

    case UNARY_LEFT_FOLD_EXPR:
      pp_cxx_unary_left_fold_expression (this, t);
      break;

    case UNARY_RIGHT_FOLD_EXPR:
      pp_cxx_unary_right_fold_expression (this, t);
    break;

    case BINARY_LEFT_FOLD_EXPR:
    case BINARY_RIGHT_FOLD_EXPR:
      pp_cxx_binary_fold_expression (this, t);
      break;

    case TEMPLATE_ID_EXPR:
      pp_cxx_template_id (this, t);
      break;

    case NONTYPE_ARGUMENT_PACK:
      {
	tree args = ARGUMENT_PACK_ARGS (t);
	int i, len = TREE_VEC_LENGTH (args);
	pp_cxx_left_brace (this);
	for (i = 0; i < len; ++i)
	  {
	    if (i > 0)
	      pp_cxx_separate_with (this, ',');
	    expression (TREE_VEC_ELT (args, i));
	  }
	pp_cxx_right_brace (this);
      }
      break;

    case LAMBDA_EXPR:
      pp_cxx_ws_string (this, "<lambda>");
      break;

    case TRAIT_EXPR:
      pp_cxx_trait (this, t);
      break;

    case ATOMIC_CONSTR:
    case CONJ_CONSTR:
    case DISJ_CONSTR:
      pp_cxx_constraint (this, t);
      break;

    case PAREN_EXPR:
      pp_cxx_left_paren (this);
      expression (TREE_OPERAND (t, 0));
      pp_cxx_right_paren (this);
      break;

    case VIEW_CONVERT_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == TEMPLATE_PARM_INDEX)
	{
	  /* Strip const VIEW_CONVERT_EXPR wrappers for class NTTPs.  */
	  expression (TREE_OPERAND (t, 0));
	  break;
	}
      /* FALLTHRU */
    default:
      c_pretty_printer::expression (t);
      break;
    }
}


/* Declarations.  */

/* function-specifier:
      inline
      virtual
      explicit   */

void
cxx_pretty_printer::function_specifier (tree t)
{
  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
      if (DECL_VIRTUAL_P (t))
	pp_cxx_ws_string (this, "virtual");
      else if (DECL_CONSTRUCTOR_P (t) && DECL_NONCONVERTING_P (t))
	pp_cxx_ws_string (this, "explicit");
      else
        c_pretty_printer::function_specifier (t);

    default:
      break;
    }
}

/* decl-specifier-seq:
      decl-specifier-seq(opt) decl-specifier

   decl-specifier:
      storage-class-specifier
      type-specifier
      function-specifier
      friend
      typedef  */

void
cxx_pretty_printer::declaration_specifiers (tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FIELD_DECL:
      storage_class_specifier (t);
      declaration_specifiers (TREE_TYPE (t));
      break;

    case TYPE_DECL:
      pp_cxx_ws_string (this, "typedef");
      declaration_specifiers (TREE_TYPE (t));
      break;

    case FUNCTION_DECL:
      /* Constructors don't have return types.  And conversion functions
	 do not have a type-specifier in their return types.  */
      if (DECL_CONSTRUCTOR_P (t) || DECL_CONV_FN_P (t))
	function_specifier (t);
      else if (DECL_IOBJ_MEMBER_FUNCTION_P (t))
	declaration_specifiers (TREE_TYPE (TREE_TYPE (t)));
      else
        c_pretty_printer::declaration_specifiers (t);
      break;
    default:
        c_pretty_printer::declaration_specifiers (t);
      break;
    }
}

/* simple-type-specifier:
      ::(opt) nested-name-specifier(opt) type-name
      ::(opt) nested-name-specifier(opt) template(opt) template-id
      decltype-specifier
      char
      wchar_t
      bool
      short
      int
      long
      signed
      unsigned
      float
      double
      void  */

void
cxx_pretty_printer::simple_type_specifier (tree t)
{
  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      pp_cxx_qualified_id (this, t);
      break;

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      pp_cxx_unqualified_id (this, t);
      if (TREE_CODE (t) == TEMPLATE_TYPE_PARM)
	if (tree c = PLACEHOLDER_TYPE_CONSTRAINTS (t))
	  pp_cxx_constrained_type_spec (this, c);
      break;

    case TYPENAME_TYPE:
      pp_cxx_ws_string (this, "typename");
      pp_cxx_nested_name_specifier (this, TYPE_CONTEXT (t));
      pp_cxx_unqualified_id (this, TYPENAME_TYPE_FULLNAME (t));
      break;

    case DECLTYPE_TYPE:
      pp_cxx_ws_string (this, "decltype");
      pp_cxx_left_paren (this);
      this->expression (DECLTYPE_TYPE_EXPR (t));
      pp_cxx_right_paren (this);
      break;

    case NULLPTR_TYPE:
      pp_cxx_ws_string (this, "std::nullptr_t");
      break;

    case TRAIT_TYPE:
      pp_cxx_trait (this, t);
      break;

    default:
      c_pretty_printer::simple_type_specifier (t);
      break;
    }
}

/* type-specifier-seq:
      type-specifier type-specifier-seq(opt)

   type-specifier:
      simple-type-specifier
      class-specifier
      enum-specifier
      elaborated-type-specifier
      cv-qualifier   */

static void
pp_cxx_type_specifier_seq (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case TEMPLATE_DECL:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case TYPE_DECL:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case DECLTYPE_TYPE:
    case NULLPTR_TYPE:
      pp_cxx_cv_qualifier_seq (pp, t);
      pp->simple_type_specifier (t);
      break;

    case METHOD_TYPE:
      pp_cxx_type_specifier_seq (pp, TREE_TYPE (t));
      pp_cxx_space_for_pointer_operator (pp, TREE_TYPE (t));
      pp_cxx_nested_name_specifier (pp, TYPE_METHOD_BASETYPE (t));
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  tree pfm = TYPE_PTRMEMFUNC_FN_TYPE (t);
	  pp->declaration_specifiers (TREE_TYPE (TREE_TYPE (pfm)));
	  pp_cxx_whitespace (pp);
	  pp_cxx_ptr_operator (pp, t);
	  break;
	}
      /* fall through */

    case OFFSET_TYPE:
      if (TYPE_PTRDATAMEM_P (t))
	{
	  pp_cxx_type_specifier_seq (pp, TREE_TYPE (t));
	  pp_cxx_whitespace (pp);
	  pp_cxx_ptr_operator (pp, t);
	  break;
	}
      /* fall through */

    default:
      if (!(TREE_CODE (t) == FUNCTION_DECL && DECL_CONSTRUCTOR_P (t)))
	pp_c_specifier_qualifier_list (pp, t);
    }
}

/* ptr-operator:
      * cv-qualifier-seq(opt)
      &
      ::(opt) nested-name-specifier * cv-qualifier-seq(opt)  */

static void
pp_cxx_ptr_operator (cxx_pretty_printer *pp, tree t)
{
  if (!TYPE_P (t) && TREE_CODE (t) != TYPE_DECL)
    t = TREE_TYPE (t);
  switch (TREE_CODE (t))
    {
    case REFERENCE_TYPE:
    case POINTER_TYPE:
      if (TYPE_PTR_OR_PTRMEM_P (TREE_TYPE (t)))
	pp_cxx_ptr_operator (pp, TREE_TYPE (t));
      pp_c_attributes_display (pp, TYPE_ATTRIBUTES (TREE_TYPE (t)));
      if (TYPE_PTR_P (t))
	{
	  pp_star (pp);
	  pp_cxx_cv_qualifier_seq (pp, t);
	}
      else
	pp_ampersand (pp);
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  pp_cxx_left_paren (pp);
	  pp_cxx_nested_name_specifier (pp, TYPE_PTRMEMFUNC_OBJECT_TYPE (t));
	  pp_star (pp);
	  break;
	}
      /* FALLTHRU */
    case OFFSET_TYPE:
      if (TYPE_PTRMEM_P (t))
	{
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    pp_cxx_left_paren (pp);
	  pp_cxx_nested_name_specifier (pp, TYPE_PTRMEM_CLASS_TYPE (t));
	  pp_star (pp);
	  pp_cxx_cv_qualifier_seq (pp, t);
	  break;
	}
      /* fall through.  */

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

static inline tree
pp_cxx_implicit_parameter_type (tree mf)
{
  return class_of_this_parm (TREE_TYPE (mf));
}

/*
   parameter-declaration:
      decl-specifier-seq declarator
      decl-specifier-seq declarator = assignment-expression
      decl-specifier-seq abstract-declarator(opt)
      decl-specifier-seq abstract-declarator(opt) assignment-expression  */

static inline void
pp_cxx_parameter_declaration (cxx_pretty_printer *pp, tree t)
{
  pp->declaration_specifiers (t);
  if (TYPE_P (t))
    pp->abstract_declarator (t);
  else
    pp->declarator (t);
}

/* parameter-declaration-clause:
      parameter-declaration-list(opt) ...(opt)
      parameter-declaration-list , ...

   parameter-declaration-list:
      parameter-declaration
      parameter-declaration-list , parameter-declaration  */

static void
pp_cxx_parameter_declaration_clause (cxx_pretty_printer *pp, tree t)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (t) || TREE_CODE (t) == FUNCTION_DECL);
  tree types, args;
  if (TYPE_P (t))
    {
      types = TYPE_ARG_TYPES (t);
      args = NULL_TREE;
    }
  else
    {
      types = FUNCTION_FIRST_USER_PARMTYPE (t);
      args = FUNCTION_FIRST_USER_PARM (t);
    }
  bool abstract = !args || (pp->flags & pp_c_flag_abstract);

  /* Skip artificial parameter for non-static member functions.  */
  if (TREE_CODE (t) == METHOD_TYPE)
    types = TREE_CHAIN (types);

  bool first = true;
  pp_cxx_left_paren (pp);
  for (; types != void_list_node; types = TREE_CHAIN (types))
    {
      if (!first)
	pp_cxx_separate_with (pp, ',');
      first = false;
      if (!types)
	{
	  pp_cxx_ws_string (pp, "...");
	  break;
	}
      pp_cxx_parameter_declaration (pp, abstract ? TREE_VALUE (types) : args);
      if (!abstract && pp->flags & pp_cxx_flag_default_argument)
	{
	  pp_cxx_whitespace (pp);
	  pp_equal (pp);
	  pp_cxx_whitespace (pp);
	  pp->assignment_expression (TREE_PURPOSE (types));
	}
      if (!abstract)
	args = TREE_CHAIN (args);
    }
  pp_cxx_right_paren (pp);
}

/* exception-specification:
      throw ( type-id-list(opt) )

   type-id-list
      type-id
      type-id-list , type-id   */

static void
pp_cxx_exception_specification (cxx_pretty_printer *pp, tree t)
{
  tree ex_spec = TYPE_RAISES_EXCEPTIONS (t);
  bool need_comma = false;

  if (ex_spec == NULL)
    return;
  if (TREE_PURPOSE (ex_spec))
    {
      pp_cxx_ws_string (pp, "noexcept");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      if (DEFERRED_NOEXCEPT_SPEC_P (ex_spec))
	pp_cxx_ws_string (pp, "<uninstantiated>");
      else
	pp->expression (TREE_PURPOSE (ex_spec));
      pp_cxx_right_paren (pp);
      return;
    }
  pp_cxx_ws_string (pp, "throw");
  pp_cxx_left_paren (pp);
  for (; ex_spec && TREE_VALUE (ex_spec); ex_spec = TREE_CHAIN (ex_spec))
    {
      tree type = TREE_VALUE (ex_spec);
      tree argpack = NULL_TREE;
      int i, len = 1;

      if (ARGUMENT_PACK_P (type))
	{
	  argpack = ARGUMENT_PACK_ARGS (type);
	  len = TREE_VEC_LENGTH (argpack);
	}

      for (i = 0; i < len; ++i)
	{
	  if (argpack)
	    type = TREE_VEC_ELT (argpack, i);

	  if (need_comma)
	    pp_cxx_separate_with (pp, ',');
	  else
	    need_comma = true;

	  pp->type_id (type);
	}
    }
  pp_cxx_right_paren (pp);
}

/* direct-declarator:
      declarator-id
      direct-declarator ( parameter-declaration-clause ) cv-qualifier-seq(opt)
					    exception-specification(opt)
      direct-declaration [ constant-expression(opt) ]
      ( declarator )  */

void
cxx_pretty_printer::direct_declarator (tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FIELD_DECL:
      if (DECL_NAME (t))
	{
	  pp_cxx_space_for_pointer_operator (this, TREE_TYPE (t));

	  if ((TREE_CODE (t) == PARM_DECL && DECL_PACK_P (t))
	      || template_parameter_pack_p (t))
	    /* A function parameter pack or non-type template
	       parameter pack.  */
	    pp_cxx_ws_string (this, "...");

	  id_expression (DECL_NAME (t));
	}
      abstract_declarator (TREE_TYPE (t));
      break;

    case FUNCTION_DECL:
      pp_cxx_space_for_pointer_operator (this, TREE_TYPE (TREE_TYPE (t)));
      expression (t);
      pp_cxx_parameter_declaration_clause (this, t);

      if (DECL_IOBJ_MEMBER_FUNCTION_P (t))
	{
	  set_padding (pp_before);
	  pp_cxx_cv_qualifier_seq (this, pp_cxx_implicit_parameter_type (t));
	}

      pp_cxx_exception_specification (this, TREE_TYPE (t));
      break;

    case TYPENAME_TYPE:
    case TEMPLATE_DECL:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TEMPLATE_PARM:
      break;

    default:
      c_pretty_printer::direct_declarator (t);
      break;
    }
}

/* declarator:
   direct-declarator
   ptr-operator declarator  */

void
cxx_pretty_printer::declarator (tree t)
{
  direct_declarator (t);

  // Print a requires clause.
  if (flag_concepts)
    if (tree ci = get_constraints (t))
      if (tree reqs = CI_DECLARATOR_REQS (ci))
        pp_cxx_requires_clause (this, reqs);
}

/* ctor-initializer:
      : mem-initializer-list

   mem-initializer-list:
      mem-initializer
      mem-initializer , mem-initializer-list

   mem-initializer:
      mem-initializer-id ( expression-list(opt) )

   mem-initializer-id:
      ::(opt) nested-name-specifier(opt) class-name
      identifier   */

static void
pp_cxx_ctor_initializer (cxx_pretty_printer *pp, tree t)
{
  t = TREE_OPERAND (t, 0);
  pp_cxx_whitespace (pp);
  pp_colon (pp);
  pp_cxx_whitespace (pp);
  for (; t; t = TREE_CHAIN (t))
    {
      tree purpose = TREE_PURPOSE (t);
      bool is_pack = PACK_EXPANSION_P (purpose);

      if (is_pack)
	pp->primary_expression (PACK_EXPANSION_PATTERN (purpose));
      else
	pp->primary_expression (purpose);
      pp_cxx_call_argument_list (pp, TREE_VALUE (t));
      if (is_pack)
	pp_cxx_ws_string (pp, "...");
      if (TREE_CHAIN (t))
	pp_cxx_separate_with (pp, ',');
    }
}

/* function-definition:
      decl-specifier-seq(opt) declarator ctor-initializer(opt) function-body
      decl-specifier-seq(opt) declarator function-try-block  */

static void
pp_cxx_function_definition (cxx_pretty_printer *pp, tree t)
{
  tree saved_scope = pp->enclosing_scope;
  pp->declaration_specifiers (t);
  pp->declarator (t);
  pp_needs_newline (pp) = true;
  pp->enclosing_scope = DECL_CONTEXT (t);
  if (DECL_SAVED_TREE (t))
    pp->statement (DECL_SAVED_TREE (t));
  else
    pp_cxx_semicolon (pp);
  pp_newline_and_flush (pp);
  pp->enclosing_scope = saved_scope;
}

/* abstract-declarator:
      ptr-operator abstract-declarator(opt)
      direct-abstract-declarator  */

void
cxx_pretty_printer::abstract_declarator (tree t)
{
  /* pp_cxx_ptr_operator prints '(' for a pointer-to-member function,
     or a pointer-to-data-member of array type:

       void (X::*)()
       int (X::*)[5]

     but not for a pointer-to-data-member of non-array type:

       int X::*

     so be mindful of that.  */
  if (TYPE_PTRMEMFUNC_P (t)
      || (TYPE_PTRDATAMEM_P (t)
	  && TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE))
    pp_cxx_right_paren (this);
  else if (INDIRECT_TYPE_P (t))
    {
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	pp_cxx_right_paren (this);
      t = TREE_TYPE (t);
    }
  direct_abstract_declarator (t);
}

/* direct-abstract-declarator:
      direct-abstract-declarator(opt) ( parameter-declaration-clause )
			   cv-qualifier-seq(opt) exception-specification(opt)
      direct-abstract-declarator(opt) [ constant-expression(opt) ]
      ( abstract-declarator )  */

void
cxx_pretty_printer::direct_abstract_declarator (tree t)
{
  switch (TREE_CODE (t))
    {
    case REFERENCE_TYPE:
      abstract_declarator (t);
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	direct_abstract_declarator (TYPE_PTRMEMFUNC_FN_TYPE (t));
      break;

    case OFFSET_TYPE:
      if (TYPE_PTRDATAMEM_P (t))
	direct_abstract_declarator (TREE_TYPE (t));
      break;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      pp_cxx_parameter_declaration_clause (this, t);
      direct_abstract_declarator (TREE_TYPE (t));
      if (TREE_CODE (t) == METHOD_TYPE)
	{
	  set_padding (pp_before);
	  pp_cxx_cv_qualifier_seq (this, class_of_this_parm (t));
	}
      pp_cxx_exception_specification (this, t);
      break;

    case TYPENAME_TYPE:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case UNBOUND_CLASS_TEMPLATE:
    case DECLTYPE_TYPE:
      break;

    default:
      c_pretty_printer::direct_abstract_declarator (t);
      break;
    }
}

/* type-id:
     type-specifier-seq abstract-declarator(opt) */

void
cxx_pretty_printer::type_id (tree t)
{
  pp_flags saved_flags = flags;
  flags |= pp_c_flag_abstract;

  switch (TREE_CODE (t))
    {
    case TYPE_DECL:
    case UNION_TYPE:
    case RECORD_TYPE:
    case ENUMERAL_TYPE:
    case TYPENAME_TYPE:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case UNBOUND_CLASS_TEMPLATE:
    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_DECL:
    case TYPEOF_TYPE:
    case TRAIT_TYPE:
    case DECLTYPE_TYPE:
    case NULLPTR_TYPE:
    case TEMPLATE_ID_EXPR:
    case OFFSET_TYPE:
      pp_cxx_type_specifier_seq (this, t);
      if (TYPE_PTRMEM_P (t))
	abstract_declarator (t);
      break;

    case TYPE_PACK_EXPANSION:
      type_id (PACK_EXPANSION_PATTERN (t));
      pp_cxx_ws_string (this, "...");
      break;

    case TYPE_ARGUMENT_PACK:
      {
	tree args = ARGUMENT_PACK_ARGS (t);
	int len = TREE_VEC_LENGTH (args);
	pp_cxx_left_brace (this);
	for (int i = 0; i < len; ++i)
	  {
	    if (i > 0)
	      pp_cxx_separate_with (this, ',');
	    type_id (TREE_VEC_ELT (args, i));
	  }
	pp_cxx_right_brace (this);
      }
      break;

    default:
      c_pretty_printer::type_id (t);
      break;
    }

  flags = saved_flags;
}

/* template-argument-list:
      template-argument ...(opt)
      template-argument-list, template-argument ...(opt)

   template-argument:
      assignment-expression
      type-id
      template-name  */

static void
pp_cxx_template_argument_list (cxx_pretty_printer *pp, tree t)
{
  int i;
  bool need_comma = false;

  if (t == NULL)
    return;
  for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
    {
      tree arg = TREE_VEC_ELT (t, i);
      tree argpack = NULL_TREE;
      int idx, len = 1;

      if (ARGUMENT_PACK_P (arg))
	{
	  argpack = ARGUMENT_PACK_ARGS (arg);
	  len = TREE_VEC_LENGTH (argpack);
	}

      for (idx = 0; idx < len; idx++)
	{
	  if (argpack)
	    arg = TREE_VEC_ELT (argpack, idx);

	  if (need_comma)
	    pp_cxx_separate_with (pp, ',');
	  else
	    need_comma = true;

	  if (TYPE_P (arg) || (TREE_CODE (arg) == TEMPLATE_DECL
			       && TYPE_P (DECL_TEMPLATE_RESULT (arg))))
	    pp->type_id (arg);
	  else
	    pp->expression (arg);
	}
    }
}


static void
pp_cxx_exception_declaration (cxx_pretty_printer *pp, tree t)
{
  t = DECL_EXPR_DECL (t);
  pp_cxx_type_specifier_seq (pp, t);
  if (TYPE_P (t))
    pp->abstract_declarator (t);
  else
    pp->declarator (t);
}

/* Statements.  */

void
cxx_pretty_printer::statement (tree t)
{
  switch (TREE_CODE (t))
    {
    case CTOR_INITIALIZER:
      pp_cxx_ctor_initializer (this, t);
      break;

    case USING_STMT:
      pp_cxx_ws_string (this, "using");
      pp_cxx_ws_string (this, "namespace");
      if (DECL_CONTEXT (t))
	pp_cxx_nested_name_specifier (this, DECL_CONTEXT (t));
      pp_cxx_qualified_id (this, USING_STMT_NAMESPACE (t));
      break;

    case USING_DECL:
      pp_cxx_ws_string (this, "using");
      pp_cxx_nested_name_specifier (this, USING_DECL_SCOPE (t));
      pp_cxx_unqualified_id (this, DECL_NAME (t));
      break;

    case EH_SPEC_BLOCK:
      break;

      /* try-block:
	    try compound-statement handler-seq  */
    case TRY_BLOCK:
      pp_maybe_newline_and_indent (this, 0);
      pp_cxx_ws_string (this, "try");
      pp_newline_and_indent (this, 3);
      statement (TRY_STMTS (t));
      pp_newline_and_indent (this, -3);
      if (CLEANUP_P (t))
	;
      else
	statement (TRY_HANDLERS (t));
      break;

      /*
	 handler-seq:
	    handler handler-seq(opt)

	 handler:
	 catch ( exception-declaration ) compound-statement

	 exception-declaration:
	    type-specifier-seq declarator
	    type-specifier-seq abstract-declarator
	    ...   */
    case HANDLER:
      pp_cxx_ws_string (this, "catch");
      pp_cxx_left_paren (this);
      pp_cxx_exception_declaration (this, HANDLER_PARMS (t));
      pp_cxx_right_paren (this);
      pp_indentation (this) += 3;
      pp_needs_newline (this) = true;
      statement (HANDLER_BODY (t));
      pp_indentation (this) -= 3;
      pp_needs_newline (this) = true;
      break;

      /* selection-statement:
	    if ( expression ) statement
	    if ( expression ) statement else statement  */
    case IF_STMT:
      pp_cxx_ws_string (this, "if");
      pp_cxx_whitespace (this);
      pp_cxx_left_paren (this);
      expression (IF_COND (t));
      pp_cxx_right_paren (this);
      pp_newline_and_indent (this, 2);
      statement (THEN_CLAUSE (t));
      pp_newline_and_indent (this, -2);
      if (ELSE_CLAUSE (t))
	{
	  tree else_clause = ELSE_CLAUSE (t);
	  pp_cxx_ws_string (this, "else");
	  if (TREE_CODE (else_clause) == IF_STMT)
	    pp_cxx_whitespace (this);
	  else
	    pp_newline_and_indent (this, 2);
	  statement (else_clause);
	  if (TREE_CODE (else_clause) != IF_STMT)
	    pp_newline_and_indent (this, -2);
	}
      break;

    case RANGE_FOR_STMT:
      pp_cxx_ws_string (this, "for");
      pp_space (this);
      pp_cxx_left_paren (this);
      if (RANGE_FOR_INIT_STMT (t))
	{
	  statement (RANGE_FOR_INIT_STMT (t));
	  pp_needs_newline (this) = false;
	  pp_cxx_whitespace (this);
	}
      statement (RANGE_FOR_DECL (t));
      pp_space (this);
      pp_needs_newline (this) = false;
      pp_colon (this);
      pp_space (this);
      statement (RANGE_FOR_EXPR (t));
      pp_cxx_right_paren (this);
      pp_newline_and_indent (this, 3);
      statement (FOR_BODY (t));
      pp_indentation (this) -= 3;
      pp_needs_newline (this) = true;
      break;

      /* expression-statement:
	    expression(opt) ;  */
    case EXPR_STMT:
      expression (EXPR_STMT_EXPR (t));
      pp_cxx_semicolon (this);
      pp_needs_newline (this) = true;
      break;

    case CLEANUP_STMT:
      pp_cxx_ws_string (this, "try");
      pp_newline_and_indent (this, 2);
      statement (CLEANUP_BODY (t));
      pp_newline_and_indent (this, -2);
      pp_cxx_ws_string (this, CLEANUP_EH_ONLY (t) ? "catch" : "finally");
      pp_newline_and_indent (this, 2);
      statement (CLEANUP_EXPR (t));
      pp_newline_and_indent (this, -2);
      break;

    case STATIC_ASSERT:
      declaration (t);
      break;

    case OMP_DEPOBJ:
      pp_cxx_ws_string (this, "#pragma omp depobj");
      pp_space (this);
      pp_cxx_left_paren (this);
      expression (OMP_DEPOBJ_DEPOBJ (t));
      pp_cxx_right_paren (this);
      if (OMP_DEPOBJ_CLAUSES (t) && OMP_DEPOBJ_CLAUSES (t) != error_mark_node)
	{
	  if (TREE_CODE (OMP_DEPOBJ_CLAUSES (t)) == OMP_CLAUSE)
	    dump_omp_clauses (this, OMP_DEPOBJ_CLAUSES (t),
			      pp_indentation (this), TDF_NONE);
	  else
	    switch (tree_to_uhwi (OMP_DEPOBJ_CLAUSES (t)))
	      {
	      case OMP_CLAUSE_DEPEND_IN:
		pp_cxx_ws_string (this, " update(in)");
		break;
	      case OMP_CLAUSE_DEPEND_INOUT:
		pp_cxx_ws_string (this, " update(inout)");
		break;
	      case OMP_CLAUSE_DEPEND_OUT:
		pp_cxx_ws_string (this, " update(out)");
		break;
	      case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
		pp_cxx_ws_string (this, " update(mutexinoutset)");
		break;
	      case OMP_CLAUSE_DEPEND_INOUTSET:
		pp_cxx_ws_string (this, " update(inoutset)");
		break;
	      case OMP_CLAUSE_DEPEND_LAST:
		pp_cxx_ws_string (this, " destroy");
		break;
	      default:
		break;
	      }
	}
      pp_needs_newline (this) = true;
      break;

    default:
      c_pretty_printer::statement (t);
      break;
    }
}

/* original-namespace-definition:
      namespace identifier { namespace-body }

  As an edge case, we also handle unnamed namespace definition here.  */

static void
pp_cxx_original_namespace_definition (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "namespace");
  if (DECL_CONTEXT (t))
    pp_cxx_nested_name_specifier (pp, DECL_CONTEXT (t));
  if (DECL_NAME (t))
    pp_cxx_unqualified_id (pp, t);
  pp_cxx_whitespace (pp);
  pp_cxx_left_brace (pp);
  /* We do not print the namespace-body.  */
  pp_cxx_whitespace (pp);
  pp_cxx_right_brace (pp);
}

/* namespace-alias:
      identifier

   namespace-alias-definition:
      namespace identifier = qualified-namespace-specifier ;

   qualified-namespace-specifier:
      ::(opt) nested-name-specifier(opt) namespace-name   */

static void
pp_cxx_namespace_alias_definition (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "namespace");
  if (DECL_CONTEXT (t))
    pp_cxx_nested_name_specifier (pp, DECL_CONTEXT (t));
  pp_cxx_unqualified_id (pp, t);
  pp_cxx_whitespace (pp);
  pp_equal (pp);
  pp_cxx_whitespace (pp);
  if (DECL_CONTEXT (DECL_NAMESPACE_ALIAS (t)))
    pp_cxx_nested_name_specifier (pp,
				  DECL_CONTEXT (DECL_NAMESPACE_ALIAS (t)));
  pp_cxx_qualified_id (pp, DECL_NAMESPACE_ALIAS (t));
  pp_cxx_semicolon (pp);
}

/* simple-declaration:
      decl-specifier-seq(opt) init-declarator-list(opt)  */

static void
pp_cxx_simple_declaration (cxx_pretty_printer *pp, tree t)
{
  pp->declaration_specifiers (t);
  pp_cxx_init_declarator (pp, t);
  pp_cxx_semicolon (pp);
  pp_needs_newline (pp) = true;
}

/*
  template-parameter-list:
     template-parameter
     template-parameter-list , template-parameter  */

static inline void
pp_cxx_template_parameter_list (cxx_pretty_printer *pp, tree t)
{
  const int n = TREE_VEC_LENGTH (t);
  int i;
  for (i = 0; i < n; ++i)
    {
      if (i)
	pp_cxx_separate_with (pp, ',');
      pp_cxx_template_parameter (pp, TREE_VEC_ELT (t, i));
    }
}

/* template-parameter:
      type-parameter
      parameter-declaration

   type-parameter:
     class ...(opt) identifier(opt)
     class identifier(opt) = type-id
     typename identifier(opt)
     typename ...(opt) identifier(opt) = type-id
     template < template-parameter-list > class ...(opt) identifier(opt)
     template < template-parameter-list > class identifier(opt) = template-name  */

static void
pp_cxx_template_parameter (cxx_pretty_printer *pp, tree t)
{
  tree parameter =  TREE_VALUE (t);
  switch (TREE_CODE (parameter))
    {
    case TYPE_DECL:
      pp_cxx_ws_string (pp, "class");
      if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (t)))
	pp_cxx_ws_string (pp, "...");
      if (DECL_NAME (parameter))
	pp_cxx_tree_identifier (pp, DECL_NAME (parameter));
      /* FIXME: Check if we should print also default argument.  */
      break;

    case PARM_DECL:
      pp_cxx_parameter_declaration (pp, parameter);
      break;

    case TEMPLATE_DECL:
      break;

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* Pretty-print a template parameter in the canonical form
   "template-parameter-<level>-<position in parameter list>".  */

void
pp_cxx_canonical_template_parameter (cxx_pretty_printer *pp, tree parm)
{
  const enum tree_code code = TREE_CODE (parm);

  /* Brings type template parameters to the canonical forms.  */
  if (code == TEMPLATE_TYPE_PARM || code == TEMPLATE_TEMPLATE_PARM
      || code == BOUND_TEMPLATE_TEMPLATE_PARM)
    parm = TEMPLATE_TYPE_PARM_INDEX (parm);

  pp_cxx_begin_template_argument_list (pp);
  pp->translate_string ("template-parameter-");
  pp_wide_integer (pp, TEMPLATE_PARM_LEVEL (parm));
  pp_minus (pp);
  pp_wide_integer (pp, TEMPLATE_PARM_IDX (parm) + 1);
  pp_cxx_end_template_argument_list (pp);
}

/* Print a constrained-type-specifier.  */

void
pp_cxx_constrained_type_spec (cxx_pretty_printer *pp, tree c)
{
  pp_cxx_whitespace (pp);
  pp_cxx_left_bracket (pp);
  pp->translate_string ("requires");
  pp_cxx_whitespace (pp);
  if (c == error_mark_node)
    {
      pp_cxx_ws_string(pp, "<unsatisfied-type-constraint>");
      return;
    }
  tree t, a;
  placeholder_extract_concept_and_args (c, t, a);
  pp->id_expression (t);
  pp_cxx_begin_template_argument_list (pp);
  pp_cxx_ws_string (pp, "<placeholder>");
  pp_cxx_separate_with (pp, ',');
  tree args = make_tree_vec (TREE_VEC_LENGTH (a) - 1);
  for (int i = 0; i < TREE_VEC_LENGTH (a) - 1; ++i)
    TREE_VEC_ELT (args, i) = TREE_VEC_ELT (a, i + 1);
  pp_cxx_template_argument_list (pp, args);
  ggc_free (args);
  pp_cxx_end_template_argument_list (pp);
  pp_cxx_right_bracket (pp);
}

/*
  template-declaration:
     export(opt) template < template-parameter-list > declaration

  Concept extensions:

  template-declaration:
     export(opt) template < template-parameter-list >
       requires-clause(opt) declaration */

static void
pp_cxx_template_declaration (cxx_pretty_printer *pp, tree t)
{
  tree tmpl = most_general_template (t);
  tree level;

  pp_maybe_newline_and_indent (pp, 0);
  for (level = DECL_TEMPLATE_PARMS (tmpl); level; level = TREE_CHAIN (level))
    {
      pp_cxx_ws_string (pp, "template");
      pp_cxx_begin_template_argument_list (pp);
      pp_cxx_template_parameter_list (pp, TREE_VALUE (level));
      pp_cxx_end_template_argument_list (pp);
      pp_newline_and_indent (pp, 3);
    }

  if (flag_concepts)
    if (tree ci = get_constraints (t))
      if (tree reqs = CI_TEMPLATE_REQS (ci))
         {
            pp_cxx_requires_clause (pp, reqs);
            pp_newline_and_indent (pp, 6);
         }

  if (TREE_CODE (t) == FUNCTION_DECL && DECL_SAVED_TREE (t))
    pp_cxx_function_definition (pp, t);
  else if (TREE_CODE (t) == CONCEPT_DECL)
    pp_cxx_concept_definition (pp, t);
  else
    pp_cxx_simple_declaration (pp, t);
}

static void
pp_cxx_explicit_specialization (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_explicit_instantiation (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_concept_definition (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_unqualified_id (pp, DECL_NAME (t));
  pp_cxx_whitespace (pp);
  pp_cxx_ws_string (pp, "=");
  pp_cxx_whitespace (pp);
  pp->expression (DECL_INITIAL (t));
  pp_cxx_semicolon (pp);
}

/*
    declaration:
       block-declaration
       function-definition
       template-declaration
       explicit-instantiation
       explicit-specialization
       linkage-specification
       namespace-definition

    block-declaration:
       simple-declaration
       asm-definition
       namespace-alias-definition
       using-declaration
       using-directive
       static_assert-declaration */
void
cxx_pretty_printer::declaration (tree t)
{
  if (TREE_CODE (t) == STATIC_ASSERT)
    {
      pp_cxx_ws_string (this, "static_assert");
      pp_cxx_left_paren (this);
      expression (STATIC_ASSERT_CONDITION (t));
      pp_cxx_separate_with (this, ',');
      expression (STATIC_ASSERT_MESSAGE (t));
      pp_cxx_right_paren (this);
    }
  else if (!DECL_LANG_SPECIFIC (t))
    pp_cxx_simple_declaration (this, t);
  else if (DECL_USE_TEMPLATE (t))
    switch (DECL_USE_TEMPLATE (t))
      {
      case 1:
	pp_cxx_template_declaration (this, t);
	break;

      case 2:
	pp_cxx_explicit_specialization (this, t);
	break;

      case 3:
	pp_cxx_explicit_instantiation (this, t);
	break;

      default:
	break;
      }
  else switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case TYPE_DECL:
      pp_cxx_simple_declaration (this, t);
      break;

    case FUNCTION_DECL:
      if (DECL_SAVED_TREE (t))
	pp_cxx_function_definition (this, t);
      else
	pp_cxx_simple_declaration (this, t);
      break;

    case NAMESPACE_DECL:
      if (DECL_NAMESPACE_ALIAS (t))
	pp_cxx_namespace_alias_definition (this, t);
      else
	pp_cxx_original_namespace_definition (this, t);
      break;

    default:
      pp_unsupported_tree (this, t);
      break;
    }
}

static void
pp_cxx_typeid_expression (cxx_pretty_printer *pp, tree t)
{
  t = TREE_OPERAND (t, 0);
  pp_cxx_ws_string (pp, "typeid");
  pp_cxx_left_paren (pp);
  if (TYPE_P (t))
    pp->type_id (t);
  else
    pp->expression (t);
  pp_cxx_right_paren (pp);
}

void
pp_cxx_va_arg_expression (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "va_arg");
  pp_cxx_left_paren (pp);
  pp->assignment_expression (TREE_OPERAND (t, 0));
  pp_cxx_separate_with (pp, ',');
  pp->type_id (TREE_TYPE (t));
  pp_cxx_right_paren (pp);
}

static bool
pp_cxx_offsetof_expression_1 (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case ARROW_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == STATIC_CAST_EXPR
	  && INDIRECT_TYPE_P (TREE_TYPE (TREE_OPERAND (t, 0))))
	{
	  pp->type_id (TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 0))));
	  pp_cxx_separate_with (pp, ',');
	  return true;
	}
      return false;
    case COMPONENT_REF:
      if (!pp_cxx_offsetof_expression_1 (pp, TREE_OPERAND (t, 0)))
	return false;
      if (TREE_CODE (TREE_OPERAND (t, 0)) != ARROW_EXPR)
	pp_cxx_dot (pp);
      pp->expression (TREE_OPERAND (t, 1));
      return true;
    case ARRAY_REF:
      if (!pp_cxx_offsetof_expression_1 (pp, TREE_OPERAND (t, 0)))
	return false;
      pp_left_bracket (pp);
      pp->expression (TREE_OPERAND (t, 1));
      pp_right_bracket (pp);
      return true;
    default:
      return false;
    }
}

void
pp_cxx_offsetof_expression (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "offsetof");
  pp_cxx_left_paren (pp);
  if (!pp_cxx_offsetof_expression_1 (pp, TREE_OPERAND (t, 0)))
    pp->expression (TREE_OPERAND (t, 0));
  pp_cxx_right_paren (pp);
}

void
pp_cxx_addressof_expression (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "__builtin_addressof");
  pp_cxx_left_paren (pp);
  pp->expression (TREE_OPERAND (t, 0));
  pp_cxx_right_paren (pp);
}

static char const*
get_fold_operator (tree t)
{
  ovl_op_info_t *info = OVL_OP_INFO (FOLD_EXPR_MODIFY_P (t),
				     FOLD_EXPR_OP (t));
  return info->name;
}

void
pp_cxx_unary_left_fold_expression (cxx_pretty_printer *pp, tree t)
{
  char const* op = get_fold_operator (t);
  tree expr = PACK_EXPANSION_PATTERN (FOLD_EXPR_PACK (t));
  pp_cxx_left_paren (pp);
  pp_cxx_ws_string (pp, "...");
  pp_cxx_ws_string (pp, op);
  pp->expression (expr);
  pp_cxx_right_paren (pp);
}

void
pp_cxx_unary_right_fold_expression (cxx_pretty_printer *pp, tree t)
{
  char const* op = get_fold_operator (t);
  tree expr = PACK_EXPANSION_PATTERN (FOLD_EXPR_PACK (t));
  pp_cxx_left_paren (pp);
  pp->expression (expr);
  pp_space (pp);
  pp_cxx_ws_string (pp, op);
  pp_cxx_ws_string (pp, "...");
  pp_cxx_right_paren (pp);
}

void
pp_cxx_binary_fold_expression (cxx_pretty_printer *pp, tree t)
{
  char const* op = get_fold_operator (t);
  tree t1 = TREE_OPERAND (t, 1);
  tree t2 = TREE_OPERAND (t, 2);
  if (t1 == FOLD_EXPR_PACK (t))
    t1 = PACK_EXPANSION_PATTERN (t1);
  else
    t2 = PACK_EXPANSION_PATTERN (t2);
  pp_cxx_left_paren (pp);
  pp->expression (t1);
  pp_cxx_ws_string (pp, op);
  pp_cxx_ws_string (pp, "...");
  pp_cxx_ws_string (pp, op);
  pp->expression (t2);
  pp_cxx_right_paren (pp);
}

void
pp_cxx_trait (cxx_pretty_printer *pp, tree t)
{
  cp_trait_kind kind;
  tree type1, type2;
  if (TREE_CODE (t) == TRAIT_EXPR)
    {
      kind = TRAIT_EXPR_KIND (t);
      type1 = TRAIT_EXPR_TYPE1 (t);
      type2 = TRAIT_EXPR_TYPE2 (t);
    }
  else
    {
      kind = TRAIT_TYPE_KIND (t);
      type1 = TRAIT_TYPE_TYPE1 (t);
      type2 = TRAIT_TYPE_TYPE2 (t);
    }

  switch (kind)
    {
#define DEFTRAIT(TCC, CODE, NAME, ARITY) \
    case CPTK_##CODE:			 \
      pp_cxx_ws_string (pp, NAME);	 \
      break;
#include "cp-trait.def"
#undef DEFTRAIT
    }

  if (kind == CPTK_TYPE_PACK_ELEMENT)
    {
      pp_cxx_begin_template_argument_list (pp);
      pp->expression (type1);
    }
  else
    {
      pp_cxx_left_paren (pp);
      if (TYPE_P (type1))
	pp->type_id (type1);
      else
	pp->expression (type1);
    }
  if (type2)
    {
      if (TREE_CODE (type2) != TREE_VEC)
	{
	  pp_cxx_separate_with (pp, ',');
	  pp->type_id (type2);
	}
      else
	for (tree arg : tree_vec_range (type2))
	  {
	    pp_cxx_separate_with (pp, ',');
	    pp->type_id (arg);
	  }
    }
  if (kind == CPTK_TYPE_PACK_ELEMENT)
    pp_cxx_end_template_argument_list (pp);
  else
    pp_cxx_right_paren (pp);
}

// requires-clause:
//    'requires' logical-or-expression
void
pp_cxx_requires_clause (cxx_pretty_printer *pp, tree t)
{
  if (!t)
    return;
  pp->set_padding (pp_before);
  pp_cxx_ws_string (pp, "requires");
  pp_space (pp);
  pp->expression (t);
}

/* requirement:
     simple-requirement
     compound-requirement
     type-requirement
     nested-requirement */
static void
pp_cxx_requirement (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case SIMPLE_REQ:
      pp_cxx_simple_requirement (pp, t);
      break;

    case TYPE_REQ:
      pp_cxx_type_requirement (pp, t);
      break;

    case COMPOUND_REQ:
      pp_cxx_compound_requirement (pp, t);
      break;

    case NESTED_REQ:
      pp_cxx_nested_requirement (pp, t);
      break;

    default:
      gcc_unreachable ();
    }
}

// requirement-list:
//    requirement
//    requirement-list ';' requirement[opt]
//
static void
pp_cxx_requirement_list (cxx_pretty_printer *pp, tree t)
{
  for (; t; t = TREE_CHAIN (t))
    pp_cxx_requirement (pp, TREE_VALUE (t));
}

// requirement-body:
//    '{' requirement-list '}'
static void
pp_cxx_requirement_body (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_left_brace (pp);
  pp_cxx_requirement_list (pp, t);
  pp_cxx_right_brace (pp);
}

// requires-expression:
//    'requires' requirement-parameter-list requirement-body
void
pp_cxx_requires_expr (cxx_pretty_printer *pp, tree t)
{
  pp_string (pp, "requires");
  if (tree parms = REQUIRES_EXPR_PARMS (t))
    {
      bool first = true;
      pp_cxx_left_paren (pp);
      for (; parms; parms = TREE_CHAIN (parms))
	{
	  if (!first)
	    pp_cxx_separate_with (pp, ',' );
	  first = false;
	  pp_cxx_parameter_declaration (pp, parms);
	}
      pp_cxx_right_paren (pp);
      pp_cxx_whitespace (pp);
    }
  pp_cxx_requirement_body (pp, TREE_OPERAND (t, 1));
}

/* simple-requirement:
     expression ';' */
void
pp_cxx_simple_requirement (cxx_pretty_printer *pp, tree t)
{
  pp->expression (TREE_OPERAND (t, 0));
  pp_cxx_semicolon (pp);
}

/* type-requirement:
     typename type-name ';' */
void
pp_cxx_type_requirement (cxx_pretty_printer *pp, tree t)
{
  pp->type_id (TREE_OPERAND (t, 0));
  pp_cxx_semicolon (pp);
}

/* compound-requirement:
     '{' expression '}' 'noexcept' [opt] trailing-return-type [opt] */
void
pp_cxx_compound_requirement (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_left_brace (pp);
  pp->expression (TREE_OPERAND (t, 0));
  pp_cxx_right_brace (pp);

  if (COMPOUND_REQ_NOEXCEPT_P (t))
    pp_cxx_ws_string (pp, "noexcept");

  if (tree type = TREE_OPERAND (t, 1))
    {
      pp_cxx_whitespace (pp);
      pp_cxx_ws_string (pp, "->");
      pp->type_id (type);
    }
  pp_cxx_semicolon (pp);
}

/* nested requirement:
     'requires' constraint-expression */
void
pp_cxx_nested_requirement (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_ws_string (pp, "requires");
  pp->expression (TREE_OPERAND (t, 0));
  pp_cxx_semicolon (pp);
}

/* Output the "[with ...]" clause for a parameter mapping of an atomic
   constraint.   */

void
pp_cxx_parameter_mapping (cxx_pretty_printer *pp, tree map)
{
  pp_cxx_whitespace (pp);
  pp_cxx_left_bracket (pp);
  pp->translate_string ("with");
  pp_cxx_whitespace (pp);

  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      tree parm = TREE_VALUE (p);
      tree arg = TREE_PURPOSE (p);

      if (TYPE_P (parm))
	pp->type_id (parm);
      else if (tree name = DECL_NAME (TEMPLATE_PARM_DECL (parm)))
	pp_cxx_tree_identifier (pp, name);
      else
	pp->translate_string ("<unnamed>");

      pp_cxx_whitespace (pp);
      pp_equal (pp);
      pp_cxx_whitespace (pp);

      if (TYPE_P (arg) || DECL_TEMPLATE_TEMPLATE_PARM_P (arg))
	pp->type_id (arg);
      else
	pp->expression (arg);

      if (TREE_CHAIN (p) != NULL_TREE)
	pp_cxx_separate_with (pp, ';');
    }

  pp_cxx_right_bracket (pp);
}

void
pp_cxx_atomic_constraint (cxx_pretty_printer *pp, tree t)
{
  /* Emit the expression.  */
  pp->expression (ATOMIC_CONSTR_EXPR (t));

  /* Emit the parameter mapping.  */
  tree map = ATOMIC_CONSTR_MAP (t);
  if (map && map != error_mark_node)
    pp_cxx_parameter_mapping (pp, map);
}

void
pp_cxx_conjunction (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_constraint (pp, TREE_OPERAND (t, 0));
  pp_string (pp, " /\\ ");
  pp_cxx_constraint (pp, TREE_OPERAND (t, 1));
}

void
pp_cxx_disjunction (cxx_pretty_printer *pp, tree t)
{
  pp_cxx_constraint (pp, TREE_OPERAND (t, 0));
  pp_string (pp, " \\/ ");
  pp_cxx_constraint (pp, TREE_OPERAND (t, 1));
}

void
pp_cxx_constraint (cxx_pretty_printer *pp, tree t)
{
  if (t == error_mark_node)
    return pp->expression (t);

  switch (TREE_CODE (t))
    {
    case ATOMIC_CONSTR:
      pp_cxx_atomic_constraint (pp, t);
      break;

    case CONJ_CONSTR:
      pp_cxx_conjunction (pp, t);
      break;

    case DISJ_CONSTR:
      pp_cxx_disjunction (pp, t);
      break;

    case EXPR_PACK_EXPANSION:
      pp->expression (TREE_OPERAND (t, 0));
      break;

    default:
      gcc_unreachable ();
    }
}


typedef c_pretty_print_fn pp_fun;

/* Initialization of a C++ pretty-printer object.  */

cxx_pretty_printer::cxx_pretty_printer ()
  : c_pretty_printer (),
    enclosing_scope (global_namespace)
{
  type_specifier_seq = (pp_fun) pp_cxx_type_specifier_seq;
  parameter_list = (pp_fun) pp_cxx_parameter_declaration_clause;
}

/* cxx_pretty_printer's implementation of pretty_printer::clone vfunc.  */

std::unique_ptr<pretty_printer>
cxx_pretty_printer::clone () const
{
  return ::make_unique<cxx_pretty_printer> (*this);
}
