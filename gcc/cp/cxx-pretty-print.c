/* Implementation of subroutines for the GNU C++ pretty-printer.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "real.h"
#include "cxx-pretty-print.h"
#include "cp-tree.h"

static void pp_cxx_unqualified_id (cxx_pretty_printer *, tree);
static void pp_cxx_qualified_id (cxx_pretty_printer *, tree);
static void pp_cxx_assignment_expression (cxx_pretty_printer *, tree);
static void pp_cxx_template_argument_list (cxx_pretty_printer *, tree);


static inline void
pp_cxx_nonconsecutive_character (cxx_pretty_printer *pp, int c)
{
  const char *p = pp_last_position_in_text (pp);

  if (p != NULL && *p == c)
    pp_space (pp);
  pp_character (pp, c);
}

#define pp_cxx_begin_template_argument_list(PP) \
  pp_cxx_nonconsecutive_character (PP, '<')
#define pp_cxx_end_template_argument_list(PP) \
  pp_cxx_nonconsecutive_character (PP, '>')

/* Declarations.  */

void
pp_cxx_declaration (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_declaration_specifiers (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}
/* type-specifier:
      simple-type-specifier
      class-specifier
      enum-specifier
      elaborated-type-specifier
      cv-qualifer   */

static void
pp_cxx_type_specifier (cxx_pretty_printer *pp, tree t)
{
  pp_c_type_qualifier_list (pp_c_base (pp), t);

  switch (TREE_CODE (t))
    {
    case TEMPLATE_DECL:
    case TYPE_DECL:
      if (pp->enclosing_scope != DECL_CONTEXT (t))
        pp_cxx_qualified_id (pp, t);
      else
        pp_cxx_unqualified_id (pp, t);
      break;

    case TYPEOF_TYPE:
      pp_c_identifier (pp_c_base (pp), "__typeof__");
      pp_left_paren (pp);
      pp_c_expression (pp_c_base (pp), TYPE_FIELDS (t));
      pp_right_paren (pp);
      break;

    default:
      pp_c_specifier_qualifier_list (pp_c_base (pp), t);
    }
}

static void
pp_cxx_declarator (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    default:
      pp_c_declarator (pp_c_base (pp), t);
      break;
    }
}

static void
pp_cxx_direct_declarator (cxx_pretty_printer *pp, tree t)
{
  pp_c_direct_declarator (pp_c_base (pp), t);
}

static void
pp_cxx_parameter_declaration (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}



/* type-id:
     type-specifier-seq abstract-declarator(opt) */
static void
pp_cxx_type_id (cxx_pretty_printer *pp, tree t)
{
  pp_flags saved_flags = pp_c_base (pp)->flags;
  pp_c_base (pp)->flags |= pp_c_flag_abstract;

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
    case TEMPLATE_ID_EXPR:
      /* FIXME: Should be pp_cxx_type_specifier_seq.  */
      pp_cxx_type_specifier (pp, t);
      pp_cxx_declarator (pp, t);
      break;

    default:
      pp_c_type_id (pp_c_base (pp), t);
      break;
    }

  pp_c_base (pp)->flags = saved_flags;
}

static void
pp_cxx_initializer (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

/* template-argument-list:
      template-argument
      template-argument-list, template-argument

   template-argument:
      assignment-expression
      type-id
      template-name   */
static void
pp_cxx_template_argument_list (cxx_pretty_printer *pp, tree t)
{
  int i;
  if (t == NULL)
    return;
  for (i = 0; i < TREE_VEC_LENGTH (t); ++i)
    {
      tree arg = TREE_VEC_ELT (t, i);
      if (i != 0)
        pp_separate_with (pp, ',');
      if (TYPE_P (arg) || (TREE_CODE (arg) == TEMPLATE_DECL
                           && TYPE_P (DECL_TEMPLATE_RESULT (arg))))
        pp_cxx_type_id (pp, arg);
      else
        pp_c_expression (pp_c_base (pp), arg);
    }
}


/* Statements.  */

void
pp_cxx_statement (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

/* Expressions. */

static inline bool
is_destructor_name (tree name)
{
  return name == complete_dtor_identifier
    || name == base_dtor_identifier
    || name == deleting_dtor_identifier;
}

/* unqualified-id:
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
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case TYPE_DECL:
    case FUNCTION_DECL:
    case NAMESPACE_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
    case USING_DECL:
      t = DECL_NAME (t);
      
    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t))
        {
          pp_c_identifier (pp_c_base (pp), "operator");
          /* FIXME: should be pp_cxx_type_specifier_seq */
          pp_cxx_type_specifier (pp, TREE_TYPE (t));
          /* FIXME:  should be pp_cxx_conversion_declarator. */
          pp_cxx_type_id (pp, TREE_TYPE (t));
        }
      else if (IDENTIFIER_OPNAME_P (t))
        {
          const char *name = operator_name_info[TREE_CODE (t)].name;
          pp_c_identifier (pp_c_base (pp), "operator");
          if (ISALPHA (name[0]))
            pp_space (pp);
          pp_base (pp)->padding = pp_none;
          pp_c_identifier (pp_c_base (pp), name);
        }
      else
        {
          if (is_destructor_name (t))
            {
              pp_complement (pp);
              t = constructor_name (TREE_TYPE (t));
            }
          pp_c_tree_identifier (pp_c_base (pp), t);
        }
      break;

    case TEMPLATE_ID_EXPR:
      pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 0));
      pp_less (pp);
      pp_cxx_template_argument_list (pp, TREE_OPERAND (t, 1));
      pp_greater (pp);
      break;

    default:
      pp_unsupported_tree (pp, t);
    }
}

/* qualified-id:
      nested-name-specifier template(opt) unqualified-id  */
static void
pp_cxx_qualified_id (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      pp_cxx_qualified_id (pp, PTRMEM_CST_CLASS (t));
      pp_colon_colon (pp);
      pp_cxx_unqualified_id (pp, PTRMEM_CST_MEMBER (t));
      break;

    case OFFSET_REF:
    case SCOPE_REF:
      pp_cxx_qualified_id (pp, TREE_OPERAND (t, 0));
      pp_colon_colon (pp);
      pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 1));
      break;

    default:
      {
        tree scope = DECL_CONTEXT (t);
        if (scope == pp->enclosing_scope)
          pp_cxx_unqualified_id (pp, t);
        else
          {
            pp_cxx_qualified_id (pp, scope);
            pp_colon_colon (pp);
            if (TYPE_P (scope) && dependent_type_p (scope)
                && TREE_CODE (t) == TEMPLATE_DECL)
              pp_c_identifier (pp_c_base (pp), "template");
            pp_cxx_unqualified_id (pp, t);
          }
      }
      break;
    }
}

/* id-expression:
      unaqualified-id
      qualified-id   */
static inline void
pp_cxx_id_expression (cxx_pretty_printer *pp, tree t)
{
  if (pp_c_base (pp)->flags
      & (pp_cxx_flag_qualified_id | pp_cxx_flag_global_scope))
    pp_cxx_qualified_id (pp, t);
  else
    pp_cxx_unqualified_id (pp, t);
}

/* primary-expression:
     literal
     this
     :: identifier
     :: operator-function-id
     :: qualifier-id
     ( expression )
     id-expression   */
static void
pp_cxx_primary_expression (cxx_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case STRING_CST:
    case INTEGER_CST:
    case REAL_CST:
      pp_c_constant (pp_c_base (pp), t);
      break;

    default:
      if (pp_c_base (pp)->flags & pp_cxx_flag_qualified_id)
        pp_cxx_qualified_id (pp, t);
      else
        {
          if (pp_c_base (pp)->flags & pp_cxx_flag_global_scope)
            pp_colon_colon (pp);
          pp_cxx_unqualified_id (pp, t);
        }
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
     typeif ( type-id )  */

static void
pp_cxx_postfix_expression (cxx_pretty_printer *pp, tree t)
{
  enum tree_code code = TREE_CODE (t);
  
  switch (code)
    {
    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
      if (code == DYNAMIC_CAST_EXPR)
        pp_identifier (pp, "dynamic_cast");
      else if (code == STATIC_CAST_EXPR)
        pp_identifier (pp, "static_cast");
      else if (code == REINTERPRET_CAST_EXPR)
        pp_identifier (pp, "reinterpret_cast");
      else
        pp_identifier (pp, "const_cast");
      pp_cxx_begin_template_argument_list (pp);
      pp_cxx_type_id (pp, TREE_TYPE (t));
      pp_cxx_end_template_argument_list (pp);
      pp_left_paren (pp);
      pp_c_expression (pp_c_base (pp), TREE_OPERAND (t, 0));
      pp_right_paren (pp);
      break;

    case EMPTY_CLASS_EXPR:
      pp_cxx_type_id (pp, TREE_TYPE (t));
      pp_left_paren (pp);
      pp_right_paren (pp);
      break;

    case TYPEID_EXPR:
      t = TREE_OPERAND (t, 0);
      pp_c_identifier (pp_c_base (pp), "typeid");
      pp_left_paren (pp);
      if (TYPE_P (t))
        pp_cxx_type_id (pp, t);
      else
        pp_c_expression (pp_c_base (pp), t);
      pp_right_paren (pp);
      break;

    case PSEUDO_DTOR_EXPR:
      pp_cxx_postfix_expression (pp, TREE_OPERAND (t, 0));
      pp_dot (pp);
      pp_cxx_qualified_id (pp, TREE_OPERAND (t, 1));
      pp_colon_colon (pp);
      pp_complement (pp);
      pp_cxx_unqualified_id (pp, TREE_OPERAND (t, 2));
      break;

    default:
      pp_c_postfix_expression (pp_c_base (pp), t);
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
  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      if (NEW_EXPR_USE_GLOBAL (t))
        pp_colon_colon (pp);
      pp_c_identifier (pp_c_base (pp), "new");
      if (TREE_OPERAND (t, 0))
        {
          pp_left_paren (pp);
          pp_c_expression_list (pp_c_base (pp), TREE_OPERAND (t, 0));
          pp_right_paren (pp);
          pp_space (pp);
        }
      /* FIXME: array-types are built with one more element.  */
      pp_cxx_type_id (pp, TREE_OPERAND (t, 1));
      if (TREE_OPERAND (t, 2))
        {
          pp_left_paren (pp);
          t = TREE_OPERAND (t, 2);
          if (TREE_CODE (t) == TREE_LIST)
            pp_c_expression_list (pp_c_base (pp), t);
          else if (t == void_zero_node)
            ;                   /* OK, empty initializer list.  */
          else
            pp_c_expression (pp_c_base (pp), t);
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
        pp_colon_colon (pp);
      pp_c_identifier (pp_c_base (pp), "delete");
      if (code == VEC_DELETE_EXPR)
        {
          pp_left_bracket (pp);
          pp_right_bracket (pp);
        }
      pp_c_cast_expression (pp_c_base (pp), TREE_OPERAND (t, 0));
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
      new-expression
      delete-expression

   unary-operator: one of
      *   &   +   -  !

   GNU extensions:
      __alignof__ unary-expression
      __alignof__ ( type-id )  */
static void
pp_cxx_unary_expression (cxx_pretty_printer *pp, tree t)
{
  enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      pp_cxx_new_expression (pp, t);
      break;

    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      pp_cxx_delete_expression (pp, t);
      break;
      
    default:
      pp_c_unary_expression (pp_c_base (pp), t);
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
      /* Handle unfortunate OFFESET_REF overloading here.  */
    case OFFSET_REF:
      if (TYPE_P (TREE_OPERAND (t, 0)))
        {
          pp_cxx_qualified_id (pp, t);
          break;
        }
      /* else fall through */
    case MEMBER_REF:
    case DOTSTAR_EXPR:
      pp_cxx_pm_expression (pp, TREE_OPERAND (t, 0));
      pp_dot (pp);
      pp_star(pp);
      pp_c_cast_expression (pp_c_base (pp), TREE_OPERAND (t, 1));
      break;


    default:
      pp_c_cast_expression (pp_c_base (pp), t);
      break;
    }
}

/* multiplicative-expression:
      pm-expression
      multiplicative-expression * pm-expression
      multiplicative-expression / pm-expression
      multiplicative-expression % pm-expression  */
static void
pp_cxx_multiplicative_expression (cxx_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      pp_cxx_multiplicative_expression (pp, TREE_OPERAND (e, 0));
      pp_space (pp);
      if (code == MULT_EXPR)
	pp_star (pp);
      else if (code == TRUNC_DIV_EXPR)
	pp_slash (pp);
      else
	pp_modulo (pp);
      pp_space (pp);
      pp_cxx_pm_expression (pp, TREE_OPERAND (e, 1));
      break;

    default:
      pp_cxx_pm_expression (pp, e);
      break;
    }
}


/* conditional-expression:
      logical-or-expression
      logical-or-expression ?  expression  : assignment-expression  */
static void
pp_cxx_conditional_expression (cxx_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == COND_EXPR)
    {
      pp_c_logical_or_expression (pp_c_base (pp), TREE_OPERAND (e, 0));
      pp_space (pp);
      pp_question (pp);
      pp_space (pp);
      pp_c_expression (pp_c_base (pp), TREE_OPERAND (e, 1));
      pp_space (pp);
      pp_cxx_assignment_expression (pp, TREE_OPERAND (e, 2));
    }
  else
    pp_c_logical_or_expression (pp_c_base (pp), e);
}

/* assignment-expression:
      conditional-expression
      logical-or-expression assignment-operator assignment-expression

   assignment-expression: one of
      =    *=    /=    %=    +=    -=    >>=    <<=    &=    ^=    |=  */
static void
pp_cxx_assignment_expression (cxx_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == MODIFY_EXPR)
    {
      pp_c_logical_or_expression (pp_c_base (pp), TREE_OPERAND (e, 0));
      pp_space (pp);
      pp_equal (pp);
      pp_space (pp);
      pp_cxx_assignment_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_cxx_conditional_expression (pp, e);
}



typedef c_pretty_print_fn pp_fun;

void
pp_cxx_pretty_printer_init (cxx_pretty_printer *pp)
{
  pp_c_pretty_printer_init (pp_c_base (pp));

  pp->c_base.declaration = (pp_fun) pp_cxx_declaration;
  pp->c_base.declaration_specifiers = (pp_fun) pp_cxx_declaration_specifiers;
  pp->c_base.type_specifier = (pp_fun) pp_cxx_type_specifier;
  pp->c_base.declarator = (pp_fun) pp_cxx_declarator;
  pp->c_base.direct_declarator = (pp_fun) pp_cxx_direct_declarator;
  pp->c_base.parameter_declaration = (pp_fun) pp_cxx_parameter_declaration;
  pp->c_base.type_id = (pp_fun) pp_cxx_type_id;
  pp->c_base.statement = (pp_fun) pp_cxx_statement;
  pp->c_base.id_expression = (pp_fun) pp_cxx_id_expression;
  pp->c_base.primary_expression = (pp_fun) pp_cxx_primary_expression;
  pp->c_base.postfix_expression = (pp_fun) pp_cxx_postfix_expression;
  pp->c_base.unary_expression = (pp_fun) pp_cxx_unary_expression;
  pp->c_base.initializer = (pp_fun) pp_cxx_initializer;
  pp->c_base.multiplicative_expression = (pp_fun) pp_cxx_multiplicative_expression;
  pp->c_base.conditional_expression = (pp_fun) pp_cxx_conditional_expression;
  pp->c_base.assignment_expression = (pp_fun) pp_cxx_assignment_expression;
  pp->enclosing_scope = NULL;
}
