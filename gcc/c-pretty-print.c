/* Subroutines common to both C and C++ pretty-printers.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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
#include "c-pretty-print.h"
#include "c-tree.h"

/* The pretty-printer code is primarily designed to closely follow
   (GNU) C and C++ grammars.  That is to be contrasted with spaghetti
   codes we used to have in the past.  Following a structured
   approach (preferably the official grammars) is believed to make it
   much easier to add extensions and nifty pretty-printing effects that
   takes expression or declaration contexts into account.  */


#define pp_c_maybe_whitespace(PP)            \
   do {                                      \
     if (pp_base (PP)->padding == pp_before) \
       pp_c_whitespace (PP);                 \
   } while (0)

#define pp_c_left_bracket(PP)         \
  do {                                \
    pp_left_bracket (PP);             \
    pp_base (PP)->padding = pp_none;  \
  } while (0)

#define pp_c_right_bracket(PP)        \
  do {                                \
    pp_right_bracket (PP);            \
    pp_base (PP)->padding = pp_none;  \
  } while (0)

#define pp_c_star(PP)                 \
  do {                                \
    pp_star (PP);                     \
    pp_base (PP)->padding = pp_none;  \
  } while (0)

/* literal  */
static void pp_c_char (c_pretty_printer *, int);

/* postfix-expression  */
static void pp_c_initializer_list (c_pretty_printer *, tree);
static void pp_c_brace_enclosed_initializer_list (c_pretty_printer *, tree);

static void pp_c_multiplicative_expression (c_pretty_printer *, tree);
static void pp_c_additive_expression (c_pretty_printer *, tree);
static void pp_c_shift_expression (c_pretty_printer *, tree);
static void pp_c_relational_expression (c_pretty_printer *, tree);
static void pp_c_equality_expression (c_pretty_printer *, tree);
static void pp_c_and_expression (c_pretty_printer *, tree);
static void pp_c_exclusive_or_expression (c_pretty_printer *, tree);
static void pp_c_inclusive_or_expression (c_pretty_printer *, tree);
static void pp_c_logical_and_expression (c_pretty_printer *, tree);
static void pp_c_conditional_expression (c_pretty_printer *, tree);
static void pp_c_assignment_expression (c_pretty_printer *, tree);

/* declarations.  */


/* Helper functions.  */

void
pp_c_whitespace (c_pretty_printer *pp)
{
  pp_space (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_left_paren (c_pretty_printer *pp)
{
  pp_left_paren (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_right_paren (c_pretty_printer *pp)
{
  pp_right_paren (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_left_brace (c_pretty_printer *pp)
{
  pp_left_brace (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_right_brace (c_pretty_printer *pp)
{
  pp_right_brace (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_dot (c_pretty_printer *pp)
{
  pp_dot (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_ampersand (c_pretty_printer *pp)
{
  pp_ampersand (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_arrow (c_pretty_printer *pp)
{
  pp_arrow (pp);
  pp_base (pp)->padding = pp_none;
}

void
pp_c_semicolon(c_pretty_printer *pp)
{
  pp_semicolon (pp);
  pp_base (pp)->padding = pp_none;
}

static void
pp_c_cv_qualifier (c_pretty_printer *pp, const char *cv)
{
  const char *p = pp_last_position_in_text (pp);
  if (p != NULL && *p == '*')
    pp_c_whitespace (pp);
  pp_c_identifier (pp, cv);
}

/* Pretty-print T using the type-cast notation '( type-name )'.  */

static void
pp_c_type_cast (c_pretty_printer *pp, tree t)
{
  pp_c_left_paren (pp);
  pp_type_id (pp, t);
  pp_c_right_paren (pp);
}

void
pp_c_space_for_pointer_operator (c_pretty_printer *pp, tree t)
{
  if (POINTER_TYPE_P (t))
    {
      tree pointee = strip_pointer_operator (TREE_TYPE (t));
      if (TREE_CODE (pointee) != ARRAY_TYPE
          && TREE_CODE (pointee) != FUNCTION_TYPE)
        pp_c_whitespace (pp);
    }
}


/* Declarations.  */

/* C++ cv-qualifiers are called type-qualifiers in C.  Print out the
   cv-qualifiers of T.  If T is a declaration then it is the cv-qualifier
   of its type.  Take care of possible extensions.

   type-qualifier-list:
       type-qualifier
       type-qualifier-list type-qualifier

   type-qualifier:
       const
       restrict                              -- C99
       __restrict__                          -- GNU C
       volatile    */

void
pp_c_type_qualifier_list (c_pretty_printer *pp, tree t)
{
   int qualifiers;
   
  if (!TYPE_P (t))
    t = TREE_TYPE (t);

  qualifiers = TYPE_QUALS (t);
  if (qualifiers & TYPE_QUAL_CONST)
    pp_c_cv_qualifier (pp, "const");
  if (qualifiers & TYPE_QUAL_VOLATILE)
    pp_c_cv_qualifier (pp, "volatile");
  if (qualifiers & TYPE_QUAL_RESTRICT)
    pp_c_cv_qualifier (pp, flag_isoc99 ? "restrict" : "__restrict__");
}

/* pointer:
      * type-qualifier-list(opt)
      * type-qualifier-list(opt) pointer  */

static void
pp_c_pointer (c_pretty_printer *pp, tree t)
{
  if (!TYPE_P (t) && TREE_CODE (t) != TYPE_DECL)
    t = TREE_TYPE (t);
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      /* It is easier to handle C++ reference types here.  */
    case REFERENCE_TYPE:
      if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
        pp_c_pointer (pp, TREE_TYPE (t));
      if (TREE_CODE (t) == POINTER_TYPE)
        pp_c_star (pp);
      else
        pp_c_ampersand (pp);
      pp_c_type_qualifier_list (pp, t);
      break;

    default:
      pp_unsupported_tree (pp, t);
    }
}

/* type-specifier:
      void
      char
      short
      int
      long
      float
      double
      signed
      unsigned
      _Bool                          -- C99
      _Complex                       -- C99
      _Imaginary                     -- C99
      struct-or-union-specifier
      enum-specifier
      typedef-name.

  GNU extensions.
  simple-type-specifier:
      __complex__
      __vector__   */

void
pp_c_type_specifier (c_pretty_printer *pp, tree t)
{
  const enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case ERROR_MARK:
      pp_c_identifier (pp, "<type-error>");
      break;

    case IDENTIFIER_NODE:
      pp_c_tree_identifier (pp, t);
      break;

    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
      if (TYPE_NAME (t))
        t = TYPE_NAME (t);
      else
        t = c_common_type_for_mode (TYPE_MODE (t), TREE_UNSIGNED (t));
      pp_c_type_specifier (pp, t);
      break;

    case TYPE_DECL:
      if (DECL_NAME (t))
	pp_id_expression (pp, t);
      else
	pp_c_identifier (pp, "<typedef-error>");
      break;

    case UNION_TYPE:
    case RECORD_TYPE:
    case ENUMERAL_TYPE:
      if (code == UNION_TYPE)
	pp_c_identifier (pp, "union");
      else if (code == RECORD_TYPE)
	pp_c_identifier (pp, "struct");
      else if (code == ENUMERAL_TYPE)
	pp_c_identifier (pp, "enum");
      else
	pp_c_identifier (pp, "<tag-error>");

      if (TYPE_NAME (t))
	pp_id_expression (pp, TYPE_NAME (t));
      else
	pp_c_identifier (pp, "<anonymous>");
      break;

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* specifier-qualifier-list:
      type-specifier specifier-qualifier-list-opt
      type-qualifier specifier-qualifier-list-opt


  Implementation note:  Because of the non-linearities in array or
  function declarations, this routine prints not just the
  specifier-qualifier-list of such entities or types of such entities,
  but also the 'pointer' production part of their declarators.  The
  remaining part is done by pp_declarator or pp_c_abstract_declarator.  */

void
pp_c_specifier_qualifier_list (c_pretty_printer *pp, tree t)
{
  const enum tree_code code = TREE_CODE (t);

  if (TREE_CODE (t) != POINTER_TYPE)
    pp_c_type_qualifier_list (pp, t);
  switch (code)
    {
    case REFERENCE_TYPE:
    case POINTER_TYPE:
      {
        /* Get the types-specifier of this type.  */
        tree pointee = strip_pointer_operator (TREE_TYPE (t));
        pp_c_specifier_qualifier_list (pp, pointee);
        if (TREE_CODE (pointee) == ARRAY_TYPE
            || TREE_CODE (pointee) == FUNCTION_TYPE)
          {
            pp_c_whitespace (pp);
            pp_c_left_paren (pp);
          }
        pp_ptr_operator (pp, t);
      }
      break;

    case FUNCTION_TYPE:
    case ARRAY_TYPE:
      pp_c_specifier_qualifier_list (pp, TREE_TYPE (t));
      break;

    case VECTOR_TYPE:
    case COMPLEX_TYPE:
      pp_c_specifier_qualifier_list (pp, TREE_TYPE (t));
      if (code == COMPLEX_TYPE)
        pp_c_identifier (pp, flag_isoc99 ? "_Complex" : "__complex__");
      else if (code == VECTOR_TYPE)
        pp_c_identifier (pp, "__vector__");
      break;

    default:
      pp_simple_type_specifier (pp, t);
      break;
    }
}

/* parameter-type-list:
      parameter-list
      parameter-list , ...

   parameter-list:
      parameter-declaration
      parameter-list , parameter-declaration

   parameter-declaration:
      declaration-specifiers declarator
      declaration-specifiers abstract-declarator(opt)   */

void
pp_c_parameter_type_list (c_pretty_printer *pp, tree t)
{
  bool want_parm_decl = DECL_P (t) && !(pp->flags & pp_c_flag_abstract);
  tree parms = want_parm_decl ? DECL_ARGUMENTS (t) :  TYPE_ARG_TYPES (t);
  pp_c_left_paren (pp);
  if (parms == void_list_node)
    pp_c_identifier (pp, "void");
  else
    {
      bool first = true;
      for ( ; parms && parms != void_list_node; parms = TREE_CHAIN (parms))
        {
          if (!first)
            pp_separate_with (pp, ',');
          first = false;
          pp_declaration_specifiers
            (pp, want_parm_decl ? parms : TREE_VALUE (parms));
          if (want_parm_decl)
            pp_declarator (pp, parms);
          else
            pp_abstract_declarator (pp, TREE_VALUE (parms));
        }
    }
  pp_c_right_paren (pp);
}

/* abstract-declarator:
      pointer
      pointer(opt) direct-abstract-declarator  */

static void
pp_c_abstract_declarator (c_pretty_printer *pp, tree t)
{
  if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE
          || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
        pp_c_right_paren (pp);
      t = TREE_TYPE (t);
    }

  pp_direct_abstract_declarator (pp, t);
}

/* direct-abstract-declarator:
      ( abstract-declarator )
      direct-abstract-declarator(opt) [ assignment-expression(opt) ]
      direct-abstract-declarator(opt) [ * ]
      direct-abstract-declarator(opt) ( parameter-type-list(opt) )  */

void
pp_c_direct_abstract_declarator (c_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      pp_abstract_declarator (pp, t);
      break;
      
    case FUNCTION_TYPE:
      pp_c_parameter_type_list (pp, t);
      pp_direct_abstract_declarator (pp, TREE_TYPE (t));
      break;

    case ARRAY_TYPE:
      pp_c_left_bracket (pp);
      if (TYPE_DOMAIN (t))
        pp_expression (pp, TYPE_MAX_VALUE (TYPE_DOMAIN (t)));
      pp_c_right_bracket (pp);
      pp_direct_abstract_declarator (pp, TREE_TYPE (t));
      break;

    case IDENTIFIER_NODE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case VECTOR_TYPE:
    case COMPLEX_TYPE:
    case TYPE_DECL:
      break;
      
    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* type-name:
      specifier-qualifier-list  abstract-declarator(opt)  */

void
pp_c_type_id (c_pretty_printer *pp, tree t)
{
  pp_c_specifier_qualifier_list (pp, t);
  pp_abstract_declarator (pp, t);
}

/* storage-class-specifier:
      typedef
      extern
      static
      auto
      register  */

void
pp_c_storage_class_specifier (c_pretty_printer *pp, tree t)
{
  if (TREE_CODE (t) == TYPE_DECL)
    pp_c_identifier (pp, "typedef");
  else if (DECL_P (t))
    {
      if (DECL_REGISTER (t))
        pp_c_identifier (pp, "register");
      else if (TREE_STATIC (t) && TREE_CODE (t) == VAR_DECL)
        pp_c_identifier (pp, "static");
    }
}

/* function-specifier:
      inline   */

void
pp_c_function_specifier (c_pretty_printer *pp, tree t)
{
  if (TREE_CODE (t) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (t))
    pp_c_identifier (pp, "inline");
}

/* declaration-specifiers:
      storage-class-specifier declaration-specifiers(opt)
      type-specifier declaration-specifiers(opt)
      type-qualifier declaration-specifiers(opt)
      function-specifier declaration-specifiers(opt)  */

void
pp_c_declaration_specifiers (c_pretty_printer *pp, tree t)
{
  pp_storage_class_specifier (pp, t);
  pp_function_specifier (pp, t);
  pp_c_specifier_qualifier_list (pp, DECL_P (t) ?  TREE_TYPE (t) : t);
}

/* direct-declarator
      identifier
      ( declarator )
      direct-declarator [ type-qualifier-list(opt) assignment-expression(opt) ]
      direct-declarator [ static type-qualifier-list(opt) assignment-expression(opt)]
      direct-declarator [ type-qualifier-list static assignment-expression ]
      direct-declarator [ type-qualifier-list * ]
      direct-declaratpr ( parameter-type-list )
      direct-declarator ( identifier-list(opt) )  */

void
pp_c_direct_declarator (c_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case TYPE_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
      if (DECL_NAME (t))
        {
          pp_c_space_for_pointer_operator (pp, TREE_TYPE (t));
          pp_c_tree_identifier (pp, DECL_NAME (t));
        }
    case ARRAY_TYPE:
    case POINTER_TYPE:
      pp_abstract_declarator (pp, TREE_TYPE (t));
      break;

    case FUNCTION_TYPE:
      pp_parameter_list (pp, t);
      pp_abstract_declarator (pp, TREE_TYPE (t));
      break;

    case FUNCTION_DECL:
      pp_c_space_for_pointer_operator (pp, TREE_TYPE (TREE_TYPE (t)));
      pp_c_tree_identifier (pp, DECL_NAME (t));
      if (pp_c_base (pp)->flags & pp_c_flag_abstract)
        pp_abstract_declarator (pp, TREE_TYPE (t));
      else
        {
          pp_parameter_list (pp, t);
          pp_abstract_declarator (pp, TREE_TYPE (TREE_TYPE (t)));
        }
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case UNION_TYPE:
    case RECORD_TYPE:
      break;

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}


/* declarator:
      pointer(opt)  direct-declarator   */

void
pp_c_declarator (c_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
    case ENUMERAL_TYPE:
    case UNION_TYPE:
    case RECORD_TYPE:
      break;

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case ARRAY_TYPE:
    case FUNCTION_TYPE:
    case FUNCTION_DECL:
    case TYPE_DECL:
      pp_direct_declarator (pp, t);
    break;

    
    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* declaration:
      declaration-specifiers init-declarator-list(opt) ;  */

void
pp_c_declaration (c_pretty_printer *pp, tree t)
{
  pp_declaration_specifiers (pp, t);
  pp_c_init_declarator (pp, t);
}

/* Pretty-print ATTRIBUTES using GNU C extension syntax.  */

void
pp_c_attributes (c_pretty_printer *pp, tree attributes)
{
  if (attributes == NULL_TREE)
    return;

  pp_c_identifier (pp, "__attribute__");
  pp_c_left_paren (pp);
  pp_c_left_paren (pp);
  for (; attributes != NULL_TREE; attributes = TREE_CHAIN (attributes))
    {
      pp_tree_identifier (pp, TREE_PURPOSE (attributes));
      if (TREE_VALUE (attributes))
        pp_c_call_argument_list (pp, TREE_VALUE (attributes));

      if (TREE_CHAIN (attributes))
	pp_separate_with (pp, ',');
    }
  pp_c_right_paren (pp);
  pp_c_right_paren (pp);
}

/* function-definition:
      declaration-specifiers declarator compound-statement  */

void
pp_c_function_definition (c_pretty_printer *pp, tree t)
{
  pp_declaration_specifiers (pp, t);
  pp_declarator (pp, t);
  pp_needs_newline (pp) = true;
  pp_statement (pp, DECL_SAVED_TREE (t));
  pp_newline (pp);
  pp_flush (pp);
}


/* Expressions.  */

/* Print out a c-char.  */

static void
pp_c_char (c_pretty_printer *pp, int c)
{
  switch (c)
    {
    case TARGET_NEWLINE:
      pp_string (pp, "\\n");
      break;
    case TARGET_TAB:
      pp_string (pp, "\\t");
      break;
    case TARGET_VT:
      pp_string (pp, "\\v");
      break;
    case TARGET_BS:
      pp_string (pp, "\\b");
      break;
    case TARGET_CR:
      pp_string (pp, "\\r");
      break;
    case TARGET_FF:
      pp_string (pp, "\\f");
      break;
    case TARGET_BELL:
      pp_string (pp, "\\a");
      break;
    case '\\':
      pp_string (pp, "\\\\");
      break;
    case '\'':
      pp_string (pp, "\\'");
      break;
    case '\"':
      pp_string (pp, "\\\"");
      break;
    default:
      if (ISPRINT (c))
	pp_character (pp, c);
      else
	pp_scalar (pp, "\\%03o", (unsigned) c);
      break;
    }
}

/* Print out a STRING literal.  */

void
pp_c_string_literal (c_pretty_printer *pp, tree s)
{
  const char *p = TREE_STRING_POINTER (s);
  int n = TREE_STRING_LENGTH (s) - 1;
  int i;
  pp_doublequote (pp);
  for (i = 0; i < n; ++i)
    pp_c_char (pp, p[i]);
  pp_doublequote (pp);
}

static void
pp_c_integer_constant (c_pretty_printer *pp, tree i)
{
  tree type = TREE_TYPE (i);

  if (TREE_INT_CST_HIGH (i) == 0)
    pp_wide_integer (pp, TREE_INT_CST_LOW (i));
  else
    {
      if (tree_int_cst_sgn (i) < 0)
        {
          pp_c_char (pp, '-');
          i = build_int_2 (-TREE_INT_CST_LOW (i),
                           ~TREE_INT_CST_HIGH (i) + !TREE_INT_CST_LOW (i));
        }
      sprintf (pp_buffer (pp)->digit_buffer,
               HOST_WIDE_INT_PRINT_DOUBLE_HEX,
               TREE_INT_CST_HIGH (i), TREE_INT_CST_LOW (i));
      pp_string (pp, pp_buffer (pp)->digit_buffer);
    }
  if (TREE_UNSIGNED (type))
    pp_character (pp, 'u');
  if (type == long_integer_type_node || type == long_unsigned_type_node)
    pp_character (pp, 'l');
  else if (type == long_long_integer_type_node
           || type == long_long_unsigned_type_node)
    pp_string (pp, "ll");
}

/* Print out a CHARACTER literal.  */

static void
pp_c_character_constant (c_pretty_printer *pp, tree c)
{
  tree type = TREE_TYPE (c);
  if (type == wchar_type_node)
    pp_character (pp, 'L'); 
  pp_quote (pp);
  if (host_integerp (c, TREE_UNSIGNED (type)))
    pp_c_char (pp, tree_low_cst (c, TREE_UNSIGNED (type)));
  else
    pp_scalar (pp, "\\x%x", (unsigned) TREE_INT_CST_LOW (c));
  pp_quote (pp);
}

/* Print out a BOOLEAN literal.  */

static void
pp_c_bool_constant (c_pretty_printer *pp, tree b)
{
  if (b == boolean_false_node)
    {
      if (c_dialect_cxx ())
	pp_c_identifier (pp, "false");
      else if (flag_isoc99)
	pp_c_identifier (pp, "_False");
      else
	pp_unsupported_tree (pp, b);
    }
  else if (b == boolean_true_node)
    {
      if (c_dialect_cxx ())
	pp_c_identifier (pp, "true");
      else if (flag_isoc99)
	pp_c_identifier (pp, "_True");
      else
	pp_unsupported_tree (pp, b);
    }
  else if (TREE_CODE (b) == INTEGER_CST)
    pp_c_integer_constant (pp, b);
  else
    pp_unsupported_tree (pp, b);
}

/* Attempt to print out an ENUMERATOR.  Return true on success.  Else return
   false; that means the value was obtained by a cast, in which case
   print out the type-id part of the cast-expression -- the casted value
   is then printed by pp_c_integer_literal.  */

static bool
pp_c_enumeration_constant (c_pretty_printer *pp, tree e)
{
  bool value_is_named = true;
  tree type = TREE_TYPE (e);
  tree value;

  /* Find the name of this constant.  */
  for (value = TYPE_VALUES (type);
       value != NULL_TREE && !tree_int_cst_equal (TREE_VALUE (value), e);
       value = TREE_CHAIN (value))
    ;

  if (value != NULL_TREE)
    pp_id_expression (pp, TREE_PURPOSE (value));
  else
    {
      /* Value must have been cast.  */
      pp_c_type_cast (pp, type);
      value_is_named = false;
    }

  return value_is_named;
}

/* Print out a REAL value as a decimal-floating-constant.  */

static void
pp_c_floating_constant (c_pretty_printer *pp, tree r)
{
  real_to_decimal (pp_buffer (pp)->digit_buffer, &TREE_REAL_CST (r),
		   sizeof (pp_buffer (pp)->digit_buffer), 0, 1);
  pp_string (pp, pp_buffer(pp)->digit_buffer);
  if (TREE_TYPE (r) == float_type_node)
    pp_character (pp, 'f');
  else if (TREE_TYPE (r) == long_double_type_node)
    pp_character (pp, 'l');
}

/* Pretty-print a compound literal expression.  GNU extensions include
   vector constants.  */ 

static void
pp_c_compound_literal (c_pretty_printer *pp, tree e)
{
  tree type = TREE_TYPE (e);  
  pp_c_type_cast (pp, type);

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ARRAY_TYPE:
    case VECTOR_TYPE:
    case COMPLEX_TYPE:
      pp_c_brace_enclosed_initializer_list (pp, e);
      break;

    default:
      pp_unsupported_tree (pp, e);
      break;
    }
}

/* constant:
      integer-constant
      floating-constant
      enumeration-constant
      character-constant   */

void
pp_c_constant (c_pretty_printer *pp, tree e)
{
  const enum tree_code code = TREE_CODE (e);

  switch (code)
    {
    case INTEGER_CST:
      {
        tree type = TREE_TYPE (e);
        if (type == boolean_type_node)
          pp_c_bool_constant (pp, e);
        else if (type == char_type_node)
          pp_c_character_constant (pp, e);
        else if (TREE_CODE (type) == ENUMERAL_TYPE
                 && pp_c_enumeration_constant (pp, e))
          ; 
        else 
          pp_c_integer_constant (pp, e);
      }
      break;

    case REAL_CST:
      pp_c_floating_constant (pp, e);
      break;

    case STRING_CST:
      pp_c_string_literal (pp, e);
      break;

    default:
      pp_unsupported_tree (pp, e);
      break;
    }
}

void
pp_c_identifier (c_pretty_printer *pp, const char *id)
{
  pp_c_maybe_whitespace (pp);            
  pp_identifier (pp, id);  
  pp_base (pp)->padding = pp_before;
}

/* Pretty-print a C primary-expression.
   primary-expression:
      identifier
      constant
      string-literal
      ( expression )   */

void
pp_c_primary_expression (c_pretty_printer *pp, tree e)
{
  switch (TREE_CODE (e))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case LABEL_DECL:
      e = DECL_NAME (e);
      /* Fall through.  */
    case IDENTIFIER_NODE:
      pp_c_tree_identifier (pp, e);
      break;

    case ERROR_MARK:
      pp_c_identifier (pp, "<erroneous-expression>");
      break;

    case RESULT_DECL:
      pp_c_identifier (pp, "<return-value>");
      break;

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      pp_c_constant (pp, e);
      break;

    case STMT_EXPR:
      pp_c_left_paren (pp);
      pp_statement (pp, STMT_EXPR_STMT (e));
      pp_c_right_paren (pp);
      break;

    default:
      /* FIXME:  Make sure we won't get into an infinie loop.  */
      pp_c_left_paren (pp);
      pp_expression (pp, e);
      pp_c_right_paren (pp);
      break;
    }
}

/* Print out a C initializer -- also support C compound-literals.
   initializer:
      assignment-expression:
      { initializer-list }
      { initializer-list , }   */

static void
pp_c_initializer (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == CONSTRUCTOR)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (e));
      if (code == RECORD_TYPE || code == UNION_TYPE || code == ARRAY_TYPE)
        pp_c_brace_enclosed_initializer_list (pp, e);
      else
	pp_unsupported_tree (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_expression (pp, e);
}

/* init-declarator:
      declarator:
      declarator = initializer   */

void
pp_c_init_declarator (c_pretty_printer *pp, tree t)
{
  pp_declarator (pp, t);
  if (DECL_INITIAL (t))
    {
      tree init = DECL_INITIAL (t);
      /* This C++ bit is handled here because it is easier to do so.
         In templates, the C++ parser builds a TREE_LIST for a
         direct-initialization; the TREE_PURPOSE is the variable to
         initialize and the TREE_VALUE is the initializer.  */
      if (TREE_CODE (init) == TREE_LIST)
        {
          pp_c_left_paren (pp);
          pp_expression (pp, TREE_VALUE (init));
          pp_right_paren (pp);
        }
      else
        {
          pp_space (pp);
          pp_equal (pp);
          pp_space (pp);
          pp_c_initializer (pp, init);
        }
    }
}

/* initializer-list:
      designation(opt) initializer
      initializer-list , designation(opt) initializer

   designation:
      designator-list =

   designator-list:
      designator
      designator-list designator

   designator:
      [ constant-expression ]
      identifier   */

static void
pp_c_initializer_list (c_pretty_printer *pp, tree e)
{
  tree type = TREE_TYPE (e);
  const enum tree_code code = TREE_CODE (type);

  switch (code)
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ARRAY_TYPE:
      {
        tree init = TREE_OPERAND (e, 0);
        for (; init != NULL_TREE; init = TREE_CHAIN (init))
          {
            if (code == RECORD_TYPE || code == UNION_TYPE)
              {
                pp_c_dot (pp);
                pp_c_primary_expression (pp, TREE_PURPOSE (init));
              }
            else
              {
                pp_c_left_bracket (pp);
                if (TREE_PURPOSE (init))
                  pp_c_constant (pp, TREE_PURPOSE (init));
                pp_c_right_bracket (pp);
              }
            pp_c_whitespace (pp);
            pp_equal (pp);
            pp_c_whitespace (pp);
            pp_initializer (pp, TREE_VALUE (init));
            if (TREE_CHAIN (init))
              pp_separate_with (pp, ',');
          }
      }
      break;

    case VECTOR_TYPE:
      pp_c_expression_list (pp, TREE_VECTOR_CST_ELTS (e));
      break;

    case COMPLEX_TYPE:
      {
        const bool cst = TREE_CODE (e) == COMPLEX_CST;
        pp_expression (pp, cst ? TREE_REALPART (e) : TREE_OPERAND (e, 0));
        pp_separate_with (pp, ',');
        pp_expression (pp, cst ? TREE_IMAGPART (e) : TREE_OPERAND (e, 1));
      }
      break;

    default:
      pp_unsupported_tree (pp, type);
      break;
    }
}

/* Pretty-print a brace-enclosed initializer-list.  */

static void
pp_c_brace_enclosed_initializer_list (c_pretty_printer *pp, tree l)
{
  pp_c_left_brace (pp);
  pp_c_initializer_list (pp, l);
  pp_c_right_brace (pp);
}


/*  This is a convenient function, used to bridge gap between C and C++
    grammars.

    id-expression:
       identifier  */

void
pp_c_id_expression (c_pretty_printer *pp, tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case TYPE_DECL:
    case FUNCTION_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
      t = DECL_NAME (t);
    case IDENTIFIER_NODE:
      pp_c_tree_identifier (pp, t);
      break;

    default:
      pp_unsupported_tree (pp, t);
      break;
    }
}

/* postfix-expression:
      primary-expression
      postfix-expression [ expression ]
      postfix-expression ( argument-expression-list(opt) )
      postfix-expression . identifier
      postfix-expression -> identifier
      postfix-expression ++
      postfix-expression --
      ( type-name ) { initializer-list }
      ( type-name ) { initializer-list , }  */

void
pp_c_postfix_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      pp_postfix_expression (pp, TREE_OPERAND (e, 0));
      pp_identifier (pp, code == POSTINCREMENT_EXPR ? "++" : "--");
      break;

    case ARROW_EXPR:
      pp_postfix_expression (pp, TREE_OPERAND (e, 0));
      pp_c_arrow (pp);
      break;

    case ARRAY_REF:
      pp_postfix_expression (pp, TREE_OPERAND (e, 0));
      pp_c_left_bracket (pp);
      pp_expression (pp, TREE_OPERAND (e, 1));
      pp_c_right_bracket (pp);
      break;

    case CALL_EXPR:
      pp_postfix_expression (pp, TREE_OPERAND (e, 0));
      pp_c_call_argument_list (pp, TREE_OPERAND (e, 1));
      break;

    case ABS_EXPR:
      pp_c_identifier (pp, "__builtin_abs");
      pp_c_left_paren (pp);
      pp_expression (pp, TREE_OPERAND (e, 0));
      pp_c_right_paren (pp);
      break;

    case COMPONENT_REF:
      {
	tree object = TREE_OPERAND (e, 0);
	if (TREE_CODE (object) == INDIRECT_REF)
	  {
	    pp_postfix_expression (pp, TREE_OPERAND (object, 0));
	    pp_c_arrow (pp);
	  }
	else
	  {
	    pp_postfix_expression (pp, object);
	    pp_c_dot (pp);
	  }
	pp_expression (pp, TREE_OPERAND (e, 1));
      }
      break;

    case COMPLEX_CST:
    case VECTOR_CST:
    case COMPLEX_EXPR:
      pp_c_compound_literal (pp, e);
      break;

    case COMPOUND_LITERAL_EXPR:
      e = DECL_INITIAL (COMPOUND_LITERAL_EXPR_DECL (e));
      /* Fall through.  */
    case CONSTRUCTOR:
      pp_initializer (pp, e);
      break;

    case VA_ARG_EXPR:
      pp_c_identifier (pp, "__builtin_va_arg");
      pp_c_left_paren (pp);
      pp_assignment_expression (pp, TREE_OPERAND (e, 0));
      pp_separate_with (pp, ',');
      pp_type_id (pp, TREE_TYPE (e));
      pp_c_right_paren (pp);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (e, 0)) == FUNCTION_DECL)
        {
          pp_c_id_expression (pp, TREE_OPERAND (e, 0));
          break;
        }
      /* else fall through.  */

    default:
      pp_primary_expression (pp, e);
      break;
    }
}

/* Print out an expression-list; E is expected to be a TREE_LIST.  */

void
pp_c_expression_list (c_pretty_printer *pp, tree e)
{
  for (; e != NULL_TREE; e = TREE_CHAIN (e))
    {
      pp_expression (pp, TREE_VALUE (e));
      if (TREE_CHAIN (e))
	pp_separate_with (pp, ',');
    }
}

/* Print out an expression-list in parens, as in a function call.  */

void
pp_c_call_argument_list (c_pretty_printer *pp, tree t)
{
  pp_c_left_paren (pp);
  if (t && TREE_CODE (t) == TREE_LIST)
    pp_c_expression_list (pp, t);
  pp_c_right_paren (pp);
}

/* unary-expression:
      postfix-expression
      ++ cast-expression
      -- cast-expression
      unary-operator cast-expression
      sizeof unary-expression
      sizeof ( type-id )

  unary-operator: one of
      * &  + - ! ~
      
   GNU extensions.
   unary-expression:
      __alignof__ unary-expression
      __alignof__ ( type-id )
      __real__ unary-expression
      __imag__ unary-expression  */

void
pp_c_unary_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      pp_identifier (pp, code == PREINCREMENT_EXPR ? "++" : "--");
      pp_c_unary_expression (pp, TREE_OPERAND (e, 0));
      break;

    case ADDR_EXPR:
    case INDIRECT_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case CONJ_EXPR:
      /* String literal are used by address.  */
      if (code == ADDR_EXPR && TREE_CODE (TREE_OPERAND (e, 0)) != STRING_CST)
	pp_ampersand (pp);
      else if (code == INDIRECT_REF)
	pp_c_star (pp);
      else if (code == NEGATE_EXPR)
	pp_minus (pp);
      else if (code == BIT_NOT_EXPR || code == CONJ_EXPR)
	pp_complement (pp);
      else if (code == TRUTH_NOT_EXPR)
	pp_exclamation (pp);
      pp_c_cast_expression (pp, TREE_OPERAND (e, 0));
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      pp_c_identifier (pp, code == SIZEOF_EXPR ? "sizeof" : "__alignof__");
      pp_c_whitespace (pp);
      if (TYPE_P (TREE_OPERAND (e, 0)))
        pp_c_type_cast (pp, TREE_OPERAND (e, 0));
      else
	pp_unary_expression (pp, TREE_OPERAND (e, 0));
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      pp_c_identifier (pp, code == REALPART_EXPR ? "__real__" : "__imag__");
      pp_c_whitespace (pp);
      pp_unary_expression (pp, TREE_OPERAND (e, 0));
      break;

    default:
      pp_postfix_expression (pp, e);
      break;
    }
}

/* cast-expression:
      unary-expression
      ( type-name ) cast-expression  */

void
pp_c_cast_expression (c_pretty_printer *pp, tree e)
{
  switch (TREE_CODE (e))
    {
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case CONVERT_EXPR:
      pp_c_type_cast (pp, TREE_TYPE (e));
      pp_c_cast_expression (pp, TREE_OPERAND (e, 0));
      break;

    default:
      pp_unary_expression (pp, e);
    }
}

/* multiplicative-expression:
      cast-expression
      multiplicative-expression * cast-expression
      multiplicative-expression / cast-expression
      multiplicative-expression % cast-expression   */

static void
pp_c_multiplicative_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      pp_multiplicative_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      if (code == MULT_EXPR)
	pp_c_star (pp);
      else if (code == TRUNC_DIV_EXPR)
	pp_slash (pp);
      else
	pp_modulo (pp);
      pp_c_whitespace (pp);
      pp_c_cast_expression (pp, TREE_OPERAND (e, 1));
      break;

    default:
      pp_c_cast_expression (pp, e);
      break;
    }
}

/* additive-expression:
      multiplicative-expression
      additive-expression + multiplicative-expression
      additive-expression - multiplicative-expression   */

static void
pp_c_additive_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      pp_c_additive_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      if (code == PLUS_EXPR)
	pp_plus (pp);
      else
	pp_minus (pp);
      pp_c_whitespace (pp);
      pp_multiplicative_expression (pp, TREE_OPERAND (e, 1)); 
      break;

    default:
      pp_multiplicative_expression (pp, e);
      break;
    }
}

/* additive-expression:
      additive-expression
      shift-expression << additive-expression
      shift-expression >> additive-expression   */

static void
pp_c_shift_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      pp_c_shift_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_identifier (pp, code == LSHIFT_EXPR ? "<<" : ">>");
      pp_c_whitespace (pp);
      pp_c_additive_expression (pp, TREE_OPERAND (e, 1));
      break;

    default:
      pp_c_additive_expression (pp, e);
    }
}

/* relational-expression:
      shift-expression
      relational-expression < shift-expression
      relational-expression > shift-expression
      relational-expression <= shift-expression
      relational-expression >= shift-expression   */

static void
pp_c_relational_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      pp_c_relational_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      if (code == LT_EXPR)
	pp_less (pp);
      else if (code == GT_EXPR)
	pp_greater (pp);
      else if (code == LE_EXPR)
	pp_identifier (pp, "<=");
      else if (code == GE_EXPR)
	pp_identifier (pp, ">=");
      pp_c_whitespace (pp);
      pp_c_shift_expression (pp, TREE_OPERAND (e, 1));
      break;

    default:
      pp_c_shift_expression (pp, e);
      break;
    }
}

/* equality-expression:
      relational-expression
      equality-expression == relational-expression
      equality-equality != relational-expression  */

static void
pp_c_equality_expression (c_pretty_printer *pp, tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case EQ_EXPR:
    case NE_EXPR:
      pp_c_equality_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_identifier (pp, code == EQ_EXPR ? "==" : "!=");
      pp_c_whitespace (pp);
      pp_c_relational_expression (pp, TREE_OPERAND (e, 1));
      break;

    default:
      pp_c_relational_expression (pp, e);
      break;
    }
}

/* AND-expression:
      equality-expression
      AND-expression & equality-equality   */

static void
pp_c_and_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == BIT_AND_EXPR)
    {
      pp_c_and_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_ampersand (pp);
      pp_c_whitespace (pp);
      pp_c_equality_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_equality_expression (pp, e);
}

/* exclusive-OR-expression:
     AND-expression
     exclusive-OR-expression ^ AND-expression  */

static void
pp_c_exclusive_or_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == BIT_XOR_EXPR)
    {
      pp_c_exclusive_or_expression (pp, TREE_OPERAND (e, 0));
      pp_c_maybe_whitespace (pp);
      pp_carret (pp);
      pp_c_whitespace (pp);
      pp_c_and_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_and_expression (pp, e);
}

/* inclusive-OR-expression:
     exclusive-OR-expression
     inclusive-OR-expression | exclusive-OR-expression  */

static void
pp_c_inclusive_or_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == BIT_IOR_EXPR)
    {
      pp_c_exclusive_or_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_bar (pp);
      pp_c_whitespace (pp);
      pp_c_exclusive_or_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_exclusive_or_expression (pp, e);
}

/* logical-AND-expression:
      inclusive-OR-expression
      logical-AND-expression && inclusive-OR-expression  */

static void
pp_c_logical_and_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == TRUTH_ANDIF_EXPR)
    {
      pp_c_logical_and_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_identifier (pp, "&&");
      pp_c_whitespace (pp);
      pp_c_inclusive_or_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_inclusive_or_expression (pp, e);
}

/* logical-OR-expression:
      logical-AND-expression
      logical-OR-expression || logical-AND-expression  */

void
pp_c_logical_or_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == TRUTH_ORIF_EXPR)
    {
      pp_c_logical_or_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_identifier (pp, "||");
      pp_c_whitespace (pp);
      pp_c_logical_and_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_logical_and_expression (pp, e);
}

/* conditional-expression:
      logical-OR-expression
      logical-OR-expression ? expression : conditional-expression  */

static void
pp_c_conditional_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == COND_EXPR)
    {
      pp_c_logical_or_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_question (pp);
      pp_c_whitespace (pp);
      pp_expression (pp, TREE_OPERAND (e, 1));
      pp_c_whitespace (pp);
      pp_colon (pp);
      pp_c_whitespace (pp);
      pp_c_conditional_expression (pp, TREE_OPERAND (e, 2));
    }
  else
    pp_c_logical_or_expression (pp, e);
}


/* assignment-expression:
      conditional-expression
      unary-expression assignment-operator  assignment-expression 

   assignment-expression: one of
      =    *=    /=    %=    +=    -=    >>=    <<=    &=    ^=    |=  */

static void
pp_c_assignment_expression (c_pretty_printer *pp, tree e)
{
  if (TREE_CODE (e) == MODIFY_EXPR || TREE_CODE (e) == INIT_EXPR)
    {
      pp_c_unary_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_equal (pp);
      pp_space (pp);
      pp_c_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_conditional_expression (pp, e);
}

/* expression:
       assignment-expression
       expression , assignment-expression

  Implementation note:  instead of going through the usual recursion
  chain, I take the liberty of dispatching nodes to the appropriate
  functions.  This makes some redundancy, but it worths it. That also
  prevents a possible infinite recursion between pp_c_primary_expression ()
  and pp_c_expression ().  */

void
pp_c_expression (c_pretty_printer *pp, tree e)
{
  switch (TREE_CODE (e))
    {
    case INTEGER_CST:
      pp_c_integer_constant (pp, e);
      break;

    case REAL_CST:
      pp_c_floating_constant (pp, e);
      break;

    case STRING_CST:
      pp_c_string_literal (pp, e);
      break;

    case IDENTIFIER_NODE:
    case FUNCTION_DECL:
    case VAR_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
    case ERROR_MARK:
    case STMT_EXPR:
      pp_primary_expression (pp, e);
      break;

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case ARROW_EXPR:
    case ARRAY_REF:
    case CALL_EXPR:
    case COMPONENT_REF:
    case COMPLEX_CST:
    case COMPLEX_EXPR:
    case VECTOR_CST:
    case ABS_EXPR:
    case CONSTRUCTOR:
    case COMPOUND_LITERAL_EXPR:
    case VA_ARG_EXPR:
      pp_postfix_expression (pp, e);
      break;

    case CONJ_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      pp_c_unary_expression (pp, e);
      break;

    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case CONVERT_EXPR:
      pp_c_cast_expression (pp, e);
      break;

    case MULT_EXPR:
    case TRUNC_MOD_EXPR:
    case TRUNC_DIV_EXPR:
      pp_multiplicative_expression (pp, e);
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      pp_c_shift_expression (pp, e);
      break;

    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      pp_c_relational_expression (pp, e);
      break;

    case BIT_AND_EXPR:
      pp_c_and_expression (pp, e);
      break;

    case BIT_XOR_EXPR:
      pp_c_exclusive_or_expression (pp, e);
      break;

    case BIT_IOR_EXPR:
      pp_c_inclusive_or_expression (pp, e);
      break;

    case TRUTH_ANDIF_EXPR:
      pp_c_logical_and_expression (pp, e);
      break;

    case TRUTH_ORIF_EXPR:
      pp_c_logical_or_expression (pp, e);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      pp_c_equality_expression (pp, e);
      break;
      
    case COND_EXPR:
      pp_conditional_expression (pp, e);
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      pp_c_additive_expression (pp, e);
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      pp_assignment_expression (pp, e);
      break;

    case COMPOUND_EXPR:
      pp_c_left_paren (pp);
      pp_expression (pp, TREE_OPERAND (e, 0));
      pp_separate_with (pp, ',');
      pp_assignment_expression (pp, TREE_OPERAND (e, 1));
      pp_c_right_paren (pp);
      break;

    case NOP_EXPR:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
    case UNSAVE_EXPR:
      pp_expression (pp, TREE_OPERAND (e, 0));
      break;

    case TARGET_EXPR:
      pp_postfix_expression (pp, TREE_OPERAND (e, 1));
      break;
      
    default:
      pp_unsupported_tree (pp, e);
      break;
    }
}



/* Statements.  */

/* statement:
      labeled-statement
      compound-statement
      expression-statement
      selection-statement
      iteration-statement
      jump-statement   */

void
pp_c_statement (c_pretty_printer *pp, tree stmt)
{
  enum tree_code code;

  if (stmt == NULL)
    return;
  
  code = TREE_CODE (stmt);
  switch (code)
    {
       /* labeled-statement:
             identifier : statement
             case constant-expression : statement
             default : statement   */
    case LABEL_STMT:
    case CASE_LABEL:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, -3);
      else
        pp_indentation (pp) -= 3;
      if (code == LABEL_STMT)
	pp_tree_identifier (pp, DECL_NAME (LABEL_STMT_LABEL (stmt)));
      else if (code == CASE_LABEL)
	{
	  if (CASE_LOW (stmt) == NULL_TREE)
	    pp_identifier (pp, "default");
	  else
	    {
	      pp_c_identifier (pp, "case");
	      pp_c_whitespace (pp);
	      pp_conditional_expression (pp, CASE_LOW (stmt));
	      if (CASE_HIGH (stmt))
		{
		  pp_identifier (pp, "...");
		  pp_conditional_expression (pp, CASE_HIGH (stmt));
		}
	    }
	}
      pp_colon (pp);
      pp_indentation (pp) += 3;
      pp_needs_newline (pp) = true;
      break;

      /* compound-statement:
            {  block-item-list(opt) }

         block-item-list:
            block-item
            block-item-list block-item

         block-item:
            declaration
            statement   */
    case COMPOUND_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_left_brace (pp);
      pp_newline_and_indent (pp, 3);
      for (stmt = COMPOUND_BODY (stmt); stmt; stmt = TREE_CHAIN (stmt))
	pp_statement (pp, stmt);
      pp_newline_and_indent (pp, -3);
      pp_c_right_brace (pp);
      pp_needs_newline (pp) = true;
      break;

      /* expression-statement:
            expression(opt) ;  */
    case EXPR_STMT:
    case CLEANUP_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      {
        tree e = code == EXPR_STMT
          ? EXPR_STMT_EXPR (stmt)
          : CLEANUP_EXPR (stmt);
        if (e)
          pp_expression (pp, e);
      }
      pp_c_semicolon (pp);
      pp_needs_newline (pp) = true;
      break;

      /* selection-statement:
            if ( expression ) statement
            if ( expression ) statement else statement
            switch ( expression ) statement   */
    case IF_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_identifier (pp, "if");
      pp_c_whitespace (pp);
      pp_c_left_paren (pp);
      pp_expression (pp, IF_COND (stmt));
      pp_c_right_paren (pp);
      pp_newline_and_indent (pp, 3);
      pp_statement (pp, THEN_CLAUSE (stmt));
      pp_newline_and_indent (pp, -3);
      if (ELSE_CLAUSE (stmt))
	{
	  tree else_clause = ELSE_CLAUSE (stmt);
	  pp_c_identifier (pp, "else");
	  if (TREE_CODE (else_clause) == IF_STMT)
	    pp_c_whitespace (pp);
	  else
	    pp_newline_and_indent (pp, 3);
	  pp_statement (pp, else_clause);
	  if (TREE_CODE (else_clause) != IF_STMT)
	    pp_newline_and_indent (pp, -3);
	}
      break;

    case SWITCH_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_identifier (pp, "switch");
      pp_space (pp);
      pp_c_left_paren (pp);
      pp_expression (pp, SWITCH_COND (stmt));
      pp_c_right_paren (pp);
      pp_indentation (pp) += 3;
      pp_needs_newline (pp) = true;
      pp_statement (pp, SWITCH_BODY (stmt));
      pp_newline_and_indent (pp, -3);
      break;

      /* iteration-statement:
            while ( expression ) statement
            do statement while ( expression ) ;
            for ( expression(opt) ; expression(opt) ; expression(opt) ) statement
            for ( declaration expression(opt) ; expression(opt) ) statement  */
    case WHILE_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_identifier (pp, "while");
      pp_space (pp);
      pp_c_left_paren (pp);
      pp_expression (pp, WHILE_COND (stmt));
      pp_c_right_paren (pp);
      pp_newline_and_indent (pp, 3);
      pp_statement (pp, WHILE_BODY (stmt));
      pp_indentation (pp) -= 3;
      pp_needs_newline (pp) = true;
      break;

    case DO_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_identifier (pp, "do");
      pp_newline_and_indent (pp, 3);
      pp_statement (pp, DO_BODY (stmt));
      pp_newline_and_indent (pp, -3);
      pp_c_identifier (pp, "while");
      pp_space (pp);
      pp_c_left_paren (pp);
      pp_expression (pp, DO_COND (stmt));
      pp_c_right_paren (pp);
      pp_c_semicolon (pp);
      pp_needs_newline (pp) = true;
      break;

    case FOR_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_c_identifier (pp, "for");
      pp_space (pp);
      pp_c_left_paren (pp);
      if (FOR_INIT_STMT (stmt))
        pp_statement (pp, FOR_INIT_STMT (stmt));
      else
        pp_c_semicolon (pp);
      pp_needs_newline (pp) = false;
      pp_c_whitespace (pp);
      if (FOR_COND (stmt))
	pp_expression (pp, FOR_COND (stmt));
      pp_c_semicolon (pp);
      pp_needs_newline (pp) = false;
      pp_c_whitespace (pp);
      if (FOR_EXPR (stmt))
	pp_expression (pp, FOR_EXPR (stmt));
      pp_c_right_paren (pp);
      pp_newline_and_indent (pp, 3);
      pp_statement (pp, FOR_BODY (stmt));
      pp_indentation (pp) -= 3;
      pp_needs_newline (pp) = true;
      break;

      /* jump-statement:
            goto identifier;
            continue ;
            return expression(opt) ;  */
    case BREAK_STMT:
    case CONTINUE_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_identifier (pp, code == BREAK_STMT ? "break" : "continue");
      pp_c_semicolon (pp);
      pp_needs_newline (pp) = true;
      break;

    case RETURN_STMT:
    case GOTO_STMT:
      {
	tree e = code == RETURN_STMT
	  ? RETURN_STMT_EXPR (stmt)
	  : GOTO_DESTINATION (stmt);
        if (pp_needs_newline (pp))
          pp_newline_and_indent (pp, 0);
	pp_c_identifier (pp, code == RETURN_STMT ? "return" : "goto");
        pp_c_whitespace (pp);
	if (e)
          {
            if (TREE_CODE (e) == INIT_EXPR
                && TREE_CODE (TREE_OPERAND (e, 0)) == RESULT_DECL)
              e = TREE_OPERAND (e, 1);
            pp_expression (pp, e);
          }
	pp_c_semicolon (pp);
	pp_needs_newline (pp) = true;
      }
      break;

    case SCOPE_STMT:
      if (!SCOPE_NULLIFIED_P (stmt) && SCOPE_NO_CLEANUPS_P (stmt))
        {
          int i = 0;
          if (pp_needs_newline (pp))
            pp_newline_and_indent (pp, 0);
          if (SCOPE_BEGIN_P (stmt))
            {
              pp_left_brace (pp);
              i = 3;
            }
          else if (SCOPE_END_P (stmt))
            {
              pp_right_brace (pp);
              i = -3;
            }
          pp_indentation (pp) += i;
          pp_needs_newline (pp) = true;
        }
      break;

    case DECL_STMT:
      if (pp_needs_newline (pp))
        pp_newline_and_indent (pp, 0);
      pp_declaration (pp, DECL_STMT_DECL (stmt));
      pp_needs_newline (pp) = true;
      break;

    case ASM_STMT:
      {
	bool has_volatile_p = ASM_VOLATILE_P (stmt);
	bool is_extended = has_volatile_p || ASM_INPUTS (stmt)
	  || ASM_OUTPUTS (stmt) || ASM_CLOBBERS (stmt);
	pp_c_identifier (pp, is_extended ? "__asm__" : "asm");
	if (has_volatile_p)
	  pp_c_identifier (pp, "__volatile__");
	pp_space (pp);
	pp_c_left_paren (pp);
	pp_c_string_literal (pp, ASM_STRING (stmt));
	if (is_extended)
	  {
	    pp_space (pp);
	    pp_separate_with (pp, ':');
	    if (ASM_OUTPUTS (stmt))
	      pp_expression (pp, ASM_OUTPUTS (stmt));
	    pp_space (pp);
	    pp_separate_with (pp, ':');
	    if (ASM_INPUTS (stmt))
	      pp_expression (pp, ASM_INPUTS (stmt));
	    pp_space (pp);
	    pp_separate_with (pp, ':');
	    if (ASM_CLOBBERS (stmt))
	      pp_expression (pp, ASM_CLOBBERS (stmt));
	  }
	pp_c_right_paren (pp);
	pp_newline (pp);
      }
      break;

    case FILE_STMT:
      pp_c_identifier (pp, "__FILE__");
      pp_space (pp);
      pp_equal (pp);
      pp_c_whitespace (pp);
      pp_c_identifier (pp, FILE_STMT_FILENAME (stmt));
      pp_c_semicolon (pp);
      pp_needs_newline (pp) = true;
      break;

    default:
      pp_unsupported_tree (pp, stmt);
    }
}


/* Initialize the PRETTY-PRINTER for handling C codes.  */

void
pp_c_pretty_printer_init (c_pretty_printer *pp)
{
  pp->offset_list               = 0;

  pp->declaration               = pp_c_declaration;
  pp->declaration_specifiers    = pp_c_declaration_specifiers;
  pp->declarator                = pp_c_declarator;
  pp->direct_declarator         = pp_c_direct_declarator;
  pp->type_specifier_seq        = pp_c_specifier_qualifier_list;
  pp->abstract_declarator       = pp_c_abstract_declarator;
  pp->direct_abstract_declarator = pp_c_direct_abstract_declarator;
  pp->ptr_operator              = pp_c_pointer;
  pp->parameter_list            = pp_c_parameter_type_list;
  pp->type_id                   = pp_c_type_id;
  pp->simple_type_specifier     = pp_c_type_specifier;
  pp->function_specifier        = pp_c_function_specifier;
  pp->storage_class_specifier   = pp_c_storage_class_specifier;

  pp->statement                 = pp_c_statement;

  pp->id_expression             = pp_c_id_expression;
  pp->primary_expression        = pp_c_primary_expression;
  pp->postfix_expression        = pp_c_postfix_expression;
  pp->unary_expression          = pp_c_unary_expression;
  pp->initializer               = pp_c_initializer;
  pp->multiplicative_expression = pp_c_multiplicative_expression;
  pp->conditional_expression    = pp_c_conditional_expression;
  pp->assignment_expression     = pp_c_assignment_expression;
  pp->expression                = pp_c_expression;
}
