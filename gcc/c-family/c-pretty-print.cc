/* Subroutines common to both C and C++ pretty-printers.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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
#include "c-pretty-print.h"
#include "gimple-pretty-print.h"
#include "diagnostic.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "attribs.h"
#include "intl.h"
#include "tree-pretty-print.h"
#include "selftest.h"
#include "langhooks.h"
#include "options.h"
#include "internal-fn.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "make-unique.h"

/* The pretty-printer code is primarily designed to closely follow
   (GNU) C and C++ grammars.  That is to be contrasted with spaghetti
   codes we used to have in the past.  Following a structured
   approach (preferably the official grammars) is believed to make it
   much easier to add extensions and nifty pretty-printing effects that
   takes expression or declaration contexts into account.  */


#define pp_c_maybe_whitespace(PP)            \
   do {                                      \
     if ((PP)->get_padding () == pp_before)  \
       pp_c_whitespace (PP);                 \
   } while (0)

/* literal  */
static void pp_c_char (c_pretty_printer *, int);

/* postfix-expression  */
static void pp_c_initializer_list (c_pretty_printer *, tree);
static void pp_c_brace_enclosed_initializer_list (c_pretty_printer *, tree);

static void pp_c_additive_expression (c_pretty_printer *, tree);
static void pp_c_shift_expression (c_pretty_printer *, tree);
static void pp_c_relational_expression (c_pretty_printer *, tree);
static void pp_c_equality_expression (c_pretty_printer *, tree);
static void pp_c_and_expression (c_pretty_printer *, tree);
static void pp_c_exclusive_or_expression (c_pretty_printer *, tree);
static void pp_c_inclusive_or_expression (c_pretty_printer *, tree);
static void pp_c_logical_and_expression (c_pretty_printer *, tree);

/* declarations.  */


/* Helper functions.  */

void
pp_c_whitespace (c_pretty_printer *pp)
{
  pp_space (pp);
  pp->set_padding (pp_none);
}

void
pp_c_left_paren (c_pretty_printer *pp)
{
  pp_left_paren (pp);
  pp->set_padding (pp_none);
}

void
pp_c_right_paren (c_pretty_printer *pp)
{
  pp_right_paren (pp);
  pp->set_padding (pp_none);
}

void
pp_c_left_brace (c_pretty_printer *pp)
{
  pp_left_brace (pp);
  pp->set_padding (pp_none);
}

void
pp_c_right_brace (c_pretty_printer *pp)
{
  pp_right_brace (pp);
  pp->set_padding (pp_none);
}

void
pp_c_left_bracket (c_pretty_printer *pp)
{
  pp_left_bracket (pp);
  pp->set_padding (pp_none);
}

void
pp_c_right_bracket (c_pretty_printer *pp)
{
  pp_right_bracket (pp);
  pp->set_padding (pp_none);
}

void
pp_c_dot (c_pretty_printer *pp)
{
  pp_dot (pp);
  pp->set_padding (pp_none);
}

void
pp_c_ampersand (c_pretty_printer *pp)
{
  pp_ampersand (pp);
  pp->set_padding (pp_none);
}

void
pp_c_star (c_pretty_printer *pp)
{
  pp_star (pp);
  pp->set_padding (pp_none);
}

void
pp_c_arrow (c_pretty_printer *pp)
{
  pp_arrow (pp);
  pp->set_padding (pp_none);
}

void
pp_c_semicolon (c_pretty_printer *pp)
{
  pp_semicolon (pp);
  pp->set_padding (pp_none);
}

void
pp_c_complement (c_pretty_printer *pp)
{
  pp_complement (pp);
  pp->set_padding (pp_none);
}

void
pp_c_exclamation (c_pretty_printer *pp)
{
  pp_exclamation (pp);
  pp->set_padding (pp_none);
}

/* Print out the external representation of QUALIFIERS.  */

void
pp_c_cv_qualifiers (c_pretty_printer *pp, int qualifiers, bool func_type)
{
  const char *p = pp_last_position_in_text (pp);

  if (!qualifiers)
    return;

  /* The C programming language does not have references, but it is much
     simpler to handle those here rather than going through the same
     logic in the C++ pretty-printer.  */
  if (p != NULL && (*p == '*' || *p == '&'))
    pp_c_whitespace (pp);

  if (qualifiers & TYPE_QUAL_ATOMIC)
    pp_c_ws_string (pp, "_Atomic");
  if (qualifiers & TYPE_QUAL_CONST)
    pp_c_ws_string (pp, func_type ? "__attribute__((const))" : "const");
  if (qualifiers & TYPE_QUAL_VOLATILE)
    pp_c_ws_string (pp, func_type ? "__attribute__((noreturn))" : "volatile");
  if (qualifiers & TYPE_QUAL_RESTRICT)
    pp_c_ws_string (pp, (flag_isoc99 && !c_dialect_cxx ()
			 ? "restrict" : "__restrict__"));
}

/* Pretty-print T using the type-cast notation '( type-name )'.  */

void
pp_c_type_cast (c_pretty_printer *pp, tree t)
{
  pp_c_left_paren (pp);
  pp->type_id (t);
  pp_c_right_paren (pp);
}

/* We're about to pretty-print a pointer type as indicated by T.
   Output a whitespace, if needed, preparing for subsequent output.  */

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
       address-space-qualifier		     -- GNU C
       volatile
       _Atomic                               -- C11

   address-space-qualifier:
       identifier			     -- GNU C  */

void
pp_c_type_qualifier_list (c_pretty_printer *pp, tree t)
{
  int qualifiers;

  if (!t || t == error_mark_node)
    return;

  if (!TYPE_P (t))
    t = TREE_TYPE (t);

  if (TREE_CODE (t) != ARRAY_TYPE)
    {
      qualifiers = TYPE_QUALS (t);
      pp_c_cv_qualifiers (pp, qualifiers,
			  TREE_CODE (t) == FUNCTION_TYPE);
    }

  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (t)))
    {
      const char *as = c_addr_space_name (TYPE_ADDR_SPACE (t));
      pp_c_identifier (pp, as);
    }
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
	{
	  pp_c_ampersand (pp);
	  if (TYPE_REF_IS_RVALUE (t))
	    pp_c_ampersand (pp);
	}
      pp_c_type_qualifier_list (pp, t);
      break;

      /* ??? This node is now in GENERIC and so shouldn't be here.  But
	 we'll fix that later.  */
    case DECL_EXPR:
      pp->declaration (DECL_EXPR_DECL (t));
      pp_needs_newline (pp) = true;
      break;

    default:
      pp_unsupported_tree (pp, t);
    }
}

/* simple-type-specifier:
     type-specifier

   type-specifier:
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
      nullptr_t                      -- C23
      struct-or-union-specifier
      enum-specifier
      typedef-name.

  GNU extensions.
  simple-type-specifier:
      __complex__
      __vector__   */

void
c_pretty_printer::simple_type_specifier (tree t)
{
  const enum tree_code code = TREE_CODE (t);
  switch (code)
    {
    case ERROR_MARK:
      translate_string ("<type-error>");
      break;

    case IDENTIFIER_NODE:
      pp_c_identifier (this, IDENTIFIER_POINTER (t));
      break;

    case VOID_TYPE:
    case OPAQUE_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      if (TYPE_NAME (t))
	{
	  t = TYPE_NAME (t);
	  simple_type_specifier (t);
	}
      else
	{
	  int prec = TYPE_PRECISION (t);
	  tree common_t;
	  if (ALL_FIXED_POINT_MODE_P (TYPE_MODE (t)))
	    common_t = c_common_type_for_mode (TYPE_MODE (t),
					       TYPE_SATURATING (t));
	  else
	    common_t = c_common_type_for_mode (TYPE_MODE (t),
					       TYPE_UNSIGNED (t));
	  if (common_t && TYPE_NAME (common_t))
	    {
	      simple_type_specifier (common_t);
	      if (TYPE_PRECISION (common_t) != prec)
		{
		  pp_colon (this);
		  pp_decimal_int (this, prec);
		}
	    }
	  else
	    {
	      switch (code)
		{
		case INTEGER_TYPE:
		  translate_string (TYPE_UNSIGNED (t)
                                    ? "<unnamed-unsigned:"
                                    : "<unnamed-signed:");
		  break;
		case REAL_TYPE:
		  translate_string ("<unnamed-float:");
		  break;
		case FIXED_POINT_TYPE:
		  translate_string ("<unnamed-fixed:");
		  break;
		default:
		  gcc_unreachable ();
		}
	      pp_decimal_int (this, prec);
	      pp_greater (this);
	    }
	}
      break;

    case BITINT_TYPE:
      if (TYPE_NAME (t))
	{
	  t = TYPE_NAME (t);
	  simple_type_specifier (t);
	}
      else
	{
	  int prec = TYPE_PRECISION (t);
	  if (TYPE_UNSIGNED (t))
	    pp_c_ws_string (this, "unsigned");
	  pp_c_ws_string (this, "_BitInt(");;
	  pp_decimal_int (this, prec);
	  pp_right_paren (this);
	}
      break;

    case TYPE_DECL:
      if (DECL_NAME (t))
	id_expression (t);
      else
	translate_string ("<typedef-error>");
      break;

    case UNION_TYPE:
    case RECORD_TYPE:
    case ENUMERAL_TYPE:
      if (TYPE_NAME (t) && TREE_CODE (TYPE_NAME (t)) == TYPE_DECL)
	/* Don't decorate the type if this is a typedef name.  */;
      else if (code == UNION_TYPE)
	pp_c_ws_string (this, "union");
      else if (code == RECORD_TYPE)
	pp_c_ws_string (this, "struct");
      else if (code == ENUMERAL_TYPE)
	pp_c_ws_string (this, "enum");
      else
	translate_string ("<tag-error>");

      if (TYPE_NAME (t))
	id_expression (TYPE_NAME (t));
      else
	translate_string ("<anonymous>");
      break;
    case NULLPTR_TYPE:
      pp_c_ws_string (this, "nullptr_t");
      break;

    default:
      pp_unsupported_tree (this, t);
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
  remaining part is done by declarator() or abstract_declarator().  */

void
pp_c_specifier_qualifier_list (c_pretty_printer *pp, tree t)
{
  const enum tree_code code = TREE_CODE (t);

  if (!(pp->flags & pp_c_flag_gnu_v3) && code != POINTER_TYPE)
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
	    /* If we're dealing with the GNU form of attributes, print this:
		 void (__attribute__((noreturn)) *f) ();
	       If it is the standard [[]] attribute, we'll print the attribute
	       in c_pretty_printer::direct_abstract_declarator/FUNCTION_TYPE.  */
	    if (!cxx11_attribute_p (TYPE_ATTRIBUTES (pointee)))
	      pp_c_attributes_display (pp, TYPE_ATTRIBUTES (pointee));
	  }
	else if (!c_dialect_cxx ())
	  pp_c_whitespace (pp);
	pp_ptr_operator (pp, t);
      }
      break;

    case FUNCTION_TYPE:
    case ARRAY_TYPE:
      pp_c_specifier_qualifier_list (pp, TREE_TYPE (t));
      break;

    case VECTOR_TYPE:
    case COMPLEX_TYPE:
      if (code == COMPLEX_TYPE)
	pp_c_ws_string (pp, (flag_isoc99 && !c_dialect_cxx ()
			     ? "_Complex" : "__complex__"));
      else if (code == VECTOR_TYPE)
	{
	  /* The syntax we print for vector types isn't real C or C++ syntax,
	     so it's better to print the type name if we have one.  */
	  tree name = TYPE_NAME (t);
	  if (!(pp->flags & pp_c_flag_gnu_v3)
	      && name
	      && TREE_CODE (name) == TYPE_DECL)
	    {
	      pp->id_expression (name);
	      break;
	    }
	  pp_c_ws_string (pp, "__vector");
	  pp_c_left_paren (pp);
	  pp_wide_integer (pp, TYPE_VECTOR_SUBPARTS (t));
	  pp_c_right_paren (pp);
	  pp_c_whitespace (pp);
	}
      pp_c_specifier_qualifier_list (pp, TREE_TYPE (t));
      break;

    default:
      pp->simple_type_specifier (t);
      break;
    }
  if ((pp->flags & pp_c_flag_gnu_v3) && code != POINTER_TYPE)
    pp_c_type_qualifier_list (pp, t);
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
    pp_c_ws_string (pp, "void");
  else
    {
      bool first = true;
      for ( ; parms && parms != void_list_node; parms = TREE_CHAIN (parms))
	{
	  if (!first)
	    pp_separate_with (pp, ',');
	  first = false;
	  pp->declaration_specifiers
	    (want_parm_decl ? parms : TREE_VALUE (parms));
	  if (want_parm_decl)
	    pp->declarator (parms);
	  else
	    pp->abstract_declarator (TREE_VALUE (parms));
	}
      if (!first && !parms)
	{
	  pp_separate_with (pp, ',');
	  pp_string (pp, "...");
	}
    }
  pp_c_right_paren (pp);
}

/* abstract-declarator:
      pointer
      pointer(opt) direct-abstract-declarator  */

void
c_pretty_printer::abstract_declarator (tree t)
{
  if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	pp_c_right_paren (this);
      t = TREE_TYPE (t);
    }

  direct_abstract_declarator (t);
}

/* direct-abstract-declarator:
      ( abstract-declarator )
      direct-abstract-declarator(opt) [ assignment-expression(opt) ]
      direct-abstract-declarator(opt) [ * ]
      direct-abstract-declarator(opt) ( parameter-type-list(opt) )  */

void
c_pretty_printer::direct_abstract_declarator (tree t)
{
  bool add_space = false;

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      abstract_declarator (t);
      break;

    case FUNCTION_TYPE:
      pp_c_parameter_type_list (this, t);
      direct_abstract_declarator (TREE_TYPE (t));
      /* If this is the standard [[]] attribute, print
	 void (*)() [[noreturn]];  */
      if (cxx11_attribute_p (TYPE_ATTRIBUTES (t)))
	{
	  pp_space (this);
	  pp_c_attributes_display (this, TYPE_ATTRIBUTES (t));
	}
      break;

    case ARRAY_TYPE:
      pp_c_left_bracket (this);

      if (int quals = TYPE_QUALS (t))
	{
	  /* Print the array qualifiers such as in "T[const restrict 3]".  */
	  pp_c_cv_qualifiers (this, quals, false);
	  add_space = true;
	}

      if (tree arr = lookup_attribute ("array", TYPE_ATTRIBUTES (t)))
	{
	  if (TREE_VALUE (arr))
	    {
	      /* Print the specifier as in "T[static 3]" that's not actually
		 part of the type but may be added by the front end.  */
	      pp_c_ws_string (this, "static");
	      add_space = true;
	    }
	  else if (!TYPE_DOMAIN (t))
	    /* For arrays of unspecified bound using the [*] notation. */
	    pp_character (this, '*');
	}

      if (tree dom = TYPE_DOMAIN (t))
	{
	  if (tree maxval = TYPE_MAX_VALUE (dom))
	    {
	      if (add_space)
		pp_space (this);

	      tree type = TREE_TYPE (maxval);

	      if (tree_fits_shwi_p (maxval))
		pp_wide_integer (this, tree_to_shwi (maxval) + 1);
	      else if (TREE_CODE (maxval) == INTEGER_CST)
		expression (fold_build2 (PLUS_EXPR, type, maxval,
					 build_int_cst (type, 1)));
	      else
		{
		  /* Strip the expressions from around a VLA bound added
		     internally to make it fit the domain mold, including
		     any casts.  */
		  if (TREE_CODE (maxval) == NOP_EXPR)
		    maxval = TREE_OPERAND (maxval, 0);
		  if (TREE_CODE (maxval) == PLUS_EXPR
		      && integer_all_onesp (TREE_OPERAND (maxval, 1)))
		    {
		      maxval = TREE_OPERAND (maxval, 0);
		      if (TREE_CODE (maxval) == NOP_EXPR)
			maxval = TREE_OPERAND (maxval, 0);
		    }
		  if (TREE_CODE (maxval) == SAVE_EXPR)
		    {
		      maxval = TREE_OPERAND (maxval, 0);
		      if (TREE_CODE (maxval) == NOP_EXPR)
			maxval = TREE_OPERAND (maxval, 0);
		    }

		  expression (maxval);
		}
	    }
	  else if (TYPE_SIZE (t))
	    /* Print zero for zero-length arrays but not for flexible
	       array members whose TYPE_SIZE is null.  */
	    pp_string (this, "0");
	}
      pp_c_right_bracket (this);
      direct_abstract_declarator (TREE_TYPE (t));
      break;

    case IDENTIFIER_NODE:
    case VOID_TYPE:
    case OPAQUE_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BITINT_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case VECTOR_TYPE:
    case COMPLEX_TYPE:
    case TYPE_DECL:
    case ERROR_MARK:
    case NULLPTR_TYPE:
      break;

    default:
      pp_unsupported_tree (this, t);
      break;
    }
}

/* type-name:
      specifier-qualifier-list  abstract-declarator(opt)  */

void
c_pretty_printer::type_id (tree t)
{
  pp_c_specifier_qualifier_list (this, t);
  abstract_declarator (t);
}

/* storage-class-specifier:
      typedef
      extern
      static
      auto
      register  */

void
c_pretty_printer::storage_class_specifier (tree t)
{
  if (TREE_CODE (t) == TYPE_DECL)
    pp_c_ws_string (this, "typedef");
  else if (DECL_P (t))
    {
      if (DECL_REGISTER (t))
	pp_c_ws_string (this, "register");
      else if (TREE_STATIC (t) && VAR_P (t))
	pp_c_ws_string (this, "static");
    }
}

/* function-specifier:
      inline   */

void
c_pretty_printer::function_specifier (tree t)
{
  if (TREE_CODE (t) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (t))
    pp_c_ws_string (this, "inline");
}

/* declaration-specifiers:
      storage-class-specifier declaration-specifiers(opt)
      type-specifier declaration-specifiers(opt)
      type-qualifier declaration-specifiers(opt)
      function-specifier declaration-specifiers(opt)  */

void
c_pretty_printer::declaration_specifiers (tree t)
{
  storage_class_specifier (t);
  function_specifier (t);
  pp_c_specifier_qualifier_list (this, DECL_P (t) ?  TREE_TYPE (t) : t);
}

/* direct-declarator
      identifier
      ( declarator )
      direct-declarator [ type-qualifier-list(opt) assignment-expression(opt) ]
      direct-declarator [ static type-qualifier-list(opt) assignment-expression(opt)]
      direct-declarator [ type-qualifier-list static assignment-expression ]
      direct-declarator [ type-qualifier-list * ]
      direct-declarator ( parameter-type-list )
      direct-declarator ( identifier-list(opt) )  */

void
c_pretty_printer::direct_declarator (tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case TYPE_DECL:
    case FIELD_DECL:
    case LABEL_DECL:
      pp_c_space_for_pointer_operator (this, TREE_TYPE (t));
      pp_c_tree_decl_identifier (this, t);
      break;

    case ARRAY_TYPE:
    case POINTER_TYPE:
      abstract_declarator (TREE_TYPE (t));
      break;

    case FUNCTION_TYPE:
      pp_parameter_list (this, t);
      abstract_declarator (TREE_TYPE (t));
      break;

    case FUNCTION_DECL:
      pp_c_space_for_pointer_operator (this, TREE_TYPE (TREE_TYPE (t)));
      pp_c_tree_decl_identifier (this, t);
      if (flags & pp_c_flag_abstract)
	abstract_declarator (TREE_TYPE (t));
      else
	{
	  pp_parameter_list (this, t);
	  abstract_declarator (TREE_TYPE (TREE_TYPE (t)));
	}
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BITINT_TYPE:
    case UNION_TYPE:
    case RECORD_TYPE:
      break;

    default:
      pp_unsupported_tree (this, t);
      break;
    }
}


/* declarator:
      pointer(opt)  direct-declarator   */

void
c_pretty_printer::declarator (tree t)
{
  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BITINT_TYPE:
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
      direct_declarator (t);
    break;


    default:
      pp_unsupported_tree (this, t);
      break;
    }
}

/* declaration:
      declaration-specifiers init-declarator-list(opt) ;  */

void
c_pretty_printer::declaration (tree t)
{
  declaration_specifiers (t);
  pp_c_init_declarator (this, t);
}

/* Pretty-print ATTRIBUTES marked to be displayed on diagnostic.  */

void
pp_c_attributes_display (c_pretty_printer *pp, tree a)
{
  bool is_first = true;

  if (a == NULL_TREE)
    return;

  const bool std_p = cxx11_attribute_p (a);

  for (; a != NULL_TREE; a = TREE_CHAIN (a))
    {
      const struct attribute_spec *as
	= lookup_attribute_spec (get_attribute_name (a));
      if (!as || as->affects_type_identity == false)
        continue;
      if (c_dialect_cxx ()
	  && !strcmp ("transaction_safe", as->name))
	/* In C++ transaction_safe is printed at the end of the declarator.  */
	continue;
      if (is_first)
	{
	  if (std_p)
	    {
	      pp_c_left_bracket (pp);
	      pp_c_left_bracket (pp);
	    }
	  else
	    {
	      pp_c_ws_string (pp, "__attribute__");
	      pp_c_left_paren (pp);
	      pp_c_left_paren (pp);
	    }
	  is_first = false;
	}
      else
	pp_separate_with (pp, ',');
      tree ns;
      if (std_p && (ns = get_attribute_namespace (a)))
	{
	  pp_tree_identifier (pp, ns);
	  pp_colon (pp);
	  pp_colon (pp);
	}
      pp_tree_identifier (pp, get_attribute_name (a));
      if (TREE_VALUE (a))
	pp_c_call_argument_list (pp, TREE_VALUE (a));
    }

  if (!is_first)
    {
      if (std_p)
	{
	  pp_c_right_bracket (pp);
	  pp_c_right_bracket (pp);
	}
      else
	{
	  pp_c_right_paren (pp);
	  pp_c_right_paren (pp);
	  pp_c_whitespace (pp);
	}
    }
}

/* function-definition:
      declaration-specifiers declarator compound-statement  */

void
pp_c_function_definition (c_pretty_printer *pp, tree t)
{
  pp->declaration_specifiers (t);
  pp->declarator (t);
  pp_needs_newline (pp) = true;
  pp->statement (DECL_SAVED_TREE (t));
  pp_newline_and_flush (pp);
}


/* Expressions.  */

/* Print out a c-char.  This is called solely for characters which are
   in the *target* execution character set.  We ought to convert them
   back to the *host* execution character set before printing, but we
   have no way to do this at present.  A decent compromise is to print
   all characters as if they were in the host execution character set,
   and not attempt to recover any named escape characters, but render
   all unprintables as octal escapes.  If the host and target character
   sets are the same, this produces relatively readable output.  If they
   are not the same, strings may appear as gibberish, but that's okay
   (in fact, it may well be what the reader wants, e.g. if they are looking
   to see if conversion to the target character set happened correctly).

   A special case: we need to prefix \, ", and ' with backslashes.  It is
   correct to do so for the *host*'s \, ", and ', because the rest of the
   file appears in the host character set.  */

static void
pp_c_char (c_pretty_printer *pp, int c)
{
  if (ISPRINT (c))
    {
      switch (c)
	{
	case '\\': pp_string (pp, "\\\\"); break;
	case '\'': pp_string (pp, "\\\'"); break;
	case '\"': pp_string (pp, "\\\""); break;
	default:   pp_character (pp, c);
	}
    }
  else
    pp_scalar (pp, "\\%03o", (unsigned) c);
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

/* Pretty-print a VOID_CST (void_node).  */

static void
pp_c_void_constant (c_pretty_printer *pp)
{
  pp_c_type_cast (pp, void_type_node);
  pp_string (pp, "0");
}

/* Pretty-print an INTEGER literal.  */

void
pp_c_integer_constant (c_pretty_printer *pp, tree i)
{
  if (tree_fits_shwi_p (i))
    pp_wide_integer (pp, tree_to_shwi (i));
  else if (tree_fits_uhwi_p (i))
    pp_unsigned_wide_integer (pp, tree_to_uhwi (i));
  else
    {
      wide_int wi = wi::to_wide (i);

      if (wi::lt_p (wi::to_wide (i), 0, TYPE_SIGN (TREE_TYPE (i))))
	{
	  pp_minus (pp);
	  wi = -wi;
	}
      unsigned int prec = wi.get_precision ();
      if ((prec + 3) / 4 > sizeof (pp_buffer (pp)->m_digit_buffer) - 3)
	{
	  char *buf = XALLOCAVEC (char, (prec + 3) / 4 + 3);
	  print_hex (wi, buf);
	  pp_string (pp, buf);
	}
      else
	{
	  print_hex (wi, pp_buffer (pp)->m_digit_buffer);
	  pp_string (pp, pp_buffer (pp)->m_digit_buffer);
	}
    }
}

/* Print out a CHARACTER literal.  */

static void
pp_c_character_constant (c_pretty_printer *pp, tree c)
{
  pp_quote (pp);
  pp_c_char (pp, (unsigned) TREE_INT_CST_LOW (c));
  pp_quote (pp);
}

/* Print out a BOOLEAN literal.  */

static void
pp_c_bool_constant (c_pretty_printer *pp, tree b)
{
  if (b == boolean_false_node)
    {
      if (c_dialect_cxx ())
	pp_c_ws_string (pp, "false");
      else if (flag_isoc99)
	pp_c_ws_string (pp, "_False");
      else
	pp_unsupported_tree (pp, b);
    }
  else if (b == boolean_true_node)
    {
      if (c_dialect_cxx ())
	pp_c_ws_string (pp, "true");
      else if (flag_isoc99)
	pp_c_ws_string (pp, "_True");
      else
	pp_unsupported_tree (pp, b);
    }
  else if (TREE_CODE (b) == INTEGER_CST)
    pp_c_integer_constant (pp, b);
  else
    pp_unsupported_tree (pp, b);
}

/* Given a value e of ENUMERAL_TYPE:
   Print out the first ENUMERATOR id with value e, if one is found,
   else print out the value as a C-style cast (type-id)value.  */

static void
pp_c_enumeration_constant (c_pretty_printer *pp, tree e)
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
    pp->id_expression (TREE_PURPOSE (value));
  else
    {
      /* Value must have been cast.  */
      pp_c_type_cast (pp, type);
      pp_c_integer_constant (pp, e);
    }
}

/* Print out a REAL value as a decimal-floating-constant.  */

static void
pp_c_floating_constant (c_pretty_printer *pp, tree r)
{
  const struct real_format *fmt
    = REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (r)));

  REAL_VALUE_TYPE floating_cst = TREE_REAL_CST (r);
  bool is_decimal = floating_cst.decimal;

  /* See ISO C++ WG N1822.  Note: The fraction 643/2136 approximates
     log10(2) to 7 significant digits.  */
  int max_digits10 = 2 + (is_decimal ? fmt->p : fmt->p * 643L / 2136);

  real_to_decimal (pp_buffer (pp)->m_digit_buffer, &TREE_REAL_CST (r),
		   sizeof (pp_buffer (pp)->m_digit_buffer),
		   max_digits10, 1);

  pp_string (pp, pp_buffer(pp)->m_digit_buffer);
  if (TREE_TYPE (r) == float_type_node)
    pp_character (pp, 'f');
  else if (TREE_TYPE (r) == long_double_type_node)
    pp_character (pp, 'l');
  else if (TREE_TYPE (r) == dfloat128_type_node)
    pp_string (pp, "dl");
  else if (TREE_TYPE (r) == dfloat64_type_node)
    pp_string (pp, "dd");
  else if (TREE_TYPE (r) == dfloat32_type_node)
    pp_string (pp, "df");
  else if (TREE_TYPE (r) != double_type_node)
    for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
      if (TREE_TYPE (r) == FLOATN_NX_TYPE_NODE (i))
	{
	  pp_character (pp, 'f');
	  pp_decimal_int (pp, floatn_nx_types[i].n);
	  if (floatn_nx_types[i].extended)
	    pp_character (pp, 'x');
	  break;
	}
}

/* Print out a FIXED value as a decimal-floating-constant.  */

static void
pp_c_fixed_constant (c_pretty_printer *pp, tree r)
{
  fixed_to_decimal (pp_buffer (pp)->m_digit_buffer, &TREE_FIXED_CST (r),
		   sizeof (pp_buffer (pp)->m_digit_buffer));
  pp_string (pp, pp_buffer(pp)->m_digit_buffer);
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

/* Pretty-print a COMPLEX_EXPR expression.  */

static void
pp_c_complex_expr (c_pretty_printer *pp, tree e)
{
  /* Handle a few common special cases, otherwise fallback
     to printing it as compound literal.  */
  tree type = TREE_TYPE (e);
  tree realexpr = TREE_OPERAND (e, 0);
  tree imagexpr = TREE_OPERAND (e, 1);

  /* Cast of an COMPLEX_TYPE expression to a different COMPLEX_TYPE.  */
  if (TREE_CODE (realexpr) == NOP_EXPR
      && TREE_CODE (imagexpr) == NOP_EXPR
      && TREE_TYPE (realexpr) == TREE_TYPE (type)
      && TREE_TYPE (imagexpr) == TREE_TYPE (type)
      && TREE_CODE (TREE_OPERAND (realexpr, 0)) == REALPART_EXPR
      && TREE_CODE (TREE_OPERAND (imagexpr, 0)) == IMAGPART_EXPR
      && TREE_OPERAND (TREE_OPERAND (realexpr, 0), 0)
	 == TREE_OPERAND (TREE_OPERAND (imagexpr, 0), 0))
    {
      pp_c_type_cast (pp, type);
      pp->expression (TREE_OPERAND (TREE_OPERAND (realexpr, 0), 0));
      return;
    }

  /* Cast of an scalar expression to COMPLEX_TYPE.  */
  if ((integer_zerop (imagexpr) || real_zerop (imagexpr))
      && TREE_TYPE (realexpr) == TREE_TYPE (type))
    {
      pp_c_type_cast (pp, type);
      if (TREE_CODE (realexpr) == NOP_EXPR)
	realexpr = TREE_OPERAND (realexpr, 0);
      pp->expression (realexpr);
      return;
    }

  pp_c_compound_literal (pp, e);
}

/* constant:
      integer-constant
      floating-constant
      fixed-point-constant
      enumeration-constant
      character-constant   */

void
c_pretty_printer::constant (tree e)
{
  const enum tree_code code = TREE_CODE (e);

  switch (code)
    {
    case VOID_CST:
      pp_c_void_constant (this);
      break;

    case INTEGER_CST:
      {
	tree type = TREE_TYPE (e);
	if (type == boolean_type_node)
	  pp_c_bool_constant (this, e);
	else if (type == char_type_node)
	  pp_c_character_constant (this, e);
	else if (TREE_CODE (type) == ENUMERAL_TYPE)
	  pp_c_enumeration_constant (this, e);
	else if (NULLPTR_TYPE_P (type))
	  pp_string (this, "nullptr");
	else
	  pp_c_integer_constant (this, e);
      }
      break;

    case REAL_CST:
      pp_c_floating_constant (this, e);
      break;

    case FIXED_CST:
      pp_c_fixed_constant (this, e);
      break;

    case STRING_CST:
      pp_c_string_literal (this, e);
      break;

    case COMPLEX_CST:
      /* Sometimes, we are confused and we think a complex literal
         is a constant.  Such thing is a compound literal which
         grammatically belongs to postfix-expr production.  */
      pp_c_compound_literal (this, e);
      break;

    default:
      pp_unsupported_tree (this, e);
      break;
    }
}

/* Pretty-print a string such as an identifier, without changing its
   encoding, preceded by whitespace is necessary.  */

void
pp_c_ws_string (c_pretty_printer *pp, const char *str)
{
  pp_c_maybe_whitespace (pp);
  pp_string (pp, str);
  pp->set_padding (pp_before);
}

void
c_pretty_printer::translate_string (const char *gmsgid)
{
  if (pp_translate_identifiers (this))
    pp_c_ws_string (this, _(gmsgid));
  else
    pp_c_ws_string (this, gmsgid);
}

/* Pretty-print an IDENTIFIER_NODE, which may contain UTF-8 sequences
   that need converting to the locale encoding, preceded by whitespace
   is necessary.  */

void
pp_c_identifier (c_pretty_printer *pp, const char *id)
{
  pp_c_maybe_whitespace (pp);
  pp_identifier (pp, id);
  pp->set_padding (pp_before);
}

/* Pretty-print a C primary-expression.
   primary-expression:
      identifier
      constant
      string-literal
      ( expression )   */

void
c_pretty_printer::primary_expression (tree e)
{
  switch (TREE_CODE (e))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case LABEL_DECL:
      pp_c_tree_decl_identifier (this, e);
      break;

    case IDENTIFIER_NODE:
      pp_c_tree_identifier (this, e);
      break;

    case ERROR_MARK:
      translate_string ("<erroneous-expression>");
      break;

    case RESULT_DECL:
      translate_string ("<return-value>");
      break;

    case VOID_CST:
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
      constant (e);
      break;

    case TARGET_EXPR:
      pp_c_ws_string (this, "__builtin_memcpy");
      pp_c_left_paren (this);
      pp_ampersand (this);
      primary_expression (TARGET_EXPR_SLOT (e));
      pp_separate_with (this, ',');
      pp_ampersand (this);
      initializer (TARGET_EXPR_INITIAL (e));
      if (TARGET_EXPR_CLEANUP (e))
	{
	  pp_separate_with (this, ',');
	  expression (TARGET_EXPR_CLEANUP (e));
	}
      pp_c_right_paren (this);
      break;

    case SSA_NAME:
      if (SSA_NAME_VAR (e))
	{
	  tree var = SSA_NAME_VAR (e);
	  if (tree id = SSA_NAME_IDENTIFIER (e))
	    {
	      const char *name = IDENTIFIER_POINTER (id);
	      const char *dot;
	      if (DECL_ARTIFICIAL (var) && (dot = strchr (name, '.')))
		{
		  /* Print the name without the . suffix (such as in VLAs).
		     Use pp_c_identifier so that it can be converted into
		     the appropriate encoding.  */
		  size_t size = dot - name;
		  char *ident = XALLOCAVEC (char, size + 1);
		  memcpy (ident, name, size);
		  ident[size] = '\0';
		  pp_c_identifier (this, ident);
		}
	      else
		primary_expression (var);
	    }
	  else
	    primary_expression (var);
	}
      else if (gimple_assign_single_p (SSA_NAME_DEF_STMT (e)))
	{
	  /* Print only the right side of the GIMPLE assignment.  */
	  gimple *def_stmt = SSA_NAME_DEF_STMT (e);
	  pp_gimple_stmt_1 (this, def_stmt, 0, TDF_RHS_ONLY);
	}
      else
	expression (e);
      break;

    default:
      /* FIXME:  Make sure we won't get into an infinite loop.  */
      if (location_wrapper_p (e))
	expression (e);
      else
	{
	  pp_c_left_paren (this);
	  expression (e);
	  pp_c_right_paren (this);
	}
      break;
    }
}

/* Print out a C initializer -- also support C compound-literals.
   initializer:
      assignment-expression:
      { initializer-list }
      { initializer-list , }   */

void
c_pretty_printer::initializer (tree e)
{
  if (TREE_CODE (e) == CONSTRUCTOR)
    pp_c_brace_enclosed_initializer_list (this, e);
  else
    expression (e);
}

/* init-declarator:
      declarator:
      declarator = initializer   */

void
pp_c_init_declarator (c_pretty_printer *pp, tree t)
{
  pp->declarator (t);
  /* We don't want to output function definitions here.  There are handled
     elsewhere (and the syntactic form is bogus anyway).  */
  if (TREE_CODE (t) != FUNCTION_DECL && DECL_INITIAL (t))
    {
      tree init = DECL_INITIAL (t);
      /* This C++ bit is handled here because it is easier to do so.
	 In templates, the C++ parser builds a TREE_LIST for a
	 direct-initialization; the TREE_PURPOSE is the variable to
	 initialize and the TREE_VALUE is the initializer.  */
      if (TREE_CODE (init) == TREE_LIST)
	{
	  pp_c_left_paren (pp);
	  pp->expression (TREE_VALUE (init));
	  pp_right_paren (pp);
	}
      else
	{
	  pp_space (pp);
	  pp_equal (pp);
	  pp_space (pp);
	  pp->initializer (init);
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

  if (TREE_CODE (e) == CONSTRUCTOR)
    {
      pp_c_constructor_elts (pp, CONSTRUCTOR_ELTS (e));
      return;
    }

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
		pp->primary_expression (TREE_PURPOSE (init));
	      }
	    else
	      {
		pp_c_left_bracket (pp);
		if (TREE_PURPOSE (init))
		  pp->constant (TREE_PURPOSE (init));
		pp_c_right_bracket (pp);
	      }
	    pp_c_whitespace (pp);
	    pp_equal (pp);
	    pp_c_whitespace (pp);
	    pp->initializer (TREE_VALUE (init));
	    if (TREE_CHAIN (init))
	      pp_separate_with (pp, ',');
	  }
      }
      return;

    case VECTOR_TYPE:
      if (TREE_CODE (e) == VECTOR_CST)
	{
	  /* We don't create variable-length VECTOR_CSTs.  */
	  unsigned int nunits = VECTOR_CST_NELTS (e).to_constant ();
	  for (unsigned int i = 0; i < nunits; ++i)
	    {
	      if (i > 0)
		pp_separate_with (pp, ',');
	      pp->expression (VECTOR_CST_ELT (e, i));
	    }
	}
      else
	break;
      return;

    case COMPLEX_TYPE:
      if (TREE_CODE (e) == COMPLEX_CST || TREE_CODE (e) == COMPLEX_EXPR)
	{
	  const bool cst = TREE_CODE (e) == COMPLEX_CST;
	  pp->expression (cst ? TREE_REALPART (e) : TREE_OPERAND (e, 0));
	  pp_separate_with (pp, ',');
	  pp->expression (cst ? TREE_IMAGPART (e) : TREE_OPERAND (e, 1));
	}
      else
	break;
      return;

    default:
      break;
    }

  pp_unsupported_tree (pp, type);
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
c_pretty_printer::id_expression (tree t)
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
      pp_c_tree_decl_identifier (this, t);
      break;

    case IDENTIFIER_NODE:
      pp_c_tree_identifier (this, t);
      break;

    default:
      pp_unsupported_tree (this, t);
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
c_pretty_printer::postfix_expression (tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      postfix_expression (TREE_OPERAND (e, 0));
      pp_string (this, code == POSTINCREMENT_EXPR ? "++" : "--");
      break;

    case ARRAY_REF:
      postfix_expression (TREE_OPERAND (e, 0));
      pp_c_left_bracket (this);
      expression (TREE_OPERAND (e, 1));
      pp_c_right_bracket (this);
      break;

    case OMP_ARRAY_SECTION:
      postfix_expression (TREE_OPERAND (e, 0));
      pp_c_left_bracket (this);
      if (TREE_OPERAND (e, 1))
	expression (TREE_OPERAND (e, 1));
      pp_colon (this);
      if (TREE_OPERAND (e, 2))
	expression (TREE_OPERAND (e, 2));
      pp_c_right_bracket (this);
      break;

    case CALL_EXPR:
      {
	call_expr_arg_iterator iter;
	tree arg;
	if (CALL_EXPR_FN (e) != NULL_TREE)
	  postfix_expression (CALL_EXPR_FN (e));
	else
	  pp_string (this, internal_fn_name (CALL_EXPR_IFN (e)));
	pp_c_left_paren (this);
	FOR_EACH_CALL_EXPR_ARG (arg, iter, e)
	  {
	    expression (arg);
	    if (more_call_expr_args_p (&iter))
	      pp_separate_with (this, ',');
	  }
	pp_c_right_paren (this);
	break;
      }

    case UNORDERED_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "isunordered"
			   : "__builtin_isunordered");
      goto two_args_fun;

    case ORDERED_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!isunordered"
			   : "!__builtin_isunordered");
      goto two_args_fun;

    case UNLT_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!isgreaterequal"
			   : "!__builtin_isgreaterequal");
      goto two_args_fun;

    case UNLE_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!isgreater"
			   : "!__builtin_isgreater");
      goto two_args_fun;

    case UNGT_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!islessequal"
			   : "!__builtin_islessequal");
      goto two_args_fun;

    case UNGE_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!isless"
			   : "!__builtin_isless");
      goto two_args_fun;

    case UNEQ_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "!islessgreater"
			   : "!__builtin_islessgreater");
      goto two_args_fun;

    case LTGT_EXPR:
      pp_c_ws_string (this, flag_isoc99
			   ? "islessgreater"
			   : "__builtin_islessgreater");
      goto two_args_fun;

    case MAX_EXPR:
      pp_c_ws_string (this, "max");
      goto two_args_fun;

    case MIN_EXPR:
      pp_c_ws_string (this, "min");
      goto two_args_fun;

    two_args_fun:
      pp_c_left_paren (this);
      expression (TREE_OPERAND (e, 0));
      pp_separate_with (this, ',');
      expression (TREE_OPERAND (e, 1));
      pp_c_right_paren (this);
      break;

    case ABS_EXPR:
      pp_c_ws_string (this, "__builtin_abs");
      pp_c_left_paren (this);
      expression (TREE_OPERAND (e, 0));
      pp_c_right_paren (this);
      break;

    case COMPONENT_REF:
      {
	tree object = TREE_OPERAND (e, 0);
	if (INDIRECT_REF_P (object))
	  {
	    postfix_expression (TREE_OPERAND (object, 0));
	    pp_c_arrow (this);
	  }
	else
	  {
	    postfix_expression (object);
	    pp_c_dot (this);
	  }
	expression (TREE_OPERAND (e, 1));
      }
      break;

    case BIT_FIELD_REF:
      {
	tree type = TREE_TYPE (e);

	type = signed_or_unsigned_type_for (TYPE_UNSIGNED (type), type);
	if (type
	    && tree_int_cst_equal (TYPE_SIZE (type), TREE_OPERAND (e, 1)))
	  {
	    HOST_WIDE_INT bitpos = tree_to_shwi (TREE_OPERAND (e, 2));
	    HOST_WIDE_INT size = tree_to_shwi (TYPE_SIZE (type));
	    if ((bitpos % size) == 0)
	      {
		pp_c_left_paren (this);
		pp_c_left_paren (this);
		type_id (type);
		pp_c_star (this);
		pp_c_right_paren (this);
		pp_c_ampersand (this);
		expression (TREE_OPERAND (e, 0));
		pp_c_right_paren (this);
		pp_c_left_bracket (this);
		pp_wide_integer (this, bitpos / size);
		pp_c_right_bracket (this);
		break;
	      }
	  }
	pp_unsupported_tree (this, e);
      }
      break;

    case MEM_REF:
    case TARGET_MEM_REF:
      expression (e);
      break;

    case COMPLEX_CST:
    case VECTOR_CST:
      pp_c_compound_literal (this, e);
      break;

    case COMPLEX_EXPR:
      pp_c_complex_expr (this, e);
      break;

    case COMPOUND_LITERAL_EXPR:
      e = DECL_INITIAL (COMPOUND_LITERAL_EXPR_DECL (e));
      /* Fall through.  */
    case CONSTRUCTOR:
      initializer (e);
      break;

    case VA_ARG_EXPR:
      pp_c_ws_string (this, "__builtin_va_arg");
      pp_c_left_paren (this);
      assignment_expression (TREE_OPERAND (e, 0));
      pp_separate_with (this, ',');
      type_id (TREE_TYPE (e));
      pp_c_right_paren (this);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (e, 0)) == FUNCTION_DECL)
	{
          id_expression (TREE_OPERAND (e, 0));
	  break;
	}
      /* fall through.  */

    default:
      primary_expression (e);
      break;
    }
}

/* Print out an expression-list; E is expected to be a TREE_LIST.  */

void
pp_c_expression_list (c_pretty_printer *pp, tree e)
{
  for (; e != NULL_TREE; e = TREE_CHAIN (e))
    {
      pp->expression (TREE_VALUE (e));
      if (TREE_CHAIN (e))
	pp_separate_with (pp, ',');
    }
}

/* Print out V, which contains the elements of a constructor.  */

void
pp_c_constructor_elts (c_pretty_printer *pp, vec<constructor_elt, va_gc> *v)
{
  unsigned HOST_WIDE_INT ix;
  tree value;

  FOR_EACH_CONSTRUCTOR_VALUE (v, ix, value)
    {
      pp->expression (value);
      if (ix != vec_safe_length (v) - 1)
	pp_separate_with (pp, ',');
    }
}

/* Print out an expression-list in parens, as if it were the argument
   list to a function.  */

void
pp_c_call_argument_list (c_pretty_printer *pp, tree t)
{
  pp_c_left_paren (pp);
  if (t && TREE_CODE (t) == TREE_LIST)
    pp_c_expression_list (pp, t);
  pp_c_right_paren (pp);
}

/* Try to fold *(type *)&op into op.fld.fld2[1] if possible.
   Only used for printing expressions.  Should punt if ambiguous
   (e.g. in unions).  */

static tree
c_fold_indirect_ref_for_warn (location_t loc, tree type, tree op,
			      offset_int &off)
{
  tree optype = TREE_TYPE (op);
  if (off == 0)
    {
      if (lang_hooks.types_compatible_p (optype, type))
	return op;
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
	       && lang_hooks.types_compatible_p (type, TREE_TYPE (optype)))
	return build1_loc (loc, REALPART_EXPR, type, op);
    }
  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
  else if (TREE_CODE (optype) == COMPLEX_TYPE
	   && lang_hooks.types_compatible_p (type, TREE_TYPE (optype))
	   && tree_to_uhwi (TYPE_SIZE_UNIT (type)) == off)
    {
      off = 0;
      return build1_loc (loc, IMAGPART_EXPR, type, op);
    }
  /* ((foo *)&fooarray)[x] => fooarray[x] */
  if (TREE_CODE (optype) == ARRAY_TYPE
      && TYPE_SIZE_UNIT (TREE_TYPE (optype))
      && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (optype))) == INTEGER_CST
      && !integer_zerop (TYPE_SIZE_UNIT (TREE_TYPE (optype))))
    {
      tree type_domain = TYPE_DOMAIN (optype);
      tree min_val = size_zero_node;
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      offset_int el_sz = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (optype)));
      offset_int idx = off / el_sz;
      offset_int rem = off % el_sz;
      if (TREE_CODE (min_val) == INTEGER_CST)
	{
	  tree index
	    = wide_int_to_tree (sizetype, idx + wi::to_offset (min_val));
	  op = build4_loc (loc, ARRAY_REF, TREE_TYPE (optype), op, index,
			   NULL_TREE, NULL_TREE);
	  off = rem;
	  if (tree ret = c_fold_indirect_ref_for_warn (loc, type, op, off))
	    return ret;
	  return op;
	}
    }
  /* ((foo *)&struct_with_foo_field)[x] => COMPONENT_REF */
  else if (TREE_CODE (optype) == RECORD_TYPE)
    {
      for (tree field = TYPE_FIELDS (optype);
	   field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_TYPE (field) != error_mark_node
	    && TYPE_SIZE_UNIT (TREE_TYPE (field))
	    && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (field))) == INTEGER_CST)
	  {
	    tree pos = byte_position (field);
	    if (TREE_CODE (pos) != INTEGER_CST)
	      continue;
	    offset_int upos = wi::to_offset (pos);
	    offset_int el_sz
	      = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (field)));
	    if (upos <= off && off < upos + el_sz)
	      {
		/* The C++ pretty printers print scope of the FIELD_DECLs,
		   so punt if it is something that can't be printed.  */
		if (c_dialect_cxx ())
		  if (tree scope = get_containing_scope (field))
		    if (TYPE_P (scope) && TYPE_NAME (scope) == NULL_TREE)
		      break;
		tree cop = build3_loc (loc, COMPONENT_REF, TREE_TYPE (field),
				       op, field, NULL_TREE);
		off = off - upos;
		if (tree ret = c_fold_indirect_ref_for_warn (loc, type, cop,
							     off))
		  return ret;
		return cop;
	      }
	  }
    }
  /* Similarly for unions, but in this case try to be very conservative,
     only match if some field has type compatible with type and it is the
     only such field.  */
  else if (TREE_CODE (optype) == UNION_TYPE)
    {
      tree fld = NULL_TREE;
      for (tree field = TYPE_FIELDS (optype);
	   field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_TYPE (field) != error_mark_node
	    && lang_hooks.types_compatible_p (TREE_TYPE (field), type))
	  {
	    if (fld)
	      return NULL_TREE;
	    else
	      fld = field;
	  }
      if (fld)
	{
	  off = 0;
	  return build3_loc (loc, COMPONENT_REF, TREE_TYPE (fld), op, fld,
			     NULL_TREE);
	}
    }

  return NULL_TREE;
}

/* Print the MEM_REF expression REF, including its type and offset.
   Apply casts as necessary if the type of the access is different
   from the type of the accessed object.  Produce compact output
   designed to include both the element index as well as any
   misalignment by preferring
     ((int*)((char*)p + 1))[2]
   over
     *(int*)((char*)p + 9)
   The former is more verbose but makes it clearer that the access
   to the third element of the array is misaligned by one byte.  */

static void
print_mem_ref (c_pretty_printer *pp, tree e)
{
  tree arg = TREE_OPERAND (e, 0);

  /* The byte offset.  Initially equal to the MEM_REF offset, then
     adjusted to the remainder of the division by the byte size of
     the access.  */
  offset_int byte_off = wi::to_offset (TREE_OPERAND (e, 1));
  /* The result of dividing BYTE_OFF by the size of the access.  */
  offset_int elt_idx = 0;
  /* True to include a cast to char* (for a nonzero final BYTE_OFF).  */
  bool char_cast = false;
  tree op = NULL_TREE;
  bool array_ref_only = false;
  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      op = c_fold_indirect_ref_for_warn (EXPR_LOCATION (e), TREE_TYPE (e),
					 TREE_OPERAND (arg, 0), byte_off);
      /* Try to fold it back to component, array ref or their combination,
	 but print it only if the types and TBAA types are compatible.  */
      if (op
	  && byte_off == 0
	  && lang_hooks.types_compatible_p (TREE_TYPE (e), TREE_TYPE (op))
	  && (!flag_strict_aliasing
	      || (get_deref_alias_set (TREE_OPERAND (e, 1))
		  == get_alias_set (op))))
	{
	  pp->expression (op);
	  return;
	}
      if (op == NULL_TREE)
	op = TREE_OPERAND (arg, 0);
      /* If the types or TBAA types are incompatible, undo the
	 UNION_TYPE handling from c_fold_indirect_ref_for_warn, and similarly
	 undo __real__/__imag__ the code below doesn't try to handle.  */
      if (op != TREE_OPERAND (arg, 0)
	  && ((TREE_CODE (op) == COMPONENT_REF
	       && TREE_CODE (TREE_TYPE (TREE_OPERAND (op, 0))) == UNION_TYPE)
	      || TREE_CODE (op) == REALPART_EXPR
	      || TREE_CODE (op) == IMAGPART_EXPR))
	op = TREE_OPERAND (op, 0);
      if (op != TREE_OPERAND (arg, 0))
	{
	  array_ref_only = true;
	  for (tree ref = op; ref != TREE_OPERAND (arg, 0);
	       ref = TREE_OPERAND (ref, 0))
	    if (TREE_CODE (ref) != ARRAY_REF)
	      {
		array_ref_only = false;
		break;
	      }
	}
    }

  tree access_type = TREE_TYPE (e);
  tree arg_type = TREE_TYPE (TREE_TYPE (arg));
  if (tree access_size = TYPE_SIZE_UNIT (access_type))
    if (byte_off != 0
	&& TREE_CODE (access_size) == INTEGER_CST
	&& !integer_zerop (access_size))
      {
	offset_int asize = wi::to_offset (access_size);
	elt_idx = byte_off / asize;
	byte_off = byte_off % asize;
      }

  /* True to include a cast to the accessed type.  */
  const bool access_cast
    = ((op && op != TREE_OPERAND (arg, 0))
       || VOID_TYPE_P (arg_type)
       || !lang_hooks.types_compatible_p (access_type, arg_type));
  const bool has_off = byte_off != 0 || (op && op != TREE_OPERAND (arg, 0));

  if (has_off && (byte_off != 0 || !array_ref_only))
    {
      /* When printing the byte offset for a pointer to a type of
	 a different size than char, include a cast to char* first,
	 before printing the cast to a pointer to the accessed type.  */
      tree size = TYPE_SIZE (arg_type);
      if (size == NULL_TREE
	  || TREE_CODE (size) != INTEGER_CST
	  || wi::to_wide (size) != BITS_PER_UNIT)
	char_cast = true;
    }

  if (elt_idx == 0)
    pp_c_star (pp);
  else if (access_cast || char_cast)
    pp_c_left_paren (pp);

  if (access_cast)
    {
      /* Include a cast to the accessed type if it isn't compatible
	 with the type of the referenced object (or if the object
	 is typeless).  */
      pp_c_left_paren (pp);
      pp->type_id (build_pointer_type (access_type));
      pp_c_right_paren (pp);
    }

  if (has_off)
    pp_c_left_paren (pp);

  if (char_cast)
    {
      /* Include a cast to char *.  */
      pp_c_left_paren (pp);
      pp->type_id (string_type_node);
      pp_c_right_paren (pp);
    }

  pp->unary_expression (arg);

  if (op && op != TREE_OPERAND (arg, 0))
    {
      auto_vec<tree, 16> refs;
      tree ref;
      unsigned i;
      bool array_refs = true;
      for (ref = op; ref != TREE_OPERAND (arg, 0); ref = TREE_OPERAND (ref, 0))
	refs.safe_push (ref);
      FOR_EACH_VEC_ELT_REVERSE (refs, i, ref)
	if (array_refs && TREE_CODE (ref) == ARRAY_REF)
	  {
	    pp_c_left_bracket (pp);
	    pp->expression (TREE_OPERAND (ref, 1));
	    pp_c_right_bracket (pp);
	  }
	else
	  {
	    if (array_refs)
	      {
		array_refs = false;
		pp_string (pp, " + offsetof");
		pp_c_left_paren (pp);
		pp->type_id (TREE_TYPE (TREE_OPERAND (ref, 0)));
		pp_comma (pp);
	      }
	    else if (TREE_CODE (ref) == COMPONENT_REF)
	      pp_c_dot (pp);
	    if (TREE_CODE (ref) == COMPONENT_REF)
	      pp->expression (TREE_OPERAND (ref, 1));
	    else
	      {
		pp_c_left_bracket (pp);
		pp->expression (TREE_OPERAND (ref, 1));
		pp_c_right_bracket (pp);
	      }
	  }
      if (!array_refs)
	pp_c_right_paren (pp);
    }

  if (byte_off != 0)
    {
      pp_space (pp);
      pp_plus (pp);
      pp_space (pp);
      tree off = wide_int_to_tree (ssizetype, byte_off);
      pp->constant (off);
    }

  if (has_off)
    pp_c_right_paren (pp);

  if (elt_idx != 0)
    {
      if (access_cast || char_cast)
	pp_c_right_paren (pp);

      pp_c_left_bracket (pp);
      tree idx = wide_int_to_tree (ssizetype, elt_idx);
      pp->constant (idx);
      pp_c_right_bracket (pp);
    }
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
c_pretty_printer::unary_expression (tree e)
{
  enum tree_code code = TREE_CODE (e);
  switch (code)
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      pp_string (this, code == PREINCREMENT_EXPR ? "++" : "--");
      unary_expression (TREE_OPERAND (e, 0));
      break;

    case ADDR_EXPR:
    case INDIRECT_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case CONJ_EXPR:
      /* String literal are used by address.  */
      if (code == ADDR_EXPR && TREE_CODE (TREE_OPERAND (e, 0)) != STRING_CST)
	pp_ampersand (this);
      else if (code == INDIRECT_REF)
	{
	  tree type = TREE_TYPE (TREE_OPERAND (e, 0));
	  if (type && TREE_CODE (type) == REFERENCE_TYPE)
	    /* Reference decay is implicit, don't print anything.  */;
	  else
	    pp_c_star (this);
	}
      else if (code == NEGATE_EXPR)
	pp_minus (this);
      else if (code == BIT_NOT_EXPR || code == CONJ_EXPR)
	pp_complement (this);
      else if (code == TRUTH_NOT_EXPR)
	pp_exclamation (this);
      pp_c_cast_expression (this, TREE_OPERAND (e, 0));
      break;

    case MEM_REF:
      print_mem_ref (this, e);
      break;

    case TARGET_MEM_REF:
      /* TARGET_MEM_REF can't appear directly from source, but can appear
	 during late GIMPLE optimizations and through late diagnostic we might
	 need to support it.  Print it as dereferencing of a pointer after
	 cast to the TARGET_MEM_REF type, with pointer arithmetics on some
	 pointer to single byte types, so
	 *(type *)((char *) ptr + step * index + index2) if all the operands
	 are present and the casts are needed.  */
      pp_c_star (this);
      if (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (TMR_BASE (e)))) == NULL_TREE
	  || !integer_onep (TYPE_SIZE_UNIT
				(TREE_TYPE (TREE_TYPE (TMR_BASE (e))))))
	{
	  if (TYPE_SIZE_UNIT (TREE_TYPE (e))
	      && integer_onep (TYPE_SIZE_UNIT (TREE_TYPE (e))))
	    {
	      pp_c_left_paren (this);
	      pp_c_type_cast (this, build_pointer_type (TREE_TYPE (e)));
	    }
	  else
	    {
	      pp_c_type_cast (this, build_pointer_type (TREE_TYPE (e)));
	      pp_c_left_paren (this);
	      pp_c_type_cast (this, build_pointer_type (char_type_node));
	    }
	}
      else if (!lang_hooks.types_compatible_p
		  (TREE_TYPE (e), TREE_TYPE (TREE_TYPE (TMR_BASE (e)))))
	{
	  pp_c_type_cast (this, build_pointer_type (TREE_TYPE (e)));
	  pp_c_left_paren (this);
	}
      else
	pp_c_left_paren (this);
      pp_c_cast_expression (this, TMR_BASE (e));
      if (TMR_STEP (e) && TMR_INDEX (e))
	{
	  pp_plus (this);
	  pp_c_cast_expression (this, TMR_INDEX (e));
	  pp_c_star (this);
	  pp_c_cast_expression (this, TMR_STEP (e));
	}
      if (TMR_INDEX2 (e))
	{
	  pp_plus (this);
	  pp_c_cast_expression (this, TMR_INDEX2 (e));
	}
      if (!integer_zerop (TMR_OFFSET (e)))
	{
	  pp_plus (this);
	  pp_c_integer_constant (this,
				 fold_convert (ssizetype, TMR_OFFSET (e)));
	}
      pp_c_right_paren (this);
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      pp_c_ws_string (this, code == REALPART_EXPR ? "__real__" : "__imag__");
      pp_c_whitespace (this);
      unary_expression (TREE_OPERAND (e, 0));
      break;

    default:
      postfix_expression (e);
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
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
    case EXCESS_PRECISION_EXPR:
      if (!location_wrapper_p (e))
	pp_c_type_cast (pp, TREE_TYPE (e));
      pp_c_cast_expression (pp, TREE_OPERAND (e, 0));
      break;

    default:
      pp->unary_expression (e);
    }
}

/* multiplicative-expression:
      cast-expression
      multiplicative-expression * cast-expression
      multiplicative-expression / cast-expression
      multiplicative-expression % cast-expression   */

void
c_pretty_printer::multiplicative_expression (tree e)
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
      pp_c_whitespace (this);
      if (code == MULT_EXPR)
	pp_c_star (this);
      else if (code != TRUNC_MOD_EXPR)
	pp_slash (this);
      else
	pp_modulo (this);
      pp_c_whitespace (this);
      pp_c_cast_expression (this, TREE_OPERAND (e, 1));
      break;

    default:
      pp_c_cast_expression (this, e);
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
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      pp_c_additive_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      if (code == PLUS_EXPR || code == POINTER_PLUS_EXPR)
	pp_plus (pp);
      else
	pp_minus (pp);
      pp_c_whitespace (pp);
      {
	tree op1 = TREE_OPERAND (e, 1);
	if (code == POINTER_PLUS_EXPR
	    && TREE_CODE (op1) == INTEGER_CST
	    && tree_int_cst_sign_bit (op1))
	  /* A pointer minus an integer is represented internally as plus a very
	     large number, don't expose that to users.  */
	  op1 = convert (ssizetype, op1);
	pp->multiplicative_expression (op1);
      }
      break;

    default:
      pp->multiplicative_expression (e);
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
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      pp_c_shift_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_string (pp, code == LSHIFT_EXPR ? "<<" :
		     code == RSHIFT_EXPR ? ">>" :
		     code == LROTATE_EXPR ? "<<<" : ">>>");
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
	pp_less_equal (pp);
      else if (code == GE_EXPR)
	pp_greater_equal (pp);
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
      pp_string (pp, code == EQ_EXPR ? "==" : "!=");
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
  if (TREE_CODE (e) == BIT_XOR_EXPR
      || TREE_CODE (e) == TRUTH_XOR_EXPR)
    {
      pp_c_exclusive_or_expression (pp, TREE_OPERAND (e, 0));
      if (TREE_CODE (e) == BIT_XOR_EXPR)
	pp_c_maybe_whitespace (pp);
      else
	pp_c_whitespace (pp);
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
  if (TREE_CODE (e) == TRUTH_ANDIF_EXPR
      || TREE_CODE (e) == TRUTH_AND_EXPR)
    {
      pp_c_logical_and_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_ampersand_ampersand (pp);
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
  if (TREE_CODE (e) == TRUTH_ORIF_EXPR
      || TREE_CODE (e) == TRUTH_OR_EXPR)
    {
      pp_c_logical_or_expression (pp, TREE_OPERAND (e, 0));
      pp_c_whitespace (pp);
      pp_bar_bar (pp);
      pp_c_whitespace (pp);
      pp_c_logical_and_expression (pp, TREE_OPERAND (e, 1));
    }
  else
    pp_c_logical_and_expression (pp, e);
}

/* conditional-expression:
      logical-OR-expression
      logical-OR-expression ? expression : conditional-expression  */

void
c_pretty_printer::conditional_expression (tree e)
{
  if (TREE_CODE (e) == COND_EXPR)
    {
      pp_c_logical_or_expression (this, TREE_OPERAND (e, 0));
      pp_c_whitespace (this);
      pp_question (this);
      pp_c_whitespace (this);
      expression (TREE_OPERAND (e, 1));
      pp_c_whitespace (this);
      pp_colon (this);
      pp_c_whitespace (this);
      conditional_expression (TREE_OPERAND (e, 2));
    }
  else
    pp_c_logical_or_expression (this, e);
}


/* assignment-expression:
      conditional-expression
      unary-expression assignment-operator  assignment-expression

   assignment-expression: one of
      =    *=    /=    %=    +=    -=    >>=    <<=    &=    ^=    |=  */

void
c_pretty_printer::assignment_expression (tree e)
{
  if (TREE_CODE (e) == MODIFY_EXPR
      || TREE_CODE (e) == INIT_EXPR)
    {
      unary_expression (TREE_OPERAND (e, 0));
      pp_c_whitespace (this);
      pp_equal (this);
      pp_space (this);
      expression (TREE_OPERAND (e, 1));
    }
  else
    conditional_expression (e);
}

/* expression:
       assignment-expression
       expression , assignment-expression

  Implementation note:  instead of going through the usual recursion
  chain, I take the liberty of dispatching nodes to the appropriate
  functions.  This makes some redundancy, but it worths it. That also
  prevents a possible infinite recursion between primary_expression ()
  and expression ().  */

void
c_pretty_printer::expression (tree e)
{
  switch (TREE_CODE (e))
    {
    case VOID_CST:
      pp_c_void_constant (this);
      break;

    case INTEGER_CST:
      pp_c_integer_constant (this, e);
      break;

    case REAL_CST:
      pp_c_floating_constant (this, e);
      break;

    case FIXED_CST:
      pp_c_fixed_constant (this, e);
      break;

    case STRING_CST:
      pp_c_string_literal (this, e);
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
      primary_expression (e);
      break;

    case SSA_NAME:
      if (SSA_NAME_VAR (e)
	  && !DECL_ARTIFICIAL (SSA_NAME_VAR (e)))
	expression (SSA_NAME_VAR (e));
      else
	translate_string ("<unknown>");
      break;

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case ARRAY_REF:
    case OMP_ARRAY_SECTION:
    case CALL_EXPR:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case COMPLEX_CST:
    case COMPLEX_EXPR:
    case VECTOR_CST:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case LTGT_EXPR:
    case UNEQ_EXPR:
    case UNLE_EXPR:
    case UNLT_EXPR:
    case UNGE_EXPR:
    case UNGT_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case ABS_EXPR:
    case CONSTRUCTOR:
    case COMPOUND_LITERAL_EXPR:
    case VA_ARG_EXPR:
      postfix_expression (e);
      break;

    case CONJ_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case MEM_REF:
    case TARGET_MEM_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      unary_expression (e);
      break;

    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
    case EXCESS_PRECISION_EXPR:
      pp_c_cast_expression (this, e);
      break;

    case MULT_EXPR:
    case TRUNC_MOD_EXPR:
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      multiplicative_expression (e);
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      pp_c_shift_expression (this, e);
      break;

    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      pp_c_relational_expression (this, e);
      break;

    case BIT_AND_EXPR:
      pp_c_and_expression (this, e);
      break;

    case BIT_XOR_EXPR:
    case TRUTH_XOR_EXPR:
      pp_c_exclusive_or_expression (this, e);
      break;

    case BIT_IOR_EXPR:
      pp_c_inclusive_or_expression (this, e);
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_AND_EXPR:
      pp_c_logical_and_expression (this, e);
      break;

    case TRUTH_ORIF_EXPR:
    case TRUTH_OR_EXPR:
      pp_c_logical_or_expression (this, e);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      pp_c_equality_expression (this, e);
      break;

    case COND_EXPR:
      conditional_expression (e);
      break;

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      pp_c_additive_expression (this, e);
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      assignment_expression (e);
      break;

    case COMPOUND_EXPR:
      pp_c_left_paren (this);
      expression (TREE_OPERAND (e, 0));
      pp_separate_with (this, ',');
      assignment_expression (TREE_OPERAND (e, 1));
      pp_c_right_paren (this);
      break;

    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      expression (TREE_OPERAND (e, 0));
      break;

    case TARGET_EXPR:
      postfix_expression (TARGET_EXPR_INITIAL (e));
      break;

    case BIND_EXPR:
    case GOTO_EXPR:
      /* We don't yet have a way of dumping statements in a
         human-readable format.  */
      pp_string (this, "({...})");
      break;

    case C_MAYBE_CONST_EXPR:
      expression (C_MAYBE_CONST_EXPR_EXPR (e));
      break;

    default:
      pp_unsupported_tree (this, e);
      break;
    }
}



/* Statements.  */

void
c_pretty_printer::statement (tree t)
{
  if (t == NULL)
    return;

  switch (TREE_CODE (t))
    {

    case SWITCH_STMT:
      pp_c_ws_string (this, "switch");
      pp_space (this);
      pp_c_left_paren (this);
      expression (SWITCH_STMT_COND (t));
      pp_c_right_paren (this);
      pp_indentation (this) += 3;
      pp_needs_newline (this) = true;
      statement (SWITCH_STMT_BODY (t));
      pp_newline_and_indent (this, -3);
      break;

      /* iteration-statement:
	    while ( expression ) statement
	    do statement while ( expression ) ;
	    for ( expression(opt) ; expression(opt) ; expression(opt) ) statement
	    for ( declaration expression(opt) ; expression(opt) ) statement  */
    case WHILE_STMT:
      pp_c_ws_string (this, "while");
      pp_space (this);
      pp_c_left_paren (this);
      expression (WHILE_COND (t));
      pp_c_right_paren (this);
      pp_newline_and_indent (this, 3);
      statement (WHILE_BODY (t));
      pp_indentation (this) -= 3;
      pp_needs_newline (this) = true;
      break;

    case DO_STMT:
      pp_c_ws_string (this, "do");
      pp_newline_and_indent (this, 3);
      statement (DO_BODY (t));
      pp_newline_and_indent (this, -3);
      pp_c_ws_string (this, "while");
      pp_space (this);
      pp_c_left_paren (this);
      expression (DO_COND (t));
      pp_c_right_paren (this);
      pp_c_semicolon (this);
      pp_needs_newline (this) = true;
      break;

    case FOR_STMT:
      pp_c_ws_string (this, "for");
      pp_space (this);
      pp_c_left_paren (this);
      if (FOR_INIT_STMT (t))
	statement (FOR_INIT_STMT (t));
      else
	pp_c_semicolon (this);
      pp_needs_newline (this) = false;
      pp_c_whitespace (this);
      if (FOR_COND (t))
	expression (FOR_COND (t));
      pp_c_semicolon (this);
      pp_needs_newline (this) = false;
      pp_c_whitespace (this);
      if (FOR_EXPR (t))
	expression (FOR_EXPR (t));
      pp_c_right_paren (this);
      pp_newline_and_indent (this, 3);
      statement (FOR_BODY (t));
      pp_indentation (this) -= 3;
      pp_needs_newline (this) = true;
      break;

      /* jump-statement:
	    goto identifier;
	    continue ;
	    return expression(opt) ;  */
    case BREAK_STMT:
      pp_string (this, "break");
      if (BREAK_NAME (t))
	{
	  pp_space (this);
	  pp_c_tree_decl_identifier (this, BREAK_NAME (t));
	}
      pp_c_semicolon (this);
      pp_needs_newline (this) = true;
      break;

    case CONTINUE_STMT:
      pp_string (this, "continue");
      if (CONTINUE_NAME (t))
	{
	  pp_space (this);
	  pp_c_tree_decl_identifier (this, CONTINUE_NAME (t));
	}
      pp_c_semicolon (this);
      pp_needs_newline (this) = true;
      break;

    default:
      if (pp_needs_newline (this))
	pp_newline_and_indent (this, 0);
      dump_generic_node (this, t, pp_indentation (this), TDF_NONE, true);
    }
}


/* Initialize the PRETTY-PRINTER for handling C codes.  */

c_pretty_printer::c_pretty_printer ()
  : pretty_printer (),
    offset_list (),
    flags ()
{
  type_specifier_seq        = pp_c_specifier_qualifier_list;
  ptr_operator              = pp_c_pointer;
  parameter_list            = pp_c_parameter_type_list;
}

/* c_pretty_printer's implementation of pretty_printer::clone vfunc.  */

std::unique_ptr<pretty_printer>
c_pretty_printer::clone () const
{
  return ::make_unique<c_pretty_printer> (*this);
}

/* Print the tree T in full, on file FILE.  */

void
print_c_tree (FILE *file, tree t)
{
  c_pretty_printer pp;

  pp_needs_newline (&pp) = true;
  pp.set_output_stream (file);
  pp.statement (t);
  pp_newline_and_flush (&pp);
}

/* Print the tree T in full, on stderr.  */

DEBUG_FUNCTION void
debug_c_tree (tree t)
{
  print_c_tree (stderr, t);
  fputc ('\n', stderr);
}

/* Output the DECL_NAME of T.  If T has no DECL_NAME, output a string made
   up of T's memory address.  */

void
pp_c_tree_decl_identifier (c_pretty_printer *pp, tree t)
{
  const char *name;

  gcc_assert (DECL_P (t));

  if (DECL_NAME (t))
    name = IDENTIFIER_POINTER (DECL_NAME (t));
  else
    {
      static char xname[8];
      sprintf (xname, "<U%4hx>", ((unsigned short) ((uintptr_t) (t)
						    & 0xffff)));
      name = xname;
    }

  pp_c_identifier (pp, name);
}

#if CHECKING_P

namespace selftest {

/* Selftests for pretty-printing trees.  */

/* Verify that EXPR printed by c_pretty_printer is EXPECTED, using
   LOC as the effective location for any failures.  */

static void
assert_c_pretty_printer_output (const location &loc, const char *expected,
				tree expr)
{
  c_pretty_printer pp;
  pp.expression (expr);
  ASSERT_STREQ_AT (loc, expected, pp_formatted_text (&pp));
}

/* Helper function for calling assert_c_pretty_printer_output.
   This is to avoid having to write SELFTEST_LOCATION.  */

#define ASSERT_C_PRETTY_PRINTER_OUTPUT(EXPECTED, EXPR) \
  SELFTEST_BEGIN_STMT						\
    assert_c_pretty_printer_output ((SELFTEST_LOCATION),	\
				    (EXPECTED),		\
				    (EXPR));			\
  SELFTEST_END_STMT

/* Verify that location wrappers don't show up in pretty-printed output.  */

static void
test_location_wrappers ()
{
  /* VAR_DECL.  */
  tree id = get_identifier ("foo");
  tree decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, id,
			  integer_type_node);
  tree wrapped_decl = maybe_wrap_with_location (decl, BUILTINS_LOCATION);
  ASSERT_NE (wrapped_decl, decl);
  ASSERT_C_PRETTY_PRINTER_OUTPUT ("foo", decl);
  ASSERT_C_PRETTY_PRINTER_OUTPUT ("foo", wrapped_decl);

  /* INTEGER_CST.  */
  tree int_cst = build_int_cst (integer_type_node, 42);
  tree wrapped_cst = maybe_wrap_with_location (int_cst, BUILTINS_LOCATION);
  ASSERT_NE (wrapped_cst, int_cst);
  ASSERT_C_PRETTY_PRINTER_OUTPUT ("42", int_cst);
  ASSERT_C_PRETTY_PRINTER_OUTPUT ("42", wrapped_cst);
}

/* Run all of the selftests within this file.  */

void
c_pretty_print_cc_tests ()
{
  test_location_wrappers ();
}

} // namespace selftest

#endif /* CHECKING_P */
