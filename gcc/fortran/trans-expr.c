/* Expression translation
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-expr.c-- generate GENERIC trees for gfc_expr.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "convert.h"
#include <stdio.h>
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "tree-gimple.h"
#include "flags.h"
#include <gmp.h>
#include <assert.h>
#include "gfortran.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
/* Only for gfc_trans_assign and gfc_trans_pointer_assign.  */
#include "trans-stmt.h"


/* Copy the scalarization loop variables.  */

static void
gfc_copy_se_loopvars (gfc_se * dest, gfc_se * src)
{
  dest->ss = src->ss;
  dest->loop = src->loop;
}


/* Initialise a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */

void
gfc_init_se (gfc_se * se, gfc_se * parent)
{
  memset (se, 0, sizeof (gfc_se));
  gfc_init_block (&se->pre);
  gfc_init_block (&se->post);

  se->parent = parent;

  if (parent)
    gfc_copy_se_loopvars (se, parent);
}


/* Advances to the next SS in the chain.  Use this rather than setting
   se->ss = se->ss->next because all the parent needs to be kept in sync.
   See gfc_init_se.  */

void
gfc_advance_se_ss_chain (gfc_se * se)
{
  gfc_se *p;

  assert (se != NULL && se->ss != NULL && se->ss != gfc_ss_terminator);

  p = se;
  /* Walk down the parent chain.  */
  while (p != NULL)
    {
      /* Simple consistancy check.  */
      assert (p->parent == NULL || p->parent->ss == p->ss);

      p->ss = p->ss->next;

      p = p->parent;
    }
}


/* Ensures the result of the expression as either a temporary variable
   or a constant so that it can be used repeatedly.  */

void
gfc_make_safe_expr (gfc_se * se)
{
  tree var;

  if (TREE_CODE_CLASS (TREE_CODE (se->expr)) == 'c')
    return;

  /* we need a temporary for this result */
  var = gfc_create_var (TREE_TYPE (se->expr), NULL);
  gfc_add_modify_expr (&se->pre, var, se->expr);
  se->expr = var;
}


/* Return an expression which determines if a dummy parameter is present.  */

tree
gfc_conv_expr_present (gfc_symbol * sym)
{
  tree decl;

  assert (sym->attr.dummy && sym->attr.optional);

  decl = gfc_get_symbol_decl (sym);
  if (TREE_CODE (decl) != PARM_DECL)
    {
      /* Array parameters use a temporary descriptor, we want the real
         parameter.  */
      assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
             || GFC_ARRAY_TYPE_P (TREE_TYPE (decl)));
      decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
    }
  return build (NE_EXPR, boolean_type_node, decl,
		fold_convert (TREE_TYPE (decl), null_pointer_node));
}


/* Generate code to initialize a string length variable. Returns the
   value.  */

void
gfc_trans_init_string_length (gfc_charlen * cl, stmtblock_t * pblock)
{
  gfc_se se;
  tree tmp;

  gfc_init_se (&se, NULL);
  gfc_conv_expr_type (&se, cl->length, gfc_strlen_type_node);
  gfc_add_block_to_block (pblock, &se.pre);

  tmp = cl->backend_decl;
  gfc_add_modify_expr (pblock, tmp, se.expr);
}

static void
gfc_conv_substring (gfc_se * se, gfc_ref * ref, int kind)
{
  tree tmp;
  tree type;
  tree var;
  gfc_se start;
  gfc_se end;

  type = gfc_get_character_type (kind, ref->u.ss.length);
  type = build_pointer_type (type);

  var = NULL_TREE;
  gfc_init_se (&start, se);
  gfc_conv_expr_type (&start, ref->u.ss.start, gfc_strlen_type_node);
  gfc_add_block_to_block (&se->pre, &start.pre);

  if (integer_onep (start.expr))
    gfc_conv_string_parameter (se);
  else
    {
      /* Change the start of the string.  */
      if (TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
	tmp = se->expr;
      else
	tmp = gfc_build_indirect_ref (se->expr);
      tmp = gfc_build_array_ref (tmp, start.expr);
      se->expr = gfc_build_addr_expr (type, tmp);
    }

  /* Length = end + 1 - start.  */
  gfc_init_se (&end, se);
  if (ref->u.ss.end == NULL)
    end.expr = se->string_length;
  else
    {
      gfc_conv_expr_type (&end, ref->u.ss.end, gfc_strlen_type_node);
      gfc_add_block_to_block (&se->pre, &end.pre);
    }
  tmp =
    build (MINUS_EXPR, gfc_strlen_type_node,
	   fold_convert (gfc_strlen_type_node, integer_one_node),
	   start.expr);
  tmp = build (PLUS_EXPR, gfc_strlen_type_node, end.expr, tmp);
  se->string_length = fold (tmp);
}


/* Convert a derived type component reference.  */

static void
gfc_conv_component_ref (gfc_se * se, gfc_ref * ref)
{
  gfc_component *c;
  tree tmp;
  tree decl;
  tree field;

  c = ref->u.c.component;

  assert (c->backend_decl);

  field = c->backend_decl;
  assert (TREE_CODE (field) == FIELD_DECL);
  decl = se->expr;
  tmp = build (COMPONENT_REF, TREE_TYPE (field), decl, field, NULL_TREE);

  se->expr = tmp;

  if (c->ts.type == BT_CHARACTER)
    {
      tmp = c->ts.cl->backend_decl;
      assert (tmp);
      if (!INTEGER_CST_P (tmp))
	gfc_todo_error ("Unknown length character component");
      se->string_length = tmp;
    }

  if (c->pointer && c->dimension == 0)
    se->expr = gfc_build_indirect_ref (se->expr);
}


/* Return the contents of a variable. Also handles reference/pointer
   variables (all Fortran pointer references are implicit).  */

static void
gfc_conv_variable (gfc_se * se, gfc_expr * expr)
{
  gfc_ref *ref;
  gfc_symbol *sym;

  sym = expr->symtree->n.sym;
  if (se->ss != NULL)
    {
      /* Check that something hasn't gone horribly wrong.  */
      assert (se->ss != gfc_ss_terminator);
      assert (se->ss->expr == expr);

      /* A scalarized term.  We already know the descriptor.  */
      se->expr = se->ss->data.info.descriptor;
      ref = se->ss->data.info.ref;
    }
  else
    {
      se->expr = gfc_get_symbol_decl (sym);

      /* Procedure actual arguments.  */
      if (sym->attr.flavor == FL_PROCEDURE
	  && se->expr != current_function_decl)
	{
	  assert (se->want_pointer);
	  if (!sym->attr.dummy)
	    {
	      assert (TREE_CODE (se->expr) == FUNCTION_DECL);
	      se->expr = gfc_build_addr_expr (NULL, se->expr);
	    }
	  return;
	}

      /* Special case for assigning the return value of a function.
         Self recursive functions must have an explicit return value.  */
      if (se->expr == current_function_decl && sym->attr.function
	  && (sym->result == sym))
	{
	  se->expr = gfc_get_fake_result_decl (sym);
	}

      /* Dereference scalar dummy variables.  */
      if (sym->attr.dummy
	  && sym->ts.type != BT_CHARACTER
	  && !sym->attr.dimension)
	se->expr = gfc_build_indirect_ref (se->expr);

      /* Dereference pointer variables.  */
      if ((sym->attr.pointer || sym->attr.allocatable)
	  && (sym->attr.dummy
	      || sym->attr.result
	      || sym->attr.function
	      || !sym->attr.dimension)
          && sym->ts.type != BT_CHARACTER)
	se->expr = gfc_build_indirect_ref (se->expr);

      ref = expr->ref;
    }

  /* For character variables, also get the length.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      se->string_length = sym->ts.cl->backend_decl;
      assert (se->string_length);
    }

  while (ref)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  /* Return the descriptor if that's what we want and this is an array
	     section reference.  */
	  if (se->descriptor_only && ref->u.ar.type != AR_ELEMENT)
	    return;
/* TODO: Pointers to single elements of array sections, eg elemental subs.  */
	  /* Return the descriptor for array pointers and allocations.  */
	  if (se->want_pointer
	      && ref->next == NULL && (se->descriptor_only))
	    return;

	  gfc_conv_array_ref (se, &ref->u.ar);
	  /* Return a pointer to an element.  */
	  break;

	case REF_COMPONENT:
	  gfc_conv_component_ref (se, ref);
	  break;

	case REF_SUBSTRING:
	  gfc_conv_substring (se, ref, expr->ts.kind);
	  break;

	default:
	  abort ();
	  break;
	}
      ref = ref->next;
    }
  /* Pointer assignment, allocation or pass by reference.  Arrays are handled
     seperately.  */
  if (se->want_pointer)
    {
      if (expr->ts.type == BT_CHARACTER)
	gfc_conv_string_parameter (se);
      else 
	se->expr = gfc_build_addr_expr (NULL, se->expr);
    }
  if (se->ss != NULL)
    gfc_advance_se_ss_chain (se);
}


/* Unary ops are easy... Or they would be if ! was a valid op.  */

static void
gfc_conv_unary_op (enum tree_code code, gfc_se * se, gfc_expr * expr)
{
  gfc_se operand;
  tree type;

  assert (expr->ts.type != BT_CHARACTER);
  /* Initialize the operand.  */
  gfc_init_se (&operand, se);
  gfc_conv_expr_val (&operand, expr->op1);
  gfc_add_block_to_block (&se->pre, &operand.pre);

  type = gfc_typenode_for_spec (&expr->ts);

  /* TRUTH_NOT_EXPR is not a "true" unary operator in GCC.
     We must convert it to a compare to 0 (e.g. EQ_EXPR (op1, 0)).
     All other unary operators have an equivalent GIMPLE unary operator  */
  if (code == TRUTH_NOT_EXPR)
    se->expr = build (EQ_EXPR, type, operand.expr,
		      convert (type, integer_zero_node));
  else
    se->expr = build1 (code, type, operand.expr);

}

/* Expand power operator to optimal multiplications when a value is raised
   to an constant integer n. See section 4.6.3, "Evaluation of Powers" of
   Donald E. Knuth, "Seminumerical Algorithms", Vol. 2, "The Art of Computer
   Programming", 3rd Edition, 1998.  */

/* This code is mostly duplicated from expand_powi in the backend.
   We establish the "optimal power tree" lookup table with the defined size.
   The items in the table are the exponents used to calculate the index
   exponents. Any integer n less than the value can get an "addition chain",
   with the first node being one.  */
#define POWI_TABLE_SIZE 256

/* The table is from Builtins.c.  */
static const unsigned char powi_table[POWI_TABLE_SIZE] =
  {
      0,   1,   1,   2,   2,   3,   3,   4,  /*   0 -   7 */
      4,   6,   5,   6,   6,  10,   7,   9,  /*   8 -  15 */
      8,  16,   9,  16,  10,  12,  11,  13,  /*  16 -  23 */
     12,  17,  13,  18,  14,  24,  15,  26,  /*  24 -  31 */
     16,  17,  17,  19,  18,  33,  19,  26,  /*  32 -  39 */
     20,  25,  21,  40,  22,  27,  23,  44,  /*  40 -  47 */
     24,  32,  25,  34,  26,  29,  27,  44,  /*  48 -  55 */
     28,  31,  29,  34,  30,  60,  31,  36,  /*  56 -  63 */
     32,  64,  33,  34,  34,  46,  35,  37,  /*  64 -  71 */
     36,  65,  37,  50,  38,  48,  39,  69,  /*  72 -  79 */
     40,  49,  41,  43,  42,  51,  43,  58,  /*  80 -  87 */
     44,  64,  45,  47,  46,  59,  47,  76,  /*  88 -  95 */
     48,  65,  49,  66,  50,  67,  51,  66,  /*  96 - 103 */
     52,  70,  53,  74,  54, 104,  55,  74,  /* 104 - 111 */
     56,  64,  57,  69,  58,  78,  59,  68,  /* 112 - 119 */
     60,  61,  61,  80,  62,  75,  63,  68,  /* 120 - 127 */
     64,  65,  65, 128,  66, 129,  67,  90,  /* 128 - 135 */
     68,  73,  69, 131,  70,  94,  71,  88,  /* 136 - 143 */
     72, 128,  73,  98,  74, 132,  75, 121,  /* 144 - 151 */
     76, 102,  77, 124,  78, 132,  79, 106,  /* 152 - 159 */
     80,  97,  81, 160,  82,  99,  83, 134,  /* 160 - 167 */
     84,  86,  85,  95,  86, 160,  87, 100,  /* 168 - 175 */
     88, 113,  89,  98,  90, 107,  91, 122,  /* 176 - 183 */
     92, 111,  93, 102,  94, 126,  95, 150,  /* 184 - 191 */
     96, 128,  97, 130,  98, 133,  99, 195,  /* 192 - 199 */
    100, 128, 101, 123, 102, 164, 103, 138,  /* 200 - 207 */
    104, 145, 105, 146, 106, 109, 107, 149,  /* 208 - 215 */
    108, 200, 109, 146, 110, 170, 111, 157,  /* 216 - 223 */
    112, 128, 113, 130, 114, 182, 115, 132,  /* 224 - 231 */
    116, 200, 117, 132, 118, 158, 119, 206,  /* 232 - 239 */
    120, 240, 121, 162, 122, 147, 123, 152,  /* 240 - 247 */
    124, 166, 125, 214, 126, 138, 127, 153,  /* 248 - 255 */
  };

/* If n is larger than lookup table's max index, we use "window method".  */
#define POWI_WINDOW_SIZE 3

/* Recursive function to expand power operator. The temporary values are put
   in tmpvar. The function return tmpvar[1] ** n.  */
static tree
gfc_conv_powi (gfc_se * se, int n, tree * tmpvar)
{
  tree op0;
  tree op1;
  tree tmp;
  int digit;

  if (n < POWI_TABLE_SIZE)
    {
      if (tmpvar[n])
        return tmpvar[n];

      op0 = gfc_conv_powi (se, n - powi_table[n], tmpvar);
      op1 = gfc_conv_powi (se, powi_table[n], tmpvar);
    }
  else if (n & 1)
    {
      digit = n & ((1 << POWI_WINDOW_SIZE) - 1);
      op0 = gfc_conv_powi (se, n - digit, tmpvar);
      op1 = gfc_conv_powi (se, digit, tmpvar);
    }
  else
    {
      op0 = gfc_conv_powi (se, n >> 1, tmpvar);
      op1 = op0;
    }

  tmp = fold (build (MULT_EXPR, TREE_TYPE (op0), op0, op1));
  tmp = gfc_evaluate_now (tmp, &se->pre);

  if (n < POWI_TABLE_SIZE)
    tmpvar[n] = tmp;

  return tmp;
}

/* Expand lhs ** rhs. rhs is an constant integer. If expand successfully,
   return 1. Else return 0 and will call runtime library functions.  */
static int
gfc_conv_cst_int_power (gfc_se * se, tree lhs, tree rhs)
{
  tree cond;
  tree tmp;
  tree type;
  tree vartmp[POWI_TABLE_SIZE];
  int n;
  int sgn;

  type = TREE_TYPE (lhs);
  n = abs (TREE_INT_CST_LOW (rhs));
  sgn = tree_int_cst_sgn (rhs);

  if ((!flag_unsafe_math_optimizations || optimize_size) && (n > 2 || n < -1))
    return 0;

  /* rhs == 0  */
  if (sgn == 0)
    {
      se->expr = gfc_build_const (type, integer_one_node);
      return 1;
    }
  /* If rhs < 0 and lhs is an integer, the result is -1, 0 or 1.  */
  if ((sgn == -1) && (TREE_CODE (type) == INTEGER_TYPE))
    {
      tmp = build (EQ_EXPR, boolean_type_node, lhs,
		   fold_convert (TREE_TYPE (lhs), integer_minus_one_node));
      cond = build (EQ_EXPR, boolean_type_node, lhs,
		    convert (TREE_TYPE (lhs), integer_one_node));

      /* If rhs is an even,
	 result = (lhs == 1 || lhs == -1) ? 1 : 0.  */
      if ((n & 1) == 0)
        {
	  tmp = build (TRUTH_OR_EXPR, boolean_type_node, tmp, cond);
	  se->expr = build (COND_EXPR, type, tmp,
			    convert (type, integer_one_node),
			    convert (type, integer_zero_node));
	  return 1;
	}
      /* If rhs is an odd,
	 result = (lhs == 1) ? 1 : (lhs == -1) ? -1 : 0.  */
      tmp = build (COND_EXPR, type, tmp,
		   convert (type, integer_minus_one_node),
		   convert (type, integer_zero_node));
      se->expr = build (COND_EXPR, type, cond,
			convert (type, integer_one_node),
			tmp);
      return 1;
    }

  memset (vartmp, 0, sizeof (vartmp));
  vartmp[1] = lhs;
  if (sgn == -1)
    {
      tmp = gfc_build_const (type, integer_one_node);
      vartmp[1] = build (RDIV_EXPR, type, tmp, vartmp[1]);
    }

  se->expr = gfc_conv_powi (se, n, vartmp);

  return 1;
}


/* Power op (**).  Constant integer exponent has special handling.  */

static void
gfc_conv_power_op (gfc_se * se, gfc_expr * expr)
{
  int kind;
  int ikind;
  gfc_se lse;
  gfc_se rse;
  tree fndecl;
  tree tmp;

  gfc_init_se (&lse, se);
  gfc_conv_expr_val (&lse, expr->op1);
  gfc_add_block_to_block (&se->pre, &lse.pre);

  gfc_init_se (&rse, se);
  gfc_conv_expr_val (&rse, expr->op2);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  if (expr->op2->ts.type == BT_INTEGER
	 && expr->op2->expr_type == EXPR_CONSTANT)
    if (gfc_conv_cst_int_power (se, lse.expr, rse.expr))
      return;        

  kind = expr->op1->ts.kind;
  switch (expr->op2->ts.type)
    {
    case BT_INTEGER:
      ikind = expr->op2->ts.kind;
      switch (ikind)
	{
	case 1:
	case 2:
	  rse.expr = convert (gfc_int4_type_node, rse.expr);
	  /* Fall through.  */

	case 4:
	  ikind = 0;
	  break;
	  
	case 8:
	  ikind = 1;
	  break;

	default:
	  abort();
	}
      switch (kind)
	{
	case 1:
	case 2:
	  if (expr->op1->ts.type == BT_INTEGER)
	    lse.expr = convert (gfc_int4_type_node, lse.expr);
	  else
	    abort ();
	  /* Fall through.  */

	case 4:
	  kind = 0;
	  break;
	  
	case 8:
	  kind = 1;
	  break;

	default:
	  abort();
	}
      
      switch (expr->op1->ts.type)
	{
	case BT_INTEGER:
	  fndecl = gfor_fndecl_math_powi[kind][ikind].integer;
	  break;

	case BT_REAL:
	  fndecl = gfor_fndecl_math_powi[kind][ikind].real;
	  break;

	case BT_COMPLEX:
	  fndecl = gfor_fndecl_math_powi[kind][ikind].cmplx;
	  break;

	default:
	  abort ();
 	}
      break;

    case BT_REAL:
      switch (kind)
	{
	case 4:
	  fndecl = built_in_decls[BUILT_IN_POWF];
	  break;
	case 8:
	  fndecl = built_in_decls[BUILT_IN_POW];
	  break;
	default:
	  abort ();
	}
      break;

    case BT_COMPLEX:
      switch (kind)
	{
	case 4:
	  fndecl = gfor_fndecl_math_cpowf;
	  break;
	case 8:
	  fndecl = gfor_fndecl_math_cpow;
	  break;
	default:
	  abort ();
	}
      break;

    default:
      abort ();
      break;
    }

  tmp = gfc_chainon_list (NULL_TREE, lse.expr);
  tmp = gfc_chainon_list (tmp, rse.expr);
  se->expr = fold (gfc_build_function_call (fndecl, tmp));
}


/* Generate code to allocate a string temporary.  */

tree
gfc_conv_string_tmp (gfc_se * se, tree type, tree len)
{
  tree var;
  tree tmp;
  tree args;

  if (TREE_TYPE (len) != gfc_strlen_type_node)
    abort ();

  if (gfc_can_put_var_on_stack (len))
    {
      /* Create a temporary variable to hold the result.  */
      tmp = fold (build (MINUS_EXPR, gfc_strlen_type_node, len,
			 convert (gfc_strlen_type_node,
				  integer_one_node)));
      tmp = build_range_type (gfc_array_index_type, gfc_index_zero_node, tmp);
      tmp = build_array_type (gfc_character1_type_node, tmp);
      var = gfc_create_var (tmp, "str");
      var = gfc_build_addr_expr (type, var);
    }
  else
    {
      /* Allocate a temporary to hold the result.  */
      var = gfc_create_var (type, "pstr");
      args = gfc_chainon_list (NULL_TREE, len);
      tmp = gfc_build_function_call (gfor_fndecl_internal_malloc, args);
      tmp = convert (type, tmp);
      gfc_add_modify_expr (&se->pre, var, tmp);

      /* Free the temporary afterwards.  */
      tmp = convert (pvoid_type_node, var);
      args = gfc_chainon_list (NULL_TREE, tmp);
      tmp = gfc_build_function_call (gfor_fndecl_internal_free, args);
      gfc_add_expr_to_block (&se->post, tmp);
    }

  return var;
}


/* Handle a string concatenation operation.  A temporary will be allocated to
   hold the result.  */

static void
gfc_conv_concat_op (gfc_se * se, gfc_expr * expr)
{
  gfc_se lse;
  gfc_se rse;
  tree len;
  tree type;
  tree var;
  tree args;
  tree tmp;

  assert (expr->op1->ts.type == BT_CHARACTER
	  && expr->op2->ts.type == BT_CHARACTER);

  gfc_init_se (&lse, se);
  gfc_conv_expr (&lse, expr->op1);
  gfc_conv_string_parameter (&lse);
  gfc_init_se (&rse, se);
  gfc_conv_expr (&rse, expr->op2);
  gfc_conv_string_parameter (&rse);

  gfc_add_block_to_block (&se->pre, &lse.pre);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  type = gfc_get_character_type (expr->ts.kind, expr->ts.cl);
  len = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
  if (len == NULL_TREE)
    {
      len = fold (build (PLUS_EXPR, TREE_TYPE (lse.string_length),
			 lse.string_length, rse.string_length));
    }

  type = build_pointer_type (type);

  var = gfc_conv_string_tmp (se, type, len);

  /* Do the actual concatenation.  */
  args = NULL_TREE;
  args = gfc_chainon_list (args, len);
  args = gfc_chainon_list (args, var);
  args = gfc_chainon_list (args, lse.string_length);
  args = gfc_chainon_list (args, lse.expr);
  args = gfc_chainon_list (args, rse.string_length);
  args = gfc_chainon_list (args, rse.expr);
  tmp = gfc_build_function_call (gfor_fndecl_concat_string, args);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Add the cleanup for the operands.  */
  gfc_add_block_to_block (&se->pre, &rse.post);
  gfc_add_block_to_block (&se->pre, &lse.post);

  se->expr = var;
  se->string_length = len;
}


/* Translates an op expression. Common (binary) cases are handled by this
   function, others are passed on. Recursion is used in either case.
   We use the fact that (op1.ts == op2.ts) (except for the power
   operand **).
   Operators need no special handling for scalarized expressions as long as
   they call gfc_conv_siple_val to get their operands.
   Character strings get special handling.  */

static void
gfc_conv_expr_op (gfc_se * se, gfc_expr * expr)
{
  enum tree_code code;
  gfc_se lse;
  gfc_se rse;
  tree type;
  tree tmp;
  int lop;
  int checkstring;

  checkstring = 0;
  lop = 0;
  switch (expr->operator)
    {
    case INTRINSIC_UPLUS:
      gfc_conv_expr (se, expr->op1);
      return;

    case INTRINSIC_UMINUS:
      gfc_conv_unary_op (NEGATE_EXPR, se, expr);
      return;

    case INTRINSIC_NOT:
      gfc_conv_unary_op (TRUTH_NOT_EXPR, se, expr);
      return;

    case INTRINSIC_PLUS:
      code = PLUS_EXPR;
      break;

    case INTRINSIC_MINUS:
      code = MINUS_EXPR;
      break;

    case INTRINSIC_TIMES:
      code = MULT_EXPR;
      break;

    case INTRINSIC_DIVIDE:
      /* If expr is a real or complex expr, use an RDIV_EXPR. If op1 is
         an integer, we must round towards zero, so we use a
         TRUNC_DIV_EXPR.  */
      if (expr->ts.type == BT_INTEGER)
	code = TRUNC_DIV_EXPR;
      else
	code = RDIV_EXPR;
      break;

    case INTRINSIC_POWER:
      gfc_conv_power_op (se, expr);
      return;

    case INTRINSIC_CONCAT:
      gfc_conv_concat_op (se, expr);
      return;

    case INTRINSIC_AND:
      code = TRUTH_ANDIF_EXPR;
      lop = 1;
      break;

    case INTRINSIC_OR:
      code = TRUTH_ORIF_EXPR;
      lop = 1;
      break;

      /* EQV and NEQV only work on logicals, but since we represent them
         as integers, we can use EQ_EXPR and NE_EXPR for them in GIMPLE.  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQV:
      code = EQ_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NEQV:
      code = NE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GT:
      code = GT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GE:
      code = GE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LT:
      code = LT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LE:
      code = LE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_USER:
    case INTRINSIC_ASSIGN:
      /* These should be converted into function calls by the frontend.  */
      abort ();
      return;

    default:
      fatal_error ("Unknown intrinsic op");
      return;
    }

  /* The only exception to this is **, which is handled seperately anyway.  */
  assert (expr->op1->ts.type == expr->op2->ts.type);

  if (checkstring && expr->op1->ts.type != BT_CHARACTER)
    checkstring = 0;

  /* lhs */
  gfc_init_se (&lse, se);
  gfc_conv_expr (&lse, expr->op1);
  gfc_add_block_to_block (&se->pre, &lse.pre);

  /* rhs */
  gfc_init_se (&rse, se);
  gfc_conv_expr (&rse, expr->op2);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  /* For string comparisons we generate a library call, and compare the return
     value with 0.  */
  if (checkstring)
    {
      gfc_conv_string_parameter (&lse);
      gfc_conv_string_parameter (&rse);
      tmp = NULL_TREE;
      tmp = gfc_chainon_list (tmp, lse.string_length);
      tmp = gfc_chainon_list (tmp, lse.expr);
      tmp = gfc_chainon_list (tmp, rse.string_length);
      tmp = gfc_chainon_list (tmp, rse.expr);

      /* Build a call for the comparison.  */
      lse.expr = gfc_build_function_call (gfor_fndecl_compare_string, tmp);
      gfc_add_block_to_block (&lse.post, &rse.post);

      rse.expr = integer_zero_node;
    }

  type = gfc_typenode_for_spec (&expr->ts);

  if (lop)
    {
      /* The result of logical ops is always boolean_type_node.  */
      tmp = fold (build (code, type, lse.expr, rse.expr));
      se->expr = convert (type, tmp);
    }
  else
    se->expr = fold (build (code, type, lse.expr, rse.expr));


  /* Add the post blocks.  */
  gfc_add_block_to_block (&se->post, &rse.post);
  gfc_add_block_to_block (&se->post, &lse.post);
}

static void
gfc_conv_function_val (gfc_se * se, gfc_symbol * sym)
{
  tree tmp;

  if (sym->attr.dummy)
    {
      tmp = gfc_get_symbol_decl (sym);
      assert (TREE_CODE (TREE_TYPE (tmp)) == POINTER_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (tmp))) == FUNCTION_TYPE);

      se->expr = tmp;
    }
  else
    {
      if (!sym->backend_decl)
	sym->backend_decl = gfc_get_extern_function_decl (sym);

      tmp = sym->backend_decl;
      assert (TREE_CODE (tmp) == FUNCTION_DECL);
      se->expr = gfc_build_addr_expr (NULL, tmp);
    }
}


/* Generate code for a procedure call.  Note can return se->post != NULL.
   If se->direct_byref is set then se->expr contains the return parameter.  */

void
gfc_conv_function_call (gfc_se * se, gfc_symbol * sym,
			gfc_actual_arglist * arg)
{
  tree arglist;
  tree tmp;
  tree fntype;
  gfc_se parmse;
  gfc_ss *argss;
  gfc_ss_info *info;
  int byref;
  tree type;
  tree var;
  tree len;
  tree stringargs;
  gfc_formal_arglist *formal;

  arglist = NULL_TREE;
  stringargs = NULL_TREE;
  var = NULL_TREE;
  len = NULL_TREE;

  if (se->ss != NULL)
    {
      if (!sym->attr.elemental)
	{
	  assert (se->ss->type == GFC_SS_FUNCTION);
          if (se->ss->useflags)
            {
              assert (gfc_return_by_reference (sym)
                      && sym->result->attr.dimension);
              assert (se->loop != NULL);

              /* Access the previously obtained result.  */
              gfc_conv_tmp_array_ref (se);
              gfc_advance_se_ss_chain (se);
              return;
            }
	}
      info = &se->ss->data.info;
    }
  else
    info = NULL;

  byref = gfc_return_by_reference (sym);
  if (byref)
    {
      if (se->direct_byref)
	arglist = gfc_chainon_list (arglist, se->expr);
      else if (sym->result->attr.dimension)
	{
	  assert (se->loop && se->ss);
	  /* Set the type of the array.  */
	  tmp = gfc_typenode_for_spec (&sym->ts);
	  info->dimen = se->loop->dimen;
	  /* Allocate a temporary to store the result.  */
	  gfc_trans_allocate_temp_array (se->loop, info, tmp, NULL_TREE);

	  /* Zero the first stride to indicate a temporary.  */
	  tmp =
	    gfc_conv_descriptor_stride (info->descriptor, gfc_rank_cst[0]);
	  gfc_add_modify_expr (&se->pre, tmp,
			       convert (TREE_TYPE (tmp), integer_zero_node));
	  /* Pass the temporary as the first argument.  */
	  tmp = info->descriptor;
	  tmp = gfc_build_addr_expr (NULL, tmp);
	  arglist = gfc_chainon_list (arglist, tmp);
	}
      else if (sym->ts.type == BT_CHARACTER)
	{
	  assert (sym->ts.cl && sym->ts.cl->length
		  && sym->ts.cl->length->expr_type == EXPR_CONSTANT);
	  len = gfc_conv_mpz_to_tree
	    (sym->ts.cl->length->value.integer, sym->ts.cl->length->ts.kind);
	  sym->ts.cl->backend_decl = len;
	  type = gfc_get_character_type (sym->ts.kind, sym->ts.cl);
	  type = build_pointer_type (type);

	  var = gfc_conv_string_tmp (se, type, len);
	  arglist = gfc_chainon_list (arglist, var);
	  arglist = gfc_chainon_list (arglist, convert (gfc_strlen_type_node,
							len));
	}
      else      /* TODO: derived type function return values.  */
	abort ();
    }

  formal = sym->formal;
  /* Evaluate the arguments.  */
  for (; arg != NULL; arg = arg->next, formal = formal ? formal->next : NULL)
    {
      if (arg->expr == NULL)
	{

	  if (se->ignore_optional)
	    {
	      /* Some intrinsics have already been resolved to the correct
	         parameters.  */
	      continue;
	    }
	  else if (arg->label)
	    {
              has_alternate_specifier = 1;
              continue;
	    }
	  else
	    {
	      /* Pass a NULL pointer for an absent arg.  */
	      gfc_init_se (&parmse, NULL);
	      parmse.expr = null_pointer_node;
              if (arg->missing_arg_type == BT_CHARACTER)
                {
                  stringargs =
		    gfc_chainon_list (stringargs,
				      convert (gfc_strlen_type_node,
					       integer_zero_node));
                }
	    }
	}
      else if (se->ss && se->ss->useflags)
	{
	  /* An elemental function inside a scalarized loop.  */
          gfc_init_se (&parmse, se);
          gfc_conv_expr_reference (&parmse, arg->expr);
	}
      else
	{
	  /* A scalar or transformational function.  */
	  gfc_init_se (&parmse, NULL);
	  argss = gfc_walk_expr (arg->expr);

	  if (argss == gfc_ss_terminator)
            {
	      gfc_conv_expr_reference (&parmse, arg->expr);
              if (formal && formal->sym->attr.pointer
		  && arg->expr->expr_type != EXPR_NULL)
                {
                  /* Scalar pointer dummy args require an extra level of
                     indirection. The null pointer already contains
		     this level of indirection.  */
                  parmse.expr = gfc_build_addr_expr (NULL, parmse.expr);
                }
            }
	  else
	    {
	      /* If the procedure requires explicit interface, actual argument
	         is passed according to corresponing formal argument.  We
		 do not use g77 method and the address of array descriptor
		 is passed if corresponing formal is pointer or
		 assumed-shape,  Otherwise use g77 method.  */
	      int f;
	      f = (formal != NULL)
		  && !formal->sym->attr.pointer
		  && formal->sym->as->type != AS_ASSUMED_SHAPE;
	      f = f || !sym->attr.always_explicit;
	      gfc_conv_array_parameter (&parmse, arg->expr, argss, f);
	    } 
	}

      gfc_add_block_to_block (&se->pre, &parmse.pre);
      gfc_add_block_to_block (&se->post, &parmse.post);

      /* Character strings are passed as two paramarers, a length and a
         pointer.  */
      if (parmse.string_length != NULL_TREE)
        stringargs = gfc_chainon_list (stringargs, parmse.string_length);

      arglist = gfc_chainon_list (arglist, parmse.expr);
    }

  /* Add the hidden string length parameters to the arguments.  */
  arglist = chainon (arglist, stringargs);

  /* Generate the actual call.  */
  gfc_conv_function_val (se, sym);
  /* If there are alternate return labels, function type should be
     integer.  */
  if (has_alternate_specifier)
    TREE_TYPE (TREE_TYPE (TREE_TYPE (se->expr))) = integer_type_node;

  fntype = TREE_TYPE (TREE_TYPE (se->expr));
  se->expr = build (CALL_EXPR, TREE_TYPE (fntype), se->expr,
		    arglist, NULL_TREE);

/* A pure function may still have side-effects - it may modify its
   parameters.  */
  TREE_SIDE_EFFECTS (se->expr) = 1;
#if 0
  if (!sym->attr.pure)
    TREE_SIDE_EFFECTS (se->expr) = 1;
#endif

  if (byref && !se->direct_byref)
    {
      gfc_add_expr_to_block (&se->pre, se->expr);

      if (sym->result->attr.dimension)
	{
	  if (flag_bounds_check)
	    {
	      /* Check the data pointer hasn't been modified.  This would happen
	         in a function returning a pointer.  */
	      tmp = gfc_conv_descriptor_data (info->descriptor);
	      tmp = build (NE_EXPR, boolean_type_node, tmp, info->data);
	      gfc_trans_runtime_check (tmp, gfc_strconst_fault, &se->pre);
	    }
	  se->expr = info->descriptor;
	}
      else if (sym->ts.type == BT_CHARACTER)
	{
	  se->expr = var;
	  se->string_length = len;
	}
      else
	abort ();
    }
}


/* Generate code to copy a string.  */

static void
gfc_trans_string_copy (stmtblock_t * block, tree dlen, tree dest,
		       tree slen, tree src)
{
  tree tmp;

  tmp = NULL_TREE;
  tmp = gfc_chainon_list (tmp, dlen);
  tmp = gfc_chainon_list (tmp, dest);
  tmp = gfc_chainon_list (tmp, slen);
  tmp = gfc_chainon_list (tmp, src);
  tmp = gfc_build_function_call (gfor_fndecl_copy_string, tmp);
  gfc_add_expr_to_block (block, tmp);
}


/* Translate a statement function.
   The value of a statement function reference is obtained by evaluating the
   expression using the values of the actual arguments for the values of the
   corresponding dummy arguments.  */

static void
gfc_conv_statement_function (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;
  gfc_symbol *fsym;
  gfc_formal_arglist *fargs;
  gfc_actual_arglist *args;
  gfc_se lse;
  gfc_se rse;
  gfc_saved_var *saved_vars;
  tree *temp_vars;
  tree type;
  tree tmp;
  int n;

  sym = expr->symtree->n.sym;
  args = expr->value.function.actual;
  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  n = 0;
  for (fargs = sym->formal; fargs; fargs = fargs->next)
    n++;
  saved_vars = (gfc_saved_var *)gfc_getmem (n * sizeof (gfc_saved_var));
  temp_vars = (tree *)gfc_getmem (n * sizeof (tree));

  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    {
      /* Each dummy shall be specified, explicitly or implicitly, to be
         scalar.  */
      assert (fargs->sym->attr.dimension == 0);
      fsym = fargs->sym;

      /* Create a temporary to hold the value.  */
      type = gfc_typenode_for_spec (&fsym->ts);
      temp_vars[n] = gfc_create_var (type, fsym->name);

      if (fsym->ts.type == BT_CHARACTER)
        {
	  /* Copy string arguments.  */
          tree arglen;

          assert (fsym->ts.cl && fsym->ts.cl->length
                  && fsym->ts.cl->length->expr_type == EXPR_CONSTANT);

          arglen = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
          tmp = gfc_build_addr_expr (build_pointer_type (type),
				     temp_vars[n]);

          gfc_conv_expr (&rse, args->expr);
          gfc_conv_string_parameter (&rse);
          gfc_add_block_to_block (&se->pre, &lse.pre);
          gfc_add_block_to_block (&se->pre, &rse.pre);

	  gfc_trans_string_copy (&se->pre, arglen, tmp, rse.string_length,
				 rse.expr);
          gfc_add_block_to_block (&se->pre, &lse.post);
          gfc_add_block_to_block (&se->pre, &rse.post);
        }
      else
        {
          /* For everything else, just evaluate the expression.  */
          gfc_conv_expr (&lse, args->expr);

          gfc_add_block_to_block (&se->pre, &lse.pre);
          gfc_add_modify_expr (&se->pre, temp_vars[n], lse.expr);
          gfc_add_block_to_block (&se->pre, &lse.post);
        }

      args = args->next;
    }

  /* Use the temporary variables in place of the real ones.  */
  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    gfc_shadow_sym (fargs->sym, temp_vars[n], &saved_vars[n]);

  gfc_conv_expr (se, sym->value);

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_conv_const_charlen (sym->ts.cl);

      /* Force the expression to the correct length.  */
      if (!INTEGER_CST_P (se->string_length)
	  || tree_int_cst_lt (se->string_length,
			      sym->ts.cl->backend_decl))
	{
	  type = gfc_get_character_type (sym->ts.kind, sym->ts.cl);
	  tmp = gfc_create_var (type, sym->name);
	  tmp = gfc_build_addr_expr (build_pointer_type (type), tmp);
	  gfc_trans_string_copy (&se->pre, sym->ts.cl->backend_decl, tmp,
				 se->string_length, se->expr);
	  se->expr = tmp;
	}
      se->string_length = sym->ts.cl->backend_decl;
    }

  /* Resore the original variables.  */
  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    gfc_restore_sym (fargs->sym, &saved_vars[n]);
  gfc_free (saved_vars);
}


/* Translate a function expression.  */

static void
gfc_conv_function_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;

  if (expr->value.function.isym)
    {
      gfc_conv_intrinsic_function (se, expr);
      return;
    }

  /* We distinguish the statement function from general function to improve
     runtime performance.  */
  if (expr->symtree->n.sym->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_conv_statement_function (se, expr);
      return;
    }

  /* expr.value.function.esym is the resolved (specific) function symbol for
     most functions.  However this isn't set for dummy procedures.  */
  sym = expr->value.function.esym;
  if (!sym)
    sym = expr->symtree->n.sym;
  gfc_conv_function_call (se, sym, expr->value.function.actual);
}

static void
gfc_conv_array_constructor_expr (gfc_se * se, gfc_expr * expr)
{
  assert (se->ss != NULL && se->ss != gfc_ss_terminator);
  assert (se->ss->expr == expr && se->ss->type == GFC_SS_CONSTRUCTOR);

  gfc_conv_tmp_array_ref (se);
  gfc_advance_se_ss_chain (se);
}


/* Build a static initializer.  EXPR is the expression for the initial value.
   The other parameters describe the variable of component being initialized.
   EXPR may be null.  */

tree
gfc_conv_initializer (gfc_expr * expr, gfc_typespec * ts, tree type,
		      bool array, bool pointer)
{
  gfc_se se;

  if (!(expr || pointer))
    return NULL_TREE;

  if (array)
    {
      /* Arrays need special handling.  */
      if (pointer)
	return gfc_build_null_descriptor (type);
      else
	return gfc_conv_array_initializer (type, expr);
    }
  else if (pointer)
    return fold_convert (type, null_pointer_node);
  else
    {
      switch (ts->type)
	{
	case BT_DERIVED:
	  gfc_init_se (&se, NULL);
	  gfc_conv_structure (&se, expr, 1);
	  return se.expr;

	case BT_CHARACTER:
	  return gfc_conv_string_init (ts->cl->backend_decl,expr);

	default:
	  gfc_init_se (&se, NULL);
	  gfc_conv_constant (&se, expr);
	  return se.expr;
	}
    }
}
  
/* Build an expression for a constructor. If init is nonzero then
   this is part of a static variable initializer.  */

void
gfc_conv_structure (gfc_se * se, gfc_expr * expr, int init)
{
  gfc_constructor *c;
  gfc_component *cm;
  tree head;
  tree tail;
  tree val;
  gfc_se cse;
  tree type;

  assert (expr->expr_type == EXPR_STRUCTURE || expr->expr_type == EXPR_NULL);
  type = gfc_typenode_for_spec (&expr->ts);
  head = build1 (CONSTRUCTOR, type, NULL_TREE);
  tail = NULL_TREE;

  cm = expr->ts.derived->components;
  for (c = expr->value.constructor; c; c = c->next, cm = cm->next)
    {
      /* Skip absent members in default initializers.  */
      if (!c->expr)
        continue;

      gfc_init_se (&cse, se);
      /* Evaluate the expression for this component.  */
      if (init)
	{
	  cse.expr = gfc_conv_initializer (c->expr, &cm->ts,
	      TREE_TYPE (cm->backend_decl), cm->dimension, cm->pointer);
	}
      else
	{
	  gfc_conv_expr (&cse, c->expr);
	  gfc_add_block_to_block (&se->pre, &cse.pre);
	  gfc_add_block_to_block (&se->post, &cse.post);
	}

      /* Build a TREE_CHAIN to hold it.  */
      val = tree_cons (cm->backend_decl, cse.expr, NULL_TREE);

      /* Add it to the list.  */
      if (tail == NULL_TREE)
        TREE_OPERAND(head, 0) = tail = val;
      else
        {
          TREE_CHAIN (tail) = val;
          tail = val;
        }
    }
  se->expr = head;
}


/*translate a substring expression */

static void
gfc_conv_substring_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_ref *ref;

  ref = expr->ref;

  assert(ref->type == REF_SUBSTRING);

  se->expr = gfc_build_string_const(expr->value.character.length,
                                    expr->value.character.string);
  se->string_length = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (se->expr)));
  TYPE_STRING_FLAG (TREE_TYPE (se->expr))=1;

  gfc_conv_substring(se,ref,expr->ts.kind);
}


/* Entry point for expression translation.  */

void
gfc_conv_expr (gfc_se * se, gfc_expr * expr)
{
  if (se->ss && se->ss->expr == expr
      && (se->ss->type == GFC_SS_SCALAR || se->ss->type == GFC_SS_REFERENCE))
    {
      /* Substiture a scalar expression evaluated outside the scalarization
         loop.  */
      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      gfc_advance_se_ss_chain (se);
      return;
    }

  switch (expr->expr_type)
    {
    case EXPR_OP:
      gfc_conv_expr_op (se, expr);
      break;

    case EXPR_FUNCTION:
      gfc_conv_function_expr (se, expr);
      break;

    case EXPR_CONSTANT:
      gfc_conv_constant (se, expr);
      break;

    case EXPR_VARIABLE:
      gfc_conv_variable (se, expr);
      break;

    case EXPR_NULL:
      se->expr = null_pointer_node;
      break;

    case EXPR_SUBSTRING:
      gfc_conv_substring_expr (se, expr);
      break;

    case EXPR_STRUCTURE:
      gfc_conv_structure (se, expr, 0);
      break;

    case EXPR_ARRAY:
      gfc_conv_array_constructor_expr (se, expr);
      break;

    default:
      abort ();
      break;
    }
}

void
gfc_conv_expr_lhs (gfc_se * se, gfc_expr * expr)
{
  gfc_conv_expr (se, expr);
  /* AFAICS all numeric lvalues have empty post chains.  If not we need to
     figure out a way of rewriting an lvalue so that it has no post chain.  */
  assert (expr->ts.type != BT_CHARACTER || !se->post.head);
}

void
gfc_conv_expr_val (gfc_se * se, gfc_expr * expr)
{
  tree val;

  assert (expr->ts.type != BT_CHARACTER);
  gfc_conv_expr (se, expr);
  if (se->post.head)
    {
      val = gfc_create_var (TREE_TYPE (se->expr), NULL);
      gfc_add_modify_expr (&se->pre, val, se->expr);
    }
}

void
gfc_conv_expr_type (gfc_se * se, gfc_expr * expr, tree type)
{
  gfc_conv_expr_val (se, expr);
  se->expr = convert (type, se->expr);
}


/* Converts an expression so that it can be passed by refernece.  Scalar
   values only.  */

void
gfc_conv_expr_reference (gfc_se * se, gfc_expr * expr)
{
  tree var;

  if (se->ss && se->ss->expr == expr
      && se->ss->type == GFC_SS_REFERENCE)
    {
      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      gfc_advance_se_ss_chain (se);
      return;
    }

  if (expr->ts.type == BT_CHARACTER)
    {
      gfc_conv_expr (se, expr);
      gfc_conv_string_parameter (se);
      return;
    }

  if (expr->expr_type == EXPR_VARIABLE)
    {
      se->want_pointer = 1;
      gfc_conv_expr (se, expr);
      if (se->post.head)
	{
	  var = gfc_create_var (TREE_TYPE (se->expr), NULL);
	  gfc_add_modify_expr (&se->pre, var, se->expr);
	  gfc_add_block_to_block (&se->pre, &se->post);
	  se->expr = var;
	}
      return;
    }

  gfc_conv_expr (se, expr);

  /* Create a temporary var to hold the value.  */
  var = gfc_create_var (TREE_TYPE (se->expr), NULL);
  gfc_add_modify_expr (&se->pre, var, se->expr);
  gfc_add_block_to_block (&se->pre, &se->post);

  /* Take the address of that value.  */
  se->expr = gfc_build_addr_expr (NULL, var);
}


tree
gfc_trans_pointer_assign (gfc_code * code)
{
  return gfc_trans_pointer_assignment (code->expr, code->expr2);
}


tree
gfc_trans_pointer_assignment (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *rss;
  stmtblock_t block;

  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);

  lss = gfc_walk_expr (expr1);
  rss = gfc_walk_expr (expr2);
  if (lss == gfc_ss_terminator)
    {
      lse.want_pointer = 1;
      gfc_conv_expr (&lse, expr1);
      assert (rss == gfc_ss_terminator);
      gfc_init_se (&rse, NULL);
      rse.want_pointer = 1;
      gfc_conv_expr (&rse, expr2);
      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_block_to_block (&block, &rse.pre);
      gfc_add_modify_expr (&block, lse.expr,
			   fold_convert (TREE_TYPE (lse.expr), rse.expr));
      gfc_add_block_to_block (&block, &rse.post);
      gfc_add_block_to_block (&block, &lse.post);
    }
  else
    {
      gfc_conv_expr_descriptor (&lse, expr1, lss);
      /* Implement Nullify.  */
      if (expr2->expr_type == EXPR_NULL)
        {
          lse.expr = gfc_conv_descriptor_data (lse.expr);
          rse.expr = fold_convert (TREE_TYPE (lse.expr), null_pointer_node);
          gfc_add_modify_expr (&block, lse.expr, rse.expr);
        }
      else
        {
          lse.direct_byref = 1;
          gfc_conv_expr_descriptor (&lse, expr2, rss);
        }
      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_block_to_block (&block, &lse.post);
    }
  return gfc_finish_block (&block);
}


/* Makes sure se is suitable for passing as a function string parameter.  */
/* TODO: Need to check all callers fo this function.  It may be abused.  */

void
gfc_conv_string_parameter (gfc_se * se)
{
  tree type;

  if (TREE_CODE (se->expr) == STRING_CST)
    {
      se->expr = gfc_build_addr_expr (pchar_type_node, se->expr);
      return;
    }

  type = TREE_TYPE (se->expr);
  if (TYPE_STRING_FLAG (type))
    {
      assert (TREE_CODE (se->expr) != INDIRECT_REF);
      se->expr = gfc_build_addr_expr (pchar_type_node, se->expr);
    }

  assert (POINTER_TYPE_P (TREE_TYPE (se->expr)));
  assert (se->string_length
	  && TREE_CODE (TREE_TYPE (se->string_length)) == INTEGER_TYPE);
}


/* Generate code for assignment of scalar variables.  Includes character
   strings.  */

tree
gfc_trans_scalar_assign (gfc_se * lse, gfc_se * rse, bt type)
{
  stmtblock_t block;

  gfc_init_block (&block);

  if (type == BT_CHARACTER)
    {
      assert (lse->string_length != NULL_TREE
	      && rse->string_length != NULL_TREE);

      gfc_conv_string_parameter (lse);
      gfc_conv_string_parameter (rse);

      gfc_add_block_to_block (&block, &lse->pre);
      gfc_add_block_to_block (&block, &rse->pre);

      gfc_trans_string_copy (&block, lse->string_length, lse->expr,
			     rse->string_length, rse->expr);
    }
  else
    {
      gfc_add_block_to_block (&block, &lse->pre);
      gfc_add_block_to_block (&block, &rse->pre);

      gfc_add_modify_expr (&block, lse->expr,
			   fold_convert (TREE_TYPE (lse->expr), rse->expr));
    }

  gfc_add_block_to_block (&block, &lse->post);
  gfc_add_block_to_block (&block, &rse->post);

  return gfc_finish_block (&block);
}


/* Try to translate array(:) = func (...), where func is a transformational
   array function, without using a temporary.  Returns NULL is this isn't the
   case.  */

static tree
gfc_trans_arrayfunc_assign (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_se se;
  gfc_ss *ss;

  /* The caller has already checked rank>0 and expr_type == EXPR_FUNCTION.  */
  if (expr2->value.function.isym && !gfc_is_intrinsic_libcall (expr2))
    return NULL;

  /* Elemental functions don't need a temporary anyway.  */
  if (expr2->symtree->n.sym->attr.elemental)
    return NULL;

  /* Check for a dependency.  */
  if (gfc_check_fncall_dependency (expr1, expr2))
    return NULL;

  /* The frontend doesn't seem to bother filling in expr->symtree for intrinsic
     functions.  */
  assert (expr2->value.function.isym
	  || (gfc_return_by_reference (expr2->symtree->n.sym)
	      && expr2->symtree->n.sym->result->attr.dimension));

  ss = gfc_walk_expr (expr1);
  assert (ss != gfc_ss_terminator);
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  se.want_pointer = 1;

  gfc_conv_array_parameter (&se, expr1, ss, 0);

  se.direct_byref = 1;
  se.ss = gfc_walk_expr (expr2);
  assert (se.ss != gfc_ss_terminator);
  gfc_conv_function_expr (&se, expr2);
  gfc_add_expr_to_block (&se.pre, se.expr);
  gfc_add_block_to_block (&se.pre, &se.post);

  return gfc_finish_block (&se.pre);
}


/* Translate an assignment.  Most of the code is concerned with
   setting up the scalarizer.  */

tree
gfc_trans_assignment (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *lss_section;
  gfc_ss *rss;
  gfc_loopinfo loop;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;

  /* Special case a single function returning an array.  */
  if (expr2->expr_type == EXPR_FUNCTION && expr2->rank > 0)
    {
      tmp = gfc_trans_arrayfunc_assign (expr1, expr2);
      if (tmp)
	return tmp;
    }

  /* Assignment of the form lhs = rhs.  */
  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = gfc_walk_expr (expr1);
  rss = NULL;
  if (lss != gfc_ss_terminator)
    {
      /* The assignment needs scalarization.  */
      lss_section = lss;

      /* Find a non-scalar SS from the lhs.  */
      while (lss_section != gfc_ss_terminator
	     && lss_section->type != GFC_SS_SECTION)
	lss_section = lss_section->next;

      assert (lss_section != gfc_ss_terminator);

      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);

      /* Walk the rhs.  */
      rss = gfc_walk_expr (expr2);
      if (rss == gfc_ss_terminator)
	{
	  /* The rhs is scalar.  Add a ss for the expression.  */
	  rss = gfc_get_ss ();
	  rss->next = gfc_ss_terminator;
	  rss->type = GFC_SS_SCALAR;
	  rss->expr = expr2;
	}
      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      /* Calculate the bounds of the scalarization.  */
      gfc_conv_ss_startstride (&loop);
      /* Resolve any data dependencies in the statement.  */
      gfc_conv_resolve_dependencies (&loop, lss_section, rss);
      /* Setup the scalarizing loops.  */
      gfc_conv_loop_setup (&loop);

      /* Setup the gfc_se structures.  */
      gfc_copy_loopinfo_to_se (&lse, &loop);
      gfc_copy_loopinfo_to_se (&rse, &loop);

      rse.ss = rss;
      gfc_mark_ss_chain_used (rss, 1);
      if (loop.temp_ss == NULL)
	{
	  lse.ss = lss;
	  gfc_mark_ss_chain_used (lss, 1);
	}
      else
	{
	  lse.ss = loop.temp_ss;
	  gfc_mark_ss_chain_used (lss, 3);
	  gfc_mark_ss_chain_used (loop.temp_ss, 3);
	}

      /* Start the scalarized loop body.  */
      gfc_start_scalarized_body (&loop, &body);
    }
  else
    gfc_init_block (&body);

  /* Translate the expression.  */
  gfc_conv_expr (&rse, expr2);

  if (lss != gfc_ss_terminator && loop.temp_ss != NULL)
    {
      gfc_conv_tmp_array_ref (&lse);
      gfc_advance_se_ss_chain (&lse);
    }
  else
    gfc_conv_expr (&lse, expr1);

  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts.type);
  gfc_add_expr_to_block (&body, tmp);

  if (lss == gfc_ss_terminator)
    {
      /* Use the scalar assignment as is.  */
      gfc_add_block_to_block (&block, &body);
    }
  else
    {
      if (lse.ss != gfc_ss_terminator)
	abort ();
      if (rse.ss != gfc_ss_terminator)
	abort ();

      if (loop.temp_ss != NULL)
	{
	  gfc_trans_scalarized_loop_boundary (&loop, &body);

	  /* We need to copy the temporary to the actual lhs.  */
	  gfc_init_se (&lse, NULL);
	  gfc_init_se (&rse, NULL);
	  gfc_copy_loopinfo_to_se (&lse, &loop);
	  gfc_copy_loopinfo_to_se (&rse, &loop);

	  rse.ss = loop.temp_ss;
	  lse.ss = lss;

	  gfc_conv_tmp_array_ref (&rse);
	  gfc_advance_se_ss_chain (&rse);
	  gfc_conv_expr (&lse, expr1);

	  if (lse.ss != gfc_ss_terminator)
	    abort ();

	  if (rse.ss != gfc_ss_terminator)
	    abort ();

	  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts.type);
	  gfc_add_expr_to_block (&body, tmp);
	}
      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &body);

      /* Wrap the whole thing up.  */
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);

      gfc_cleanup_loop (&loop);
    }

  return gfc_finish_block (&block);
}

tree
gfc_trans_assign (gfc_code * code)
{
  return gfc_trans_assignment (code->expr, code->expr2);
}
