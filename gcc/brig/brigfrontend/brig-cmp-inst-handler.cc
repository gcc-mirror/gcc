/* brig-cmp-inst-handler.cc -- brig cmp instruction handling
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#include "brig-code-entry-handler.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "print-tree.h"
#include "brig-util.h"
#include "convert.h"

size_t
brig_cmp_inst_handler::operator () (const BrigBase *base)
{
  const BrigInstBase *inst_base = (const BrigInstBase *) base;
  const BrigInstCmp *inst = (const BrigInstCmp *) base;

  tree cmp_type = get_tree_expr_type_for_hsa_type (inst->sourceType);

  /* The destination type to convert the comparison result to.  */
  tree dest_type = gccbrig_tree_type_for_hsa_type (inst_base->type);

  const bool is_fp16_dest
    = (inst_base->type & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16;
  const bool is_boolean_dest
    = (inst_base->type & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_B1;

  bool is_int_cmp = VECTOR_TYPE_P (cmp_type)
		      ? INTEGRAL_TYPE_P (TREE_TYPE (cmp_type))
		      : INTEGRAL_TYPE_P (cmp_type);

  /* The type for the GENERIC comparison.  It should match the
     input operand width for vector comparisons, a boolean
     otherwise.  */
  tree result_type = get_comparison_result_type (cmp_type);

  /* Save the result as a boolean and extend/convert it to the
     wanted destination type.  */
  tree expr = NULL_TREE;

  std::vector<tree> operands = build_operands (*inst_base);

  switch (inst->compare)
    {
    case BRIG_COMPARE_SEQ:
    case BRIG_COMPARE_EQ:
      expr = build2 (EQ_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SNE:
    case BRIG_COMPARE_NE:
      expr = build2 (NE_EXPR, result_type, operands[1], operands[2]);

      if (!is_int_cmp)
	expr = build2 (BIT_AND_EXPR, TREE_TYPE (expr),
		       expr,
		       build2 (ORDERED_EXPR, result_type, operands[1],
			       operands[2]));
      break;
    case BRIG_COMPARE_SLT:
    case BRIG_COMPARE_LT:
      expr = build2 (LT_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SLE:
    case BRIG_COMPARE_LE:
      expr = build2 (LE_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SGT:
    case BRIG_COMPARE_GT:
      expr = build2 (GT_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SGE:
    case BRIG_COMPARE_GE:
      expr = build2 (GE_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SEQU:
    case BRIG_COMPARE_EQU:
      expr = build2 (UNEQ_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SNEU:
    case BRIG_COMPARE_NEU:
      expr = build2 (NE_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SLTU:
    case BRIG_COMPARE_LTU:
      expr = build2 (UNLT_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SLEU:
    case BRIG_COMPARE_LEU:
      expr = build2 (UNLE_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SGTU:
    case BRIG_COMPARE_GTU:
      expr = build2 (UNGT_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SGEU:
    case BRIG_COMPARE_GEU:
      expr = build2 (UNGE_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SNUM:
    case BRIG_COMPARE_NUM:
      expr = build2 (ORDERED_EXPR, result_type, operands[1], operands[2]);
      break;
    case BRIG_COMPARE_SNAN:
    case BRIG_COMPARE_NAN:
      expr = build2 (UNORDERED_EXPR, result_type, operands[1], operands[2]);
      break;
    default:
      break;
    }

  if (expr == NULL_TREE)
    gcc_unreachable ();

  if (is_fp16_dest)
    {
      expr = convert_to_real (brig_to_generic::s_fp32_type, expr);
    }
  else if (VECTOR_TYPE_P (dest_type) && ANY_INTEGRAL_TYPE_P (dest_type)
	   && !is_boolean_dest
	   && (inst->sourceType & BRIG_TYPE_BASE_MASK) != BRIG_TYPE_F16)
    {
      /* In later gcc versions, the output of comparison is not
	 all ones for vectors like still in 4.9.1.  We need to use
	 an additional VEC_COND_EXPR to produce the all ones 'true' value
	 required by HSA.
	 VEC_COND_EXPR <a == b, { -1, -1, -1, -1 }, { 0, 0, 0, 0 }>; */

      tree all_ones
	= build_vector_from_val (dest_type,
			       build_minus_one_cst (TREE_TYPE (dest_type)));
      tree all_zeroes
	= build_vector_from_val (dest_type,
			       build_zero_cst (TREE_TYPE (dest_type)));
      expr = build3 (VEC_COND_EXPR, dest_type, expr, all_ones, all_zeroes);
    }
  else if (INTEGRAL_TYPE_P (dest_type) && !is_boolean_dest)
    {
      /* We need to produce the all-ones pattern for the width of the whole
	 resulting integer type.  Use back and forth shifts for propagating
	 the lower 1.  */
      tree signed_type = signed_type_for (dest_type);
      tree signed_result = convert_to_integer (signed_type, expr);

      size_t result_width = int_size_in_bytes (dest_type) * BITS_PER_UNIT;

      tree shift_amount_cst
	= build_int_cstu (signed_type, result_width - 1);

      tree shift_left_result
	= build2 (LSHIFT_EXPR, signed_type, signed_result, shift_amount_cst);

      expr = build2 (RSHIFT_EXPR, signed_type, shift_left_result,
		     shift_amount_cst);
    }
  else if (SCALAR_FLOAT_TYPE_P (dest_type))
    {
      expr = convert_to_real (dest_type, expr);
    }
  else if (VECTOR_TYPE_P (dest_type)
	   && (inst->sourceType & BRIG_TYPE_BASE_MASK) == BRIG_TYPE_F16)
    {
      /* Because F16 comparison is emulated as an F32 comparison with S32
	 results, we must now truncate the result vector to S16s so it
	 fits to the destination register.  We can build the target vector
	 type from the f16 storage type (unsigned ints).  */
      expr = m_parent.m_cf->add_temp_var ("wide_cmp_result", expr);
      tree_stl_vec wide_elements;
      tree_stl_vec shrunk_elements;
      m_parent.m_cf->unpack (expr, wide_elements);
      for (size_t i = 0; i < wide_elements.size (); ++i)
	{
	  tree wide = wide_elements.at (i);
	  shrunk_elements.push_back
	    (convert_to_integer (short_integer_type_node, wide));
	}
      expr = m_parent.m_cf->pack (shrunk_elements);
    }
  build_output_assignment (*inst_base, operands[0], expr);

  return base->byteCount;
}
