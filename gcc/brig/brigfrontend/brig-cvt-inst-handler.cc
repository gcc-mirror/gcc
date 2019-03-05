/* brig-cvt-inst-handler.cc -- brig cvt (convert) instruction handling
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#include <sstream>

#include "brig-code-entry-handler.h"

#include "gimple-expr.h"
#include "errors.h"
#include "convert.h"
#include "tree-pretty-print.h"
#include "print-tree.h"
#include "diagnostic-core.h"
#include "brig-util.h"

const BrigAluModifier8_t *
brig_cvt_inst_handler::modifier (const BrigBase *base) const
{
  const BrigInstCvt *inst = (const BrigInstCvt *) base;
  return &inst->modifier;
}

const BrigRound8_t *
brig_cvt_inst_handler::round (const BrigBase *base) const
{
  const BrigInstCvt *inst = (const BrigInstCvt *) base;
  return &inst->round;
}

size_t
brig_cvt_inst_handler::generate (const BrigBase *base)
{
  /* In cvt instructions there can be at least four data types involved:

     - the input register type
     - the output register type
     - the conversion source type
     - the conversion destination type
  */

  const BrigInstBase *brig_inst
    = (const BrigInstBase *) &((const BrigInstBasic *) base)->base;
  const BrigInstCvt *cvt_inst = (const BrigInstCvt *) base;

  const BrigAluModifier8_t *inst_modifier = modifier (base);
  const bool FTZ = inst_modifier != NULL && (*inst_modifier) & BRIG_ALU_FTZ;

  /* The conversion source type.  */
  tree src_type = get_tree_expr_type_for_hsa_type (cvt_inst->sourceType);

  bool src_is_fp16 = cvt_inst->sourceType == BRIG_TYPE_F16;

  /* The conversion destination type.  */
  tree dest_type = gccbrig_tree_type_for_hsa_type (brig_inst->type);

  bool dest_is_fp16 = brig_inst->type == BRIG_TYPE_F16;

  if (!dest_type || !src_type)
    {
      gcc_unreachable ();
      return base->byteCount;
    }

  tree_stl_vec operands = build_operands (*brig_inst);
  tree &input = operands.at (1);
  tree &output = operands.at (0);

  if (m_parent.m_cf->is_id_val (input))
    {
      input = m_parent.m_cf->id_val (input);
      src_type = TREE_TYPE (input);
    }

  size_t conv_src_size = int_size_in_bytes (src_type);
  size_t conv_dst_size = int_size_in_bytes (dest_type);
  size_t src_reg_size = int_size_in_bytes (TREE_TYPE (input));

  /* The input register can be of different type&size than the
     conversion input size.  First cast the input to the conversion
     input type.  These casts are always bitcasts which can be
     expressed as casts between different unsigned integers.  */
  if (src_reg_size != conv_src_size)
    {
      tree unsigned_int_type = NULL_TREE;
      if (INTEGRAL_TYPE_P (src_type))
	unsigned_int_type = unsigned_type_for (src_type);
      else /* Find a matching size int type for the REAL type.  */
	{
	  if (conv_src_size == 2)
	    unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U16);
	  else if (conv_src_size == 4)
	    unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U32);
	  else if (conv_src_size == 8)
	    unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U64);
	  else
	    gcc_unreachable ();
	}
      input = convert_to_integer (unsigned_int_type, input);
    }

  if (src_is_fp16)
    input = build_h2f_conversion (input);

  /* Flush the float operand to zero if indicated with 'ftz'.  */
  if (FTZ && SCALAR_FLOAT_TYPE_P (src_type))
    {
      tree casted_input = build_resize_convert_view (src_type, input);
      input = flush_to_zero (src_is_fp16) (*this, casted_input);
    }

  tree conversion_result = NULL_TREE;
  if (brig_inst->type == BRIG_TYPE_B1)
    {
      /* When the destination is b1, cvt does a 'ztest' operation which is
	 defined as a != 0 for integers and similarly (!= 0.0f) for floats.  */
      if (INTEGRAL_TYPE_P (src_type))
	{
	  /* Generate an integer not equal operation.  */
	  conversion_result = build2 (NE_EXPR, TREE_TYPE (input), input,
				      build_int_cst (TREE_TYPE (input), 0));
	}
      else
	{
	  /* For REAL source types, ztest returns 1 if the value is not +- 0.0f.
	     We can perform this check with an integer comparison after
	     masking away the sign bit from a correct position.  This is safer
	     than using absf because of exceptions in case of a NaN
	     input (NaN exceptions are not generated with cvt).  */
	  tree unsigned_int_type = NULL_TREE;
	  /* Bit battern with all but the upper bit 1.  */
	  tree and_mask = NULL_TREE;
	  if (conv_src_size == 2)
	    {
	      unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U16);
	      and_mask = build_int_cst (unsigned_int_type, 0x7FFF);
	    }
	  else if (conv_src_size == 4)
	    {
	      unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U32);
	      and_mask = build_int_cst (unsigned_int_type, 0x7FFFFFFF);
	    }
	  else if (conv_src_size == 8)
	    {
	      unsigned_int_type = gccbrig_tree_type_for_hsa_type (BRIG_TYPE_U64);
	      and_mask = build_int_cst (unsigned_int_type, 0x7FFFFFFFFFFFFFFF);
	    }
	  else
	    gcc_unreachable ();
	  tree casted_input = build_resize_convert_view (unsigned_int_type,
							 input);
	  tree masked_input
	    = build2 (BIT_AND_EXPR, unsigned_int_type, casted_input, and_mask);
	  conversion_result
	    = build2 (NE_EXPR, TREE_TYPE (masked_input), masked_input,
		      build_int_cst (unsigned_int_type, 0));
	}
      /* The result from the comparison is a boolean, convert it to such.  */
      conversion_result
	= convert_to_integer (gccbrig_tree_type_for_hsa_type (BRIG_TYPE_B1),
			      conversion_result);
    }
  else if (dest_is_fp16)
    {
      tree casted_input = build_resize_convert_view (src_type, input);
      conversion_result
	= convert_to_real (brig_to_generic::s_fp32_type, casted_input);
      if (FTZ)
	conversion_result = flush_to_zero (true) (*this, conversion_result);
      conversion_result = build_f2h_conversion (conversion_result);
    }
  else if (SCALAR_FLOAT_TYPE_P (dest_type))
    {
      tree casted_input = build_resize_convert_view (src_type, input);
      conversion_result = convert_to_real (dest_type, casted_input);
    }
  else if (INTEGRAL_TYPE_P (dest_type) && INTEGRAL_TYPE_P (src_type))
    {
      conversion_result = extend_int (input, dest_type, src_type);
    }
  else if (INTEGRAL_TYPE_P (dest_type) && SCALAR_FLOAT_TYPE_P (src_type))
    {

      if (cvt_inst->round == BRIG_ROUND_INTEGER_ZERO_SAT)
	{

	  /* Use builtins for the saturating conversions.  */
#undef DEF_HSAIL_SAT_BUILTIN
#undef DEF_HSAIL_BUILTIN
#undef DEF_HSAIL_ATOMIC_BUILTIN
#undef DEF_HSAIL_INTR_BUILTIN
#undef DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN

	  tree builtin = NULL_TREE;
	  BrigType16_t src_arith_type
	    = src_is_fp16
	    ? (BrigType16_t) BRIG_TYPE_F32 : cvt_inst->sourceType;
#define DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN(ENUM, HSAIL_DST_TYPE, HSAIL_SRC_TYPE, \
					NAME, TYPE, ATTRS)		\
	  if (brig_inst->type == HSAIL_DST_TYPE				\
	      && src_arith_type == HSAIL_SRC_TYPE)		\
	    builtin = builtin_decl_explicit (ENUM);			\
	  else
#include "brig-builtins.def"
	    gcc_unreachable ();

	  tree casted_input = build_resize_convert_view (src_type, input);
	  conversion_result
	    = call_builtin (builtin, 1, dest_type, src_type, casted_input);
	}
      else
	{
	  tree casted_input = build_resize_convert_view (src_type, input);

	  /* Perform the float to int conversion.  */
	  conversion_result = convert_to_integer (dest_type, casted_input);
	}
    }
  else
    {
      /* Just use CONVERT_EXPR and hope for the best.  */
      tree casted_input = build_resize_convert_view (dest_type, input);
      conversion_result = build1 (CONVERT_EXPR, dest_type, casted_input);
    }

  size_t dst_reg_size = int_size_in_bytes (TREE_TYPE (output));

  /* The output register can be of different type&size than the
     conversion output size. Only need to handle signed integers, rest
     is handled by reinterpret_cast.  */
  tree casted_output = conversion_result;
  if (dst_reg_size > conv_dst_size &&
      INTEGRAL_TYPE_P (TREE_TYPE (casted_output)))
    {
      gcc_assert (!VECTOR_TYPE_P (casted_output));

      bool unsignedp = TYPE_UNSIGNED (TREE_TYPE (casted_output));
      tree resized_int_type
        = build_nonstandard_integer_type (dst_reg_size * BITS_PER_UNIT,
					  unsignedp);
      casted_output = build1 (CONVERT_EXPR, resized_int_type, casted_output);
    }

  casted_output
    = build_resize_convert_view (TREE_TYPE (output), casted_output);
  tree assign = build2 (MODIFY_EXPR, TREE_TYPE (output), output, casted_output);

  m_parent.m_cf->append_statement (assign);

  return base->byteCount;
}
