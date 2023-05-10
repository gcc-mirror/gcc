;; Machine description for auto-vectorization using RVV for GNU compiler.
;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; =========================================================================
;; == Loads/Stores
;; =========================================================================

;; len_load/len_store is a sub-optimal pattern for RVV auto-vectorization support.
;; We will replace them when len_maskload/len_maskstore is supported in loop vectorizer.
(define_expand "len_load_<mode>"
  [(match_operand:V 0 "register_operand")
   (match_operand:V 1 "memory_operand")
   (match_operand 2 "vector_length_operand")
   (match_operand 3 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_nonvlmax_op (code_for_pred_mov (<MODE>mode), operands[0],
				  operands[1], operands[2], <VM>mode);
  DONE;
})

(define_expand "len_store_<mode>"
  [(match_operand:V 0 "memory_operand")
   (match_operand:V 1 "register_operand")
   (match_operand 2 "vector_length_operand")
   (match_operand 3 "const_0_operand")]
  "TARGET_VECTOR"
{
  riscv_vector::emit_nonvlmax_op (code_for_pred_mov (<MODE>mode), operands[0],
				  operands[1], operands[2], <VM>mode);
  DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:V 0 "nonimmediate_operand")
	(match_operand:V 1 "general_operand"))]
  "TARGET_VECTOR"
  {
    /* Equivalent to a normal move for our purpooses.  */
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

;; =========================================================================
;; == Vector creation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Linear series
;; -------------------------------------------------------------------------
;; Includes:
;; - vid.v
;; - vmul.vx
;; - vadd.vx/vadd.vi
;; -------------------------------------------------------------------------

(define_expand "@vec_series<mode>"
  [(match_operand:VI 0 "register_operand")
   (match_operand:<VEL> 1 "reg_or_int_operand")
   (match_operand:<VEL> 2 "reg_or_int_operand")]
  "TARGET_VECTOR"
  {
    riscv_vector::expand_vec_series (operands[0], operands[1], operands[2]);
    DONE;
  }
)
