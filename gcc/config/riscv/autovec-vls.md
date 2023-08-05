;; Machine description for VLS of RVV auto-vectorization.
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

;; We define VLS modes as 'define_insn_and_split' with normal
;; RTX_CODE operation, so we can gain benefits from Combine optimizations.

;; -----------------------------------------------------------------
;; ---- Moves Operations
;; -----------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:VLS_AVL_IMM 0 "reg_or_mem_operand")
	(match_operand:VLS_AVL_IMM 1 "general_operand"))]
  "TARGET_VECTOR"
{
  if (riscv_vector::legitimize_move (operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "*mov<mode>_mem_to_mem"
  [(set (match_operand:VLS_AVL_IMM 0 "memory_operand")
	(match_operand:VLS_AVL_IMM 1 "memory_operand"))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    if (GET_MODE_BITSIZE (<MODE>mode).to_constant () <= MAX_BITS_PER_WORD)
      {
        /* Opitmize the following case:

	    typedef int8_t v2qi __attribute__ ((vector_size (2)));
	    v2qi v = *(v2qi*)in;
	    *(v2qi*)out = v;

	    We prefer scalar load/store instead of vle.v/vse.v when
	    the VLS modes size is smaller scalar mode.  */
        machine_mode mode;
        unsigned size = GET_MODE_BITSIZE (<MODE>mode).to_constant ();
        if (FLOAT_MODE_P (<MODE>mode))
	  mode = mode_for_size (size, MODE_FLOAT, 0).require ();
        else
	  mode = mode_for_size (size, MODE_INT, 0).require ();
        emit_move_insn (gen_lowpart (mode, operands[0]),
		        gen_lowpart (mode, operands[1]));
      }
    else
      {
	operands[1] = force_reg (<MODE>mode, operands[1]);
	emit_move_insn (operands[0], operands[1]);
      }
    DONE;
  }
)

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VLS_AVL_IMM 0 "reg_or_mem_operand" "=vr, m, vr")
	(match_operand:VLS_AVL_IMM 1 "reg_or_mem_operand" "  m,vr, vr"))]
  "TARGET_VECTOR
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   #
   #
   vmv%m1r.v\t%0,%1"
  "&& reload_completed
   && (!register_operand (operands[0], <MODE>mode)
       || !register_operand (operands[1], <MODE>mode))"
  [(const_int 0)]
  {
    bool ok_p = riscv_vector::legitimize_move (operands[0], operands[1]);
    gcc_assert (ok_p);
    DONE;
  }
)

(define_expand "mov<mode>"
  [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand")
	(match_operand:VLS_AVL_REG 1 "general_operand"))]
  "TARGET_VECTOR"
{
  bool ok_p = riscv_vector::legitimize_move (operands[0], operands[1]);
  gcc_assert (ok_p);
  DONE;
})

(define_expand "@mov<VLS_AVL_REG:mode><P:mode>_lra"
  [(parallel
    [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand")
	  (match_operand:VLS_AVL_REG 1 "reg_or_mem_operand"))
   (clobber (match_scratch:P 2))])]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)"
{})

(define_insn_and_split "*mov<VLS_AVL_REG:mode><P:mode>_lra"
  [(set (match_operand:VLS_AVL_REG 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:VLS_AVL_REG 1 "reg_or_mem_operand" "  m,vr,vr"))
   (clobber (match_scratch:P 2 "=&r,&r,X"))]
  "TARGET_VECTOR && (lra_in_progress || reload_completed)
   && (register_operand (operands[0], <VLS_AVL_REG:MODE>mode)
       || register_operand (operands[1], <VLS_AVL_REG:MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  if (REG_P (operands[0]) && REG_P (operands[1]))
      emit_insn (gen_rtx_SET (operands[0], operands[1]));
  else
    {
      emit_move_insn (operands[2], gen_int_mode (GET_MODE_NUNITS (<VLS_AVL_REG:MODE>mode),
						 Pmode));
      riscv_vector::emit_nonvlmax_insn (code_for_pred_mov (<VLS_AVL_REG:MODE>mode),
					riscv_vector::RVV_UNOP, operands, operands[2]);
    }
  DONE;
})

(define_insn "*mov<mode>_vls"
  [(set (match_operand:VLS 0 "register_operand" "=vr")
	(match_operand:VLS 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv%m1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

;; -----------------------------------------------------------------
;; ---- Duplicate Operations
;; -----------------------------------------------------------------

(define_insn_and_split "@vec_duplicate<mode>"
  [(set (match_operand:VLS 0 "register_operand")
        (vec_duplicate:VLS
          (match_operand:<VEL> 1 "reg_or_int_operand")))]
  "TARGET_VECTOR && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
  {
    riscv_vector::emit_vlmax_insn (code_for_pred_broadcast (<MODE>mode),
                                   riscv_vector::RVV_UNOP, operands);
    DONE;
  }
)
