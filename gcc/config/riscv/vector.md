;; Machine description for RISC-V 'V' Extension for GNU compiler.
;; Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

;; This file describes the RISC-V 'V' Extension, Version 1.0.
;;
;; This file include :
;;
;; - Intrinsics (https://github.com/riscv/rvv-intrinsic-doc)
;; - Auto-vectorization (TBD)
;; - Combine optimization (TBD)

(include "vector-iterators.md")

(define_constants [
   (INVALID_ATTRIBUTE            255)
])

;; True if the type is RVV instructions that include VTYPE
;; global status register in the use op list.
;; We known VTYPE has 4 fields: SEW, LMUL, TA, MA.
;; The instruction need any of VTYPE field is set as true
;; in this attribute.
(define_attr "has_vtype_op" "false,true"
  (cond [(eq_attr "type" "vlde,vste,vldm,vstm,vlds,vsts,\
			  vldux,vldox,vstux,vstox,vldff,\
			  vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,\
			  vimul,vidiv,viwmul,vimuladd,viwmuladd,vimerge,vimov,\
			  vsalu,vaalu,vsmul,vsshift,vnclip,\
			  vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,\
			  vfcmp,vfsgnj,vfclass,vfmerge,vfmov,\
			  vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,\
			  vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,\
			  vired,viwred,vfred,vfredo,vfwred,vfwredo,\
			  vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,\
			  vislide,vislide1,vfslide1,vgather,vcompress")
	 (const_string "true")]
	(const_string "false")))

;; True if the type is RVV instructions that include VL
;; global status register in the use op list.
;; The instruction need vector length to be specified is set
;; in this attribute.
(define_attr "has_vl_op" "false,true"
  (cond [(eq_attr "type" "vlde,vste,vldm,vstm,vlds,vsts,\
			  vldux,vldox,vstux,vstox,vldff,\
			  vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,\
			  vimul,vidiv,viwmul,vimuladd,viwmuladd,vimerge,vimov,\
			  vsalu,vaalu,vsmul,vsshift,vnclip,\
			  vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,\
			  vfcmp,vfsgnj,vfclass,vfmerge,vfmov,\
			  vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,\
			  vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,\
			  vired,viwred,vfred,vfredo,vfwred,vfwredo,\
			  vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovxv,vfmovfv,\
			  vislide,vislide1,vfslide1,vgather,vcompress")
	 (const_string "true")]
	(const_string "false")))

;; The default SEW of RVV instruction. This attribute doesn't mean the instruction
;; is necessary to require SEW check for example vlm.v which require ratio to
;; check. However, we need default value of SEW for vsetvl instruction since there
;; is no field for ratio in the vsetvl instruction encoding.
(define_attr "sew" ""
  (cond [(eq_attr "mode" "VNx1QI,VNx2QI,VNx4QI,VNx8QI,VNx16QI,VNx32QI,VNx64QI,\
			  VNx1BI,VNx2BI,VNx4BI,VNx8BI,VNx16BI,VNx32BI,VNx64BI")
	 (const_int 8)
	 (eq_attr "mode" "VNx1HI,VNx2HI,VNx4HI,VNx8HI,VNx16HI,VNx32HI")
	 (const_int 16)
	 (eq_attr "mode" "VNx1SI,VNx2SI,VNx4SI,VNx8SI,VNx16SI,\
			  VNx1SF,VNx2SF,VNx4SF,VNx8SF,VNx16SF")
	 (const_int 32)
	 (eq_attr "mode" "VNx1DI,VNx2DI,VNx4DI,VNx8DI,\
			  VNx1DF,VNx2DF,VNx4DF,VNx8DF")
	 (const_int 64)]
	(const_int INVALID_ATTRIBUTE)))

;; Ditto to LMUL.
(define_attr "vlmul" ""
  (cond [(eq_attr "mode" "VNx1QI,VNx1BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx1QImode)")
	 (eq_attr "mode" "VNx2QI,VNx2BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx2QImode)")
	 (eq_attr "mode" "VNx4QI,VNx4BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx4QImode)")
	 (eq_attr "mode" "VNx8QI,VNx8BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx8QImode)")
	 (eq_attr "mode" "VNx16QI,VNx16BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx16QImode)")
	 (eq_attr "mode" "VNx32QI,VNx32BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx32QImode)")
	 (eq_attr "mode" "VNx64QI,VNx64BI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx64QImode)")
	 (eq_attr "mode" "VNx1HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx1HImode)")
	 (eq_attr "mode" "VNx2HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx2HImode)")
	 (eq_attr "mode" "VNx4HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx4HImode)")
	 (eq_attr "mode" "VNx8HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx8HImode)")
	 (eq_attr "mode" "VNx16HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx16HImode)")
	 (eq_attr "mode" "VNx32HI")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx32HImode)")
	 (eq_attr "mode" "VNx1SI,VNx1SF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx1SImode)")
	 (eq_attr "mode" "VNx2SI,VNx2SF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx2SImode)")
	 (eq_attr "mode" "VNx4SI,VNx4SF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx4SImode)")
	 (eq_attr "mode" "VNx8SI,VNx8SF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx8SImode)")
	 (eq_attr "mode" "VNx16SI,VNx16SF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx16SImode)")
	 (eq_attr "mode" "VNx1DI,VNx1DF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx1DImode)")
	 (eq_attr "mode" "VNx2DI,VNx2DF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx2DImode)")
	 (eq_attr "mode" "VNx4DI,VNx4DF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx4DImode)")
	 (eq_attr "mode" "VNx8DI,VNx8DF")
	   (symbol_ref "riscv_vector::get_vlmul(E_VNx8DImode)")]
	(const_int INVALID_ATTRIBUTE)))

;; It is valid for instruction that require sew/lmul ratio.
(define_attr "ratio" ""
  (cond [(eq_attr "type" "vimov,vfmov,vldux,vldox,vstux,vstox")
	   (const_int INVALID_ATTRIBUTE)
	 (eq_attr "mode" "VNx1QI,VNx1BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx1QImode)")
	 (eq_attr "mode" "VNx2QI,VNx2BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx2QImode)")
	 (eq_attr "mode" "VNx4QI,VNx4BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx4QImode)")
	 (eq_attr "mode" "VNx8QI,VNx8BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx8QImode)")
	 (eq_attr "mode" "VNx16QI,VNx16BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx16QImode)")
	 (eq_attr "mode" "VNx32QI,VNx32BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx32QImode)")
	 (eq_attr "mode" "VNx64QI,VNx64BI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx64QImode)")
	 (eq_attr "mode" "VNx1HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx1HImode)")
	 (eq_attr "mode" "VNx2HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx2HImode)")
	 (eq_attr "mode" "VNx4HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx4HImode)")
	 (eq_attr "mode" "VNx8HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx8HImode)")
	 (eq_attr "mode" "VNx16HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx16HImode)")
	 (eq_attr "mode" "VNx32HI")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx32HImode)")
	 (eq_attr "mode" "VNx1SI,VNx1SF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx1SImode)")
	 (eq_attr "mode" "VNx2SI,VNx2SF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx2SImode)")
	 (eq_attr "mode" "VNx4SI,VNx4SF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx4SImode)")
	 (eq_attr "mode" "VNx8SI,VNx8SF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx8SImode)")
	 (eq_attr "mode" "VNx16SI,VNx16SF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx16SImode)")
	 (eq_attr "mode" "VNx1DI,VNx1DF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx1DImode)")
	 (eq_attr "mode" "VNx2DI,VNx2DF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx2DImode)")
	 (eq_attr "mode" "VNx4DI,VNx4DF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx4DImode)")
	 (eq_attr "mode" "VNx8DI,VNx8DF")
	   (symbol_ref "riscv_vector::get_ratio(E_VNx8DImode)")]
	(const_int INVALID_ATTRIBUTE)))

;; The index of operand[] to get the merge op.
(define_attr "merge_op_idx" ""
	(cond [(eq_attr "type" "vlde,vimov,vfmov,vldm,vlds,vmalu,vldux,vldox")
	 (const_int 2)]
	(const_int INVALID_ATTRIBUTE)))

;; The index of operand[] to get the avl op.
(define_attr "vl_op_idx" ""
  (cond [(eq_attr "type" "vlde,vste,vimov,vfmov,vldm,vstm,vmalu,vsts,vstux,vstox")
	   (const_int 4)

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
         (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
             (const_int 5)
             (const_int 4))

	 (eq_attr "type" "vldux,vldox")
	   (const_int 5)]
  (const_int INVALID_ATTRIBUTE)))

;; The tail policy op value.
(define_attr "ta" ""
  (cond [(eq_attr "type" "vlde,vimov,vfmov")
	   (symbol_ref "riscv_vector::get_ta(operands[5])")

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (symbol_ref "riscv_vector::get_ta(operands[6])")
	     (symbol_ref "riscv_vector::get_ta(operands[5])"))

	 (eq_attr "type" "vldux,vldox")
	   (symbol_ref "riscv_vector::get_ta(operands[6])")]
	(const_int INVALID_ATTRIBUTE)))

;; The mask policy op value.
(define_attr "ma" ""
  (cond [(eq_attr "type" "vlde")
	   (symbol_ref "riscv_vector::get_ma(operands[6])")

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (symbol_ref "riscv_vector::get_ma(operands[7])")
	     (symbol_ref "riscv_vector::get_ma(operands[6])"))

	 (eq_attr "type" "vldux,vldox")
	   (symbol_ref "riscv_vector::get_ma(operands[7])")]
	(const_int INVALID_ATTRIBUTE)))

;; The avl type value.
(define_attr "avl_type" ""
  (cond [(eq_attr "type" "vlde,vlde,vste,vimov,vimov,vimov,vfmov")
	   (symbol_ref "INTVAL (operands[7])")
	 (eq_attr "type" "vldm,vstm,vimov,vmalu,vmalu")
	   (symbol_ref "INTVAL (operands[5])")

	 ;; If operands[3] of "vlds" is not vector mode, it is pred_broadcast.
	 ;; wheras it is pred_strided_load if operands[3] is vector mode.
	 (eq_attr "type" "vlds")
	   (if_then_else (match_test "VECTOR_MODE_P (GET_MODE (operands[3]))")
	     (const_int INVALID_ATTRIBUTE)
	     (symbol_ref "INTVAL (operands[7])"))

	 (eq_attr "type" "vldux,vldox")
	   (symbol_ref "INTVAL (operands[8])")
	 (eq_attr "type" "vstux,vstox")
	   (symbol_ref "INTVAL (operands[5])")]
	(const_int INVALID_ATTRIBUTE)))

;; -----------------------------------------------------------------
;; ---- Miscellaneous Operations
;; -----------------------------------------------------------------

(define_insn "vundefined<mode>"
  [(set (match_operand:V 0 "register_operand" "=vr")
	(unspec:V [(const_int 0)] UNSPEC_VUNDEF))]
  "TARGET_VECTOR"
  "")

;; This pattern is used to hold the AVL operand for
;; RVV instructions that implicity use VLMAX AVL.
;; RVV instruction implicitly use GPR that is ultimately
;; defined by this pattern is safe for VSETVL pass emit
;; a vsetvl instruction modify this register after RA.
;; Case 1:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   vadd (implicit use a5)  ====> emit: vsetvl a5,zero
;; Case 2:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   mv a6,a5
;;   ... (across many blocks)
;;   vadd (implicit use a6)  ====> emit: vsetvl a6,zero
;; Case 3:
;;   vlmax_avl a5
;;   ... (across many blocks)
;;   store mem,a5 (spill)
;;   ... (across many blocks)
;;   load a7,mem (spill)
;;   ... (across many blocks)
;;   vadd (implicit use a7)  ====> emit: vsetvl a7,zero
;; Such cases are all safe for VSETVL PASS to emit a vsetvl
;; instruction that modifies the AVL operand.
(define_insn "@vlmax_avl<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "const_int_operand" "i")] UNSPEC_VLMAX))]
  "TARGET_VECTOR"
  "")

;; -----------------------------------------------------------------
;; ---- Moves Operations
;; -----------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:V 0 "reg_or_mem_operand")
	(match_operand:V 1 "vector_move_operand"))]
  "TARGET_VECTOR"
{
  /* For whole register move, we transform the pattern into the format
     that excludes the clobber of scratch register.

     We include clobber of a scalar scratch register which is going to be
     used for emit of vsetvl instruction after reload_completed since we
     need vsetvl instruction to set VL/VTYPE global status for fractional
     vector load/store.

     For example:
       [(set (match_operand:VNx1QI v24)
	     (match_operand:VNx1QI (mem: a4)))
	     (clobber (scratch:SI a5))]
     ====>> vsetvl a5,zero,e8,mf8
     ====>> vle8.v v24,(a4)

     Philosophy:

       - Clobber a scalar scratch register for each mov<mode>.

       - Classify the machine_mode mode = <MODE>mode into 2 class:
	 Whole register move and fractional register move.

       - Transform and remove scratch clobber register for whole
	 register move so that we can avoid occupying the scalar
	 registers.

       - We can not leave it to TARGET_SECONDARY_RELOAD since it happens
	 before spilling. The clobber scratch is used by spilling fractional
	 registers in IRA/LRA so it's too early.  */

  if (riscv_vector::legitimize_move (operands[0], operands[1], <VM>mode))
    DONE;
})

;; This pattern is used for code-gen for whole register load/stores.
;; Also applicable for all register moves.
;; Fractional vector modes load/store are not allowed to match this pattern.
;; Mask modes load/store are not allowed to match this pattern.
;; We seperate "*mov<mode>" into "*mov<mode>_whole" and "*mov<mode>_fract" because
;; we don't want to include fractional load/store in "*mov<mode>" which will
;; create unexpected patterns in LRA.
;; For example:
;; ira rtl:
;;   (insn 20 19 9 2 (set (reg/v:VNx2QI 97 v1 [ v1 ])
;;      (reg:VNx2QI 134 [ _1 ])) "rvv.c":9:22 571 {*movvnx2qi_fract}
;;   (nil))
;; When the value of pseudo register 134 of the insn above is discovered already
;; spilled in the memory during LRA.
;; LRA will reload this pattern into a memory load instruction pattern.
;; Because VNx2QI is a fractional vector, we want LRA reload this pattern into
;;  (insn 20 19 9 2 (parallel [
;;       (set (reg:VNx2QI 98 v2 [orig:134 _1 ] [134])
;;           (mem/c:VNx2QI (reg:SI 13 a3 [155]) [1 %sfp+[-2, -2] S[2, 2] A8]))
;;       (clobber (reg:SI 14 a4 [149]))])
;; So that we could be able to emit vsetvl instruction using clobber sratch a4.
;; To let LRA generate the expected pattern, we should exclude fractional vector
;; load/store in "*mov<mode>_whole". Otherwise, it will reload this pattern into:
;;  (insn 20 19 9 2 (set (reg:VNx2QI 98 v2 [orig:134 _1 ] [134])
;;           (mem/c:VNx2QI (reg:SI 13 a3 [155]) [1 %sfp+[-2, -2] S[2, 2] A8])))
;; which is not the pattern we want.
;; According the facts above, we make "*mov<mode>_whole" includes load/store/move for whole
;; vector modes according to '-march' and "*mov<mode>_fract" only include fractional vector modes.
(define_insn "*mov<mode>_whole"
  [(set (match_operand:V_WHOLE 0 "reg_or_mem_operand" "=vr, m,vr")
	(match_operand:V_WHOLE 1 "reg_or_mem_operand" "  m,vr,vr"))]
  "TARGET_VECTOR"
  "@
   vl%m1re<sew>.v\t%0,%1
   vs%m1r.v\t%1,%0
   vmv%m1r.v\t%0,%1"
  [(set_attr "type" "vldr,vstr,vmov")
   (set_attr "mode" "<MODE>")])

(define_insn "*mov<mode>_fract"
  [(set (match_operand:V_FRACT 0 "register_operand" "=vr")
	(match_operand:V_FRACT 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

(define_expand "mov<mode>"
  [(set (match_operand:VB 0 "reg_or_mem_operand")
	(match_operand:VB 1 "vector_move_operand"))]
  "TARGET_VECTOR"
{
  if (riscv_vector::legitimize_move (operands[0], operands[1], <MODE>mode))
    DONE;
})

(define_insn "*mov<mode>"
  [(set (match_operand:VB 0 "register_operand" "=vr")
	(match_operand:VB 1 "register_operand" " vr"))]
  "TARGET_VECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

;; -----------------------------------------------------------------
;; ---- Duplicate Operations
;; -----------------------------------------------------------------

;; According to GCC internal:
;; This pattern only handles duplicates of non-constant inputs.
;; Constant vectors go through the movm pattern instead.
;; So "direct_broadcast_operand" can only be mem or reg, no CONSTANT.
(define_expand "vec_duplicate<mode>"
  [(set (match_operand:V 0 "register_operand")
	(vec_duplicate:V
	  (match_operand:<VEL> 1 "direct_broadcast_operand")))]
  "TARGET_VECTOR"
  {
    riscv_vector::emit_pred_op (
      code_for_pred_broadcast (<MODE>mode), operands[0], operands[1], <VM>mode);
    DONE;
  }
)

;; -----------------------------------------------------------------
;; ---- 6. Configuration-Setting Instructions
;; -----------------------------------------------------------------
;; Includes:
;; - 6.1 vsetvli/vsetivl/vsetvl instructions
;; -----------------------------------------------------------------

;; we dont't define vsetvli as unspec_volatile which has side effects.
;; This instruction can be scheduled by the instruction scheduler.
;; This means these instructions will be deleted when
;; there is no instructions using vl or vtype in the following.
;; rd  | rs1 | AVL value | Effect on vl
;; -   | !x0 | x[rs1]    | Normal stripmining
;; !x0 | x0  | ~0        | Set vl to VLMAX
;; operands[0]: VL.
;; operands[1]: AVL.
;; operands[2]: SEW
;; operands[3]: LMUL
;; operands[4]: Tail policy 0 or 1 (undisturbed/agnostic)
;; operands[5]: Mask policy 0 or 1 (undisturbed/agnostic)

;; We define 2 types of "vsetvl*" instruction patterns:

;; -  "@vsetvl<mode>" is a parallel format which has side effects.

;; -  "@vsetvl<mode>_no_side_effects" has no side effects.

;; -  "@vsetvl<mode>" is used by "vsetvl" intrinsics and "insert-vsetvl" PASS.

;; -  "@vsetvl<mode>_no_side_effects" is used by GCC standard patterns.

;; -  "@vsetvl<mode>" includes VL/VTYPE global registers status (define set)
;; and each RVV instruction includes VL/VTYPE global registers status (use)
;; so that we can guarantee each RVV instruction can execute with correct
;; VL/VTYPE global registers status after "insert-vsetvl" PASS.

;; -  "@vsetvl<mode>_no_side_effects" has no side effects and excludes VL/VTYPE
;; global registers status (define set). It's only used by GCC standard pattern
;; expansion. For example: "mov<mode>" pattern for fractional vector modes which
;; need to set VL/VTYPE. Then we could manually call this pattern to gain benefits
;; from the optimization of each GCC internal PASS.

;; 1. void foo (float *in, float *out)
;;    {
;;      vfloat32mf2_t v = *(vfloat32mf2_t*)in;
;;      *(vfloat32mf2_t*)out = v;
;;    }
;; We could eliminate the second "vsetvl" by calling "@vsetvl<mode>_no_side_effects".
;;
;; "@vsetvl<mode>":               ;; "@vsetvl<mode>_no_side_effects":
;; vsetvli a4,zero,e32,mf2,ta,ma  ;; vsetvli a4,zero,e32,mf2,ta,ma
;; vle32.v v24,(a0)               ;; vle32.v v24,(a0)
;; vsetvli a4,zero,e32,mf2,ta,ma  ;; --
;; vse32.v v24,(a1)               ;; vse32.v v24,(a1)
;; ret                            ;; ret

;; 2. void foo (int8_t *in, int8_t *out, int M)
;;    {
;;      for (int i = 0; i < M; i++){
;;        vint8mf2_t v = *(vint8mf2_t*)(in + i);
;;        *(vint8mf2_t*)(out + i) = v;
;;      }
;;    }
;;
;; Hoist "vsetvl" instruction in LICM:
;; "@vsetvl<mode>":                  ;; "@vsetvl<mode>_no_side_effects":
;; -                                 ;;   vsetvli a4,zero,e32,mf2,ta,ma
;; LOOP:                             ;; LOOP:
;;   vsetvli a4,zero,e32,mf2,ta,ma   ;; -
;;   vle32.v v24,(a0)                ;;   vle32.v v24,(a0)
;;   vsetvli a4,zero,e32,mf2,ta,ma   ;; -
;;   vse32.v v24,(a1)                ;;   vse32.v v24,(a1)

;; However, it may produce wrong codegen if we exclude VL/VTYPE in "vsevl<mode>".
;; 3. void foo (int8_t *in, int8_t *out, int32_t *in2, int32_t *out2, int M)
;;    {
;;      for (int i = 0; i < M; i++){
;;        vint8mf2_t v = *(vint8mf2_t*)(in + i);
;;        vint32mf2_t v2 = *(vint32mf2_t*)(in + i + i);
;;        *(vint8mf2_t*)(out + i) = v;
;;        *(vint32mf2_t*)(out + i + i) = v2;
;;      }
;;    }
;;
;; vsetvli a6,zero,e8,mf2,ta,ma
;; vsetvli a2,zero,e32,mf2,ta,ma
;; LOOP:
;;   vle8.v  v25,(a0)
;;   vle32.v v24,(a5)
;;   addi    a0,a0,1
;;   vse8.v  v25,(a1)
;;   vse32.v v24,(a3)
;;
;; Both vle8.v and vle32.v are using the wrong VL/VTYPE status.
;; We leave it to "insert-vsetvl" PASS to correct this situation.

;; The "insert-vsetvl" PASS mechanism:
;; 1. Before "insert-vsetvl" PASS, only RVV instructions are generated
;;    by GCC standard pattern expansion has the corresponding "vsetvl".
;;    We exploit each GCC internal optimization pass to optimize the "vsetvl".
;; 2. Correct the VL/VTYPE status for each GCC standard pattern RVV instructions.
;;    Insert vsetvl for each RVV instructions that has no VL/VTYPE status if necessary.
;;    For example: RVV intrinsics.
;; 3. Optimize "vsetvl" instructions.

(define_insn "@vsetvl<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "csr_operand" "rK")
		   (match_operand 2 "const_int_operand" "i")
		   (match_operand 3 "const_int_operand" "i")
		   (match_operand 4 "const_int_operand" "i")
		   (match_operand 5 "const_int_operand" "i")] UNSPEC_VSETVL))
   (set (reg:SI VL_REGNUM)
	(unspec:SI [(match_dup 1)
		    (match_dup 2)
		    (match_dup 3)] UNSPEC_VSETVL))
   (set (reg:SI VTYPE_REGNUM)
	(unspec:SI [(match_dup 2)
		    (match_dup 3)
		    (match_dup 4)
		    (match_dup 5)] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vset%i1vli\t%0,%1,e%2,%m3,t%p4,m%p5"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "<MODE>")
   (set (attr "sew") (symbol_ref "INTVAL (operands[2])"))
   (set (attr "vlmul") (symbol_ref "INTVAL (operands[3])"))
   (set (attr "ta") (symbol_ref "INTVAL (operands[4])"))
   (set (attr "ma") (symbol_ref "INTVAL (operands[5])"))])

;; vsetvl zero,zero,vtype instruction.
;; This pattern has no side effects and does not set X0 register.
(define_insn "vsetvl_vtype_change_only"
  [(set (reg:SI VTYPE_REGNUM)
	(unspec:SI
	  [(match_operand 0 "const_int_operand" "i")
	   (match_operand 1 "const_int_operand" "i")
	   (match_operand 2 "const_int_operand" "i")
	   (match_operand 3 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vsetvli\tzero,zero,e%0,%m1,t%p2,m%p3"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "SI")])

;; vsetvl zero,rs1,vtype instruction.
;; The reason we need this pattern since we should avoid setting X0 register
;; in vsetvl instruction pattern.
(define_insn "@vsetvl_discard_result<mode>"
  [(set (reg:SI VL_REGNUM)
	(unspec:SI [(match_operand:P 0 "csr_operand" "rK")
		    (match_operand 1 "const_int_operand" "i")
		    (match_operand 2 "const_int_operand" "i")] UNSPEC_VSETVL))
   (set (reg:SI VTYPE_REGNUM)
	(unspec:SI [(match_dup 1)
		    (match_dup 2)
		    (match_operand 3 "const_int_operand" "i")
		    (match_operand 4 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "vset%i0vli\tzero,%0,e%1,%m2,t%p3,m%p4"
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "<MODE>")
   (set (attr "sew") (symbol_ref "INTVAL (operands[1])"))
   (set (attr "vlmul") (symbol_ref "INTVAL (operands[2])"))
   (set (attr "ta") (symbol_ref "INTVAL (operands[3])"))
   (set (attr "ma") (symbol_ref "INTVAL (operands[4])"))])

;; It's emit by vsetvl/vsetvlmax intrinsics with no side effects.
;; Since we have many optmization passes from "expand" to "reload_completed",
;; such pattern can allow us gain benefits of these optimizations.
(define_insn_and_split "@vsetvl<mode>_no_side_effects"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "csr_operand" "rK")
		   (match_operand 2 "const_int_operand" "i")
		   (match_operand 3 "const_int_operand" "i")
		   (match_operand 4 "const_int_operand" "i")
		   (match_operand 5 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "#"
  "&& epilogue_completed"
  [(parallel
    [(set (match_dup 0)
	  (unspec:P [(match_dup 1) (match_dup 2) (match_dup 3)
		     (match_dup 4) (match_dup 5)] UNSPEC_VSETVL))
     (set (reg:SI VL_REGNUM)
	  (unspec:SI [(match_dup 1) (match_dup 2) (match_dup 3)] UNSPEC_VSETVL))
     (set (reg:SI VTYPE_REGNUM)
	  (unspec:SI [(match_dup 2) (match_dup 3) (match_dup 4)
		      (match_dup 5)] UNSPEC_VSETVL))])]
  ""
  [(set_attr "type" "vsetvl")
   (set_attr "mode" "SI")])

;; RVV machine description matching format
;; (define_insn ""
;;   [(set (match_operand:MODE 0)
;; 	(if_then_else:MODE
;; 	  (unspec:<MODE:VM>
;; 	    [(match_operand:<VM> 1 "vector_mask_operand")
;; 	     (match_operand N + 4 "vector_length_operand")
;; 	     (match_operand N + 5 "const_int_operand")
;; 	     (match_operand N + 6 "const_int_operand")
;; 	     (reg:SI VL_REGNUM)
;; 	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
;; 	  (instruction operation:MODE
;; 	     (match_operand 3
;; 	     (match_operand 4
;; 	     (match_operand 5
;;           ................
;; 	     (match_operand N + 3)
;; 	  (match_operand:MODE 2 "vector_reg_or_const0_operand")))]
;;
;; (unspec:[........] UNSPEC_VPREDICATE) is a predicate wrapper.
;; Include mask predicate && length predicate && vector policy.

;; -------------------------------------------------------------------------------
;; ---- Predicated Mov
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.4. Vector Unit-Stride Instructions
;; - 11.16 Vector Integer Move Instructions
;; - 13.16 Vector Floating-Point Move Instruction
;; - 15.1 Vector Mask-Register Logical Instructions
;; -------------------------------------------------------------------------------

;; vle.v/vse.v/vmv.v.v/vmv.v.x/vmv.v.i/vfmv.v.f.
;; For vle.v/vmv.v.v/vmv.v.x/vmv.v.i/vfmv.v.f, we may need merge and mask operand.
;; For vse.v, we don't need merge operand, so it should always match "vu".
;; constraint alternative 0 ~ 1 match vle.v.
;; constraint alternative 2 match vse.v.
;; constraint alternative 3 match vmv.v.v.
;; constraint alternative 4 match vmv.v.i.
;; For vmv.v.i, we allow 2 following cases:
;;    1. (const_vector:VNx1QI repeat [
;;                (const_int:QI N)]), -15 <= N < 16.
;;    2. (const_vector:VNx1SF repeat [
;;                (const_double:SF 0.0 [0x0.0p+0])]).
(define_insn_and_split "@pred_mov<mode>"
  [(set (match_operand:V 0 "nonimmediate_operand"      "=vr,    vr,    vd,     m,    vr,    vr")
    (if_then_else:V
      (unspec:<VM>
        [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm, vmWc1,   Wc1,   Wc1")
         (match_operand 4 "vector_length_operand"    "   rK,    rK,    rK,    rK,    rK,    rK")
         (match_operand 5 "const_int_operand"        "    i,     i,     i,     i,     i,     i")
         (match_operand 6 "const_int_operand"        "    i,     i,     i,     i,     i,     i")
         (match_operand 7 "const_int_operand"        "    i,     i,     i,     i,     i,     i")
         (reg:SI VL_REGNUM)
         (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (match_operand:V 3 "vector_move_operand"       "    m,     m,     m,    vr,    vr, viWc0")
      (match_operand:V 2 "vector_merge_operand"      "    0,    vu,    vu,    vu,   vu0,   vu0")))]
  "TARGET_VECTOR"
  "@
   vle<sew>.v\t%0,%3%p1
   vle<sew>.v\t%0,%3
   vle<sew>.v\t%0,%3,%1.t
   vse<sew>.v\t%3,%0%p1
   vmv.v.v\t%0,%3
   vmv.v.i\t%0,%v3"
  "&& register_operand (operands[0], <MODE>mode)
   && register_operand (operands[3], <MODE>mode)
   && satisfies_constraint_vu (operands[2])"
  [(set (match_dup 0) (match_dup 3))]
  ""
  [(set_attr "type" "vlde,vlde,vlde,vste,vimov,vimov")
   (set_attr "mode" "<MODE>")])

;; Dedicated pattern for vse.v instruction since we can't reuse pred_mov pattern to include
;; memory operand as input which will produce inferior codegen.
(define_insn "@pred_store<mode>"
  [(set (match_operand:V 0 "memory_operand"                 "+m")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
	     (match_operand 3 "vector_length_operand"    "   rK")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:V 2 "register_operand"         "    vr")
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "vse<sew>.v\t%2,%0%p1"
  [(set_attr "type" "vste")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type") (symbol_ref "riscv_vector::NONVLMAX"))
   (set_attr "vl_op_idx" "3")])

;; vlm.v/vsm.v/vmclr.m/vmset.m.
;; constraint alternative 0 match vlm.v.
;; constraint alternative 1 match vsm.v.
;; constraint alternative 3 match vmclr.m.
;; constraint alternative 4 match vmset.m.
(define_insn_and_split "@pred_mov<mode>"
  [(set (match_operand:VB 0 "nonimmediate_operand"       "=vr,   m,  vr,  vr,  vr")
	(if_then_else:VB
	  (unspec:VB
	    [(match_operand:VB 1 "vector_mask_operand"   "Wc1, Wc1, Wc1, Wc1, Wc1")
	     (match_operand 4 "vector_length_operand"    " rK,  rK,  rK,  rK,  rK")
	     (match_operand 5 "const_int_operand"        "  i,   i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:VB 3 "vector_move_operand"      "  m,  vr,  vr, Wc0, Wc1")
	  (match_operand:VB 2 "vector_merge_operand"     " vu,  vu,  vu,  vu,  vu")))]
  "TARGET_VECTOR"
  "@
   vlm.v\t%0,%3
   vsm.v\t%3,%0
   #
   vmclr.m\t%0
   vmset.m\t%0"
  "&& register_operand (operands[0], <MODE>mode)
   && register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (match_dup 3))]
  ""
  [(set_attr "type" "vldm,vstm,vimov,vmalu,vmalu")
   (set_attr "mode" "<MODE>")])

;; Dedicated pattern for vsm.v instruction since we can't reuse pred_mov pattern to include
;; memory operand as input which will produce inferior codegen.
(define_insn "@pred_store<mode>"
  [(set (match_operand:VB 0 "memory_operand"            "+m")
	(if_then_else:VB
	  (unspec:VB
	    [(match_operand:VB 1 "vector_mask_operand" "Wc1")
	     (match_operand 3 "vector_length_operand"  " rK")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:VB 2 "register_operand"       " vr")
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "vsm.v\t%2,%0"
  [(set_attr "type" "vstm")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type") (symbol_ref "riscv_vector::NONVLMAX"))
   (set_attr "vl_op_idx" "3")])

;; -------------------------------------------------------------------------------
;; ---- Predicated Broadcast
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.5. Vector Strided Instructions (zero stride)
;; - 11.16 Vector Integer Move Instructions (vmv.v.x)
;; - 13.16 Vector Floating-Point Move Instruction (vfmv.v.f)
;; -------------------------------------------------------------------------------

(define_insn "@pred_broadcast<mode>"
  [(set (match_operand:V 0 "register_operand"                 "=vr,  vr,  vr,  vr")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"      " Wc1, Wc1, vm, Wc1")
	     (match_operand 4 "vector_length_operand"         " rK,  rK,  rK,  rK")
	     (match_operand 5 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand 6 "const_int_operand"             "  i,   i,   i,   i")
	     (match_operand 7 "const_int_operand"             "  i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (vec_duplicate:V
	    (match_operand:<VEL> 3 "direct_broadcast_operand" "  r,   f, Wdm, Wdm"))
	  (match_operand:V 2 "vector_merge_operand"           "vu0, vu0, vu0, vu0")))]
  "TARGET_VECTOR"
  "@
   vmv.v.x\t%0,%3
   vfmv.v.f\t%0,%3
   vlse<sew>.v\t%0,%3,zero,%1.t
   vlse<sew>.v\t%0,%3,zero"
  [(set_attr "type" "vimov,vfmov,vlds,vlds")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated Strided loads/stores
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.5. Vector Strided Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_strided_load<mode>"
  [(set (match_operand:V 0 "register_operand"              "=vr,    vr,    vd")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm")
	     (match_operand 5 "vector_length_operand"    "   rK,    rK,    rK")
	     (match_operand 6 "const_int_operand"        "    i,     i,     i")
	     (match_operand 7 "const_int_operand"        "    i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V
	    [(match_operand:V 3 "memory_operand"         "    m,     m,     m")
	     (match_operand 4 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")] UNSPEC_STRIDED)
	  (match_operand:V 2 "vector_merge_operand"      "    0,    vu,    vu")))]
  "TARGET_VECTOR"
  "vlse<sew>.v\t%0,%3,%z4%p1"
  [(set_attr "type" "vlds")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_strided_store<mode>"
  [(set (match_operand:V 0 "memory_operand"                 "+m")
	(if_then_else:V
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:V
	    [(match_operand 2 "pmode_reg_or_0_operand"   "   rJ")
	     (match_operand:V 3 "register_operand"       "   vr")] UNSPEC_STRIDED)
	  (match_dup 0)))]
  "TARGET_VECTOR"
  "vsse<sew>.v\t%3,%0,%z2%p1"
  [(set_attr "type" "vsts")
   (set_attr "mode" "<MODE>")])

;; -------------------------------------------------------------------------------
;; ---- Predicated indexed loads/stores
;; -------------------------------------------------------------------------------
;; Includes:
;; - 7.6. Vector Indexed Instructions
;; -------------------------------------------------------------------------------

(define_insn "@pred_indexed_<order>load<VNX1_QHSD:mode><VNX1_QHSDI:mode>"
  [(set (match_operand:VNX1_QHSD 0 "register_operand"      "=&vr")
	(if_then_else:VNX1_QHSD
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX1_QHSD
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX1_QHSDI 4 "register_operand" " vr")] ORDER)
	  (match_operand:VNX1_QHSD 2 "vector_merge_operand" "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX1_QHSDI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX1_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX1_QHSD:mode><VNX1_QHSDI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX1_QHSDI 2 "register_operand" "  vr")
	   (match_operand:VNX1_QHSD 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX1_QHSDI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX1_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX2_QHSD:mode><VNX2_QHSDI:mode>"
  [(set (match_operand:VNX2_QHSD 0 "register_operand"      "=&vr")
	(if_then_else:VNX2_QHSD
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX2_QHSD
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX2_QHSDI 4 "register_operand" " vr")] ORDER)
	  (match_operand:VNX2_QHSD 2 "vector_merge_operand" "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX2_QHSDI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX2_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX2_QHSD:mode><VNX2_QHSDI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX2_QHSDI 2 "register_operand" "  vr")
	   (match_operand:VNX2_QHSD 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX2_QHSDI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX2_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX4_QHSD:mode><VNX4_QHSDI:mode>"
  [(set (match_operand:VNX4_QHSD 0 "register_operand"      "=&vr")
	(if_then_else:VNX4_QHSD
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX4_QHSD
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX4_QHSDI 4 "register_operand" " vr")] ORDER)
	  (match_operand:VNX4_QHSD 2 "vector_merge_operand" "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX4_QHSDI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX4_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX4_QHSD:mode><VNX4_QHSDI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX4_QHSDI 2 "register_operand" "  vr")
	   (match_operand:VNX4_QHSD 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX4_QHSDI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX4_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX8_QHSD:mode><VNX8_QHSDI:mode>"
  [(set (match_operand:VNX8_QHSD 0 "register_operand"      "=&vr")
	(if_then_else:VNX8_QHSD
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX8_QHSD
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX8_QHSDI 4 "register_operand" " vr")] ORDER)
	  (match_operand:VNX8_QHSD 2 "vector_merge_operand" "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX8_QHSDI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX8_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX8_QHSD:mode><VNX8_QHSDI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX8_QHSDI 2 "register_operand" "  vr")
	   (match_operand:VNX8_QHSD 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX8_QHSDI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX8_QHSD:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX16_QHS:mode><VNX16_QHSI:mode>"
  [(set (match_operand:VNX16_QHS 0 "register_operand"      "=&vr")
	(if_then_else:VNX16_QHS
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX16_QHS
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX16_QHSI 4 "register_operand" " vr")] ORDER)
	  (match_operand:VNX16_QHS 2 "vector_merge_operand" "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX16_QHSI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX16_QHS:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX16_QHS:mode><VNX16_QHSI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX16_QHSI 2 "register_operand" "  vr")
	   (match_operand:VNX16_QHS 3 "register_operand"  "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX16_QHSI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX16_QHS:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX32_QH:mode><VNX32_QHI:mode>"
  [(set (match_operand:VNX32_QH 0 "register_operand"      "=&vr")
	(if_then_else:VNX32_QH
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX32_QH
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX32_QHI 4 "register_operand"  " vr")] ORDER)
	  (match_operand:VNX32_QH 2 "vector_merge_operand"  "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX32_QHI:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX32_QH:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX32_QH:mode><VNX32_QHI:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX32_QHI 2 "register_operand"  "  vr")
	   (match_operand:VNX32_QH 3 "register_operand"   "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX32_QHI:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX32_QH:MODE>")])

(define_insn "@pred_indexed_<order>load<VNX64_Q:mode><VNX64_Q:mode>"
  [(set (match_operand:VNX64_Q 0 "register_operand"      "=&vr")
	(if_then_else:VNX64_Q
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  "vmWc1")
	     (match_operand 5 "vector_length_operand"     "   rK")
	     (match_operand 6 "const_int_operand"         "    i")
	     (match_operand 7 "const_int_operand"         "    i")
	     (match_operand 8 "const_int_operand"         "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (unspec:VNX64_Q
	    [(match_operand 3 "pmode_register_operand"    "    r")
	     (mem:BLK (scratch))
	     (match_operand:VNX64_Q 4 "register_operand"    " vr")] ORDER)
	  (match_operand:VNX64_Q 2 "vector_merge_operand"   "0vu")))]
  "TARGET_VECTOR"
  "vl<order>xei<VNX64_Q:sew>.v\t%0,(%3),%4%p1"
  [(set_attr "type" "vld<order>x")
   (set_attr "mode" "<VNX64_Q:MODE>")])

(define_insn "@pred_indexed_<order>store<VNX64_Q:mode><VNX64_Q:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "   rK")
	     (match_operand 5 "const_int_operand"        "    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (match_operand 1 "pmode_register_operand"      "   r")
	   (match_operand:VNX64_Q 2 "register_operand"    "  vr")
	   (match_operand:VNX64_Q 3 "register_operand"    "  vr")] ORDER))]
  "TARGET_VECTOR"
  "vs<order>xei<VNX64_Q:sew>.v\t%3,(%1),%2%p0"
  [(set_attr "type" "vst<order>x")
   (set_attr "mode" "<VNX64_Q:MODE>")])
