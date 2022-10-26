;; Machine description for RISC-V 'V' Extension for GNU compiler.
;; Copyright (C) 2022-2022 Free Software Foundation, Inc.
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

(define_c_enum "unspec" [
  UNSPEC_VSETVL
  UNSPEC_VUNDEF
  UNSPEC_VPREDICATE
])

;; -----------------------------------------------------------------
;; ---- Miscellaneous Operations
;; -----------------------------------------------------------------

(define_insn "vundefined<mode>"
  [(set (match_operand:V 0 "register_operand" "=vr")
	(unspec:V [(const_int 0)] UNSPEC_VUNDEF))]
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
  if (riscv_vector::legitimize_move (operands[0], operands[1], <VM>mode))
    DONE;
})

;; This pattern is used for code-gen for whole register load/stores.
;; Also applicable for all register moves.
;; Fractional vector modes load/store are not allowed to match this pattern.
;; Mask modes load/store are not allowed to match this pattern.
(define_insn "*mov<mode>"
  [(set (match_operand:V 0 "reg_or_mem_operand" "=vr,m,vr")
	(match_operand:V 1 "reg_or_mem_operand" "m,vr,vr"))]
  "TARGET_VECTOR && ((register_operand (operands[0], <MODE>mode)
		      && register_operand (operands[1], <MODE>mode))
   || known_ge (GET_MODE_SIZE (<MODE>mode), BYTES_PER_RISCV_VECTOR))"
  "@
   vl%m1re<sew>.v\t%0,%1
   vs%m1r.v\t%1,%0
   vmv%m1r.v\t%0,%1"
  [(set_attr "type" "vldr,vstr,vmov")
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
	(match_operand:VB 1 "register_operand" "vr"))]
  "TARGET_VECTOR"
  "vmv1r.v\t%0,%1"
  [(set_attr "type" "vmov")
   (set_attr "mode" "<MODE>")])

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
   (set_attr "mode" "<MODE>")])

;; We keep it as no side effects before reload_completed.
;; In this case, we can gain benefits from different GCC
;; internal PASS such as cprop, fwprop, combine,...etc.

;; Then recover it for "insert-vsetvl" and "sched2" PASS
;; in order to get correct codegen.
(define_insn_and_split "@vsetvl<mode>_no_side_effects"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "csr_operand" "rK")
		   (match_operand 2 "const_int_operand" "i")
		   (match_operand 3 "const_int_operand" "i")
		   (match_operand 4 "const_int_operand" "i")
		   (match_operand 5 "const_int_operand" "i")] UNSPEC_VSETVL))]
  "TARGET_VECTOR"
  "#"
  "&& reload_completed"
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
   (set_attr "mode" "<MODE>")])

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
(define_insn "@pred_mov<mode>"
  [(set (match_operand:V 0 "nonimmediate_operand"        "=vd,  vr,     m,    vr,    vr")
        (if_then_else:V
          (unspec:<VM>
            [(match_operand:<VM> 1 "vector_mask_operand" " vm, Wc1, vmWc1, vmWc1,   Wc1")
             (match_operand 4 "vector_length_operand"    " rK,  rK,    rK,    rK,    rK")
             (match_operand 5 "const_int_operand"        "  i,   i,     i,     i,     i")
             (match_operand 6 "const_int_operand"        "  i,   i,     i,     i,     i")
             (reg:SI VL_REGNUM)
             (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
          (match_operand:V 3 "vector_move_operand"       "  m,   m,    vr,    vr, viWc0")
          (match_operand:V 2 "vector_merge_operand"      "  0,  vu,     0,   vu0,   vu0")))]
  "TARGET_VECTOR"
  "@
   vle<sew>.v\t%0,%3%p1
   vle<sew>.v\t%0,%3%p1
   vse<sew>.v\t%3,%0%p1
   vmv.v.v\t%0,%3
   vmv.v.i\t%0,v%3"
  [(set_attr "type" "vlde,vlde,vste,vimov,vimov")
   (set_attr "mode" "<MODE>")])

;; vlm.v/vsm.v/vmclr.m/vmset.m.
;; constraint alternative 0 match vlm.v.
;; constraint alternative 2 match vsm.v.
;; constraint alternative 3 match vmclr.m.
;; constraint alternative 4 match vmset.m.
(define_insn "@pred_mov<mode>"
  [(set (match_operand:VB 0 "nonimmediate_operand"       "=vr,   m,  vr,  vr")
        (if_then_else:VB
          (unspec:VB
            [(match_operand:VB 1 "vector_mask_operand"   "Wc1, Wc1, Wc1, Wc1")
             (match_operand 4 "vector_length_operand"    " rK,  rK,  rK,  rK")
             (match_operand 5 "const_int_operand"        "  i,   i,   i,   i")
             (match_operand 6 "const_int_operand"        "  i,   i,   i,   i")
             (reg:SI VL_REGNUM)
             (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
          (match_operand:VB 3 "vector_move_operand"      "  m,  vr, Wc0, Wc1")
          (match_operand:VB 2 "vector_merge_operand"     " vu,   0,  vu,  vu")))]
  "TARGET_VECTOR"
  "@
   vlm.v\t%0,%3
   vsm.v\t%3,%0
   vmclr.m\t%0
   vmset.m\t%0"
  [(set_attr "type" "vldm,vstm,vmalu,vmalu")
   (set_attr "mode" "<MODE>")])
