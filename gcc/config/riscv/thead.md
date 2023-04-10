;; Machine description for T-Head vendor extensions
;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; XTheadBa

(define_insn "*th_addsl<mode>4"
  [(set (match_operand:X 0 "register_operand" "=r")
	(plus:X (ashift:X (match_operand:X 1 "register_operand" "r")
			  (match_operand 2 "const_int_operand" "n"))
		(match_operand:X 3 "register_operand" "r")))]
  "TARGET_XTHEADBA
   && (INTVAL (operands[2]) >= 0) && (INTVAL (operands[2]) <= 3)"
  "th.addsl\t%0,%3,%1,%2"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

;; XTheadBb

(define_insn "*th_srri<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "r")
		     (match_operand 2 "const_int_operand" "n")))]
  "TARGET_XTHEADBB && (TARGET_64BIT || <MODE>mode == SImode)"
  {
    bool wform = TARGET_64BIT && (<MODE>mode == SImode);
    operands[2] = GEN_INT (INTVAL (operands[2])
                  & (GET_MODE_BITSIZE (<MODE>mode) - 1));
    return wform ? "th.srriw\t%0,%1,%2" : "th.srri\t%0,%1,%2";
  }
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_ext<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(sign_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			(match_operand 2 "const_int_operand")
			(match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "th.ext\t%0,%1,%2,%3";
}
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_extu<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			(match_operand 2 "const_int_operand")
			(match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "th.extu\t%0,%1,%2,%3";
}
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_clz<mode>2"
  [(set (match_operand:X 0 "register_operand" "=r")
	(clz:X (match_operand:X 1 "register_operand" "r")))]
  "TARGET_XTHEADBB"
  "th.ff1\t%0,%1"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*th_rev<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(bswap:GPR (match_operand:GPR 1 "register_operand" "r")))]
  "TARGET_XTHEADBB && (TARGET_64BIT || <MODE>mode == SImode)"
  {
    bool wform = TARGET_64BIT && (<MODE>mode == SImode);
    return wform ? "th.revw\t%0,%1" : "th.rev\t%0,%1";
  }
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

;; XTheadBs

(define_insn "*th_tst<mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			(const_int 1)
			(match_operand 2 "const_int_operand" "n")))]
  "TARGET_XTHEADBS && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
  "th.tst\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

;; XTheadCondMov

(define_insn "*th_cond_mov<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(if_then_else:GPR
	 (match_operator 4 "equality_operator"
		[(match_operand:GPR2 1 "register_operand" "r,r")
		 (const_int 0)])
	 (match_operand:GPR 2 "reg_or_0_operand" "rJ,0")
	 (match_operand:GPR 3 "reg_or_0_operand" "0,rJ")))]
  "TARGET_XTHEADCONDMOV"
{
  if (which_alternative == 0)
    return "th.mv%C4z\t%0,%z2,%1";

  /* Invert the condition and take else-block.  */
  rtx_code code = GET_CODE (operands[4]);
  code = (code == EQ) ? NE : EQ;
  operands[4] = gen_rtx_fmt_ee (code, VOIDmode, const0_rtx, const0_rtx);
  return "th.mv%C4z\t%0,%z3,%1";
}
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*th_cond_gpr_mov<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(if_then_else:GPR
	 (match_operand:GPR2 1 "register_operand" "r,r")
	 (match_operand:GPR 2 "reg_or_0_operand" "rJ,0")
	 (match_operand:GPR 3 "reg_or_0_operand" "0,rJ")))]
  "TARGET_XTHEADCONDMOV"
  "@
   th.mvnez\t%0,%z2,%1
   th.mveqz\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

;; XTheadFmv

;; In RV32, we lack fmv.x.d and fmv.d.x, but XTheadFmv has instructions
;; that cover this case.

(define_insn "th_fmv_hw_w_x"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:SI 1 "register_operand" "r")
                (match_operand:SI 2 "register_operand" "r")]
     UNSPEC_XTHEADFMV))]
  "!TARGET_64BIT && TARGET_XTHEADFMV"
  "fmv.w.x\t%0,%2\n\tth.fmv.hw.x\t%0,%1"
  [(set_attr "move_type" "move")
   (set_attr "mode" "DF")])

(define_insn "th_fmv_x_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:DF 1 "register_operand" "f")]
     UNSPEC_XTHEADFMV))]
  "!TARGET_64BIT && TARGET_XTHEADFMV"
  "fmv.x.w\t%0,%1"
  [(set_attr "move_type" "move")
   (set_attr "mode" "DF")])

(define_insn "th_fmv_x_hw"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:DF 1 "register_operand" "f")]
     UNSPEC_XTHEADFMV_HW))]
  "!TARGET_64BIT && TARGET_XTHEADFMV"
  "th.fmv.x.hw\t%0,%1"
  [(set_attr "move_type" "move")
   (set_attr "mode" "DF")])

;; XTheadMac

(define_insn "*th_mula<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	      (plus:X (mult:X (match_operand:X 1 "register_operand" "r")
			      (match_operand:X 2 "register_operand" "r"))
		      (match_operand:X 3 "register_operand" "0")))]
  "TARGET_XTHEADMAC"
  "th.mula\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")]
)

(define_insn "*th_mulawsi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "register_operand" "r"))
		   (match_operand:SI 3 "register_operand" "0"))))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulaw\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_mulawsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	      (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				(match_operand:SI 2 "register_operand" "r"))
		       (match_operand:SI 3 "register_operand" "0")))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulaw\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_maddhisi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (plus:SI
	    (mult:SI
	      (sign_extend:SI (match_operand:HI 1 "register_operand" " r"))
	      (sign_extend:SI (match_operand:HI 2 "register_operand" " r")))
	    (match_operand:SI 3 "register_operand" " 0")))]
  "TARGET_XTHEADMAC"
  "th.mulah\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_sextw_maddhisi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (plus:SI
	    (mult:SI
	      (sign_extend:SI (match_operand:HI 1 "register_operand" " r"))
	      (sign_extend:SI (match_operand:HI 2 "register_operand" " r")))
	    (match_operand:SI 3 "register_operand" " 0"))))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulah\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_muls<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	      (minus:X (match_operand:X 3 "register_operand" "0")
		       (mult:X (match_operand:X 1 "register_operand" "r")
			       (match_operand:X 2 "register_operand" "r"))))]
  "TARGET_XTHEADMAC"
  "th.muls\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")]
)

(define_insn "*th_mulswsi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (minus:SI (match_operand:SI 3 "register_operand" "0")
		    (mult:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand:SI 2 "register_operand" "r")))))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulsw\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_mulswsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 3 "register_operand" "0")
		  (mult:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulsw\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_msubhisi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 3 "register_operand" " 0")
	  (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" " r"))
	    (sign_extend:SI (match_operand:HI 2 "register_operand" " r")))))]
  "TARGET_XTHEADMAC"
  "th.mulsh\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

(define_insn "*th_sextw_msubhisi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (minus:SI (match_operand:SI 3 "register_operand" " 0")
	    (mult:SI
	      (sign_extend:SI (match_operand:HI 1 "register_operand" " r"))
	      (sign_extend:SI (match_operand:HI 2 "register_operand" " r"))))))]
  "TARGET_XTHEADMAC && TARGET_64BIT"
  "th.mulsh\\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")]
)

;; XTheadMemPair

;; MEMPAIR load 64/32 bit
(define_insn "*th_mempair_load_<GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(match_operand:GPR 1 "memory_operand" "m"))
   (set (match_operand:GPR 2 "register_operand" "=r")
	(match_operand:GPR 3 "memory_operand" "m"))]
  "TARGET_XTHEADMEMPAIR && reload_completed
   && th_mempair_operands_p (operands, true, <GPR:MODE>mode)"
  { return th_mempair_output_move (operands, true, <GPR:MODE>mode, UNKNOWN); }
  [(set_attr "move_type" "load")
   (set_attr "mode" "<GPR:MODE>")])

;; MEMPAIR store 64/32 bit
(define_insn "*th_mempair_store_<GPR:mode>2"
  [(set (match_operand:GPR 0 "memory_operand" "=m")
	(match_operand:GPR 1 "register_operand" "r"))
   (set (match_operand:GPR 2 "memory_operand" "=m")
	(match_operand:GPR 3 "register_operand" "r"))]
  "TARGET_XTHEADMEMPAIR && reload_completed
   && th_mempair_operands_p (operands, false, <GPR:MODE>mode)"
  { return th_mempair_output_move (operands, false, <GPR:MODE>mode, UNKNOWN); }
  [(set_attr "move_type" "store")
   (set_attr "mode" "<GPR:MODE>")])

;; MEMPAIR load DI extended signed SI
(define_insn "*th_mempair_load_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "memory_operand" "m")))
   (set (match_operand:DI 2 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "m")))]
  "TARGET_XTHEADMEMPAIR && TARGET_64BIT && reload_completed
   && th_mempair_operands_p (operands, true, SImode)"
  { return th_mempair_output_move (operands, true, SImode, SIGN_EXTEND); }
  [(set_attr "move_type" "load")
   (set_attr "mode" "DI")
   (set_attr "length" "8")])

;; MEMPAIR load DI extended unsigned SI
(define_insn "*th_mempair_load_zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "memory_operand" "m")))
   (set (match_operand:DI 2 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "m")))]
  "TARGET_XTHEADMEMPAIR && TARGET_64BIT && reload_completed
   && th_mempair_operands_p (operands, true, SImode)"
  { return th_mempair_output_move (operands, true, SImode, ZERO_EXTEND); }
  [(set_attr "move_type" "load")
   (set_attr "mode" "DI")
   (set_attr "length" "8")])
