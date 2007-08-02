;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) 
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; This includes expands for all the intrinsics.
;; spu_expand_builtin looks at the mode of match_operand.


;; load/store

(define_expand "spu_lqd"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (mem:TI (and:SI (plus:SI (match_operand:SI 1 "spu_reg_operand" "")
				 (match_operand:SI 2 "spu_nonmem_operand" ""))
		        (const_int -16))))]
  ""
  {
    if (GET_CODE (operands[2]) == CONST_INT
	&& (INTVAL (operands[2]) & 15) != 0)
      operands[2] = GEN_INT (INTVAL (operands[2]) & -16);
    if (GET_CODE (operands[2]) != CONST_INT)
      {
	rtx op2 = operands[2];
	operands[2] = force_reg (Pmode, operands[2]);
	if (!ALIGNED_SYMBOL_REF_P (op2))
	  emit_insn (gen_andsi3 (operands[2], operands[2], GEN_INT (-16)));
      }
  })

(define_expand "spu_lqx"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (mem:TI (and:SI (plus:SI (match_operand:SI 1 "spu_reg_operand" "")
                                 (match_operand:SI 2 "spu_reg_operand" ""))
                        (const_int -16))))]
  ""
  "")

(define_expand "spu_lqa"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (mem:TI (and:SI (match_operand:SI 1 "immediate_operand" "")
                        (const_int -16))))]
  ""
  {
    if (GET_CODE (operands[1]) == CONST_INT
	&& (INTVAL (operands[1]) & 15) != 0)
      operands[1] = GEN_INT (INTVAL (operands[1]) & -16);
  })

(define_expand "spu_lqr"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
	(mem:TI (and:SI (match_operand:SI 1 "address_operand" "")
			(const_int -16))))]
  ""
  "")

(define_expand "spu_stqd"
  [(set (mem:TI (and:SI (plus:SI (match_operand:SI 1 "spu_reg_operand" "")
				 (match_operand:SI 2 "spu_nonmem_operand" ""))
		        (const_int -16)))
        (match_operand:TI 0 "spu_reg_operand" "r,r"))]
  ""
  {
    if (GET_CODE (operands[2]) == CONST_INT
	&& (INTVAL (operands[2]) & 15) != 0)
      operands[2] = GEN_INT (INTVAL (operands[2]) & -16);
    if (GET_CODE (operands[2]) != CONST_INT)
      {
	rtx op2 = operands[2];
	operands[2] = force_reg (Pmode, operands[2]);
	if (!ALIGNED_SYMBOL_REF_P (op2))
	  emit_insn (gen_andsi3 (operands[2], operands[2], GEN_INT (-16)));
      }
  })

(define_expand "spu_stqx"
  [(set (mem:TI (and:SI (plus:SI (match_operand:SI 1 "spu_reg_operand" "")
				 (match_operand:SI 2 "spu_reg_operand" ""))
		        (const_int -16)))
        (match_operand:TI 0 "spu_reg_operand" "r"))]
  ""
  "")

(define_expand "spu_stqa"
  [(set (mem:TI (and:SI (match_operand:SI 1 "immediate_operand" "")
			(const_int -16)))
        (match_operand:TI 0 "spu_reg_operand" "r"))]
  ""
  {
    if (GET_CODE (operands[1]) == CONST_INT
	&& (INTVAL (operands[1]) & 15) != 0)
      operands[1] = GEN_INT (INTVAL (operands[1]) & -16);
  })

(define_expand "spu_stqr"
    [(set (mem:TI (and:SI (match_operand:SI 1 "address_operand" "")
			  (const_int -16)))
	  (match_operand:TI 0 "spu_reg_operand" ""))]
  ""
  "")


;; generate control word

(define_expand "spu_cbx"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (unspec:TI [(match_operand:SI 1 "spu_reg_operand" "")
                    (match_operand:SI 2 "spu_nonmem_operand" "")
                    (const_int 1)] UNSPEC_CPAT))]
  ""
  "")

(define_expand "spu_chx"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (unspec:TI [(match_operand:SI 1 "spu_reg_operand" "")
                    (match_operand:SI 2 "spu_nonmem_operand" "")
                    (const_int 2)] UNSPEC_CPAT))]
  ""
  "")

(define_expand "spu_cwx"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (unspec:TI [(match_operand:SI 1 "spu_reg_operand" "")
                    (match_operand:SI 2 "spu_nonmem_operand" "")
                    (const_int 4)] UNSPEC_CPAT))]
  ""
  "")

(define_expand "spu_cdx"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
        (unspec:TI [(match_operand:SI 1 "spu_reg_operand" "")
                    (match_operand:SI 2 "spu_nonmem_operand" "")
                    (const_int 8)] UNSPEC_CPAT))]
  ""
  "")



;; Constant formation

(define_expand "spu_ilhu"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (const_vector:V4SI [(match_operand:SI 1 "immediate_operand" "")]))]
  ""
  "{ emit_insn(gen_movv4si(operands[0], spu_const(V4SImode, (INTVAL(operands[1]) << 16))));
     DONE;
   }")


;; integer subtract
(define_expand "spu_sfh"
  [(set (match_operand:V8HI 0 "spu_reg_operand" "")
        (minus:V8HI (match_operand:V8HI 2 "spu_nonmem_operand" "")
                    (match_operand:V8HI 1 "spu_reg_operand" "")))]
  ""
  "")

(define_expand "spu_sf"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (minus:V4SI (match_operand:V4SI 2 "spu_nonmem_operand" "")
                    (match_operand:V4SI 1 "spu_reg_operand" "")))]
  ""
  "")

(define_expand "spu_sfx"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (unspec:V4SI [(match_operand:V4SI 2 "spu_reg_operand" "")
		      (match_operand:V4SI 1 "spu_reg_operand" "")
		      (match_operand:V4SI 3 "spu_reg_operand" "")] UNSPEC_SFX))]
  ""
  "")

(define_expand "spu_bg"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (unspec:V4SI [(match_operand:V4SI 2 "spu_reg_operand" "")
		      (match_operand:V4SI 1 "spu_reg_operand" "")] UNSPEC_BG))]
  ""
  "")

(define_expand "spu_bgx"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (unspec:V4SI [(match_operand:V4SI 2 "spu_reg_operand" "")
		      (match_operand:V4SI 1 "spu_reg_operand" "")
		      (match_operand:V4SI 3 "spu_reg_operand" "")] UNSPEC_BGX))]
  ""
  "")

;; integer multiply
(define_insn "spu_mpy"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r,r")
        (mult:V4SI
	  (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "spu_reg_operand" "r,r")
	      (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))
          (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "spu_arith_operand" "r,B")
	      (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))))]
  ""
  "@
   mpy\t%0,%1,%2
   mpyi\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyu"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r,r")
        (mult:V4SI
	  (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "spu_reg_operand" "r,r")
	      (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))
          (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "spu_arith_operand" "r,B")
	      (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))))]
  ""
  "@
   mpyu\t%0,%1,%2
   mpyui\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpya"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "spu_reg_operand" "r")
		(parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "spu_reg_operand" "r")
		(parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)]))))
	(match_operand:V4SI 3 "spu_reg_operand" "r")))]
  ""
  "mpya\t%0,%1,%2,%3"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyh"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (ashift:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "spu_reg_operand" "r")
		(parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "spu_reg_operand" "r")
		(parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)]))))
	  (const_vector:V4SI [(const_int 16)(const_int 16)(const_int 16)(const_int 16)])))]
  ""
  "mpyh\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpys"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (ashiftrt:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "spu_reg_operand" "r")
		(parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "spu_reg_operand" "r")
		(parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)]))))
	  (const_vector:V4SI [(const_int 16)(const_int 16)(const_int 16)(const_int 16)])))]
  ""
  "mpys\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyhhu"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(mult:V4SI
	  (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "spu_reg_operand" "r")
	      (parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))
	  (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "spu_reg_operand" "r")
	      (parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))))]
  ""
  "mpyhhu\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyhh"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(mult:V4SI
	  (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "spu_reg_operand" "r")
	      (parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))
	  (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "spu_reg_operand" "r")
	      (parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))))]
  ""
  "mpyhh\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyhhau"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (plus:V4SI
	  (mult:V4SI
	    (zero_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "spu_reg_operand" "r")
		(parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))
	    (zero_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "spu_reg_operand" "r")
		(parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)]))))
	  (match_operand:V4SI 3 "spu_reg_operand" "0")))]
  ""
  "mpyhhau\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_mpyhha"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "spu_reg_operand" "r")
		(parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "spu_reg_operand" "r")
		(parallel [(const_int 0)(const_int 2)(const_int 4)(const_int 6)]))))
	  (match_operand:V4SI 3 "spu_reg_operand" "0")))]
  ""
  "mpyhha\t%0,%1,%2"
  [(set_attr "type" "fp7")])

;; form select mask
(define_insn "spu_fsmb"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r,r")
        (unspec:V16QI [(match_operand:SI 1 "spu_nonmem_operand" "r,MN")] UNSPEC_FSMB))]
  ""
  "@
  fsmb\t%0,%1
  fsmbi\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "spu_fsmh"
  [(set (match_operand:V8HI 0 "spu_reg_operand" "=r")
        (unspec:V8HI [(match_operand:SI 1 "spu_reg_operand" "r")] UNSPEC_FSMH))]
  ""
  "fsmh\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "spu_fsm"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec:V4SI [(match_operand:SI 1 "spu_reg_operand" "r")] UNSPEC_FSM))]
  ""
  "fsm\t%0,%1"
  [(set_attr "type" "shuf")])


;; gather bits
(define_insn "spu_gbb"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec:V4SI [(match_operand:V16QI 1 "spu_reg_operand" "r")] UNSPEC_GBB))]
  ""
  "gbb\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "spu_gbh"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec:V4SI [(match_operand:V8HI 1 "spu_reg_operand" "r")] UNSPEC_GBH))]
  ""
  "gbh\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "spu_gb"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec:V4SI [(match_operand:V4SI 1 "spu_reg_operand" "r")] UNSPEC_GB))]
  ""
  "gb\t%0,%1"
  [(set_attr "type" "shuf")])

;; misc byte operations
(define_insn "spu_avgb"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
        (unspec:V16QI [(match_operand:V16QI 1 "spu_reg_operand" "r")
		       (match_operand:V16QI 2 "spu_reg_operand" "r")] UNSPEC_AVGB))]
  ""
  "avgb\t%0,%1,%2"
  [(set_attr "type" "fxb")])

(define_insn "spu_absdb"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
        (unspec:V16QI [(match_operand:V16QI 1 "spu_reg_operand" "r")
		       (match_operand:V16QI 2 "spu_reg_operand" "r")] UNSPEC_ABSDB))]
  ""
  "absdb\t%0,%1,%2"
  [(set_attr "type" "fxb")])

(define_insn "spu_sumb"
  [(set (match_operand:V8HI 0 "spu_reg_operand" "=r")
        (unspec:V8HI [(match_operand:V16QI 1 "spu_reg_operand" "r")
		      (match_operand:V16QI 2 "spu_reg_operand" "r")] UNSPEC_SUMB))]
  ""
  "sumb\t%0,%1,%2"
  [(set_attr "type" "fxb")])

;; sign extend
(define_insn "spu_xsbh"
  [(set (match_operand:V8HI 0 "spu_reg_operand" "=r")
        (sign_extend:V8HI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "spu_reg_operand" "r")
	    (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)
	               (const_int 9)(const_int 11)(const_int 13)(const_int 15)]))))]
  ""
  "xsbh\t%0,%1")

(define_insn "spu_xshw"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (sign_extend:V4SI
	  (vec_select:V4HI
	    (match_operand:V8HI 1 "spu_reg_operand" "r")
	    (parallel [(const_int 1)(const_int 3)(const_int 5)(const_int 7)]))))]
  ""
  "xshw\t%0,%1")

(define_insn "spu_xswd"
  [(set (match_operand:V2DI 0 "spu_reg_operand" "=r")
        (sign_extend:V2DI
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "spu_reg_operand" "r")
	    (parallel [(const_int 1)(const_int 3)]))))]
  ""
  "xswd\t%0,%1")

;; or across

(define_insn "spu_orx"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(unspec:V4SI [(match_operand:V4SI 1 "spu_reg_operand" "r")] UNSPEC_ORX))]
  ""
  "orx\t%0,%1")


;; compare & halt
(define_insn "spu_heq"
  [(unspec_volatile [(match_operand:SI 0 "spu_reg_operand" "r,r")
	             (match_operand:SI 1 "spu_nonmem_operand" "r,K")] UNSPEC_HEQ)]
  ""
  "@
  heq\t%0,%1
  heqi\t%0,%1")

(define_insn "spu_hgt"
  [(unspec_volatile [(match_operand:SI 0 "spu_reg_operand" "r,r")
	             (match_operand:SI 1 "spu_nonmem_operand" "r,K")] UNSPEC_HGT)]
  ""
  "@
  hgt\t%0,%1
  hgti\t%0,%1")

(define_insn "spu_hlgt"
  [(unspec_volatile [(match_operand:SI 0 "spu_reg_operand" "r,r")
	             (match_operand:SI 1 "spu_nonmem_operand" "r,K")] UNSPEC_HLGT)]
  ""
  "@
  hlgt\t%0,%1
  hlgti\t%0,%1")

;; branches

;; The description below hides the fact that bisled conditionally
;; executes the call depending on the value in channel 0.  This was 
;; done so that the description would conform to the format of a call 
;; insn.  Otherwise (if this were not part of call insn), the link 
;; register, $lr, would not be saved/restored in the prologue/epilogue.

(define_insn "spu_bisled"
  [(parallel
    [(call (mem:QI (match_operand:SI 0 "spu_reg_operand" "r"))
            (const_int 0))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))
     (use (match_operand:SI 1 "address_operand" ""))
     (use (const_int 0))])]
  ""
  "bisled\t$lr,%0"
  [(set_attr "type" "br")])

(define_insn "spu_bisledd"
  [(parallel
    [(call (mem:QI (match_operand:SI 0 "spu_reg_operand" "r"))
            (const_int 0))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))
     (use (match_operand:SI 1 "address_operand" ""))
     (use (const_int 1))])]
  ""
  "bisledd\t$lr,%0"
  [(set_attr "type" "br")])

(define_insn "spu_bislede"
  [(parallel
    [(call (mem:QI (match_operand:SI 0 "spu_reg_operand" "r"))
            (const_int 0))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))
     (use (match_operand:SI 1 "address_operand" ""))
     (use (const_int 2))])]
  ""
  "bislede\t$lr,%0"
  [(set_attr "type" "br")])

;; float convert
(define_insn "spu_csflt"
  [(set (match_operand:V4SF 0 "spu_reg_operand" "=r")
	(unspec:V4SF [(match_operand:V4SI 1 "spu_reg_operand" "r")
		      (match_operand:SI 2 "immediate_operand" "K")] UNSPEC_CSFLT ))]
  ""
  "csflt\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_cflts"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(unspec:V4SI [(match_operand:V4SF 1 "spu_reg_operand" "r")
                      (match_operand:SI 2 "immediate_operand" "J")] UNSPEC_CFLTS ))]
  ""
  "cflts\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_cuflt"
  [(set (match_operand:V4SF 0 "spu_reg_operand" "=r")
	(unspec:V4SF [(match_operand:V4SI 1 "spu_reg_operand" "r")
		      (match_operand:SI 2 "immediate_operand" "K")] UNSPEC_CUFLT ))]
  ""
  "cuflt\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "spu_cfltu"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(unspec:V4SI [(match_operand:V4SF 1 "spu_reg_operand" "r")
		      (match_operand:SI 2 "immediate_operand" "J")] UNSPEC_CFLTU ))]
  ""
  "cfltu\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_expand "spu_frds"
   [(set (match_operand:V4SF 0 "spu_reg_operand" "")
         (vec_select:V4SF
	   (vec_concat:V4SF
	     (float_truncate:V2SF (match_operand:V2DF 1 "spu_reg_operand" ""))
	     (match_dup:V2SF 2))
	   (parallel [(const_int 0)(const_int 2)(const_int 1)(const_int 3)])))]
  ""
  "operands[2] = spu_const(V2SFmode, 0);")

(define_insn "_frds"
   [(set (match_operand:V4SF 0 "spu_reg_operand" "=r")
        (vec_select:V4SF
	  (vec_concat:V4SF
	    (float_truncate:V2SF (match_operand:V2DF 1 "spu_reg_operand" "r"))
	    (match_operand:V2SF 2 "vec_imm_operand" "i"))
	  (parallel [(const_int 0)(const_int 2)(const_int 1)(const_int 3)])))]
  ""
  "frds\t%0,%1"
  [(set_attr "type" "fpd")])

(define_insn "spu_fesd"
  [(set (match_operand:V2DF 0 "spu_reg_operand" "=r")
        (float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "spu_reg_operand" "r")
	      (parallel [(const_int 0)(const_int 2)]))))]
  ""
  "fesd\t%0,%1"
  [(set_attr "type" "fpd")])

;; control
(define_insn "spu_stop"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "M")] UNSPEC_STOP)]
  ""
  "stop\t%0"
  [(set_attr "type" "br")])

(define_insn "spu_stopd"
  [(unspec_volatile [(match_operand:SI 0 "spu_reg_operand" "r")
		     (match_operand:SI 1 "spu_reg_operand" "r")
		     (match_operand:SI 2 "spu_reg_operand" "r")] UNSPEC_STOPD)]
  ""
  "stopd\t%0,%1,%2"
  [(set_attr "type" "br")])

;; interrupt disable/enable
(define_expand "spu_idisable"
  [(parallel
    [(unspec_volatile [(const_int 0)] UNSPEC_SET_INTR)
     (clobber (match_dup:SI 0))
     (clobber (mem:BLK (scratch)))])]
  ""
  "operands[0] = gen_reg_rtx (SImode);")

(define_expand "spu_ienable"
  [(parallel
    [(unspec_volatile [(const_int 1)] UNSPEC_SET_INTR)
     (clobber (match_dup:SI 0))
     (clobber (mem:BLK (scratch)))])]
  ""
  "operands[0] = gen_reg_rtx (SImode);")

(define_insn "set_intr"
  [(unspec_volatile [(match_operand 1 "const_int_operand" "i")] UNSPEC_SET_INTR)
   (clobber (match_operand:SI 0 "spu_reg_operand" "=&r"))
   (clobber (mem:BLK (scratch)))]
  "! flag_pic"
  "ila\t%0,.+8\;bi%I1\t%0"
  [(set_attr "length" "8")
   (set_attr "type" "multi0")])

(define_insn "set_intr_pic"
  [(unspec_volatile [(match_operand 1 "const_int_operand" "i")] UNSPEC_SET_INTR)
   (clobber (match_operand:SI 0 "spu_reg_operand" "=&r"))
   (clobber (mem:BLK (scratch)))]
  "flag_pic"
  "brsl\t%0,.+4\;ai\t%0,%0,8\;bi%I1\t%0"
  [(set_attr "length" "12")
   (set_attr "type" "multi1")])

(define_insn "set_intr_cc"
  [(cond_exec (match_operator 1 "branch_comparison_operator"
		[(match_operand 2 "spu_reg_operand" "r")
		 (const_int 0)])
              (parallel [(unspec_volatile [(match_operand:SI 3 "const_int_operand" "i")] UNSPEC_SET_INTR)
                         (clobber (match_operand:SI 0 "spu_reg_operand" "=&r"))
			 (clobber (mem:BLK (scratch)))]))]
  "! flag_pic"
  "ila\t%0,.+8\;bi%b2%b1z%I3\t%2,%0"
  [(set_attr "length" "8")
   (set_attr "type" "multi0")])

(define_insn "set_intr_cc_pic"
  [(cond_exec (match_operator 1 "branch_comparison_operator"
		[(match_operand 2 "spu_reg_operand" "r")
		 (const_int 0)])
              (parallel [(unspec_volatile [(match_operand:SI 3 "const_int_operand" "i")] UNSPEC_SET_INTR)
                         (clobber (match_operand:SI 0 "spu_reg_operand" "=&r"))
			 (clobber (mem:BLK (scratch)))]))]
  "flag_pic"
  "brsl\t%0,.+4\;ai\t%0,%0,8\;bi%b2%b1z%I3\t%2,%0"
  [(set_attr "length" "12")
   (set_attr "type" "multi1")])

(define_insn "set_intr_return"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "i")] UNSPEC_SET_INTR)
   (return)]
  ""
  "bi%I0\t$lr"
  [(set_attr "type" "br")])

(define_peephole2
  [(parallel
    [(unspec_volatile [(match_operand:SI 0 "const_int_operand")] UNSPEC_SET_INTR)
     (clobber (match_operand:SI 1 "spu_reg_operand"))
     (clobber (mem:BLK (scratch)))])
   (use (reg:SI 0))
   (return)]
  ""
  [(use (reg:SI 0))
   (parallel
    [(unspec_volatile [(match_dup:SI 0)] UNSPEC_SET_INTR)
     (return)])]
  "")

;; special purpose registers
(define_insn "spu_fscrrd"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:V4SI [(const_int 6)] UNSPEC_FSCRRD))]
  ""
  "fscrrd\t%0"
  [(set_attr "type" "spr")])

(define_insn "spu_fscrwr"
  [(unspec_volatile [(match_operand:V4SI 0 "spu_reg_operand" "r")] UNSPEC_FSCRWR)]
  ""
  "fscrwr\t$0,%0"
  [(set_attr "type" "spr")])

(define_insn "spu_mfspr"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "J")] UNSPEC_MFSPR))]
  ""
  "mfspr\t%0,$sp%1"
  [(set_attr "type" "spr")])

(define_insn "spu_mtspr"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "J")
	             (match_operand:SI 1 "spu_reg_operand" "r")] UNSPEC_MTSPR)]
  ""
  "mtspr\t$sp%0,%1"
  [(set_attr "type" "spr")])

;; channels
(define_expand "spu_rdch"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "")
        (unspec_volatile:V4SI [(match_operand:SI 1 "immediate_operand" "")] UNSPEC_RDCH))]
  ""
  "{
    if (spu_safe_dma (INTVAL (operands[1])))
      {
        emit_insn (gen_spu_rdch_clobber (operands[0], operands[1]));
        DONE;
      }
   }")

(define_expand "spu_rchcnt"
  [(set (match_operand:SI 0 "spu_reg_operand" "")
        (unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "")] UNSPEC_RCHCNT))]
  ""
  "{
    if (spu_safe_dma (INTVAL (operands[1])))
      {
        emit_insn (gen_spu_rchcnt_clobber (operands[0], operands[1]));
        DONE;
      }
   }")

(define_expand "spu_wrch"
   [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "")
 	              (match_operand:V4SI 1 "spu_reg_operand" "")] UNSPEC_WRCH)]
   ""
  "{
    if (spu_safe_dma (INTVAL (operands[0])))
      {
        emit_insn (gen_spu_wrch_clobber (operands[0], operands[1]));
        DONE;
      }
   }")

(define_insn "spu_rdch_noclobber"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:V4SI [(match_operand:SI 1 "immediate_operand" "J")] UNSPEC_RDCH))]
  ""
  "rdch\t%0,$ch%1"
  [(set_attr "type" "spr")])

(define_insn "spu_rchcnt_noclobber"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "J")] UNSPEC_RCHCNT))]
  ""
  "rchcnt\t%0,$ch%1"
  [(set_attr "type" "spr")])

(define_insn "spu_wrch_noclobber"
   [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "J")
 	              (match_operand:V4SI 1 "spu_reg_operand" "r")] UNSPEC_WRCH)]
   ""
   "wrch\t$ch%0,%1"
   [(set_attr "type" "spr")])

(define_insn "spu_rdch_clobber"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:V4SI [(match_operand:SI 1 "immediate_operand" "J")] UNSPEC_RDCH))
    (clobber (mem:BLK (scratch)))]
  ""
  "rdch\t%0,$ch%1"
  [(set_attr "type" "spr")])

(define_insn "spu_rchcnt_clobber"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "J")] UNSPEC_RCHCNT))
    (clobber (mem:BLK (scratch)))]
  ""
  "rchcnt\t%0,$ch%1"
  [(set_attr "type" "spr")])

(define_insn "spu_wrch_clobber"
   [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "J")
 	              (match_operand:V4SI 1 "spu_reg_operand" "r")] UNSPEC_WRCH)
    (clobber (mem:BLK (scratch)))]
   ""
   "wrch\t$ch%0,%1"
   [(set_attr "type" "spr")])

(define_expand "spu_splats" 
  [(set (match_operand 0 "spu_reg_operand" "")
        (vec_duplicate (match_operand 1 "spu_nonmem_operand" "")))]
  ""
  {
    spu_builtin_splats(operands);
    DONE;
  })

(define_expand "spu_extract"
  [(set (match_operand 0 "spu_reg_operand" "")
	(unspec [(match_operand 1 "spu_reg_operand" "")
		 (match_operand 2 "spu_nonmem_operand" "")] 0))]
  ""
  {
    spu_builtin_extract (operands);
    DONE;
  })

(define_expand "spu_insert"
  [(set (match_operand 0 "spu_reg_operand" "")
        (unspec [(match_operand 1 "spu_reg_operand" "")
                 (match_operand 2 "spu_reg_operand" "")
                 (match_operand:SI 3 "spu_nonmem_operand" "")] 0))] 
  ""
  {
    spu_builtin_insert(operands);
    DONE;
  })

(define_expand "spu_promote"
  [(set (match_operand 0 "spu_reg_operand" "")
        (unspec [(match_operand 1 "spu_reg_operand" "")
                 (match_operand:SI 2 "immediate_operand" "")] 0))] 
  ""
  {
    spu_builtin_promote(operands);
    DONE;
  })

;; Currently doing nothing with this but expanding its args.
(define_expand "spu_align_hint"
  [(unspec [(match_operand:SI 0 "address_operand" "")
            (match_operand:SI 1 "immediate_operand" "")
            (match_operand:SI 2 "immediate_operand" "")] 0)]
  ""
  {
     DONE;
  })

