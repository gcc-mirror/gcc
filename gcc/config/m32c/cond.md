;; Machine Descriptions for R8C/M16C/M32C
;; Copyright (C) 2005-2021 Free Software Foundation, Inc.
;; Contributed by Red Hat.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; conditionals - cmp, jcc, setcc, etc.

; Special note about conditional instructions: GCC always emits the
; compare right before the insn, which is good, because m32c's mov
; insns modify the flags.  However, this means that any conditional
; insn that may require reloading must be kept with its compare until
; after reload finishes, else the reload insns might clobber the
; flags.  Thus, these rules:
;
; * the cmp* expanders just save the operands in compare_op0 and
;   compare_op1 via m32c_pend_compare.
; * conditional insns that won't need reload can call
;   m32c_unpend_compare before their expansion.
; * other insns must expand to include the compare operands within,
;   then split after reload to a separate compare and conditional.

; Until support for relaxing is supported in gas, we must assume that
; short labels won't reach, so we must use long labels.
; Unfortunately, there aren't any conditional jumps with long labels,
; so instead we invert the conditional and jump around a regular jump.

; Note that we can, at some point in the future, add code to omit the
; "cmp" portion of the insn if the preceding insn happened to set the
; right flags already.  For example, a mov followed by a "cmp *,0" is
; redundant; the move already set the Z flag.

(define_insn_and_split "cbranch<mode>4"
  [(set (pc) (if_then_else
	      (match_operator 0 "m32c_cmp_operator"
			      [(match_operand:QHPSI 1 "mra_operand" "RraSd")
			       (match_operand:QHPSI 2 "mrai_operand" "iRraSd")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "#"
  "reload_completed"
  [(set (reg:CC FLG_REGNO)
	(compare (match_dup 1)
		 (match_dup 2)))
   (set (pc) (if_then_else (match_op_dup 0 [(reg:CC FLG_REGNO) (const_int 0)])
			   (label_ref (match_dup 3))
			   (pc)))]
  ""
  )

(define_insn "bcc_op"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(reg:CC FLG_REGNO) (const_int 0)])
                      (label_ref (match_operand 1 ""))
                      (pc)))]
  ""
  "j%c0\t%l1"
  [(set_attr "flags" "n")]
)

(define_insn "stzx_16"
  [(set (match_operand:QI 0 "register_operand" "=R0w,R0w,R0w")
	(if_then_else:QI (eq (reg:CC FLG_REGNO) (const_int 0))
			 (match_operand:QI 1 "const_int_operand" "i,i,0")
			 (match_operand:QI 2 "const_int_operand" "i,0,i")))]
  "TARGET_A16 && reload_completed"
  "@
   stzx\t%1,%2,%0
   stz\t%1,%0
   stnz\t%2,%0"
  [(set_attr "flags" "n,n,n")]
)

(define_insn "stzx_24_<mode>"
  [(set (match_operand:QHI 0 "mra_operand" "=RraSd,RraSd,RraSd")
	(if_then_else:QHI (eq (reg:CC FLG_REGNO) (const_int 0))
			 (match_operand:QHI 1 "const_int_operand" "i,i,0")
			 (match_operand:QHI 2 "const_int_operand" "i,0,i")))]
  "TARGET_A24 && reload_completed"
  "@
   stzx.<bwl>\t%1,%2,%0
   stz.<bwl>\t%1,%0
   stnz.<bwl>\t%2,%0"
  [(set_attr "flags" "n,n,n")])

(define_insn_and_split "stzx_reversed_<mode>"
  [(set (match_operand:QHI 0 "m32c_r0_operand" "=R0w")
	(if_then_else:QHI (ne (reg:CC FLG_REGNO) (const_int 0))
			 (match_operand:QHI 1 "const_int_operand" "")
			 (match_operand:QHI 2 "const_int_operand" "")))]
  "(TARGET_A24 || GET_MODE (operands[0]) == QImode) && reload_completed"
  "#"
  ""
  [(set (match_dup 0)
	(if_then_else:QHI (eq (reg:CC FLG_REGNO) (const_int 0))
		      (match_dup 2)
		      (match_dup 1)))]
  ""
  )


(define_insn "cmp<mode>_op"
  [(set (reg:CC FLG_REGNO)
	(compare (match_operand:QHPSI 0 "mra_operand" "RraSd")
		 (match_operand:QHPSI 1 "mrai_operand" "RraSdi")))]
  ""
  "* return m32c_output_compare(insn, operands); "
  [(set_attr "flags" "oszc")])

;; m32c_conditional_register_usage changes the setcc_gen_code array to
;; point to the _24 variants if needed.

;; We need to keep the compare and conditional sets together through
;; reload, because reload might need to add address reloads to the
;; set, which would clobber the flags.  By keeping them together, the
;; reloads get put before the compare, thus preserving the flags.

;; These are the post-split patterns for the conditional sets.

(define_insn "scc_op"
  [(set (match_operand:QI 0 "register_operand" "=Rqi")
	(match_operator:QI 1 "ordered_comparison_operator"
	 [(reg:CC FLG_REGNO) (const_int 0)]))]
  "TARGET_A16 && reload_completed"
  "* return m32c_scc_pattern(operands, GET_CODE (operands[1]));")

(define_insn "scc_24_op"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd")
	(match_operator:HI 1 "ordered_comparison_operator"
	 [(reg:CC FLG_REGNO) (const_int 0)]))]
  "TARGET_A24 && reload_completed"
  "sc%c1\t%0"
  [(set_attr "flags" "n")]
)

;; These are the pre-split patterns for the conditional sets.

(define_expand "cstore<mode>4"
  [(set (match_operand:QI 0 "register_operand")
	(match_operator:QI 1 "ordered_comparison_operator"
	 [(match_operand:QHPSI 2 "mra_operand")
	  (match_operand:QHPSI 3 "mrai_operand")]))]
  ""
{
  if (TARGET_A24)
    {
      rtx o = gen_reg_rtx (HImode);
      emit_insn (gen_cstore<mode>4_24 (o, operands[1],
				       operands[2], operands[3]));
      emit_move_insn (operands[0], gen_lowpart (QImode, o));
      DONE;
    }
})

(define_insn_and_split "*cstore<mode>4_16"
  [(set (match_operand:QI 0 "register_operand" "=Rqi")
	(match_operator:QI 1 "ordered_comparison_operator"
	 [(match_operand:QHPSI 2 "mra_operand" "RraSd")
	  (match_operand:QHPSI 3 "mrai_operand" "RraSdi")]))]
  "TARGET_A16"
  "#"
  "&& reload_completed"
  [(set (reg:CC FLG_REGNO)
	(compare (match_dup 2)
		 (match_dup 3)))
   (set (match_dup 0)
	(match_op_dup 1 [(reg:CC FLG_REGNO) (const_int 0)]))]
  ""
  [(set_attr "flags" "x")]
)

(define_insn_and_split "cstore<mode>4_24"
  [(set (match_operand:HI 0 "mra_nopp_operand" "=RhiSd")
	(match_operator:HI 1 "ordered_comparison_operator"
	 [(match_operand:QHPSI 2 "mra_operand" "RraSd")
	  (match_operand:QHPSI 3 "mrai_operand" "RraSdi")]))]
  "TARGET_A24"
  "#"
  "&& reload_completed"
  [(set (reg:CC FLG_REGNO)
	(compare (match_dup 2)
		 (match_dup 3)))
   (set (match_dup 0)
	(match_op_dup 1 [(reg:CC FLG_REGNO) (const_int 0)]))]
  ""
  [(set_attr "flags" "x")]
)

(define_insn_and_split "movqicc_<code>_<mode>"
  [(set (match_operand:QI 0 "register_operand" "=R0w")
        (if_then_else:QI (eqne_cond (match_operand:QHPSI 1 "mra_operand" "RraSd")
				    (match_operand:QHPSI 2 "mrai_operand" "RraSdi"))
			  (match_operand:QI 3 "const_int_operand" "")
			  (match_operand:QI 4 "const_int_operand" "")))]
  ""
  "#"
  "reload_completed"
  [(set (reg:CC FLG_REGNO)
	(compare (match_dup 1)
		 (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:QI (eqne_cond (reg:CC FLG_REGNO) (const_int 0))
			 (match_dup 3)
			 (match_dup 4)))]
  ""
  [(set_attr "flags" "x")]
  )

(define_insn_and_split "movhicc_<code>_<mode>"
  [(set (match_operand:HI 0 "register_operand" "=R0w")
        (if_then_else:HI (eqne_cond (match_operand:QHPSI 1 "mra_operand" "RraSd")
				    (match_operand:QHPSI 2 "mrai_operand" "RraSdi"))
			  (match_operand:HI 3 "const_int_operand" "")
			  (match_operand:HI 4 "const_int_operand" "")))]
  "TARGET_A24"
  "#"
  "reload_completed"
  [(set (reg:CC FLG_REGNO)
	(compare (match_dup 1)
		 (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:HI (eqne_cond (reg:CC FLG_REGNO) (const_int 0))
			 (match_dup 3)
			 (match_dup 4)))]
  ""
  [(set_attr "flags" "x")]
  )

;; And these are the expanders.

(define_expand "movqicc"
  [(set (match_operand:QI 0 "register_operand" "")
        (if_then_else:QI (match_operand 1 "m32c_eqne_operator" "")
                         (match_operand:QI 2 "const_int_operand" "")
                         (match_operand:QI 3 "const_int_operand" "")))]
  ""
  "if (m32c_expand_movcc(operands))
     FAIL;
   DONE;"
)

(define_expand "movhicc"
  [(set (match_operand:HI 0 "mra_operand" "")
        (if_then_else:HI (match_operand 1 "m32c_eqne_operator" "")
                         (match_operand:HI 2 "const_int_operand" "")
                         (match_operand:HI 3 "const_int_operand" "")))]
  "TARGET_A24"
  "if (m32c_expand_movcc(operands))
     FAIL;
   DONE;"
)


;; CMP opcodes subtract two values, set the flags, and discard the
;; value.  This pattern recovers the sign of the discarded value based
;; on the flags.  Operand 0 is set to -1, 0, or 1.  This is used for
;; the cmpstr pattern.  For optimal code, this should be removed if
;; followed by a suitable CMP insn (see the peephole following).  This
;; pattern is 7 bytes and 5 cycles.  If you don't need specific
;; values, a 5/4 pattern can be made with SCGT and BMLT to set the
;; appropriate bits.

(define_insn "cond_to_int"
  [(set (match_operand:HI 0 "mra_qi_operand" "=Rqi")
	(if_then_else:HI (lt (reg:CC FLG_REGNO) (const_int 0))
			 (const_int -1)
			 (if_then_else:HI (eq (reg:CC FLG_REGNO) (const_int 0))
					  (const_int 0)
					  (const_int -1))))]
  "TARGET_A24"
  "sceq\t%0\n\tbmgt\t1,%h0\n\tdec.w\t%0"
  [(set_attr "flags" "x")]
  )  

;; A cond_to_int followed by a compare against zero is essentially a
;; no-op.  However, the result of the cond_to_int may be used by later
;; insns, so make sure it's dead before deleting its set.

(define_peephole2
  [(set (match_operand:HI 0 "mra_qi_operand" "")
	(if_then_else:HI (lt (reg:CC FLG_REGNO) (const_int 0))
			 (const_int -1)
			 (if_then_else:HI (eq (reg:CC FLG_REGNO) (const_int 0))
					  (const_int 0)
					  (const_int -1))))
   (set (reg:CC FLG_REGNO)
	(compare (match_operand:HI 1 "mra_qi_operand" "")
		 (const_int 0)))
   ]
  "rtx_equal_p (operands[0], operands[1])
     && dead_or_set_p (peep2_next_insn (1), operands[0])"
  [(const_int 1)]
  "")
