;; ----------------------------------------------------------------------
;; MULTIPLY INSTRUCTIONS
;; ----------------------------------------------------------------------

;; Note that the H8/300 can only handle umulqihi3.

(define_expand "mulqihi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" ""))
		 ;; intentionally-mismatched modes
		 (match_operand:QI 2 "reg_or_nibble_operand" "")))]
  ""
  {
    if (GET_MODE (operands[2]) != VOIDmode)
      operands[2] = gen_rtx_SIGN_EXTEND (HImode, operands[2]);
  })

(define_insn_and_split "*mulqihi3_const"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (match_operand:QI 2 "nibble_operand" "IP4>X")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (mult:HI (sign_extend:HI (match_dup 1)) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*mulqihi3_const<cczn>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (match_operand:QI 2 "nibble_operand" "IP4>X")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "mulxs.b	%X2,%T0"
  [(set_attr "length" "4")])

(define_insn_and_split "*mulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (sign_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (mult:HI (sign_extend:HI (match_dup 1))
			    (sign_extend:HI (match_dup 2))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*mulqihi3<cczn>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (sign_extend:HI (match_operand:QI 2 "register_operand" "r"))))
   (clobber (reg:CC CC_REG))]
  ""
  "mulxs.b	%X2,%T0"
  [(set_attr "length" "4")])

(define_expand "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" ""))
		 ;; intentionally-mismatched modes
		 (match_operand:HI 2 "reg_or_nibble_operand" "")))]
  ""
  {
    if (GET_MODE (operands[2]) != VOIDmode)
      operands[2] = gen_rtx_SIGN_EXTEND (SImode, operands[2]);
  })

(define_insn_and_split "*mulhisi3_const"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (match_operand:SI 2 "nibble_operand" "IP4>X")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (mult:SI (sign_extend:SI (match_dup 1)) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*mulhisi3_const<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (match_operand:SI 2 "nibble_operand" "IP4>X")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "mulxs.w	%T2,%S0"
  [(set_attr "length" "4")])

(define_insn_and_split "*mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (mult:SI (sign_extend:SI (match_dup 1))
			    (sign_extend:SI (match_dup 2))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*mulhisi3<cczn>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))
   (clobber (reg:CC CC_REG))]
  ""
  "mulxs.w	%T2,%S0"
  [(set_attr "length" "4")])

(define_expand "umulqihi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" ""))
		 ;; intentionally-mismatched modes
		 (match_operand:QI 2 "reg_or_nibble_operand" "")))]
  ""
  {
    if (GET_MODE (operands[2]) != VOIDmode)
      operands[2] = gen_rtx_ZERO_EXTEND (HImode, operands[2]);
  })

(define_insn "*umulqihi3_const"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (match_operand:QI 2 "nibble_operand" "IP4>X")))]
  "TARGET_H8300SX"
  "mulxu.b	%X2,%T0"
  [(set_attr "length" "4")])

(define_insn "*umulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
		 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "mulxu.b	%X2,%T0"
  [(set_attr "length" "2")])

(define_expand "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" ""))
		 ;; intentionally-mismatched modes
		 (match_operand:HI 2 "reg_or_nibble_operand" "")))]
  ""
  {
    if (GET_MODE (operands[2]) != VOIDmode)
      operands[2] = gen_rtx_ZERO_EXTEND (SImode, operands[2]);
  })

(define_insn "*umulhisi3_const"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (match_operand:SI 2 "nibble_operand" "IP4>X")))]
  "TARGET_H8300SX"
  "mulxu.w	%T2,%S0"
  [(set_attr "length" "4")])

(define_insn "*umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "%0"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  ""
  "mulxu.w	%T2,%S0"
  [(set_attr "length" "2")])

;; We could have used mulu.[wl] here, but mulu.[lw] is only available
;; on a H8SX with a multiplier, whereas muls.w seems to be available
;; on all H8SX variants.

(define_insn_and_split "mul<mode>3"
  [(set (match_operand:HSI 0 "register_operand" "=r")
        (mult:HSI (match_operand:HSI 1 "register_operand" "%0")
		  (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (mult:HSI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "mul<mode>3_clobber_flags"
  [(set (match_operand:HSI 0 "register_operand" "=r")
        (mult:HSI (match_operand:HSI 1 "register_operand" "%0")
		  (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  { return <MODE>mode == HImode ? "muls.w\\t%T2,%T0" : "muls.l\\t%S2,%S0"; }
  [(set_attr "length" "4")])

(define_insn_and_split "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (sign_extend:DI (match_operand:SI 1 "register_operand" "%0"))
	   (sign_extend:DI (match_operand:SI 2 "reg_or_nibble_operand" "r IP4>X")))
	  (const_int 32))))]
  "TARGET_H8300SXMUL"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (truncate:SI (lshiftrt:DI (mult:DI
					       (sign_extend:DI (match_dup 1))
					       (sign_extend:DI (match_dup 2)))
					     (const_int 32))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "smulsi3_highpart_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (sign_extend:DI (match_operand:SI 1 "register_operand" "%0"))
	   (sign_extend:DI (match_operand:SI 2 "reg_or_nibble_operand" "r IP4>X")))
	  (const_int 32))))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SXMUL"
  "muls/u.l\\t%S2,%S0"
  [(set_attr "length" "4")])

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (ashiftrt:DI
	    (mult:DI
	      (zero_extend:DI (match_operand:SI 1 "register_operand" "%0"))
	      (zero_extend:DI (match_operand:SI 2 "reg_or_nibble_operand" "r IP4>X")))
	    (const_int 32))))]
  "TARGET_H8300SX"
  "mulu/u.l\\t%S2,%S0"
  [(set_attr "length" "4")])

;; This is a "bridge" instruction.  Combine can't cram enough insns
;; together to crate a MAC instruction directly, but it can create
;; this instruction, which then allows combine to create the real
;; MAC insn.
;;
;; Unfortunately, if combine doesn't create a MAC instruction, this
;; insn must generate reasonably correct code.  Egad.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(mult:SI
	  (sign_extend:SI
	    (mem:HI (post_inc:SI (match_operand:SI 1 "register_operand" "r"))))
	  (sign_extend:SI
	    (mem:HI (post_inc:SI (match_operand:SI 2 "register_operand" "r"))))))]
  "TARGET_MAC"
  "clrmac\;mac	@%2+,@%1+"
  [(set_attr "length" "6")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (mult:SI
	  (sign_extend:SI (mem:HI
	    (post_inc:SI (match_operand:SI 1 "register_operand" "r"))))
	  (sign_extend:SI (mem:HI
	    (post_inc:SI (match_operand:SI 2 "register_operand" "r")))))
	      (match_operand:SI 3 "register_operand" "0")))]
  "TARGET_MAC"
  "mac	@%2+,@%1+"
  [(set_attr "length" "4")])

