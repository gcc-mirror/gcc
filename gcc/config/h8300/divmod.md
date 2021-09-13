;; ----------------------------------------------------------------------
;; DIVIDE/MOD INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn_and_split "udiv<mode>3"
  [(set (match_operand:HSI 0 "register_operand" "=r")
	(udiv:HSI (match_operand:HSI 1 "register_operand" "0")
		  (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (udiv:HSI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "udiv<mode>3_clobber_flags"
  [(set (match_operand:HSI 0 "register_operand" "=r")
	(udiv:HSI (match_operand:HSI 1 "register_operand" "0")
		  (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  { return <MODE>mode == HImode ? "divu.w\\t%T2,%T0" : "divu.l\\t%S2,%S0"; }
  [(set_attr "length" "4")])

(define_insn_and_split "div<mode>3"
  [(set (match_operand:HSI 0 "register_operand" "=r")
	(div:HSI (match_operand:HSI 1 "register_operand" "0")
		 (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (div:HSI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "div<mode>3_clobber_flags"
  [(set (match_operand:HSI 0 "register_operand" "=r")
	(div:HSI (match_operand:HSI 1 "register_operand" "0")
		 (match_operand:HSI 2 "reg_or_nibble_operand" "r IP4>X")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  { return <MODE>mode == HImode ? "divs.w\\t%T2,%T0" : "divs.l\\t%S2,%S0"; }
  [(set_attr "length" "4")])

(define_insn_and_split "udivmodqi4"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI
	  (udiv:HI
	    (match_operand:HI 1 "register_operand" "0")
	    (zero_extend:HI (match_operand:QI 2 "register_operand" "r")))))
   (set (match_operand:QI 3 "register_operand" "=r")
	(truncate:QI
	  (umod:HI
	    (match_dup 1)
	    (zero_extend:HI (match_dup 2)))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (truncate:QI
				   (udiv:HI (match_dup 1)
					    (zero_extend:HI (match_dup 2)))))
	      (set (match_dup 3) (truncate:QI
				   (umod:HI (match_dup 1)
					    (zero_extend:HI (match_dup 2)))))
	      (clobber (reg:CC CC_REG))])])
							

(define_insn "udivmodqi4_clobber_flags"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI
	  (udiv:HI
	    (match_operand:HI 1 "register_operand" "0")
	    (zero_extend:HI (match_operand:QI 2 "register_operand" "r")))))
   (set (match_operand:QI 3 "register_operand" "=r")
	(truncate:QI
	  (umod:HI
	    (match_dup 1)
	    (zero_extend:HI (match_dup 2)))))
   (clobber (reg:CC CC_REG))]
  ""
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return "divxu.b\\t%X2,%T0";
  else
    return "divxu.b\\t%X2,%T0\;mov.b\\t%t0,%s3";
}
  [(set_attr "length" "4")])

(define_insn_and_split "divmodqi4"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI
	  (div:HI
	    (match_operand:HI 1 "register_operand" "0")
	    (sign_extend:HI (match_operand:QI 2 "register_operand" "r")))))
   (set (match_operand:QI 3 "register_operand" "=r")
	(truncate:QI
	  (mod:HI
	    (match_dup 1)
	    (sign_extend:HI (match_dup 2)))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (truncate:QI
				   (div:HI (match_dup 1)
					   (sign_extend:HI (match_dup 2)))))
	      (set (match_dup 3) (truncate:QI
				   (mod:HI (match_dup 1)
					   (sign_extend:HI (match_dup 2)))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "divmodqi4_clobber_flags"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI
	  (div:HI
	    (match_operand:HI 1 "register_operand" "0")
	    (sign_extend:HI (match_operand:QI 2 "register_operand" "r")))))
   (set (match_operand:QI 3 "register_operand" "=r")
	(truncate:QI
	  (mod:HI
	    (match_dup 1)
	    (sign_extend:HI (match_dup 2)))))
   (clobber (reg:CC CC_REG))]
  ""
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return "divxs.b\\t%X2,%T0";
  else
    return "divxs.b\\t%X2,%T0\;mov.b\\t%t0,%s3";
}
  [(set_attr "length" "6")])

(define_insn_and_split "udivmodhi4"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI
	  (udiv:SI
	    (match_operand:SI 1 "register_operand" "0")
	    (zero_extend:SI (match_operand:HI 2 "register_operand" "r")))))
   (set (match_operand:HI 3 "register_operand" "=r")
	(truncate:HI
	  (umod:SI
	    (match_dup 1)
	    (zero_extend:SI (match_dup 2)))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (truncate:HI
				   (udiv:SI (match_dup 1)
					    (zero_extend:SI (match_dup 2)))))
	      (set (match_dup 3) (truncate:HI
				   (umod:SI (match_dup 1)
					    (zero_extend:SI (match_dup 2)))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "udivmodhi4_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI
	  (udiv:SI
	    (match_operand:SI 1 "register_operand" "0")
	    (zero_extend:SI (match_operand:HI 2 "register_operand" "r")))))
   (set (match_operand:HI 3 "register_operand" "=r")
	(truncate:HI
	  (umod:SI
	    (match_dup 1)
	    (zero_extend:SI (match_dup 2)))))
   (clobber (reg:CC CC_REG))]
  ""
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return "divxu.w\\t%T2,%S0";
  else
    return "divxu.w\\t%T2,%S0\;mov.w\\t%e0,%f3";
}
  [(set_attr "length" "4")])

(define_insn_and_split "divmodhi4"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI
	  (div:SI
	    (match_operand:SI 1 "register_operand" "0")
	    (sign_extend:SI (match_operand:HI 2 "register_operand" "r")))))
   (set (match_operand:HI 3 "register_operand" "=r")
	(truncate:HI
	  (mod:SI
	    (match_dup 1)
	    (sign_extend:SI (match_dup 2)))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (truncate:HI
				   (div:SI (match_dup 1)
					   (sign_extend:SI (match_dup 2)))))
	      (set (match_dup 3) (truncate:HI
				   (mod:SI (match_dup 1)
					   (sign_extend:SI (match_dup 2)))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "divmodhi4_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI
	  (div:SI
	    (match_operand:SI 1 "register_operand" "0")
	    (sign_extend:SI (match_operand:HI 2 "register_operand" "r")))))
   (set (match_operand:HI 3 "register_operand" "=r")
	(truncate:HI
	  (mod:SI
	    (match_dup 1)
	    (sign_extend:SI (match_dup 2)))))
   (clobber (reg:CC CC_REG))]
  ""
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return "divxs.w\\t%T2,%S0";
  else
    return "divxs.w\\t%T2,%S0\;mov.w\\t%e0,%f3";
}
  [(set_attr "length" "6")])
