;; ----------------------------------------------------------------------
;; ADD INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "add<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(plus:QHSI (match_operand:QHSI 1 "register_operand" "")
		   (match_operand:QHSI 2 "h8300_src_operand" "")))]
  ""
  "")

(define_insn_and_split "*addqi3"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(plus:QI (match_operand:QI 1 "h8300_dst_operand" "%0")
		 (match_operand:QI 2 "h8300_src_operand" "rQi")))]
  "h8300_operands_match_p (operands)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (plus:QI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addqi3_flags<cczn>"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(plus:QI (match_operand:QI 1 "h8300_dst_operand" "%0")
		 (match_operand:QI 2 "h8300_src_operand" "rQi")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && h8300_operands_match_p (operands)"
  "add.b	%X2,%X0"
  [(set_attr "length_table" "add")])

(define_insn_and_split "*addhi"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "L,N,J,n,r")))]
  "!TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (plus:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addhi3_flags<cczn>"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "M,O,J,n,r")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && !TARGET_H8300SX"
  "*
  {
    switch (which_alternative)
      {
      case 0:
	return \"inc %T2,%T0\";
      case 1:
	return \"dec %G2,%T0\";
      case 2:
	return \"add.b	%t2,%t0\";
      case 3:
	{
	  /* If the constant is 4 or -4 and we do not need the
	     flags, then we can use adds/subs which is two bytes
	     shorter.  */
	  rtx x = XVECEXP (PATTERN (insn), 0, 1);
	  bool clobber = GET_CODE (x) == CLOBBER;
	  if (clobber && INTVAL (operands[2]) == 4)
	    return \"adds	%2,%S0\";
	  if (clobber && INTVAL (operands[2]) == -4)
	    return \"subs	%G2,%S0\";
	  return \"add.w	%T2,%T0\";
	}
      case 4:
	return \"add.w	%T2,%T0\";
      default:
	gcc_unreachable ();
      }
  }"
  [(set_attr "length" "2,2,2,4,2")])

(define_insn_and_split "*addhi3_h8sx"
  [(set (match_operand:HI 0 "h8300_dst_operand" "=rU,rU,r,rQ")
	(plus:HI (match_operand:HI 1 "h8300_dst_operand" "%0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "P3>X,P3<X,J,rQi")))]
  "TARGET_H8300SX && h8300_operands_match_p (operands)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (plus:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addhi3_h8sx_flags<cczn>"
  [(set (match_operand:HI 0 "h8300_dst_operand" "=rU,rU,r,rQ")
	(plus:HI (match_operand:HI 1 "h8300_dst_operand" "%0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "P3>X,P3<X,J,rQi")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && TARGET_H8300SX && h8300_operands_match_p (operands)"
  "@
   add.w	%T2:3,%T0
   sub.w	%G2:3,%T0
   add.b	%t2,%t0
   add.w	%T2,%T0"
  [(set_attr "length_table" "short_immediate,short_immediate,*,add")
   (set_attr "length" "*,*,2,*")])

(define_split
  [(set (match_operand:HSI 0 "register_operand" "")
	(plus:HSI (match_dup 0)
		 (match_operand:HSI 1 "two_insn_adds_subs_operand" "")))]
  "!reload_completed"
  [(const_int 0)]
  {
    split_adds_subs (<MODE>mode, operands);
    DONE;
  })


(define_insn_and_split "*addsi"
  [(set (match_operand:SI 0 "h8300_dst_operand" "=rQ,rQ")
	(plus:SI (match_operand:SI 1 "h8300_dst_operand" "%0,0")
		 (match_operand:SI 2 "h8300_src_operand" "i,rQ")))]
  "h8300_operands_match_p (operands)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addsi_flags<cczn>"
  [(set (match_operand:SI 0 "h8300_dst_operand" "=rQ,rQ")
	(plus:SI (match_operand:SI 1 "h8300_dst_operand" "%0,0")
		 (match_operand:SI 2 "h8300_src_operand" "i,rQ")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && h8300_operands_match_p (operands)"
{
  rtx x = XVECEXP (PATTERN (insn), 0, 1);
  return output_plussi (operands, GET_CODE (x) != CLOBBER);
}
  [(set (attr "length")
	(symbol_ref "compute_plussi_length (operands, false)"))])

;; ----------------------------------------------------------------------
;; SUBTRACT INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "sub<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(minus:QHSI (match_operand:QHSI 1 "register_operand" "")
		    (match_operand:QHSI 2 "h8300_src_operand" "")))]
  "")

(define_insn_and_split "*subqi3"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(minus:QI (match_operand:QI 1 "h8300_dst_operand" "0")
		  (match_operand:QI 2 "h8300_dst_operand" "rQ")))]
  "h8300_operands_match_p (operands)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (minus:QI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*subqi3_flags<cczn>"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(minus:QI (match_operand:QI 1 "h8300_dst_operand" "0")
		  (match_operand:QI 2 "h8300_dst_operand" "rQ")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && h8300_operands_match_p (operands)"
  "sub.b	%X2,%X0"
  [(set_attr "length_table" "add")])

(define_insn_and_split "*sub<mode>3"
  [(set (match_operand:HSI 0 "h8300_dst_operand" "=rQ,rQ")
	(minus:HSI (match_operand:HSI 1 "h8300_dst_operand" "0,0")
		   (match_operand:HSI 2 "h8300_src_operand" "rQ,i")))]
  "h8300_operands_match_p (operands)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (minus:HSI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*sub<mode>3_flags<cczn>"
  [(set (match_operand:HSI 0 "h8300_dst_operand" "=rQ,rQ")
	(minus:HSI (match_operand:HSI 1 "h8300_dst_operand" "0,0")
		   (match_operand:HSI 2 "h8300_src_operand" "rQ,i")))
   (clobber (reg:CC CC_REG))]
  "reload_completed && h8300_operands_match_p (operands)"
  { 
    if (<MODE>mode == HImode)
      return "sub.w	%T2,%T0";
    else if (<MODE>mode == SImode)
      return "sub.l	%S2,%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "add")])

;; ----------------------------------------------------------------------
;; NEGATION INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "neg<mode>2"
  [(set (match_operand:QHSIF 0 "register_operand" "")
	(neg:QHSIF (match_operand:QHSIF 1 "register_operand" "")))]
  ""
  "")

(define_insn_and_split "*neg<mode>2"
  [(set (match_operand:QHSI 0 "h8300_dst_operand" "=rQ")
	(neg:QHSI (match_operand:QHSI 1 "h8300_dst_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (neg:QHSI (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*neg<mode>2_flags<cczn>"
  [(set (match_operand:QHSI 0 "h8300_dst_operand" "=rQ")
	(neg:QHSI (match_operand:QHSI 1 "h8300_dst_operand" "0")))
   (clobber (reg:CC CC_REG))]
  "reload_completed"
  {
    if (<MODE>mode == E_QImode)
      return "neg	%X0";
    if (<MODE>mode == E_HImode)
      return "neg.w	%T0";
    if (<MODE>mode == E_SImode)
      return "neg.l	%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "unary")])

(define_insn_and_split "*negsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(neg:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (neg:SF (match_dup 1)))
	      (clobber (reg:CC CC_REG))])])
  
(define_insn "*negsf2_clobber_flags"
  [(set (match_operand:SF 0 "register_operand" "=r")
       (neg:SF (match_operand:SF 1 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  "reload_completed"
  "xor.w\\t#32768,%e0"
  [(set_attr "length" "4")])
