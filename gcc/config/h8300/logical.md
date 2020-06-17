;; ----------------------------------------------------------------------
;; AND INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn "bclrqi_msx"
  [(set (match_operand:QI 0 "bit_register_indirect_operand" "=WU")
	(and:QI (match_operand:QI 1 "bit_register_indirect_operand" "%0")
		(match_operand:QI 2 "single_zero_operand" "Y0")))]
  "TARGET_H8300SX && rtx_equal_p (operands[0], operands[1])"
  "bclr\\t%W2,%0"
  [(set_attr "length" "8")])

(define_split
  [(set (match_operand:HI 0 "bit_register_indirect_operand")
	(and:HI (match_operand:HI 1 "bit_register_indirect_operand")
		(match_operand:HI 2 "single_zero_operand")))]
  "TARGET_H8300SX && abs (INTVAL (operands[2])) > 0xff"
  [(set (match_dup 0)
	(and:QI (match_dup 1)
		(match_dup 2)))]
  {
    operands[0] = adjust_address (operands[0], QImode, 0);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[2] = GEN_INT ((INTVAL (operands[2])) >> 8);
  })

(define_insn "*andqi3_2"
  [(set (match_operand:QI 0 "bit_operand" "=U,rQ,r")
	(and:QI (match_operand:QI 1 "bit_operand" "%0,0,WU")
		(match_operand:QI 2 "h8300_src_operand" "Y0,rQi,IP1>X")))]
  "TARGET_H8300SX"
  "@
   bclr\\t %W2,%R0
   and  %X2,%X0
   bfld %2,%1,%R0"
  [(set_attr "length" "8,*,8")
   (set_attr "length_table" "*,logicb,*")
   (set_attr "cc" "none_0hit,set_znv,none_0hit")])

(define_insn "andqi3_1"
  [(set (match_operand:QI 0 "bit_operand" "=U,r")
	(and:QI (match_operand:QI 1 "bit_operand" "%0,0")
		(match_operand:QI 2 "h8300_src_operand" "Y0,rn")))]
  "register_operand (operands[0], QImode)
   || single_zero_operand (operands[2], QImode)"
  "@
   bclr %W2,%R0
   and  %X2,%X0"
  [(set_attr "length" "2,8")
   (set_attr "cc" "none_0hit,set_znv")])

(define_expand "and<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(and:QHSI (match_operand:QHSI 1 "register_operand" "")
		  (match_operand:QHSI 2 "h8300_src_operand" "")))]
  ""
  "")

(define_insn "*andor<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(ior:QHSI (and:QHSI (match_operand:QHSI 2 "register_operand" "r")
			    (match_operand:QHSI 3 "single_one_operand" "n"))
		  (match_operand:QHSI 1 "register_operand" "0")))]
  "(<MODE>mode == QImode
    || <MODE>mode == HImode
    || (<MODE>mode == SImode
	&& (INTVAL (operands[3]) & 0xffff) != 0))"
  {
    if (<MODE>mode == QImode)
      return "bld\\t%V3,%X2\;bor\\t%V3,%X0\;bst\\t%V3,%X0";

    if (<MODE>mode == HImode)
      {
	operands[3] = GEN_INT (INTVAL (operands[3]) & 0xffff);
	if (INTVAL (operands[3]) > 128)
	  {
	    operands[3] = GEN_INT (INTVAL (operands[3]) >> 8);
	    return "bld\\t%V3,%t2\;bor\\t%V3,%t0\;bst\\t%V3,%t0";
	  }
	return "bld\\t%V3,%s2\;bor\\t%V3,%s0\;bst\\t%V3,%s0";
      }

    if (<MODE>mode == SImode)
      {
	operands[3] = GEN_INT (INTVAL (operands[3]) & 0xffff);
	if (INTVAL (operands[3]) > 128)
	  {
	    operands[3] = GEN_INT (INTVAL (operands[3]) >> 8);
	    return "bld\\t%V3,%x2\;bor\\t%V3,%x0\;bst\\t%V3,%x0";
	  }
	return "bld\\t%V3,%w2\;bor\\t%V3,%w0\;bst\\t%V3,%w0";
      }

    gcc_unreachable ();
	
  }
  [(set_attr "length" "6")])

(define_insn "*andorsi3_shift_8"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (ashift:SI (match_operand:SI 2 "register_operand" "r")
				   (const_int 8))
			(const_int 65280))
		(match_operand:SI 1 "register_operand" "0")))]
  ""
  "or.b\\t%w2,%x0"
  [(set_attr "length" "2")])

;; ----------------------------------------------------------------------
;; OR/XOR INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn "b<code>qi_msx"
  [(set (match_operand:QI 0 "bit_register_indirect_operand" "=WU")
	(ors:QI (match_operand:QI 1 "bit_register_indirect_operand" "%0")
		(match_operand:QI 2 "single_one_operand" "Y2")))]
  "TARGET_H8300SX && rtx_equal_p (operands[0], operands[1])"
  { return <CODE> == IOR ? "bset\\t%V2,%0" : "bnot\\t%V2,%0"; }
  [(set_attr "length" "8")])

(define_split
  [(set (match_operand:HI 0 "bit_register_indirect_operand")
	(ors:HI (match_operand:HI 1 "bit_register_indirect_operand")
		(match_operand:HI 2 "single_one_operand")))]
  "TARGET_H8300SX && abs (INTVAL (operands[2])) > 0xff"
  [(set (match_dup 0)
	(and:QI (match_dup 1)
		(match_dup 2)))]
  {
    operands[0] = adjust_address (operands[0], QImode, 0);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[2] = GEN_INT ((INTVAL (operands[2])) >> 8);
  })

(define_insn "<code>qi3_1"
  [(set (match_operand:QI 0 "bit_operand" "=U,rQ")
	(ors:QI (match_operand:QI 1 "bit_operand" "%0,0")
		(match_operand:QI 2 "h8300_src_operand" "Y2,rQi")))]
  "TARGET_H8300SX || register_operand (operands[0], QImode)
   || single_one_operand (operands[2], QImode)"
  {
    if (which_alternative == 0)
      return <CODE> == IOR ? "bset\\t%V2,%R0" : "bnot\\t%V2,%R0"; 
    else if (which_alternative == 1)
      return <CODE> == IOR ? "or\\t%X2,%X0" : "xor\\t%X2,%X0";
    gcc_unreachable ();
  }
  [(set_attr "length" "8,*")
   (set_attr "length_table" "*,logicb")
   (set_attr "cc" "none_0hit,set_znv")])

(define_expand "<code><mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(ors:QHSI (match_operand:QHSI 1 "register_operand" "")
		  (match_operand:QHSI 2 "h8300_src_operand" "")))]
  ""
  "")

;; ----------------------------------------------------------------------
;; {AND,IOR,XOR}{HI3,SI3} PATTERNS
;; ----------------------------------------------------------------------

(define_insn "*logical<mode>3"
  [(set (match_operand:HSI 0 "h8300_dst_operand" "=rQ")
	(match_operator:HSI 3 "bit_operator"
	  [(match_operand:HSI 1 "h8300_dst_operand" "%0")
	   (match_operand:HSI 2 "h8300_src_operand" "rQi")]))]
  "h8300_operands_match_p (operands)"
  { return output_logical_op (<MODE>mode, operands); }
  [(set (attr "length")
	(symbol_ref "compute_logical_op_length (<MODE>mode, operands)"))
   (set (attr "cc")
	(symbol_ref "compute_logical_op_cc (<MODE>mode, operands)"))])


;; ----------------------------------------------------------------------
;; NOT INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:QHSI 0 "h8300_dst_operand" "=rQ")
	(not:QHSI (match_operand:QHSI 1 "h8300_dst_operand" "0")))]
  ""
  {
    if (<MODE>mode == E_QImode)
      return "not	%X0";
    if (<MODE>mode == E_HImode)
      return "not.w	%T0";
    if (<MODE>mode == E_SImode)
      return "not.l	%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "unary")
   (set_attr "cc" "set_znv")])
