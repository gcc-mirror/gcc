;; ----------------------------------------------------------------------
;; TEST INSTRUCTIONS
;; ----------------------------------------------------------------------

;; (define_insn_and_split "*tst_extzv_1_n"
;;   [(set (cc0)
;; 	(compare (zero_extract:SI (match_operand:QI 0 "general_operand_src" "r,U,mn>")
;; 				  (const_int 1)
;; 				  (match_operand 1 "const_int_operand" "n,n,n"))
;; 		 (const_int 0)))
;;   (clobber (match_scratch:QI 2 "=X,X,&r"))]
;;  "!CONSTANT_P (operands[0])"
;;  "@
;;   btst\\t%Z1,%Y0
;;   btst\\t%Z1,%Y0
;;   #"
;;  "&& reload_completed
;;   && !satisfies_constraint_U (operands[0])"
;;  [(set (match_dup 2)
;;	(match_dup 0))
;;   (parallel [(set (cc0) (compare (zero_extract:SI (match_dup 2)
;;						   (const_int 1)
;;						   (match_dup 1))
;;				  (const_int 0)))
;;	      (clobber (scratch:QI))])]
;;  ""
;;  [(set_attr "length" "2,8,10")])
;;
(define_insn ""
  [(set (reg:CCZ CC_REG)
	(eq (zero_extract:HSI (match_operand:HSI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand 1 "const_int_operand" "n"))
	    (const_int 0)))]
  "INTVAL (operands[1]) < 16"
  "btst	%Z1,%Y0"
  [(set_attr "length" "2")])

(define_insn "*tst<mode>"
  [(set (reg:CCZN CC_REG)
	(compare:CCZN (match_operand:QHSI 0 "register_operand" "r")
		      (const_int 0)))]
  ""
  {
    if (<MODE>mode == QImode)
      return "mov.b	%X0,%X0";
    else if (<MODE>mode == HImode)
      return "mov.w	%T0,%T0";
    else if (<MODE>mode == SImode)
      return "mov.l	%S0,%S0";
    gcc_unreachable ();
  }
  [(set_attr "length" "2")])

(define_insn "*tsthi_upper"
  [(set (reg:CCZN CC_REG)
	(compare (and:HI (match_operand:HI 0 "register_operand" "r")
			 (const_int -256))
		 (const_int 0)))]
  "reload_completed"
  "mov.b	%t0,%t0"
  [(set_attr "length" "2")])

(define_insn "*tstsi_upper"
  [(set (reg:CCZN CC_REG)
	(compare (and:SI (match_operand:SI 0 "register_operand" "r")
			 (const_int -65536))
		 (const_int 0)))]
  "reload_completed"
  "mov.w	%e0,%e0"
  [(set_attr "length" "2")])

(define_insn "*cmp<mode>_c"
  [(set (reg:CCC CC_REG)
	(ltu (match_operand:QHSI 0 "h8300_dst_operand" "rQ")
	     (match_operand:QHSI 1 "h8300_src_operand" "rQi")))]
  "reload_completed"
  {
    if (<MODE>mode == QImode)
      return "cmp.b	%X1,%X0";
    else if (<MODE>mode == HImode)
      return "cmp.w	%T1,%T0";
    else if (<MODE>mode == SImode)
      return "cmp.l	%S1,%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "add")])

(define_insn "*cmpqi"
  [(set (reg:CC CC_REG)
	(compare (match_operand:QI 0 "h8300_dst_operand" "rQ")
		 (match_operand:QI 1 "h8300_src_operand" "rQi")))]
  "reload_completed"
  "cmp.b	%X1,%X0"
  [(set_attr "length_table" "add")])

(define_insn "*cmphi"
  [(set (reg:CC CC_REG)
	(compare (match_operand:HI 0 "h8300_dst_operand" "rU,rQ")
		 (match_operand:HI 1 "h8300_src_operand" "P3>X,rQi")))]
  "reload_completed"
{
  switch (which_alternative)
    {
    case 0:
      if (!TARGET_H8300SX)
	return "cmp.w	%T1,%T0";
      else
	return "cmp.w	%T1:3,%T0";
    case 1:
      return "cmp.w	%T1,%T0";
    default:
      gcc_unreachable ();
      }
}
  [(set_attr "length_table" "short_immediate,add")])

(define_insn "cmpsi"
  [(set (reg:CC CC_REG)
	(compare (match_operand:SI 0 "h8300_dst_operand" "r,rQ")
		 (match_operand:SI 1 "h8300_src_operand" "P3>X,rQi")))]
  "reload_completed"
{
  switch (which_alternative)
    {
    case 0:
      if (!TARGET_H8300SX)
	return "cmp.l	%S1,%S0";
      else
	return "cmp.l	%S1:3,%S0";
    case 1:
      return "cmp.l	%S1,%S0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "length" "2,*")
   (set_attr "length_table" "*,add")])

;; Convert a memory comparison to a move if there is a scratch register.

(define_peephole2
  [(match_scratch:QHSI 1 "r")
   (set (reg:CC CC_REG)
	(compare (match_operand:QHSI 0 "memory_operand" "")
		 (const_int 0)))]
  ""
  [(parallel [(set (match_dup 1) (match_dup 0)) (clobber (reg:CC CC_REG))])
   (set (reg:CC CC_REG) (compare:CC (match_dup 1) (const_int 0)))])

;; The compare-elimination pass does not handle memory reference.  So this
;; little peephole helps fill the gap and avoid code quality regressions.
(define_peephole2
  [(parallel [(set (match_operand:QHSI 0 "register_operand" "")
		   (match_operand:QHSI 1 "simple_memory_operand" ""))
	      (clobber (reg:CC CC_REG))])
   (set (reg:CCZN CC_REG)
	(compare:CCZN (match_dup 0) (const_int 0)))]
  ""
  [(parallel [(set (reg:CCZN CC_REG) (compare:CCZN (match_dup 1) (const_int 0)))
	      (set (match_dup 0) (match_dup 1))])])

;; This exists solely to convince ifcvt to try some store-flag sequences.
;;
;; Essentially we don't want to expose a general store-flag capability.
;; The only generally useful/profitable case is when we want to test the
;; C bit.  In that case we can use addx, subx, bst, or bist to get the bit
;; into a GPR.
;;
;; Others could be handled with stc, shifts and masking, but it likely isn't
;; profitable.
;;
(define_expand "cstore<mode>4"
  [(use (match_operator 1 "eqne_operator"
         [(match_operand:QHSI 2 "h8300_dst_operand" "")
          (match_operand:QHSI 3 "h8300_src_operand" "")]))
   (clobber (match_operand:QHSI 0 "register_operand"))]
  ""
  {
    FAIL;
  })

;; Storing the C bit is pretty simple since there are many ways to
;; introduce it into a GPR.  addx, subx and a variety of bit manipulation
;; instructions
;;
(define_insn "*store_c_<mode>"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(eqne:QHSI (reg:CCC CC_REG) (const_int 0)))]
  "reload_completed"
  {
    if (<CODE> == NE)
      {
	if (<MODE>mode == QImode)
	  return "xor.b\t%X0,%X0\;bst\t#0,%X0";
	else if (<MODE>mode == HImode)
	  return "xor.w\t%T0,%T0\;bst\t#0,%s0";
	else if (<MODE>mode == SImode)
	  return "xor.l\t%S0,%S0\;bst\t#0,%w0";
	gcc_unreachable ();
      }
    else if (<CODE> == EQ)
      {
	if (<MODE>mode == QImode)
	  return "xor.b\t%X0,%X0\;bist\t#0,%X0";
	else if (<MODE>mode == HImode)
	  return "xor.w\t%T0,%T0\;bist\t#0,%s0";
	else if (<MODE>mode == SImode)
	  return "xor.l\t%S0,%S0\;bist\t#0,%w0";
	gcc_unreachable ();
      }
  }
  [(set (attr "length") (symbol_ref "<MODE>mode == SImode ? 6 : 4"))])

;; Similarly, but with a negated result
(define_insn "*store_neg_c_<mode>"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(neg:QHSI (ne:QHSI (reg:CCC CC_REG) (const_int 0))))]
  "reload_completed"
  {
    if (<MODE>mode == QImode)
      return "subx\t%X0,%X0";
    else if (<MODE>mode == HImode)
      return "subx\t%X0,%X0\;exts.w\t%T0";
    else if (<MODE>mode == SImode)
      return "subx\t%X0,%X0\;exts.w\t%T0\;exts.l\t%S0";
    gcc_unreachable ();
  }
  [(set
     (attr "length")
     (symbol_ref "(<MODE>mode == SImode ? 6 : <MODE>mode == HImode ? 4 : 2)"))])

;; Using b[i]st we can store the C bit into any of the low 16 bits of
;; a destination.  We can also rotate it up into the high bit of a 32 bit
;; destination.
(define_insn "*store_shifted_c<mode>"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(ashift:QHSI (eqne:QHSI (reg:CCC CC_REG) (const_int 0))
		     (match_operand 1 "immediate_operand" "n")))]
  "(reload_completed
    && (INTVAL (operands[1]) == 31 || INTVAL (operands[1]) <= 15))"
  {
    if (<CODE> == NE)
      {
	if (<MODE>mode == QImode)
	  return "xor.b\t%X0,%X0\;bst\t%1,%X0";
	else if (<MODE>mode == HImode && INTVAL (operands[1]) < 8)
	  return "xor.w\t%T0,%T0\;bst\t%1,%X0";
	else if (<MODE>mode == HImode)
	  {
	    operands[1] = GEN_INT (INTVAL (operands[1]) - 8);
	    output_asm_insn ("xor.w\t%T0,%T0\;bst\t%1,%t0", operands);
	    return "";
	  }
	else if (<MODE>mode == SImode && INTVAL (operands[1]) == 31)
	  return "xor.l\t%S0,%S0\;rotxr.l\t%S0";
	else if (<MODE>mode == SImode && INTVAL (operands[1]) < 8)
	  return "xor.l\t%S0,%S0\;bst\t%1,%X0";
	else if (<MODE>mode == SImode)
	  {
	    operands[1] = GEN_INT (INTVAL (operands[1]) - 8);
	    output_asm_insn ("xor.l\t%S0,%S0\;bst\t%1,%t0", operands);
	    return "";
	  }
	gcc_unreachable ();
      }
    else if (<CODE> == EQ)
      {
	if (<MODE>mode == QImode)
	  return "xor.b\t%X0,%X0\;bist\t%1,%X0";
	else if (<MODE>mode == HImode && INTVAL (operands[1]) < 8)
	  return "xor.w\t%T0,%T0\;bist\t%1,%X0";
	else if (<MODE>mode == HImode)
	  {
	    operands[1] = GEN_INT (INTVAL (operands[1]) - 8);
	    output_asm_insn ("xor.w\t%T0,%T0\;bist\t%1,%t0", operands);
	    return "";
	  }
	else if (<MODE>mode == SImode && INTVAL (operands[1]) == 31)
	  return "xor.l\t%S0,%S0\;bixor\t#0,%X0\;rotxr.l\t%S0";
	else if (<MODE>mode == SImode && INTVAL (operands[1]) < 8)
	  return "xor.l\t%S0,%S0\;bist\t%1,%X0";
	else if (<MODE>mode == SImode)
	  {
	    operands[1] = GEN_INT (INTVAL (operands[1]) - 8);
	    output_asm_insn ("xor.l\t%S0,%S0\;bist\t%1,%t0", operands);
	    return "";
	  }
	gcc_unreachable ();
      }
    gcc_unreachable ();
  }
  [(set
     (attr "length")
     (symbol_ref "(<MODE>mode == QImode ? 4
		   : <MODE>mode == HImode ? 4
		   : <CODE> == NE ? 6
		   : INTVAL (operands[1]) == 31 ? 8 : 6)"))])

;; Recognize this scc and generate code we can match
(define_insn_and_split "*store_c"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(geultu:QHSI (match_operand:QHSI2 1 "register_operand" "r")
		     (match_operand:QHSI2 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:CCC CC_REG)
	(ltu:CCC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(<geultu_to_c>:QHSI (reg:CCC CC_REG) (const_int 0)))])

;; We can fold in negation of the result and generate better code than
;; what the generic bits would do when testing for C == 1
(define_insn_and_split "*store_neg_c"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(neg:QHSI
	  (ltu:QHSI (match_operand:QHSI2 1 "register_operand" "r")
		    (match_operand:QHSI2 2 "register_operand" "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:CCC CC_REG)
	(ltu:CCC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(neg:QHSI (ne:QHSI (reg:CCC CC_REG) (const_int 0))))])

;; We can use rotates and bst/bist to put the C bit into various places
;; in the destination.
(define_insn_and_split "*store_shifted_c"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
       (ashift:QHSI (geultu:QHSI (match_operand:QHSI2 1 "register_operand" "r")
                                 (match_operand:QHSI2 2 "register_operand" "r"))
		    (match_operand 3 "immediate_operand" "n")))]
  "INTVAL (operands[3]) == 31 || INTVAL (operands[3]) <= 15"
  "#"
  "&& reload_completed"
  [(set (reg:CCC CC_REG) (ltu:CCC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(ashift:QHSI (<geultu_to_c>:QHSI (reg:CCC CC_REG) (const_int 0))
		     (match_dup 3)))])

