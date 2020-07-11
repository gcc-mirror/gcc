;; ----------------------------------------------------------------------
;; ADD INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "add<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(plus:QHSI (match_operand:QHSI 1 "register_operand" "")
		   (match_operand:QHSI 2 "h8300_src_operand" "")))]
  ""
  "")

(define_insn "*addqi3"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(plus:QI (match_operand:QI 1 "h8300_dst_operand" "%0")
		 (match_operand:QI 2 "h8300_src_operand" "rQi")))]
  "h8300_operands_match_p (operands)"
  "add.b	%X2,%X0"
  [(set_attr "length_table" "add")
   (set_attr "cc" "set_zn")])

(define_insn "*addhi3_h8300hs"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "L,N,J,n,r")))]
  "!TARGET_H8300SX"
  "@
   adds	%2,%S0
   subs	%G2,%S0
   add.b	%t2,%t0
   add.w	%T2,%T0
   add.w	%T2,%T0"
  [(set_attr "length" "2,2,2,4,2")
   (set_attr "cc" "none_0hit,none_0hit,clobber,set_zn,set_zn")])

(define_insn "*add<mode>3_incdec"
  [(set (match_operand:HSI 0 "register_operand" "=r,r")
	(unspec:HSI [(match_operand:HSI 1 "register_operand" "0,0")
		     (match_operand:HSI 2 "incdec_operand" "M,O")]
		    UNSPEC_INCDEC))]
  ""
  {
    if (which_alternative == 0)
      return <MODE>mode == HImode ? "inc.w\t%2,%T0" : "inc.l\t%2,%S0";
    else if (which_alternative == 1)
      return <MODE>mode == HImode ? "dec.w\t%G2,%T0" : "dec.l\t%G2,%S0";
    gcc_unreachable ();
   }
  [(set_attr "length" "2,2")
   (set_attr "cc" "set_zn,set_zn")])

(define_insn "*addhi3_h8sx"
  [(set (match_operand:HI 0 "h8300_dst_operand" "=rU,rU,r,rQ")
	(plus:HI (match_operand:HI 1 "h8300_dst_operand" "%0,0,0,0")
		 (match_operand:HI 2 "h8300_src_operand" "P3>X,P3<X,J,rQi")))]
  "TARGET_H8300SX && h8300_operands_match_p (operands)"
  "@
   add.w	%T2:3,%T0
   sub.w	%G2:3,%T0
   add.b	%t2,%t0
   add.w	%T2,%T0"
  [(set_attr "length_table" "short_immediate,short_immediate,*,add")
   (set_attr "length" "*,*,2,*")
   (set_attr "cc" "set_zn")])

(define_split
  [(set (match_operand:HSI 0 "register_operand" "")
	(plus:HSI (match_dup 0)
		 (match_operand:HSI 1 "two_insn_adds_subs_operand" "")))]
  ""
  [(const_int 0)]
  {
    split_adds_subs (<MODE>mode, operands);
    DONE;
  })


(define_insn "*addsi_h8300hs"
  [(set (match_operand:SI 0 "h8300_dst_operand" "=rQ,rQ")
	(plus:SI (match_operand:SI 1 "h8300_dst_operand" "%0,0")
		 (match_operand:SI 2 "h8300_src_operand" "i,rQ")))]
  "h8300_operands_match_p (operands)"
{
  return output_plussi (operands);
}
  [(set (attr "length")
	(symbol_ref "compute_plussi_length (operands)"))
   (set (attr "cc")
	(symbol_ref "compute_plussi_cc (operands)"))])

;; ----------------------------------------------------------------------
;; SUBTRACT INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "sub<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(minus:QHSI (match_operand:QHSI 1 "register_operand" "")
		    (match_operand:QHSI 2 "h8300_src_operand" "")))]
  ""
  {
  })

(define_insn "*subqi3"
  [(set (match_operand:QI 0 "h8300_dst_operand" "=rQ")
	(minus:QI (match_operand:QI 1 "h8300_dst_operand" "0")
		  (match_operand:QI 2 "h8300_dst_operand" "rQ")))]
  "h8300_operands_match_p (operands)"
  "sub.b	%X2,%X0"
  [(set_attr "length_table" "add")
   (set_attr "cc" "set_zn")])

(define_insn "*sub<mode>3_h8300hs"
  [(set (match_operand:HSI 0 "h8300_dst_operand" "=rQ,rQ")
	(minus:HSI (match_operand:HSI 1 "h8300_dst_operand" "0,0")
		   (match_operand:HSI 2 "h8300_src_operand" "rQ,i")))]
  "h8300_operands_match_p (operands)"
  { 
    if (<MODE>mode == HImode)
      return "sub.w	%T2,%T0";
    else if (<MODE>mode == SImode)
      return "sub.l	%S2,%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "add")
   (set_attr "cc" "set_zn")])

;; ----------------------------------------------------------------------
;; NEGATION INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_expand "neg<mode>2"
  [(set (match_operand:QHSIF 0 "register_operand" "")
	(neg:QHSIF (match_operand:QHSIF 1 "register_operand" "")))]
  ""
  "")

(define_insn "*neg<mode>2"
  [(set (match_operand:QHSI 0 "h8300_dst_operand" "=rQ")
	(neg:QHSI (match_operand:QHSI 1 "h8300_dst_operand" "0")))]
  ""
  {
    if (<MODE>mode == E_QImode)
      return "neg	%X0";
    if (<MODE>mode == E_HImode)
      return "neg.w	%T0";
    if (<MODE>mode == E_SImode)
      return "neg.l	%S0";
    gcc_unreachable ();
  }
  [(set_attr "length_table" "unary")
   (set_attr "cc" "set_zn")])


(define_insn "*negsf2_h8300hs"
  [(set (match_operand:SF 0 "register_operand" "=r")
       (neg:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "xor.w\\t#32768,%e0"
  [(set_attr "length" "4")])

