;; ----------------------------------------------------------------------
;; SHIFTS
;; ----------------------------------------------------------------------
;;
;; We make some attempt to provide real efficient shifting.  One example is
;; doing an 8-bit shift of a 16-bit value by moving a byte reg into the other
;; reg and moving 0 into the former reg.
;;
;; We also try to achieve this in a uniform way.  IE: We don't try to achieve
;; this in both rtl and at insn emit time.  Ideally, we'd use rtl as that would
;; give the optimizer more cracks at the code.  However, we wish to do things
;; like optimizing shifting the sign bit to bit 0 by rotating the other way.
;; There is rtl to handle this (rotate + and), but the H8/300 doesn't handle
;; 16-bit rotates.  Also, if we emit complicated rtl, combine may not be able
;; to detect cases it can optimize.
;;
;; For these and other fuzzy reasons, I've decided to go the less pretty but
;; easier "do it at insn emit time" route.


(define_expand "ashl<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(ashift:QHSI (match_operand:QHSI 1 "register_operand" "")
		     (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  {
    if (expand_a_shift (<MODE>mode, ASHIFT, operands))
    DONE;
  })

(define_expand "ashr<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(ashiftrt:QHSI (match_operand:QHSI 1 "register_operand" "")
		       (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  {
    if (expand_a_shift (<MODE>mode, ASHIFTRT, operands))
    DONE;
  })

(define_expand "lshr<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(lshiftrt:QHSI (match_operand:QHSI 1 "register_operand" "")
		       (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  {
    if (expand_a_shift (<MODE>mode, LSHIFTRT, operands))
    DONE;
  })

;; QI/HI/SI BIT SHIFTS

(define_insn ""
  [(set (match_operand:QHSI 0 "h8300_dst_operand" "=rQ")
	(match_operator:QHSI 3 "h8sx_unary_shift_operator"
	 [(match_operand:QHSI 1 "h8300_dst_operand" "0")
	  (match_operand:QI 2 "const_int_operand" "")]))]
  "h8300_operands_match_p (operands)"
{
  if (<MODE>mode == E_QImode)
    return output_h8sx_shift (operands, 'b', 'X');
  if (<MODE>mode == E_HImode)
    return output_h8sx_shift (operands, 'w', 'T');
  if (<MODE>mode == E_SImode)
    return output_h8sx_shift (operands, 'l', 'S');
  gcc_unreachable ();
}
  [(set_attr "length_table" "unary")
   (set_attr "cc" "set_znv")])

(define_insn ""
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(match_operator:QHSI 3 "h8sx_binary_shift_operator"
	 [(match_operand:QHSI 1 "register_operand" "0")
	  (match_operand:QI 2 "nonmemory_operand" "r P5>X")]))]
  ""
{
  if (<MODE>mode == QImode)
    return output_h8sx_shift (operands, 'b', 'X');
  if (<MODE>mode == HImode)
    return output_h8sx_shift (operands, 'w', 'T');
  if (<MODE>mode == SImode)
    return output_h8sx_shift (operands, 'l', 'S');
  gcc_unreachable ();
}
  [(set_attr "length" "4")
   (set_attr "cc" "set_znv")])

(define_insn "*shiftqi"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(match_operator:QI 3 "nshift_operator"
	 [(match_operand:QI 1 "register_operand" "0,0")
	  (match_operand:QI 2 "nonmemory_operand" "R,rn")]))
   (clobber (match_scratch:QI 4 "=X,&r"))]
  ""
{
  return output_a_shift (operands);
}
  [(set (attr "length")
	(symbol_ref "compute_a_shift_length (insn, operands)"))
   (set (attr "cc")
	(symbol_ref "compute_a_shift_cc (insn, operands)"))])

(define_insn "*shifthi"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(match_operator:HI 3 "nshift_operator"
	 [(match_operand:HI 1 "register_operand" "0,0")
	  (match_operand:QI 2 "nonmemory_operand" "S,rn")]))
   (clobber (match_scratch:QI 4 "=X,&r"))]
  ""
{
  return output_a_shift (operands);
}
  [(set (attr "length")
	(symbol_ref "compute_a_shift_length (insn, operands)"))
   (set (attr "cc")
	(symbol_ref "compute_a_shift_cc (insn, operands)"))])

(define_insn "*shiftsi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(match_operator:SI 3 "nshift_operator"
	 [(match_operand:SI 1 "register_operand" "0,0")
	  (match_operand:QI 2 "nonmemory_operand" "T,rn")]))
   (clobber (match_scratch:QI 4 "=X,&r"))]
  ""
{
  return output_a_shift (operands);
}
  [(set (attr "length")
	(symbol_ref "compute_a_shift_length (insn, operands)"))
   (set (attr "cc")
	(symbol_ref "compute_a_shift_cc (insn, operands)"))])


;; Split a variable shift into a loop.  If the register containing
;; the shift count dies, then we just use that register.

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operator 2 "nshift_operator"
	 [(match_dup 0)
	  (match_operand:QI 1 "register_operand" "")]))
   (clobber (match_operand:QI 3 "register_operand" ""))]
  "epilogue_completed
   && find_regno_note (insn, REG_DEAD, REGNO (operands[1]))"
  [(set (cc0) (compare (match_dup 1) (const_int 0)))
   (set (pc)
        (if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_dup 5))
		      (pc)))
   (match_dup 4)
   (parallel
     [(set (match_dup 0)
	   (match_op_dup 2 [(match_dup 0) (const_int 1)]))
      (clobber (scratch:QI))])
   (set (match_dup 1) (plus:QI (match_dup 1) (const_int -1)))
   (set (cc0) (compare (match_dup 1) (const_int 0)))
   (set (pc)
        (if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_dup 4))
		      (pc)))
   (match_dup 5)]
  {
    operands[4] = gen_label_rtx ();
    operands[5] = gen_label_rtx ();
  })

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operator 2 "nshift_operator"
	 [(match_dup 0)
	  (match_operand:QI 1 "register_operand" "")]))
   (clobber (match_operand:QI 3 "register_operand" ""))]
  "epilogue_completed
   && !find_regno_note (insn, REG_DEAD, REGNO (operands[1]))"
  [(set (match_dup 3)
	(match_dup 1))
   (set (cc0) (compare (match_dup 3) (const_int 0)))
   (set (pc)
        (if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_dup 5))
		      (pc)))
   (match_dup 4)
   (parallel
     [(set (match_dup 0)
	   (match_op_dup 2 [(match_dup 0) (const_int 1)]))
      (clobber (scratch:QI))])
   (set (match_dup 3) (plus:QI (match_dup 3) (const_int -1)))
   (set (cc0) (compare (match_dup 3) (const_int 0)))
   (set (pc)
        (if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_dup 4))
		      (pc)))
   (match_dup 5)]
  {
    operands[4] = gen_label_rtx ();
    operands[5] = gen_label_rtx ();
  })

;; ----------------------------------------------------------------------
;; ROTATIONS
;; ----------------------------------------------------------------------

(define_expand "rotl<mode>3"
  [(set (match_operand:QHSI 0 "register_operand" "")
	(rotate:QHSI (match_operand:QHSI 1 "register_operand" "")
		     (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  {
    if (expand_a_rotate (operands))
    DONE;
  })

(define_insn "rotl<mode>3_1"
  [(set (match_operand:QHSI 0 "register_operand" "=r")
	(rotate:QHSI (match_operand:QHSI 1 "register_operand" "0")
		     (match_operand:QI 2 "immediate_operand" "")))]
  ""
{
  return output_a_rotate (ROTATE, operands);
}
  [(set (attr "length")
	(symbol_ref "compute_a_rotate_length (operands)"))])
