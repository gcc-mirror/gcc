;; ----------------------------------------------------------------------
;; JUMP INSTRUCTIONS
;; ----------------------------------------------------------------------

;; Conditional jump instructions

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		        [(match_operand:QHSI 1 "h8300_dst_operand")
			 (match_operand:QHSI 2 "h8300_src_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "")

(define_insn_and_split "*branch"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:QHSI 1 "h8300_dst_operand" "rQ")
			(match_operand:QHSI 2 "h8300_src_operand" "rQi")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(match_dup 5))
   (set (pc)
	(if_then_else (match_op_dup 0
		       [(match_dup 4) (const_int 0)])
		      (label_ref (match_dup 3)) (pc)))]
  "
{
  machine_mode mode;

  if (REG_P (operands[1])
      && operands[2] == const0_rtx
      && (GET_CODE (operands[0]) == EQ
	  || GET_CODE (operands[0]) == NE
	  || GET_CODE (operands[0]) == LT
	  || GET_CODE (operands[0]) == GE
	  /* Our tstxx insns will set ZN and clear V, so we can handle
	     a couple additional cases.  */
	  || GET_CODE (operands[0]) == LE
	  || GET_CODE (operands[0]) == GT))
    mode = E_CCZNmode;
  else
    mode = E_CCmode;
  operands[4] = gen_rtx_REG (mode, CC_REG);
  operands[5] = gen_rtx_COMPARE (mode, operands[1], operands[2]);
}")

(define_insn "*branch_1"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(reg:H8cc CC_REG) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "reload_completed"
{
  if (get_attr_length (insn) == 2)
    return "b%j1	%l0";
  else if (get_attr_length (insn) == 4)
    return "b%j1	%l0:16";
  else
    return "b%k1	.Lh8BR%=\;jmp	@%l0\\n.Lh8BR%=:";
}
 [(set_attr "type" "branch")])


(define_insn "*branch_1_false"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(reg:H8cc CC_REG) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "reload_completed"
{
  if (get_attr_length (insn) == 2)
    return "b%k1	%l0";
  else if (get_attr_length (insn) == 4)
    return "b%k1	%l0:16";
  else
    return "b%j1	.Lh8BR%=\;jmp	@%l0\\n.Lh8BR%=:";
}
 [(set_attr "type" "branch")])

;; The brabc/brabs patterns have been disabled because their length computation
;; is horribly broken.  When we call out to a function via a SYMBOL_REF we get
;; bogus default and minimum lengths.  The trick used by the PA port seems to
;; fix the minimum, but not the default length.  The broken lengths can lead
;; to bogusly using a short jump when a long jump was needed and thus
;; incorrect code.
;;
;; Given the restricted addressing modes for operand 1, we could probably just
;; open-code the necessary length computation in the two affected patterns
;; rather than using a function call.  I think that would fix this problem.
(define_insn "*brabc"
  [(set (pc)
	(if_then_else (eq (zero_extract (match_operand:QI 1 "bit_memory_operand" "WU")
					(const_int 1)
					(match_operand:QI 2 "immediate_operand" "n"))
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "0 && TARGET_H8300SX"
{
  switch (get_attr_length (insn)
	  - h8300_insn_length_from_table (insn, operands))
    {
    case 2:
      return "bra/bc	%2,%R1,%l0";
    case 4:
      return "bra/bc	%2,%R1,%l0:16";
    default:
      return "bra/bs	%2,%R1,.Lh8BR%=\;jmp	@%l0\\n.Lh8BR%=:";
    }
}
  [(set_attr "type" "bitbranch")
   (set_attr "length_table" "bitbranch")])

(define_insn "*brabs"
  [(set (pc)
	(if_then_else (ne (zero_extract (match_operand:QI 1 "bit_memory_operand" "WU")
					(const_int 1)
					(match_operand:QI 2 "immediate_operand" "n"))
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "0 && TARGET_H8300SX"
{
  switch (get_attr_length (insn)
	  - h8300_insn_length_from_table (insn, operands))
    {
    case 2:
      return "bra/bs	%2,%R1,%l0";
    case 4:
      return "bra/bs	%2,%R1,%l0:16";
    default:
      return "bra/bc	%2,%R1,.Lh8BR%=\;jmp	@%l0\\n.Lh8BR%=:";
    }
}
  [(set_attr "type" "bitbranch")
   (set_attr "length_table" "bitbranch")])

(define_insn_and_split ""
  [(set (pc)
	(if_then_else (match_operator 3 "eqne_operator"
			[(zero_extract:QHSI (match_operand:QHSI 1 "register_operand" "r")
					    (const_int 1)
					    (match_operand 2 "const_int_operand" "n"))
			 (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "INTVAL (operands[2]) < 16"
  "#"
  "&& reload_completed"
  [(set (reg:CCZ CC_REG)
	(eq (zero_extract:QHSI (match_dup 1) (const_int 1) (match_dup 2))
	    (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 3 [(reg:CCZ CC_REG) (const_int 0)])
		      (label_ref (match_dup 0))
		      (pc)))])

(define_insn_and_split ""
  [(set (pc)
	(if_then_else (match_operator 3 "eqne_operator"
			[(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
					  (const_int 1)
					  (match_operand 2 "const_int_operand" "n"))
			 (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (match_scratch:SI 4 "=&r"))]
  "INTVAL (operands[2]) >= 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 4)
		   (ior:SI (and:SI (match_dup 4) (const_int -65536))
			   (lshiftrt:SI (match_dup 1) (const_int 16))))
	      (clobber (reg:CC CC_REG))])
   (set (reg:CCZ CC_REG)
	(eq (zero_extract:SI (match_dup 4) (const_int 1) (match_dup 2))
	    (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 3 [(reg:CCZ CC_REG) (const_int 0)])
		      (label_ref (match_dup 0))
		      (pc)))]
  "operands[2] = GEN_INT (INTVAL (operands[2]) - 16);")

;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
{
  if (final_sequence != 0)
    {
      if (get_attr_length (insn) == 2)
	return "bra/s	%l0";
      else
	{
	  /* The branch isn't short enough to use bra/s.  Output the
	     branch and delay slot in their normal order.

	     If this is a backward branch, it will now be branching two
	     bytes further than previously thought.  The length-based
	     test for bra vs. jump is very conservative though, so the
	     branch will still be within range.  */
	  rtx_sequence *seq;
	  int seen;

	  seq = final_sequence;
	  final_sequence = 0;
	  final_scan_insn (seq->insn (1), asm_out_file, optimize, 1, & seen);
	  final_scan_insn (seq->insn (0), asm_out_file, optimize, 1, & seen);
	  seq->insn (1)->set_deleted ();
	  return "";
	}
    }
  else if (get_attr_length (insn) == 2)
    return "bra	%l0";
  else if (get_attr_length (insn) == 4)
    return "bra	%l0:16";
  else
    return "jmp	@%l0";
}
  [(set_attr "type" "branch")
   (set (attr "delay_slot")
	(if_then_else (match_test "TARGET_H8300SX")
		      (const_string "jump")
		      (const_string "none")))])

;; This is a define expand, because pointers may be either 16 or 32 bits.

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" ""))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
  "")

(define_insn "tablejump<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  {
    if (<MODE>mode == E_HImode)
      return "jmp	@%0";
    if (<MODE>mode == E_SImode)
      return "jmp	@%S0";
    abort ();
  }
  [(set_attr "length" "2")])

;; This is a define expand, because pointers may be either 16 or 32 bits.

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "jump_address_operand" ""))]
  ""
  "")

(define_insn "*indirect_jump_<mode>"
  [(set (pc) (match_operand:P 0 "jump_address_operand" "Vr"))]
  ""
  {
    if (<MODE>mode == E_HImode)
      return "jmp	@%0";
    if (<MODE>mode == E_SImode)
      return "jmp	@%S0";
    abort ();
  }
  [(set_attr "length" "2")])

;; Call subroutine with no return value.

;; ??? Even though we use HImode here, this works on the H8/300H and H8S.

(define_expand "call"
  [(call (match_operand:QI 0 "call_expander_operand" "")
	 (match_operand 1 "general_operand" ""))]
  ""
  {
    if (!register_operand (XEXP (operands[0], 0), Pmode)
	&& GET_CODE (XEXP (operands[0], 0)) != SYMBOL_REF)
      XEXP (operands[0], 0) = force_reg (Pmode, XEXP (operands[0], 0));
  })

(define_insn "call_insn_<mode>"
  [(call (mem:QI (match_operand 0 "call_insn_operand" "Cr"))
	         (match_operand:P 1 "general_operand" "g"))]
  "!SIBLING_CALL_P (insn)"
{
  rtx xoperands[1];
  xoperands[0] = gen_rtx_MEM (QImode, operands[0]);
  gcc_assert (GET_MODE (operands[0]) == Pmode);
  if (GET_CODE (XEXP (xoperands[0], 0)) == SYMBOL_REF
      && (SYMBOL_REF_FLAGS (XEXP (xoperands[0], 0)) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    output_asm_insn ("jsr\\t@%0:8", xoperands);
  else
    output_asm_insn ("jsr\\t%0", xoperands);
  return "";
}
  [(set_attr "type" "call")
   (set (attr "length")
	(if_then_else (match_operand:QI 0 "small_call_insn_operand" "")
		      (const_int 2)
		      (const_int 4)))])

;; Call subroutine, returning value in operand 0
;; (which must be a hard register).

;; ??? Even though we use HImode here, this works on the H8/300H and H8S.

(define_expand "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:QI 1 "call_expander_operand" "")
	      (match_operand 2 "general_operand" "")))]
  ""
  {
    if (!register_operand (XEXP (operands[1], 0), Pmode)
	&& GET_CODE (XEXP (operands[1], 0)) != SYMBOL_REF)
      XEXP (operands[1], 0) = force_reg (Pmode, XEXP (operands[1], 0));
  })

(define_insn "call_value_insn_<mode>"
  [(set (match_operand 0 "" "=r")
	(call (mem:QI (match_operand 1 "call_insn_operand" "Cr"))
		      (match_operand:P 2 "general_operand" "g")))]
  "!SIBLING_CALL_P (insn)"
{
  rtx xoperands[2];
  gcc_assert (GET_MODE (operands[1]) == Pmode);
  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx_MEM (QImode, operands[1]);
  if (GET_CODE (XEXP (xoperands[1], 0)) == SYMBOL_REF
      && (SYMBOL_REF_FLAGS (XEXP (xoperands[1], 0)) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    output_asm_insn ("jsr\\t@%1:8", xoperands);
  else
    output_asm_insn ("jsr\\t%1", xoperands);
  return "";
}
  [(set_attr "type" "call")
   (set (attr "length")
	(if_then_else (match_operand:QI 0 "small_call_insn_operand" "")
		      (const_int 2)
		      (const_int 4)))])

(define_expand "sibcall"
  [(call (match_operand:QI 0 "call_expander_operand" "")
	 (match_operand 1 "general_operand" ""))]
  ""
  {
    if (!register_operand (XEXP (operands[0], 0), Pmode)
	&& GET_CODE (XEXP (operands[0], 0)) != SYMBOL_REF)
      XEXP (operands[0], 0) = force_reg (Pmode, XEXP (operands[0], 0));
  })

(define_insn "sibcall_insn_<mode>"
  [(call (mem:QI (match_operand 0 "call_insn_operand" "Cr"))
	         (match_operand:P 1 "general_operand" "g"))]
  "SIBLING_CALL_P (insn)"
{
  rtx xoperands[1];
  xoperands[0] = gen_rtx_MEM (QImode, operands[0]);
  gcc_assert (GET_MODE (operands[0]) == Pmode);
  if (GET_CODE (XEXP (xoperands[0], 0)) == SYMBOL_REF
      && (SYMBOL_REF_FLAGS (XEXP (xoperands[0], 0)) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    output_asm_insn ("jmp\\t@%0:8", xoperands);
  else
    output_asm_insn ("jmp\\t%0", xoperands);
  return "";
}
  [(set_attr "type" "call")
   (set (attr "length")
	(if_then_else (match_operand:QI 0 "small_call_insn_operand" "")
		      (const_int 2)
		      (const_int 4)))])

;; Call subroutine, returning value in operand 0
;; (which must be a hard register).

;; ??? Even though we use HImode here, this works on the H8/300H and H8S.

(define_expand "sibcall_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:QI 1 "call_expander_operand" "")
	      (match_operand 2 "general_operand" "")))]
  ""
  {
    if (!register_operand (XEXP (operands[1], 0), Pmode)
	&& GET_CODE (XEXP (operands[1], 0)) != SYMBOL_REF)
      XEXP (operands[1], 0) = force_reg (Pmode, XEXP (operands[1], 0));
  })

(define_insn "sibcall_value_insn_<mode>"
  [(set (match_operand 0 "" "=r")
	(call (mem:QI (match_operand 1 "call_insn_operand" "Cr"))
		      (match_operand:P 2 "general_operand" "g")))]
  "SIBLING_CALL_P (insn)"
{
  rtx xoperands[2];
  gcc_assert (GET_MODE (operands[1]) == Pmode);
  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx_MEM (QImode, operands[1]);
  if (GET_CODE (XEXP (xoperands[1], 0)) == SYMBOL_REF
      && (SYMBOL_REF_FLAGS (XEXP (xoperands[1], 0)) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    output_asm_insn ("jmp\\t@%1:8", xoperands);
  else
    output_asm_insn ("jmp\\t%1", xoperands);
  return "";
}
  [(set_attr "type" "call")
   (set (attr "length")
	(if_then_else (match_operand:QI 0 "small_call_insn_operand" "")
		      (const_int 2)
		      (const_int 4)))])

