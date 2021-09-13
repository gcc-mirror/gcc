;; -----------------------------------------------------------------
;; BIT FIELDS
;; -----------------------------------------------------------------
;; The H8/300 has given 1/8th of its opcode space to bitfield
;; instructions so let's use them as well as we can.

;; You'll never believe all these patterns perform one basic action --
;; load a bit from the source, optionally invert the bit, then store it
;; in the destination (which is known to be zero).
;;
;; Combine obviously need some work to better identify this situation and
;; canonicalize the form better.

;;
;; Inverted loads with a 16bit destination.
;;

(define_insn_and_split ""
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(zero_extract:HI (xor:HI (match_operand:HI 1 "register_operand" "r")
				 (match_operand:HI 3 "const_int_operand" "n"))
			 (const_int 1)
			 (match_operand:HI 2 "const_int_operand" "n")))]
  "(TARGET_H8300SX)
    && (1 << INTVAL (operands[2])) == INTVAL (operands[3])"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:HI (xor:HI (match_dup 1) (match_dup 3))
				    (const_int 1)
				    (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(zero_extract:HI (xor:HI (match_operand:HI 1 "register_operand" "r")
				 (match_operand:HI 3 "const_int_operand" "n"))
			 (const_int 1)
			 (match_operand:HI 2 "const_int_operand" "n")))
   (clobber (reg:CC CC_REG))]
  "(TARGET_H8300SX)
    && (1 << INTVAL (operands[2])) == INTVAL (operands[3])"
  "sub.w	%0,%0\;bild	%Z2,%Y1\;bst	#0,%X0"
  [(set_attr "length" "8")])

;;
;; Normal loads with a 32bit destination.
;;

(define_insn_and_split "*extzv_1_r"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "?0,r")
			 (const_int 1)
			 (match_operand 2 "const_int_operand" "n,n")))]
  "INTVAL (operands[2]) < 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:SI (match_dup 1) (const_int 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extzv_1_r_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "?0,r")
			 (const_int 1)
			 (match_operand 2 "const_int_operand" "n,n")))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[2]) < 16"
{
  return output_simode_bld (0, operands);
}
  [(set_attr "length" "8,6")])

;;
;; Inverted loads with a 32bit destination.
;;

(define_insn_and_split "*extzv_1_r_inv"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (xor:SI (match_operand:SI 1 "register_operand" "?0,r")
				 (match_operand 3 "const_int_operand" "n,n"))
			 (const_int 1)
			 (match_operand 2 "const_int_operand" "n,n")))]
  "INTVAL (operands[2]) < 16
    && (1 << INTVAL (operands[2])) == INTVAL (operands[3])"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:SI (xor:SI (match_dup 1) (match_dup 3))
				    (const_int 1)
				    (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extzv_1_r_inv_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (xor:SI (match_operand:SI 1 "register_operand" "?0,r")
				 (match_operand 3 "const_int_operand" "n,n"))
			 (const_int 1)
			 (match_operand 2 "const_int_operand" "n,n")))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[2]) < 16
    && (1 << INTVAL (operands[2])) == INTVAL (operands[3])"
{
  return output_simode_bld (1, operands);
}
  [(set_attr "length" "8,6")])

(define_expand "insv"
  [(set (zero_extract:HI (match_operand:HI 0 "general_operand" "")
			 (match_operand:HI 1 "general_operand" "")
			 (match_operand:HI 2 "general_operand" ""))
	(match_operand:HI 3 "general_operand" ""))]
  "TARGET_H8300SX"
  {
    if (GET_CODE (operands[1]) == CONST_INT
	&& GET_CODE (operands[2]) == CONST_INT
	&& INTVAL (operands[1]) <= 8
	&& INTVAL (operands[2]) >= 0
	&& INTVAL (operands[1]) + INTVAL (operands[2]) <= 8
	&& memory_operand (operands[0], GET_MODE (operands[0])))
      {
	/* If the source operand is zero, it's better to use AND rather
	   than BFST.  Likewise OR if the operand is all ones.  */
	if (GET_CODE (operands[3]) == CONST_INT)
	  {
	    HOST_WIDE_INT mask = (1 << INTVAL (operands[1])) - 1;
	    if ((INTVAL (operands[3]) & mask) == 0)
	      FAIL;
	    if ((INTVAL (operands[3]) & mask) == mask)
	      FAIL;
	  }
	if (! bit_memory_operand (operands[0], GET_MODE (operands[0])))
	  {
	    if (!can_create_pseudo_p ())
	      FAIL;
	    operands[0] =  replace_equiv_address (operands[0], force_reg (Pmode,
						  XEXP (operands[0], 0)));
	  }
	operands[3] = gen_lowpart (QImode, operands[3]);
	if (! operands[3])
	  FAIL;
	if (! register_operand (operands[3], QImode))
	  {
	    if (!can_create_pseudo_p ())
	      FAIL;
	    operands[3] = force_reg (QImode, operands[3]);
	  }
	emit_insn (gen_bfst (adjust_address (operands[0], QImode, 0),
					     operands[3], operands[1], operands[2]));
	DONE;
      }
    FAIL;
  })

(define_insn_and_split ""
  [(set (zero_extract:HI (match_operand:HI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:HI 1 "immediate_operand" "n"))
	(match_operand:HI 2 "register_operand" "r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:HI (match_dup 0) (const_int 1) (match_dup 1))
		   (match_dup 2))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (zero_extract:HI (match_operand:HI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:HI 1 "immediate_operand" "n"))
	(match_operand:HI 2 "register_operand" "r"))
   (clobber (reg:CC CC_REG))]
  ""
  "bld	#0,%R2\;bst	%Z1,%Y0 ; i1"
  [(set_attr "length" "4")])

(define_expand "extzv"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extract:HI (match_operand:HI 1 "bit_operand" "")
			 (match_operand:HI 2 "general_operand" "")
			 (match_operand:HI 3 "general_operand" "")))]
  "TARGET_H8300SX"
  {
    if (GET_CODE (operands[2]) == CONST_INT
	&& GET_CODE (operands[3]) == CONST_INT
	&& INTVAL (operands[2]) <= 8
	&& INTVAL (operands[3]) >= 0
	&& INTVAL (operands[2]) + INTVAL (operands[3]) <= 8
	&& memory_operand (operands[1], QImode))
      {
	rtx temp;

	/* Optimize the case where we're extracting into a paradoxical
	   subreg.  It's only necessary to extend to the inner reg.  */
	if (GET_CODE (operands[0]) == SUBREG
	    && subreg_lowpart_p (operands[0])
	    && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[0])))
		< GET_MODE_SIZE (GET_MODE (operands[0])))
	    && (GET_MODE_CLASS (GET_MODE (SUBREG_REG (operands[0])))
		== MODE_INT))
	   operands[0] = SUBREG_REG (operands[0]);

	if (!can_create_pseudo_p ())
	  temp = gen_lowpart (QImode, operands[0]);
	else
	  temp = gen_reg_rtx (QImode);
	if (! temp)
	  FAIL;
        if (! bit_memory_operand (operands[1], QImode))
	  {
	    if (!can_create_pseudo_p ())
	      FAIL;
	    operands[1] = replace_equiv_address (operands[1],
						 force_reg (Pmode, XEXP (operands[1], 0)));
	  }
	emit_insn (gen_bfld (temp, operands[1], operands[2], operands[3]));
	convert_move (operands[0], temp, 1);
	DONE;
      }
    FAIL;
  })

;; BAND, BOR, and BXOR patterns

(define_insn_and_split ""
  [(set (match_operand:HI 0 "bit_operand" "=Ur")
	(match_operator:HI 4 "bit_operator"
	 [(zero_extract:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 2 "immediate_operand" "n"))
	  (match_operand:HI 3 "bit_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 4 [(zero_extract:HI (match_dup 1)
						     (const_int 1)
						     (match_dup 2))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:HI 0 "bit_operand" "=Ur")
	(match_operator:HI 4 "bit_operator"
	 [(zero_extract:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 2 "immediate_operand" "n"))
	  (match_operand:HI 3 "bit_operand" "0")]))
   (clobber (reg:CC CC_REG))]
  ""
  "bld	%Z2,%Y1\;b%c4	#0,%R0\;bst	#0,%R0; bl1"
  [(set_attr "length" "6")])

(define_insn_and_split ""
  [(set (match_operand:HI 0 "bit_operand" "=Ur")
	(match_operator:HI 5 "bit_operator"
	 [(zero_extract:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 2 "immediate_operand" "n"))
	  (zero_extract:HI (match_operand:HI 3 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 4 "immediate_operand" "n"))]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 5 [(zero_extract:HI (match_dup 1)
						     (const_int 1)
						     (match_dup 2))
				    (zero_extract:HI (match_dup 3)
						     (const_int 1)
						     (match_dup 4))]))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:HI 0 "bit_operand" "=Ur")
	(match_operator:HI 5 "bit_operator"
	 [(zero_extract:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 2 "immediate_operand" "n"))
	  (zero_extract:HI (match_operand:HI 3 "register_operand" "r")
			   (const_int 1)
			   (match_operand:HI 4 "immediate_operand" "n"))]))
   (clobber (reg:CC CC_REG))]
  ""
  "bld	%Z2,%Y1\;b%c5	%Z4,%Y3\;bst	#0,%R0; bl3"
  [(set_attr "length" "6")])

(define_insn_and_split "bfld"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(zero_extract:QI (match_operand:QI 1 "bit_memory_operand" "WU")
			 (match_operand:QI 2 "immediate_operand" "n")
			 (match_operand:QI 3 "immediate_operand" "n")))]
  "TARGET_H8300SX && INTVAL (operands[2]) + INTVAL (operands[3]) <= 8"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:QI (match_dup 1) (match_dup 2) (match_dup 3)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "bfld_clobber_flags"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(zero_extract:QI (match_operand:QI 1 "bit_memory_operand" "WU")
			 (match_operand:QI 2 "immediate_operand" "n")
			 (match_operand:QI 3 "immediate_operand" "n")))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX && INTVAL (operands[2]) + INTVAL (operands[3]) <= 8"
{
  operands[2] = GEN_INT ((1 << (INTVAL (operands[2]) + INTVAL (operands[3])))
			 - (1 << INTVAL (operands[3])));
  return "bfld	%2,%1,%R0";
}
  [(set_attr "length_table" "bitfield")])

(define_insn_and_split "bfst"
  [(set (zero_extract:QI (match_operand:QI 0 "bit_memory_operand" "+WU")
			 (match_operand:QI 2 "immediate_operand" "n")
			 (match_operand:QI 3 "immediate_operand" "n"))
	(match_operand:QI 1 "register_operand" "r"))]
  "TARGET_H8300SX && INTVAL (operands[2]) + INTVAL (operands[3]) <= 8"
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:QI (match_dup 0) (match_dup 2) (match_dup 3))
		   (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "bfst_clobber_flags"
  [(set (zero_extract:QI (match_operand:QI 0 "bit_memory_operand" "+WU")
			 (match_operand:QI 2 "immediate_operand" "n")
			 (match_operand:QI 3 "immediate_operand" "n"))
	(match_operand:QI 1 "register_operand" "r"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX && INTVAL (operands[2]) + INTVAL (operands[3]) <= 8"
{
  operands[2] = GEN_INT ((1 << (INTVAL (operands[2]) + INTVAL (operands[3])))
			 - (1 << INTVAL (operands[3])));
  return "bfst	%R1,%2,%0";
}
  [(set_attr "length_table" "bitfield")])

;;(define_insn "*bstzhireg"
;;  [(set (match_operand:HI 0 "register_operand" "=r")
;;	(match_operator:HI 1 "eqne_operator" [(cc0) (const_int 0)]))]
;;  "TARGET_H8300SX"
;;  "mulu.w	#0,%T0\;b%k1	.Lh8BR%=\;inc.w	#1,%T0\\n.Lh8BR%=:")

;;(define_insn_and_split "*cmpstz"
;;  [(set (zero_extract:QI (match_operand:QI 0 "bit_memory_operand" "+WU,WU")
;;			 (const_int 1)
;;			 (match_operand:QI 1 "immediate_operand" "n,n"))
;;	(match_operator:QI 2 "eqne_operator"
;;	 [(match_operand 3 "h8300_dst_operand" "r,rQ")
;;	  (match_operand 4 "h8300_src_operand" "I,rQi")]))]
;;  "TARGET_H8300SX
;;   && (GET_MODE (operands[3]) == GET_MODE (operands[4])
;;       || GET_CODE (operands[4]) == CONST_INT)
;;   && GET_MODE_CLASS (GET_MODE (operands[3])) == MODE_INT
;;   && GET_MODE_SIZE (GET_MODE (operands[3])) <= 4"
;;  "#"
;;  "reload_completed"
;;  [(set (cc0) (match_dup 5))
;;   (set (zero_extract:QI (match_dup 0) (const_int 1) (match_dup 1))
;;	(match_op_dup:QI 2 [(cc0) (const_int 0)]))]
;;  {
;;    operands[5] = gen_rtx_COMPARE (VOIDmode, operands[3], operands[4]);
;;  })

;;(define_insn "*bstz"
;;  [(set (zero_extract:QI (match_operand:QI 0 "bit_memory_operand" "+WU")
;;			 (const_int 1)
;;			 (match_operand:QI 1 "immediate_operand" "n"))
;;	(eq:QI (cc0) (const_int 0)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bstz	%1,%0"
;;  [(set_attr "length_table" "unary")])

;;(define_insn "*bistz"
;;  [(set (zero_extract:QI (match_operand:QI 0 "bit_memory_operand" "+WU")
;;			 (const_int 1)
;;			 (match_operand:QI 1 "immediate_operand" "n"))
;;	(ne:QI (cc0) (const_int 0)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bistz	%1,%0"
;;  [(set_attr "length_table" "unary")])

;;(define_insn_and_split "*cmpcondbset"
;;  [(set (match_operand:QI 0 "nonimmediate_operand" "=WU,WU")
;;	(if_then_else:QI (match_operator 1 "eqne_operator"
;;			  [(match_operand 2 "h8300_dst_operand" "r,rQ")
;;			   (match_operand 3 "h8300_src_operand" "I,rQi")])
;;			 (ior:QI (match_operand:QI 4 "bit_memory_operand" "0,0")
;;				 (match_operand:QI 5 "single_one_operand" "n,n"))
;;			 (match_dup 4)))]
;;  "TARGET_H8300SX"
;;  "#"
;;  "reload_completed"
;;  [(set (cc0) (match_dup 6))
;;   (set (match_dup 0)
;;	(if_then_else:QI (match_op_dup 1 [(cc0) (const_int 0)])
;;			 (ior:QI (match_dup 4) (match_dup 5))
;;			 (match_dup 4)))]
;;  {
;;    operands[6] = gen_rtx_COMPARE (VOIDmode, operands[2], operands[3]);
;;  })

;;(define_insn "*condbset"
;;  [(set (match_operand:QI 0 "bit_memory_operand" "=WU")
;;	(if_then_else:QI (match_operator:QI 2 "eqne_operator"
;;			  [(cc0) (const_int 0)])
;;			 (ior:QI (match_operand:QI 3 "bit_memory_operand" "0")
;;				 (match_operand:QI 1 "single_one_operand" "n"))
;;			 (match_dup 3)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bset/%j2\t%V1,%0"
;;  [(set_attr "length_table" "logicb")])

;;(define_insn_and_split "*cmpcondbclr"
;;  [(set (match_operand:QI 0 "nonimmediate_operand" "=WU,WU")
;;	(if_then_else:QI (match_operator 1 "eqne_operator"
;;			  [(match_operand 2 "h8300_dst_operand" "r,rQ")
;;			   (match_operand 3 "h8300_src_operand" "I,rQi")])
;;			 (and:QI (match_operand:QI 4 "bit_memory_operand" "0,0")
;;				 (match_operand:QI 5 "single_zero_operand" "n,n"))
;;			 (match_dup 4)))]
;;  "TARGET_H8300SX"
;;  "#"
;;  "reload_completed"
;;  [(set (cc0) (match_dup 6))
;;   (set (match_dup 0)
;;	(if_then_else:QI (match_op_dup 1 [(cc0) (const_int 0)])
;;			 (and:QI (match_dup 4) (match_dup 5))
;;			 (match_dup 4)))]
;;  {
;;    operands[6] = gen_rtx_COMPARE (VOIDmode, operands[2], operands[3]);
;;  })

;;(define_insn "*condbclr"
;;  [(set (match_operand:QI 0 "bit_memory_operand" "=WU")
;;	(if_then_else:QI (match_operator:QI 2 "eqne_operator"
;;			  [(cc0) (const_int 0)])
;;			 (and:QI (match_operand:QI 3 "bit_memory_operand" "0")
;;				 (match_operand:QI 1 "single_zero_operand" "n"))
;;			 (match_dup 3)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bclr/%j2\t%W1,%0"
;;  [(set_attr "length_table" "logicb")])

;;(define_insn_and_split "*cmpcondbsetreg"
;;  [(set (match_operand:QI 0 "nonimmediate_operand" "=WU,WU")
;;	(if_then_else:QI (match_operator 1 "eqne_operator"
;;			  [(match_operand 2 "h8300_dst_operand" "r,rQ")
;;			   (match_operand 3 "h8300_src_operand" "I,rQi")])
;;			 (ior:QI (match_operand:QI 4 "bit_memory_operand" "0,0")
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 5 "register_operand" "r,r")))
;;			 (match_dup 4)))]
;;  "TARGET_H8300SX"
;;  "#"
;;  "reload_completed"
;;  [(set (cc0) (match_dup 6))
;;   (set (match_dup 0)
;;	(if_then_else:QI (match_op_dup 1 [(cc0) (const_int 0)])
;;			 (ior:QI (match_dup 4)
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 5 "register_operand" "r,r")))
;;			 (match_dup 4)))]
;;  {
;;    operands[6] = gen_rtx_COMPARE (VOIDmode, operands[2], operands[3]);
;;  })

;;(define_insn "*condbsetreg"
;;  [(set (match_operand:QI 0 "bit_memory_operand" "=WU")
;;	(if_then_else:QI (match_operator:QI 2 "eqne_operator"
;;			  [(cc0) (const_int 0)])
;;			 (ior:QI (match_operand:QI 3 "bit_memory_operand" "0")
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 1 "register_operand" "r")))
;;			 (match_dup 3)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bset/%j2\t%R1,%0"
;;  [(set_attr "length_table" "logicb")])

;;(define_insn_and_split "*cmpcondbclrreg"
;;  [(set (match_operand:QI 0 "nonimmediate_operand" "=WU,WU")
;;	(if_then_else:QI (match_operator 1 "eqne_operator"
;;			  [(match_operand 2 "h8300_dst_operand" "r,rQ")
;;			   (match_operand 3 "h8300_src_operand" "I,rQi")])
;;			 (and:QI (match_operand:QI 4 "bit_memory_operand" "0,0")
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 5 "register_operand" "r,r")))
;;			 (match_dup 4)))]
;;  "TARGET_H8300SX"
;;  "#"
;;  "reload_completed"
;;  [(set (cc0) (match_dup 6))
;;   (set (match_dup 0)
;;	(if_then_else:QI (match_op_dup 1 [(cc0) (const_int 0)])
;;			 (and:QI (match_dup 4)
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 5 "register_operand" "r,r")))
;;			 (match_dup 4)))]
;;  {
;;    operands[6] = gen_rtx_COMPARE (VOIDmode, operands[2], operands[3]);
;;  })

;;(define_insn "*condbclrreg"
;;  [(set (match_operand:QI 0 "bit_memory_operand" "=WU")
;;	(if_then_else:QI (match_operator:QI 2 "eqne_operator"
;;			  [(cc0) (const_int 0)])
;;			 (and:QI (match_operand:QI 3 "bit_memory_operand" "0")
;;				 (ashift:QI (const_int 1)
;;					    (match_operand:QI 1 "register_operand" "r")))
;;			 (match_dup 3)))]
;;  "TARGET_H8300SX && reload_completed"
;;  "bclr/%j2\t%R1,%0"
;;  [(set_attr "length_table" "logicb")])

