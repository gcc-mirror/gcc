;; -----------------------------------------------------------------
;; COMBINE PATTERNS
;; -----------------------------------------------------------------

;; insv:SI

(define_insn "*insv_si_1_n"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(match_operand:SI 2 "register_operand" "r"))]
  "INTVAL (operands[1]) < 16"
  "bld\\t#0,%w2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "4")])

(define_insn "*insv_si_1_n_lshiftrt"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (match_operand:SI 3 "const_int_operand" "n")))]
  "INTVAL (operands[1]) < 16 && INTVAL (operands[3]) < 16"
  "bld\\t%Z3,%Y2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "4")])

(define_insn "*insv_si_1_n_lshiftrt_16"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16)))]
  "INTVAL (operands[1]) < 16"
  "rotr.w\\t%e2\;rotl.w\\t%e2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "6")])

(define_insn "*insv_si_8_8"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(match_operand:SI 1 "register_operand" "r"))]
  ""
  "mov.b\\t%w1,%x0"
  [(set_attr "length" "2")])

(define_insn "*insv_si_8_8_lshiftrt_8"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (const_int 8)))]
  ""
  "mov.b\\t%x1,%x0"
  [(set_attr "length" "2")])

;; extzv:SI

(define_insn "*extzv_8_8"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "?0,r")
			 (const_int 8)
			 (const_int 8)))]
  ""
  "@
   mov.b\\t%x1,%w0\;extu.w\\t%f0\;extu.l\\t%S0
   sub.l\\t%S0,%S0\;mov.b\\t%x1,%w0"
  [(set_attr "cc" "set_znv,clobber")
   (set_attr "length" "6,4")])

(define_insn "*extzv_8_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (const_int 16)))]
  ""
  "mov.w\\t%e1,%f0\;extu.w\\t%f0\;extu.l\\t%S0"
  [(set_attr "cc" "set_znv")
   (set_attr "length" "6")])

(define_insn "*extzv_16_8"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 16)
			 (const_int 8)))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_H8300H"
  "mov.w\\t%e1,%f2\;mov.b\\t%x1,%w0\;mov.b\\t%w2,%x0\;extu.l\\t%S0"
  [(set_attr "length" "8")
   (set_attr "cc" "set_znv")])

;; Extract the exponent of a float.

(define_insn_and_split "*extzv_8_23"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "0")
			 (const_int 8)
			 (const_int 23)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 1)))
	      (clobber (scratch:QI))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0)
				(const_int 24)))
	      (clobber (scratch:QI))])]
  "")

;; and:SI

;; ((SImode) HImode) << 15

(define_insn_and_split "*twoshifts_l16_r1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand" "0")
			   (const_int 15))
		(const_int 2147450880)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0)
				(const_int 1)))
	      (clobber (scratch:QI))])]
  "")

;; Transform (SImode << B) & 0xffff into (SImode) (HImode << B).

(define_insn_and_split "*andsi3_ashift_n_lower"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand" "0,0")
			   (match_operand:QI 2 "const_int_operand" "S,n"))
		(match_operand:SI 3 "const_int_operand" "n,n")))
   (clobber (match_scratch:QI 4 "=X,&r"))]
  "INTVAL (operands[2]) <= 15
   && UINTVAL (operands[3]) == ((HOST_WIDE_INT_M1U << INTVAL (operands[2]))
				& 0xffff)"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 5)
		   (ashift:HI (match_dup 5)
			      (match_dup 2)))
	      (clobber (match_dup 4))])
   (set (match_dup 0)
	(zero_extend:SI (match_dup 5)))]
  {
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

;; Accept (A >> 30) & 2 and the like.

(define_insn "*andsi3_lshiftrt_n_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "single_one_operand" "n")))]
  "exact_log2 (INTVAL (operands[3])) < 16
   && INTVAL (operands[2]) + exact_log2 (INTVAL (operands[3])) == 31"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  return "shll.l\\t%S0\;xor.l\\t%S0,%S0\;bst\\t%Z3,%Y0";
}
  [(set_attr "length" "8")])

(define_insn_and_split "*andsi3_lshiftrt_9_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (const_int 9))
		(const_int 4194304)))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(and:SI (lshiftrt:SI (match_dup 0)
			     (const_int 25))
		(const_int 64)))
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))])]
  "")

;; plus:SI

(define_insn "*addsi3_upper"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (const_int 65536))
		 (match_operand:SI 2 "register_operand" "0")))]
  ""
  "add.w\\t%f1,%e0"
  [(set_attr "length" "2")])

(define_insn "*addsi3_lshiftrt_16_zexthi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
			      (const_int 16))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "0"))))]
  ""
  "add.w\\t%e1,%f0\;xor.w\\t%e0,%e0\;rotxl.w\\t%e0"
  [(set_attr "length" "6")])

(define_insn_and_split "*addsi3_and_r_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (and:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 1))
		 (match_operand:SI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(set (cc0) (compare (zero_extract:SI (match_dup 1)
					(const_int 1)
					(const_int 0))
		       (const_int 0)))
   (set (pc)
        (if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_dup 3))
		      (pc)))
   (set (match_dup 2)
        (plus:SI (match_dup 2)
		 (const_int 1)))
   (match_dup 3)]
  {
    operands[3] = gen_label_rtx ();
  })

(define_insn_and_split "*addsi3_and_not_r_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
			 (const_int 1))
		 (match_operand:SI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(set (cc0) (compare (zero_extract:SI (match_dup 1)
					(const_int 1)
					(const_int 0))
		       (const_int 0)))
   (set (pc)
        (if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_dup 3))
		      (pc)))
   (set (match_dup 2)
        (plus:SI (match_dup 2)
		 (const_int 1)))
   (match_dup 3)]
  {
    operands[3] = gen_label_rtx ();
  })

;; [ix]or:HI

(define_insn "*ixorhi3_zext"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "iorxor_operator"
	 [(zero_extend:HI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:HI 3 "register_operand" "0")]))]
  ""
  "%c1.b\\t%X2,%s0"
  [(set_attr "length" "2")])

;; [ix]or:SI

(define_insn "*ixorsi3_zext_qi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "%c1.b\\t%X2,%w0"
  [(set_attr "length" "2")])

(define_insn "*ixorsi3_zext_hi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "%c1.w\\t%T2,%f0"
  [(set_attr "length" "2")])

(define_insn "*ixorsi3_ashift_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(ashift:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "%c1.w\\t%f2,%e0"
  [(set_attr "length" "2")])

(define_insn "*ixorsi3_lshiftrt_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		       (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "%c1.w\\t%e2,%f0"
  [(set_attr "length" "2")])

;; ior:HI

(define_insn "*iorhi3_ashift_8"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (ashift:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))]
  ""
  "or.b\\t%s1,%t0"
  [(set_attr "length" "2")])

(define_insn "*iorhi3_lshiftrt_8"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r")
			     (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))]
  ""
  "or.b\\t%t1,%s0"
  [(set_attr "length" "2")])

(define_insn "*iorhi3_two_qi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
		(ashift:HI (match_operand:HI 2 "register_operand" "r")
			   (const_int 8))))]
  ""
  "mov.b\\t%s2,%t0"
  [(set_attr "length" "2")])

(define_insn "*iorhi3_two_qi_mem"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "memory_operand" "m"))
		(ashift:HI (subreg:HI (match_operand:QI 2 "memory_operand" "m") 0)
			   (const_int 8))))]
  ""
  "mov.b\\t%X2,%t0\;mov.b\\t%X1,%s0"
  [(set_attr "length" "16")])

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "memory_operand" ""))
		(ashift:HI (subreg:HI (match_operand:QI 2 "memory_operand" "") 0)
			   (const_int 8))))]
  "reload_completed
   && byte_accesses_mergeable_p (XEXP (operands[2], 0), XEXP (operands[1], 0))"
  [(set (match_dup 0)
	(match_dup 3))]
  {
    operands[3] = gen_rtx_MEM (HImode, XEXP (operands[2], 0));
  })

;; ior:SI

(define_insn "*iorsi3_two_hi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "0"))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))))]
  ""
  "mov.w\\t%f2,%e0"
  [(set_attr "length" "2")])

(define_insn_and_split "*iorsi3_two_qi_zext"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ior:SI (zero_extend:SI (match_operand:QI 1 "memory_operand" "m"))
		(and:SI (ashift:SI (subreg:SI (match_operand:QI 2 "memory_operand" "m") 0)
				   (const_int 8))
			(const_int 65280))))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(ior:HI (zero_extend:HI (match_dup 1))
		(ashift:HI (subreg:HI (match_dup 2) 0)
			   (const_int 8))))
   (set (match_dup 0)
	(zero_extend:SI (match_dup 3)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

(define_insn "*iorsi3_e2f"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int -65536))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			     (const_int 16))))]
  ""
  "mov.w\\t%e2,%f0"
  [(set_attr "length" "2")])

(define_insn_and_split "*iorsi3_two_qi_sext"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extend:SI (match_operand:QI 1 "register_operand" "0"))
		(ashift:SI (sign_extend:SI (match_operand:QI 2 "register_operand" "r"))
			   (const_int 8))))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(ior:HI (zero_extend:HI (match_dup 1))
		(ashift:HI (match_dup 4)
			   (const_int 8))))
   (set (match_dup 0)
	(sign_extend:SI (match_dup 3)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

(define_insn "*iorsi3_w"
  [(set (match_operand:SI 0 "register_operand" "=r,&r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0,0")
			(const_int -256))
		(zero_extend:SI (match_operand:QI 2 "general_operand_src" "r,g>"))))]
  ""
  "mov.b\\t%X2,%w0"
  [(set_attr "length" "2,8")])

(define_insn "*iorsi3_ashift_31"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (const_int 31))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "rotxl.l\\t%S0\;bor\\t#0,%w1\;rotxr.l\\t%S0"
  [(set_attr "length" "6")
   (set_attr "cc" "set_znv")])

(define_insn "*iorsi3_and_ashift"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))]
  "(INTVAL (operands[3]) & ~0xffff) == 0"
{
  rtx srcpos = GEN_INT (exact_log2 (INTVAL (operands[3]))
			- INTVAL (operands[2]));
  rtx dstpos = GEN_INT (exact_log2 (INTVAL (operands[3])));
  operands[2] = srcpos;
  operands[3] = dstpos;
  return "bld\\t%Z2,%Y1\;bor\\t%Z3,%Y0\;bst\\t%Z3,%Y0";
}
  [(set_attr "length" "6")])

(define_insn "*iorsi3_and_lshiftrt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))]
  "((INTVAL (operands[3]) << INTVAL (operands[2])) & ~0xffff) == 0"
{
  rtx srcpos = GEN_INT (exact_log2 (INTVAL (operands[3]))
			+ INTVAL (operands[2]));
  rtx dstpos = GEN_INT (exact_log2 (INTVAL (operands[3])));
  operands[2] = srcpos;
  operands[3] = dstpos;
  return "bld\\t%Z2,%Y1\;bor\\t%Z3,%Y0\;bst\\t%Z3,%Y0";
}
  [(set_attr "length" "6")])

(define_insn "*iorsi3_zero_extract"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
				 (const_int 1)
				 (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "register_operand" "0")))]
  "INTVAL (operands[2]) < 16"
  "bld\\t%Z2,%Y1\;bor\\t#0,%w0\;bst\\t#0,%w0"
  [(set_attr "length" "6")])

(define_insn "*iorsi3_and_lshiftrt_n_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 30))
			(const_int 2))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "rotl.l\\t%S1\;rotr.l\\t%S1\;bor\\t#1,%w0\;bst\\t#1,%w0"
  [(set_attr "length" "8")])

(define_insn "*iorsi3_and_lshiftrt_9_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 9))
			(const_int 4194304))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (match_scratch:HI 3 "=&r"))]
  ""
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "shll.l\\t%S1\;xor.w\\t%T3,%T3\;bst\\t#6,%s3\;or.w\\t%T3,%e0";
  else
    return "rotl.l\\t%S1\;rotr.l\\t%S1\;xor.w\\t%T3,%T3\;bst\\t#6,%s3\;or.w\\t%T3,%e0";
}
  [(set_attr "length" "10")])

;; Used to OR the exponent of a float.

(define_insn "*iorsi3_shift"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (const_int 23))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "")
			   (const_int 23))
		(match_dup 0)))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "epilogue_completed
   && find_regno_note (insn, REG_DEAD, REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])"
  [(parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))])
   (set (match_dup 0)
	(ior:SI (ashift:SI (match_dup 1)
			   (const_int 16))
		(match_dup 0)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[1]));
  })

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "")
			   (const_int 23))
		(match_dup 0)))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "epilogue_completed
   && !(find_regno_note (insn, REG_DEAD, REGNO (operands[1]))
	&& REGNO (operands[0]) != REGNO (operands[1]))"
  [(set (match_dup 2)
	(match_dup 1))
   (parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))])
   (set (match_dup 0)
	(ior:SI (ashift:SI (match_dup 2)
			   (const_int 16))
		(match_dup 0)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

(define_insn "*iorsi2_and_1_lshiftrt_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int 1))
		(lshiftrt:SI (match_dup 1)
			     (const_int 1))))]
  ""
  "shlr.l\\t%S0\;bor\\t#0,%w0\;bst\\t#0,%w0"
  [(set_attr "length" "6")])

(define_insn_and_split "*iorsi3_ashift_16_ashift_24"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "0")
			   (const_int 16))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 24))))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (ior:HI (ashift:HI (match_dup 4)
			   (const_int 8))
		(match_dup 3)))
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))])]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

(define_insn_and_split "*iorsi3_ashift_16_ashift_24_mem"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ior:SI (and:SI (ashift:SI (subreg:SI (match_operand:QI 1 "memory_operand" "m") 0)
				   (const_int 16))
			(const_int 16711680))
		(ashift:SI (subreg:SI (match_operand:QI 2 "memory_operand" "m") 0)
			   (const_int 24))))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
        (ior:HI (zero_extend:HI (match_dup 1))
		(ashift:HI (subreg:HI (match_dup 2) 0)
			   (const_int 8))))
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))])]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

;; Used to add the exponent of a float.

(define_insn "*addsi3_shift"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (const_int 8388608))
		 (match_operand:SI 2 "register_operand" "0")))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			  (const_int 8388608))
		 (match_dup 0)))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "epilogue_completed
   && find_regno_note (insn, REG_DEAD, REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])"
  [(parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))])
   (set (match_dup 0)
	(plus:SI (mult:SI (match_dup 1)
			  (const_int 65536))
		 (match_dup 0)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[1]));
  })

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			  (const_int 8388608))
		 (match_dup 0)))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "epilogue_completed
   && !(find_regno_note (insn, REG_DEAD, REGNO (operands[1]))
	&& REGNO (operands[0]) != REGNO (operands[1]))"
  [(set (match_dup 2)
	(match_dup 1))
   (parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))])
   (set (match_dup 0)
	(plus:SI (mult:SI (match_dup 2)
			  (const_int 65536))
		 (match_dup 0)))]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

;; ashift:SI

(define_insn_and_split "*ashiftsi_sextqi_7"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (sign_extend:SI (match_operand:QI 1 "register_operand" "0"))
		   (const_int 7)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 2)
		   (ashift:HI (match_dup 2)
			      (const_int 8)))
	      (clobber (scratch:QI))])
   (set (match_dup 0)
	(sign_extend:SI (match_dup 2)))
   (parallel [(set (match_dup 0)
		   (ashiftrt:SI (match_dup 0)
				(const_int 1)))
	      (clobber (scratch:QI))])]
  {
    operands[2] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

;; Storing a part of HImode to QImode.

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm<")
	(subreg:QI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r")
				(const_int 8)) 1))]
  ""
  "mov.b\\t%t1,%R0"
  [(set_attr "cc" "set_znv")
   (set_attr "length" "8")])

;; Storing a part of SImode to QImode.

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm<")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				(const_int 8)) 3))]
  ""
  "mov.b\\t%x1,%R0"
  [(set_attr "cc" "set_znv")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm<")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				(const_int 16)) 3))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "mov.w\\t%e1,%f2\;mov.b\\t%w2,%R0"
  [(set_attr "cc" "set_znv")
   (set_attr "length" "10")])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm<")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				(const_int 24)) 3))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "mov.w\\t%e1,%f2\;mov.b\\t%x2,%R0"
  [(set_attr "cc" "set_znv")
   (set_attr "length" "10")])

(define_insn_and_split ""
  [(set (pc)
	(if_then_else (eq (zero_extract:SI (subreg:SI (match_operand:QI 0 "register_operand" "") 0)
					   (const_int 1)
					   (const_int 7))
			  (const_int 0))
		      (match_operand 1 "pc_or_label_operand" "")
		      (match_operand 2 "pc_or_label_operand" "")))]
  "operands[1] == pc_rtx || operands[2] == pc_rtx"
  "#"
  ""
  [(set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (match_dup 1)
		      (match_dup 2)))])

(define_insn_and_split ""
  [(set (pc)
	(if_then_else (ne (zero_extract:SI (subreg:SI (match_operand:QI 0 "register_operand" "") 0)
					   (const_int 1)
					   (const_int 7))
			  (const_int 0))
		      (match_operand 1 "pc_or_label_operand" "")
		      (match_operand 2 "pc_or_label_operand" "")))]
  "operands[1] == pc_rtx || operands[2] == pc_rtx"
  "#"
  ""
  [(set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (match_dup 1)
		      (match_dup 2)))])
