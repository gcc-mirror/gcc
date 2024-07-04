;; -----------------------------------------------------------------
;; COMBINE PATTERNS
;; -----------------------------------------------------------------

;; insv:SI

(define_insn_and_split "*insv_si_1_n"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(match_operand:SI 2 "register_operand" "r"))]
  "INTVAL (operands[1]) < 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
		   (match_dup 2))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*insv_si_1_n_clobber_flags"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(match_operand:SI 2 "register_operand" "r"))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[1]) < 16"
  "bld\\t#0,%w2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "4")])

(define_insn_and_split "*insv_si_1_n_lshiftrt"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (match_operand:SI 3 "const_int_operand" "n")))]
  "INTVAL (operands[1]) < 16 && INTVAL (operands[3]) < 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
		   (lshiftrt:SI (match_dup 2) (match_dup 3)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*insv_si_1_n_lshiftrt_clobber_flags"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (match_operand:SI 3 "const_int_operand" "n")))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[1]) < 16 && INTVAL (operands[3]) < 16"
  "bld\\t%Z3,%Y2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "4")])

(define_insn_and_split "*insv_si_1_n_lshiftrt_16"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16)))]
  "INTVAL (operands[1]) < 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
		   (lshiftrt:SI (match_dup 2) (const_int 16)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*insv_si_1_n_lshiftrt_16_clobber_flags"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16)))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[1]) < 16"
  "rotr.w\\t%e2\;rotl.w\\t%e2\;bst\\t%Z1,%Y0"
  [(set_attr "length" "6")])

(define_insn_and_split "*insv_si_8_8"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(match_operand:SI 1 "register_operand" "r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 8) (const_int 8))
		   (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*insv_si_8_8_clobber_flags"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(match_operand:SI 1 "register_operand" "r"))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%w1,%x0"
  [(set_attr "length" "2")])

(define_insn_and_split "*insv_si_8_8_lshiftrt_8"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (const_int 8)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 8) (const_int 8))
		   (lshiftrt:SI (match_dup 1) (const_int 8)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*insv_si_8_8_lshiftrt_8_clobber_flags"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 8))
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (const_int 8)))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%x1,%x0"
  [(set_attr "length" "2")])

;; extzv:SI

(define_insn_and_split "*extzv_8_8"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "?0,r")
			 (const_int 8)
			 (const_int 8)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:SI (match_dup 1) (const_int 8) (const_int 8)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extzv_8_8_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "?0,r")
			 (const_int 8)
			 (const_int 8)))
   (clobber (reg:CC CC_REG))]
  ""
  "@
   mov.b\\t%x1,%w0\;extu.w\\t%f0\;extu.l\\t%S0
   sub.l\\t%S0,%S0\;mov.b\\t%x1,%w0"
  [(set_attr "length" "6,4")])

(define_insn_and_split "*extzv_8_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (const_int 16)))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:SI (match_dup 1) (const_int 8) (const_int 16)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extzv_8_16_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (const_int 16)))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.w\\t%e1,%f0\;extu.w\\t%f0\;extu.l\\t%S0"
  [(set_attr "length" "6")])

(define_insn_and_split "*extzv_16_8"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 16)
			 (const_int 8)))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_H8300H"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (zero_extract:SI (match_dup 1) (const_int 16) (const_int 8)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*extzv_16_8_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 16)
			 (const_int 8)))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300H"
  "mov.w\\t%e1,%f2\;mov.b\\t%x1,%w0\;mov.b\\t%w2,%x0\;extu.l\\t%S0"
  [(set_attr "length" "8")])

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
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0)
				(const_int 24)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])])

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
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0)
				(const_int 1)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])])

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
	      (clobber (match_dup 4))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (zero_extend:SI (match_dup 5)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[5] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

;; Accept (A >> 30) & 2 and the like.

(define_insn_and_split "*andsi3_lshiftrt_n_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "single_one_operand" "n")))]
  "exact_log2 (INTVAL (operands[3])) < 16
   && INTVAL (operands[2]) + exact_log2 (INTVAL (operands[3])) == 31"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (and:SI (lshiftrt:SI (match_dup 1) (match_dup 2))
			   (match_dup 3)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*andsi3_lshiftrt_n_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "single_one_operand" "n")))
   (clobber (reg:CC CC_REG))]
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
  [(parallel [(set (match_dup 0)
		   (and:SI (lshiftrt:SI (match_dup 0) (const_int 25))
			   (const_int 64)))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])])

;; plus:SI

(define_insn_and_split "*addsi3_upper"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (const_int 65536))
		 (match_operand:SI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:SI (mult:SI (match_dup 1) (const_int 65536))
			    (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addsi3_upper_clobber_regs"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (const_int 65536))
		 (match_operand:SI 2 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "add.w\\t%f1,%e0"
  [(set_attr "length" "2")])

(define_insn_and_split "*addsi3_lshiftrt_16_zexthi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
			      (const_int 16))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "0"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:SI (lshiftrt:SI (match_dup 1) (const_int 16))
			    (zero_extend:SI (match_dup 2))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*addsi3_lshiftrt_16_zexthi_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
			      (const_int 16))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "0"))))
   (clobber (reg:CC CC_REG))]
  ""
  "add.w\\t%e1,%f0\;xor.w\\t%e0,%e0\;rotxl.w\\t%e0"
  [(set_attr "length" "6")])

;;(define_insn_and_split "*addsi3_and_r_1"
;;  [(set (match_operand:SI 0 "register_operand" "=r")
;;	(plus:SI (and:SI (match_operand:SI 1 "register_operand" "r")
;;			 (const_int 1))
;;		 (match_operand:SI 2 "register_operand" "0")))]
;;  ""
;;  "#"
;;  "&& reload_completed"
;;  [(set (cc0) (compare (zero_extract:SI (match_dup 1)
;;					(const_int 1)
;;					(const_int 0))
;;		       (const_int 0)))
;;   (set (pc)
;;        (if_then_else (eq (cc0)
;;			  (const_int 0))
;;		      (label_ref (match_dup 3))
;;		      (pc)))
;;   (set (match_dup 2)
;;        (plus:SI (match_dup 2)
;;		 (const_int 1)))
;;   (match_dup 3)]
;;  {
;;    operands[3] = gen_label_rtx ();
;;  })

;;(define_insn_and_split "*addsi3_and_not_r_1"
;;  [(set (match_operand:SI 0 "register_operand" "=r")
;;	(plus:SI (and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
;;			 (const_int 1))
;;		 (match_operand:SI 2 "register_operand" "0")))]
;;  ""
;;  "#"
;;  "&& reload_completed"
;;  [(set (cc0) (compare (zero_extract:SI (match_dup 1)
;;					(const_int 1)
;;					(const_int 0))
;;		       (const_int 0)))
;;   (set (pc)
;;        (if_then_else (ne (cc0)
;;			  (const_int 0))
;;		      (label_ref (match_dup 3))
;;		      (pc)))
;;   (set (match_dup 2)
;;        (plus:SI (match_dup 2)
;;		 (const_int 1)))
;;   (match_dup 3)]
;;  {
;;    operands[3] = gen_label_rtx ();
;;  })

;; [ix]or:HI

(define_insn_and_split "*ixorhi3_zext"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "iorxor_operator"
	 [(zero_extend:HI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:HI 3 "register_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 1 [(zero_extend:HI (match_dup 2))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])


(define_insn "*ixorhi3_zext_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "iorxor_operator"
	 [(zero_extend:HI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:HI 3 "register_operand" "0")]))
   (clobber (reg:CC CC_REG))]
  ""
  "%c1.b\\t%X2,%s0"
  [(set_attr "length" "2")])

;; [ix]or:SI

(define_insn_and_split "*ixorsi3_zext_qi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 1 [(zero_extend:SI (match_dup 2))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*ixorsi3_zext_qi_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:QI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))
   (clobber (reg:CC CC_REG))]
  ""
  "%c1.b\\t%X2,%w0"
  [(set_attr "length" "2")])

(define_insn_and_split "*ixorsi3_zext_hi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 1 [(zero_extend:SI (match_dup 2))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*ixorsi3_zext_hi_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(zero_extend:SI (match_operand:HI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "0")]))
   (clobber (reg:CC CC_REG))]
  ""
  "%c1.w\\t%T2,%f0"
  [(set_attr "length" "2")])

(define_insn_and_split "*ixorsi3_ashift_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(ashift:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 1 [(ashift:SI (match_dup 2) (const_int 16))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*ixorsi3_ashift_16_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(ashift:SI (match_operand:SI 2 "register_operand" "r")
		     (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))
  (clobber (reg:CC CC_REG))]
  ""
  "%c1.w\\t%f2,%e0"
  [(set_attr "length" "2")])

(define_insn_and_split "*ixorsi3_lshiftrt_16"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		       (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 1 [(lshiftrt:SI (match_dup 2) (const_int 16))
				    (match_dup 3)]))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*ixorsi3_lshiftrt_16_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "iorxor_operator"
	 [(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
		       (const_int 16))
	  (match_operand:SI 3 "register_operand" "0")]))
   (clobber (reg:CC CC_REG))]
  ""
  "%c1.w\\t%e2,%f0"
  [(set_attr "length" "2")])

;; ior:HI

(define_insn_and_split "*iorhi3_ashift_8"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (ashift:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:HI (ashift:HI (match_dup 1) (const_int 8))
			   (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorhi3_ashift_8_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (ashift:HI (match_operand:HI 1 "register_operand" "r")
			   (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "or.b\\t%s1,%t0"
  [(set_attr "length" "2")])

(define_insn_and_split "*iorhi3_lshiftrt_8"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r")
			     (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:HI (lshiftrt:HI (match_dup 1) (const_int 8))
			   (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorhi3_lshiftrt_8_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r")
			     (const_int 8))
		(match_operand:HI 2 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "or.b\\t%t1,%s0"
  [(set_attr "length" "2")])

(define_insn_and_split "*iorhi3_two_qi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
		(ashift:HI (match_operand:HI 2 "register_operand" "r")
			   (const_int 8))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:HI (zero_extend:HI (match_dup 1))
			   (ashift:HI (match_dup 2) (const_int 8))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorhi3_two_qi_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
		(ashift:HI (match_operand:HI 2 "register_operand" "r")
			   (const_int 8))))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%s2,%t0"
  [(set_attr "length" "2")])

(define_insn_and_split "*iorhi3_two_qi_mem"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "memory_operand" "m"))
		(ashift:HI (subreg:HI (match_operand:QI 2 "memory_operand" "m") 0)
			   (const_int 8))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:HI (zero_extend:HI (match_dup 1))
			   (ashift:HI (subreg:HI (match_dup 2) 0)
				      (const_int 8))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorhi3_two_qi_mem_clobber_flags"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(ior:HI (zero_extend:HI (match_operand:QI 1 "memory_operand" "m"))
		(ashift:HI (subreg:HI (match_operand:QI 2 "memory_operand" "m") 0)
			   (const_int 8))))
   (clobber (reg:CC CC_REG))]
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
  [(parallel [(set (match_dup 0) (match_dup 3))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[3] = gen_rtx_MEM (HImode, XEXP (operands[2], 0));
  })

;; ior:SI

(define_insn_and_split "*iorsi3_two_hi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "0"))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (zero_extend:SI (match_dup 1))
			   (ashift:SI (match_dup 2) (const_int 16))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_two_hi_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "0"))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))))
   (clobber (reg:CC CC_REG))]
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
  [(parallel [(set (match_dup 3)
		   (ior:HI (zero_extend:HI (match_dup 1))
			   (ashift:HI (subreg:HI (match_dup 2) 0)
				      (const_int 8))))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0) (zero_extend:SI (match_dup 3)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

(define_insn_and_split "*iorsi3_e2f"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int -65536))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			     (const_int 16))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (match_dup 1) (const_int -65536))
			   (lshiftrt:SI (match_dup 2) (const_int 16))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_e2f_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int -65536))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			     (const_int 16))))
   (clobber (reg:CC CC_REG))]
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
  [(parallel [(set (match_dup 3)
		   (ior:HI (zero_extend:HI (match_dup 1))
			   (ashift:HI (match_dup 4) (const_int 8))))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0) (sign_extend:SI (match_dup 3)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
    operands[4] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

(define_insn_and_split "*iorsi3_w"
  [(set (match_operand:SI 0 "register_operand" "=r,&r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0,0")
			(const_int -256))
		(zero_extend:SI (match_operand:QI 2 "general_operand_src" "r,g>"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (match_dup 1) (const_int -256))
			   (zero_extend:SI (match_dup 2))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_w_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r,&r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0,0")
			(const_int -256))
		(zero_extend:SI (match_operand:QI 2 "general_operand_src" "r,g>"))))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%X2,%w0"
  [(set_attr "length" "2,8")])

(define_insn_and_split "*iorsi3_ashift_31"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (const_int 31))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (ashift:SI (match_dup 1) (const_int 31))
			   (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_ashift_31_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (const_int 31))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "rotxl.l\\t%S0\;bor\\t#0,%w1\;rotxr.l\\t%S0"
  [(set_attr "length" "6")])

(define_insn_and_split "*iorsi3_and_ashift"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))]
  "(INTVAL (operands[3]) & ~0xffff) == 0"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (ashift:SI (match_dup 1) (match_dup 2))
				   (match_dup 3))
			  (match_dup 4)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_and_ashift_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
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

(define_insn_and_split "*iorsi3_and_lshiftrt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))]
  "((INTVAL (operands[3]) << INTVAL (operands[2])) & ~0xffff) == 0"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (lshiftrt:SI (match_dup 1) (match_dup 2))
				   (match_dup 3))
			   (match_dup 4)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_and_lshiftrt_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (match_operand:SI 2 "const_int_operand" "n"))
			(match_operand:SI 3 "single_one_operand" "n"))
		(match_operand:SI 4 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
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

(define_insn_and_split "*iorsi3_zero_extract"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
				 (const_int 1)
				 (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "register_operand" "0")))]
  "INTVAL (operands[2]) < 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (zero_extract:SI (match_dup 1)
					    (const_int 1)
					    (match_dup 2))
			   (match_dup 3)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_zero_extract_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
				 (const_int 1)
				 (match_operand:SI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  "INTVAL (operands[2]) < 16"
  "bld\\t%Z2,%Y1\;bor\\t#0,%w0\;bst\\t#0,%w0"
  [(set_attr "length" "6")])

(define_insn_and_split "*iorsi3_and_lshiftrt_n_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 30))
			(const_int 2))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (lshiftrt:SI (match_dup 1) (const_int 30))
				   (const_int 2))
			   (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi3_and_lshiftrt_n_sb_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 30))
			(const_int 2))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (reg:CC CC_REG))]
  ""
  "rotl.l\\t%S1\;rotr.l\\t%S1\;bor\\t#1,%w0\;bst\\t#1,%w0"
  [(set_attr "length" "8")])

(define_insn_and_split "*iorsi3_and_lshiftrt_9_sb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 9))
			(const_int 4194304))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (match_scratch:HI 3 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (lshiftrt:SI (match_dup 1) (const_int 9))
				   (const_int 4194304))
			   (match_dup 2)))
	      (clobber (match_dup 3))
	      (clobber (reg:CC CC_REG))])])


(define_insn "*iorsi3_and_lshiftrt_9_sb_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (const_int 9))
			(const_int 4194304))
		(match_operand:SI 2 "register_operand" "0")))
   (clobber (match_scratch:HI 3 "=&r"))
   (clobber (reg:CC CC_REG))]
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
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		    (ior:SI (ashift:SI (match_dup 1) (const_int 16))
			    (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
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
  [(parallel [(set (match_dup 2) (match_dup 1))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (ior:SI (ashift:SI (match_dup 2) (const_int 16))
			   (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[3] = gen_rtx_REG (HImode, REGNO (operands[2]));
  })

(define_insn_and_split "*iorsi2_and_1_lshiftrt_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int 1))
		(lshiftrt:SI (match_dup 1)
			     (const_int 1))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:SI (and:SI (match_dup 1) (const_int 1))
			   (lshiftrt:SI (match_dup 1) (const_int 1))))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*iorsi2_and_1_lshiftrt_1_clobber_flags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int 1))
		(lshiftrt:SI (match_dup 1)
			     (const_int 1))))
   (clobber (reg:CC CC_REG))]
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
  [(parallel [(set (match_dup 3)
		   (ior:HI (ashift:HI (match_dup 4) (const_int 8))
			   (match_dup 3)))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])]
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
  [(parallel [(set (match_dup 3)
		   (ior:HI (zero_extend:HI (match_dup 1))
			   (ashift:HI (subreg:HI (match_dup 2) 0)
				      (const_int 8))))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0)
			      (const_int 16)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])]
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
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (plus:SI (mult:SI (match_dup 1) (const_int 65536))
			    (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
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
  [(parallel [(set (match_dup 2) (match_dup 1))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 3)
		   (ashift:HI (match_dup 3)
			      (const_int 7)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (plus:SI (mult:SI (match_dup 2) (const_int 65536))
			    (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
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
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0) (sign_extend:SI (match_dup 2)))
	      (clobber (reg:CC CC_REG))])
   (parallel [(set (match_dup 0)
		   (ashiftrt:SI (match_dup 0)
				(const_int 1)))
	      (clobber (scratch:QI))
	      (clobber (reg:CC CC_REG))])]
  {
    operands[2] = gen_rtx_REG (HImode, REGNO (operands[0]));
  })

;; Storing a part of HImode to QImode.

(define_insn_and_split ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh")
	(subreg:QI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 8)) 1))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (subreg:QI (lshiftrt:HI (match_dup 1)
							 (const_int 8)) 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:HI (match_operand:HI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 8)) 1))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%t1,%R0"
  [(set_attr "length" "8")])

;; Storing a part of SImode to QImode.

(define_insn_and_split ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 8)) 3))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (subreg:QI (lshiftrt:SI (match_dup 1) (const_int 8)) 3))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 8)) 3))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.b\\t%x1,%R0"
  [(set_attr "length" "8")])

(define_insn_and_split ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 16)) 3))
   (clobber (match_scratch:SI 2 "=&r,&r,&r,&r,&r,&r,&r,&r,&r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (subreg:QI (lshiftrt:SI (match_dup 1) (const_int 16)) 3))
	      (clobber (match_dup 2))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 16)) 3))
   (clobber (match_scratch:SI 2 "=&r,&r,&r,&r,&r,&r,&r,&r,&r"))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.w\\t%e1,%f2\;mov.b\\t%w2,%R0"
  [(set_attr "length" "10")])

(define_insn_and_split ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 24)) 3))
   (clobber (match_scratch:SI 2 "=&r,&r,&r,&r,&r,&r,&r,&r,&r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (subreg:QI (lshiftrt:SI (match_dup 1) (const_int 24)) 3))
	      (clobber (match_dup 2))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:QI 0 "general_operand_dst" "=rm,Za,Zb,Zc,Zd,Ze,Zf,Zh,Zg")
	(subreg:QI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7")
				(const_int 24)) 3))
   (clobber (match_scratch:SI 2 "=&r,&r,&r,&r,&r,&r,&r,&r,&r"))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.w\\t%e1,%f2\;mov.b\\t%x2,%R0"
  [(set_attr "length" "10")])

;;(define_insn_and_split ""
;;  [(set (pc)
;;	(if_then_else (eq (zero_extract:SI (subreg:SI (match_operand:QI 0 "register_operand" "") 0)
;;					   (const_int 1)
;;					   (const_int 7))
;;			  (const_int 0))
;;		      (label_ref (match_operand 1 "" ""))
;;		      (pc)))]
;;  ""
;;  "#"
;;  ""
;;  [(set (cc0) (compare (match_dup 0)
;;		       (const_int 0)))
;;   (set (pc)
;;	(if_then_else (ge (cc0)
;;			  (const_int 0))
;;		      (label_ref (match_dup 1))
;;		      (pc)))]
;;  "")
;; 
;; (define_insn_and_split ""
;;  [(set (pc)
;; 	(if_then_else (ne (zero_extract:SI (subreg:SI (match_operand:QI 0 "register_operand" "") 0)
;; 					   (const_int 1)
;; 					   (const_int 7))
;; 			  (const_int 0))
;; 		      (label_ref (match_operand 1 "" ""))
;; 		      (pc)))]
;;   ""
;;   "#"
;;   ""
;;   [(set (cc0) (compare (match_dup 0)
;; 		       (const_int 0)))
;;    (set (pc)
;; 	(if_then_else (lt (cc0)
;; 			  (const_int 0))
;; 		      (label_ref (match_dup 1))
;; 		      (pc)))]
;;   "")

;; This is a signed bitfield extraction starting at bit 0
;; It's usually faster than using shifts, but not always,
;; particularly on the H8/S and H8/SX variants.
(define_insn_and_split "*extvsi_n_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand 2 "const_int_operand")
			 (const_int 0)))]
  "INTVAL (operands[2]) > 1
   && INTVAL (operands[2]) < (TARGET_H8300S ? 26 - TARGET_H8300SX : 29)
   && (!TARGET_H8300SX || (INTVAL (operands[2]) != 24 && INTVAL (operands[2]) != 17))"
  "#"
  "&& reload_completed"
[(parallel [(set (match_dup 0) (and:SI (match_dup 0) (match_dup 3)))
	    (clobber (reg:CC CC_REG))])
 (parallel [(set (match_dup 0) (xor:SI (match_dup 0) (match_dup 4)))
	    (clobber (reg:CC CC_REG))])
 (parallel [(set (match_dup 0) (minus:SI (match_dup 0) (match_dup 4)))
	    (clobber (reg:CC CC_REG))])]
{
  int tmp = INTVAL (operands[2]);
  operands[3] = GEN_INT (~(HOST_WIDE_INT_M1U << tmp));
  operands[4] = GEN_INT (HOST_WIDE_INT_1U << (tmp - 1));
})

(define_insn_and_split "*extvsi_n_n"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand 2 "const_int_operand")
			 (match_operand 3 "const_int_operand")))]
  "(!h8300_shift_needs_scratch_p (INTVAL (operands[3]), SImode, LSHIFTRT)
    && use_extvsi (INTVAL (operands[2]), INTVAL (operands[3])))"
  "#"
  "&& reload_completed"
[(parallel [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 3)))
	    (clobber (reg:CC CC_REG))])
 (parallel [(set (match_dup 0) (and:SI (match_dup 0) (match_dup 4)))
	    (clobber (reg:CC CC_REG))])
 (parallel [(set (match_dup 0) (xor:SI (match_dup 0) (match_dup 5)))
	    (clobber (reg:CC CC_REG))])
 (parallel [(set (match_dup 0) (minus:SI (match_dup 0) (match_dup 5)))
	    (clobber (reg:CC CC_REG))])]
{
  int tmp = INTVAL (operands[2]);
  operands[4] = gen_int_mode (~(HOST_WIDE_INT_M1U << tmp), SImode);
  operands[5] = gen_int_mode (HOST_WIDE_INT_1U << (tmp - 1), SImode);
})

;;
;; Testing showed this only triggering with SImode, probably because
;; of how insv/extv are defined.
(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:QHSI 1 "register_operand" "0")
			 (const_int 1)
			 (match_operand 2 "immediate_operand")))]
  "!TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (sign_extract:SI (match_dup 1) (const_int 1) (match_dup 2)))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:QHSI 1 "register_operand" "0")
			 (const_int 1)
			 (match_operand 2 "immediate_operand")))
   (clobber (reg:CC CC_REG))]
  "!TARGET_H8300SX"
{
  int position = INTVAL (operands[2]);

  /* For bit position 31, 30, left shift the bit we want into C.  */
  bool bit_in_c = false;
  if (position == 31)
    {
      output_asm_insn ("shll.l\t%0", operands);
      bit_in_c = true;
    }
  else if (position == 30 && TARGET_H8300S)
    {
      output_asm_insn ("shll.l\t#2,%0", operands);
      bit_in_c = true;
    }

  /* Similar for positions 16, 17, but with a right shift into C.  */
  else if (position == 16)
    {
      output_asm_insn ("shlr.w\t%e0", operands);
      bit_in_c = true;
    }
  else if (position == 17 && TARGET_H8300S)
    {
      output_asm_insn ("shlr.w\t#2,%e0", operands);
      bit_in_c = true;
    }


  /* For all the other cases in the upper 16 bits, move the upper 16
     bits into the lower 16 bits, then use the standard sequence for
     extracting one of the low 16 bits.  */
  else if (position >= 16)
    {
      output_asm_insn ("mov.w\t%e1,%f0", operands);

      /* We'll use the standard sequences for the low word now.  */
      position %= 16;
    }

  /* Same size/speed as the general sequence, but slightly faster
     to simulate.  */
  if (position == 0)
    return "and.l\t#1,%0\;neg.l\t%0";

  rtx xoperands[3];
  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = GEN_INT (position);

  /* If the bit we want is not already in C, get it there  */
  if (!bit_in_c)
    {
      if (position >= 8)
	{
	  xoperands[2] = GEN_INT (position % 8);
	  output_asm_insn ("bld\t%2,%t1", xoperands);
	}
      else
	output_asm_insn ("bld\t%2,%s1", xoperands);
    }

  /* Now the bit we want is in C, emit the generalized sequence
     to get that bit into the destination, properly extended.  */
  return "subx\t%s0,%s0\;exts.w %T0\;exts.l %0";
}
  [(set (attr "length") (symbol_ref "INTVAL (operands[2]) >= 16 ? 10 : 8"))])

;; For shift counts >= 16 we can always do better than the
;; generic sequences.  Other patterns handle smaller counts.
(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (match_operand 2 "immediate_operand" "n"))
		(const_int 1)))]
  "!TARGET_H8300SX && INTVAL (operands[2]) >= 16"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (and:SI (lshiftrt:SI (match_dup 0) (match_dup 2))
					 (const_int 1)))
	      (clobber (reg:CC CC_REG))])])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
			     (match_operand 2 "immediate_operand" "n"))
		(const_int 1)))
   (clobber (reg:CC CC_REG))]
  "!TARGET_H8300SX && INTVAL (operands[2]) >= 16"
{
  int position = INTVAL (operands[2]);

  /* If the bit we want is the highest bit we can just rotate it into position
     and mask off everything else.  */
  if (position == 31)
    {
      output_asm_insn ("rotl.l\t%0", operands);
      return "and.l\t#1,%0";
    }

  /* Special case for H8/S.  Similar to bit 31.  */
  if (position == 30 && TARGET_H8300S)
    return "rotl.l\t#2,%0\;and.l\t#1,%0";

  if (position <= 30 && position >= 17)
    {
      /* Shift 16 bits, without worrying about extensions.  */
      output_asm_insn ("mov.w\t%e1,%f0", operands);

      /* Get the bit we want into C.  */
      operands[2] = GEN_INT (position % 8);
      if (position >= 24)
	output_asm_insn ("bld\t%2,%t0", operands);
      else
	output_asm_insn ("bld\t%2,%s0", operands);

      /* xor + rotate to clear the destination, then rotate
	 the C into position.  */
      return "xor.l\t%0,%0\;rotxl.l\t%0";
    }

  if (position == 16)
    {
      /* Shift 16 bits, without worrying about extensions.  */
      output_asm_insn ("mov.w\t%e1,%f0", operands);

      /* And finally, mask out everything we don't want.  */
      return "and.l\t#1,%0";
    }

  gcc_unreachable ();
}
  [(set_attr "length" "10")])
