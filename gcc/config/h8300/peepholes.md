;; -----------------------------------------------------------------
;; PEEPHOLE PATTERNS
;; -----------------------------------------------------------------

;; Convert (A >> B) & C to (A & 255) >> B if C == 255 >> B.

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
		   (lshiftrt:HI (match_dup 0)
				(match_operand:HI 1 "const_int_operand" "")))
	      (clobber (match_operand:HI 2 "" ""))])
   (set (match_dup 0)
	(and:HI (match_dup 0)
		(match_operand:HI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (255 >> INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:HI (match_dup 0)
		(const_int 255)))
   (parallel [(set (match_dup 0)
		   (lshiftrt:HI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Convert (A << B) & C to (A & 255) << B if C == 255 << B.

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
		   (ashift:HI (match_dup 0)
			      (match_operand:HI 1 "const_int_operand" "")))
	      (clobber (match_operand:HI 2 "" ""))])
   (set (match_dup 0)
	(and:HI (match_dup 0)
		(match_operand:HI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (255 << INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:HI (match_dup 0)
		(const_int 255)))
   (parallel [(set (match_dup 0)
		   (ashift:HI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Convert (A >> B) & C to (A & 255) >> B if C == 255 >> B.

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (lshiftrt:SI (match_dup 0)
				(match_operand:SI 1 "const_int_operand" "")))
	      (clobber (match_operand:SI 2 "" ""))])
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand:SI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (255 >> INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 255)))
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Convert (A << B) & C to (A & 255) << B if C == 255 << B.

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ashift:SI (match_dup 0)
			      (match_operand:SI 1 "const_int_operand" "")))
	      (clobber (match_operand:SI 2 "" ""))])
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand:SI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (255 << INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 255)))
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Convert (A >> B) & C to (A & 65535) >> B if C == 65535 >> B.

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (lshiftrt:SI (match_dup 0)
				(match_operand:SI 1 "const_int_operand" "")))
	      (clobber (match_operand:SI 2 "" ""))])
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand:SI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (65535 >> INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 65535)))
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Convert (A << B) & C to (A & 65535) << B if C == 65535 << B.

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ashift:SI (match_dup 0)
			      (match_operand:SI 1 "const_int_operand" "")))
	      (clobber (match_operand:SI 2 "" ""))])
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand:SI 3 "const_int_operand" "")))]
  "INTVAL (operands[3]) == (65535 << INTVAL (operands[1]))"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 65535)))
   (parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 0) (match_dup 1)))
	      (clobber (match_dup 2))])]
  "")

;; Cram four pushes into stm.l.

(define_peephole2
  [(set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 2 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 3 "register_operand" ""))]
  "TARGET_H8300S && !TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[3])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && REGNO (operands[2]) == REGNO (operands[0]) + 2
       && REGNO (operands[3]) == REGNO (operands[0]) + 3
       && (TARGET_H8300SX || REGNO (operands[0]) == 0))"
  [(parallel [(set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -12)))
		   (match_dup 2))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -16)))
		   (match_dup 3))
	      (set (reg:SI SP_REG)
		   (plus:SI (reg:SI SP_REG)
			    (const_int -16)))])]
  "")

(define_peephole2
  [(set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 2 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 3 "register_operand" ""))]
  "TARGET_H8300S && TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[3])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && REGNO (operands[2]) == REGNO (operands[0]) + 2
       && REGNO (operands[3]) == REGNO (operands[0]) + 3
       && (TARGET_H8300SX || REGNO (operands[0]) == 0))"
  [(parallel [(set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -12)))
		   (match_dup 2))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -16)))
		   (match_dup 3))
	      (set (reg:HI SP_REG)
		   (plus:HI (reg:HI SP_REG)
			    (const_int -16)))])]
  "")

;; Cram three pushes into stm.l.

(define_peephole2
  [(set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 2 "register_operand" ""))]
  "TARGET_H8300S && !TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[2])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && REGNO (operands[2]) == REGNO (operands[0]) + 2
       && (TARGET_H8300SX || (REGNO (operands[0]) & 3) == 0))"
  [(parallel [(set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -12)))
		   (match_dup 2))
	      (set (reg:SI SP_REG)
		   (plus:SI (reg:SI SP_REG)
			    (const_int -12)))])]
  "")

(define_peephole2
  [(set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 2 "register_operand" ""))]
  "TARGET_H8300S && TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[2])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && REGNO (operands[2]) == REGNO (operands[0]) + 2
       && (TARGET_H8300SX || (REGNO (operands[0]) & 3) == 0))"
  [(parallel [(set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -12)))
		   (match_dup 2))
	      (set (reg:HI SP_REG)
		   (plus:HI (reg:HI SP_REG)
			    (const_int -12)))])]
  "")

;; Cram two pushes into stm.l.

(define_peephole2
  [(set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:SI (reg:SI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))]
  "TARGET_H8300S && !TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[1])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && (TARGET_H8300SX || (REGNO (operands[0]) & 1) == 0))"
  [(parallel [(set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:SI (reg:SI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (reg:SI SP_REG)
		   (plus:SI (reg:SI SP_REG)
			    (const_int -8)))])]
  "")

(define_peephole2
  [(set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 0 "register_operand" ""))
   (set (mem:SI (pre_dec:HI (reg:HI SP_REG)))
	(match_operand:SI 1 "register_operand" ""))]
  "TARGET_H8300S && TARGET_NORMAL_MODE
   && (REGNO_REG_CLASS (REGNO (operands[1])) == GENERAL_REGS
       && REGNO (operands[1]) == REGNO (operands[0]) + 1
       && (TARGET_H8300SX || (REGNO (operands[0]) & 1) == 0))"
  [(parallel [(set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -4)))
		   (match_dup 0))
	      (set (mem:SI (plus:HI (reg:HI SP_REG) (const_int -8)))
		   (match_dup 1))
	      (set (reg:HI SP_REG)
		   (plus:HI (reg:HI SP_REG)
			    (const_int -8)))])]
  "")

;; Turn
;;
;;   mov.w #2,r0
;;   add.w r7,r0  (6 bytes)
;;
;; into
;;
;;   mov.w r7,r0
;;   adds  #2,r0  (4 bytes)

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "const_int_operand" ""))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (match_operand:HI 2 "register_operand" "")))]
  "REG_P (operands[0]) && REG_P (operands[2])
   && REGNO (operands[0]) != REGNO (operands[2])
   && (satisfies_constraint_J (operands[1])
       || satisfies_constraint_L (operands[1])
       || satisfies_constraint_N (operands[1]))"
  [(set (match_dup 0)
	(match_dup 2))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (match_dup 1)))]
  "")

;; Turn
;;
;;   sub.l  er0,er0
;;   add.b  #4,r0l
;;   add.l  er7,er0  (6 bytes)
;;
;; into
;;
;;   mov.l  er7,er0
;;   adds   #4,er0   (4 bytes)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "register_operand" "")))]
  "REG_P (operands[0]) && REG_P (operands[2])
   && REGNO (operands[0]) != REGNO (operands[2])
   && (satisfies_constraint_L (operands[1])
       || satisfies_constraint_N (operands[1]))"
  [(set (match_dup 0)
	(match_dup 2))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_dup 1)))]
  "")

;; Turn
;;
;;   mov.l er7,er0
;;   add.l #10,er0  (takes 8 bytes)
;;
;; into
;;
;;   sub.l er0,er0
;;   add.b #10,r0l
;;   add.l er7,er0  (takes 6 bytes)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "const_int_operand" "")))]
  "operands[0] != stack_pointer_rtx
   && REG_P (operands[0]) && REG_P (operands[1])
   && REGNO (operands[0]) != REGNO (operands[1])
   && !satisfies_constraint_L (operands[2])
   && !satisfies_constraint_N (operands[2])
   && ((INTVAL (operands[2]) & 0xff) == INTVAL (operands[2])
       || (INTVAL (operands[2]) & 0xff00) == INTVAL (operands[2])
       || INTVAL (operands[2]) == 0xffff
       || INTVAL (operands[2]) == 0xfffe)"
  [(set (match_dup 0)
	(match_dup 2))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_dup 1)))]
  "")

;; Turn
;;
;;   subs   #1,er4
;;   mov.w  r4,r4
;;   bne    .L2028
;;
;; into
;;
;;   dec.w  #1,r4
;;   bne    .L2028

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(plus:HI (match_dup 0)
		 (match_operand 1 "incdec_operand" "")))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (match_operand:HI 0 "register_operand" "")
	(unspec:HI [(match_dup 0)
		    (match_dup 1)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))])

;; The SImode version of the previous pattern.

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0)
		 (match_operand 1 "incdec_operand" "")))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec:SI [(match_dup 0)
		    (match_dup 1)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))])

(define_peephole2
  [(parallel [(set (cc0)
		   (compare (zero_extract:SI (match_operand:QI 0 "register_operand" "")
					     (const_int 1)
					     (const_int 7))
			    (const_int 0)))
	      (clobber (scratch:QI))])
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[4] = ((GET_CODE (operands[4]) == EQ)
		   ? gen_rtx_GE (VOIDmode, cc0_rtx, const0_rtx)
		   : gen_rtx_LT (VOIDmode, cc0_rtx, const0_rtx));
  })

;; The next three peephole2's will try to transform
;;
;;   mov.b A,r0l    (or mov.l A,er0)
;;   and.l #CST,er0
;;
;; into
;;
;;   sub.l er0
;;   mov.b A,r0l
;;   and.b #CST,r0l (if CST is not 255)

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "general_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(and:SI (match_dup 2)
		(const_int 255)))]
  "!reg_overlap_mentioned_p (operands[2], operands[1])
   && REGNO (operands[0]) == REGNO (operands[2])"
  [(set (match_dup 2)
	(const_int 0))
   (set (strict_low_part (match_dup 0))
	(match_dup 1))]
  "")

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "nonimmediate_operand" ""))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 255)))]
  "!reg_overlap_mentioned_p (operands[0], operands[1])
   && !(GET_CODE (operands[1]) == MEM && !offsettable_memref_p (operands[1]))
   && !(GET_CODE (operands[1]) == MEM && MEM_VOLATILE_P (operands[1]))"
  [(set (match_dup 0)
	(const_int 0))
   (set (strict_low_part (match_dup 2))
	(match_dup 3))]
  {
    operands[2] = gen_lowpart (QImode, operands[0]);
    operands[3] = gen_lowpart (QImode, operands[1]);
  })

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(and:SI (match_dup 2)
		(match_operand:SI 3 "const_int_qi_operand" "")))]
  "(GET_MODE (operands[0]) == QImode
    || GET_MODE (operands[0]) == HImode
    || GET_MODE (operands[0]) == SImode)
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && REGNO (operands[0]) == REGNO (operands[2])
   && !reg_overlap_mentioned_p (operands[2], operands[1])
   && !(GET_MODE (operands[1]) != QImode
	&& GET_CODE (operands[1]) == MEM
	&& !offsettable_memref_p (operands[1]))
   && !(GET_MODE (operands[1]) != QImode
	&& GET_CODE (operands[1]) == MEM
	&& MEM_VOLATILE_P (operands[1]))"
  [(set (match_dup 2)
	(const_int 0))
   (set (strict_low_part (match_dup 4))
	(match_dup 5))
   (set (match_dup 2)
	(and:SI (match_dup 2)
		(match_dup 6)))]
  {
    operands[4] = gen_lowpart (QImode, operands[0]);
    operands[5] = gen_lowpart (QImode, operands[1]);
    operands[6] = GEN_INT (~0xff | INTVAL (operands[3]));
  })

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(const_int 65280)))]
  "!reg_overlap_mentioned_p (operands[0], operands[1])"
  [(set (match_dup 0)
	(const_int 0))
   (set (zero_extract:SI (match_dup 0)
			 (const_int 8)
			 (const_int 8))
	(lshiftrt:SI (match_dup 1)
		     (const_int 8)))]
  "")

;; If a load of mem:SI is followed by an AND that turns off the upper
;; half, then we can load mem:HI instead.

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "memory_operand" ""))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand:SI 2 "const_int_operand" "")))]
  "!MEM_VOLATILE_P (operands[1])
   && offsettable_memref_p (operands[1])
   && (INTVAL (operands[2]) & ~0xffff) == 0
   && INTVAL (operands[2]) != 255"
  [(set (match_dup 3)
	(match_dup 4))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_dup 2)))]
  {
    operands[3] = gen_lowpart (HImode, operands[0]);
    operands[4] = gen_lowpart (HImode, operands[1]);
  })

;; Convert a memory comparison to a move if there is a scratch register.

(define_peephole2
  [(match_scratch:QHSI 1 "r")
   (set (cc0)
	(compare (match_operand:QHSI 0 "memory_operand" "")
		 (const_int 0)))]
  ""
  [(set (match_dup 1)
	(match_dup 0))
   (set (cc0) (compare (match_dup 1)
		       (const_int 0)))]
  "")

;; (compare (reg:HI) (const_int)) takes 4 bytes, so we try to achieve
;; the equivalent with shorter sequences.  Here is the summary.  Cases
;; are grouped for each define_peephole2.
;;
;; reg  const_int                   use     insn
;; --------------------------------------------------------
;; dead    -2                       eq/ne   inc.l
;; dead    -1                       eq/ne   inc.l
;; dead     1                       eq/ne   dec.l
;; dead     2                       eq/ne   dec.l
;;
;; dead     1                       ge/lt shar.l
;; dead     3 (H8S)                 ge/lt shar.l
;;
;; dead     1                       geu/ltu shar.l
;; dead     3 (H8S)                 geu/ltu shar.l
;;
;; ----   255                       ge/lt mov.b
;;
;; ----   255                       geu/ltu mov.b

;; Transform
;;
;;	cmp.w	#1,r0
;;	bne	.L1
;;
;; into
;;
;;	dec.w	#1,r0
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "")
		 (match_operand:HI 1 "incdec_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "INTVAL (operands[1]) != 0 && peep2_reg_dead_p (1, operands[0])"
  [(set (match_dup 0)
	(unspec:HI [(match_dup 0)
		    (match_dup 5)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (- INTVAL (operands[1]));
  })

;; Transform
;;
;;	cmp.w	#1,r0
;;	bgt	.L1
;;
;; into
;;
;;	shar.w	r0
;;	bgt	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "")
		 (match_operand:HI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))"
  [(parallel [(set (match_dup 0)
		   (ashiftrt:HI (match_dup 0)
				(match_dup 5)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
  })

;; Transform
;;
;;	cmp.w	#1,r0
;;	bhi	.L1
;;
;; into
;;
;;	shar.w	r0
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "")
		 (match_operand:HI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtuleu_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))"
  [(parallel [(set (match_dup 0)
		   (ashiftrt:HI (match_dup 0)
				(match_dup 5)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 6)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
    operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[4]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; Transform
;;
;;	cmp.w	#255,r0
;;	bgt	.L1
;;
;; into
;;
;;	mov.b	r0h,r0h
;;	bgt	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "")
		 (const_int 255)))
   (set (pc)
	(if_then_else (match_operator 1 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (cc0) (compare (and:HI (match_dup 0)
			       (const_int -256))
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 1)
		      (match_dup 2)
		      (match_dup 3)))])

;; Transform
;;
;;	cmp.w	#255,r0
;;	bhi	.L1
;;
;; into
;;
;;	mov.b	r0h,r0h
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "")
		 (const_int 255)))
   (set (pc)
	(if_then_else (match_operator 1 "gtuleu_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (cc0) (compare (and:HI (match_dup 0)
			       (const_int -256))
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[4] = gen_rtx_fmt_ee (GET_CODE (operands[1]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; (compare (reg:SI) (const_int)) takes 6 bytes, so we try to achieve
;; the equivalent with shorter sequences.  Here is the summary.  Cases
;; are grouped for each define_peephole2.
;;
;; reg  const_int                   use     insn
;; --------------------------------------------------------
;; live    -2                       eq/ne   copy and inc.l
;; live    -1                       eq/ne   copy and inc.l
;; live     1                       eq/ne   copy and dec.l
;; live     2                       eq/ne   copy and dec.l
;;
;; dead    -2                       eq/ne   inc.l
;; dead    -1                       eq/ne   inc.l
;; dead     1                       eq/ne   dec.l
;; dead     2                       eq/ne   dec.l
;;
;; dead -131072                     eq/ne   inc.w and test
;; dead  -65536                     eq/ne   inc.w and test
;; dead   65536                     eq/ne   dec.w and test
;; dead  131072                     eq/ne   dec.w and test
;;
;; dead 0x000000?? except 1 and 2   eq/ne   xor.b and test
;; dead 0x0000??00                  eq/ne   xor.b and test
;; dead 0x0000ffff                  eq/ne   not.w and test
;;
;; dead 0xffffff?? except -1 and -2 eq/ne   xor.b and not.l
;; dead 0xffff??ff                  eq/ne   xor.b and not.l
;; dead 0x40000000 (H8S)            eq/ne   rotl.l and dec.l
;; dead 0x80000000                  eq/ne   rotl.l and dec.l
;;
;; live     1                       ge/lt copy and shar.l
;; live     3 (H8S)                 ge/lt copy and shar.l
;;
;; live     1                       geu/ltu copy and shar.l
;; live     3 (H8S)                 geu/ltu copy and shar.l
;;
;; dead     1                       ge/lt shar.l
;; dead     3 (H8S)                 ge/lt shar.l
;;
;; dead     1                       geu/ltu shar.l
;; dead     3 (H8S)                 geu/ltu shar.l
;;
;; dead     3 (H8/300H)             ge/lt and.b and test
;; dead     7                       ge/lt and.b and test
;; dead    15                       ge/lt and.b and test
;; dead    31                       ge/lt and.b and test
;; dead    63                       ge/lt and.b and test
;; dead   127                       ge/lt and.b and test
;; dead   255                       ge/lt and.b and test
;;
;; dead     3 (H8/300H)             geu/ltu and.b and test
;; dead     7                       geu/ltu and.b and test
;; dead    15                       geu/ltu and.b and test
;; dead    31                       geu/ltu and.b and test
;; dead    63                       geu/ltu and.b and test
;; dead   127                       geu/ltu and.b and test
;; dead   255                       geu/ltu and.b and test
;;
;; ---- 65535                       ge/lt mov.w
;;
;; ---- 65535                       geu/ltu mov.w

;; Transform
;;
;;	cmp.l	#1,er0
;;	beq	.L1
;;
;; into
;;
;;	dec.l	#1,er0
;;	beq	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "incdec_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "INTVAL (operands[1]) != 0 && peep2_reg_dead_p (1, operands[0])"
  [(set (match_dup 0)
	(unspec:SI [(match_dup 0)
		    (match_dup 5)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (- INTVAL (operands[1]));
  })

;; Transform
;;
;;	cmp.l	#65536,er0
;;	beq	.L1
;;
;; into
;;
;;	dec.l	#1,e0
;;	beq	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == -131072
       || INTVAL (operands[1]) == -65536
       || INTVAL (operands[1]) == 65536
       || INTVAL (operands[1]) == 131072)"
  [(set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_dup 5)))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (- INTVAL (operands[1]));
  })

;; Transform
;;
;;	cmp.l	#100,er0
;;	beq	.L1
;;
;; into
;;
;;	xor.b	#100,er0
;;	mov.l	er0,er0
;;	beq	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && ((INTVAL (operands[1]) & 0x00ff) == INTVAL (operands[1])
       || (INTVAL (operands[1]) & 0xff00) == INTVAL (operands[1])
       || INTVAL (operands[1]) == 0x0000ffff)
   && INTVAL (operands[1]) != 0
   && INTVAL (operands[1]) != 1
   && INTVAL (operands[1]) != 2"
  [(set (match_dup 0)
	(xor:SI (match_dup 0)
		(match_dup 1)))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))])

;; Transform
;;
;;	cmp.l	#-100,er0
;;	beq	.L1
;;
;; into
;;
;;	xor.b	#99,er0
;;	not.l	er0
;;	beq	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && ((INTVAL (operands[1]) | 0x00ff) == -1
       || (INTVAL (operands[1]) | 0xff00) == -1)
   && INTVAL (operands[1]) != -1
   && INTVAL (operands[1]) != -2"
  [(set (match_dup 0)
	(xor:SI (match_dup 0)
		(match_dup 5)))
   (set (match_dup 0)
	(not:SI (match_dup 0)))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (INTVAL (operands[1]) ^ -1);
  })

;; Transform
;;
;;	cmp.l	#-2147483648,er0
;;	beq	.L1
;;
;; into
;;
;;	rotl.l	er0
;;	dec.l	#1,er0
;;	beq	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == -2147483647 - 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 1073741824))"
  [(set (match_dup 0)
	(rotate:SI (match_dup 0)
		   (match_dup 5)))
   (set (match_dup 0)
	(unspec:SI [(match_dup 0)
		    (const_int -1)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (INTVAL (operands[1]) == -2147483647 - 1 ? 1 : 2);
  })

;; Transform
;;
;;	cmp.l	#1,er0
;;	bgt	.L1
;;
;; into
;;
;;	mov.l	er0,er1
;;	shar.l	er1
;;	bgt	.L1

;; We avoid this transformation if we see more than one copy of the
;; same compare insn immediately before this one.

(define_peephole2
  [(match_scratch:SI 5 "r")
   (set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "!peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))
   && !same_cmp_preceding_p (insn)"
  [(set (match_dup 5)
	(match_dup 0))
   (parallel [(set (match_dup 5)
		   (ashiftrt:SI (match_dup 5)
				(match_dup 6)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 5)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[6] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
  })

;; Transform
;;
;;	cmp.l	#1,er0
;;	bhi	.L1
;;
;; into
;;
;;	mov.l	er0,er1
;;	shar.l	er1
;;	bne	.L1

;; We avoid this transformation if we see more than one copy of the
;; same compare insn immediately before this one.

(define_peephole2
  [(match_scratch:SI 5 "r")
   (set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtuleu_operator"
		         [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "!peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))
   && !same_cmp_preceding_p (insn)"
  [(set (match_dup 5)
	(match_dup 0))
   (parallel [(set (match_dup 5)
		   (ashiftrt:SI (match_dup 5)
				(match_dup 6)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 5)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 7)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[6] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
    operands[7] = gen_rtx_fmt_ee (GET_CODE (operands[4]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; Transform
;;
;;	cmp.l	#1,er0
;;	bgt	.L1
;;
;; into
;;
;;	shar.l	er0
;;	bgt	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))"
  [(parallel [(set (match_dup 0)
		   (ashiftrt:SI (match_dup 0)
				(match_dup 5)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
  })

;; Transform
;;
;;	cmp.l	#1,er0
;;	bhi	.L1
;;
;; into
;;
;;	shar.l	er0
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtuleu_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 1
       || (TARGET_H8300S && INTVAL (operands[1]) == 3))"
  [(parallel [(set (match_dup 0)
		   (ashiftrt:SI (match_dup 0)
				(match_dup 5)))
	      (clobber (scratch:QI))])
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 6)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (exact_log2 (INTVAL (operands[1]) + 1));
    operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[4]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; Transform
;;
;;	cmp.l	#15,er0
;;	bgt	.L1
;;
;; into
;;
;;	and	#240,r0l
;;	mov.l	er0,er0
;;	bgt	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && (INTVAL (operands[1]) == 3
       || INTVAL (operands[1]) == 7
       || INTVAL (operands[1]) == 15
       || INTVAL (operands[1]) == 31
       || INTVAL (operands[1]) == 63
       || INTVAL (operands[1]) == 127
       || INTVAL (operands[1]) == 255)"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(match_dup 5)))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (~INTVAL (operands[1]));
  })

;; Transform
;;
;;	cmp.l	#15,er0
;;	bhi	.L1
;;
;; into
;;
;;	and	#240,r0l
;;	mov.l	er0,er0
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "gtuleu_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "peep2_reg_dead_p (1, operands[0])
   && ((TARGET_H8300H && INTVAL (operands[1]) == 3)
	|| INTVAL (operands[1]) == 7
	|| INTVAL (operands[1]) == 15
	|| INTVAL (operands[1]) == 31
	|| INTVAL (operands[1]) == 63
	|| INTVAL (operands[1]) == 127
	|| INTVAL (operands[1]) == 255)"
  [(set (match_dup 0)
	(and:SI (match_dup 0)
		(match_dup 5)))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 6)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[5] = GEN_INT (~INTVAL (operands[1]));
    operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[4]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; Transform
;;
;;	cmp.l	#65535,er0
;;	bgt	.L1
;;
;; into
;;
;;	mov.l	e0,e0
;;	bgt	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (const_int 65535)))
   (set (pc)
	(if_then_else (match_operator 1 "gtle_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (cc0) (compare (and:SI (match_dup 0)
			       (const_int -65536))
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 1)
		      (match_dup 2)
		      (match_dup 3)))])

;; Transform
;;
;;	cmp.l	#65535,er0
;;	bhi	.L1
;;
;; into
;;
;;	mov.l	e0,e0
;;	bne	.L1

(define_peephole2
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (const_int 65535)))
   (set (pc)
	(if_then_else (match_operator 1 "gtuleu_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  ""
  [(set (cc0) (compare (and:SI (match_dup 0)
			       (const_int -65536))
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 4)
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[4] = gen_rtx_fmt_ee (GET_CODE (operands[1]) == GTU ? NE : EQ,
				  VOIDmode, cc0_rtx, const0_rtx);
  })

;; Transform
;;
;;	cmp.l	#1,er0
;;	beq	.L1
;;
;; into
;;
;;	mov.l	er0,er1
;;	dec.l	#1,er1
;;	beq	.L1

;; We avoid this transformation if we see more than one copy of the
;; same compare insn.

(define_peephole2
  [(match_scratch:SI 5 "r")
   (set (cc0)
	(compare (match_operand:SI 0 "register_operand" "")
		 (match_operand:SI 1 "incdec_operand" "")))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "INTVAL (operands[1]) != 0
   && !peep2_reg_dead_p (1, operands[0])
   && !same_cmp_following_p (insn)"
  [(set (match_dup 5)
	(match_dup 0))
   (set (match_dup 5)
	(unspec:SI [(match_dup 5)
		    (match_dup 6)]
		   UNSPEC_INCDEC))
   (set (cc0) (compare (match_dup 5)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    operands[6] = GEN_INT (- INTVAL (operands[1]));
  })

;; Narrow the mode of testing if possible.

(define_peephole2
  [(set (match_operand:HSI 0 "register_operand" "")
	(and:HSI (match_dup 0)
		 (match_operand:HSI 1 "const_int_operand" "")))
   (set (cc0) (compare (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_operator 4 "eqne_operator"
		       [(cc0) (const_int 0)])
		      (match_operand 2 "pc_or_label_operand" "")
		      (match_operand 3 "pc_or_label_operand" "")))]
  "((const_int_qi_operand (operands[1], QImode)
     || (GET_MODE (operands[0]) == SImode
	 && const_int_hi_operand (operands[1], HImode)))
    && peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 5) (match_dup 7))
   (set (cc0) (compare (match_dup 5)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 4 [(cc0) (const_int 0)])
		      (match_dup 2)
		      (match_dup 3)))]
  {
    enum machine_mode mode;

    mode = const_int_qi_operand (operands[1], QImode) ? QImode : HImode;
    operands[5] = gen_rtx_REG (mode, REGNO (operands[0]));
    operands[6] = gen_int_mode (INTVAL (operands[1]), mode);
    operands[7] = gen_rtx_AND (mode, operands[5], operands[6]);
  })

;; These triggers right at the end of allocation of locals in the
;; prologue (and possibly at other places).

;; stack adjustment of -4, generate one push
;;
;; before : 6 bytes
;; after  : 4 bytes

(define_peephole2
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -4)))
   (set (mem:SFI (reg:SI SP_REG))
	(match_operand:SFI 0 "register_operand" ""))]
  "!TARGET_NORMAL_MODE && REGNO (operands[0]) != SP_REG"
  [(set (mem:SFI (pre_dec:SI (reg:SI SP_REG)))
	(match_dup 0))])

;; stack adjustment of -8, generate one push
;;
;; before : 8 bytes
;; after  : 6 bytes

(define_peephole2
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -8)))
   (set (mem:SFI (reg:SI SP_REG))
	(match_operand:SFI 0 "register_operand" ""))]
  "!TARGET_NORMAL_MODE && REGNO (operands[0]) != SP_REG"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -4)))
   (set (mem:SFI (pre_dec:SI (reg:SI SP_REG)))
	(match_dup 0))])

;; stack adjustment of -12, generate one push
;;
;; before : 10 bytes
;; after  :  8 bytes

(define_peephole2
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -12)))
   (set (mem:SFI (reg:SI SP_REG))
	(match_operand:SFI 0 "register_operand" ""))]
  "!TARGET_NORMAL_MODE && REGNO (operands[0]) != SP_REG"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -4)))
   (set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int -4)))
   (set (mem:SFI (pre_dec:SI (reg:SI SP_REG)))
	(match_dup 0))])

;; Transform
;;
;;	mov	dst,reg
;;	op	reg
;;	mov	reg,dst
;;
;; into
;;
;;	op	dst
;;
;; if "reg" dies at the end of the sequence.

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "memory_operand" ""))
   (set (match_dup 0)
	(match_operator 2 "h8sx_unary_memory_operator"
	 [(match_dup 0)]))
   (set (match_operand 3 "memory_operand" "")
	(match_dup 0))]
  "TARGET_H8300SX
   && peep2_reg_dead_p (3, operands[0])
   && !reg_overlap_mentioned_p (operands[0], operands[3])
   && h8sx_mergeable_memrefs_p (operands[3], operands[1])"
  [(set (match_dup 3)
	(match_dup 4))]
  {
    operands[4] = shallow_copy_rtx (operands[2]);
    XEXP (operands[4], 0) = operands[1];
  })

;; Transform
;;
;;	mov	src1,reg
;;	cmp	reg,src2
;;
;; into
;;
;;	cmp	src1,src2
;;
;; if "reg" dies in the comparison.

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "h8300_dst_operand" ""))
   (set (cc0)
	(compare (match_dup 0)
		 (match_operand 2 "h8300_src_operand" "")))]
  "TARGET_H8300SX
   && peep2_reg_dead_p (2, operands[0])
   && !reg_overlap_mentioned_p (operands[0], operands[2])
   && operands[2] != const0_rtx"
  [(set (cc0)
	(compare (match_dup 1)
		 (match_dup 2)))])

;; Likewise for the second operand.

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "h8300_src_operand" ""))
   (set (cc0)
	(compare (match_operand 2 "h8300_dst_operand" "")
		 (match_dup 0)))]
  "TARGET_H8300SX
   && peep2_reg_dead_p (2, operands[0])
   && !reg_overlap_mentioned_p (operands[0], operands[2])"
  [(set (cc0)
	(compare (match_dup 2)
		 (match_dup 1)))])

;; Combine two moves.

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "h8300_src_operand" ""))
   (set (match_operand 2 "h8300_dst_operand" "")
	(match_dup 0))]
  "TARGET_H8300SX
   && peep2_reg_dead_p (2, operands[0])
   && !reg_overlap_mentioned_p (operands[0], operands[2])"
  [(set (match_dup 2)
	(match_dup 1))])


