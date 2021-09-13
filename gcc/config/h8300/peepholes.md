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


