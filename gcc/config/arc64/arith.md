;; SI instructions having short instruction variant
(define_insn "*<optab><mode>_insn"
  [(set (                match_operand:GPI 0 "register_operand"   "=q,q,     r,    r,     r,    r,    r,    r,r")
	(COMMUTATIVE:GPI (match_operand:GPI 1 "nonmemory_operand" "%0,q,     0,    0,     r,U06S0,S12S0,S32S0,r")
			 (match_operand:GPI 2 "nonmemory_operand" " q,0,rU06S0,S12S0,rU06S0,    r,    0,    r,S32S0")))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "@
   <mntab><sfxtab>%?\\t%0,%1,%2
   <mntab><sfxtab>%?\\t%0,%2,%1
   <mntab><sfxtab>%?\\t%0,%1,%2
   <mntab><sfxtab>%?\\t%0,%1,%2
   <mntab><sfxtab>%?\\t%0,%1,%2
   <mntab><sfxtab>%?\\t%0,%2,%1
   <mntab><sfxtab>%?\\t%0,%2,%1
   <mntab><sfxtab>%?\\t%0,%1,%2
   <mntab><sfxtab>%?\\t%0,%1,%2"
  [(set_attr "iscompact" "maybe,maybe,no,no,no,no,no,no,no")
   (set_attr "length"     "*,*,4,4,4,4,4,8,8")
   (set_attr "type"       "<mntab>")]
  )

;; The zero extend variant of the above
(define_insn "*<optab>si3_zextend"
  [(set (match_operand:DI 0 "register_operand"   "=q,q,     r,    r,     r,    r,    r,    r,r")
	(zero_extend:DI
	 (COMMUTATIVE:SI
	  (match_operand:SI 1 "nonmemory_operand" "%0,q,     0,    0,     r,U06S0,S12S0,S32S0,r")
	  (match_operand:SI 2 "nonmemory_operand" " q,0,rU06S0,S12S0,rU06S0,    r,    0,    r,S32S0"))))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   <mntab>%?\\t%0,%1,%2
   <mntab>%?\\t%0,%2,%1
   <mntab>%?\\t%0,%1,%2
   <mntab>%?\\t%0,%1,%2
   <mntab>%?\\t%0,%1,%2
   <mntab>%?\\t%0,%2,%1
   <mntab>%?\\t%0,%2,%1
   <mntab>%?\\t%0,%1,%2
   <mntab>%?\\t%0,%1,%2"
  [(set_attr "iscompact" "maybe,maybe,no,no,no,no,no,no,no")
   (set_attr "length"     "*,*,4,4,4,4,4,8,8")
   (set_attr "type"       "<mntab>")]
  )

(define_insn "*<optab><mode>_insn"
  [(set (            match_operand:GPI 0 "register_operand"  "=q,     r,    r,    r,    r,r")
	(ASHIFT:GPI (match_operand:GPI 1 "nonmemory_operand" " 0,     0,    0,    r,S32S0,r")
		    (match_operand:GPI 2 "nonmemory_operand" " q,rU06S0,S12S0,rU06S0,   r,S32S0")))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "<mntab><sfxtab>%?\\t%0,%1,%2"
  [(set_attr "iscompact" "maybe,no,no,no,no,no")
   (set_attr "length"     "*,4,4,4,8,8")
   (set_attr "type"       "<mntab>")])

(define_insn "*<optab>zsidi_insn"
  [(set (match_operand:DI 0 "register_operand"    "=q,     r,    r,    r,    r,r")
	(zero_extend:DI
	 (ASHIFT:SI
	  (match_operand:SI 1 "nonmemory_operand" " 0,     0,    0,    r,S32S0,r")
	  (match_operand:SI 2 "nonmemory_operand" " q,rU06S0,S12S0,rU06S0,   r,S32S0"))))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "<mntab>%?\\t%0,%1,%2"
  [(set_attr "iscompact" "yes,no,no,no,no,no")
   (set_attr "length"     "*,4,4,4,8,8")
   (set_attr "type"       "<mntab>")])

(define_insn "*<optab><mode>_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (ASHIFT:GPI
	  (match_operand:GPI 1 "register_operand"  "     0,0")
	  (match_operand:GPI 2 "nonmemory_operand" "rU06S0,S32S0"))
	 (const_int 0)))
    (set (match_operand:GPI 0 "register_operand"   "=     r,r")
	 (ASHIFT:GPI (match_dup 1) (match_dup 2)))]
  ""
  "<mntab><sfxtab>.f\\t%0,%1,%2"
  [(set_attr "iscompact" "no")
   (set_attr "length"     "4,8")
   (set_attr "type"       "<mntab>")])

(define_insn "*<optab><mode>_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (ASHIFT:GPI
	  (match_operand:GPI 0 "register_operand"  "     r,r")
	  (match_operand:GPI 1 "nonmemory_operand" "rU06S0,S32S0"))
	 (const_int 0)))]
  ""
  "<mntab><sfxtab>.f\\t0,%0,%1"
  [(set_attr "iscompact" "no")
   (set_attr "length"     "4,8")
   (set_attr "type"       "<mntab>")])


(define_insn "*sub<mode>_insn"
  [(set (           match_operand:GPI 0 "register_operand"  "=q,    q,     r,     r,    r,     r,    r,    r,    r,r")
	(minus:GPI (match_operand:GPI 1 "nonmemory_operand" " 0,    0,     0,rU06S0,    0,     r,U06S0,S12S0,S32S0,r")
		   (match_operand:GPI 2 "nonmemory_operand" " q,U05S0,rU06Sx,     0,S12S0,rU06Sx,    r,    0,    r,S32S0")))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "@
   sub<sfxtab>%?\\t%0,%1,%2
   sub<sfxtab>%?\\t%0,%1,%2
   sub%s2<sfxtab>%?\\t%0,%1,%S2
   rsub<sfxtab>%?\\t%0,%2,%1
   sub<sfxtab>%?\\t%0,%1,%2
   sub%s2<sfxtab>%?\\t%0,%1,%S2
   rsub<sfxtab>%?\\t%0,%2,%1
   rsub<sfxtab>%?\\t%0,%2,%1
   sub<sfxtab>%?\\t%0,%1,%2
   sub<sfxtab>%?\\t%0,%1,%2"
  [(set_attr "iscompact"  "yes,maybe,no,no,no,no,no,no,no,no")
   (set_attr "length"     "2,*,4,4,4,4,4,4,8,8")
   (set_attr "type"       "sub")]
  )

;; zero extend of the above
(define_insn "*subsi3r_zextend"
  [(set (match_operand:DI 0 "register_operand"   "=q,r")
	(zero_extend:DI
	 (minus:SI
	  (match_operand:SI 1 "register_operand" " 0,r")
	  (match_operand:SI 2 "register_operand" " q,r"))))]
  ""
  "sub%?\\t%0,%1,%2"
  [(set_attr "iscompact"  "yes,no")
   (set_attr "length"     "2,4")
   (set_attr "type"       "sub")]
  )

(define_insn "*add<mode>_insn"
  [(set (          match_operand:GPI 0 "register_operand"  "=q, q,q,     r,    r,    r,     r,    r,r")
	(plus:GPI (match_operand:GPI 1 "register_operand"  "%0, 0,q,     0,    0,    0,     r,    r,r")
		  (match_operand:GPI 2 "nonmemory_operand" " q,qh,q,rU06Sx,N06Sx,S12Sx,rU06Sx,N06Sx,S32S0")))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "@
   add<sfxtab>%?\\t%0,%1,%2
   add<sfxtab>%?\\t%0,%1,%2
   add<sfxtab>%?\\t%0,%1,%2
   add%s2<sfxtab>%?\\t%0,%1,%S2
   sub%s2<sfxtab>%?\\t%0,%1,%N2
   add%s2<sfxtab>%?\\t%0,%1,%S2
   add%s2<sfxtab>%?\\t%0,%1,%S2
   sub%s2<sfxtab>%?\\t%0,%1,%N2
   add<sfxtab>%?\\t%0,%1,%2"
  [(set_attr "iscompact"  "yes,maybe,maybe,no,no,no,no,no,no")
   (set_attr "length"     "2,*,*,4,4,4,4,4,8")
   (set_attr "type"       "add")]
  )

;; zero extend of the above
(define_insn "*addsi3_zextend"
  [(set (match_operand:DI 0 "register_operand"            "=q, q,q,    r,    r,    r,     r,    r,r")
	(zero_extend:DI
	 (plus:SI (match_operand:SI 1 "register_operand"  "%0, 0,q,     0,    0,    0,     r,    r,r")
		  (match_operand:SI 2 "nonmemory_operand" " q,qh,q,rU06Sx,N06Sx,S12Sx,rU06Sx,N06Sx,S32S0"))))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   add%?\\t%0,%1,%2
   add%?\\t%0,%1,%2
   add%?\\t%0,%1,%2
   add%s2%?\\t%0,%1,%S2
   sub%s2%?\\t%0,%1,%N2
   add%s2%?\\t%0,%1,%S2
   add%s2%?\\t%0,%1,%S2
   sub%s2%?\\t%0,%1,%N2
   add%?\\t%0,%1,%2"
  [(set_attr "iscompact"  "yes,maybe,maybe,no,no,no,no,no,no")
   (set_attr "length"     "2,*,*,4,4,4,4,4,8")
   (set_attr "type"       "add")])

;; This pattern is needed because the GT (pnz) is not reversible and I
;; cannot convert CCmode to CC_ZNmode.
(define_insn "*<ADDSUB:optab><GPI:mode>3_f"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (ADDSUB:GPI
	  (match_operand:GPI 1 "arc64_nonmem_operand" "0,    0,    0,    r,r,S32S0,    r")
	  (match_operand:GPI 2 "arc64_nonmem_operand" "r,U06S0,S12S0,U06S0,r,    r,S32S0"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand"      "=r,    r,    r,    r,r,    r,    r")
	(ADDSUB:GPI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "<ADDSUB:optab><GPI:sfxtab>.f\\t%0,%1,%2"
  [(set_attr "predicable" "yes,yes,no,no,no,no,no")
   (set_attr "length"     "4,4,4,4,4,8,8")
   (set_attr "type"       "<ADDSUB:optab><GPI:sfxtab>")])

;; Arithmetic patterns used by the combiner.
(define_insn "*bic<mode>3"
  [(set (                  match_operand:GPI 0 "register_operand"  "=q,r,r,    r")
	(and:GPI (not:GPI (match_operand:GPI 1 "register_operand"   "q,r,r,    r"))
		 (         match_operand:GPI 2 "nonmemory_operand"  "0,0,r,S32S0")))]
  ""
  "bic<sfxtab>%?\\t%0,%2,%1"
  [(set_attr "iscompact" "maybe,no,no,no")
   (set_attr "predicable" "no,yes,no,no")
   (set_attr "length"     "*,4,4,8")
   (set_attr "type"       "bic")])

(define_insn "*bic<mode>3_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (and:GPI
	  (not:GPI (match_operand:GPI 1 "register_operand"   "r,r,    r"))
	  (match_operand:GPI 2 "nonmemory_operand"  "0,r,S32S0"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand"  "=r,r,r")
	(and:GPI (not:GPI (match_dup 1)) (match_dup 2)))]
  ""
  "bic<sfxtab>%?.f\\t%0,%2,%1"
  [(set_attr "iscompact" "no,no,no")
   (set_attr "predicable" "yes,no,no")
   (set_attr "length"     "4,4,8")
   (set_attr "type"       "bic")])

(define_insn "*bic<mode>3_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (and:GPI
	  (not:GPI (match_operand:GPI 0 "register_operand"   "r,r"))
	  (match_operand:GPI 1 "nonmemory_operand"  "r,S32S0"))
	 (const_int 0)))]
  ""
  "bic<sfxtab>.f\\t0,%1,%0"
  [(set_attr "iscompact"  "no,no")
   (set_attr "predicable" "no,no")
   (set_attr "length"     "4,8")
   (set_attr "type"       "bic")])

(define_insn "*<bit_optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(BIT:GPI
	 (ashift:GPI
	  (const_int 1)
	  (match_operand:GPI 1 "register_operand" "r,r,r"))
	 (match_operand:GPI 2 "nonmemory_operand" "0,r,S32S0")))]
  ""
  "<bit_optab><sfxtab>%?\\t%0,%2,%1"
  [(set_attr "type" "<bit_optab>")
   (set_attr "iscompact" "no")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*bset<mode>3_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (BIT:GPI
	  (ashift:GPI
	   (const_int 1)
	   (match_operand:GPI 1 "register_operand" "r,r,r"))
	  (match_operand:GPI 2 "nonmemory_operand" "0,r,S32S0"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(BIT:GPI
	 (ashift:GPI
	  (const_int 1)
	  (match_dup 1))
	 (match_dup 2)))]
  ""
  "<bit_optab><sfxtab>%?.f\\t%0,%2,%1"
  [(set_attr "type" "<bit_optab>")
   (set_attr "iscompact" "no")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*bset<mode>3_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (BIT:GPI
	  (ashift:GPI
	   (const_int 1)
	   (match_operand:GPI 0 "register_operand" "r,r"))
	  (match_operand:GPI 1 "nonmemory_operand" "r,S32S0"))
	 (const_int 0)))]
  ""
  "<bit_optab><sfxtab>.f\\t0,%1,%0"
  [(set_attr "type" "<bit_optab>")
   (set_attr "iscompact" "no")
   (set_attr "length" "4,8")
   (set_attr "predicable" "no,no")])

(define_insn "<optab><mode>_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (COMMUTATIVEF:GPI
	  (match_operand:GPI 1 "nonmemory_operand" "%     0,    0,     r,U06S0,S12S0,S32S0,r")
	  (match_operand:GPI 2 "nonmemory_operand" " rU06S0,S12S0,rU06S0,    r,    0,    r,S32S0"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand"  "=     r,    r,     r,    r,    r,    r,r")
	(COMMUTATIVEF:GPI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "@
   <mntab><sfxtab>%?.f\\t%0,%1,%2
   <mntab><sfxtab>%?.f\\t%0,%1,%2
   <mntab><sfxtab>%?.f\\t%0,%1,%2
   <mntab><sfxtab>%?.f\\t%0,%2,%1
   <mntab><sfxtab>%?.f\\t%0,%2,%1
   <mntab><sfxtab>%?.f\\t%0,%1,%2
   <mntab><sfxtab>%?.f\\t%0,%1,%2"
  [(set_attr "iscompact"  "no,no,no,no,no,no,no")
   (set_attr "predicable" "yes,no,no,no,no,no,no")
   (set_attr "length"     "4,4,4,4,4,8,8")
   (set_attr "type"       "<mntab>")]
  )

;; It may be worth to have a separate pattern for AND to take
;; advantage of TST_S instruction.
(define_insn "*<optab><mode>_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (COMMUTATIVEF:GPI
	  (match_operand:GPI 0 "nonmemory_operand" "%     r,U06S0,S32S0,r")
	  (match_operand:GPI 1 "nonmemory_operand" " rU06S0,    r,    r,S32S0"))
	 (const_int 0)))]
  "register_operand (operands[0], <MODE>mode)
   || register_operand (operands[1], <MODE>mode)"
  "@
   <mntab><sfxtab>.f\\t0,%0,%1
   <mntab><sfxtab>.f\\t0,%1,%0
   <mntab><sfxtab>.f\\t0,%0,%1
   <mntab><sfxtab>.f\\t0,%0,%1"
  [(set_attr "iscompact"  "no")
   (set_attr "predicable" "no")
   (set_attr "length"     "4,4,8,8")
   (set_attr "type"       "<mntab>")]
  )

(define_insn "*sub<mode>_insn_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (minus:GPI (match_operand:GPI 1 "nonmemory_operand" "    0,     r,U06S0,S12S0,S32S0,r")
		    (match_operand:GPI 2 "nonmemory_operand" "S12S0,rU06Sx,    r,    0,    r,S32S0"))
	 (const_int 0)))
   (set (           match_operand:GPI 0 "register_operand"  "=    r,     r,    r,    r,    r,r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "@
   sub<sfxtab>.f\\t%0,%1,%2
   sub%s2<sfxtab>.f\\t%0,%1,%S2
   rsub<sfxtab>.f\\t%0,%2,%1
   rsub<sfxtab>.f\\t%0,%2,%1
   sub<sfxtab>.f\\t%0,%1,%2
   sub<sfxtab>.f\\t%0,%1,%2"
  [(set_attr "iscompact"  "no")
   (set_attr "length"     "4,4,4,4,8,8")
   (set_attr "type"       "sub")]
  )

(define_insn "*sub<mode>_insn_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (minus:GPI (match_operand:GPI 0 "nonmemory_operand" "     r,U06S0,S32S0,r")
		    (match_operand:GPI 1 "nonmemory_operand" "rU06Sx,    r,    r,S32S0"))
	 (const_int 0)))]
  "register_operand (operands[0], <MODE>mode)
   || register_operand (operands[1], <MODE>mode)"
  "@
   sub%s1<sfxtab>.f\\t0,%0,%S1
   rsub<sfxtab>.f\\t0,%1,%0
   sub<sfxtab>.f\\t0,%0,%1
   sub<sfxtab>.f\\t0,%0,%1"
  [(set_attr "iscompact"  "no")
   (set_attr "length"     "4,4,8,8")
   (set_attr "type"       "sub")]
  )

(define_insn "*<ANY_EXTEND:optab><SHORT:mode>si2_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (ANY_EXTEND:SI (match_operand:SHORT 0 "register_operand" "r"))
	 (const_int 0)))]
  ""
  "<ANY_EXTEND:mntab><SHORT:exttab>.f\\t0,%0"
  [(set_attr "type" "<ANY_EXTEND:mntab>")
   (set_attr "length" "4")])

(define_insn "*extend<EXT:mode>di2_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (sign_extend:DI (match_operand:EXT 0 "register_operand" "r"))
	 (const_int 0)))]
  ""
  "sex<EXT:exttab>l.f\\t0,%0"
  [(set_attr "type" "sex")
   (set_attr "length" "4")])

(define_insn "*<ANY_EXTEND:optab><SHORT:mode>si_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (ANY_EXTEND:SI (match_operand:SHORT 1 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(ANY_EXTEND:SI (match_dup 1)))]
  ""
  "<ANY_EXTEND:mntab><SHORT:exttab>.f\\t%0,%1"
  [(set_attr "type" "<ANY_EXTEND:mntab>")
   (set_attr "length" "4")])

(define_insn "*extend<EXT:mode>di_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (sign_extend:DI (match_operand:EXT 1 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_dup 1)))]
  ""
  "sex<EXT:exttab>l.f\\t%0,%1"
  [(set_attr "type" "sex")
   (set_attr "length" "4")])

(define_insn "*btst<mode>"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN (zero_extract:GPI
			(match_operand:GPI 0 "register_operand"   "q,r")
			(const_int 1)
			(match_operand 1 "const_int_operand" "U05S0,S12S0"))
		       (const_int 0)))]
  ""
  "btst<sfxtab>%?\\t%0,%1"
  [(set_attr "type" "btst")
   (set_attr "length" "*,4")
   (set_attr "iscompact" "maybe,no")
   (set_attr "cost" "2,4")])

;; SI/DI DIV/REM instructions.
(define_expand "<optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(DIVREM:GPI (match_operand:GPI 1 "nonmemory_operand")
		    (match_operand:GPI 2 "nonmemory_operand")))]
  "TARGET_ARC64_DIVREM"
  {
   if (<MODE>mode == DImode
       && !register_operand (operands[2], DImode))
     operands[2] = force_reg (DImode, operands[2]);
   if (<MODE>mode == DImode
       && !register_operand (operands[1], DImode))
     operands[1] = force_reg (DImode, operands[1]);
  }
  )

(define_insn "*<optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand"                 "=r,    r,    r,    r,r,    r,    r")
	(DIVREM:GPI (match_operand:GPI 1 "arc64_nonmem_operand" " 0,    0,    0,    r,r,S32S0,    r")
		    (match_operand:GPI 2 "arc64_nonmem_operand" " r,U06S0,S12S0,U06S0,r,    r,S32S0")))]
  "TARGET_ARC64_DIVREM
   && (register_operand (operands[1], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "<mntab><sfxtab>%?\\t%0,%1,%2"
  [(set_attr "length"     "4,4,4,4,4,8,8")
   (set_attr "type"       "<optab><sfxtab>")])

(define_insn "*<optab><mode>3_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (DIVREM:GPI
	  (match_operand:GPI 1 "arc64_nonmem_operand" " 0,r,S32S0,    r")
	  (match_operand:GPI 2 "arc64_nonmem_operand" " r,r,    r,S32S0"))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand"       "=r,r,    r,    r")
	(DIVREM:GPI (match_dup 1)
		    (match_dup 2)))]
  "TARGET_ARC64_DIVREM
   && (register_operand (operands[1], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "<mntab><sfxtab>.f\\t%0,%1,%2"
  [(set_attr "length"     "4,4,8,8")
   (set_attr "type"       "<optab><sfxtab>")])

(define_insn "*<optab><mode>3_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (DIVREM:GPI
	  (match_operand:GPI 0 "arc64_nonmem_operand" "r,S32S0,    r")
	  (match_operand:GPI 1 "arc64_nonmem_operand" "r,    r,S32S0"))
	(const_int 0)))]
  "TARGET_ARC64_DIVREM
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "<mntab><sfxtab>.f\\t0,%0,%1"
  [(set_attr "length"     "4,8,8")
   (set_attr "type"       "<optab><sfxtab>")])

;; To be merged into adddi3
(define_insn "*add_tls_off<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(plus:P (match_operand:P 1 "register_operand" "r")
		(unspec:P [(match_operand 2 "" "")]
			    ARC64_UNSPEC_TLS_OFF)))]
  ""
  "add<sfxtab>\\t%0,%1,%2@tpoff"
  [(set_attr "type" "add<sfxtab>")
   (set_attr "length" "8")]
  )

(define_insn "sub<mode>3_cmp"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:GPI 1 "arc64_nonmem_operand" "    0,    r,r,S32S0,    r")
	 (match_operand:GPI 2 "arc64_nonmem_operand" "S12S0,U06S0,r,    r,S32S0")))
   (set (match_operand:GPI 0 "register_operand"         "=r,    r,r,    r,    r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "sub<sfxtab>.f\\t%0,%1,%2"
  [(set_attr "length"     "4,4,4,8,8")
   (set_attr "type"       "sub<sfxtab>")]
  )

(define_insn "add<mode>3_cmp"
  [(set (match_operand 3 "cc_register" "")
	(match_operator 4 "cc_compare_operator"
			[(plus:GPI
			  (match_operand:GPI 1 "arc64_nonmem_operand" "    0,    r,r,S32S0,    r")
			  (match_operand:GPI 2 "arc64_nonmem_operand" "S12S0,U06S0,r,    r,S32S0"))
			 (match_dup 1)]))
   (set (match_operand:GPI 0 "register_operand"          "=r,    r,r,    r,    r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], <MODE>mode)
   || register_operand (operands[2], <MODE>mode)"
  "add<sfxtab>.f\\t%0,%1,%2"
  [(set_attr "length"     "4,4,4,8,8")
   (set_attr "type"       "add<sfxtab>")])

;; Extending this pattern to handle CCmode, we need to match GEU code
;; also.
(define_insn "add<mode>3_carry"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI
	 (plus:GPI (match_operand:GPI 1 "register_operand" "r")
		   (match_operand:GPI 2 "register_operand" "r"))
	 (ltu:GPI (reg:CC_C CC_REGNUM) (const_int 0))))]
  ""
  "adc<sfxtab>\\t%0,%1,%2"
  [(set_attr "type" "adc<sfxtab>")
   (set_attr "length" "4")])

;; Extending this pattern to handle Cmode, we need to match GEU code
;; also.
(define_insn "sub<mode>3_carry"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	 (minus:GPI (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:GPI 2 "register_operand" "r"))
	 (ltu:GPI (reg:CC CC_REGNUM) (const_int 0))))]
  ""
  "sbc<sfxtab>\\t%0,%1,%2"
  [(set_attr "type" "sbc<sfxtab>")
   (set_attr "length" "4")])

(define_expand "add<mode>3_Ccmp"
  [(parallel
    [(set (reg:CC_C CC_REGNUM)
	  (compare:CC_C
	   (plus:GPI
	    (match_operand:GPI 1 "arc64_nonmem_operand")
	    (match_operand:GPI 2 "arc64_nonmem_operand"))
	   (match_dup 1)))
     (set (match_operand:GPI 0 "register_operand")
	  (plus:GPI (match_dup 1) (match_dup 2)))])]
  ""
  )

(define_expand "sub<mode>3_Ccmp"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC
	   (match_operand:GPI 1 "arc64_nonmem_operand")
	   (match_operand:GPI 2 "arc64_nonmem_operand")))
     (set (match_operand:GPI 0 "register_operand")
	  (minus:GPI (match_dup 1) (match_dup 2)))])]
  ""
  )

(define_expand "<optab><mode>3"
  [(set (match_operand:DBLI 0 "register_operand")
	(ADDSUB:DBLI (match_operand:DBLI 1 "register_operand")
		     (match_operand:DBLI 2 "nonmemory_operand")))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  if (GET_MODE_SIZE (<MODE>mode) == (UNITS_PER_WORD * 2))
    {
      high_dest = gen_highpart (<REL>mode, operands[0]);
      low_dest = gen_lowpart (<REL>mode, operands[0]);
      op1_high = gen_highpart (<REL>mode, operands[1]);
      op1_low = gen_lowpart (<REL>mode, operands[1]);
      op2_high = gen_highpart_mode (<REL>mode, <MODE>mode, operands[2]);
      op2_low = gen_lowpart (<REL>mode, operands[2]);

      emit_insn (gen_<optab><rel>3_Ccmp (low_dest, op1_low,
					 force_reg (<REL>mode, op2_low)));
      emit_insn (gen_<optab><rel>3_carry (high_dest, op1_high,
					  force_reg (<REL>mode, op2_high)));

      DONE;
    }
  else if (!register_operand (operands[2], <MODE>mode)
	   && !satisfies_constraint_S32S0 (operands[2]))
    operands[2] = force_reg (<MODE>mode, operands[2]);

})

;; Shifted adds and subs
(define_insn "*add<mode>_shift"
  [(set (match_operand:GPI 0 "register_operand" "=q,r,r,r")
	(plus:GPI
	 (ashift:GPI (match_operand:GPI 1 "register_operand" "q,r,r,r")
		     (match_operand:GPI 2 "_1_2_3_operand" ""))
	 (match_operand:GPI 3 "arc64_regsym_operand"  "0,0,r,S32S0SymMV")))]
  ""
  "add%2<sfxtab>%?\\t%0,%3,%1"
  [(set_attr "type" "add")
   (set_attr "length" "*,4,4,8")
   (set_attr "iscompact" "maybe,no,no,no")])

(define_insn "*addzsidi_shift"
  [(set (match_operand:DI 0 "register_operand" "=q,r,r,r")
	(zero_extend:DI
	 (plus:SI
	  (ashift:SI (match_operand:SI 1 "register_operand" "q,r,r,r")
		     (match_operand:SI 2 "_1_2_3_operand" ""))
	 (match_operand:SI 3 "arc64_regsym_operand"  "0,0,r,S32S0SymMV"))))]
   ""
   "add%2%?\\t%0,%3,%1"
   [(set_attr "type" "add")
    (set_attr "length" "*,4,4,8")
    (set_attr "iscompact" "yes,no,no,no")])

(define_insn "*addx<mode>_cmp0"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (plus:GPI (ashift:GPI (match_operand:GPI 1 "register_operand" "r,r,r")
			       (match_operand:GPI 2 "_1_2_3_operand" ""))
		  (match_operand:GPI 3 "arc64_regsym_operand"  "0,r,S32S0SymMV"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(plus:GPI (ashift:GPI (match_dup 1) (match_dup 2))
		  (match_dup 3)))]
  ""
  "add%2<sfxtab>%?.f\\t%0,%3,%1"
  [(set_attr "type" "add<sfxtab>")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*addx<mode>_cmp0_noout"
  [(set (reg:CC_ZN CC_REGNUM)
	(compare:CC_ZN
	 (plus:GPI (ashift:GPI (match_operand:GPI 0 "register_operand" "r,r,r")
			       (match_operand:GPI 1 "_1_2_3_operand" ""))
		  (match_operand:GPI 2 "arc64_regsym_operand"  "0,r,S32S0SymMV"))
	 (const_int 0)))]
  ""
  "add%1<sfxtab>%?.f\\t0,%2,%1"
  [(set_attr "type" "add<sfxtab>")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*sub<mode>_shift"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(minus:GPI (match_operand:GPI 1 "arc64_regsym_operand" "0,r,S32S0SymMV")
		   (ashift:GPI (match_operand:GPI 2 "register_operand" "r,r,r")
			       (match_operand:GPI 3 "_1_2_3_operand" ""))))]
  ""
  "sub%3<sfxtab>\\t%0,%1,%2"
  [(set_attr "type" "sub<sfxtab>")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*add<mode>_mult"
  [(set (match_operand:GPI 0 "register_operand" "=q,r,r")
	(plus:GPI
	 (mult:GPI (match_operand:GPI 1 "register_operand" "q,r,r")
		   (match_operand:GPI 2 "_2_4_8_operand" ""))
	 (match_operand:GPI 3 "arc64_regsym_operand"  "0,r,S32S0SymMV")))]
  ""
  "add%s2<sfxtab>%?\\t%0,%3,%1"
  [(set_attr "type" "add")
   (set_attr "length" "*,4,8")
   (set_attr "iscompact" "maybe,no,no")])

(define_insn "*add<mode>_mult2"
  [(set (match_operand:GPI 0 "register_operand" "=q,r,r")
	(plus:GPI
	 (match_operand:GPI 1 "nonmemory_operand"  "0,r,S32S0")
	 (mult:GPI (match_operand:GPI 2 "register_operand" "q,r,r")
		   (match_operand:GPI 3 "_2_4_8_operand" ""))))]
  ""
  "add%s3<sfxtab>%?\\t%0,%1,%2"
  [(set_attr "type" "add")
   (set_attr "length" "*,4,8")
   (set_attr "iscompact" "maybe,no,no")])

;; Multiplications

(define_expand "<ANY_EXTEND:su_optab>mulhisi3"
  [(set (match_operand:SI 0 "register_operand")
	(mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand"))
		 (ANY_EXTEND:SI (match_operand:HI 2 "nonmemory_operand"))))]
   ""
   "
    if (CONSTANT_P (operands[2]))
    {
      operands[2] = force_reg (HImode, operands[2]);
    }
   "
  )

(define_insn "*<ANY_EXTEND:su_optab>mulhisi3r"
  [(set (match_operand:SI 0 "register_operand"                         "=q,r,r")
	(mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" "%0,0,r"))
		 (ANY_EXTEND:SI (match_operand:HI 2 "register_operand"  "q,r,r"))))]
  ""
  "mpy<ANY_EXTEND:su_optab>w%?\\t%0,%1,%2"
  [(set_attr "length" "*,4,4")
   (set_attr "iscompact" "maybe,no,no")
   (set_attr "type" "mpy")
   (set_attr "predicable" "yes,yes,no")
   ])

(define_insn "*<ANY_EXTEND:su_optab>mulhisi3rze"
  [(set (match_operand:DI 0 "register_operand"                         "=q,r,r")
	(zero_extend:DI
	 (mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" "%0,0,r"))
		  (ANY_EXTEND:SI (match_operand:HI 2 "register_operand"  "q,r,r")))))]
  ""
  "mpy<ANY_EXTEND:su_optab>w%?\\t%0,%1,%2"
  [(set_attr "length" "*,4,4")
   (set_attr "iscompact" "maybe,no,no")
   (set_attr "type" "mpy")])

(define_insn "mulhisi3i"
  [(set (match_operand:SI 0 "register_operand"            "=r,    r,    r,    r,accrn,r")
	(mult:SI
	 (sign_extend:SI
	  (match_operand:HI 1 "register_operand"          "%0,    r,    0,    0,    r,r"))
	 (match_operand:HI 2 "short_immediate_operand" "U06S0,U06S0,S12S0,S16S0,S16S0,S16S0")))]
  ""
  "@
   mpyw%?\\t%0,%1,%2
   mpyw%?\\t%0,%1,%2
   mpyw%?\\t%0,%1,%2
   mpyw%?\\t%0,%1,%2
   dmpyh\\t0,%1,%2
   mpyw%?\\t%0,%1,%2"
  [(set_attr "length" "4,4,4,8,8,8")
   (set_attr "type" "mpy")
   (set_attr "predicable" "yes,no,no,yes,no,no")])

(define_insn "umulhisi3i"
  [(set (match_operand:SI 0 "register_operand"            "=r,     r,    r,    r,accrn,r")
	(mult:SI
	 (zero_extend:SI
	  (match_operand:HI 1 "register_operand"           "%0,    r,    0,    0,    r,r"))
	 (match_operand:HI 2 "unsign_immediate_operand" "U06S0,U06S0,U12S0,U16S0,U16S0,U16S0")))]
  ""
  "@
   mpyuw%?\\t%0,%1,%2
   mpyuw%?\\t%0,%1,%2
   mpyuw%?\\t%0,%1,%2
   mpyuw%?\\t%0,%1,%2
   dmpyhu\\t0,%1,%2
   mpyuw%?\\t%0,%1,%2"
  [(set_attr "length" "4,4,4,8,8,8")
   (set_attr "type" "mpy")
   (set_attr "predicable" "yes,no,no,yes,no,no")])

;faulty;(define_insn "<ANY_EXTEND:su_optab>mulhisi3ize"
;faulty;  [(set (match_operand:DI 0 "register_operand"              "=r,    r,    r,r,r")
;faulty;	(zero_extend:DI
;faulty;	 (mult:SI (ANY_EXTEND:SI
;faulty;		   (match_operand:HI 1 "register_operand"    "%0,    r,    0,0,r"))
;faulty;		  (match_operand:HI 2 "immediate_operand" "U06S0,U06S0,S12S0,i,i"))))]
;faulty;  ""
;faulty;  "mpy<ANY_EXTEND:su_optab>w%?\\t%0,%1,%2"
;faulty;  [(set_attr "length" "4,4,4,8,8")
;faulty;   (set_attr "type" "mpy")
;faulty;   (set_attr "predicable" "yes,no,no,yes,no")])

(define_insn "*mul<mode>3"
  [(set (match_operand:GPI 0 "register_operand"            "=q,q,     r,     r,    r,    r, accrn,    r")
	(mult:GPI (match_operand:GPI 1 "register_operand"  "%0,q,     0,     r,    0,    0,     r,    r")
		  (match_operand:GPI 2 "nonmemory_operand"  "q,0,rU06S0,rU06S0,S12S0,S32S0,S32S0r,S32S0")))]
 ""
 "@
  mpy<sfxtab>%?\\t%0,%1,%2
  mpy<sfxtab>%?\\t%0,%2,%1
  mpy<sfxtab>%?\\t%0,%1,%2
  mpy<sfxtab>%?\\t%0,%1,%2
  mpy<sfxtab>%?\\t%0,%1,%2
  mpy<sfxtab>%?\\t%0,%1,%2
  mpyd%?\\t0,%1,%2
  mpy<sfxtab>%?\\t%0,%1,%2"
 [(set_attr "length" "*,*,4,4,4,8,8,8")
  (set_attr "iscompact" "maybe,maybe,no,no,no,no,no,no")
  (set_attr "type" "mpy<sfxtab>")
  (set_attr "predicable" "no,no,yes,no,no,yes,no,no")])

(define_insn "*mulsi3ze"
  [(set (match_operand:DI 0 "register_operand"   "=q,q,     r,     r,    r,    r,    r")
	(zero_extend:DI
	 (mult:SI
	  (match_operand:SI 1 "register_operand"  "%0,q,     0,     r,    0,    0,    r")
	  (match_operand:SI 2 "nonmemory_operand"  "q,0,rU06S0,rU06S0,S12S0,S32S0,S32S0"))))]
 ""
 "@
  mpy%?\\t%0,%1,%2
  mpy%?\\t%0,%2,%1
  mpy%?\\t%0,%1,%2
  mpy%?\\t%0,%1,%2
  mpy%?\\t%0,%1,%2
  mpy%?\\t%0,%1,%2
  mpy%?\\t%0,%1,%2"
 [(set_attr "length" "*,*,4,4,4,8,8")
  (set_attr "iscompact" "yes,yes,no,no,no,no,no")
  (set_attr "type" "mpy")
  (set_attr "predicable" "no,no,yes,no,no,yes,no")])

(define_insn "*mulsi3_cmp0"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (mult:SI
	  (match_operand:SI 1 "register_operand"      "%r,    0,r")
	  (match_operand:SI 2 "nonmemory_operand" "rU06S0,S12S0,i"))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand"        "=r,    r,r")
	(mult:SI (match_dup 1) (match_dup 2)))]
 ""
 "mpy%?.f\\t%0,%1,%2"
 [(set_attr "length" "4,4,8")
  (set_attr "type" "mpy")])

(define_insn "*mulsi3_cmp0_noout"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (mult:SI
	  (match_operand:SI 0 "register_operand"  "%     r,    r,r")
	  (match_operand:SI 1 "nonmemory_operand"  "rU06S0,S12S0,i"))
	 (const_int 0)))]
 ""
 "mpy%?.f\\t0,%0,%1"
 [(set_attr "length" "4,4,8")
  (set_attr "type" "mpy")])

(define_insn "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "%0,r"))
	   (ANY_EXTEND:DI (match_operand:SI 2 "register_operand"  "r,r")))
	  (const_int 32))))]
  ""
  "mpym<su_optab>%?\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "mpy")
   (set_attr "predicable" "yes,no")])

(define_insn "<su>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI
	   (ANY_EXTEND:TI (match_operand:DI 1 "register_operand" "%0,r"))
	   (ANY_EXTEND:TI (match_operand:DI 2 "register_operand" "r,r")))
	  (const_int 64))))]
  "TARGET_64BIT"
  "mpym<su_optab>l%?\\t%0,%1,%2"
  [(set_attr "type" "mpyl")
   (set_attr "length" "4")
   (set_attr "predicable" "yes,no")])

(define_expand "<su_optab>mulditi3"
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (ANY_EXTEND:TI (match_operand:DI 1 "register_operand"))
		 (ANY_EXTEND:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT"
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_<su>muldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_expand "usmulditi3"
  [(set (match_operand:TI                          0 "register_operand")
	(mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand"))
		 (sign_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT"
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_usmuldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "usmuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (zero_extend:TI
		    (match_operand:DI 1 "register_operand"  "r"))
		   (sign_extend:TI
		    (match_operand:DI 2 "register_operand" " r")))
	  (const_int 64))))]
  ""
  "mpymsul\t%0,%2,%1"
  [(set_attr "type" "mpyl")
   (set_attr "length" "4")])


;; 32 x 32 -> 64 (signed/unsigned) Triggers FAIL: c-c++-common/torture/builtin-arith-overflow-12.c
(define_expand "<ANY_EXTEND:su_optab>mulsidi3"
  [(parallel [(set (match_operand:DI 0 "register_operand")
		   (mult:DI
		    (ANY_EXTEND:DI (match_operand:SI 1 "register_operand"))
		    (ANY_EXTEND:DI (match_operand:SI 2 "nonmemory_operand"))))
	      (clobber (reg:DI R58_REGNUM))])]
   "TARGET_SIMD"
   "
    if (CONSTANT_P (operands[2]))
    {
      operands[2] = force_reg (SImode, operands[2]);
    }
   ")

(define_insn "*mpyd<ANY_EXTEND:su_optab>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI
	 (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r"))
	 (ANY_EXTEND:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "mpyd<ANY_EXTEND:su_optab>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "mpy")])

(define_insn "*mpyd<ANY_EXTEND:su_optab>0"
  [(set (reg:DI R58_REGNUM)
	(mult:DI
	 (ANY_EXTEND:DI (match_operand:SI 0 "register_operand" "r"))
	 (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r"))))]
  "TARGET_SIMD"
  "mpyd<ANY_EXTEND:su_optab>\\t0,%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "mpy")])

(define_insn "*mpyd<ANY_EXTEND:su_optab>i"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(mult:DI
	 (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r,0,r"))
	 (match_operand:SI 2 "<su_optab>signed32b_operand" "U06S0,S12S0,i")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "mpyd<ANY_EXTEND:su_optab>\\t%0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mpy")])

;; 16bit operations using SIMD instructions
;; This gives worst code, keep it here for any other ideas.
;; exth -> add -> exth/extb :: add -> exth/extb
;;(define_insn "<optab>hi3"
;;  [(set (match_operand:HI 0 "register_operand"  "=r,r")
;;	(ADDSUB:HI
;;	 (match_operand:HI 1 "register_operand"  "r,r")
;;	 (match_operand:HI 2 "nonmemory_operand" "r,i")))]
;;  "TARGET_SIMD"
;;  "@
;;   v<optab>2h\\t%0,%1,%2
;;   v<optab>2h\\t%0,%1,%2@u32"
;;   [(set_attr "length" "4,8")
;;   (set_attr "type" "v<optab>")])


;; MADD patterns
;; 32 + (signe) 16 x (signe) 16 -> 32
(define_expand "<ANY_EXTEND:su_optab>maddhisi4"
  [(set (match_operand: SI 0 "register_operand")
	(plus:SI
	 (mult:SI
	  (ANY_EXTEND:SI (match_operand:HI 1 "register_operand"))
	  (ANY_EXTEND:SI (match_operand:HI 2 "register_operand")))
	 (match_operand:SI 3 "register_operand")))]
  "TARGET_SIMD && TARGET_64BIT"
  {
   rtx acc = gen_rtx_REG (SImode, R58_REGNUM);

   emit_move_insn (acc, operands[3]);
   emit_insn (gen_<ANY_EXTEND:su_optab>machi (operands[0], operands[1],
                                             operands[2], acc));
   DONE;
   })

(define_insn "<ANY_EXTEND:su_optab>machi0"
 [(set (reg:SI R58_REGNUM)
       (plus:SI
	(mult:SI
	 (ANY_EXTEND:SI (match_operand:HI 0 "register_operand" "%r,r"))
	 (ANY_EXTEND:SI (match_operand:HI 1 "nonmemory_operand" "rU06S0,i")))
	(reg:SI R58_REGNUM)))]
 "TARGET_SIMD && TARGET_64BIT"
 "vmac2h<ANY_EXTEND:su_optab>\\t0,%0,%1"
 [(set_attr "length" "4,8")
  (set_attr "type" "mac")])

;; The second move instruction can be remove, however, we need to add
;; a step that recognizes implicit accumulator reads and writes.
(define_insn_and_split "<ANY_EXTEND:su_optab>machi"
 [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
       (plus:SI
	(mult:SI
	 (ANY_EXTEND:SI (match_operand:HI 1 "register_operand"      "%r,    0,r,r"))
	 (ANY_EXTEND:SI (match_operand:HI 2 "nonmemory_operand" "rU06S0,S12S0,i,*ri")))
	(match_operand:SI 3 "register_operand" "accum,accum,accum,*r")))
  (clobber (reg:SI R58_REGNUM))]
 "TARGET_SIMD && TARGET_64BIT"
 "@
  vmac2h<ANY_EXTEND:su_optab>\\t%0,%1,%2
  vmac2h<ANY_EXTEND:su_optab>\\t%0,%1,%2
  vmac2h<ANY_EXTEND:su_optab>\\t%0,%1,%2
  #"
 "&& reload_completed && (REGNO (operands[3]) != R58_REGNUM)"
 [(set (reg:SI R58_REGNUM) (match_dup 3))
  (set (reg:SI R58_REGNUM)
       (plus:SI (mult:SI (ANY_EXTEND:SI (match_dup 1))
			 (ANY_EXTEND:SI (match_dup 2)))
		(reg:SI R58_REGNUM)))
  (set (match_dup 0) (reg:SI R58_REGNUM))]
  ""
  [(set_attr "length" "4,4,8,8")
   (set_attr "type" "mac")])

;; 64 + (signe) 32 x (signe) 32 -> 64
(define_expand "<ANY_EXTEND:su_optab>maddsidi4"
  [(set (match_operand: DI 0 "register_operand")
	(plus:DI
	 (mult:DI
	  (ANY_EXTEND:DI (match_operand:SI 1 "register_operand"))
	  (ANY_EXTEND:DI (match_operand:SI 2 "register_operand")))
	 (match_operand:DI 3 "register_operand")))]
  "TARGET_SIMD"
  {
   rtx acc = gen_rtx_REG (DImode, R58_REGNUM);

   emit_move_insn (acc, operands[3]);
   emit_insn (gen_<ANY_EXTEND:su_optab>macd (operands[0], operands[1],
                                            operands[2], acc));
   DONE;
   })

(define_insn "<ANY_EXTEND:su_optab>macd0"
 [(set (reg:DI R58_REGNUM)
       (plus:DI
	(mult:DI
	 (ANY_EXTEND:DI (match_operand:SI 0 "register_operand" "%r,r"))
	 (ANY_EXTEND:DI (match_operand:SI 1 "nonmemory_operand" "rU06S0,i")))
	(reg:DI R58_REGNUM)))]
 "TARGET_SIMD"
 "macd<ANY_EXTEND:su_optab>\\t0,%0,%1"
 [(set_attr "length" "4,8")
  (set_attr "type" "mac")])

(define_insn_and_split "<ANY_EXTEND:su_optab>macd"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(plus:DI
	 (mult:DI
	  (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "%r,0,r,r"))
	  (ANY_EXTEND:DI (match_operand:SI 2 "nonmemory_operand" "rU06S0,S12S0,i,*ri")))
	 (match_operand:DI 3 "register_operand" "accum,accum,accum,*r")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "@
   macd<ANY_EXTEND:su_optab>\\t%0,%1,%2
   macd<ANY_EXTEND:su_optab>\\t%0,%1,%2
   macd<ANY_EXTEND:su_optab>\\t%0,%1,%2
   #"
  "&& reload_completed && (REGNO (operands[3]) != R58_REGNUM)"
  [(set (reg:DI R58_REGNUM) (match_dup 3))
   (parallel
    [(set (match_dup 0)
	  (plus:DI (mult:DI (ANY_EXTEND:DI (match_dup 1))
			    (ANY_EXTEND:DI (match_dup 2)))
		   (reg:DI R58_REGNUM)))
     (clobber (reg:DI R58_REGNUM))])]
  ""
  [(set_attr "length" "4,4,8,8")
   (set_attr "type" "mac")])

;; This is a combiner pattern: we need to split it in 3 instructions.
;; The second move is propagated to fallowing instructions by
;; cprop_hardreg.  Unfortunately, I cannot use a second peephole
;; pattern for merging the left overs from cprop_hardreg back to mac
;; instruction as there is no peephole step following it, thus, we
;; make use of ARC's specific machine reorder step to merge back into
;; MAC instruction the MOV instructions which were not propagated by
;; cprop_hardreg step.

(define_insn_and_split "macsi"
 [(set (match_operand:SI 0 "register_operand" "=r,r,r")
       (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "%r,r,r")
			 (match_operand:SI 2 "nonmemory_operand" "rU06S0,i,*ri"))
		(match_operand:SI 3 "nonmemory_operand" "accum,accum,*ri")))
  (clobber (reg:SI R58_REGNUM))]
 "TARGET_SIMD"
 "@
  mac\\t%0,%1,%2
  mac\\t%0,%1,%2
  #"
 "&& reload_completed && (REGNO (operands[3]) != R58_REGNUM)"
 [(set (reg:SI R58_REGNUM) (match_dup 3))
  (set (reg:SI R58_REGNUM)
   (plus:SI (mult:SI (match_dup 1) (match_dup 2)) (reg:SI R58_REGNUM)))
  (set (match_dup 0) (reg:SI R58_REGNUM))]
 ""
 [(set_attr "length" "4,8,8")
  (set_attr "type" "mac")])

(define_insn "macsi0"
 [(set (reg:SI R58_REGNUM)
       (plus:SI (mult:SI (match_operand:SI 0 "register_operand" "%r,r")
			 (match_operand:SI 1 "nonmemory_operand" "rU06S0,i"))
		(reg:SI R58_REGNUM)))]
 "TARGET_SIMD"
 "mac\\t0,%0,%1"
 [(set_attr "length" "4,8")
  (set_attr "type" "mac")])

;; Try to propagate first move into adjacent previous instructions
;; N.B. Probably we need to make a more complex step to take care of
;; this operation when we schedule
(define_peephole2
  [(set (match_operand:HI_SI 0 "register_operand" "")
	(ARITH:HI_SI (match_operand:HI_SI 1 "register_operand" "")
		     (match_operand:HI_SI 2 "nonmemory_operand" "")))
   (set (reg:HI_SI R58_REGNUM) (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:HI_SI R58_REGNUM) (ARITH:HI_SI (match_dup 1) (match_dup 2)))])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" ""))
		 (ANY_EXTEND:SI (match_operand:HI 2 "register_operand" ""))))
   (set (reg:SI R58_REGNUM) (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:SI R58_REGNUM)
	(mult:SI (ANY_EXTEND:SI (match_dup 1)) (ANY_EXTEND:SI (match_dup 2))))])

(define_peephole2
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (mult:DI (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" ""))
		   (ANY_EXTEND:DI (match_operand:SI 2 "register_operand" ""))))
     (clobber (reg:DI R58_REGNUM))])
   (set (reg:DI R58_REGNUM) (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:DI R58_REGNUM)
	(mult:DI (ANY_EXTEND:DI (match_dup 1)) (ANY_EXTEND:DI (match_dup 2))))])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" ""))
		  (match_operand 2 "immediate_operand" "")))
   (set (reg:SI R58_REGNUM) (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:SI R58_REGNUM)
	(mult:SI (ANY_EXTEND:SI (match_dup 1)) (match_dup 2)))])

;; Propagate r58 to arithmetic operation when dealing with zero extension
(define_peephole2
  [(set (match_operand:HI 0 "register_operand")
	(ADDSUB:HI (match_operand:HI 1 "register_operand")
		   (match_operand:HI 2 "nonmemory_operand")))
   (set (reg:SI R58_REGNUM) (match_operand:SI 3 "register_operand"))]
  "peep2_reg_dead_p (2, operands[0])
   && (REGNO (operands[3]) == REGNO (operands[0]))"
  [(set (reg:HI R58_REGNUM) (ADDSUB:HI (match_dup 1) (match_dup 2)))])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (ANY_EXTEND:SI (match_operand:HI 1 "register_operand" ""))
		  (match_operand 2 "immediate_operand" "")))
   (set (reg:HI R58_REGNUM) (match_operand:HI 3 "register_operand"))]
  "peep2_reg_dead_p (2, operands[0])
   && (REGNO (operands[3]) == REGNO (operands[0]))"
  [(set (reg:SI R58_REGNUM)
	(mult:SI (ANY_EXTEND:SI (match_dup 1)) (match_dup 2)))])

;; Another combiner pattern (observed in rgbyiq01)
(define_insn_and_split "dmpywhu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (mult:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand 2 "unsign_immediate_operand" "i"))
	 (mult:SI (match_operand:SI 3 "register_operand" "r")
		  (match_operand 4 "unsign_immediate_operand" "i"))))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (unspec:SI [(match_dup 5) (match_dup 2)]
			      ARC64_UNSPEC_DMPYWHU))
	      (clobber (reg:DI R58_REGNUM))])]
  {
   operands[5] = gen_lowpart (DImode, operands[0]);
   emit_insn (gen_pack2silo (operands[5], operands[3], operands[1]));
   operands[2] = GEN_INT ((INTVAL (operands[2]) << 16) + INTVAL (operands[4]));
  }
  [(set_attr "length" "8")
   (set_attr "type" "dmpywh")])

(define_insn "dmpywhu0"
  [(set (match_operand:SI 0 "register_operand"        "=accum,r")
	(unspec:SI [(match_operand:DI 1 "register_operand" "r,r")
		    (match_operand 2 "immediate_operand"   "i,i")]
		   ARC64_UNSPEC_DMPYWHU))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "@
   dmpywhu\\t0,%1,%2@u32
   dmpywhu\\t%0,%1,%2@u32"
  [(set_attr "length" "8")
   (set_attr "type" "dmpywh")])

(define_insn_and_split "dmpywh"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (mult:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand 2 "short_immediate_operand" "i"))
	 (mult:SI (match_operand:SI 3"register_operand" "r")
		  (match_operand 4 "short_immediate_operand" "i"))))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (unspec:SI [(match_dup 5) (match_dup 2)]
			      ARC64_UNSPEC_DMPYWH))
	      (clobber (reg:SI R58_REGNUM))])]
  {
   operands[5] = gen_lowpart (DImode, operands[0]);
   emit_insn (gen_pack2silo (operands[5], operands[3], operands[1]));
   operands[2] = GEN_INT ((INTVAL (operands[2]) << 16) +
			  (INTVAL (operands[4]) & 0xffff));
  }
  [(set_attr "length" "8")
   (set_attr "type" "dmpywh")])

(define_insn "dmpywh0"
  [(set (match_operand:SI 2 "register_operand"        "=accum,r")
	(unspec:SI [(match_operand:DI 0 "register_operand" "r,r")
		    (match_operand 1 "immediate_operand"   "i,i")]
		   ARC64_UNSPEC_DMPYWH))
   (clobber (reg:SI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "@
   dmpywh\\t0,%0,%1@u32
   dmpywh\\t%2,%0,%1@u32"
  [(set_attr "length" "8")
   (set_attr "type" "dmpywh")])

(define_insn "*mpywhu"
  [(set (match_operand:SI 0 "register_operand"                 "=r,r")
	(mult:SI
	 (zero_extend:SI (match_operand:HI 1 "register_operand" "r,r"))
	 (match_operand:SI 2 "arc64_reg_or_unsig_operand"       "r,i")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "dmpywhu\\t%0,%2,%1"
  [(set_attr "length" "4,8")
   (set_attr "type" "dmpywh")
   ])

(define_insn "*mpywh"
  [(set (match_operand:SI 0 "register_operand"                 "=r,r")
	(mult:SI
	 (sign_extend:SI (match_operand:HI 1 "register_operand" "r,r"))
	 (match_operand:SI 2 "arc64_nonmem_operand"             "r,i")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "dmpywh\\t%0,%2,%1"
  [(set_attr "length" "4,8")
   (set_attr "type" "dmpywh")])

;; dmach combine pattern used to implement 16b MAC patterns.  Extra
;; care needs to be taken when dealing with immediates which needs to
;; set the higher 16b to zero.  I.e. we cannot use safely U6 or S12
;; instruction variants.
(define_insn_and_split "dmach"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(plus:HI
	 (mult:HI (match_operand:HI 1 "register_operand" "%r,r,r")
		  (match_operand:HI 2 "nonmemory_operand" "r,i,*ri"))
	 (match_operand:HI 3 "nonmemory_operand" "accum,accum,*ri")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "@
   dmach\\t%0,%1,%2
   dmach\\t%0,%1,%V2@u32
   #"
  "&& reload_completed
   && (CONST_INT_P (operands[3]) || (REGNO (operands[3]) != R58_REGNUM))"
  [(set (reg:HI R58_REGNUM) (match_dup 3))
   (set (reg:HI R58_REGNUM)
	(plus:HI (mult:HI (match_dup 1) (match_dup 2)) (reg:HI R58_REGNUM)))
   (set (match_dup 0) (reg:HI R58_REGNUM))]
  ""
 [(set_attr "length" "4,8,8")
  (set_attr "type" "mac")])

(define_insn "dmach0"
 [(set (reg:HI R58_REGNUM)
       (plus:HI (mult:HI (match_operand:HI 0 "register_operand" "%r,r")
			 (match_operand:HI 1 "nonmemory_operand" "r,i"))
		(reg:HI R58_REGNUM)))]
 "TARGET_SIMD"
 "@
  dmach\\t0,%0,%1
  dmach\\t0,%0,%V1@u32"
 [(set_attr "length" "4,8")
  (set_attr "type" "mac")])

;; macwh combine pattern
;; FIXME! maybe we shoudl use r58 as intermediate result holder to
;; enable linking (back-to-back) with other MAC instructions, but I
;; haven't seen any example.
(define_insn_and_split "dmacwh"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (plus:SI
	  (mult:SI (match_operand:SI 1 "register_operand"     "r")
		   (match_operand 2 "short_immediate_operand" "i"))
	  (mult:SI (match_operand:SI 3 "register_operand"     "r")
		   (match_operand 4 "short_immediate_operand" "i")))
	 (match_operand:SI 5 "nonmemory_operand"  "ri")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(unspec:SI [(match_dup 6) (match_dup 2) (reg:SI R58_REGNUM)]
		    ARC64_UNSPEC_DMACWH))]
  {
   emit_move_insn (gen_rtx_REG (SImode, R58_REGNUM), operands[5]);
   operands[6] = gen_lowpart (DImode, operands[0]);
   emit_insn (gen_pack2silo (operands[6], operands[3], operands[1]));
   operands[2] = GEN_INT ((INTVAL (operands[2]) << 16)
			  + (INTVAL (operands[4]) & 0xffff));
  }
  [(set_attr "length" "8")
   (set_attr "type" "mac")])

(define_insn "pack2silo"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")]
		   ARC64_UNSPEC_VPACK2WL))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "dmacwh0"
  [(set (match_operand:SI 0 "register_operand" "=accum,r")
	(unspec:SI [(match_operand:DI 1 "register_operand"  "r,r")
		    (match_operand 2 "immediate_operand" "i,i")
		    (reg:SI R58_REGNUM)]
		   ARC64_UNSPEC_DMACWH))]
  "TARGET_SIMD"
  "@
   dmacwh\\t0,%1,%2@u32
   dmacwh\\t%0,%1,%2@u32"
  [(set_attr "length" "8")
   (set_attr "type" "mac")])

;; FIXME! maybe we shoudl use r58 as intermediate result holder to
;; enable linking (back-to-back) with other MAC instructions, but I
;; haven't seen any example.
(define_insn_and_split "dmacwhu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (plus:SI
	  (mult:SI (match_operand:SI 1 "register_operand"      "r")
		   (match_operand 2 "unsign_immediate_operand" "i"))
	  (mult:SI (match_operand:SI 3 "register_operand"      "r")
		   (match_operand 4 "unsign_immediate_operand" "i")))
	 (match_operand:SI 5 "nonmemory_operand"  "ri")))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(unspec:SI [(match_dup 6) (match_dup 2) (reg:SI R58_REGNUM)]
		    ARC64_UNSPEC_DMACWHU))]
  {
   emit_move_insn (gen_rtx_REG (SImode, R58_REGNUM), operands[5]);
   operands[6] = gen_lowpart (DImode, operands[0]);
   emit_insn (gen_pack2silo (operands[6], operands[3], operands[1]));
   operands[2] = GEN_INT ((INTVAL (operands[2]) << 16) + INTVAL (operands[4]));
  }
  [(set_attr "length" "8")
   (set_attr "type" "mac")])

(define_insn "dmacwhu0"
  [(set (match_operand:SI 0 "register_operand" "=accum,r")
	(unspec:SI [(match_operand:DI 1 "register_operand"  "r,r")
		    (match_operand 2 "immediate_operand" "i,i")
		    (reg:SI R58_REGNUM)]
		   ARC64_UNSPEC_DMACWHU))]
  "TARGET_SIMD"
  "@
   dmacwhu\\t0,%1,%2@u32
   dmacwhu\\t%0,%1,%2@u32"
  [(set_attr "length" "8")
   (set_attr "type" "mac")])

(define_insn "*vpack2hl_scalar"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI
	 (and:SI (match_operand:SI 1 "register_operand" "r")
		 (const_int 65535))
	 (ashift:SI (match_operand:SI 2 "register_operand" "r")
		    (const_int 16))))]
  "TARGET_SIMD"
  "vpack2hl\\t%0,%2,%1"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

(define_insn "*vpack2wl_scalar"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI
	 (ashift:DI (match_operand:DI 1 "register_operand" "r")
		    (const_int 32))
	 (zero_extend:DI (match_operand:SI 2  "register_operand" "r"))))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%2,%1"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

;; -------------------------------------------------------------------
;; Integer SIMD instructions
;; -------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:VALL 0 "nonimmediate_operand")
	(match_operand:VALL 1 "general_operand"))]
  "TARGET_SIMD"
  "
   if (arc64_prepare_move_operands (operands[0], operands[1], <MODE>mode))
    DONE;
  ")

(define_expand "movmisalign<mode>"
  [(set (match_operand:VALL 0 "nonimmediate_operand")
	(match_operand:VALL 1 "general_operand"))]
  "TARGET_SIMD && !STRICT_ALIGNMENT"
  "
   if (arc64_prepare_move_operands (operands[0], operands[1], <MODE>mode))
    DONE;
  ")

(define_insn "*mov<mode>_insn"
  [(set (match_operand:VALL 0 "arc64_dest_operand"  "=r,r,Ustor")
	(match_operand:VALL 1 "nonimmediate_operand" "r,m,r"))]
  "TARGET_SIMD && TARGET_64BIT
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   mov<mcctab>\\t%0,%1
   ld<mcctab>%U1\\t%0,%1
   st<mcctab>%U0\\t%1,%0"
  [(set_attr "type" "move,ld,st")])

(define_insn "arc64_vpack_v4hihi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(unspec:V4HI [(match_operand:HI 1 "register_operand" "r")
		      (match_operand:HI 2 "register_operand" "r")]
		     ARC64_UNSPEC_VPACK4HL))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_vpack_v2sisi"
  [(set (match_operand:V2SI 0 "register_operand"    "=r,    r,r,r")
	(vec_concat:V2SI
         (match_operand:SI 1 "register_operand"  "    r,    0,r,r")
	 (match_operand:SI 2 "nonmemory_operand" "U06S0,S12S0,r,S32S0")))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%2"
  [(set_attr "length" "4,4,4,8")
   (set_attr "type" "vpack")])

(define_expand "vec_init<mode><vel>"
  [(match_operand:V64I 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_SIMD && TARGET_64BIT"
  {
   arc64_expand_vector_init (operands[0], operands[1]);
   DONE;
  })

(define_insn "<optab><mode>3"
  [(set (match_operand:VALL 0 "register_operand"           "=r")
	(ADDSUB:VALL (match_operand:VALL 1 "register_operand" "r")
		     (match_operand:VALL 2 "register_operand" "r")))]
  "TARGET_SIMD"
  "v<mntab><sfxtab>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "v<mntab>")])

;; Add with duplicate input.
(define_insn "*add<mode>3_dup"
  [(set (match_operand:VALL 0 "register_operand" "=r,r")
	(plus:VALL
	 (vec_duplicate:VALL
	  (match_operand 1 "vectdup_immediate_operand" "S06S0,S12S0"))
	 (match_operand:VALL 2 "register_operand" "r,0")))]
  "TARGET_SIMD"
  "vadd<sfxtab>\\t%0,%2,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vadd")])

(define_insn "neg<mode>2"
  [(set (match_operand:VALL 0 "register_operand" "=r")
	(neg:VALL (match_operand:VALL 1 "register_operand" "r")))]
  "TARGET_SIMD"
  "vsub<sfxtab>\\t%0,0,%1"
  [(set_attr "length" "8")
   (set_attr "type" "vsub")])

(define_expand "vec_widen_<su>mult_lo_v4hi"
 [(match_operand:V2SI 0 "register_operand")
  (ANY_EXTEND:V2SI (match_operand:V4HI 1 "register_operand"))
  (ANY_EXTEND:V2SI (match_operand:V4HI 2 "register_operand"))]
  "TARGET_SIMD"
  {
    emit_insn (gen_arc64_<su>vmpy2h (operands[0],
				     operands[1],
				     operands[2]));
    DONE;
  })

(define_expand "vec_widen_<su>mult_hi_v4hi"
 [(match_operand:V2SI 0 "register_operand")
  (ANY_EXTEND:V2SI (match_operand:V4HI 1 "register_operand"))
  (ANY_EXTEND:V2SI (match_operand:V4HI 2 "register_operand"))]
  "TARGET_SIMD"
  {
    rtx tmp1;
    rtx tmp2;
    if (TARGET_64BIT)
      {
	tmp1 = gen_reg_rtx (V4HImode);
	tmp2 = gen_reg_rtx (V4HImode);
	emit_insn (gen_arc64_swapl (tmp1, operands[1]));
	emit_insn (gen_arc64_swapl (tmp2, operands[2]));
	emit_insn (gen_arc64_<su>vmpy2h (operands[0], tmp1, tmp2));
      }
    else
      {
	tmp1 = operands[1];
	tmp2 = operands[2];
	emit_insn (gen_arc32_<su>vmpy2h_hi (operands[0], tmp1, tmp2));
      }
    DONE;
  })

 (define_insn "arc64_<su>vmpy2h"
   [(set (match_operand:V2SI 0 "register_operand"  "=r")
	 (mult:V2SI
	  (ANY_EXTEND:V2SI
	   (vec_select:V2HI
	    (match_operand:V4HI 1 "register_operand" "r")
	    (parallel [(const_int 0) (const_int 1)])))
	  (ANY_EXTEND:V2SI
	   (vec_select:V2HI
	    (match_operand:V4HI 2 "register_operand" "r")
	    (parallel [(const_int 0) (const_int 1)])))))
    (clobber (reg:V2SI R58_REGNUM))]
   "TARGET_SIMD"
   "vmpy2h<su_optab>\\t%0,%1,%2"
   [(set_attr "length" "4")
    (set_attr "type" "vmpy2h")])

(define_insn "arc64_swapl"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	 (vec_select:V2HI (match_operand:V4HI 1 "register_operand" "r")
			  (parallel [(const_int 2) (const_int 3)]))
	 (vec_select:V2HI (match_dup 1) (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SIMD && TARGET_64BIT"
  "swapl\\t%0,%1"
 [(set_attr "length" "4")
  (set_attr "type" "swapl")])

(define_expand "<su>dot_prodv4hi"
  [(match_operand:V2SI 0 "register_operand")
   (ANY_EXTEND:V2SI (match_operand:V4HI 1 "register_operand"))
   (ANY_EXTEND:V2SI (match_operand:V4HI 2 "register_operand"))
   (match_operand:V2SI 3 "register_operand")]
  "TARGET_SIMD"
{
  rtx acc_reg  = gen_rtx_REG (V2SImode, R58_REGNUM);

  emit_move_insn (acc_reg, operands[3]);
  emit_insn (gen_arc64_<su>vmach_zero (operands[1], operands[2]));
  if (TARGET_64BIT)
    {
      rtx op1_high = gen_reg_rtx (V4HImode);
      rtx op2_high = gen_reg_rtx (V4HImode);
      emit_insn (gen_arc64_swapl (op1_high, operands[1]));
      emit_insn (gen_arc64_swapl (op2_high, operands[2]));
      emit_insn (gen_arc64_<su>vmach (operands[0], op1_high, op2_high));
    }
  else
    {
      emit_insn (gen_arc32_<su>vmach_hi (operands[0], operands[1], operands[2]));
    }
  DONE;
})

(define_insn "arc64_<su>vmach"
 [(set (match_operand:V2SI 0 "register_operand" "=r")
       (plus:V2SI
	(mult:V2SI
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 1 "register_operand" "r")
			   (parallel [(const_int 0) (const_int 1)])))
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 2 "register_operand" "r")
			   (parallel [(const_int 0) (const_int 1)]))))
	(reg:V2SI R58_REGNUM)))
  (clobber (reg:V2SI R58_REGNUM))]
  "TARGET_SIMD"
  "vmac2h<su_optab>%?\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vmac2h")])

(define_insn "arc64_<su>vmach_zero"
 [(set (reg:V2SI R58_REGNUM)
       (plus:V2SI
	(mult:V2SI
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 0 "register_operand" "r")
			   (parallel [(const_int 0) (const_int 1)])))
	 (ANY_EXTEND:V2SI
	  (vec_select:V2HI (match_operand:V4HI 1 "register_operand" "r")
			   (parallel [(const_int 0) (const_int 1)]))))
	(reg:V2SI R58_REGNUM)))]
  "TARGET_SIMD"
  "vmac2h<su_optab>%?\\t0,%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vmac2h")])

;; FIXME! for v2hi -> dmpyh
(define_expand "reduc_plus_scal_v4hi"
  [(parallel
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (unspec:HI [(match_operand:V4HI 1 "register_operand" "r")]
		     ARC64_UNSPEC_QMPYH))
     (clobber (reg:DI R58_REGNUM))])]
  "TARGET_SIMD"
  "")

(define_insn_and_split "*reduc_v4hi"
  [(set (match_operand:HI 0 "register_operand" "=accum,r")
	(unspec:HI [(match_operand:V4HI 1 "register_operand" "r,r")]
		   ARC64_UNSPEC_QMPYH))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "qmpyh\\t%0,%1,1"
  "&& reload_completed && !TARGET_64BIT && (REGNO (operands[0]) != R58_REGNUM)"
  [(parallel
    [(set (reg:HI R58_REGNUM)
	  (unspec:HI [(match_dup 1)] ARC64_UNSPEC_QMPYH))
     (clobber (reg:DI R58_REGNUM))])
   (set (match_dup 0) (reg:HI R58_REGNUM))]
  ""
  [(set_attr "length" "8,4")
   (set_attr "type" "qmpyh")])

(define_insn "reduc_plus_scal_v2si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:V2SI 1 "register_operand" "r")]
		   ARC64_UNSPEC_DMPYWH))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD && TARGET_64BIT"
  "dmpywh\\t%0,%1,1"
  [(set_attr "length" "4")
   (set_attr "type" "dmpywh")])

;; FIXME! for v2hi -> dmach
(define_expand "fold_left_plus_v4hi"
  [(set (match_operand:HI 0 "register_operand")
	(unspec:HI [(match_operand:HI 1 "register_operand")
		    (match_operand:V4HI 2 "register_operand")]
		   ARC64_UNSPEC_QMACH))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  {
    rtx acc_reg = gen_rtx_REG (HImode, R58_REGNUM);
    emit_move_insn (acc_reg, operands[1]);
    operands[1] = acc_reg;
  })

(define_insn "*qmach"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(unspec:HI [(reg:HI R58_REGNUM)
		    (match_operand:V4HI 1 "register_operand" "r")]
		   ARC64_UNSPEC_QMACH))
   (clobber (reg:DI R58_REGNUM))]
  "TARGET_SIMD"
  "qmach\\t%0,%1,1"
  [(set_attr "length" "4")
   (set_attr "type" "qmach")])

(define_expand "mulv2hi3"
  [(set (match_operand:V2HI 0 "register_operand")
	(mult:V2HI (match_operand:V2HI 1 "register_operand")
		   (match_operand:V2HI 2 "register_operand")))]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmp = gen_reg_rtx (V2SImode);
    emit_insn (gen_arc64_svmpy2h_lo (tmp, operands[1], operands[2]));
    emit_insn (gen_arc64_packv2hi_lo (operands[0], tmp));
    DONE;
  })

(define_insn "arc64_packv2hi_lo"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(unspec:V2HI [(match_operand:V2SI 1 "register_operand" "r")
		      (const_int 0)]
		     ARC64_UNSPEC_VPACK4HL))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hl\\t%0,%1,0"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_expand "<su>mulv2hi3_highpart"
  [(match_operand:V2HI 0 "register_operand")
   (ANY_EXTEND:SI (match_operand:V2HI 1 "register_operand"))
   (ANY_EXTEND:SI (match_operand:V2HI 2 "register_operand"))]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmp = gen_reg_rtx (V2SImode);
    emit_insn (gen_arc64_<su>vmpy2h_lo (tmp, operands[1], operands[2]));
    emit_insn (gen_arc64_packv2hi_hi (operands[0], tmp));
    DONE;
  })

(define_insn "arc64_packv2hi_hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(unspec:V2HI [(match_operand:V2SI 1 "register_operand" "r")
		      (const_int 1)]
		     ARC64_UNSPEC_VPACK4HM))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hm\\t%0,%1,0"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

 (define_insn "arc64_<su>vmpy2h_lo"
   [(set (match_operand:V2SI 0 "register_operand"  "=r")
	 (mult:V2SI
	  (ANY_EXTEND:V2SI
	    (match_operand:V2HI 1 "register_operand" "r"))
	  (ANY_EXTEND:V2SI
	    (match_operand:V2HI 2 "register_operand" "r"))))
    (clobber (reg:V2SI R58_REGNUM))]
   "TARGET_SIMD"
   "vmpy2h<su_optab>\\t%0,%1,%2"
   [(set_attr "length" "4")
    (set_attr "type" "vmpy2h")])

(define_expand "mulv4hi3"
  [(match_operand:V4HI 0 "register_operand")
   (match_operand:V4HI 1 "register_operand")
   (match_operand:V4HI 2 "register_operand")]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmpA = gen_reg_rtx (V2SImode);
    rtx tmpB = gen_reg_rtx (V2SImode);
    rtx tmp1 = gen_reg_rtx (V4HImode);
    rtx tmp2 = gen_reg_rtx (V4HImode);

    emit_insn (gen_arc64_swapl (tmp1, operands[1]));
    emit_insn (gen_arc64_swapl (tmp2, operands[2]));
    emit_insn (gen_arc64_svmpy2h (tmpA, operands[1], operands[2]));
    emit_insn (gen_arc64_svmpy2h (tmpB, tmp1, tmp2));
    emit_insn (gen_arc64_pack4hi (operands[0], tmpA, tmpB));
    DONE;
    })

(define_insn "arc64_pack4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	 (truncate:V2HI
	  (match_operand:V2SI 1 "register_operand" "r"))
	 (truncate:V2HI
	  (match_operand:V2SI 2 "register_operand" "r"))))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "bswap<mode>2"
  [(set (match_operand:VALL 0 "register_operand" "=r")
	(bswap:VALL (match_operand:VALL 1 "register_operand" "r")))]
  "TARGET_SIMD"
  "swape<mcctab>\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "swap")])

(define_insn "vec_extract<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=r")
	(vec_select:<VEL> (match_operand:VALL 1 "register_operand" "r")
			  (parallel [(match_operand:SI 2 "const_int_operand" "n")])))]
  "TARGET_SIMD"
  {
    HOST_WIDE_INT elem = INTVAL (operands[2]);
    gcc_assert (elem < 4);
    elem = (((<vextrsz> - 1) & <vextrmsk>) << <vextrsh>)
      | ((elem * <vextrsz>) & <vextrmsk>);
    operands[2] = GEN_INT (elem);
    return "xbfu<mcctab>\\t%0,%1,%2";
  }
  [(set_attr "length" "8")
   (set_attr "type" "xbfu")])

;; Alternative
;;   emit_insn (gen_arc64_swap (tmpA, operands[1])); swap tmpA op1
;;   emit_insn (gen_arc64_sel_lo (tmpB, operands[1])); bmask tmpB,15
;;   emit_insn (gen_arc64_pack2si (operands[0], tmpB, tmpA)); vpack4hl op0,tmpB,tmpA
(define_expand "vec_unpacku_lo_v4hi"
  [(set (match_operand:V2SI 0 "register_operand")
	(zero_extend:V2SI
	 (vec_select:V2HI
	  (match_operand:V4HI 1 "register_operand")
	  (parallel [(const_int 0)(const_int 1)]))))]
  "TARGET_SIMD && TARGET_64BIT"
 {
   rtx tmpA = gen_reg_rtx (HImode);
   rtx tmpB = gen_reg_rtx (HImode);

   emit_insn (gen_vec_extractv4hi (tmpA, operands[1], GEN_INT (0)));
   emit_insn (gen_vec_extractv4hi (tmpB, operands[1], GEN_INT (1)));
   emit_insn (gen_arc64_vec_concat (operands[0], tmpA, tmpB));
   DONE;
 })

;; Alternative
;;   emit_insn (gen_arc64_swapl (tmp0, operands[1]));
;;   emit_insn (gen_arc64_swap (tmpA, tmp0));
;;   emit_insn (gen_arc64_sel_lo (tmpB, tmp0));
;;   emit_insn (gen_arc64_pack2si (operands[0], tmpB, tmpA));
(define_expand "vec_unpacku_hi_v4hi"
  [(set (match_operand:V2SI 0 "register_operand")
	(zero_extend:V2SI
	 (vec_select:V2HI
	  (match_operand:V4HI 1 "register_operand")
	  (parallel [(const_int 2)(const_int 3)]))))]
  "TARGET_SIMD && TARGET_64BIT"
 {
   rtx tmpA = gen_reg_rtx (HImode);
   rtx tmpB = gen_reg_rtx (HImode);

   emit_insn (gen_vec_extractv4hi (tmpA, operands[1], GEN_INT (2)));
   emit_insn (gen_vec_extractv4hi (tmpB, operands[1], GEN_INT (3)));
   emit_insn (gen_arc64_vec_concat (operands[0], tmpA, tmpB));
   DONE;
 })

(define_insn "arc64_vec_concat"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(unspec:V2SI [(match_operand:HI 1 "register_operand" "r")
		      (match_operand:HI 2 "register_operand" "r")]
		     ARC64_UNSPEC_VPACK2WL))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_expand "vec_duplicatev4hi"
  [(set (match_operand:V4HI 0 "register_operand")
	(vec_duplicate:V4HI (match_operand:HI 1 "register_operand")))]
 "TARGET_SIMD && TARGET_64BIT"
 {
   rtx tmp = gen_reg_rtx (V2SImode);
   emit_insn (gen_arc64_duplicate_v2hi(tmp, operands[1]));
   emit_insn (gen_arc64_pack4hi(operands[0], tmp, tmp));
   DONE;
 })

(define_insn "arc64_duplicate_v2hi"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(unspec:V2SI [(match_operand:HI 1 "register_operand" "r")
		      (const_int 0)]
		     ARC64_UNSPEC_VPACK4HL))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hl\\t%0,%1,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "vec_duplicatev2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_duplicate:V2SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "vec_shr_<mode>"
  [(set (match_operand:V64I 0 "register_operand" "=r,r")
	(unspec:V64I [(match_operand:V64I 1 "register_operand"  "0,r")
		      (match_operand:SI 2 "immediate_operand" "S12S0,i")]
		     ARC64_UNSPEC_VEC_SHR))]
  "TARGET_SIMD && TARGET_64BIT"
  "asrl\\t%0,%1,%2"
  [(set_attr "length" "4,8")
   (set_attr "type" "asl")])

(define_insn "vec_shl_<mode>"
  [(set (match_operand:V64I 0 "register_operand" "=r,r")
	(unspec:V64I [(match_operand:V64I 1 "register_operand"  "0,r")
		      (match_operand:SI 2 "immediate_operand" "S12S0,i")]
		     ARC64_UNSPEC_VEC_SHL))]
  "TARGET_SIMD && TARGET_64BIT"
  "asll\\t%0,%1,%2"
  [(set_attr "length" "4,8")
   (set_attr "type" "asl")])

;; Patterns used by vect_perm
(define_insn "arc64_dup_lane0v2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_duplicate:V2SI
	 (vec_select:SI
	  (match_operand:V2SI 1 "register_operand" "r")
	  (parallel [(const_int 0)])
	  )))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_dup_lane1v2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_duplicate:V2SI
	 (vec_select:SI
	  (match_operand:V2SI 1 "register_operand" "r")
	  (parallel [(const_int 1)])
	  )))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wm\\t%0,%1,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_sel_lane0_v2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_concat:V2SI
	 (vec_select:SI
	  (match_operand:V2SI 1 "register_operand" "r")
	  (parallel [(const_int 0)]))
	 (vec_select:SI
	  (match_operand:V2SI 2 "register_operand" "r")
	  (parallel [(const_int 0)]))
	 ))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_sel_lane1_v2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_concat:V2SI
	 (vec_select:SI
	  (match_operand:V2SI 1 "register_operand" "r")
	  (parallel [(const_int 1)]))
	 (vec_select:SI
	  (match_operand:V2SI 2 "register_operand" "r")
	  (parallel [(const_int 1)]))
	 ))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wm\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_sel_lane2_0v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	  (vec_select:V2HI
	   (match_operand:V4HI 1 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 2)]))
	  (vec_select:V2HI
	   (match_operand:V4HI 2 "register_operand" "r")
	   (parallel [(const_int 0) (const_int 2)]))))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_sel_lane3_1v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	  (vec_select:V2HI
	   (match_operand:V4HI 1 "register_operand" "r")
	   (parallel [(const_int 1) (const_int 3)]))
	  (vec_select:V2HI
	   (match_operand:V4HI 2 "register_operand" "r")
	   (parallel [(const_int 1) (const_int 3)]))))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack4hm\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_swaplv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(vec_concat:V2SI
	 (vec_select:SI
	  (match_operand:V2SI 1 "register_operand" "r")
	  (parallel [(const_int 1)]))
	 (vec_select:SI
	  (match_dup 1)
	  (parallel [(const_int 0)]))))]
  "TARGET_64BIT"
  "swapl\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "swapl")])

(define_insn "arc64_swapv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(unspec:V4HI
	 [(match_operand:V4HI 1 "register_operand" "r")]
	  ARC64_UNSPEC_SWAP))]
  "TARGET_64BIT"
  "swap\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "swap")])

(define_insn "arc64_swapv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(unspec:V2HI
	 [(match_operand:V2HI 1 "register_operand" "r")]
	  ARC64_UNSPEC_SWAP))]
  ""
  "swap\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "swap")])

(define_insn "arc64_swp_lane0_v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	 (vec_select:V2HI
	  (match_operand:V4HI 1 "register_operand" "r")
	  (parallel [(const_int 0) (const_int 1)]))
	 (vec_select:V2HI
	  (match_operand:V4HI 2 "register_operand" "r")
	  (parallel [(const_int 0) (const_int 1)]))
	 ))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_swp_lane1_v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	 (vec_select:V2HI
	  (match_operand:V4HI 1 "register_operand" "r")
	  (parallel [(const_int 2) (const_int 3)]))
	 (vec_select:V2HI
	  (match_operand:V4HI 2 "register_operand" "r")
	  (parallel [(const_int 2) (const_int 3)]))
	 ))]
  "TARGET_SIMD && TARGET_64BIT"
  "vpack2wm\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "*arc64_vsubadd<mode>3"
    [(set (match_operand:VALL 0 "register_operand" "=r")
	(unspec:VALL [(match_operand:VALL 1 "register_operand" "r")
		      (match_operand:VALL 2 "register_operand" "r")]
		      ARC64_UNSPEC_VSUBADD))]
  "TARGET_SIMD"
  "vsubadd<sfxtab>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vaddsub")])

;; In 64b arches, we miss a shuffle pattern that swaps 16b pairs in a
;; 64b reg.  In 32b arches, we miss a quick way to exchange 2 32b
;; regs.  Hence, no support for v4hi.
(define_expand "cadd90v2si3"
  [(set (match_operand:V2SI 0 "register_operand")
	(unspec:V2SI [(match_operand:V2SI 1 "register_operand")
		      (match_operand:V2SI 2 "register_operand")]
		      ARC64_UNSPEC_VSUBADD))]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmp = gen_reg_rtx (V2SImode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (V2SImode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_SWAPL));
    operands[2] = tmp;
  })

(define_expand "cadd90v2hi3"
  [(set (match_operand:V2HI 0 "register_operand")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand")
		      (match_operand:V2HI 2 "register_operand")]
		      ARC64_UNSPEC_VSUBADD))]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmp = gen_reg_rtx (V2HImode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (V2HImode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_SWAP));
    operands[2] = tmp;
  })

(define_insn "*arc64_vaddsub<mode>3"
    [(set (match_operand:VALL 0 "register_operand" "=r")
	(unspec:VALL [(match_operand:VALL 1 "register_operand" "r")
		      (match_operand:VALL 2 "register_operand" "r")]
		      ARC64_UNSPEC_VADDSUB))]
  "TARGET_SIMD"
  "vaddsub<sfxtab>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vaddsub")])

;; In 64b arches, we miss a shuffle pattern that swaps 16b pairs in a
;; 64b reg.  In 32b arches, we miss a quick way to exchange 2 32b
;; regs.  Hence, no support for v4hi.
(define_expand "cadd270v2si3"
  [(set (match_operand:V2SI 0 "register_operand")
	(unspec:V2SI [(match_operand:V2SI 1 "register_operand")
		      (match_operand:V2SI 2 "register_operand")]
		      ARC64_UNSPEC_VADDSUB))]
  "TARGET_SIMD && TARGET_64BIT"
  {
    rtx tmp = gen_reg_rtx (V2SImode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (V2SImode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_SWAPL));
    operands[2] = tmp;
  })

(define_expand "cadd270v2hi3"
  [(set (match_operand:V2HI 0 "register_operand")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand")
		      (match_operand:V2HI 2 "register_operand")]
		      ARC64_UNSPEC_VADDSUB))]
  "TARGET_SIMD"
  {
    rtx tmp = gen_reg_rtx (V2HImode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (V2HImode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_SWAP));
    operands[2] = tmp;
  })

;; Conversions.
(define_insn "arc64_truncate_lo_v2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(truncate:V2HI (match_operand:V2SI 1 "register_operand" "r")))]
  "TARGET_SIMD && !TARGET_64BIT"
  "vpack2hl\\t%0,%H1,%L1"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

(define_insn "arc64_truncate_hi_v4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=r")
	(vec_concat:V4HI
	 (match_operand:V2HI 1 "register_operand" "0")
	 (truncate:V2HI (match_operand:V2SI 2 "register_operand" "r"))))]
  "TARGET_SIMD && !TARGET_64BIT"
  "vpack2hl\\t%H0,%H2,%L2"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

;; Vector Pack:
;; 32bit:
;;   vpack2hl RTl, RAh, RAl
;;   vpack2hl RTh, RBh, RBl
;; 64bit:
;;   vpack4hl RT, RA, RB
(define_expand "vec_pack_trunc_v2si"
  [(set (match_operand:V4HI 0 "register_operand")
	(vec_concat:V4HI
	 (truncate:V2HI
	  (match_operand:V2SI 1 "register_operand"))
	 (truncate:V2HI
	  (match_operand:V2SI 2 "register_operand"))
	 ))]
  "TARGET_SIMD"
  {
   if (!TARGET_64BIT)
     {
       rtx tmp = gen_reg_rtx (V2HImode);

       emit_insn (gen_arc64_truncate_lo_v2hi (tmp, operands[1]));
       emit_insn (gen_arc64_truncate_hi_v4hi (operands[0], tmp, operands[2]));

       DONE;
     }
  })

(define_insn "vec_pack_trunc_si"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
	 (truncate:HI
	  (match_operand:SI 1 "register_operand" "r"))
	 (truncate:HI
	  (match_operand:SI 2 "register_operand" "r"))
	 ))]
  "TARGET_SIMD"
  "vpack2hl\\t%0,%1,%2"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

(define_insn "vec_duplicatev2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_duplicate:V2HI (match_operand:HI 1 "register_operand" "r")))]
  "TARGET_SIMD"
  "vpack2hl\\t%0,%1,%1"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

(define_insn "arc64_sel_lane0_v2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
	 (vec_select:HI
	  (match_operand:V2HI 1 "register_operand" "r")
	  (parallel [(const_int 0)]))
	 (vec_select:HI
	  (match_operand:V2HI 2 "register_operand" "r")
	  (parallel [(const_int 0)]))
	 ))]
  "TARGET_SIMD"
  "vpack2hl\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_sel_lane1_v2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
	 (vec_select:HI
	  (match_operand:V2HI 1 "register_operand" "r")
	  (parallel [(const_int 1)]))
	 (vec_select:HI
	  (match_operand:V2HI 2 "register_operand" "r")
	  (parallel [(const_int 1)]))
	 ))]
  "TARGET_SIMD"
  "vpack2hm\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vpack")])

(define_insn "arc64_vpack_v2hihi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(vec_concat:V2HI
	 (match_operand:HI 1 "register_operand" "r")
	 (match_operand:HI 2 "register_operand" "r")
	 ))]
  "TARGET_SIMD"
  "vpack2hl\\t%0,%1,%2"
  [(set_attr "type" "vpack")
   (set_attr "length" "4")])

(define_insn "<optab>v2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=r")
	(MINMAX:V2SI (match_operand:V2SI 1 "register_operand" "%r")
                     (match_operand:V2SI 2 "register_operand" "r")))]
  "TARGET_SIMD"
  "v<mntab>2\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "<mntab>")])

(define_insn "*<optab>v2si3_dup"
  [(set (match_operand:V2SI 0 "register_operand" "=r,r")
	(MINMAX:V2SI
	 (vec_duplicate:V2SI
	  (match_operand 1 "vectdup_immediate_operand" "S06S0,S12S0"))
	 (match_operand:V2SI 2 "register_operand" "r,0")))]
  "TARGET_SIMD"
  "v<mntab>2\\t%0,%2,%1"
  [(set_attr "length" "4")
   (set_attr "type" "<mntab>")])

;; -------------------------------------------------------------------
;; FP SIMD instructions
;; -------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:VALLF 0 "nonimmediate_operand")
	(match_operand:VALLF 1 "general_operand"))]
  "ARC64_HAS_FP_BASE"
  "
   if (arc64_prepare_move_operands (operands[0], operands[1], <MODE>mode))
    DONE;
  ")

(define_expand "movmisalign<mode>"
  [(set (match_operand:VALLF 0 "nonimmediate_operand")
	(match_operand:VALLF 1 "general_operand"))]
  "ARC64_HAS_FP_BASE && !STRICT_ALIGNMENT"
  "
   if (arc64_prepare_move_operands (operands[0], operands[1], <MODE>mode))
    DONE;
  ")

(define_insn "*mov<mode>"
  [(set (match_operand:VALLF_64 0 "arc64_dest_operand"  "=w,    w,Ufpms,*r,*w,*r,*r,*Ustor")
	(match_operand:VALLF_64 1 "nonimmediate_operand" "w,Ufpms,    w,*w,*r,*r,*m,*r"))]
  "ARC64_HAS_FP_BASE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   vf<sfxtab>mov\\t%0,%1
   fld<sizef>%U1\\t%0,%1
   fst<sizef>%U0\\t%1,%0
   fmv<fmvftab>2<fmvitab>\\t%0,%1
   fmv<fmvitab>2<fmvftab>\\t%0,%1
   mov<mcctab>\\t%0,%1
   ld<slfp>%U1\\t%0,%1
   st<slfp>%U0\\t%1,%0"
  [(set_attr "type" "fmov,ld,st,move,move,move,ld,st")
   (set_attr "length" "4,*,*,4,4,4,*,*")])

;; The 128 bit moves need special care.
(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VALLF_128 0 "arc64_fsimd_moperand" "=w,    w,Ufpms,*r,*w")
	(match_operand:VALLF_128 1 "arc64_fsimd_moperand"  "w,Ufpms,    w,*w,*r"))]
  "ARC64_HAS_FP_BASE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   vf<sfxtab>mov\\t%0,%1
   fld<sizef>%U1\\t%0,%1
   fst<sizef>%U0\\t%1,%0
   #
   #"
  "&& reload_completed
   && arc64_split_double_move_p (operands, <MODE>mode)"
  [(const_int 0)]
  {
   arc64_split_double_move (operands, <MODE>mode);
   DONE;
  }
  [(set_attr "type" "fmov,ld,st,move,move")
   (set_attr "length" "4,*,*,8,8")])

(define_insn "<optab><mode>3"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register"            "=w")
	(VOPS:VALLF (match_operand:VALLF 1 "arc64_fsimd_register" "w")
		    (match_operand:VALLF 2 "arc64_fsimd_register" "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab><mntab>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vf<mntab>")])

;; We need a neg pattern (observed in specInt2006 481.wrf)
(define_expand "neg<mode>2"
  [(set (match_operand:V1FRF 0 "arc64_fsimd_register" "=w")
	(neg:V1FRF (match_operand:V1FRF 1 "arc64_fsimd_register" "w")))]
  "ARC64_HAS_FP_BASE"
  "{
    rtx tmp = gen_reg_rtx (<VEL>mode);
    emit_move_insn (tmp, CONST0_RTX (<VEL>mode));
    emit_insn (gen_vfnmadds<mode> (operands[0], operands[1],
				   tmp, operands[1]));
    DONE;
  }")

(define_insn "vec_duplicate<mode>"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(vec_duplicate:VALLF (match_operand:<VEL> 1 "register_operand" "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>rep\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vfrep")])

(define_insn "<optab><mode>3_rep"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(VOPS:VALLF
	 (match_operand:VALLF 1 "arc64_fsimd_register" "w")
	 (vec_duplicate:VALLF
	  (match_operand:<VEL> 2 "register_operand" "w"))))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab><mntab>s\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vf<mntab>")])

;; Canonical of the above (selected) patterns.
(define_insn "<optab><mode>3_rep2"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(VCOP:VALLF
	 (vec_duplicate:VALLF
	  (match_operand:<VEL> 1 "register_operand" "w"))
	 (match_operand:VALLF 2 "arc64_fsimd_register" "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab><mntab>s\\t%0,%2,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vf<mntab>")])

(define_expand "vec_set<mode>"
  [(set (match_operand:VALLF 0 "register_operand")
	(vec_merge:VALLF
	 (vec_duplicate:VALLF
	  (match_operand:<VEL> 1 "register_operand"))
	 (match_dup 0)
	 (match_operand:SI 2 "immediate_operand")))]
  "ARC64_HAS_FP_BASE"
  {
   HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << INTVAL (operands[2]);
   operands[2] = GEN_INT (elem);
  })

(define_insn "*vec_set<mode>"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(vec_merge:VALLF
	 (vec_duplicate:VALLF
	  (match_operand:<VEL> 1 "register_operand" "w"))
	 (match_operand:VALLF 3 "arc64_fsimd_register" "0")
	 (match_operand:SI 2 "immediate_operand" "i")))]
  "ARC64_HAS_FP_BASE"
 {
   int elt = exact_log2 (INTVAL (operands[2]));
   gcc_assert (UNSIGNED_INT5 (elt));
   operands[2] = GEN_INT (elt);
   return  "vf<sfxtab>ins\\t%0[%2],%1";
 }
  [(set_attr "length" "4")
   (set_attr "type" "vfins")])

(define_insn "vec_extract<mode><vel>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(vec_select:<VEL> (match_operand:VALLF 1 "arc64_fsimd_register" "w")
			  (parallel [(match_operand:SI 2 "const_int_operand" "U05S0")])))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>ext\\t%0,%1[%2]"
  [(set_attr "length" "4")
   (set_attr "type" "vfext")])

;; FV<P>MADD
(define_insn "fma<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w")
		   (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
		   (match_operand:VALLF 3 "arc64_fsimd_register"  "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>madd\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fmadd")])

;; FV<P>MSUB
(define_insn "fnma<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (neg:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w"))
		   (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
		   (match_operand:VALLF 3 "arc64_fsimd_register"  "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>msub\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fmsub")])

(define_insn "fms<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w")
		   (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
		   (neg:VALLF (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode) && ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmsub\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmsub")])

;; -(op3 - (op1 * op2))
(define_insn "*nfnms<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(neg:VALLF (fma:VALLF (neg:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w"))
			      (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
			      (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmsub\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmsub")])

;; FV<P>NMADD
(define_insn "fnms<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (neg:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w"))
		   (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
		   (neg:VALLF (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode) && ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmadd\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmadd")])

;; -(op3 + (op1 * op2))
(define_insn "*nfms<mode>4"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(neg:VALLF (fma:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w")
			      (match_operand:VALLF 2 "arc64_fsimd_register"  "w")
			      (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmadd\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmadd")])

;; FV<P>SQRT
(define_insn "sqrt<mode>2"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(sqrt:VALLF (match_operand:VALLF 1 "arc64_fsimd_register" "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>sqrt\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "fsqrt")])

;; FV<P>MADDS
(define_insn "fma<mode>4_rep"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w")
		   (vec_duplicate:VALLF
		    (match_operand:<VEL> 2 "register_operand"  "w"))
		   (match_operand:VALLF 3 "arc64_fsimd_register"  "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>madds\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fmadd")])

(define_peephole2
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "")
	(vec_duplicate:VALLF (match_operand:<VEL> 1 "register_operand"  "")))
   (set (match_operand:VALLF 2 "arc64_fsimd_register" "")
	(fma:VALLF (match_operand:VALLF 3 "arc64_fsimd_register" "")
		   (match_dup 0)
		   (match_operand:VALLF 4 "arc64_fsimd_register" "")))]
  "ARC64_HAS_FP_BASE
   && peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 2)
	(fma:VALLF (match_dup 3) (vec_duplicate:VALLF (match_dup 1))
		   (match_dup 4)))]
  "")

;; FV<P>MSUBS
(define_insn "fnma<mode>4_rep"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(fma:VALLF (neg:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w"))
		   (vec_duplicate:VALLF
		    (match_operand:<VEL> 2 "register_operand"  "w"))
		   (match_operand:VALLF 3 "arc64_fsimd_register"  "w")))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>msubs\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fmsub")])

(define_peephole2
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "")
	(vec_duplicate:VALLF (match_operand:<VEL> 1 "register_operand"  "")))
   (set (match_operand:VALLF 2 "arc64_fsimd_register" "")
	(fma:VALLF (neg:VALLF (match_operand:VALLF 3 "arc64_fsimd_register" ""))
		   (match_dup 0)
		   (match_operand:VALLF 4 "arc64_fsimd_register" "")))]
  "ARC64_HAS_FP_BASE
   && peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 2)
	(fma:VALLF (neg:VALLF (match_dup 3)) (vec_duplicate:VALLF (match_dup 1))
		   (match_dup 4)))]
  "")

;; FV<P>NMADDS
(define_insn "vfnmadds<mode>"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(neg:VALLF
	 (fma:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w")
		    (vec_duplicate:VALLF
		     (match_operand:<VEL> 2 "register_operand"  "w"))
		    (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmadds\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmadd")])

;; FV<P>NMSUBS
(define_insn "vfnmsubs<mode>"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(neg:VALLF
	 (fma:VALLF (neg:VALLF (match_operand:VALLF 1 "arc64_fsimd_register"  "w"))
		    (vec_duplicate:VALLF
		     (match_operand:<VEL> 2 "register_operand"  "w"))
		    (match_operand:VALLF 3 "arc64_fsimd_register"  "w"))))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>nmsubs\\t%0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "fnmsub")])

;; Exchange unspecs used for reduction ops.
(define_insn "arc64_dexch<mode>"
  [(set (match_operand:VALLF_128 0 "register_operand" "=w")
	(unspec:VALLF_128 [(match_operand:VALLF_128 1 "register_operand" "w")]
			  ARC64_UNSPEC_DEXCH))]
  "ARC64_HAS_FP_BASE"
  "vfdexch\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vfexch")])

(define_insn "arc64_sexch<mode>"
  [(set (match_operand:V1FRF 0 "register_operand" "=w")
	(unspec:V1FRF [(match_operand:V1FRF 1 "register_operand" "w")]
		      ARC64_UNSPEC_SEXCH))]
  "ARC64_HAS_FP_BASE"
  "vfsexch\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vfexch")])

(define_insn "arc64_hexch<mode>"
  [(set (match_operand:VxHF 0 "register_operand" "=w")
	(unspec:VxHF [(match_operand:VxHF 1 "register_operand" "w")]
		     ARC64_UNSPEC_HEXCH))]
  "ARC64_HAS_FP_BASE"
  "vfhexch\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vfexch")])

(define_expand "reduc_plus_scal_v8hf"
  [(match_operand:HF 0 "register_operand")
   (match_operand:V8HF 1 "register_operand")]
  ""
  {
    rtx low = gen_lowpart (HFmode, operands[1]);
    rtx high = gen_reg_rtx (HFmode);
    rtx tmp0, tmp1, tmp2, tmp3;

    tmp0 = gen_reg_rtx (V8HFmode);
    tmp1 = gen_reg_rtx (V8HFmode);
    tmp2 = gen_reg_rtx (V8HFmode);
    tmp3 = gen_reg_rtx (V8HFmode);

    /* 1/2 of the vector.  */
    emit_insn (gen_arc64_dexchv8hf (tmp0, operands[1]));
    emit_insn (gen_addv8hf3 (tmp1, tmp0, operands[1]));

    /* 1/4 of the vector.  */
    emit_insn (gen_arc64_sexchv8hf (tmp2, tmp1));
    emit_insn (gen_addv8hf3 (tmp3, tmp2, tmp1));

    /* Last 2 elements.  */
    emit_insn (gen_vec_extractv8hfhf (high, tmp3, GEN_INT (1)));
    emit_insn (gen_addhf3 (operands[0], high, low));
    DONE;
  })

;; Vector reduction instructions (emulated)
(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V2xF 1 "register_operand")]
  ""
  {
    rtx low = gen_lowpart (<VEL>mode, operands[1]);
    rtx high = gen_reg_rtx (<VEL>mode);

    emit_insn (gen_vec_extract<mode><vel> (high, operands[1], GEN_INT (1)));
    emit_insn (gen_add<vel>3 (operands[0], high, low));
    DONE;
  })

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:V4xF 1 "register_operand")]
  ""
  {
#if 0
    rtx op0 = gen_lowpart (<VEL>mode, operands[1]);
    rtx op1 = gen_reg_rtx (<VEL>mode);
    rtx op2 = gen_reg_rtx (<VEL>mode);
    rtx op3 = gen_reg_rtx (<VEL>mode);
    rtx tmp1 = gen_reg_rtx (<VEL>mode);
    rtx tmp2 = gen_reg_rtx (<VEL>mode);

    emit_insn (gen_vec_extract<mode><vel> (op1, operands[1], GEN_INT (1)));
    emit_insn (gen_add<vel>3 (tmp1, op1, op0));

    if (<MODE>mode == V4SFmode)
      op2 = gen_lowpart (SFmode, gen_highpart (DFmode, operands[1]));
    else
      emit_insn (gen_vec_extract<mode><vel> (op2, operands[1], GEN_INT (2)));

    emit_insn (gen_vec_extract<mode><vel> (op3, operands[1], GEN_INT (3)));
    emit_insn (gen_add<vel>3 (tmp2, op2, op3));

    emit_insn (gen_add<vel>3 (operands[0], tmp1, tmp2));
    DONE;
#else
    rtx low = gen_lowpart (<VEL>mode, operands[1]);
    rtx high = gen_reg_rtx (<VEL>mode);
    rtx tmp0, tmp1;

    tmp0 = gen_reg_rtx (<MODE>mode);
    tmp1 = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_arc64_<fmextab>exch<mode> (tmp0, operands[1]));
    emit_insn (gen_add<mode>3 (tmp1, tmp0, operands[1]));

    emit_insn (gen_vec_extract<mode><vel> (high, tmp1, GEN_INT (1)));
    emit_insn (gen_add<vel>3 (operands[0], high, low));
    DONE;
#endif
  })

;; Emulated vector ops using scalar function, only for double width vectors.
;; MAX/MIN
(define_insn_and_split "<optab><mode>3"
  [(set (match_operand:W2xF 0 "arc64_fsimd_register" "=w")
	(MINMAX:W2xF (match_operand:W2xF 1 "arc64_fsimd_register" "w")
		     (match_operand:W2xF 2 "arc64_fsimd_register" "w")))]
  "ARC64_VFP_128"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
   rtx high_dest = gen_highpart (<VEL>mode, operands[0]);
   rtx low_dest = gen_lowpart (<VEL>mode, operands[0]);
   rtx high_op1 = gen_highpart (<VEL>mode, operands[1]);
   rtx low_op1 = gen_lowpart (<VEL>mode, operands[1]);
   rtx high_op2 = gen_highpart (<VEL>mode, operands[2]);
   rtx low_op2 = gen_lowpart (<VEL>mode, operands[2]);
   emit_insn (gen_<optab><vel>3 (low_dest, low_op1,  low_op2));
   emit_insn (gen_<optab><vel>3 (high_dest, high_op1,  high_op2));
   DONE;
  }
  [(set_attr "length" "8")
   (set_attr "type" "f<mntab>")])

;; NEG/ABS
(define_insn_and_split "<optab><mode>2"
  [(set (match_operand:W2xF 0 "arc64_fsimd_register" "=w")
	(ABS_NEG:W2xF (match_operand:W2xF 1 "arc64_fsimd_register" "w")))]
  "ARC64_VFP_128"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
   rtx high_dest = gen_highpart (<VEL>mode, operands[0]);
   rtx low_dest = gen_lowpart (<VEL>mode, operands[0]);
   rtx high_op1 = gen_highpart (<VEL>mode, operands[1]);
   rtx low_op1 = gen_lowpart (<VEL>mode, operands[1]);
   emit_insn (gen_<optab><vel>2 (low_dest, low_op1));
   emit_insn (gen_<optab><vel>2 (high_dest, high_op1));
   DONE;
  }
  [(set_attr "length" "8")
   (set_attr "type" "fsgnjn")])

;; Conversions.
(define_expand "vec_pack_trunc_v2df"
  [(set (match_operand:V4SF 0 "register_operand")
      (vec_concat:V4SF
	(float_truncate:V2SF
	    (match_operand:V2DF 1 "register_operand"))
	(float_truncate:V2SF
	    (match_operand:V2DF 2 "register_operand"))
	  ))]
  "ARC64_VFP_128"
  {
    rtx high_dest = gen_lowpart (SFmode,
				 gen_highpart (DFmode,
					      operands[0]));
    rtx low_dest = gen_lowpart (SFmode, operands[0]);

    rtx high_op1 = gen_highpart (DFmode, operands[1]);
    rtx low_op1 = gen_lowpart (DFmode, operands[1]);
    rtx high_op2 = gen_highpart (DFmode, operands[2]);
    rtx low_op2 = gen_lowpart (DFmode, operands[2]);
    rtx tmp1 = gen_reg_rtx (SFmode);
    rtx tmp3 = gen_reg_rtx (SFmode);

    emit_insn (gen_truncdfsf2 (tmp3, high_op1));
    emit_insn (gen_truncdfsf2 (high_dest, low_op1));
    emit_insn (gen_truncdfsf2 (tmp1, high_op2));
    emit_insn (gen_truncdfsf2 (low_dest, low_op2));

    emit_insn (gen_vec_setv4sf (operands[0], tmp1, GEN_INT (1)));
    emit_insn (gen_vec_setv4sf (operands[0], tmp3, GEN_INT (3)));
    DONE;
  })

(define_expand "vec_pack_trunc_df"
  [(set (match_operand:V2SF 0 "register_operand")
      (vec_concat:V2SF
	(float_truncate:SF
	    (match_operand:DF 1 "register_operand"))
	(float_truncate:SF
	    (match_operand:DF 2 "register_operand"))
	  ))]
 "ARC64_VFP_64"
 {
    rtx low_dest = gen_lowpart (SFmode, operands[0]);
    rtx tmp1 = gen_reg_rtx (SFmode);

    emit_insn (gen_truncdfsf2 (low_dest, operands[2]));
    emit_insn (gen_truncdfsf2 (tmp1, operands[1]));
    emit_insn (gen_vec_setv2sf (operands[0], tmp1, GEN_INT (1)));
    DONE;
 })

;; vec_load_lanes used when wide_simd is off and wide_ldst is
;; on. Hence the simd lengthis 64bit

;; Patterns used to vect permutate.

;; This one pattern is only used when we don't want to make
;; dup_permutations using vec_dup (see arc64_simd_dup).
(define_insn "arc64_dup_lane0<mode>"
  [(set (match_operand:VALLF 0 "arc64_fsimd_register" "=w")
	(vec_duplicate:VALLF
	 (vec_select:<VEL>
	  (match_operand:VALLF 1 "arc64_fsimd_register" "w")
	  (parallel [(const_int 0)])
	  )))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>rep\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vfrep")])

(define_insn "arc64_dup_lane1<mode>"
  [(set (match_operand:W2xF 0 "arc64_fsimd_register" "=w")
	(vec_duplicate:W2xF
	 (vec_select:<VEL>
	  (match_operand:W2xF 1 "arc64_fsimd_register" "w")
	  (parallel [(const_int 1)])
	  )))]
  "ARC64_VFP_128"
  "vf<sfxtab>rep\\t%0,%H1"
  [(set_attr "length" "4")
   (set_attr "type" "vfrep")])

;; Shuffle patterns
(define_insn "arc64_d<PERMUTED:perm_pat><mode>"
  [(set (match_operand:VALLF_128 0 "register_operand" "=w")
	(unspec:VALLF_128 [(match_operand:VALLF_128 1 "register_operand" "w")
			   (match_operand:VALLF_128 2 "register_operand" "w")]
			  PERMUTED))]
  "ARC64_HAS_FP_BASE"
  "vfd<PERMUTED:perm_pat>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vf<PERMUTED:perm_pat>")])

(define_insn "arc64_s<PERMUTES:perm_pat><mode>"
  [(set (match_operand:V1FRF 0 "register_operand" "=w")
	(unspec:V1FRF [(match_operand:V1FRF 1 "register_operand" "w")
		       (match_operand:V1FRF 2 "register_operand" "w")]
		      PERMUTES))]
  "ARC64_HAS_FP_BASE"
  "vfs<PERMUTES:perm_pat>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vf<PERMUTES:perm_pat>")])

(define_insn "arc64_h<PERMUTEH:perm_pat><mode>"
  [(set (match_operand:VxHF 0 "register_operand" "=w")
	(unspec:VxHF [(match_operand:VxHF 1 "register_operand" "w")
		      (match_operand:VxHF 2 "register_operand" "w")]
		      PERMUTEH))]
  "ARC64_HAS_FP_BASE"
  "vfh<PERMUTEH:perm_pat>\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vf<PERMUTEH:perm_pat>")])

;; Required pattern needed for vector reduction operations.
;;(define_expand "vec_shr_<mode>"
;;  [(match_operand:VALLF 0 "register_operand")
;;   (match_operand:VALLF 1 "register_operand")
;;   (match_operand:SI 2 "immediate_operand")]
;;  "ARC64_HAS_FP_BASE"
;;  {
;;   if (arc64_expand_fvect_shr (operands))
;;      DONE;
;;   FAIL;
;;  })

;;(define_insn "vec_shr_<mode>"
;;  [(set (match_operand:VALLF 0 "arc64_fsimd_moperand" "=w")
;;	(unspec:VALLF [(match_operand:VALLF 1 "arc64_fsimd_moperand" "w")
;;		       (match_operand:SI 2 "immediate_operand")]
;;		      ARC64_UNSPEC_VEC_SHR))]
;;  "ARC64_HAS_FP_BASE"
;;  "vfasrl\\t%0,%1,%2"
;;  [(set_attr "length" "4")
;;   (set_attr "type" "asl")])


(define_insn "*arc64_vfsubadd<mode>3"
  [(set (match_operand:VALLF 0 "register_operand" "=w")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand" "w")
		       (match_operand:VALLF 2 "register_operand" "w")]
		      ARC64_UNSPEC_VFSUBADD))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>subadd\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vfsubadd")])

(define_expand "cadd90<mode>3"
  [(set (match_operand:VALLF 0 "register_operand")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand")
		       (match_operand:VALLF 2 "register_operand")]
		      ARC64_UNSPEC_VFSUBADD))]
  "ARC64_HAS_FP_BASE"
 {
    rtx tmp = gen_reg_rtx (<MODE>mode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (<MODE>mode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_<cplxtab>EXCH));
    operands[2] = tmp;
 })

(define_insn "*arc64_vfaddsub<mode>3"
  [(set (match_operand:VALLF 0 "register_operand" "=w")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand" "w")
		       (match_operand:VALLF 2 "register_operand" "w")]
		      ARC64_UNSPEC_VFADDSUB))]
  "ARC64_HAS_FP_BASE"
  "vf<sfxtab>addsub\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vfaddsub")])

(define_expand "cadd270<mode>3"
  [(set (match_operand:VALLF 0 "register_operand")
	(unspec:VALLF [(match_operand:VALLF 1 "register_operand")
		       (match_operand:VALLF 2 "register_operand")]
		      ARC64_UNSPEC_VFADDSUB))]
  "ARC64_HAS_FP_BASE"
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);

    emit_move_insn (tmp, gen_rtx_UNSPEC (<MODE>mode,
					 gen_rtvec (1, operands[2]),
					 ARC64_UNSPEC_<cplxtab>EXCH));
    operands[2] = tmp;
  })
