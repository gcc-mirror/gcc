;; ----------------------------------------------------------------------
;; TEST INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn_and_split "*tst_extzv_1_n"
  [(set (cc0)
	(compare (zero_extract:SI (match_operand:QI 0 "general_operand_src" "r,U,mn>")
				  (const_int 1)
				  (match_operand 1 "const_int_operand" "n,n,n"))
		 (const_int 0)))
   (clobber (match_scratch:QI 2 "=X,X,&r"))]
  "!CONSTANT_P (operands[0])"
  "@
   btst\\t%Z1,%Y0
   btst\\t%Z1,%Y0
   #"
  "&& reload_completed
   && !satisfies_constraint_U (operands[0])"
  [(set (match_dup 2)
	(match_dup 0))
   (parallel [(set (cc0) (compare (zero_extract:SI (match_dup 2)
						   (const_int 1)
						   (match_dup 1))
				  (const_int 0)))
	      (clobber (scratch:QI))])]
  ""
  [(set_attr "length" "2,8,10")
   (set_attr "cc" "set_zn,set_zn,set_zn")])

(define_insn ""
  [(set (cc0)
	(compare (zero_extract:HSI (match_operand:HSI 0 "register_operand" "r")
				   (const_int 1)
				   (match_operand 1 "const_int_operand" "n"))
		 (const_int 0)))]
  "INTVAL (operands[1]) <= 15"
  "btst	%Z1,%Y0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_zn")])

(define_insn_and_split "*tstsi_upper_bit"
  [(set (cc0)
	(compare (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
				  (const_int 1)
				  (match_operand 1 "const_int_operand" "n"))
		 (const_int 0)))
   (clobber (match_scratch:SI 2 "=&r"))]
  "INTVAL (operands[1]) >= 16"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ior:SI (and:SI (match_dup 2)
			(const_int -65536))
		(lshiftrt:SI (match_dup 0)
			     (const_int 16))))
   (set (cc0)
	(compare (zero_extract:SI (match_dup 2)
				  (const_int 1)
				  (match_dup 3))
		 (const_int 0)))]
  {
    operands[3] = GEN_INT (INTVAL (operands[1]) - 16);
  })

(define_insn "*tstsi_variable_bit"
  [(set (cc0)
	(compare (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
				  (const_int 1)
				  (and:SI (match_operand:SI 1 "register_operand" "r")
					  (const_int 7)))
		 (const_int 0)))]
  ""
  "btst	%w1,%w0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_zn")])

(define_insn_and_split "*tstsi_variable_bit_qi"
  [(set (cc0)
	(compare (zero_extract:SI (zero_extend:SI (match_operand:QI 0 "general_operand_src" "r,U,mn>"))
				  (const_int 1)
				  (and:SI (match_operand:SI 1 "register_operand" "r,r,r")
					  (const_int 7)))
		 (const_int 0)))
   (clobber (match_scratch:QI 2 "=X,X,&r"))]
  "!CONSTANT_P (operands[0])"
  "@
   btst\\t%w1,%X0
   btst\\t%w1,%X0
   #"
  "&& reload_completed
   && !satisfies_constraint_U (operands[0])"
  [(set (match_dup 2)
	(match_dup 0))
   (parallel [(set (cc0)
		   (compare (zero_extract:SI (zero_extend:SI (match_dup 2))
					     (const_int 1)
					     (and:SI (match_dup 1)
						     (const_int 7)))
			    (const_int 0)))
	      (clobber (scratch:QI))])]
  ""
  [(set_attr "length" "2,8,10")
   (set_attr "cc" "set_zn,set_zn,set_zn")])

(define_insn "*tst<mode>"
  [(set (cc0)
	(compare (match_operand:QHI 0 "register_operand" "r")
		 (const_int 0)))]
  ""
  {
    if (<MODE>mode == QImode)
      return "mov.b	%X0,%X0";
    else if (<MODE>mode == HImode)
      return "mov.w	%T0,%T0";
    gcc_unreachable ();
  }
  [(set_attr "length" "2")
   (set_attr "cc" "set_znv")])

(define_insn "*tsthi_upper"
  [(set (cc0)
	(compare (and:HI (match_operand:HI 0 "register_operand" "r")
			 (const_int -256))
		 (const_int 0)))]
  ""
  "mov.b	%t0,%t0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_znv")])

(define_insn "*tstsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "r")
		 (const_int 0)))]
  ""
  "mov.l	%S0,%S0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_znv")])

(define_insn "*tstsi_upper"
  [(set (cc0)
	(compare (and:SI (match_operand:SI 0 "register_operand" "r")
			 (const_int -65536))
		 (const_int 0)))]
  ""
  "mov.w	%e0,%e0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_znv")])

(define_insn "*cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "h8300_dst_operand" "rQ")
		 (match_operand:QI 1 "h8300_src_operand" "rQi")))]
  ""
  "cmp.b	%X1,%X0"
  [(set_attr "length_table" "add")
   (set_attr "cc" "compare")])

(define_insn "*cmphi_h8300hs_znvc"
  [(set (cc0)
	(compare (match_operand:HI 0 "h8300_dst_operand" "rU,rQ")
		 (match_operand:HI 1 "h8300_src_operand" "P3>X,rQi")))]
  ""
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
  [(set_attr "length_table" "short_immediate,add")
   (set_attr "cc" "compare,compare")])

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "h8300_dst_operand" "r,rQ")
		 (match_operand:SI 1 "h8300_src_operand" "P3>X,rQi")))]
  ""
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
   (set_attr "length_table" "*,add")
   (set_attr "cc" "compare,compare")])
