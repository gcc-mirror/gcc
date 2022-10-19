;; ----------------------------------------------------------------------
;; MOVE INSTRUCTIONS
;; ----------------------------------------------------------------------

;; movqi

(define_insn_and_split "*movqi"
  [(set (match_operand:QI 0 "general_operand_dst" "=r,r ,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m")
	(match_operand:QI 1 "general_operand_src" " I,r>,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,n,m,r"))]
  "!TARGET_H8300SX && h8300_move_ok (operands[0], operands[1])"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movqi<cczn>"
  [(set (match_operand:QI 0 "general_operand_dst" "=r,r ,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m")
	(match_operand:QI 1 "general_operand_src" " I,r>,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,n,m,r"))
   (clobber (reg:CC CC_REG))]
  "!TARGET_H8300SX && h8300_move_ok (operands[0], operands[1])"
  "@
   sub.b	%X0,%X0
   mov.b	%R1,%X0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%X1,%R0
   mov.b	%R1,%X0
   mov.b	%R1,%X0
   mov.b	%X1,%R0"
  [(set (attr "length") (symbol_ref "compute_mov_length (operands)"))])

(define_insn_and_split "*movqi_h8sx"
  [(set (match_operand:QI 0 "general_operand_dst" "=Zz,rQ")
	(match_operand:QI 1 "general_operand_src" "P4>X,rQi"))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movqi_h8sx<cczn>"
  [(set (match_operand:QI 0 "general_operand_dst" "=Zz,rQ")
	(match_operand:QI 1 "general_operand_src" "P4>X,rQi"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "@
    mov.b	%X1:4,%X0
    mov.b	%X1,%X0"
  [(set_attr "length_table" "mov_imm4,movb")])

(define_expand "mov<mode>"
  [(set (match_operand:QHSIF 0 "general_operand_dst" "")
	(match_operand:QHSIF 1 "general_operand_src" ""))]
  ""
  {
    enum machine_mode mode = <MODE>mode;
    if (!TARGET_H8300SX)
      {
	/* Other H8 chips, except the H8/SX family can only handle a
	   single memory operand, which is checked by h8300_move_ok.

	   We could perhaps have h8300_move_ok handle the H8/SX better
	   and just remove the !TARGET_H8300SX conditional.  */
	if (!h8300_move_ok (operands[0], operands[1]))
	  operands[1] = copy_to_mode_reg (mode, operand1);
      }
  })

(define_insn_and_split "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "general_operand_dst" "+r,r"))
			 (match_operand:QI 1 "general_operand_src" "I,rmi>"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (strict_low_part (match_dup 0)) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])


(define_insn "*movstrictqi<cczn>"
  [(set (strict_low_part (match_operand:QI 0 "general_operand_dst" "+r,r"))
			 (match_operand:QI 1 "general_operand_src" "I,rmi>"))
   (clobber (reg:CC CC_REG))]
  ""
  "@
   sub.b	%X0,%X0
   mov.b	%X1,%X0"
  [(set_attr "length" "2,*")
   (set_attr "length_table" "*,movb")])

;; movhi

(define_insn_and_split "*movhi"
  [(set (match_operand:HI 0 "general_operand_dst" "=r,r,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m")
	(match_operand:HI 1 "general_operand_src" "I,r>,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,i,m,r"))]
  "!TARGET_H8300SX
    && h8300_move_ok (operands[0], operands[1])"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movhi<cczn>"
  [(set (match_operand:HI 0 "general_operand_dst" "=r,r,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m")
	(match_operand:HI 1 "general_operand_src" "I,r>,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,i,m,r"))
   (clobber (reg:CC CC_REG))]
  "!TARGET_H8300SX
    && h8300_move_ok (operands[0], operands[1])"
  "@
   sub.w	%T0,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0"
  [(set (attr "length") (symbol_ref "compute_mov_length (operands)"))])

(define_insn_and_split "*movhi_h8sx"
  [(set (match_operand:HI 0 "general_operand_dst" "=r,r,Zz,Q,rQ")
	(match_operand:HI 1 "general_operand_src" "I,P3>X,P4>X,IP8>X,rQi"))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])
  
(define_insn "*movhi_h8sx<cczn>"
  [(set (match_operand:HI 0 "general_operand_dst" "=r,r,Zz,Q,rQ")
	(match_operand:HI 1 "general_operand_src" "I,P3>X,P4>X,IP8>X,rQi"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "@
   sub.w	%T0,%T0
   mov.w	%T1:3,%T0
   mov.w	%T1:4,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0"
  [(set_attr "length_table" "*,*,mov_imm4,short_immediate,movw")
   (set_attr "length" "2,2,*,*,*")])

(define_insn_and_split "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "general_operand_dst" "+r,r,r"))
			 (match_operand:HI 1 "general_operand_src" "I,P3>X,rmi"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (strict_low_part (match_dup 0)) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movstricthi<cczn>"
  [(set (strict_low_part (match_operand:HI 0 "general_operand_dst" "+r,r,r"))
			 (match_operand:HI 1 "general_operand_src" "I,P3>X,rmi"))
   (clobber (reg:CC CC_REG))]
  ""
  "@
   sub.w	%T0,%T0
   mov.w	%T1,%T0
   mov.w	%T1,%T0"
  [(set_attr "length" "2,2,*")
   (set_attr "length_table" "*,*,movw")])

;; movsi
(define_insn_and_split "*movsi"
  [(set (match_operand:SI 0 "general_operand_dst" "=r,r,r,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m,*a,*a,r")
	(match_operand:SI 1 "general_operand_src" "I,r,i,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,>,m,r,I,r,*a"))]
  "(TARGET_H8300S || TARGET_H8300H) && !TARGET_H8300SX
    && h8300_move_ok (operands[0], operands[1])"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movsi_clobber_flags"
  [(set (match_operand:SI 0 "general_operand_dst" "=r,r,r,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m,*a,*a, r")
	(match_operand:SI 1 "general_operand_src" " I,r,i,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,>,m,r, I, r,*a"))
   (clobber (reg:CC CC_REG))]
  "(TARGET_H8300S || TARGET_H8300H) && !TARGET_H8300SX
    && h8300_move_ok (operands[0], operands[1])"
{
  switch (which_alternative)
    {
    case 0:
      return "sub.l	%S0,%S0";
    case 14:
      return "clrmac";
    case 15:
      return "clrmac\;ldmac %1,macl";
    case 16:
      return "stmac	macl,%0";
    default:
      if (GET_CODE (operands[1]) == CONST_INT)
	{
	  int val = INTVAL (operands[1]);

	  /* Look for constants which can be made by adding an 8-bit
	     number to zero in one of the two low bytes.  */
	  if (val == (val & 0xff))
	    {
	      operands[1] = GEN_INT ((char) val & 0xff);
	      return "sub.l\\t%S0,%S0\;add.b\\t%1,%w0";
	    }

	  if (val == (val & 0xff00))
	    {
	      operands[1] = GEN_INT ((char) (val >> 8) & 0xff);
	      return "sub.l\\t%S0,%S0\;add.b\\t%1,%x0";
	    }

	  /* Look for constants that can be obtained by subs, inc, and
	     dec to 0.  */
	  switch (val & 0xffffffff)
	    {
	    case 0xffffffff:
	      return "sub.l\\t%S0,%S0\;subs\\t#1,%S0";
	    case 0xfffffffe:
	      return "sub.l\\t%S0,%S0\;subs\\t#2,%S0";
	    case 0xfffffffc:
	      return "sub.l\\t%S0,%S0\;subs\\t#4,%S0";

	    case 0x0000ffff:
	      return "sub.l\\t%S0,%S0\;dec.w\\t#1,%f0";
	    case 0x0000fffe:
	      return "sub.l\\t%S0,%S0\;dec.w\\t#2,%f0";

	    case 0xffff0000:
	      return "sub.l\\t%S0,%S0\;dec.w\\t#1,%e0";
	    case 0xfffe0000:
	      return "sub.l\\t%S0,%S0\;dec.w\\t#2,%e0";

	    case 0x00010000:
	      return "sub.l\\t%S0,%S0\;inc.w\\t#1,%e0";
	    case 0x00020000:
	      return "sub.l\\t%S0,%S0\;inc.w\\t#2,%e0";
	    }
	}
    }
   return "mov.l	%S1,%S0";
}
  [(set (attr "length") (symbol_ref "compute_mov_length (operands)"))])

(define_insn "*movsi_cczn"
  [(set (reg:CCZN CC_REG)
	(compare:CCZN
	  (match_operand:SI 1 "general_operand_src" " I,r,i,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,>,m,r")
	  (const_int 0)))
   (set (match_operand:SI 0 "general_operand_dst" "=r,r,r,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r,r,m")
	(match_dup 1))]
  "(TARGET_H8300S || TARGET_H8300H) && !TARGET_H8300SX
    && h8300_move_ok (operands[0], operands[1])"
  "@
   sub.l	%S0,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0"
  [(set (attr "length") (symbol_ref "compute_mov_length (operands)"))])

(define_insn_and_split "*movsi_h8sx"
  [(set (match_operand:SI 0 "general_operand_dst" "=r,r,Q,rQ,*a,*a,r")
	(match_operand:SI 1 "general_operand_src" "I,P3>X,IP8>X,rQi,I,r,*a"))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movsi_h8sx_clobber_flags"
  [(set (match_operand:SI 0 "general_operand_dst" "=r,r,Q,rQ,*a,*a,r")
	(match_operand:SI 1 "general_operand_src" "I,P3>X,IP8>X,rQi,I,r,*a"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "@
   sub.l	%S0,%S0
   mov.l	%S1:3,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   clrmac
   clrmac\;ldmac	%1,macl
   stmac	macl,%0"
  [(set_attr "length_table" "*,*,short_immediate,movl,*,*,*")
   (set_attr "length" "2,2,*,*,2,6,4")])

(define_insn "*movsi_h8sx_ccnz"
  [(set (reg:CCZN CC_REG)
	(compare:CCZN
	  (match_operand:SI 1 "general_operand_src" "I,P3>X,IP8>X,rQi")
	  (const_int 0)))
   (set (match_operand:SI 0 "general_operand_dst" "=r,r,Q,rQ")
	(match_dup 1))]
  "TARGET_H8300SX"
  "@
   sub.l	%S0,%S0
   mov.l	%S1:3,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0"
  [(set_attr "length_table" "*,*,short_immediate,movl")
   (set_attr "length" "2,2,*,*")])

(define_insn_and_split "*movsf_h8sx"
  [(set (match_operand:SF 0 "general_operand_dst" "=r,rQ")
	(match_operand:SF 1 "general_operand_src" "G,rQi"))]
  "TARGET_H8300SX"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movsf_h8sx_clobber_flags"
  [(set (match_operand:SF 0 "general_operand_dst" "=r,rQ")
	(match_operand:SF 1 "general_operand_src" "G,rQi"))
   (clobber (reg:CC CC_REG))]
  "TARGET_H8300SX"
  "@
    sub.l	%S0,%S0
    mov.l	%S1,%S0"
  [(set_attr "length" "2,*")
   (set_attr "length_table" "*,movl")])

(define_insn_and_split "*movsf"
  [(set (match_operand:SF 0 "general_operand_dst" "=r,r,r,m,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r")
	(match_operand:SF 1 "general_operand_src" "G,r,im,r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,>"))]
  "!TARGET_H8300SX
    && (register_operand (operands[0], SFmode)
	|| register_operand (operands[1], SFmode))"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*movsf_clobber_flags"
  [(set (match_operand:SF 0 "general_operand_dst" "=r,r,r,m,Za,Zb,Zc,Zd,Ze,Zf,Zg,Zh,r")
	(match_operand:SF 1 "general_operand_src" "G,r,im,r,Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7,>"))
   (clobber (reg:CC CC_REG))]
  "!TARGET_H8300SX
    && (register_operand (operands[0], SFmode)
	|| register_operand (operands[1], SFmode))"
  "@
   sub.l	%S0,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0
   mov.l	%S1,%S0"
  [(set (attr "length") (symbol_ref "compute_mov_length (operands)"))])

;; ----------------------------------------------------------------------
;; PUSH INSTRUCTIONS
;; ----------------------------------------------------------------------

(define_insn_and_split "*push1_<QHI:mode>"
  [(set (mem:QHI
	(pre_modify:P
	  (reg:P SP_REG)
	  (plus:P (reg:P SP_REG) (const_int -4))))
	(match_operand:QHI 0 "register_no_sp_elim_operand" "r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (mem:QHI
		     (pre_modify:P (reg:P SP_REG)
				   (plus:P (reg:P SP_REG) (const_int -4))))
		   (match_dup 0))
	      (clobber (reg:CC CC_REG))])])

(define_insn "*push1_<QHI:mode><cczn>"
  [(set (mem:QHI
	(pre_modify:P
	  (reg:P SP_REG)
	  (plus:P (reg:P SP_REG) (const_int -4))))
	(match_operand:QHI 0 "register_no_sp_elim_operand" "r"))
   (clobber (reg:CC CC_REG))]
  ""
  "mov.l\\t%S0,@-er7"
  [(set_attr "length" "4")])

