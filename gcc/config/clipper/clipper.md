;;- Machine description for GNU compiler, Clipper Version
;;   Copyright (C) 1987, 88, 91, 93, 94, 1997 Free Software Foundation, Inc.
;; Contributed by Holger Teutsch (holger@hotbso.rhein-main.de)

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;;
;; define attributes
;;
;; instruction type
;;
;; unknown is temporary in order to generate 'cc clobber' until attribute
;; assignment is consistent
;;
(define_attr "type" "load,store,arith,fp,branch,unknown"
 (const_string "unknown"))

;; condition code setting
;;
;; clobber	destroyed
;; unchanged
;; set1		set cc_status.value1, e.g. sub r0,r1
;; set2		set value1 and value2, e.g. mov r0,r1
;; change0	may be side effect, i.e. load mem,r0
;;
;; note: loadi and loadq are 'arith' instructions that set the condition codes
;;       mul,div,mod do NOT set the condition codes
;;
(define_attr "cc" "clobber,unchanged,set1,set2,change0"
 (cond [(eq_attr "type" "load")	(const_string "change0")
	(eq_attr "type" "store,branch") (const_string "unchanged")
	(eq_attr "type" "arith") (if_then_else (match_operand:SI 0 "" "")
				  (const_string "set1")
				  (const_string "clobber"))
	]
  (const_string "clobber")))

;;
;; clipper seems to be a traditional risc processor
;; we define a functional unit 'memory'
;;
(define_function_unit "memory" 1 1 (eq_attr "type" "load") 4 0)     


;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "int_reg_operand" "r"))]
  ""
  "cmpq   $0,%0")

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "nonimmediate_operand" "r,r,n")
		 (match_operand:SI 1 "nonmemory_operand" "r,n,r")))]
  ""
  "*
{
  int val;

  if (which_alternative == 0)
    return \"cmpw   %1,%0\";

  if (which_alternative == 1)
    {
      val = INTVAL (operands[1]);
      if (0 <= val && val < 16)
	return \"cmpq   %1,%0\";
      return \"cmpi   %1,%0\";
    }

  cc_status.flags |= CC_REVERSED;	/* immediate must be first */

  val = INTVAL (operands[0]);

  if (0 <= val && val < 16)
    return \"cmpq   %0,%1\";

  return \"cmpi   %0,%1\";
}")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "fp_reg_operand" "f")
		 (match_operand:DF 1 "fp_reg_operand" "f")))]
  ""
  "cmpd   %1,%0")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "fp_reg_operand" "f")
		 (match_operand:SF 1 "fp_reg_operand" "f")))]
  ""
  "cmps   %1,%0")


;;
;; double and single float move
;;
(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	operands[1] = force_reg (DFmode,
				 force_const_mem (DFmode, operands[1]));
      else if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (DFmode, operands[1]);
    }

  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = force_const_mem (DFmode, operands[1]);
}")

;;
;; provide two patterns with different predicates as we don't want combine
;; to recombine a mem -> mem move
;; 
(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=*rf")
	(match_operand:DF 1 "nonimmediate_operand" "*rfo"))]
  ""
  "*
{
#define FP_REG_P(X) (GET_CODE (X) == REG && REGNO (X) >= 16)

  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))	/* f -> f */
	return \"movd   %1,%0\";

      if (GET_CODE (operands[1]) == REG) /* r -> f */
	return \"movld  %1,%0\";

      return \"loadd  %1,%0\";		/* m -> f */
    }

  if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG) /* f -> r */
	return \"movdl  %1,%0\";

      abort ();
    }

  if (GET_CODE (operands[1]) == MEM)	/* m -> r */
    {
      rtx xops[4];
      xops[0] = operands[0];
      xops[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
      xops[2] = operands[1];
      xops[3] = adj_offsettable_operand (operands[1], 4);
      output_asm_insn (\"loadw  %2,%0\;loadw  %3,%1\", xops);
      return \"\";
    }

  if (GET_CODE (operands[1]) == REG)	/* r -> r */
    {
      rtx xops[4];
      xops[0] = operands[0];
      xops[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
      xops[2] = operands[1];
      xops[3] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
      output_asm_insn (\"movw   %2,%0\;movw   %3,%1\", xops);
      return \"\";
    }

  abort ();
#undef FP_REG_P
}")


(define_insn ""
  [(set (match_operand:DF 0 "memory_operand" "=o,m")
	(match_operand:DF 1 "register_operand" "*rf,f"))]
  ""
  "*
{
  rtx xops[4];

  if (REGNO (operands[1]) >= 16)	/* f -> m */
    return \"stord  %1,%0\";

  xops[0] = operands[0];		/* r -> o */
  xops[1] = adj_offsettable_operand (operands[0], 4);
  xops[2] = operands[1];
  xops[3] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  output_asm_insn (\"storw  %2,%0\;storw  %3,%1\", xops);
  return \"\";
}"
[(set_attr "type" "store,store")
 (set_attr "cc" "clobber,unchanged")])


(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	operands[1] = force_reg (SFmode,
				 force_const_mem (SFmode, operands[1]));
      else if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (SFmode, operands[1]);
    }

  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = force_const_mem (SFmode, operands[1]);
}")

;;
;; provide two patterns with different predicates as we don't want combine
;; to recombine a mem -> mem move
;; 
(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=*rf")
	(match_operand:SF 1 "nonimmediate_operand" "*rfm"))]
  ""
  "*
{
#define FP_REG_P(X) (GET_CODE (X) == REG && REGNO (X) >= 16)

  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))	/* f -> f */
	return \"movs   %1,%0\";
      if (GET_CODE (operands[1]) == REG) /* r -> f */
	return
	  \"subq   $8,sp\;storw  %1,(sp)\;loads  (sp),%0\;addq   $8,sp\";
      return \"loads  %1,%0\";		/* m -> f */
    }

  if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG) /* f -> r */
	return
	  \"subq   $8,sp\;stors  %1,(sp)\;loadw  (sp),%0\;addq   $8,sp\";
      abort ();
    }

  if (GET_CODE (operands[1]) == MEM)	/* m -> r */
    return \"loadw   %1,%0\";

  if (GET_CODE (operands[1]) == REG)	/* r -> r */
    return \"movw    %1,%0\";

  abort ();
#undef FP_REG_P
}")

(define_insn ""
  [(set (match_operand:SF 0 "memory_operand" "=m")
	(match_operand:SF 1 "register_operand" "*rf"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) >= 16)
    return \"stors  %1,%0\";		/* f-> m */

  return \"storw   %1,%0\";		/* r -> m */
}"
[(set_attr "type" "store")])


(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) != REG)
    operands[1] = force_reg (DImode, operands[1]);
}")

;; If an operand is a MEM but not offsettable, we can't load it into
;; a register, so we must force the third alternative to be the one
;; reloaded.  Hence we show the first as more expensive.
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=?r,r,r")
	(match_operand:DI 1 "general_operand"   "r,n,o"))]
  ""
  "*
{
  rtx xoperands[2],yoperands[2];

  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);

  if (which_alternative == 0)		/* r -> r */
    {
      output_asm_insn (\"movw   %1,%0\", operands);
      xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
      output_asm_insn (\"movw   %1,%0\", xoperands);
      return \"\";
    }

  if (which_alternative == 1)		/* n -> r */
    {
      if (GET_CODE (operands[1]) == CONST_INT)
	{
	  output_asm_insn (\"loadi   %1,%0\", operands);
	  output_asm_insn (\"loadq   $0,%0\", xoperands);
	  return \"\";
	}

      if (GET_CODE (operands[1]) != CONST_DOUBLE)
	abort ();

      yoperands[0] = operands[0];
      yoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
      output_asm_insn (\"loadi  %1,%0\", yoperands);

      xoperands[1] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
      output_asm_insn (\"loadi  %1,%0\", xoperands);
      return \"\";
    }
					/* m -> r */
  output_asm_insn (\"loadw  %1,%0\", operands);
  xoperands[1] = adj_offsettable_operand (operands[1], 4);
  output_asm_insn (\"loadw  %1,%0\", xoperands);
  return \"\";
}" 
[(set_attr "type" "arith,arith,load")
  (set_attr "cc" "clobber,clobber,clobber")])

(define_insn ""
  [(set (match_operand:DI 0 "memory_operand" "=o")
	(match_operand:DI 1 "register_operand" "r"))]
  ""
  "*
{
  rtx xops[4];
  xops[0] = operands[0];
  xops[1] = adj_offsettable_operand (operands[0], 4);
  xops[2] = operands[1];
  xops[3] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  output_asm_insn (\"storw  %2,%0\;storw  %3,%1\", xops);
  return \"\";
}"
[(set_attr "type" "store")
 (set_attr "cc" "clobber")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM &&
      GET_CODE (operands[1]) != REG)
    operands[1] = force_reg (SImode, operands[1]);
}")

;; Reject both args with `general_operand' if not reloading because a
;; mem -> mem move that was split by 'movsi' can be recombined to
;; mem -> mem by the combiner.
;;
;; As a pseudo register can end up in a stack slot during reloading we must
;; allow a r->m move for the next pattern. 
;; The first predicate must be `general_operand' because a predicate must
;; be true for each constraint.
;;  
(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,r,m")
	(match_operand:SI 1 "general_operand"  "r,m,n,i,r"))]
  "reload_in_progress || register_operand (operands[0], SImode)"
  "*
{
  int val;

  if (which_alternative == 0)
    return \"movw   %1,%0\";		/* reg -> reg */

  if (which_alternative == 1)
    return \"loadw  %1,%0\";		/* mem -> reg */

  if (which_alternative == 2)
    {
      val = INTVAL (operands[1]);	/* known const ->reg */

      if (val == -1)
	return \"notq   $0,%0\";

      if (val < 0 || val >= 16)
	return \"loadi  %1,%0\";

      return \"loadq  %1,%0\";
    }

  if (which_alternative == 3)		/* unknown const */
    return \"loada  %a1,%0\";

  return \"storw  %1,%0\";
}"
[(set_attr "type" "arith,load,arith,load,store")
 (set_attr "cc" "set2,change0,set1,change0,unchanged")])


(define_insn ""
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "int_reg_operand" "r"))]
  ""
  "storw  %1,%0"
[(set_attr "type" "store")])

;; movhi
;;
;; loadh  mem to reg
;; storh  reg to mem
;;
;;
(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM
      && ! register_operand (operands[1], HImode))
    operands[1] = force_reg (HImode, operands[1]);
}")


(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"   "r,m,n"))]
  ""
  "@
   movw   %1,%0
   loadh  %1,%0
   loadi  %1,%0"
[(set_attr "type" "arith,load,arith")])

(define_insn ""
  [(set (match_operand:HI 0 "memory_operand"  "=m")
	(match_operand:HI 1 "register_operand" "r"))]
  ""
  "storh  %1,%0"
 [(set_attr "type" "store")])

;; movqi
;;
;; loadb  mem to reg
;; storb  reg to mem
;;
(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM && 
      ! register_operand (operands[1], QImode))
    operands[1] = force_reg (QImode, operands[1]);
}")


(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r,r,r")
	(match_operand:QI 1 "general_operand"   "r,m,n"))]
  ""
  "@
   movw   %1,%0
   loadb  %1,%0
   loadi  %1,%0"
[(set_attr "type" "arith,load,arith")])

(define_insn ""
  [(set (match_operand:QI 0 "memory_operand" "=m")
	(match_operand:QI 1 "register_operand" "r"))]
  ""
  "storb  %1,%0"
[(set_attr "type" "store")])


;;
;; block move
;;
(define_expand "movstrsi"
  [(parallel
    [(set (match_operand:BLK 0 "memory_operand" "")
          (match_operand:BLK 1 "memory_operand" ""))
     (use (match_operand:SI 2 "general_operand" ""))
     (use (match_operand:SI 3 "const_int_operand" ""))
     (clobber (match_scratch:SI 4 ""))
     (clobber (match_scratch:SI 5 ""))
     (clobber (match_dup 6))
     (clobber (match_dup 7))])]
  ""
  "
{
  rtx addr0, addr1;

  addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  addr1 = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  operands[6] = addr0;
  operands[7] = addr1;

  operands[0] = change_address (operands[0], VOIDmode, addr0);
  operands[1] = change_address (operands[1], VOIDmode, addr1);

  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = force_reg (SImode, operands[2]);
}")

;;
;; there is a problem with this insn in gcc-2.2.3
;; (clobber (match_dup 2)) does not prevent use of this operand later
;; we always use a scratch register and leave operand 2 unchanged
;;
(define_insn ""
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "r")))
   (use (match_operand:SI 2 "nonmemory_operand" "rn"))
   (use (match_operand:SI 3 "const_int_operand" "n"))
   (clobber (match_scratch:SI 4 "=r"))
   (clobber (match_scratch:SI 5 "=r"))
   (clobber (match_dup 0))
   (clobber (match_dup 1))]
  ""
  "*
{
  extern void clipper_movstr ();
  clipper_movstr (operands);
  return \"\";
}"
[(set_attr "cc" "clobber")])



;; Extension and truncation insns.
(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $65535,%0\;xori   $32768,%0\;subi   $32768,%0
   loadh  %1,%0"
[(set_attr "type" "arith,load")])


(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "int_reg_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $255,%0\;xori   $128,%0\;subi   $128,%0
   loadb  %1,%0"
[(set_attr "type" "arith,load")
 (set_attr "cc" "set1,change0")])


(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $255,%0\;xori   $128,%0\;subi   $128,%0
   loadb  %1,%0"
[(set_attr "type" "arith,load")])


(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "fp_reg_operand" "f")))]
  ""
  "cnvsd  %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "fp_reg_operand" "f")))]
  ""
  "cnvds  %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $65535,%0
   loadhu %1,%0"
[(set_attr "type" "arith,load")])


(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "int_reg_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $255,%0
   loadbu %1,%0"
[(set_attr "type" "arith,load")
 (set_attr "cc" "clobber,clobber")])


(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "0,m")))]
  ""
  "@
   andi   $255,%0
   loadbu %1,%0"
[(set_attr "type" "arith,load")])



;; Fix-to-float conversion insns.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(float:SF (match_operand:SI 1 "int_reg_operand" "r")))]
  ""
  "cnvws  %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(float:DF (match_operand:SI 1 "int_reg_operand" "r")))]
  ""
  "cnvwd  %1,%0")


;; Float-to-fix conversion insns.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" "f"))))]
  ""
  "cnvtsw %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(fix:SI (fix:DF (match_operand:DF 1 "fp_reg_operand" "f"))))]
  ""
  "cnvtdw %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(plus:DF (match_operand:DF 1 "fp_reg_operand" "0")
		 (match_operand:DF 2 "fp_reg_operand" "f")))]
  ""
  "addd   %2,%0"
 [(set_attr "type" "fp")])


(define_insn "addsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(plus:SF (match_operand:SF 1 "fp_reg_operand" "0")
		 (match_operand:SF 2 "fp_reg_operand" "f")))]
  ""
  "adds   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(plus:DI (match_operand:DI 1 "int_reg_operand" "%0")
		 (match_operand:DI 2 "int_reg_operand" "r")))]
  ""
  "*
{
  rtx xoperands[4];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  xoperands[2] = operands[2];
  xoperands[3] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  output_asm_insn (\"addw   %2,%0\;addwc  %3,%1\", xoperands);
  return \"\";
}"
[(set_attr "type" "arith")
 (set_attr "cc" "clobber")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "int_reg_operand" "%0,r,r")
		 (match_operand:SI 2 "nonmemory_operand" "rn,0,rn")))]
  ""
  "*
{
  if (which_alternative == 2)		/* 3 address version */
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	return \"loada  %a2(%1),%0\";
      return \"loada  [%2](%1),%0\";
    }
					/* 2 address version */
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int val = INTVAL (operands[2]);

      if (val >= 16 || val == 0x80000000)
	return \"addi   %2,%0\";

      if (val < 0)			/* change to sub */
	{
	  rtx xops[2];

	  val = -val;

	  xops[0] = operands[0];
	  xops[1] = GEN_INT (val);

	  if (val >= 16)
	    output_asm_insn (\"subi   %1,%0\", xops);
	  else
	    output_asm_insn (\"subq   %1,%0\", xops);

	  return \"\";
	}

      return \"addq   %2,%0\";
    }

  if (which_alternative == 0)
    return \"addw   %2,%0\";

  return \"addw   %1,%0\";
}"
[(set_attr "type" "arith,arith,arith")
 (set_attr "cc" "set1,set1,change0")])


;;- All kinds of subtract instructions.

(define_insn "subdi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(minus:DI (match_operand:DI 1 "int_reg_operand" "0")
		  (match_operand:DI 2 "int_reg_operand" "r")))]
  ""
  "*
{
  rtx xoperands[4];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  xoperands[2] = operands[2];
  xoperands[3] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  output_asm_insn (\"subw   %2,%0\;subwc  %3,%1\", xoperands);
  return \"\";
}"
[(set_attr "type" "arith")
 (set_attr "cc" "clobber")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(minus:SI (match_operand:SI 1 "int_reg_operand" "0")
		  (match_operand:SI 2 "nonmemory_operand" "rn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int val = INTVAL (operands[2]);

      if (val < 0 || val >= 16)
	return \"subi   %2,%0\";
      else
	return \"subq   %2,%0\";
    }

  return \"subw   %2,%0\";
}"
[(set_attr "type" "arith")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(minus:DF (match_operand:DF 1 "fp_reg_operand" "0")
		  (match_operand:DF 2 "fp_reg_operand" "f")))]
  ""
  "subd   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(minus:SF (match_operand:SF 1 "fp_reg_operand" "0")
		  (match_operand:SF 2 "fp_reg_operand" "f")))]
  ""
  "subs   %2,%0"
 [(set_attr "type" "fp")])


;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(mult:DF (match_operand:DF 1 "fp_reg_operand" "0")
		 (match_operand:DF 2 "fp_reg_operand" "f")))]
  ""
  "muld   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(mult:SF (match_operand:SF 1 "fp_reg_operand" "0")
		 (match_operand:SF 2 "fp_reg_operand" "f")))]
  ""
  "muls   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "int_reg_operand" "%0"))
	         (sign_extend:DI (match_operand:SI 2 "int_reg_operand" "r"))))]
  ""
  "mulwx  %2,%0"
[(set_attr "type" "arith")
 (set_attr "cc" "clobber")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "int_reg_operand" "%0"))
	         (zero_extend:DI (match_operand:SI 2 "int_reg_operand" "r"))))]
  ""
  "mulwux %2,%0"
[(set_attr "type" "arith")
 (set_attr "cc" "clobber")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(mult:SI (match_operand:SI 1 "int_reg_operand" "%0")
	         (match_operand:SI 2 "int_reg_operand" "r")))]
  ""
  "mulw   %2,%0"
 [(set_attr "type" "arith")
  (set_attr "cc" "clobber")])


;;- Divide and mod instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(div:DF (match_operand:DF 1 "fp_reg_operand" "0")
		(match_operand:DF 2 "fp_reg_operand" "f")))]
  ""
  "divd   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(div:SF (match_operand:SF 1 "fp_reg_operand" "0")
		(match_operand:SF 2 "fp_reg_operand" "f")))]
  ""
  "divs   %2,%0"
 [(set_attr "type" "fp")])

(define_insn "divsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(div:SI (match_operand:SI 1 "int_reg_operand" "0")
		(match_operand:SI 2 "int_reg_operand" "r")))]
  ""
  "divw   %2,%0"
 [(set_attr "type" "arith")
  (set_attr "cc" "clobber")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(udiv:SI (match_operand:SI 1 "int_reg_operand" "0")
	         (match_operand:SI 2 "int_reg_operand" "r")))]
  ""
  "divwu  %2,%0"
 [(set_attr "type" "arith")
  (set_attr "cc" "clobber")])


(define_insn "modsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(mod:SI (match_operand:SI 1 "int_reg_operand" "0")
		(match_operand:SI 2 "int_reg_operand" "r")))]
  ""
  "modw   %2,%0"
 [(set_attr "type" "arith")
  (set_attr "cc" "clobber")])

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(umod:SI (match_operand:SI 1 "int_reg_operand" "0")
	         (match_operand:SI 2 "int_reg_operand" "r")))]
  ""
  "modwu  %2,%0"
 [(set_attr "type" "arith")
  (set_attr "cc" "clobber")])

;;
;; bit and/or instructions
;;
(define_insn "andsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
        (and:SI (match_operand:SI 1 "int_reg_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   andw   %2,%0
   andi   %2,%0"
 [(set_attr "type" "arith")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "int_reg_operand" "%0,0")
	        (match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   orw    %2,%0
   ori    %2,%0"
 [(set_attr "type" "arith")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "int_reg_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   xorw   %2,%0
   xori   %2,%0"
 [(set_attr "type" "arith")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "fp_reg_operand" "=f")
	(neg:DF (match_operand:DF 1 "fp_reg_operand" "f")))]
  ""
  "negd   %1,%0"
 [(set_attr "type" "fp")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
	(neg:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  ""
  "negs   %1,%0"
 [(set_attr "type" "fp")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(neg:SI (match_operand:SI 1 "int_reg_operand" "r")))]
  ""
  "negw   %1,%0"
 [(set_attr "type" "arith")])


(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(not:SI (match_operand:SI 1 "int_reg_operand" "r")))]
  ""
  "notw   %1,%0"
 [(set_attr "type" "arith")])



;; Right shift on the clipper works by negating the shift count,
;; then emitting a right shift with the shift count negated.  This means
;; that all actual shift counts in the RTL will be positive.

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "int_reg_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "shali  $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "shal   %2,%0"
 [(set_attr "type" "arith")])

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "shai   $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "shaw   %2,%0"
 [(set_attr "type" "arith")])

;;
;; left shift
;;

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r,r")
	(ashift:DI (match_operand:DI 1 "int_reg_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   shal   %2,%0
   shali  %2,%0"
 [(set_attr "type" "arith")])


(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "int_reg_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "*
{
  int val;

  if (which_alternative == 0)
   return \"shaw   %2,%0\";

  val = INTVAL (operands[2]);

  if (val == 2)
    return \"addw   %0,%0\;addw   %0,%0\";

  if (val == 1)
    return \"addw   %0,%0\";

  return \"shai   %2,%0\";
}"
[(set_attr "type" "arith")])

;;
;; logical shift
;;

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "int_reg_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "shlli  $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "shll   %2,%0"
 [(set_attr "type" "arith")])

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "shli   $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "shlw   %2,%0"
 [(set_attr "type" "arith")])


;;
;; rotate insn
;;
(define_expand "rotrdi3"
  [(set (match_operand:DI 0 "int_reg_operand" "")
	(rotatert:DI (match_operand:DI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(rotatert:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "rotli  $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:DI 0 "int_reg_operand" "=r")
	(rotatert:DI (match_operand:DI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "rotl   %2,%0"
 [(set_attr "type" "arith")])

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "")
	(rotatert:SI (match_operand:SI 1 "int_reg_operand" "")
	             (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "roti   $%n2,%0"
 [(set_attr "type" "arith")])

(define_insn ""
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "int_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "rotw   %2,%0"
 [(set_attr "type" "arith")])

(define_insn "rotldi3"
  [(set (match_operand:DI 0 "int_reg_operand" "=r,r")
	(rotate:DI (match_operand:DI 1 "int_reg_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   rotl   %2,%0
   rotli  %2,%0"
 [(set_attr "type" "arith")])

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "int_reg_operand" "=r,r")
	(rotate:SI (match_operand:SI 1 "int_reg_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "@
   rotw   %2,%0
   roti   %2,%0"
 [(set_attr "type" "arith")])


;;
;; jump and branch insns
;;
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "b      %l0"
 [(set_attr "type" "branch")])

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "b      (%0)"
 [(set_attr "type" "branch")])

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "breq   %l0"
 [(set_attr "type" "branch")])

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brne   %l0"
 [(set_attr "type" "branch")])

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brgt   %l0"
 [(set_attr "type" "branch")])

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brgtu  %l0"
 [(set_attr "type" "branch")])

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brlt   %l0"
 [(set_attr "type" "branch")])

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brltu  %l0"
 [(set_attr "type" "branch")])

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brge   %l0"
 [(set_attr "type" "branch")])

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brgeu  %l0"
 [(set_attr "type" "branch")])

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
 ""
 "brle   %l0"
 [(set_attr "type" "branch")])

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
 ""
 "brleu  %l0"
 [(set_attr "type" "branch")])

;; Recognize reversed jumps.
(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0)
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
 ""
 "br%C0    %l1" ; %C0 negates condition
 [(set_attr "type" "branch")])

;;
;; call instructions
;;
(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "m")
	 (match_operand:SI 1 "general_operand" ""))]
  ;; Operand 1 not used on the clipper.
  ""
  "call   sp,%0")

(define_insn "call_value"
  [(set (match_operand 0 "" "=rf")
	(call (match_operand:QI 1 "general_operand" "m")
	      (match_operand:SI 2 "general_operand" "g")))]
  ;; Operand 2 not used on the clipper
  ""
  "call   sp,%1")

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "b      (%0)"
 [(set_attr "type" "branch")])


(define_insn "nop"
  [(const_int 0)]
  ""
  "noop"
 [(set_attr "type" "arith")
  (set_attr "cc" "unchanged")])



;; while (--foo >= 0)
;;
;; Combiners for 'decrement test and branch' do not work for clipper.
;; These patters are jump_insns that do not allow output reloads and clipper
;; can only decrement and test registers.
;;
