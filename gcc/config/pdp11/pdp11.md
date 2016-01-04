;;- Machine description for the pdp11 for GNU C compiler
;; Copyright (C) 1994-2016 Free Software Foundation, Inc.
;; Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(include "predicates.md")
(include "constraints.md")

(define_c_enum "unspecv"
  [
    UNSPECV_BLOCKAGE
    UNSPECV_SETD
    UNSPECV_SETI
  ])

(define_constants
  [
   ;; Register numbers
   (R0_REGNUM     	  0)
   (RETVAL_REGNUM     	  0)
   (HARD_FRAME_POINTER_REGNUM  5)
   (STACK_POINTER_REGNUM  6)
   (PC_REGNUM             7)
   (AC0_REGNUM            8)
   (AC3_REGNUM            11)
   (AC4_REGNUM            12)
   (AC5_REGNUM            13)
   ;; The next two are not physical registers but are used for addressing
   ;; arguments.
   (FRAME_POINTER_REGNUM  14)
   (ARG_POINTER_REGNUM    15)
   (FIRST_PSEUDO_REGISTER 16)
   ;; Branch offset limits, as byte offsets from instruction address
   (MIN_BRANCH            -254)
   (MAX_BRANCH            256)
   (MIN_SOB               -126)
   (MAX_SOB               0)])

;; HI is 16 bit
;; QI is 8 bit 

;; Integer modes supported on the PDP11, with a mapping from machine mode
;; to mnemonic suffix.  SImode and DImode always are special cases.
(define_mode_iterator PDPint [QI HI])
(define_mode_attr  isfx [(QI "b") (HI "")])

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;;- Operand classes for the register allocator:

;; Compare instructions.

;; currently we only support df floats, which saves us quite some
;; hassle switching the FP mode! 
;; we assume that CPU is always in long float mode, and 
;; 16 bit integer mode - currently, the prologue for main does this,
;; but maybe we should just set up a NEW crt0 properly, 
;; -- and what about signal handling code?
;; (we don't even let sf floats in the register file, so
;; we only should have to worry about truncating and widening 
;; when going to memory)

;; abort() call by g++ - must define libfunc for cmp_optab
;; and ucmp_optab for mode SImode, because we don't have that!!!
;; - yet since no libfunc is there, we abort ()

;; The only thing that remains to be done then is output 
;; the floats in a way the assembler can handle it (and 
;; if you're really into it, use a PDP11 float emulation
;; library to do floating point constant folding - but 
;; I guess you'll get reasonable results even when not
;; doing this)
;; the last thing to do is fix the UPDATE_CC macro to check
;; for floating point condition codes, and set cc_status
;; properly, also setting the CC_IN_FCCR flag. 

;; define attributes
;; currently type is only fpu or arith or unknown, maybe branch later ?
;; default is arith
(define_attr "type" "unknown,arith,fp" (const_string "arith"))

;; length default is 2 bytes each
(define_attr "length" "" (const_int 2))

;; a user's asm statement
(define_asm_attributes
  [(set_attr "type" "unknown")
; length for asm is the max length per statement.  That would be
; 3 words, for a two-operand instruction with extra word addressing
; modes for both operands.
   (set_attr "length" "6")])

;; define function units

;; Prologue and epilogue support.

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  pdp11_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  pdp11_expand_epilogue ();
  DONE;
})

(define_expand "return"
  [(return)]
  "reload_completed && !frame_pointer_needed && pdp11_sp_frame_offset () == 0"
  "")

(define_insn "*rts"
  [(return)]
  ""
  "rts pc")

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")])

(define_insn "setd"
  [(unspec_volatile [(const_int 0)] UNSPECV_SETD)]
  ""
  "setd")

(define_insn "seti"
  [(unspec_volatile [(const_int 0)] UNSPECV_SETI)]
  ""
  "seti")

;; arithmetic - values here immediately when next insn issued
;; or does it mean the number of cycles after this insn was issued?
;; how do I say that fpu insns use cpu also? (pre-interaction phase)

;(define_function_unit "cpu" 1 1 (eq_attr "type" "arith") 0 0)
;(define_function_unit "fpu" 1 1 (eq_attr "type" "fp") 0 0)

;; compare
(define_insn "*cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "fR,fR,Q,QF")
		 (match_operand:DF 1 "register_or_const0_operand" "G,a,G,a")))]
  "TARGET_FPU"
  "*
{
  cc_status.flags = CC_IN_FPU;
  if (which_alternative == 0 || which_alternative == 2)
    return \"{tstd|tstf} %0\;cfcc\";
  else
    return \"{cmpd|cmpf} %0, %1\;cfcc\";
}"
  [(set_attr "length" "4,4,6,6")]) 

(define_insn "*cmp<mode>"
  [(set (cc0)
	(compare (match_operand:PDPint 0 "general_operand" "rR,rR,rR,Q,Qi,Qi")
		 (match_operand:PDPint 1 "general_operand" "N,rR,Qi,N,rR,Qi")))]
  ""
  "@
   tst<PDPint:isfx> %0
   cmp<PDPint:isfx> %0,%1
   cmp<PDPint:isfx> %0,%1
   tst<PDPint:isfx> %0
   cmp<PDPint:isfx> %0,%1
   cmp<PDPint:isfx> %0,%1"
  [(set_attr "length" "2,2,4,4,4,6")])

;; sob instruction - we need an assembler which can make this instruction
;; valid under _all_ circumstances!

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (plus:HI (match_operand:HI 0 "register_operand" "+r")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int -1)))]
  "TARGET_40_PLUS"
  "*
{
 static int labelcount = 0;
 static char buf[1000];

 if (get_attr_length (insn) == 2)
    return \"sob %0, %l1\";

 /* emulate sob */
 output_asm_insn (\"dec %0\", operands);
 
 sprintf (buf, \"bge LONG_SOB%d\", labelcount);
 output_asm_insn (buf, NULL);

 output_asm_insn (\"jmp %l1\", operands);
 
 sprintf (buf, \"LONG_SOB%d:\", labelcount++);
 output_asm_insn (buf, NULL);

 return \"\";
}"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 0)
						       (pc))
						(const_int MIN_SOB))
					   (gt (minus (match_dup 0)
						       (pc))
						(const_int MAX_SOB)))
				      (const_int 8)
				      (const_int 2)))])

;; These control RTL generation for conditional jump insns
;; and match them for register allocation.

(define_expand "cbranchdf4"
  [(set (cc0)
        (compare (match_operand:DF 1 "general_operand")
		 (match_operand:DF 2 "register_or_const0_operand")))
   (set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_FPU"
  "")

(define_expand "cbranch<mode>4"
  [(set (cc0)
        (compare (match_operand:PDPint 1 "general_operand")
		 (match_operand:PDPint 2 "general_operand")))
   (set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "")

;; problem with too short jump distance! we need an assembler which can 
;; make this valid for all jump distances!
;; e.g. gas!

;; these must be changed to check for CC_IN_FCCR if float is to be 
;; enabled

(define_insn "*branch"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "* return output_jump(GET_CODE (operands[0]), 0, get_attr_length(insn));"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 1)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 1)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 6)
				      (const_int 2)))])


;; These match inverted jump insns for register allocation.

(define_insn "*branch_inverted"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "* return output_jump(GET_CODE (operands[0]), 1, get_attr_length(insn));"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 1)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 1)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 6)
				      (const_int 2)))])

;; Move instructions

(define_insn "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,g")
	(match_operand:DI 1 "general_operand" "rN,g"))]
  ""
  "* return output_move_multiple (operands);"
;; what's the mose expensive code - say twice movsi = 16
  [(set_attr "length" "16,32")])

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,g,g")
	(match_operand:SI 1 "general_operand" "rN,IJ,IJ,g"))]
  ""
  "* return output_move_multiple (operands);"
;; what's the most expensive code ? - I think 8!
;; we could split it up and make several sub-cases...
  [(set_attr "length" "4,6,8,16")])

(define_insn "mov<mode>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(match_operand:PDPint 1 "general_operand" "rRN,Qi,rRN,Qi"))]
  ""
  "*
{
  if (operands[1] == const0_rtx)
    return \"clr<PDPint:isfx> %0\";

  return \"mov<PDPint:isfx> %1, %0\";
}"
  [(set_attr "length" "2,4,4,6")])

(define_insn "movdf"
  [(set (match_operand:DF 0 "float_nonimm_operand" "=a,fR,a,Q,g")
        (match_operand:DF 1 "float_operand" "fR,a,FQ,a,g"))]
  "TARGET_FPU"
  "* if (which_alternative ==0 || which_alternative == 2)
       return \"ldd %1, %0\";
     else if (which_alternative == 1 || which_alternative == 3)
       return \"std %1, %0\";
     else 
       return output_move_multiple (operands); "
;; last one is worst-case
  [(set_attr "length" "2,2,4,4,24")])

(define_insn "movsf"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=a,fR,a,Q,g")
        (match_operand:SF 1 "float_operand" "fR,a,FQ,a,g"))]
  "TARGET_FPU"
  "* if (which_alternative ==0 || which_alternative == 2)
       return \"{ldcfd|movof} %1, %0\";
     else if (which_alternative == 1 || which_alternative == 3)
       return \"{stcdf|movfo} %1, %0\";
     else 
       return output_move_multiple (operands); "
;; last one is worst-case
  [(set_attr "length" "2,2,4,4,12")])

;; maybe fiddle a bit with move_ratio, then 
;; let constraints only accept a register ...

(define_expand "movmemhi"
  [(parallel [(set (match_operand:BLK 0 "general_operand" "=g,g")
		   (match_operand:BLK 1 "general_operand" "g,g"))
	      (use (match_operand:HI 2 "general_operand" "n,mr"))
	      (use (match_operand:HI 3 "immediate_operand" "i,i"))
	      (clobber (match_scratch:HI 4 "=&r,X"))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))
	      (clobber (match_dup 2))])]
  "(TARGET_BCOPY_BUILTIN)"
  "
{
  operands[0]
    = replace_equiv_address (operands[0],
			     copy_to_mode_reg (Pmode, XEXP (operands[0], 0)));
  operands[1]
    = replace_equiv_address (operands[1],
			     copy_to_mode_reg (Pmode, XEXP (operands[1], 0)));

  operands[5] = XEXP (operands[0], 0);
  operands[6] = XEXP (operands[1], 0);
}")


(define_insn "movmemhi1"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "r,r"))
	(mem:BLK (match_operand:HI 1 "register_operand" "r,r")))
   (use (match_operand:HI 2 "general_operand" "n,r"))
   (use (match_operand:HI 3 "immediate_operand" "i,i"))
   (clobber (match_scratch:HI 4 "=&r,X"))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_dup 2))]
  "(TARGET_BCOPY_BUILTIN)"
  "* return output_block_move (operands);"
;;; just a guess
  [(set_attr "length" "80")])
   


;;- truncation instructions

(define_insn  "truncdfsf2"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=f,R,Q")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f,a,a")))]
  "TARGET_FPU"
  "* if (which_alternative ==0)
     {
       return \"\";
     }
     else if (which_alternative == 1)
       return \"{stcdf|movfo} %1, %0\";
     else 
       return \"{stcdf|movfo} %1, %0\";
  "
  [(set_attr "length" "0,2,4")])


(define_expand "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(subreg:HI 
	  (match_operand:SI 1 "general_operand" "or")
          0))]
  ""
  "")


;;- zero extension instructions

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "0,0")))]
  ""
  "bic $0177400, %0"
  [(set_attr "length" "4,6")])
			 
(define_expand "zero_extendhisi2"
  [(set (subreg:HI 
          (match_dup 0)
          2)
        (match_operand:HI 1 "register_operand" "r"))
   (set (subreg:HI 
          (match_operand:SI 0 "register_operand" "=r")
          0)
        (const_int 0))]
  ""
  "/* operands[1] = make_safe_from (operands[1], operands[0]); */")


;;- sign extension instructions

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,a,a")
	(float_extend:DF (match_operand:SF 1 "float_operand" "f,R,Q")))]
  "TARGET_FPU"
  "@
   /* nothing */
   {ldcfd|movof} %1, %0
   {ldcfd|movof} %1, %0"
  [(set_attr "length" "0,2,4")])

;; does movb sign extend in register-to-register move?
(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "rR,Q")))]
  ""
  "movb %1, %0"
  [(set_attr "length" "2,4")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "rR,Q")))]
  "TARGET_40_PLUS"
  "*
{
  rtx latehalf[2];

  /* make register pair available */
  latehalf[0] = operands[0];
  operands[0] = gen_rtx_REG (HImode, REGNO (operands[0])+ 1);

  output_asm_insn(\"movb %1, %0\", operands);
  output_asm_insn(\"sxt %0\", latehalf);
    
  return \"\";
}"
  [(set_attr "length" "4,6")])

;; maybe we have to use define_expand to say that we have the instruction,
;; unconditionally, and then match dependent on CPU type:

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "")
  
(define_insn "" ; "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=o,<,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g,g,g")))]
  "TARGET_40_PLUS"
  "*
{
  rtx latehalf[2];

  /* we don't want to mess with auto increment */
  
  switch (which_alternative)
  {
    case 0:

      latehalf[0] = operands[0];
      operands[0] = adjust_address(operands[0], HImode, 2);
  
      output_asm_insn(\"mov %1, %0\", operands);
      output_asm_insn(\"sxt %0\", latehalf);

      return \"\";

    case 1:

      /* - auto-decrement - right direction ;-) */
      output_asm_insn(\"mov %1, %0\", operands);
      output_asm_insn(\"sxt %0\", operands);

      return \"\";

    case 2:

      /* make register pair available */
      latehalf[0] = operands[0];
      operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);

      output_asm_insn(\"mov %1, %0\", operands);
      output_asm_insn(\"sxt %0\", latehalf);

      return \"\";

    default:

      gcc_unreachable ();
  }
}"
  [(set_attr "length" "10,6,6")])


(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "0")))]
  "(! TARGET_40_PLUS)"
  "*
{
  static int count = 0;
  char buf[100];
  rtx lateoperands[2];

  lateoperands[0] = operands[0];
  operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);

  output_asm_insn(\"tst %0\", operands);
  sprintf(buf, \"bge extendhisi%d\", count);
  output_asm_insn(buf, NULL);
  output_asm_insn(\"mov -1, %0\", lateoperands);
  sprintf(buf, \"bne extendhisi%d\", count+1);
  output_asm_insn(buf, NULL);
  sprintf(buf, \"\\nextendhisi%d:\", count);
  output_asm_insn(buf, NULL);
  output_asm_insn(\"clr %0\", lateoperands);
  sprintf(buf, \"\\nextendhisi%d:\", count+1);
  output_asm_insn(buf, NULL);

  count += 2;

  return \"\";
}"
  [(set_attr "length" "12")])

;; make float to int and vice versa 
;; using the cc_status.flag field we could probably cut down
;; on seti and setl
;; assume that we are normally in double and integer mode -
;; what do pdp library routines do to fpu mode ?

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,a,a")
	(float:DF (match_operand:SI 1 "general_operand" "r,R,Q")))]
  "TARGET_FPU"
  "* if (which_alternative ==0)
     {
       rtx latehalf[2];

       latehalf[0] = NULL; 
       latehalf[1] = gen_rtx_REG (HImode, REGNO (operands[1]) + 1);
       output_asm_insn(\"mov %1, -(sp)\", latehalf);
       output_asm_insn(\"mov %1, -(sp)\", operands);
       
       output_asm_insn(\"setl\", operands);
       output_asm_insn(\"{ldcld|movif} (sp)+, %0\", operands);
       output_asm_insn(\"seti\", operands);
       return \"\";
     }
     else if (which_alternative == 1)
       return \"setl\;{ldcld|movif} %1, %0\;seti\";
     else 
       return \"setl\;{ldcld|movif} %1, %0\;seti\";
  "
  [(set_attr "length" "10,6,8")])

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(float:DF (match_operand:HI 1 "general_operand" "rR,Qi")))]
  "TARGET_FPU"
  "{ldcid|movif} %1, %0"
  [(set_attr "length" "2,4")])
	
;; cut float to int
(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,R,Q")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "a,a,a"))))]
  "TARGET_FPU"
  "* if (which_alternative ==0)
     {
       output_asm_insn(\"setl\", operands);
       output_asm_insn(\"{stcdl|movfi} %1, -(sp)\", operands);
       output_asm_insn(\"seti\", operands);
       output_asm_insn(\"mov (sp)+, %0\", operands);
       operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);
       output_asm_insn(\"mov (sp)+, %0\", operands);
       return \"\";
     }
     else if (which_alternative == 1)
       return \"setl\;{stcdl|movfi} %1, %0\;seti\";
     else 
       return \"setl\;{stcdl|movfi} %1, %0\;seti\";
  "
  [(set_attr "length" "10,6,8")])

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(fix:HI (fix:DF (match_operand:DF 1 "register_operand" "a,a"))))]
  "TARGET_FPU"
  "{stcdi|movfi} %1, %0"
  [(set_attr "length" "2,4")])


;;- arithmetic instructions
;;- add instructions

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(plus:DF (match_operand:DF 1 "register_operand" "%0,0")
		 (match_operand:DF 2 "general_operand" "fR,QF")))]
  "TARGET_FPU"
  "{addd|addf} %2, %0"
  [(set_attr "length" "2,4")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,0,0,0")
		 (match_operand:DI 2 "general_operand" "r,on,r,on")))]
  ""
  "*
{
  rtx inops[2];
  rtx exops[4][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, NULL, either);
  
  if (!CONSTANT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"add %1, %0\", exops[0]);
  if (!CONSTANT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"add %1, %0\", exops[1]);
    output_asm_insn (\"adc %0\", exops[0]);
  }
  if (!CONSTANT_P (exops[2][1]) || INTVAL (exops[2][1]) != 0)
  {
    output_asm_insn (\"add %1, %0\", exops[2]);
    output_asm_insn (\"adc %0\", exops[1]);
    output_asm_insn (\"adc %0\", exops[0]);
  }
  if (!CONSTANT_P (exops[3][1]) || INTVAL (exops[3][1]) != 0)
  {
    output_asm_insn (\"add %1, %0\", exops[3]);
    output_asm_insn (\"adc %0\", exops[2]);
    output_asm_insn (\"adc %0\", exops[1]);
    output_asm_insn (\"adc %0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "20,28,40,48")])

;; Note that the register operand is not marked earlyclobber.
;; The reason is that SI values go in register pairs, so they
;; can't partially overlap.  They can be either disjoint, or
;; source and destination can be equal.  The latter case is 
;; handled properly because of the ordering of the individual
;; instructions used.  Specifically, carry from the low to the
;; high word is added at the end, so the adding of the high parts
;; will always used the original high part and not a high part
;; modified by carry (which would amount to double carry).
(define_insn "addsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,o,o")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,0,0,0")
		 (match_operand:SI 2 "general_operand" "r,on,r,on")))]
  ""
  "*
{
  rtx inops[2];
  rtx exops[2][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, NULL, either);
  
  if (!CONSTANT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"add %1, %0\", exops[0]);
  if (!CONSTANT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"add %1, %0\", exops[1]);
    output_asm_insn (\"adc %0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "6,10,12,16")])

(define_insn "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(plus:HI (match_operand:HI 1 "general_operand" "%0,0,0,0")
		 (match_operand:HI 2 "general_operand" "rRLM,Qi,rRLM,Qi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL(operands[2]) == 1)
	return \"inc %0\";
      else if (INTVAL(operands[2]) == -1)
        return \"dec %0\";
    }

  return \"add %2, %0\";
}"
  [(set_attr "length" "2,4,4,6")])


;;- subtract instructions
;; we don't have to care for constant second 
;; args, since they are canonical plus:xx now!
;; also for minus:DF ??

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(minus:DF (match_operand:DF 1 "register_operand" "0,0")
		  (match_operand:DF 2 "general_operand" "fR,Q")))]
  "TARGET_FPU"
  "{subd|subf} %2, %0"
  [(set_attr "length" "2,4")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(minus:DI (match_operand:DI 1 "general_operand" "0,0,0,0")
		 (match_operand:DI 2 "general_operand" "r,on,r,on")))]
  ""
  "*
{
  rtx inops[2];
  rtx exops[4][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, NULL, either);
  
  if (!CONSTANT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"sub %1, %0\", exops[0]);
  if (!CONSTANT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"sub %1, %0\", exops[1]);
    output_asm_insn (\"sbc %0\", exops[0]);
  }
  if (!CONSTANT_P (exops[2][1]) || INTVAL (exops[2][1]) != 0)
  {
    output_asm_insn (\"sub %1, %0\", exops[2]);
    output_asm_insn (\"sbc %0\", exops[1]);
    output_asm_insn (\"sbc %0\", exops[0]);
  }
  if (!CONSTANT_P (exops[3][1]) || INTVAL (exops[3][1]) != 0)
  {
    output_asm_insn (\"sub %1, %0\", exops[3]);
    output_asm_insn (\"sbc %0\", exops[2]);
    output_asm_insn (\"sbc %0\", exops[1]);
    output_asm_insn (\"sbc %0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "20,28,40,48")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,o,o")
	(minus:SI (match_operand:SI 1 "general_operand" "0,0,0,0")
		 (match_operand:SI 2 "general_operand" "r,on,r,on")))]
  ""
  "*
{
  rtx inops[2];
  rtx exops[2][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, NULL, either);
  
  if (!CONSTANT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"sub %1, %0\", exops[0]);
  if (!CONSTANT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"sub %1, %0\", exops[1]);
    output_asm_insn (\"sbc %0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "6,10,12,16")])

(define_insn "subhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(minus:HI (match_operand:HI 1 "general_operand" "0,0,0,0")
		  (match_operand:HI 2 "general_operand" "rR,Qi,rR,Qi")))]
  ""
  "*
{
  gcc_assert (GET_CODE (operands[2]) != CONST_INT);

  return \"sub %2, %0\";
}"
  [(set_attr "length" "2,4,4,6")])

;;;;- and instructions
;; Bit-and on the pdp (like on the VAX) is done with a clear-bits insn.

(define_expand "and<mode>3"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "")
	(and:PDPint (not:PDPint (match_operand:PDPint 1 "general_operand" ""))
		   (match_operand:PDPint 2 "general_operand" "")))]
  ""
  "
{
  rtx op1 = operands[1];

  /* If there is a constant argument, complement that one.
     Similarly, if one of the inputs is the same as the output,
     complement the other input.  */
  if ((CONST_INT_P (operands[2]) && ! CONST_INT_P (op1)) ||
      rtx_equal_p (operands[0], operands[1]))
    {
      operands[1] = operands[2];
      operands[2] = op1;
      op1 = operands[1];
    }

  if (CONST_INT_P (op1))
    operands[1] = GEN_INT (~INTVAL (op1));
  else
    operands[1] = expand_unop (<MODE>mode, one_cmpl_optab, op1, 0, 1);
}")

(define_insn "*bic<mode>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(and:PDPint
	     (not: PDPint (match_operand:PDPint 1 "general_operand" "rR,Qi,rR,Qi"))
	     (match_operand:PDPint 2 "general_operand" "0,0,0,0")))]
  ""
  "bic<PDPint:isfx> %1, %0"
  [(set_attr "length" "2,4,4,6")])

;;- Bit set (inclusive or) instructions
(define_insn "ior<mode>3"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(ior:PDPint (match_operand:PDPint 1 "general_operand" "%0,0,0,0")
		(match_operand:PDPint 2 "general_operand" "rR,Qi,rR,Qi")))]
  ""
  "bis<PDPint:isfx> %2, %0"
  [(set_attr "length" "2,4,4,6")])

;;- xor instructions
(define_insn "xorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(xor:HI (match_operand:HI 1 "general_operand" "%0,0")
		(match_operand:HI 2 "register_operand" "r,r")))]
  "TARGET_40_PLUS"
  "xor %2, %0"
  [(set_attr "length" "2,4")])

;;- one complement instructions

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
        (not:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))]
  ""
  "com<PDPint:isfx> %0"
  [(set_attr "length" "2,4")])

;;- arithmetic shift instructions
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "ashc %2,%0"
  [(set_attr "length" "2,4")])

;; Arithmetic right shift on the pdp works by negating the shift count.
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:HI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = negate_rtx (HImode, operands[2]);
}")

;; define asl aslb asr asrb - ashc missing!

;; asl 
(define_insn "" 
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(ashift:HI (match_operand:HI 1 "general_operand" "0,0")
		   (const_int 1)))]
  ""
  "asl %0"
  [(set_attr "length" "2,4")])

;; and another possibility for asr is << -1
;; might cause problems since -1 can also be encoded as 65535!
;; not in gcc2 ??? 

;; asr
(define_insn "" 
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(ashift:HI (match_operand:HI 1 "general_operand" "0,0")
		   (const_int -1)))]
  ""
  "asr %0"
  [(set_attr "length" "2,4")])

;; lsr
(define_insn "lsrhi1" 
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "0,0")
		   (const_int 1)))]
  ""
  "clc\;ror %0"
  [(set_attr "length" "2,4")])

(define_insn "lsrsi1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "0")
                   (const_int 1)))]
  ""
{

  rtx lateoperands[2];

  lateoperands[0] = operands[0];
  operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);

  lateoperands[1] = operands[1];
  operands[1] = gen_rtx_REG (HImode, REGNO (operands[1]) + 1);

  output_asm_insn (\"clc\", operands);
  output_asm_insn (\"ror %0\", lateoperands);
  output_asm_insn (\"ror %0\", operands);

  return \"\";
}
  [(set_attr "length" "10")])

(define_expand "lshrsi3"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "0")
   (match_operand:HI 2 "general_operand" "")]
  ""
  "
{
  rtx r;

  if (!TARGET_40_PLUS &&
      (GET_CODE (operands[2]) != CONST_INT ||
       (unsigned) INTVAL (operands[2]) > 3))
    FAIL;
  emit_insn (gen_lsrsi1 (operands[0], operands[1]));
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      r = gen_reg_rtx (HImode);
      emit_insn (gen_addhi3 (r, operands [2], GEN_INT (-1)));
      emit_insn (gen_ashrsi3 (operands[0], operands[0], r));
    }
  else if ((unsigned) INTVAL (operands[2]) != 1)
    {
      emit_insn (gen_ashlsi3 (operands[0], operands[0],
                              GEN_INT (1 - INTVAL (operands[2]))));
    }
  DONE;
}
"
)

;; shift is by arbitrary count is expensive, 
;; shift by one cheap - so let's do that, if
;; space doesn't matter
(define_insn "" 
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r")
	(ashift:HI (match_operand:HI 1 "general_operand" "0")
		   (match_operand:HI 2 "expand_shift_operand" "O")))]
  "! optimize_size"
  "*
{
  register int i;

  for (i = 1; i <= abs(INTVAL(operands[2])); i++)
    if (INTVAL(operands[2]) < 0)
      output_asm_insn(\"asr %0\", operands);
    else
      output_asm_insn(\"asl %0\", operands);
      
  return \"\";
}"
;; longest is 4
  [(set (attr "length") (const_int 8))])

;; aslb
(define_insn "" 
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,o")
	(ashift:QI (match_operand:QI 1 "general_operand" "0,0")
		   (match_operand:HI 2 "const_int_operand" "n,n")))]
  ""
  "*
{ /* allowing predec or post_inc is possible, but hairy! */
  int i, cnt;

  cnt = INTVAL(operands[2]) & 0x0007;

  for (i=0 ; i < cnt ; i++)
       output_asm_insn(\"aslb %0\", operands);

  return \"\";
}"
;; set attribute length ( match_dup 2 & 7 ) *(1 or 2) !!!
  [(set_attr_alternative "length" 
                         [(const_int 14)
                          (const_int 28)])])

;;; asr 
;(define_insn "" 
;  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
;	(ashiftrt:HI (match_operand:HI 1 "general_operand" "0,0")
;		     (const_int 1)))]
;  ""
;  "asr %0"
;  [(set_attr "length" "2,4")])

;; asrb
(define_insn "" 
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,o")
	(ashiftrt:QI (match_operand:QI 1 "general_operand" "0,0")
		     (match_operand:HI 2 "const_int_operand" "n,n")))]
  ""
  "*
{ /* allowing predec or post_inc is possible, but hairy! */
  int i, cnt;

  cnt = INTVAL(operands[2]) & 0x0007;

  for (i=0 ; i < cnt ; i++)
       output_asm_insn(\"asrb %0\", operands);

  return \"\";
}"
  [(set_attr_alternative "length" 
                         [(const_int 14)
                          (const_int 28)])])

;; the following is invalid - too complex!!! - just say 14 !!!
;  [(set (attr "length") (plus (and (match_dup 2)
;                                   (const_int 14))
;                              (and (match_dup 2)
;                                   (const_int 14))))])



;; can we get +-1 in the next pattern? should 
;; have been caught by previous patterns!

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0,0")
		   (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "*
{
  if (GET_CODE(operands[2]) == CONST_INT)
    {
      if (INTVAL(operands[2]) == 1)
	return \"asl %0\";
      else if (INTVAL(operands[2]) == -1)
	return \"asr %0\";
    }

  return \"ash %2,%0\";
}"
  [(set_attr "length" "2,4")])

;; Arithmetic right shift on the pdp works by negating the shift count.
(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0")
		   (match_operand:HI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = negate_rtx (HImode, operands[2]);
}")

(define_expand "lshrhi3"
  [(match_operand:HI 0 "register_operand" "")
   (match_operand:HI 1 "register_operand" "")
   (match_operand:HI 2 "general_operand" "")]
  ""
  "
{
  rtx r;

  if (!TARGET_40_PLUS &&
      (GET_CODE (operands[2]) != CONST_INT ||
       (unsigned) INTVAL (operands[2]) > 3))
    FAIL;
  emit_insn (gen_lsrhi1 (operands[0], operands[1]));
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      r = gen_reg_rtx (HImode);
      emit_insn (gen_addhi3 (r, operands [2], GEN_INT (-1)));
      emit_insn (gen_ashrhi3 (operands[0], operands[0], r));
    }
  else if ((unsigned) INTVAL (operands[2]) != 1)
    {
      emit_insn (gen_ashlhi3 (operands[0], operands[0],
                              GEN_INT (1 - INTVAL (operands[2]))));
    }
  DONE;
}
"
)

;; absolute 

(define_insn "absdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fR,Q")
	(abs:DF (match_operand:DF 1 "general_operand" "0,0")))]
  "TARGET_FPU"
  "{absd|absf} %0"
  [(set_attr "length" "2,4")])


;; negate insns

(define_insn "negdf2"
  [(set (match_operand:DF 0 "float_nonimm_operand" "=fR,Q")
	(neg:DF (match_operand:DF 1 "register_operand" "0,0")))]
  "TARGET_FPU"
  "{negd|negf} %0"
  [(set_attr "length" "2,4")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,o")
	(neg:DI (match_operand:DI 1 "general_operand" "0,0")))]
  ""
{
  rtx exops[4][2];
  
  pdp11_expand_operands (operands, exops, 1, NULL, either);

  output_asm_insn (\"com %0\", exops[3]);
  output_asm_insn (\"com %0\", exops[2]);
  output_asm_insn (\"com %0\", exops[1]);
  output_asm_insn (\"com %0\", exops[0]);
  output_asm_insn (\"add $1, %0\", exops[3]);
  output_asm_insn (\"adc %0\", exops[2]);
  output_asm_insn (\"adc %0\", exops[1]);
  output_asm_insn (\"adc %0\", exops[0]);

  return \"\";
}
[(set_attr "length" "18,34")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,o")
	(neg:SI (match_operand:SI 1 "general_operand" "0,0")))]
  ""
{
  rtx exops[2][2];
  
  pdp11_expand_operands (operands, exops, 1, NULL, either);

  output_asm_insn (\"com %0\", exops[1]);
  output_asm_insn (\"com %0\", exops[0]);
  output_asm_insn (\"add $1, %0\", exops[1]);
  output_asm_insn (\"adc %0\", exops[0]);

  return \"\";
}
[(set_attr "length" "12,20")])

(define_insn "neg<mode>2"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
	(neg:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))]
  ""
  "neg<isfx> %0"
  [(set_attr "length" "2,4")])


;; Unconditional and other jump instructions
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
 if (get_attr_length (insn) == 2)
    return \"br %l0\";
 return \"jmp %l0\";
}"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 0)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 0)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 4)
				      (const_int 2)))])

(define_insn ""
  [(set (pc)
    (label_ref (match_operand 0 "" "")))
   (clobber (const_int 1))]
  ""
  "jmp %l0"
  [(set_attr "length" "4")])

(define_insn "tablejump"
  [(set (pc) (match_operand:HI 0 "general_operand" "r,R,Q"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "@
  jmp (%0)
  jmp %@%0
  jmp %@%0"
  [(set_attr "length" "2,2,4")])

;; indirect jump - let's be conservative!
;; allow only register_operand, even though we could also 
;; allow labels etc.

(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "register_operand" "r"))]
  ""
  "jmp (%0)")

;;- jump to subroutine

(define_insn "call"
  [(call (match_operand:HI 0 "general_operand" "rR,Q")
	 (match_operand:HI 1 "general_operand" "g,g"))
;;   (use (reg:HI 0)) what was that ???
  ]
  ;;- Don't use operand 1 for most machines.
  ""
  "jsr pc, %0"
  [(set_attr "length" "2,4")])

;;- jump to subroutine
(define_insn "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:HI 1 "general_operand" "rR,Q")
	      (match_operand:HI 2 "general_operand" "g,g")))
;;   (use (reg:HI 0)) - what was that ????
  ]
  ;;- Don't use operand 2 for most machines.
  ""
  "jsr pc, %1"
  [(set_attr "length" "2,4")])

;;- nop instruction
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")


;;- multiply 

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(mult:DF (match_operand:DF 1 "register_operand" "%0,0")
		 (match_operand:DF 2 "float_operand" "fR,QF")))]
  "TARGET_FPU"
  "{muld|mulf} %2, %0"
  [(set_attr "length" "2,4")])

;; 16 bit result multiply:
;; currently we multiply only into odd registers, so we don't use two 
;; registers - but this is a bit inefficient at times. If we define 
;; a register class for each register, then we can specify properly 
;; which register need which scratch register ....

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d") ; multiply regs
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "float_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "mul %2, %0"
  [(set_attr "length" "2,4")])

;; 32 bit result
(define_expand "mulhisi3"
  [(set (match_dup 3)
	(match_operand:HI 1 "nonimmediate_operand" "g,g"))
   (set (match_operand:SI 0 "register_operand" "=r,r") ; even numbered!
	(mult:SI (truncate:HI 
                  (match_dup 0))
		 (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "operands[3] = gen_lowpart(HImode, operands[1]);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r") ; even numbered!
	(mult:SI (truncate:HI 
                  (match_operand:SI 1 "register_operand" "%0,0"))
		 (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "mul %2, %0"
  [(set_attr "length" "2,4")])

;(define_insn "mulhisi3"
;  [(set (match_operand:SI 0 "register_operand" "=r,r") ; even numbered!
;	(mult:SI (truncate:HI 
;                  (match_operand:SI 1 "register_operand" "%0,0"))
;		 (match_operand:HI 2 "general_operand" "rR,Qi")))]
;  "TARGET_40_PLUS"
;  "mul %2, %0"
;  [(set_attr "length" "2,4")])

;;- divide
(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(div:DF (match_operand:DF 1 "register_operand" "0,0")
		(match_operand:DF 2 "general_operand" "fR,QF")))]
  "TARGET_FPU"
  "{divd|divf} %2, %0"
  [(set_attr "length" "2,4")])

	 
(define_expand "divhi3"
  [(set (subreg:HI (match_dup 1) 0)
	(div:HI (match_operand:SI 1 "register_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))
   (set (match_operand:HI 0 "register_operand" "=r")
        (subreg:HI (match_dup 1) 0))]
  "TARGET_40_PLUS"
  "")

(define_insn ""
  [(set (subreg:HI (match_operand:SI 0 "register_operand" "=r") 0)
	(div:HI (match_operand:SI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "div %2,%0"
  [(set_attr "length" "4")])

(define_expand "modhi3"
  [(set (subreg:HI (match_dup 1) 2)
	(mod:HI (match_operand:SI 1 "register_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))
   (set (match_operand:HI 0 "register_operand" "=r")
        (subreg:HI (match_dup 1) 2))]
  "TARGET_40_PLUS"
  "")

(define_insn ""
  [(set (subreg:HI (match_operand:SI 0 "register_operand" "=r") 2)
	(mod:HI (match_operand:SI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "div %2,%0"
  [(set_attr "length" "4")])

;(define_expand "divmodhi4"
;  [(parallel [(set (subreg:HI (match_dup 1) 0)
;	           (div:HI (match_operand:SI 1 "register_operand" "0")
;		           (match_operand:HI 2 "general_operand" "g")))
;              (set (subreg:HI (match_dup 1) 2)
;	           (mod:HI (match_dup 1)
;		           (match_dup 2)))])
;   (set (match_operand:HI 3 "register_operand" "=r")
;        (subreg:HI (match_dup 1) 2))
;   (set (match_operand:HI 0 "register_operand" "=r")
;        (subreg:HI (match_dup 1) 0))]
;  "TARGET_40_PLUS"
;  "")
;
;(define_insn ""
;  [(set (subreg:HI (match_operand:SI 0 "register_operand" "=r") 0)
;	           (div:HI (match_operand:SI 1 "general_operand" "0")
;		           (match_operand:HI 2 "general_operand" "g")))
;   (set (subreg:HI (match_dup 0) 2)
;	           (mod:HI (match_dup 1)
;		           (match_dup 2)))]
;  "TARGET_40_PLUS"
;  "div %2, %0")
;
   
;; is rotate doing the right thing to be included here ????
