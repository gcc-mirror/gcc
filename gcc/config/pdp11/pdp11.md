;;- Machine description for the pdp11 for GNU C compiler
;; Copyright (C) 1994-2021 Free Software Foundation, Inc.
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
    UNSPECV_CPYMEM
  ])

(define_constants
  [
   ;; Register numbers
   (R0_REGNUM     	  0)
   (RETVAL_REGNUM     	  0)
   (FRAME_POINTER_REGNUM  5)
   (STACK_POINTER_REGNUM  6)
   (PC_REGNUM             7)
   (AC0_REGNUM            8)
   (AC3_REGNUM            11)
   (AC4_REGNUM            12)
   (AC5_REGNUM            13)
   ;; The next one is not a physical register but is used for
   ;; addressing arguments.
   (ARG_POINTER_REGNUM    14)
   ;; Condition code registers
   (CC_REGNUM             15)
   (FCC_REGNUM            16)
   ;; End of hard registers
   (FIRST_PSEUDO_REGISTER 17)
   
   ;; Branch offset limits, as byte offsets from (pc).  That is NOT
   ;; the same thing as "instruction address" -- it is for backward
   ;; branches, but for forward branches it refers to the address
   ;; following the instruction.  So the max forward distance
   ;; matches what the processor handbook says, while the max
   ;; backward branch is 2 less than the book.
   (MIN_BRANCH            -254)
   (MAX_BRANCH            254)
   (MIN_SOB               -124)
   (MAX_SOB               0)])

;; DF is 64 bit
;; SF is 32 bit
;; SI is 32 bit
;; HI is 16 bit
;; QI is 8 bit 

;; Integer modes supported on the PDP11, with a mapping from machine mode
;; to mnemonic suffix.  SImode and DImode are usually special cases.
(define_mode_iterator PDPint [QI HI])
(define_mode_attr  isfx [(QI "b") (HI "")])
(define_mode_attr  mname [(QI "QImode") (HI "HImode") (SI "SImode") (DI "DImode")])
(define_mode_attr  e_mname [(QI "E_QImode") (HI "E_HImode") (SI "E_SImode") (DI "E_DImode")])
(define_mode_attr  hmode [(QI "hi") (HI "hi") (SI "si") (DI "di")])

;; These are analogous for use in splitters and expanders.
(define_mode_iterator HSint [HI SI])
(define_mode_iterator QHSint [QI HI SI])
(define_mode_iterator QHSDint [QI HI SI DI])

(define_code_iterator SHF [ashift ashiftrt lshiftrt])

(define_mode_iterator PDPfp [SF DF])

;; Substitution to turn a CC clobber into a CC setter.  We have four of
;; these: for CCmode vs. CCNZmode, and for CC_REGNUM vs. FCC_REGNUM.
(define_subst "cc_cc"
  [(set (match_operand 0 "") (match_operand 1 ""))
   (clobber (reg CC_REGNUM))]
  ""
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst "cc_ccnz"
  [(set (match_operand 0 "") (match_operand 1 ""))
   (clobber (reg CC_REGNUM))]
  ""
  [(set (reg:CCNZ CC_REGNUM)
	(compare:CCNZ (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst "fcc_cc"
  [(set (match_operand:PDPfp 0 "") (match_operand:PDPfp 1 ""))
   (clobber (reg FCC_REGNUM))]
  ""
  [(set (reg:CC FCC_REGNUM)
	(compare:CC (match_dup 1) (const_double_zero:PDPfp)))
   (set (match_dup 0) (match_dup 1))])

(define_subst "fcc_ccnz"
  [(set (match_operand:PDPfp 0 "") (match_operand:PDPfp 1 ""))
   (clobber (reg FCC_REGNUM))]
  ""
  [(set (reg:CCNZ FCC_REGNUM)
	(compare:CCNZ (match_dup 1) (const_double_zero:PDPfp)))
   (set (match_dup 0) (match_dup 1))])

(define_subst_attr "cc_cc" "cc_cc" "_nocc" "_cc")
(define_subst_attr "fcc_cc" "fcc_cc" "_nocc" "_cc")
(define_subst_attr "cc_ccnz" "cc_ccnz" "_nocc" "_cc")
(define_subst_attr "fcc_ccnz" "fcc_ccnz" "_nocc" "_cc")

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

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

;; define attributes
;; currently type is only fpu or arith or unknown, maybe branch later ?
;; default is arith
(define_attr "type" "unknown,arith,fp" (const_string "arith"))

;; length default is 2 bytes each
(define_attr "length" "" (const_int 2))

;; instruction base cost (not counting operands)
(define_attr "base_cost" "" (const_int 2))

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

(define_insn "rtspc"
  [(return)]
  ""
  "rts\tpc")

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
  [(set (reg:CC FCC_REGNUM)
	(compare:CC (match_operand:DF 0 "general_operand" "fR,fR,Q,QF")
		    (match_operand:DF 1 "register_or_const0_operand" "G,a,G,a")))]
  "TARGET_FPU && reload_completed"
  "*
{
  if (which_alternative == 0 || which_alternative == 2)
    return \"{tstd|tstf}\t%0\";
  else
    return \"{cmpd|cmpf}\t%0,%1\";
}"
  [(set_attr "length" "2,2,4,4")
   (set_attr "base_cost" "4")
   (set_attr "type" "fp")]) 

;; Copy floating point processor condition code register to main CPU
;; condition code register.
(define_insn "*cfcc"
  [(set (reg CC_REGNUM) (reg FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "cfcc")

(define_insn "cmp<mode>"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:PDPint 0 "general_operand" "rR,rR,rR,Q,Qi,Qi")
		    (match_operand:PDPint 1 "general_operand" "N,rR,Qi,N,rR,Qi")))]
  ""
  "@
   tst<PDPint:isfx>\t%0
   cmp<PDPint:isfx>\t%0,%1
   cmp<PDPint:isfx>\t%0,%1
   tst<PDPint:isfx>\t%0
   cmp<PDPint:isfx>\t%0,%1
   cmp<PDPint:isfx>\t%0,%1"
  [(set_attr "length" "2,2,4,4,4,6")])

;; Two word compare
(define_insn "cmpsi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "general_operand" "rDQi")
		    (match_operand:SI 1 "general_operand" "rDQi")))]
  ""
{
  rtx inops[2];
  rtx exops[2][2];
  rtx lb[1];
  
  inops[0] = operands[0];
  inops[1] = operands[1];
  pdp11_expand_operands (inops, exops, 2, 2, NULL, big);
  lb[0] = gen_label_rtx ();
  
  if (CONST_INT_P (exops[0][1]) && INTVAL (exops[0][1]) == 0)
   output_asm_insn ("tst\t%0", exops[0]);
  else
   output_asm_insn ("cmp\t%0,%1", exops[0]);
  output_asm_insn ("bne\t%l0", lb);
  if (CONST_INT_P (exops[1][1]) && INTVAL (exops[1][1]) == 0)
   output_asm_insn ("tst\t%0", exops[1]);
  else
   output_asm_insn ("cmp\t%0,%1", exops[1]);
  output_asm_label (lb[0]);
  fputs (":\n", asm_out_file);

  return "";
}
  [(set (attr "length")
	(symbol_ref "pdp11_cmp_length (operands, 2)"))
   (set_attr "base_cost" "0")])

;; Four word compare
(define_insn "cmpdi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:DI 0 "general_operand" "rDQi")
		    (match_operand:DI 1 "general_operand" "rDQi")))]
  ""
{
  rtx inops[4];
  rtx exops[4][2];
  rtx lb[1];
  int i;
  
  inops[0] = operands[0];
  inops[1] = operands[1];
  pdp11_expand_operands (inops, exops, 2, 4, NULL, big);
  lb[0] = gen_label_rtx ();

  for (i = 0; i < 3; i++)
    {
      if (CONST_INT_P (exops[i][1]) && INTVAL (exops[i][1]) == 0)
        output_asm_insn ("tst\t%0", exops[i]);
      else
        output_asm_insn ("cmp\t%0,%1", exops[i]);
       output_asm_insn ("bne\t%l0", lb);
     }
  if (CONST_INT_P (exops[3][1]) && INTVAL (exops[3][1]) == 0)
   output_asm_insn ("tst\t%0", exops[3]);
  else
   output_asm_insn ("cmp\t%0,%1", exops[3]);
  output_asm_label (lb[0]);
   fputs (":\n", asm_out_file);

  return "";
}
  [(set (attr "length")
	(symbol_ref "pdp11_cmp_length (operands, 2)"))
   (set_attr "base_cost" "0")])

;; sob instruction
;;
;; This expander has to check for mode match because the doloop pass
;; in gcc that invokes it does not do so, i.e., it may attempt to apply
;; this pattern even if the count operand is QI or SI mode.
(define_expand "doloop_end"
  [(parallel [(set (pc)
		   (if_then_else
		    (ne (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
			(const_int 1))
		    (label_ref (match_operand 1 "" ""))
		    (pc)))
	      (set (match_dup 0)
		   (plus:HI (match_dup 0)
			 (const_int -1)))])]
  "TARGET_40_PLUS"
  "{
    if (GET_MODE (operands[0]) != HImode)
      FAIL;
  }")

;; Do a define_split because some alternatives clobber CC.
;; Some don't, but it isn't all that interesting to cover that case.
(define_insn_and_split "doloop_end_insn"
  [(set (pc)
	(if_then_else
	 (ne (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
	     (const_int 1))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int -1)))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (pc)
		   (if_then_else
		    (ne (match_dup 0) (const_int 1))
		    (label_ref (match_dup 1))
		    (pc)))
	      (set (match_dup 0)
		   (plus:HI (match_dup 0)
			 (const_int -1)))
	      (clobber (reg:CC CC_REGNUM))])]
  "")

;; Note that there is a memory alternative here.  This is as documented
;; in gccint, which says that doloop_end, since it has both a jump and
;; an output interrupt "must handle its own reloads".  That translates
;; to: must accept memory operands as valid though they may be deprecated.
(define_insn "doloop_end_nocc"
  [(set (pc)
	(if_then_else
	 (ne (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
	     (const_int 1))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
	      (const_int -1)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "*
{
 rtx lb[1];

 if (get_attr_length (insn) == 2)
    return \"sob\t%0,%l1\";

 /* emulate sob */
 lb[0] = gen_label_rtx ();
 output_asm_insn (\"dec\t%0\", operands);
 output_asm_insn (\"beq\t%l0\", lb);
 output_asm_insn (\"jmp\t%l1\", operands);
 
 output_asm_label (lb[0]);
 fputs (\":\\n\", asm_out_file);

 return \"\";
}"
  [(set (attr "length")
        (if_then_else (eq (symbol_ref ("which_alternative")) (const_int 1))
                          (const_int 10)
                          (if_then_else (ior (lt (minus (match_dup 1) (pc))
					         (const_int MIN_SOB))
					     (gt (minus (match_dup 1) (pc))
					         (const_int MAX_SOB)))
				        (const_int 8)
				        (const_int 2))))])

;; These control RTL generation for conditional jump insns
;; and match them for register allocation.
;; Post reload these get expanded into insns that actually
;; manipulate the condition code registers.  We can't do that before
;; because instructions generated by reload clobber condition codes (new
;; CC design, type #2).
(define_insn_and_split "cbranchdf4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:DF 1 "general_operand" "fg")
			(match_operand:DF 2 "general_operand" "a")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(set (reg:CC FCC_REGNUM)
	(compare:CC (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (match_op_dup 0
                      [(reg:CC FCC_REGNUM) (const_int 0)])
		      (label_ref (match_dup 3))
		      (pc)))]
  "")

(define_insn_and_split "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:QHSDint 1 "general_operand" "g")
			(match_operand:QHSDint 2 "general_operand" "g")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (match_op_dup 0
                      [(reg:CC CC_REGNUM) (const_int 0)])
		      (label_ref (match_dup 3))
		      (pc)))]
  "")

;; This splitter turns a branch on float condition into a branch on
;; CPU condition, by adding a CFCC.
(define_split
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
                      [(reg:CC FCC_REGNUM) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_FPU && reload_completed"
  [(set (reg:CC CC_REGNUM) (reg:CC FCC_REGNUM))
   (set (pc)
	(if_then_else (match_op_dup 0
                      [(reg:CC CC_REGNUM) (const_int 0)])
		      (label_ref (match_dup 1))
		      (pc)))]
  "")

(define_insn "cond_branch"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(reg:CC CC_REGNUM) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "reload_completed"
  "* return output_jump (operands, 0, get_attr_length (insn));"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 1)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 1)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 6)
				      (const_int 2)))])

(define_insn "*branch"
  [(set (pc)
	(if_then_else (match_operator 0 "ccnz_operator"
		       [(reg:CCNZ CC_REGNUM) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "reload_completed"
  "* return output_jump (operands, 1, get_attr_length (insn));"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 1)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 1)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 6)
				      (const_int 2)))])


;; Move instructions

;; "length" is defined even though this pattern won't appear at
;; assembly language output time.  But the length is used by
;; pdp11_insn_cost, before the post-reload splitter adds the
;; CC clobber to the insn.
(define_insn "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,g")
	(match_operand:DI 1 "general_operand" "rN,g"))]
  ""
  ""
  [(set_attr "length" "16,32")])


(define_insn "*movdi_nocc"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,g")
	(match_operand:DI 1 "general_operand" "rN,g"))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "* return output_move_multiple (operands);"
  [(set_attr "length" "16,32")])

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,g,g")
	(match_operand:SI 1 "general_operand" "rN,IJ,IJ,g"))]
  ""
  ""
  [(set_attr "length" "4,6,8,16")])

(define_insn "*movsi_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,g,g")
	(match_operand:SI 1 "general_operand" "rN,IJ,IJ,g"))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "* return output_move_multiple (operands);"
  [(set_attr "length" "4,6,8,16")])

;; That long string of "Z" constraints enforces the restriction that
;; a register source and auto increment or decrement destination must
;; not use the same register, because that case is not consistently
;; implemented across the PDP11 models.
;; TODO: the same should be applied to insn like add, but this is not
;; necessary yet because the incdec optimization pass does not apply
;; that optimization to 3-operand insns at the moment.
(define_insn "mov<mode>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Za,Zb,Zc,Zd,Ze,Zf,Zg,rD,rR,Q,Q")
	(match_operand:PDPint 1 "general_operand" "RN,Z0,Z1,Z2,Z3,Z4,Z5,Z6,r,Qi,rRN,Qi"))]
  ""
  ""
  [(set_attr "length" "2,2,2,2,2,2,2,2,2,4,4,6")])

;; This splits all the integer moves: DI and SI modes as well as
;; the simple machine operations.
(define_split 
  [(set (match_operand:QHSDint 0 "nonimmediate_operand" "")
	(match_operand:QHSDint 1 "general_operand" ""))]
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (clobber (reg:CC CC_REGNUM))])]
  "")
  
;; MOV clears V
(define_insn "*mov<mode>_<cc_cc>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Za,Zb,Zc,Zd,Ze,Zf,Zg,rD,rR,Q,Q")
	(match_operand:PDPint 1 "general_operand" "RN,Z0,Z1,Z2,Z3,Z4,Z5,Z6,r,Qi,rRN,Qi"))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  if (operands[1] == const0_rtx)
    return \"clr<PDPint:isfx>\t%0\";

  return \"mov<PDPint:isfx>\t%1,%0\";
}"
  [(set_attr "length" "2,2,2,2,2,2,2,2,2,4,4,6")])

;; movdf has unusually complicated condition code handling, because
;; load (into float register) updates the FCC, while store (from
;; float register) leaves it untouched.
;;
;; 1. Loads are:  ac4, ac5, or non-register into load-register
;; 2. Stores are: load-register to non-register, ac4, or ac5
;; 3. Moves from ac0-ac3 to another ac0-ac3 can be handled
;;    either as loads or as stores.

(define_expand "movdf"
  [(set (match_operand:DF 0 "float_nonimm_operand" "")
        (match_operand:DF 1 "float_operand" ""))]
  "TARGET_FPU"
  "")

;; Splitter for all these cases.  Store is the first two
;; alternatives, which are not split.  Note that case 3
;; is treated as a store, i.e., not split.
(define_insn_and_split "movdf_split"
  [(set (match_operand:DF 0 "float_nonimm_operand" "=fR,FQ,a,a,a")
        (match_operand:DF 1 "float_operand" "a,a,hR,FQ,G"))]
  "TARGET_FPU"
  "*
  gcc_assert (which_alternative < 2);
  return \"std\t%1,%0\";
  "
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (clobber (reg:CC FCC_REGNUM))])]
  "{
  if (GET_CODE (operands[1]) == REG && 
      REGNO_REG_CLASS (REGNO (operands[1])) == LOAD_FPU_REGS)
    FAIL;
  }"
  [(set_attr "length" "2,4,0,0,0")])

;; Loads (case 1).  
(define_insn "*ldd<fcc_cc>"
  [(set (match_operand:DF 0 "float_nonimm_operand" "=a,a,a")
        (match_operand:DF 1 "float_operand" "hR,FQ,G"))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "@
  ldd\t%1,%0
  ldd\t%1,%0
  clrd\t%0"
  [(set_attr "length" "2,4,2")])

;; SFmode is easier because that uses convert load/store, which
;; always change condition codes.
;; Note that these insns are cheating a bit.  We actually have
;; DFmode operands in the FPU registers, which is why the
;; ldcfd and stcdf instructions appear.  But GCC likes to think
;; of these as SFmode loads and does the conversion once in the
;; register, at least in many cases.  So we pretend to do this,
;; but then extend and truncate register-to-register are NOP and
;; generate no code.
(define_insn_and_split "movsf"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=a,fR,a,Q,a")
        (match_operand:SF 1 "float_operand" "fRG,a,FQ,a,G"))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,2,4,4,2")])
  
(define_insn "*movsf<fcc_ccnz>"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=a,fR,a,Q,a")
        (match_operand:SF 1 "float_operand" "fR,a,FQ,a,G"))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "@
  {ldcfd|movof}\t%1,%0
  {stcdf|movfo}\t%1,%0
  {ldcfd|movof}\t%1,%0
  {stcdf|movfo}\t%1,%0
  clrf\t%0"
  [(set_attr "length" "2,2,4,4,2")])

;; Expand a block move.  We turn this into a move loop.
(define_expand "cpymemhi"
  [(parallel [(unspec_volatile [(const_int 0)] UNSPECV_CPYMEM)
	      (match_operand:BLK 0 "general_operand" "=g")
	      (match_operand:BLK 1 "general_operand" "g")
	      (match_operand:HI 2 "immediate_operand" "i")
	      (match_operand:HI 3 "immediate_operand" "i")
	      (clobber (mem:BLK (scratch)))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_dup 2))])]
  ""
  "
{
  int count;
  count = INTVAL (operands[2]);
  if (count == 0)
    DONE;
  if (INTVAL (operands [3]) >= 2 && (count & 1) == 0)
    count >>= 1;
  else
    operands[3] = const1_rtx;
  operands[2] = copy_to_mode_reg (HImode,
                                  gen_rtx_CONST_INT (HImode, count));

  /* Load BLKmode MEM addresses into scratch registers.  */
  operands[0] = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
}")

;; Expand a block move.  We turn this into a move loop.
(define_insn_and_split "cpymemhi1"
  [(unspec_volatile [(const_int 0)] UNSPECV_CPYMEM)
   (match_operand:HI 0 "register_operand" "+r")
   (match_operand:HI 1 "register_operand" "+r")
   (match_operand:HI 2 "register_operand" "+r")
   (match_operand:HI 3 "immediate_operand" "i")
   (clobber (mem:BLK (scratch)))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_dup 2))]
  ""
  "#"
  "reload_completed"
  [(parallel [(unspec_volatile [(const_int 0)] UNSPECV_CPYMEM)
	      (match_dup 0)
	      (match_dup 1)
	      (match_dup 2)
	      (match_dup 3)
	      (clobber (mem:BLK (scratch)))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_dup 2))
	      (clobber (reg:CC CC_REGNUM))])]
  "")

(define_insn "cpymemhi_nocc"
  [(unspec_volatile [(const_int 0)] UNSPECV_CPYMEM)
   (match_operand:HI 0 "register_operand" "+r")
   (match_operand:HI 1 "register_operand" "+r")
   (match_operand:HI 2 "register_operand" "+r")
   (match_operand:HI 3 "immediate_operand" "i")
   (clobber (mem:BLK (scratch)))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_dup 2))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  rtx lb[2];
  
  lb[0] = operands[2];
  lb[1] = gen_label_rtx ();
  
  output_asm_label (lb[1]);
  fputs (\":\n\", asm_out_file);
  if (INTVAL (operands[3]) > 1)
    output_asm_insn (\"mov\t(%1)+,(%0)+\", operands);
  else
    output_asm_insn (\"movb\t(%1)+,(%0)+\", operands);
  if (TARGET_40_PLUS)
    output_asm_insn (\"sob\t%0,%l1\", lb);
  else
    {
      output_asm_insn (\"dec\t%0\", lb);
      output_asm_insn (\"bne\t%l1\", lb);
    }
  return \"\";
}"
  [(set (attr "length")
	(if_then_else (match_test "TARGET_40_PLUS")
		      (const_int 4)
		      (const_int 6)))])

;;- truncation instructions

;; We sometimes end up doing a register to register truncate,
;; which isn't right because we actually load registers always
;; with a DFmode value.  But even with PROMOTE the compiler
;; doesn't always get that (so we don't use it).  That means
;; a register to register truncate is a NOP.
(define_insn_and_split  "truncdfsf2"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=f,R,Q")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "0,a,a")))]
  "TARGET_FPU"
  {
    gcc_assert (which_alternative == 0);
    return "";
  }	      
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (float_truncate:SF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
  "{
  if (GET_CODE (operands[0]) == REG && 
      GET_CODE (operands[1]) == REG && 
      REGNO (operands[0]) == REGNO (operands[1]))
    FAIL;
  }"
  [(set_attr "length" "0,0,0")])

(define_insn "*truncdfsf2_<fcc_cc>"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=R,Q")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "a,a")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
   "{stcdf|movfo}\t%1,%0"
  [(set_attr "length" "2,4")])


;;- zero extension instruction

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rD,Q,&r,&r")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "0,0,rR,Q")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (zero_extend:HI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  "{
    rtx r;

    if (!REG_P (operands[0]))
      {
        r = gen_rtx_MEM (QImode, operands[0]);
        adjust_address (r, QImode, 1);
        emit_move_insn (r, const0_rtx);
        DONE;
      }
    else if (!REG_P (operands[1]) ||
             REGNO (operands[0]) != REGNO (operands[1]))
      {
        /* Alternatives 2 and 3 */
        emit_move_insn (operands[0], const0_rtx);
        r = gen_rtx_REG (QImode, REGNO (operands[0]));
        emit_insn (gen_iorqi3_nocc (r, r, operands[1]));
        DONE;
      }
  }"
  [(set_attr "length" "4,4,4,6")])

(define_insn "*zero_extendqihi2<cc_cc>"
  [(parallel [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
		   (zero_extend:HI (match_operand:QI 1 "general_operand" "0,0")))
	      (clobber (reg:CC CC_REGNUM))])]
  "reload_completed"
  "bic\t%#0177400,%0"
  [(set_attr "length" "4,6")])
			 
;;- sign extension instructions

;; We sometimes end up doing a register to register extend,
;; which isn't right because we actually load registers always
;; with a DFmode value.  But even with PROMOTE the compiler
;; doesn't always get that (so we don't use it).  That means
;; a register to register truncate is a NOP.
(define_insn_and_split "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,a,a")
	(float_extend:DF (match_operand:SF 1 "float_operand" "0,R,Q")))]
  "TARGET_FPU"
  {
    gcc_assert (which_alternative == 0);
    return "";
  }	      
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (float_extend:DF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
  "{
  if (GET_CODE (operands[0]) == REG && 
      GET_CODE (operands[1]) == REG && 
      REGNO (operands[0]) == REGNO (operands[1]))
    FAIL;
  }"
  [(set_attr "length" "0,0,0")])

(define_insn "*extendsfdf2_<fcc_cc>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(float_extend:DF (match_operand:SF 1 "float_operand" "R,Q")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{ldcfd|movof}\t%1,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "6")])

;; movb sign extends if destination is a register
(define_insn_and_split "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "rR,Q")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:HI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; MOVB clears V
(define_insn "*extendqihi2<cc_cc>"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "rR,Q")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "movb\t%1,%0"
  [(set_attr "length" "2,4")])

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=o,<,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g,g,g")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:SI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "10,6,6")])

(define_insn "*extendhisi2_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=o,<,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g,g,g")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "*
{
  rtx latehalf[2];

  /* we don't want to mess with auto increment */
  
  switch (which_alternative)
  {
    case 0:

      latehalf[0] = operands[0];
      operands[0] = adjust_address(operands[0], HImode, 2);
  
      output_asm_insn(\"mov\t%1,%0\", operands);
      output_asm_insn(\"sxt\t%0\", latehalf);

      return \"\";

    case 1:

      /* - auto-decrement - right direction ;-) */
      output_asm_insn(\"mov\t%1,%0\", operands);
      output_asm_insn(\"sxt\t%0\", operands);

      return \"\";

    case 2:

      /* make register pair available */
      latehalf[0] = operands[0];
      operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);

      output_asm_insn(\"mov\t%1,%0\", operands);
      output_asm_insn(\"sxt\t%0\", latehalf);

      return \"\";

    default:

      gcc_unreachable ();
  }
}"
  [(set_attr "length" "10,6,6")])

;; make float to int and vice versa 
;; assume that we are normally in double and integer mode -
;; what do pdp library routines do to fpu mode ?

;; Note: the hardware treats register source as
;; a 16-bit (high order only) source, which isn't
;; what we want.  But we do need to support register
;; dest because gcc asks for it.
(define_insn_and_split "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,a,a")
	(float:DF (match_operand:SI 1 "general_operand" "r,R,Q")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (float:DF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "10,6,8")])

(define_insn "*floatsidf2<fcc_cc>"
  [(set (match_operand:DF 0 "register_operand" "=a,a,a")
	(float:DF (match_operand:SI 1 "general_operand" "r,R,Q")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "* if (which_alternative ==0)
     {
       rtx latehalf[2];
 
       latehalf[0] = NULL; 
       latehalf[1] = gen_rtx_REG (HImode, REGNO (operands[1]) + 1);
       output_asm_insn(\"mov\t%1,-(sp)\", latehalf);
       output_asm_insn(\"mov\t%1,-(sp)\", operands);
       
       output_asm_insn(\"setl\", operands);
       output_asm_insn(\"{ldcld|movif}\t(sp)+,%0\", operands);
       output_asm_insn(\"seti\", operands);
       return \"\";
     }
     else 
       return \"setl\;{ldcld|movif}\t%1,%0\;seti\";
  "
  [(set_attr "length" "10,6,8")
   (set_attr "base_cost" "12")])

(define_insn_and_split "floathidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(float:DF (match_operand:HI 1 "general_operand" "rR,Qi")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (float:DF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "*floathidf2<fcc_cc>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(float:DF (match_operand:HI 1 "general_operand" "rR,Qi")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{ldcid|movif}\t%1,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "12")])

;; cut float to int

;; Note: the hardware treats register destination as
;; a 16-bit (high order only) destination, which isn't
;; what we want.  But we do need to support register
;; dest because gcc asks for it.
(define_insn_and_split "fix_truncdfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,R,Q")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "a,a,a"))))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (fix:SI (fix:DF (match_dup 1))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "10,6,8")])

;; Note: this clobbers both sets of condition codes!
(define_insn "*fix_truncdfsi2_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,R,Q")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "a,a,a"))))
   (clobber (reg:CC CC_REGNUM))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "* if (which_alternative ==0)
     {
       output_asm_insn(\"setl\", operands);
       output_asm_insn(\"{stcdl|movfi}\t%1,-(sp)\", operands);
       output_asm_insn(\"seti\", operands);
       output_asm_insn(\"mov\t(sp)+,%0\", operands);
       operands[0] = gen_rtx_REG (HImode, REGNO (operands[0]) + 1);
       output_asm_insn(\"mov\t(sp)+,%0\", operands);
       return \"\";
     }
     else 
       return \"setl\;{stcdl|movfi}\t%1,%0\;seti\";
  "
  [(set_attr "length" "10,6,8")
   (set_attr "base_cost" "12")])

(define_insn_and_split "fix_truncdfhi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(fix:HI (fix:DF (match_operand:DF 1 "register_operand" "a,a"))))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (fix:HI (fix:DF (match_dup 1))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; Note: this clobbers both sets of condition codes!
(define_insn "*fix_truncdfhi2_nocc"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(fix:HI (fix:DF (match_operand:DF 1 "register_operand" "a,a"))))
   (clobber (reg:CC CC_REGNUM))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{stcdi|movfi}\t%1,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "12")])


;;- arithmetic instructions
;;- add instructions

(define_insn_and_split "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(plus:DF (match_operand:DF 1 "register_operand" "%0,0")
		 (match_operand:DF 2 "general_operand" "fR,QF")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:DF (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; Float add sets V if overflow from add
(define_insn "*adddf3<fcc_ccnz>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(plus:DF (match_operand:DF 1 "register_operand" "%0,0")
	      (match_operand:DF 2 "general_operand" "fR,QF")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{addd|addf}\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "6")])

(define_insn_and_split "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,0,0,0")
		 (match_operand:DI 2 "general_operand" "r,on,r,on")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "20,28,40,48")])

(define_insn "*adddi3_nocc"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,0,0,0")
	      (match_operand:DI 2 "general_operand" "r,on,r,on")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"  
  "*
{
  rtx inops[2];
  rtx exops[4][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, 4, NULL, big);
  
  if (!CONST_INT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"add\t%1,%0\", exops[0]);
  if (!CONST_INT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"add\t%1,%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  }
  if (!CONST_INT_P (exops[2][1]) || INTVAL (exops[2][1]) != 0)
  {
    output_asm_insn (\"add\t%1,%0\", exops[2]);
    output_asm_insn (\"adc\t%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  }
  if (!CONST_INT_P (exops[3][1]) || INTVAL (exops[3][1]) != 0)
  {
    output_asm_insn (\"add\t%1,%0\", exops[3]);
    output_asm_insn (\"adc\t%0\", exops[2]);
    output_asm_insn (\"adc\t%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "20,28,40,48")
   (set_attr "base_cost" "0")])

;; Note that the register operand is not marked earlyclobber.
;; The reason is that SI values go in register pairs, so they
;; can't partially overlap.  They can be either disjoint, or
;; source and destination can be equal.  The latter case is 
;; handled properly because of the ordering of the individual
;; instructions used.  Specifically, carry from the low to the
;; high word is added at the end, so the adding of the high parts
;; will always used the original high part and not a high part
;; modified by carry (which would amount to double carry).
(define_insn_and_split "addsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&r,r,o,o")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,0,0,0")
		 (match_operand:SI 2 "general_operand" "r,on,r,on")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "6,10,12,16")])

(define_insn "*addsi3_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&r,r,o,o")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,0,0,0")
	      (match_operand:SI 2 "general_operand" "r,on,r,on")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  rtx inops[2];
  rtx exops[2][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, 2, NULL, big);
  
  if (!CONST_INT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"add\t%1,%0\", exops[0]);
  if (!CONST_INT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"add\t%1,%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "6,10,12,16")
   (set_attr "base_cost" "0")])

(define_insn_and_split "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(plus:HI (match_operand:HI 1 "general_operand" "%0,0,0,0")
		 (match_operand:HI 2 "general_operand" "rRLM,Qi,rRLM,Qi")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4,4,6")])

;; Add sets V if overflow from the add
(define_insn "*addhi3<cc_ccnz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(plus:HI (match_operand:HI 1 "general_operand" "%0,0,0,0")
	         (match_operand:HI 2 "general_operand" "rRLM,Qi,rRLM,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL(operands[2]) == 1)
	return \"inc\t%0\";
      else if (INTVAL(operands[2]) == -1)
        return \"dec\t%0\";
    }

  return \"add\t%2,%0\";
}"
  [(set_attr "length" "2,4,4,6")])

(define_insn_and_split "addqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=rR,Q")
	(plus:QI (match_operand:QI 1 "general_operand" "%0,0")
		 (match_operand:QI 2 "incdec_operand" "LM,LM")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:QI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; Inc/dec sets V if overflow from the operation
(define_insn "*addqi3<cc_ccnz>"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=rR,Q")
	(plus:QI (match_operand:QI 1 "general_operand" "%0,0")
	         (match_operand:QI 2 "incdec_operand" "LM,LM")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  if (INTVAL(operands[2]) == 1)
    return \"incb\t%0\";
  else
    return \"decb\t%0\";
}"
  [(set_attr "length" "2,4")])


;;- subtract instructions
;; we don't have to care for constant second 
;; args, since they are canonical plus:xx now!
;; also for minus:DF ??

(define_insn_and_split "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(minus:DF (match_operand:DF 1 "register_operand" "0,0")
		  (match_operand:DF 2 "general_operand" "fR,Q")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (minus:DF (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "*subdf3<fcc_ccnz>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(minus:DF (match_operand:DF 1 "register_operand" "0,0")
	          (match_operand:DF 2 "general_operand" "fR,QF")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{subd|subf}\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "6")])

(define_insn_and_split "subdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(minus:DI (match_operand:DI 1 "general_operand" "0,0,0,0")
		 (match_operand:DI 2 "general_operand" "r,on,r,on")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (minus:DI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "20,28,40,48")])

(define_insn "*subdi3_nocc"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&r,r,o,o")
	(minus:DI (match_operand:DI 1 "general_operand" "0,0,0,0")
	      (match_operand:DI 2 "general_operand" "r,on,r,on")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  rtx inops[2];
  rtx exops[4][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, 4, NULL, big);
  
  if (!CONST_INT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"sub\t%1,%0\", exops[0]);
  if (!CONST_INT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"sub\t%1,%0\", exops[1]);
    output_asm_insn (\"sbc\t%0\", exops[0]);
  }
  if (!CONST_INT_P (exops[2][1]) || INTVAL (exops[2][1]) != 0)
  {
    output_asm_insn (\"sub\t%1,%0\", exops[2]);
    output_asm_insn (\"sbc\t%0\", exops[1]);
    output_asm_insn (\"sbc\t%0\", exops[0]);
  }
  if (!CONST_INT_P (exops[3][1]) || INTVAL (exops[3][1]) != 0)
  {
    output_asm_insn (\"sub\t%1,%0\", exops[3]);
    output_asm_insn (\"sbc\t%0\", exops[2]);
    output_asm_insn (\"sbc\t%0\", exops[1]);
    output_asm_insn (\"sbc\t%0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "20,28,40,48")
   (set_attr "base_cost" "0")])

(define_insn_and_split "subsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&r,r,o,o")
	(minus:SI (match_operand:SI 1 "general_operand" "0,0,0,0")
		 (match_operand:SI 2 "general_operand" "r,on,r,on")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "6,10,12,16")])

(define_insn "*subsi3_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&r,r,o,o")
	(minus:SI (match_operand:SI 1 "general_operand" "0,0,0,0")
	      (match_operand:SI 2 "general_operand" "r,on,r,on")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  rtx inops[2];
  rtx exops[2][2];
  
  inops[0] = operands[0];
  inops[1] = operands[2];
  pdp11_expand_operands (inops, exops, 2, 2, NULL, big);
  
  if (!CONST_INT_P (exops[0][1]) || INTVAL (exops[0][1]) != 0)
    output_asm_insn (\"sub\t%1,%0\", exops[0]);
  if (!CONST_INT_P (exops[1][1]) || INTVAL (exops[1][1]) != 0)
  {
    output_asm_insn (\"sub\t%1,%0\", exops[1]);
    output_asm_insn (\"sbc\t%0\", exops[0]);
  }

  return \"\";
}"
  [(set_attr "length" "6,10,12,16")
   (set_attr "base_cost" "0")])

(define_insn_and_split "subhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(minus:HI (match_operand:HI 1 "general_operand" "0,0,0,0")
		  (match_operand:HI 2 "general_operand" "rRLM,Qi,rRLM,Qi")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (minus:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4,4,6")])

;; Note: the manual says that (minus m (const_int n)) is converted
;; to (plus m (const_int -n)) but that does not appear to be
;; the case when it's wrapped in a PARALLEL.  So instead we handle
;; that case here, which is easy enough.
(define_insn "*subhi3<cc_ccnz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(minus:HI (match_operand:HI 1 "general_operand" "0,0,0,0")
	          (match_operand:HI 2 "general_operand" "rRLM,Qi,rRLM,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL(operands[2]) == 1)
	return \"dec\t%0\";
      else if (INTVAL(operands[2]) == -1)
        return \"inc\t%0\";
    }

  return \"sub\t%2,%0\";
}"
  [(set_attr "length" "2,4,4,6")])

(define_insn_and_split "subqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=rR,Q")
	(plus:QI (match_operand:QI 1 "general_operand" "%0,0")
		 (match_operand:QI 2 "incdec_operand" "LM,LM")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:QI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; Inc/dec sets V if overflow from the operation
(define_insn "*subqi3<cc_ccnz>"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=rR,Q")
	(plus:QI (match_operand:QI 1 "general_operand" "%0,0")
	         (match_operand:QI 2 "incdec_operand" "LM,LM")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "*
{
  if (INTVAL(operands[2]) == -1)
    return \"incb\t%0\";
  else
    return \"decb\t%0\";
}"
  [(set_attr "length" "2,4")])

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
}"
  [(set_attr "length" "2,4,4,6")])

(define_insn_and_split "*bic<mode>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(and:PDPint
	     (not: PDPint (match_operand:PDPint 1 "general_operand" "rR,Qi,rR,Qi"))
	     (match_operand:PDPint 2 "general_operand" "0,0,0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (and:PDPint (not:PDPint (match_dup 1)) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  "")

(define_insn "*bic<mode><cc_cc>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(and:PDPint
	     (not: PDPint (match_operand:PDPint 1 "general_operand" "rR,Qi,rR,Qi"))
			  (match_operand:PDPint 2 "general_operand" "0,0,0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "bic<PDPint:isfx>\t%1,%0"
  [(set_attr "length" "2,4,4,6")])

;;- Bit set (inclusive or) instructions
(define_insn_and_split "ior<mode>3"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(ior:PDPint (match_operand:PDPint 1 "general_operand" "%0,0,0,0")
		    (match_operand:PDPint 2 "general_operand" "rR,Qi,rR,Qi")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:PDPint (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4,4,6")])

(define_insn "ior<mode>3<cc_cc>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,rR,Q,Q")
	(ior:PDPint (match_operand:PDPint 1 "general_operand" "%0,0,0,0")
	     (match_operand:PDPint 2 "general_operand" "rR,Qi,rR,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "bis<PDPint:isfx>\t%2,%0"
  [(set_attr "length" "2,4,4,6")])

;;- xor instructions
(define_insn_and_split "xorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(xor:HI (match_operand:HI 1 "general_operand" "%0,0")
		(match_operand:HI 2 "register_operand" "r,r")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (xor:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "*xorhi3<cc_cc>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(xor:HI (match_operand:HI 1 "general_operand" "%0,0")
	     (match_operand:HI 2 "register_operand" "r,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "xor\t%2,%0"
  [(set_attr "length" "2,4")])

;;- one complement instructions

(define_insn_and_split "one_cmpl<mode>2"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
        (not:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (not:PDPint (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "*one_cmpl<mode>2<cc_cc>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
	(not:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "com<PDPint:isfx>\t%0"
  [(set_attr "length" "2,4")])

;;- arithmetic shift instructions
;;
;; There is a fair amount of complexity here because with -m10
;; (pdp-11/10, /20) we only have shift by one bit.  Iterators are
;; used to reduce the amount of very similar code.
;;
;; First the insns used for small constant shifts.
(define_insn_and_split "<code><mode>_sc"
  [(set (match_operand:QHSint 0 "nonimmediate_operand" "=rD,Q")
	(SHF:QHSint (match_operand:QHSint 1 "general_operand" "0,0")
	            (match_operand:HI 2 "expand_shift_operand" "O,O")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (SHF:QHSint (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set (attr "length")
	(symbol_ref "pdp11_shift_length (operands, <QHSint:mname>, 
                                         <CODE>, which_alternative == 0)"))
   (set_attr "base_cost" "0")])

(define_insn "<code><mode>_sc<cc_ccnz>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rD,Q")
	(SHF:PDPint (match_operand:PDPint 1 "general_operand" "0,0")
	     (match_operand:HI 2 "expand_shift_operand" "O,O")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "* return pdp11_assemble_shift (operands, <PDPint:mname>, <CODE>);"
  [(set (attr "length")
	(symbol_ref "pdp11_shift_length (operands, <PDPint:mname>, 
                                         <CODE>, which_alternative == 0)"))
   (set_attr "base_cost" "0")])

;; This one comes only in clobber flavor.
(define_insn "<code>si_sc_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rD,Q")
	(SHF:SI (match_operand:SI 1 "general_operand" "0,0")
	     (match_operand:HI 2 "expand_shift_operand" "O,O")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "* return pdp11_assemble_shift (operands, SImode, <CODE>);"
  [(set (attr "length")
	(symbol_ref "pdp11_shift_length (operands, SImode, 
                                         <CODE>, which_alternative == 0)"))
   (set_attr "base_cost" "0")])

;; Next, shifts that are done as a loop on base (11/10 class) machines.
;; This applies to shift counts too large to unroll, or variable shift
;; counts.  The check for count <= 0 is done before we get here.
(define_insn_and_split "<code><mode>_base"
  [(set (match_operand:QHSint 0 "nonimmediate_operand" "=rD,Q")
	(SHF:QHSint (match_operand:QHSint 1 "general_operand" "0,0")
	     (match_operand:HI 2 "register_operand" "r,r")))
   (clobber (match_dup 2))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (SHF:QHSint (match_dup 1) (match_dup 2)))
	      (clobber (match_dup 2))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set (attr "length")
	(symbol_ref "pdp11_shift_length (operands, <QHSint:mname>, 
                                         <CODE>, which_alternative == 0)"))
   (set_attr "base_cost" "0")])

(define_insn "<code><mode>_base_nocc"
  [(set (match_operand:QHSint 0 "nonimmediate_operand" "=rD,Q")
	(SHF:QHSint (match_operand:QHSint 1 "general_operand" "0,0")
	     (match_operand:HI 2 "register_operand" "r,r")))
   (clobber (match_dup 2))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  "* return pdp11_assemble_shift (operands, <QHSint:mname>, <CODE>);"
  [(set (attr "length")
	(symbol_ref "pdp11_shift_length (operands, <QHSint:mname>, 
                                         <CODE>, which_alternative == 0)"))
   (set_attr "base_cost" "0")])

;; Next the insns that use the extended instructions ash and ashc.
;; Note that these are just left shifts, and HI/SI only.  (Right shifts
;; are done by shifting by a negative amount.)
(define_insn_and_split "aslhi_op"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r")
	(ashift:HI (match_operand:HI 1 "general_operand" "0,0")
	               (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])

(define_insn "aslhi_op<cc_ccnz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r")
	(ashift:HI (match_operand:HI 1 "general_operand" "0,0")
		(match_operand:HI 2 "general_operand" "rR,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "ash\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])

(define_insn_and_split "aslsi_op"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "general_operand" "0,0")
	           (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])

(define_insn "aslsi_op_<cc_ccnz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "general_operand" "0,0")
		(match_operand:HI 2 "general_operand" "rR,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "ashc\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])

;; Now the expanders that produce the insns defined above. 
(define_expand "ashl<mode>3"
  [(match_operand:QHSint 0 "nonimmediate_operand" "")
   (match_operand:QHSint 1 "general_operand" "")
   (match_operand:HI 2 "general_operand" "")]
  ""
  "
{
  rtx r;

  if (!pdp11_expand_shift (operands, gen_ashift<mode>_sc, gen_ashift<mode>_base))
    {
      if (<QHSint:e_mname> == E_QImode)
        {
          r = copy_to_mode_reg (HImode, gen_rtx_ZERO_EXTEND (HImode, operands[1]));
          emit_insn (gen_aslhi_op (r, r, operands[2]));
          emit_insn (gen_movqi (operands[0], gen_rtx_SUBREG (QImode, r, 0)));
        }
      else
        {
          emit_insn (gen_asl<QHSint:hmode>_op (operands[0], operands[1], operands[2]));
        }
    }
  DONE;
}")

(define_expand "ashr<mode>3"
  [(match_operand:QHSint 0 "nonimmediate_operand" "")
   (match_operand:QHSint 1 "general_operand" "")
   (match_operand:HI 2 "general_operand" "")]
  ""
  "
{
  rtx r;

  if (!pdp11_expand_shift (operands, gen_ashiftrt<mode>_sc, gen_ashiftrt<mode>_base))
    {
      operands[2] = negate_rtx (HImode, operands[2]);
      if (<QHSint:e_mname> == E_QImode)
        {
          r = copy_to_mode_reg (HImode, gen_rtx_ZERO_EXTEND (HImode, operands[1]));
          emit_insn (gen_aslhi_op (r, r, operands[2]));
          emit_insn (gen_movqi (operands[0], gen_rtx_SUBREG (QImode, r, 0)));
        }
      else
        {
          emit_insn (gen_asl<QHSint:hmode>_op (operands[0], operands[1], operands[2]));
        }
    }
  DONE;
}")

(define_expand "lshr<mode>3"
  [(match_operand:QHSint 0 "nonimmediate_operand" "")
   (match_operand:QHSint 1 "general_operand" "")
   (match_operand:HI 2 "general_operand" "")]
  ""
  "
{
  rtx r, n;

  if (!pdp11_expand_shift (operands, gen_lshiftrt<mode>_sc, gen_lshiftrt<mode>_base))
    {
      if (<QHSint:e_mname> == E_QImode)
        {
          r = copy_to_mode_reg (HImode, gen_rtx_ZERO_EXTEND (HImode, operands[1]));
          emit_insn (gen_aslhi_op (r, r, operands[2]));
          emit_insn (gen_movqi (operands[0], gen_rtx_SUBREG (QImode, r, 0)));
        }
      else
        {
          r = gen_reg_rtx (<QHSint:mname>);
          emit_insn (gen_lshiftrt<mode>_sc (r, operands[1], const1_rtx));
          if (GET_CODE (operands[2]) != CONST_INT)
            {
              n = gen_reg_rtx (HImode);
              emit_insn (gen_addhi3 (n, operands [2], GEN_INT (-1)));
              emit_insn (gen_ashr<mode>3 (operands[0], r, n));
            }
          else
            emit_insn (gen_asl<QHSint:hmode>_op (operands[0], r,
				  GEN_INT (1 - INTVAL (operands[2]))));
        }
    }
  DONE;
}")

;; absolute 

(define_insn_and_split "absdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fR,Q")
	(abs:DF (match_operand:DF 1 "general_operand" "0,0")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (abs:DF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
   ""
  [(set_attr "length" "2,4")])

(define_insn "absdf2<fcc_cc>"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fR,Q")
	(abs:DF (match_operand:DF 1 "general_operand" "0,0")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{absd|absf}\t%0"
  [(set_attr "length" "2,4")])

;; negate insns

(define_insn_and_split "negdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fR,Q")
	(neg:DF (match_operand:DF 1 "general_operand" "0,0")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (neg:DF (match_dup 1)))
	      (clobber (reg:CC FCC_REGNUM))])]
   ""
  [(set_attr "length" "2,4")])

(define_insn "negdf2<fcc_cc>"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fR,Q")
	(neg:DF (match_operand:DF 1 "general_operand" "0,0")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{negd|negf}\t%0"
  [(set_attr "length" "2,4")])

(define_insn_and_split "negdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,o")
	(neg:DI (match_operand:DI 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (neg:DI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "18,34")])
  
;; TODO: this can be neg/adc/neg/adc... I believe.  Check.  Saves one word.
(define_insn "negdi2_nocc"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,o")
	(neg:DI (match_operand:DI 1 "general_operand" "0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  {
    rtx inops[2];
    rtx exops[4][2];

    inops[0] = operands[0];
    pdp11_expand_operands (inops, exops, 1, 4, NULL, big);
  
    output_asm_insn (\"com\t%0\", exops[3]);
    output_asm_insn (\"com\t%0\", exops[2]);
    output_asm_insn (\"com\t%0\", exops[1]);
    output_asm_insn (\"com\t%0\", exops[0]);
    output_asm_insn (\"add\t%#1,%0\", exops[3]);
    output_asm_insn (\"adc\t%0\", exops[2]);
    output_asm_insn (\"adc\t%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  
    return \"\";
  }
  [(set_attr "length" "18,34")
   (set_attr "base_cost" "0")])

(define_insn_and_split "negsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,o")
	(neg:SI (match_operand:SI 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (neg:SI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "10,18")])
  
;; TODO: this can be neg/adc/neg/adc... I believe.  Check.  Saves one word.
(define_insn "negsi2_nocc"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,o")
	(neg:SI (match_operand:SI 1 "general_operand" "0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "reload_completed"
  {
    rtx inops[2];
    rtx exops[4][2];

    inops[0] = operands[0];
    pdp11_expand_operands (inops, exops, 1, 2, NULL, big);
  
    output_asm_insn (\"com\t%0\", exops[1]);
    output_asm_insn (\"com\t%0\", exops[0]);
    output_asm_insn (\"add\t%#1,%0\", exops[1]);
    output_asm_insn (\"adc\t%0\", exops[0]);
  
    return \"\";
  }
  [(set_attr "length" "10,18")
   (set_attr "base_cost" "0")])

(define_insn_and_split "neg<mode>2"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
	(neg:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (neg:PDPint (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])
  
(define_insn "neg<mode>2<cc_ccnz>"
  [(set (match_operand:PDPint 0 "nonimmediate_operand" "=rR,Q")
	(neg:PDPint (match_operand:PDPint 1 "general_operand" "0,0")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "neg<PDPint:isfx>\t%0"
  [(set_attr "length" "2,4")])


;; Unconditional and other jump instructions
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (get_attr_length (insn) == 2)
    return \"br\t%l0\";
  return \"jmp\t%l0\";
}"
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 0)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 0)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 4)
				      (const_int 2)))])

(define_insn "tablejump"
  [(set (pc) (match_operand:HI 0 "general_operand" "r,R,Q"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "@
  jmp\t(%0)
  jmp\t%@%0
  jmp\t%@%0"
  [(set_attr "length" "2,2,4")])

;; indirect jump.  TODO: this needs a constraint that allows memory
;; references but not indirection, since we add a level of indirection
;; in the generated code.
(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "general_operand" "r"))]
  ""
  "jmp\t@%0"
  [(set_attr "length" "2")])

;;- jump to subroutine

(define_insn "call"
  [(call (match_operand:HI 0 "general_operand" "rR,Q")
	 (match_operand:HI 1 "general_operand" "g,g"))]
  ;;- Don't use operand 1 for most machines.
  ""
  "jsr\tpc,%0"
  [(set_attr "length" "2,4")])

;;- jump to subroutine
(define_insn "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:HI 1 "general_operand" "rR,Q")
	      (match_operand:HI 2 "general_operand" "g,g")))]
  ;;- Don't use operand 2 for most machines.
  ""
  "jsr\tpc,%1"
  [(set_attr "length" "2,4")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

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
})

;;- nop instruction
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")


;;- multiply 

(define_insn_and_split "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(mult:DF (match_operand:DF 1 "register_operand" "%0,0")
		 (match_operand:DF 2 "float_operand" "fR,QF")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (mult:DF (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC FCC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "muldf3<fcc_ccnz>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(mult:DF (match_operand:DF 1 "register_operand" "%0,0")
	      (match_operand:DF 2 "float_operand" "fR,QF")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{muld|mulf}\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "20")])

;; 16 bit result multiply.  This uses odd numbered registers.

(define_insn_and_split "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d") ; multiply regs
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (mult:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "mulhi3<cc_cc>"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
	      (match_operand:HI 2 "general_operand" "rR,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "mul\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "20")])

;; 32 bit result from 16 bit operands
(define_insn_and_split "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0,0"))
	         (sign_extend:SI (match_operand:HI 2 "general_operand" "rR,Qi"))))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (mult:SI (sign_extend:SI (match_dup 1))
			 (sign_extend:SI (match_dup 2))))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "mulhisi3<cc_cc>"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0,0"))
	      (sign_extend:SI (match_operand:HI 2 "general_operand" "rR,Qi"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "mul\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "20")])

;;- divide
(define_insn_and_split "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(div:DF (match_operand:DF 1 "register_operand" "0,0")
		(match_operand:DF 2 "general_operand" "fR,QF")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0) (div:DF (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC FCC_REGNUM))])]
   ""
  [(set_attr "length" "2,4")])
  
(define_insn "divdf3<fcc_ccnz>"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(div:DF (match_operand:DF 1 "register_operand" "0,0")
	     (match_operand:DF 2 "general_operand" "fR,QF")))
   (clobber (reg:CC FCC_REGNUM))]
  "TARGET_FPU && reload_completed"
  "{divd|divf}\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "20")])

(define_expand "divmodhi4"
  [(parallel
    [(set (subreg:HI (match_dup 1) 0)
	(div:HI (match_operand:SI 1 "register_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))
     (set (subreg:HI (match_dup 1) 2)
	(mod:HI (match_dup 1) (match_dup 2)))])
   (set (match_operand:HI 0 "register_operand" "=r")
        (subreg:HI (match_dup 1) 0))
   (set (match_operand:HI 3 "register_operand" "=r")
        (subreg:HI (match_dup 1) 2))]
  "TARGET_40_PLUS"
  "")

(define_insn_and_split "*divmodhi4"
  [(set (subreg:HI (match_operand:SI 0 "register_operand" "=r,r") 0)
	(div:HI (match_operand:SI 1 "register_operand" "0,0")
	     (match_operand:HI 2 "general_operand" "rR,Qi")))
   (set (subreg:HI (match_dup 1) 2)
	(mod:HI (match_dup 1) (match_dup 2)))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (subreg:HI (match_dup 0) 0)
		   (div:HI (match_dup 1) (match_dup 2)))
	      (set (subreg:HI (match_dup 1) 2)
		   (mod:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

;; Note that there is no corresponding CC setter pattern.
;; The reason is that it won't be generated, because
;; compare-elim.c only does the transformation on input
;; insns that have a two-element PARALLEL, as opposed to
;; the three-element one we have here.     
(define_insn "divmodhi4_nocc"
  [(set (subreg:HI (match_operand:SI 0 "register_operand" "=r,r") 0)
	(div:HI (match_operand:SI 1 "register_operand" "0,0")
	        (match_operand:HI 2 "general_operand" "rR,Qi")))
   (set (subreg:HI (match_dup 1) 2)
	(mod:HI (match_dup 1) (match_dup 2)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS"
   "div\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "40")])

;; Byte swap
(define_insn_and_split "bswaphi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(bswap:HI (match_operand:HI 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (bswap:HI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")])

(define_insn "bswaphi2<cc_ccnz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rR,Q")
	(bswap:HI (match_operand:HI 1 "general_operand" "0,0")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "swab\t%0"
  [(set_attr "length" "2,4")])

(define_insn_and_split "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(bswap:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (bswap:SI (match_dup 1)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "10")])

(define_insn "bswapsi2_nocc"
  [(set (match_operand:SI 0 "register_operand" "=&r,&r,&r")
	(bswap:SI (match_operand:SI 1 "general_operand" "r,D,Q")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  {
    rtx inops[2];
    rtx exops[2][2];
    rtx t;

    inops[0] = operands[0];
    inops[1] = operands[1];
    pdp11_expand_operands (inops, exops, 2, 2, NULL, either);

    t = exops[0][0];
    exops[0][0] = exops[1][0];
    exops[1][0] = t;

    output_asm_insn ("mov\t%0,%1", exops[0]);
    output_asm_insn ("mov\t%0,%1", exops[1]);
    output_asm_insn ("swab\t%0", exops[0]);
    output_asm_insn ("swab\t%0", exops[1]);
    return "";
  }
  [(set_attr "length" "8,10,12")])

(define_expand "rotrhi3"
  [(match_operand:HI 0 "register_operand" "")
   (match_operand:HI 1 "register_operand" "")
   (match_operand:HI 2 "general_operand" "")]
  "TARGET_40_PLUS"
  "
{
  operands[2] = negate_rtx (HImode, operands[2]);
  emit_insn (gen_rotlhi3 (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn_and_split "rotlhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(rotate:HI (match_operand:HI 1 "register_operand" "0,0")
	           (match_operand:HI 2 "general_operand" "rR,Qi")))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
		   (rotate:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])

(define_insn "rotlhi3<cc_ccnz>"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(rotate:HI (match_operand:HI 1 "register_operand" "0,0")
		   (match_operand:HI 2 "general_operand" "rR,Qi")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_40_PLUS && reload_completed"
  "ashc\t%2,%0"
  [(set_attr "length" "2,4")
   (set_attr "base_cost" "8")])


  
;; Some peephole optimizations

;; Move then conditional branch on the result of the move is handled
;; by compare elimination, but an earlier pass sometimes changes the
;; compare operand to the move input, and then the compare is not
;; eliminated.  Do so here.
(define_peephole2
  [(parallel [(set (match_operand:PDPint 0 "nonimmediate_operand" "")
		   (match_operand:PDPint 1 "general_operand" ""))
	      (clobber (reg:CC CC_REGNUM))])
   (set (reg:CC CC_REGNUM) (compare:CC (match_dup 1) (const_int 0)))]
  ""
  [(parallel [(set (reg:CC CC_REGNUM) (compare:CC (match_dup 1) (const_int 0)))
	      (set (match_dup 0) (match_dup 1))])]
  "")
