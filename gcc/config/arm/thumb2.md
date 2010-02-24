;; ARM Thumb-2 Machine Description
;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.
;; Written by CodeSourcery, LLC.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; Note: Thumb-2 is the variant of the Thumb architecture that adds
;; 32-bit encodings of [almost all of] the Arm instruction set.
;; Some old documents refer to the relatively minor interworking
;; changes made in armv5t as "thumb2".  These are considered part
;; the 16-bit Thumb-1 instruction set.

(define_insn "*thumb2_incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "arm_comparison_operator"
                    [(match_operand:CC 3 "cc_register" "") (const_int 0)])
                 (match_operand:SI 1 "s_register_operand" "0,?r")))]
  "TARGET_THUMB2"
  "@
  it\\t%d2\;add%d2\\t%0, %1, #1
  ite\\t%D2\;mov%D2\\t%0, %1\;add%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "6,10")]
)

(define_insn "*thumb2_decscc"
  [(set (match_operand:SI            0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI  1 "s_register_operand" "0,?r")
		  (match_operator:SI 2 "arm_comparison_operator"
                   [(match_operand   3 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2"
  "@
   it\\t%d2\;sub%d2\\t%0, %1, #1
   ite\\t%D2\;mov%D2\\t%0, %1\;sub%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "6,10")]
)

;; Thumb-2 only allows shift by constant on data processing instructions 
(define_insn "*thumb_andsi_not_shiftsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operator:SI 4 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "const_int_operand" "M")]))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "bic%?\\t%0, %1, %2%S4"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "2")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_smaxsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=r,r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand"  "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand"    "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%1, %2\;it\\tlt\;movlt\\t%0, %2
   cmp\\t%1, %2\;it\\tge\;movge\\t%0, %1
   cmp\\t%1, %2\;ite\\tge\;movge\\t%0, %1\;movlt\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")]
)

(define_insn "*thumb2_sminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%1, %2\;it\\tge\;movge\\t%0, %2
   cmp\\t%1, %2\;it\\tlt\;movlt\\t%0, %1
   cmp\\t%1, %2\;ite\\tlt\;movlt\\t%0, %1\;movge\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")]
)

(define_insn "*thumb32_umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%1, %2\;it\\tcc\;movcc\\t%0, %2
   cmp\\t%1, %2\;it\\tcs\;movcs\\t%0, %1
   cmp\\t%1, %2\;ite\\tcs\;movcs\\t%0, %1\;movcc\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")]
)

(define_insn "*thumb2_uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%1, %2\;it\\tcs\;movcs\\t%0, %2
   cmp\\t%1, %2\;it\\tcc\;movcc\\t%0, %1
   cmp\\t%1, %2\;ite\\tcc\;movcc\\t%0, %1\;movcs\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")]
)

(define_insn "*thumb2_notsi_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 3 "shift_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "const_int_operand"  "M")])))]
  "TARGET_THUMB2"
  "mvn%?\\t%0, %1%S3"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_notsi_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "const_int_operand"  "M")]))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_op_dup 3 [(match_dup 1) (match_dup 2)])))]
  "TARGET_THUMB2"
  "mvn%.\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_not_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "const_int_operand"  "M")]))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_THUMB2"
  "mvn%.\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

;; Thumb-2 does not have rsc, so use a clever trick with shifter operands.
(define_insn "*thumb2_negdi2"
  [(set (match_operand:DI         0 "s_register_operand" "=&r,r")
	(neg:DI (match_operand:DI 1 "s_register_operand"  "?r,0")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "negs\\t%Q0, %Q1\;sbc\\t%R0, %R1, %R1, lsl #1"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*thumb2_abssi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%0, #0\;it\tlt\;rsblt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31"
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   ;; predicable can't be set based on the variant, so left as no
   (set_attr "length" "10,8")]
)

(define_insn "*thumb2_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "@
   cmp\\t%0, #0\;it\\tgt\;rsbgt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31"
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   ;; predicable can't be set based on the variant, so left as no
   (set_attr "length" "10,8")]
)

(define_insn "*thumb2_movdi"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r, r, r, r, m")
	(match_operand:DI 1 "di_operand"              "rDa,Db,Dc,mi,r"))]
  "TARGET_THUMB2
  && !(TARGET_HARD_FLOAT && (TARGET_MAVERICK || TARGET_VFP))
  && !TARGET_IWMMXT"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    default:
      return output_move_double (operands);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "*,*,*,load2,store2")
   (set_attr "pool_range" "*,*,*,4096,*")
   (set_attr "neg_pool_range" "*,*,*,0,*")]
)

(define_insn "*thumb2_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,r,r,rk,m")
	(match_operand:SI 1 "general_operand"	   "rk ,I,K,N,mi,rk"))]
  "TARGET_THUMB2 && ! TARGET_IWMMXT
   && !(TARGET_HARD_FLOAT && TARGET_VFP)
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   movw%?\\t%0, %1
   ldr%?\\t%0, %1
   str%?\\t%1, %0"
  [(set_attr "type" "*,*,*,*,load1,store1")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,*,*,*,4096,*")
   (set_attr "neg_pool_range" "*,*,*,*,0,*")]
)

;; ??? We can probably do better with thumb2
(define_insn "pic_load_addr_thumb2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "" "mX")] UNSPEC_PIC_SYM))]
  "TARGET_THUMB2 && flag_pic"
  "ldr%?\\t%0, %1"
  [(set_attr "type" "load1")
   (set_attr "pool_range" "4096")
   (set_attr "neg_pool_range" "0")]
)

;; Set reg to the address of this instruction plus four.  The low two
;; bits of the PC are always read as zero, so ensure the instructions is
;; word aligned.
(define_insn "pic_load_dot_plus_four"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 4)
		    (match_operand 1 "" "")]
		   UNSPEC_PIC_BASE))]
  "TARGET_THUMB2"
  "*
  assemble_align(BITS_PER_WORD);
  (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
			     INTVAL (operands[1]));
  /* We use adr because some buggy gas assemble add r8, pc, #0
     to add.w r8, pc, #0, not addw r8, pc, #0.  */
  asm_fprintf (asm_out_file, \"\\tadr\\t%r, %LLPIC%d + 4\\n\",
	       REGNO(operands[0]), (int)INTVAL (operands[1]));
  return \"\";
  "
  [(set_attr "length" "6")]
)

;; Thumb-2 always has load/store halfword instructions, so we can avoid a lot
;; of the messiness associated with the ARM patterns.
(define_insn "*thumb2_movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,m,r")    
	(match_operand:HI 1 "general_operand"      "rI,n,r,m"))]
  "TARGET_THUMB2"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   movw%?\\t%0, %L1\\t%@ movhi
   str%(h%)\\t%1, %0\\t%@ movhi
   ldr%(h%)\\t%0, %1\\t%@ movhi"
  [(set_attr "type" "*,*,store1,load1")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,*,*,4096")
   (set_attr "neg_pool_range" "*,*,*,250")]
)

(define_insn "*thumb2_movsf_soft_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "general_operand"  "r,mE,r"))]
  "TARGET_THUMB2
   && TARGET_SOFT_FLOAT
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], SFmode))"
  "@
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
  [(set_attr "predicable" "yes")
   (set_attr "type" "*,load1,store1")
   (set_attr "pool_range" "*,4096,*")
   (set_attr "neg_pool_range" "*,0,*")]
)

(define_insn "*thumb2_movdf_soft_insn"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=r,r,r,r,m")
	(match_operand:DF 1 "soft_df_operand" "rDa,Db,Dc,mF,r"))]
  "TARGET_THUMB2 && TARGET_SOFT_FLOAT
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    default:
      return output_move_double (operands);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "*,*,*,load2,store2")
   (set_attr "pool_range" "1020")
   (set_attr "neg_pool_range" "0")]
)

(define_insn "*thumb2_cmpsi_shiftsi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI   0 "s_register_operand" "r")
		    (match_operator:SI  3 "shift_operator"
		     [(match_operand:SI 1 "s_register_operand" "r")
		      (match_operand:SI 2 "const_int_operand"  "M")])))]
  "TARGET_THUMB2"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_cmpsi_shiftsi_swp"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (match_operator:SI 3 "shift_operator"
			 [(match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "const_int_operand" "M")])
			(match_operand:SI 0 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_cmpsi_neg_shiftsi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "s_register_operand" "r")
		    (neg:SI (match_operator:SI 3 "shift_operator"
			     [(match_operand:SI 1 "s_register_operand" "r")
			      (match_operand:SI 2 "const_int_operand" "M")]))))]
  "TARGET_THUMB2"
  "cmn%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_mov_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "arm_comparison_operator"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  "TARGET_THUMB2"
  "ite\\t%D1\;mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "10")]
)

(define_insn "*thumb2_mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2"
  "ite\\t%D1\;mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
  [(set_attr "conds" "use")
   (set_attr "length" "10")]
)

(define_insn "*thumb2_mov_notscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2"
  "ite\\t%D1\;mov%D1\\t%0, #0\;mvn%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "10")]
)

(define_insn "*thumb2_movsicc_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 3 "arm_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,0,rI,K,rI,rI,K,K")
	 (match_operand:SI 2 "arm_not_operand" "rI,K,0,0,rI,K,rI,K")))]
  "TARGET_THUMB2"
  "@
   it\\t%D3\;mov%D3\\t%0, %2
   it\\t%D3\;mvn%D3\\t%0, #%B2
   it\\t%d3\;mov%d3\\t%0, %1
   it\\t%d3\;mvn%d3\\t%0, #%B1
   ite\\t%d3\;mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   ite\\t%d3\;mov%d3\\t%0, %1\;mvn%D3\\t%0, #%B2
   ite\\t%d3\;mvn%d3\\t%0, #%B1\;mov%D3\\t%0, %2
   ite\\t%d3\;mvn%d3\\t%0, #%B1\;mvn%D3\\t%0, #%B2"
  [(set_attr "length" "6,6,6,6,10,10,10,10")
   (set_attr "conds" "use")]
)

(define_insn "*thumb2_movsfcc_soft_insn"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(if_then_else:SF (match_operator 3 "arm_comparison_operator"
			  [(match_operand 4 "cc_register" "") (const_int 0)])
			 (match_operand:SF 1 "s_register_operand" "0,r")
			 (match_operand:SF 2 "s_register_operand" "r,0")))]
  "TARGET_THUMB2 && TARGET_SOFT_FLOAT"
  "@
   it\\t%D3\;mov%D3\\t%0, %2
   it\\t%d3\;mov%d3\\t%0, %1"
  [(set_attr "length" "6,6")
   (set_attr "conds" "use")]
)

(define_insn "*call_reg_thumb2"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2"
  "blx%?\\t%0"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_reg_thumb2"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2"
  "blx\\t%1"
  [(set_attr "type" "call")]
)

(define_insn "*thumb2_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "l*r"))]
  "TARGET_THUMB2"
  "bx\\t%0"
  [(set_attr "conds" "clob")]
)
;; Don't define thumb2_load_indirect_jump because we can't guarantee label
;; addresses will have the thumb bit set correctly. 


;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn "*thumb2_arith_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operator:SI 1 "shiftable_operator"
          [(match_operator:SI 3 "shift_operator"
             [(match_operand:SI 4 "s_register_operand" "r")
              (match_operand:SI 5 "const_int_operand" "M")])
           (match_operand:SI 2 "s_register_operand" "r")]))]
  "TARGET_THUMB2"
  "%i1%?\\t%0, %2, %4%S3"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "4")
   (set_attr "type" "alu_shift")]
)

;; ??? What does this splitter do?  Copied from the ARM version
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "shiftable_operator"
	 [(match_operator:SI 2 "shiftable_operator"
	   [(match_operator:SI 3 "shift_operator"
	     [(match_operand:SI 4 "s_register_operand" "")
	      (match_operand:SI 5 "const_int_operand" "")])
	    (match_operand:SI 6 "s_register_operand" "")])
	  (match_operand:SI 7 "arm_rhs_operand" "")]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT"
  [(set (match_dup 8)
	(match_op_dup 2 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 6)]))
   (set (match_dup 0)
	(match_op_dup 1 [(match_dup 8) (match_dup 7)]))]
  "")

(define_insn "*thumb2_arith_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "const_int_operand" "M")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 1 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 2)]))]
  "TARGET_32BIT"
  "%i1%.\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_arith_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "const_int_operand" "M")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_THUMB2"
  "%i1%.\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_sub_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r")
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r")
		    (match_operand:SI 4 "const_int_operand" "M")])))]
  "TARGET_THUMB2"
  "sub%?\\t%0, %1, %3%S2"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "3")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_sub_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r")
		     (match_operand:SI 4 "const_int_operand" "M")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "TARGET_THUMB2"
  "sub%.\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_sub_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r")
		     (match_operand:SI 4 "const_int_operand" "M")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_THUMB2"
  "sub%.\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "type" "alu_shift")]
)

(define_insn "*thumb2_and_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "ite\\t%D1\;mov%D1\\t%0, #0\;and%d1\\t%0, %2, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "10")]
)

(define_insn "*thumb2_ior_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator:SI 2 "arm_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "0,?r")))]
  "TARGET_THUMB2"
  "@
   it\\t%d2\;orr%d2\\t%0, %1, #1
   ite\\t%D2\;mov%D2\\t%0, %1\;orr%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "6,10")]
)

(define_insn "*thumb2_compare_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator:SI 1 "arm_comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
    if (operands[3] == const0_rtx)
      {
	if (GET_CODE (operands[1]) == LT)
	  return \"lsr\\t%0, %2, #31\";

	if (GET_CODE (operands[1]) == GE)
	  return \"mvn\\t%0, %2\;lsr\\t%0, %0, #31\";

	if (GET_CODE (operands[1]) == EQ)
	  return \"rsbs\\t%0, %2, #1\;it\\tcc\;movcc\\t%0, #0\";
      }

    if (GET_CODE (operands[1]) == NE)
      {
        if (which_alternative == 1)
	  return \"adds\\t%0, %2, #%n3\;it\\tne\;movne\\t%0, #1\";
        return \"subs\\t%0, %2, %3\;it\\tne\;movne\\t%0, #1\";
      }
    if (which_alternative == 1)
      output_asm_insn (\"cmn\\t%2, #%n3\", operands);
    else
      output_asm_insn (\"cmp\\t%2, %3\", operands);
    return \"ite\\t%D1\;mov%D1\\t%0, #0\;mov%d1\\t%0, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "14")]
)

(define_insn "*thumb2_cond_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI (match_operator 3 "equality_operator"
			  [(match_operator 4 "arm_comparison_operator"
			    [(match_operand 5 "cc_register" "") (const_int 0)])
			   (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
			 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))]
  "TARGET_THUMB2"
  "*
    if (GET_CODE (operands[3]) == NE)
      {
        if (which_alternative != 1)
	  output_asm_insn (\"it\\t%D4\;mov%D4\\t%0, %2\", operands);
        if (which_alternative != 0)
	  output_asm_insn (\"it\\t%d4\;mov%d4\\t%0, %1\", operands);
        return \"\";
      }
    switch (which_alternative)
      {
      case 0:
	output_asm_insn (\"it\\t%d4\", operands);
	break;
      case 1:
	output_asm_insn (\"it\\t%D4\", operands);
	break;
      case 2:
	output_asm_insn (\"ite\\t%D4\", operands);
	break;
      default:
	abort();
      }
    if (which_alternative != 0)
      output_asm_insn (\"mov%D4\\t%0, %1\", operands);
    if (which_alternative != 1)
      output_asm_insn (\"mov%d4\\t%0, %2\", operands);
    return \"\";
  "
  [(set_attr "conds" "use")
   (set_attr "length" "6,6,10")]
)

(define_insn "*thumb2_cond_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (match_operator:SI 5 "shiftable_operator" 
	 [(match_operator:SI 4 "arm_comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
          (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
    if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
      return \"%i5\\t%0, %1, %2, lsr #31\";

    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (GET_CODE (operands[5]) == AND)
      {
	output_asm_insn (\"ite\\t%D4\", operands);
	output_asm_insn (\"mov%D4\\t%0, #0\", operands);
      }
    else if (GET_CODE (operands[5]) == MINUS)
      {
	output_asm_insn (\"ite\\t%D4\", operands);
	output_asm_insn (\"rsb%D4\\t%0, %1, #0\", operands);
      }
    else if (which_alternative != 0)
      {
	output_asm_insn (\"ite\\t%D4\", operands);
	output_asm_insn (\"mov%D4\\t%0, %1\", operands);
      }
    else
      output_asm_insn (\"it\\t%d4\", operands);
    return \"%i5%d4\\t%0, %1, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "14")]
)

(define_insn "*thumb2_cond_sub"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 4 "arm_comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (which_alternative != 0)
      {
	output_asm_insn (\"ite\\t%D4\", operands);
	output_asm_insn (\"mov%D4\\t%0, %1\", operands);
      }
    else
      output_asm_insn (\"it\\t%d4\", operands);
    return \"sub%d4\\t%0, %1, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "10,14")]
)

(define_insn "*thumb2_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
  if (GET_CODE (operands[3]) == LT && operands[2] == const0_rtx)
    return \"asr\\t%0, %1, #31\";

  if (GET_CODE (operands[3]) == NE)
    return \"subs\\t%0, %1, %2\;it\\tne\;mvnne\\t%0, #0\";

  output_asm_insn (\"cmp\\t%1, %2\", operands);
  output_asm_insn (\"ite\\t%D3\", operands);
  output_asm_insn (\"mov%D3\\t%0, #0\", operands);
  return \"mvn%d3\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "14")]
)

(define_insn "*thumb2_movcond"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL,rIL")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
	 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
  if (GET_CODE (operands[5]) == LT
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"and\\t%0, %1, %3, asr #31\";
	  return \"ands\\t%0, %1, %3, asr #32\;it\\tcc\;movcc\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  if (operands[1] == const0_rtx)
	    return \"bic\\t%0, %2, %3, asr #31\";
	  return \"bics\\t%0, %2, %3, asr #32\;it\\tcs\;movcs\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }

  if (GET_CODE (operands[5]) == GE
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"bic\\t%0, %1, %3, asr #31\";
	  return \"bics\\t%0, %1, %3, asr #32\;it\\tcs\;movcs\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  if (operands[1] == const0_rtx)
	    return \"and\\t%0, %2, %3, asr #31\";
	  return \"ands\\t%0, %2, %3, asr #32\;it\tcc\;movcc\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }
  if (GET_CODE (operands[4]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    output_asm_insn (\"cmp\\t%3, %4\", operands);
  switch (which_alternative)
    {
    case 0:
      output_asm_insn (\"it\\t%D5\", operands);
      break;
    case 1:
      output_asm_insn (\"it\\t%d5\", operands);
      break;
    case 2:
      output_asm_insn (\"ite\\t%d5\", operands);
      break;
    default:
      abort();
    }
  if (which_alternative != 0)
    output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  if (which_alternative != 1)
    output_asm_insn (\"mov%D5\\t%0, %2\", operands);
  return \"\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")]
)

;; Zero and sign extension instructions.

(define_insn "*thumb2_zero_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "*
    /* ??? Output both instructions unconditionally, otherwise the conditional
       execution insn counter gets confused.
    if (REGNO (operands[1])
        != REGNO (operands[0]) + (WORDS_BIG_ENDIAN ? 1 : 0)) */
      output_asm_insn (\"mov%?\\t%Q0, %1\", operands);
    return \"mov%?\\t%R0, #0\";
  "
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")]
)

(define_insn "*thumb2_zero_extendqidi2"
  [(set (match_operand:DI                 0 "s_register_operand"  "=r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2"
  "@
   and%?\\t%Q0, %1, #255\;mov%?\\t%R0, #0
   ldr%(b%)\\t%Q0, %1\;mov%?\\t%R0, #0"
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")
   (set_attr "type" "*,load_byte")
   (set_attr "pool_range" "*,4092")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "*thumb2_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "*
    /* ??? Output both instructions unconditionally, otherwise the conditional
       execution insn counter gets confused.
    if (REGNO (operands[1])
        != REGNO (operands[0]) + (WORDS_BIG_ENDIAN ? 1 : 0)) */
      output_asm_insn (\"mov%?\\t%Q0, %1\", operands);
    return \"asr%?\\t%R0, %Q0, #31\";
  "
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "shift" "1")
   (set_attr "predicable" "yes")]
)

;; All supported Thumb2 implementations are armv6, so only that case is
;; provided.
(define_insn "*thumb2_extendqisi_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   sxtb%?\\t%0, %1
   ldr%(sb%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4096")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "*thumb2_zero_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   uxth%?\\t%0, %1
   ldr%(h%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4096")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "*thumb2_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   uxtb%(%)\\t%0, %1
   ldr%(b%)\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4096")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "thumb2_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 4 "=&r"))
	      (use (label_ref (match_dup 2)))])]
  "TARGET_THUMB2 && !flag_pic"
  "* return thumb2_output_casesi(operands);"
  [(set_attr "conds" "clob")
   (set_attr "length" "16")]
)

(define_insn "thumb2_casesi_internal_pic"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 4 "=&r"))
	      (clobber (match_scratch:SI 5 "=r"))
	      (use (label_ref (match_dup 2)))])]
  "TARGET_THUMB2 && flag_pic"
  "* return thumb2_output_casesi(operands);"
  [(set_attr "conds" "clob")
   (set_attr "length" "20")]
)

(define_insn_and_split "thumb2_eh_return"
  [(unspec_volatile [(match_operand:SI 0 "s_register_operand" "r")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  "TARGET_THUMB2"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  {
    thumb_set_return_address (operands[0], operands[1]);
    DONE;
  }"
)

;; Peepholes and insns for 16-bit flag clobbering instructions.
;; The conditional forms of these instructions do not clobber CC.
;; However by the time peepholes are run it is probably too late to do
;; anything useful with this information.
(define_peephole2
  [(set (match_operand:SI          0 "low_register_operand" "")
        (match_operator:SI 3 "thumb_16bit_operator"
	 [(match_operand:SI 1  "low_register_operand" "")
	  (match_operand:SI 2 "low_register_operand" "")]))]
  "TARGET_THUMB2
   && (rtx_equal_p(operands[0], operands[1])
       || GET_CODE(operands[3]) == PLUS
       || GET_CODE(operands[3]) == MINUS)
   && peep2_regno_dead_p(0, CC_REGNUM)"
  [(parallel
    [(set (match_dup 0)
	  (match_op_dup 3
	   [(match_dup 1)
	    (match_dup 2)]))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_alusi3_short"
  [(set (match_operand:SI          0 "s_register_operand" "=l")
        (match_operator:SI 3 "thumb_16bit_operator"
	 [(match_operand:SI 1 "s_register_operand" "0")
	  (match_operand:SI 2 "s_register_operand" "l")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed
   && GET_CODE(operands[3]) != PLUS
   && GET_CODE(operands[3]) != MINUS"
  "%I3%!\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

;; Similarly for 16-bit shift instructions
;; There is no 16-bit rotate by immediate instruction.
(define_peephole2
  [(set (match_operand:SI   0 "low_register_operand" "")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "low_register_operand" "")
	  (match_operand:SI 2 "low_reg_or_int_operand" "")]))]
  "TARGET_THUMB2
   && peep2_regno_dead_p(0, CC_REGNUM)
   && ((GET_CODE(operands[3]) != ROTATE && GET_CODE(operands[3]) != ROTATERT)
       || REG_P(operands[2]))"
  [(parallel
    [(set (match_dup 0)
	  (match_op_dup 3
	   [(match_dup 1)
	    (match_dup 2)]))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_shiftsi3_short"
  [(set (match_operand:SI   0 "low_register_operand" "=l")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "low_register_operand"  "l")
	  (match_operand:SI 2 "low_reg_or_int_operand" "lM")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed
   && ((GET_CODE(operands[3]) != ROTATE && GET_CODE(operands[3]) != ROTATERT)
       || REG_P(operands[2]))"
  "* return arm_output_shift(operands, 2);"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set_attr "length" "2")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
)

;; 16-bit load immediate
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB2
   && peep2_regno_dead_p(0, CC_REGNUM)
   && (unsigned HOST_WIDE_INT) INTVAL(operands[1]) < 256"
  [(parallel
    [(set (match_dup 0)
	  (match_dup 1))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_movsi_shortim"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(match_operand:SI 1 "const_int_operand" "I"))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "mov%!\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

;; 16-bit add/sub immediate
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(plus:SI (match_operand:SI 1 "low_register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_THUMB2
   && peep2_regno_dead_p(0, CC_REGNUM)
   && ((rtx_equal_p(operands[0], operands[1])
	&& INTVAL(operands[2]) > -256 && INTVAL(operands[2]) < 256)
       || (INTVAL(operands[2]) > -8 && INTVAL(operands[2]) < 8))"
  [(parallel
    [(set (match_dup 0)
	  (plus:SI (match_dup 1)
		   (match_dup 2)))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_addsi_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l,l")
	(plus:SI (match_operand:SI 1 "low_register_operand" "l,0")
		 (match_operand:SI 2 "low_reg_or_int_operand" "lPt,Ps")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "*
    HOST_WIDE_INT val;

    if (GET_CODE (operands[2]) == CONST_INT)
      val = INTVAL(operands[2]);
    else
      val = 0;

    /* We prefer eg. subs rn, rn, #1 over adds rn, rn, #0xffffffff.  */
    if (val < 0 && const_ok_for_arm(ARM_SIGN_EXTEND (-val)))
      return \"sub%!\\t%0, %1, #%n2\";
    else
      return \"add%!\\t%0, %1, %2\";
  "
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

(define_insn "divsi3"
  [(set (match_operand:SI	  0 "s_register_operand" "=r")
	(div:SI (match_operand:SI 1 "s_register_operand"  "r")
		(match_operand:SI 2 "s_register_operand"  "r")))]
  "TARGET_THUMB2 && arm_arch_hwdiv"
  "sdiv%?\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "sdiv")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI	   0 "s_register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "s_register_operand"  "r")
		 (match_operand:SI 2 "s_register_operand"  "r")))]
  "TARGET_THUMB2 && arm_arch_hwdiv"
  "udiv%?\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "udiv")]
)

(define_insn "*thumb2_subsi_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(minus:SI (match_operand:SI 1 "low_register_operand" "l")
		  (match_operand:SI 2 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "sub%!\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

(define_insn "*thumb2_cbz"
  [(set (pc) (if_then_else
	      (eq (match_operand:SI 0 "s_register_operand" "l,?r")
		  (const_int 0))
	      (label_ref (match_operand 1 "" ""))
	      (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
  if (get_attr_length (insn) == 2)
    return \"cbz\\t%0, %l1\";
  else
    return \"cmp\\t%0, #0\;beq\\t%l1\";
  "
  [(set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 1) (pc)) (const_int 2))
	         (le (minus (match_dup 1) (pc)) (const_int 128))
	         (eq (symbol_ref ("which_alternative")) (const_int 0)))
	    (const_int 2)
	    (const_int 8)))]
)

(define_insn "*thumb2_cbnz"
  [(set (pc) (if_then_else
	      (ne (match_operand:SI 0 "s_register_operand" "l,?r")
		  (const_int 0))
	      (label_ref (match_operand 1 "" ""))
	      (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
  if (get_attr_length (insn) == 2)
    return \"cbnz\\t%0, %l1\";
  else
    return \"cmp\\t%0, #0\;bne\\t%l1\";
  "
  [(set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 1) (pc)) (const_int 2))
	         (le (minus (match_dup 1) (pc)) (const_int 128))
	         (eq (symbol_ref ("which_alternative")) (const_int 0)))
	    (const_int 2)
	    (const_int 8)))]
)

;; 16-bit complement
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(not:SI (match_operand:SI 1 "low_register_operand" "")))]
  "TARGET_THUMB2
   && peep2_regno_dead_p(0, CC_REGNUM)"
  [(parallel
    [(set (match_dup 0)
	  (not:SI (match_dup 1)))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_one_cmplsi2_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(not:SI (match_operand:SI 1 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "mvn%!\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

;; 16-bit negate
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(neg:SI (match_operand:SI 1 "low_register_operand" "")))]
  "TARGET_THUMB2
   && peep2_regno_dead_p(0, CC_REGNUM)"
  [(parallel
    [(set (match_dup 0)
	  (neg:SI (match_dup 1)))
     (clobber (reg:CC CC_REGNUM))])]
  ""
)

(define_insn "*thumb2_negsi2_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(neg:SI (match_operand:SI 1 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "neg%!\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")]
)

