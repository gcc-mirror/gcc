;; ARM VFP instruction patterns
;; Copyright (C) 2003-2016 Free Software Foundation, Inc.
;; Written by CodeSourcery.
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

;; Patterns for HI moves which provide more data transfer instructions when VFP
;; support is enabled.
(define_insn "*arm_movhi_vfp"
 [(set
   (match_operand:HI 0 "nonimmediate_operand"
    "=rk,  r, r, m, r, *t,  r, *t")
   (match_operand:HI 1 "general_operand"
    "rIk, K, n, r, mi, r, *t, *t"))]
 "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP
  && !TARGET_VFP_FP16INST
  && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
{
  switch (which_alternative)
    {
    case 0:
      return "mov%?\t%0, %1\t%@ movhi";
    case 1:
      return "mvn%?\t%0, #%B1\t%@ movhi";
    case 2:
      return "movw%?\t%0, %L1\t%@ movhi";
    case 3:
      return "strh%?\t%1, %0\t%@ movhi";
    case 4:
      return "ldrh%?\t%0, %1\t%@ movhi";
    case 5:
    case 6:
      return "vmov%?\t%0, %1\t%@ int";
    case 7:
      return "vmov%?.f32\t%0, %1\t%@ int";
    default:
      gcc_unreachable ();
    }
}
 [(set_attr "predicable" "yes")
  (set_attr_alternative "type"
   [(if_then_else
     (match_operand 1 "const_int_operand" "")
     (const_string "mov_imm")
     (const_string "mov_reg"))
    (const_string "mvn_imm")
    (const_string "mov_imm")
    (const_string "store1")
    (const_string "load1")
    (const_string "f_mcr")
    (const_string "f_mrc")
    (const_string "fmov")])
  (set_attr "arch" "*, *, v6t2, *, *, *, *, *")
  (set_attr "pool_range" "*, *, *, *, 256, *, *, *")
  (set_attr "neg_pool_range" "*, *, *, *, 244, *, *, *")
  (set_attr "length" "4")]
)

(define_insn "*thumb2_movhi_vfp"
 [(set
   (match_operand:HI 0 "nonimmediate_operand"
    "=rk, r, l, r, m, r, *t, r, *t")
   (match_operand:HI 1 "general_operand"
    "rk, I, Py, n, r, m, r, *t, *t"))]
 "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP
  && !TARGET_VFP_FP16INST
  && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
{
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return "mov%?\t%0, %1\t%@ movhi";
    case 3:
      return "movw%?\t%0, %L1\t%@ movhi";
    case 4:
      return "strh%?\t%1, %0\t%@ movhi";
    case 5:
      return "ldrh%?\t%0, %1\t%@ movhi";
    case 6:
    case 7:
      return "vmov%?\t%0, %1\t%@ int";
    case 8:
      return "vmov%?.f32\t%0, %1\t%@ int";
    default:
      gcc_unreachable ();
    }
}
 [(set_attr "predicable" "yes")
  (set_attr "predicable_short_it"
   "yes, no, yes, no, no, no, no, no, no")
  (set_attr "type"
   "mov_reg, mov_imm, mov_imm, mov_imm, store1, load1,\
    f_mcr, f_mrc, fmov")
  (set_attr "arch" "*, *, *, v6t2, *, *, *, *, *")
  (set_attr "pool_range" "*, *, *, *, *, 4094, *, *, *")
  (set_attr "neg_pool_range" "*, *, *, *, *, 250, *, *, *")
  (set_attr "length" "2, 4, 2, 4, 4, 4, 4, 4, 4")]
)

;; Patterns for HI moves which provide more data transfer instructions when FP16
;; instructions are available.
(define_insn "*arm_movhi_fp16"
 [(set
   (match_operand:HI 0 "nonimmediate_operand"
    "=r,  r, r, m, r, *t,  r, *t")
   (match_operand:HI 1 "general_operand"
    "rIk, K, n, r, mi, r, *t, *t"))]
 "TARGET_ARM && TARGET_VFP_FP16INST
  && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
{
  switch (which_alternative)
    {
    case 0:
      return "mov%?\t%0, %1\t%@ movhi";
    case 1:
      return "mvn%?\t%0, #%B1\t%@ movhi";
    case 2:
      return "movw%?\t%0, %L1\t%@ movhi";
    case 3:
      return "strh%?\t%1, %0\t%@ movhi";
    case 4:
      return "ldrh%?\t%0, %1\t%@ movhi";
    case 5:
    case 6:
      return "vmov.f16\t%0, %1\t%@ int";
    case 7:
      return "vmov%?.f32\t%0, %1\t%@ int";
    default:
      gcc_unreachable ();
    }
}
 [(set_attr "predicable" "yes, yes, yes, yes, yes, no, no, yes")
  (set_attr_alternative "type"
   [(if_then_else
     (match_operand 1 "const_int_operand" "")
     (const_string "mov_imm")
     (const_string "mov_reg"))
    (const_string "mvn_imm")
    (const_string "mov_imm")
    (const_string "store1")
    (const_string "load1")
    (const_string "f_mcr")
    (const_string "f_mrc")
    (const_string "fmov")])
  (set_attr "arch" "*, *, v6t2, *, *, *, *, *")
  (set_attr "pool_range" "*, *, *, *, 256, *, *, *")
  (set_attr "neg_pool_range" "*, *, *, *, 244, *, *, *")
  (set_attr "length" "4")]
)

(define_insn "*thumb2_movhi_fp16"
 [(set
   (match_operand:HI 0 "nonimmediate_operand"
    "=rk, r, l, r, m, r, *t, r, *t")
   (match_operand:HI 1 "general_operand"
    "rk, I, Py, n, r, m, r, *t, *t"))]
 "TARGET_THUMB2 && TARGET_VFP_FP16INST
  && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
{
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return "mov%?\t%0, %1\t%@ movhi";
    case 3:
      return "movw%?\t%0, %L1\t%@ movhi";
    case 4:
      return "strh%?\t%1, %0\t%@ movhi";
    case 5:
      return "ldrh%?\t%0, %1\t%@ movhi";
    case 6:
    case 7:
      return "vmov.f16\t%0, %1\t%@ int";
    case 8:
      return "vmov%?.f32\t%0, %1\t%@ int";
    default:
      gcc_unreachable ();
    }
}
 [(set_attr "predicable"
   "yes, yes, yes, yes, yes, yes, no, no, yes")
  (set_attr "predicable_short_it"
   "yes, no, yes, no, no, no, no, no, no")
  (set_attr "type"
   "mov_reg, mov_imm, mov_imm, mov_imm, store1, load1,\
    f_mcr, f_mrc, fmov")
  (set_attr "arch" "*, *, *, v6t2, *, *, *, *, *")
  (set_attr "pool_range" "*, *, *, *, *, 4094, *, *, *")
  (set_attr "neg_pool_range" "*, *, *, *, *, 250, *, *, *")
  (set_attr "length" "2, 4, 2, 4, 4, 4, 4, 4, 4")]
)

;; SImode moves
;; ??? For now do not allow loading constants into vfp regs.  This causes
;; problems because small constants get converted into adds.
(define_insn "*arm_movsi_vfp"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,r,r,rk,m ,*t,r,*t,*t, *Uv")
      (match_operand:SI 1 "general_operand"	   "rk, I,K,j,mi,rk,r,*t,*t,*Uvi,*t"))]
  "TARGET_ARM && TARGET_VFP && TARGET_HARD_FLOAT
   && (   s_register_operand (operands[0], SImode)
       || s_register_operand (operands[1], SImode))"
  "*
  switch (which_alternative)
    {
    case 0: case 1:
      return \"mov%?\\t%0, %1\";
    case 2:
      return \"mvn%?\\t%0, #%B1\";
    case 3:
      return \"movw%?\\t%0, %1\";
    case 4:
      return \"ldr%?\\t%0, %1\";
    case 5:
      return \"str%?\\t%1, %0\";
    case 6:
      return \"vmov%?\\t%0, %1\\t%@ int\";
    case 7:
      return \"vmov%?\\t%0, %1\\t%@ int\";
    case 8:
      return \"vmov%?.f32\\t%0, %1\\t%@ int\";
    case 9: case 10:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "type" "mov_reg,mov_reg,mvn_imm,mov_imm,load1,store1,
		     f_mcr,f_mrc,fmov,f_loads,f_stores")
   (set_attr "pool_range"     "*,*,*,*,4096,*,*,*,*,1020,*")
   (set_attr "neg_pool_range" "*,*,*,*,4084,*,*,*,*,1008,*")]
)

;; See thumb2.md:thumb2_movsi_insn for an explanation of the split
;; high/low register alternatives for loads and stores here.
;; The l/Py alternative should come after r/I to ensure that the short variant
;; is chosen with length 2 when the instruction is predicated for
;; arm_restrict_it.
(define_insn "*thumb2_movsi_vfp"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,l,r,r, l,*hk,m, *m,*t, r,*t,*t,  *Uv")
	(match_operand:SI 1 "general_operand"	   "rk,I,Py,K,j,mi,*mi,l,*hk, r,*t,*t,*Uvi,*t"))]
  "TARGET_THUMB2 && TARGET_VFP && TARGET_HARD_FLOAT
   && (   s_register_operand (operands[0], SImode)
       || s_register_operand (operands[1], SImode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"mov%?\\t%0, %1\";
    case 3:
      return \"mvn%?\\t%0, #%B1\";
    case 4:
      return \"movw%?\\t%0, %1\";
    case 5:
    case 6:
      return \"ldr%?\\t%0, %1\";
    case 7:
    case 8:
      return \"str%?\\t%1, %0\";
    case 9:
      return \"vmov%?\\t%0, %1\\t%@ int\";
    case 10:
      return \"vmov%?\\t%0, %1\\t%@ int\";
    case 11:
      return \"vmov%?.f32\\t%0, %1\\t%@ int\";
    case 12: case 13:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no,yes,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "type" "mov_reg,mov_reg,mov_reg,mvn_reg,mov_imm,load1,load1,store1,store1,f_mcr,f_mrc,fmov,f_loads,f_stores")
   (set_attr "length" "2,4,2,4,4,4,4,4,4,4,4,4,4,4")
   (set_attr "pool_range"     "*,*,*,*,*,1018,4094,*,*,*,*,*,1018,*")
   (set_attr "neg_pool_range" "*,*,*,*,*,   0,   0,*,*,*,*,*,1008,*")]
)


;; DImode moves

(define_insn "*movdi_vfp"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r,r,r,r,q,q,m,w,r,w,w, Uv")
       (match_operand:DI 1 "di_operand"              "r,rDa,Db,Dc,mi,mi,q,r,w,w,Uvi,w"))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP && arm_tune != cortexa8
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))
   && !(TARGET_NEON && CONST_INT_P (operands[1])
        && neon_immediate_valid_for_move (operands[1], DImode, NULL, NULL))"
  "*
  switch (which_alternative)
    {
    case 0: 
    case 1:
    case 2:
    case 3:
      return \"#\";
    case 4:
    case 5:
    case 6:
      return output_move_double (operands, true, NULL);
    case 7:
      return \"vmov%?\\t%P0, %Q1, %R1\\t%@ int\";
    case 8:
      return \"vmov%?\\t%Q0, %R0, %P1\\t%@ int\";
    case 9:
      if (TARGET_VFP_SINGLE)
	return \"vmov%?.f32\\t%0, %1\\t%@ int\;vmov%?.f32\\t%p0, %p1\\t%@ int\";
      else
	return \"vmov%?.f64\\t%P0, %P1\\t%@ int\";
    case 10: case 11:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "type" "multiple,multiple,multiple,multiple,load2,load2,store2,f_mcrr,f_mrrc,ffarithd,f_loadd,f_stored")
   (set (attr "length") (cond [(eq_attr "alternative" "1,4,5,6") (const_int 8)
                              (eq_attr "alternative" "2") (const_int 12)
                              (eq_attr "alternative" "3") (const_int 16)
                              (eq_attr "alternative" "9")
                               (if_then_else
                                 (match_test "TARGET_VFP_SINGLE")
                                 (const_int 8)
                                 (const_int 4))]
                              (const_int 4)))
   (set_attr "arm_pool_range"     "*,*,*,*,1020,4096,*,*,*,*,1020,*")
   (set_attr "thumb2_pool_range"     "*,*,*,*,1018,4094,*,*,*,*,1018,*")
   (set_attr "neg_pool_range" "*,*,*,*,1004,0,*,*,*,*,1004,*")
   (set_attr "arch"           "t2,any,any,any,a,t2,any,any,any,any,any,any")]
)

(define_insn "*movdi_vfp_cortexa8"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r,r,r,r,r,r,m,w,!r,w,w, Uv")
       (match_operand:DI 1 "di_operand"              "r,rDa,Db,Dc,mi,mi,r,r,w,w,Uvi,w"))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP && arm_tune == cortexa8
    && (   register_operand (operands[0], DImode)
        || register_operand (operands[1], DImode))
    && !(TARGET_NEON && CONST_INT_P (operands[1])
	 && neon_immediate_valid_for_move (operands[1], DImode, NULL, NULL))"
  "*
  switch (which_alternative)
    {
    case 0: 
    case 1:
    case 2:
    case 3:
      return \"#\";
    case 4:
    case 5:
    case 6:
      return output_move_double (operands, true, NULL);
    case 7:
      return \"vmov%?\\t%P0, %Q1, %R1\\t%@ int\";
    case 8:
      return \"vmov%?\\t%Q0, %R0, %P1\\t%@ int\";
    case 9:
      return \"vmov%?.f64\\t%P0, %P1\\t%@ int\";
    case 10: case 11:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "type" "multiple,multiple,multiple,multiple,load2,load2,store2,f_mcrr,f_mrrc,ffarithd,f_loadd,f_stored")
   (set (attr "length") (cond [(eq_attr "alternative" "1") (const_int 8)
                               (eq_attr "alternative" "2") (const_int 12)
                               (eq_attr "alternative" "3") (const_int 16)
                               (eq_attr "alternative" "4,5,6") 
			       (symbol_ref 
				"arm_count_output_move_double_insns (operands) \
                                 * 4")]
                              (const_int 4)))
   (set_attr "predicable"    "yes")
   (set_attr "arm_pool_range"     "*,*,*,*,1018,4094,*,*,*,*,1018,*")
   (set_attr "thumb2_pool_range"     "*,*,*,*,1018,4094,*,*,*,*,1018,*")
   (set_attr "neg_pool_range" "*,*,*,*,1004,0,*,*,*,*,1004,*")
   (set (attr "ce_count") 
	(symbol_ref "get_attr_length (insn) / 4"))
   (set_attr "arch"           "t2,any,any,any,a,t2,any,any,any,any,any,any")]
 )

;; HFmode moves

(define_insn "*movhf_vfp_fp16"
  [(set (match_operand:HF 0 "nonimmediate_operand"
			  "= r,m,t,r,t,r,t,t,Um,r")
	(match_operand:HF 1 "general_operand"
			  "  m,r,t,r,r,t,Dv,Um,t,F"))]
  "TARGET_32BIT
   && TARGET_VFP_FP16INST
   && (s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
 {
  switch (which_alternative)
    {
    case 0: /* ARM register from memory.  */
      return \"ldrh%?\\t%0, %1\\t%@ __fp16\";
    case 1: /* Memory from ARM register.  */
      return \"strh%?\\t%1, %0\\t%@ __fp16\";
    case 2: /* S register from S register.  */
      return \"vmov\\t%0, %1\t%@ __fp16\";
    case 3: /* ARM register from ARM register.  */
      return \"mov%?\\t%0, %1\\t%@ __fp16\";
    case 4: /* S register from ARM register.  */
    case 5: /* ARM register from S register.  */
    case 6: /* S register from immediate.  */
      return \"vmov.f16\\t%0, %1\t%@ __fp16\";
    case 7: /* S register from memory.  */
      return \"vld1.16\\t{%z0}, %A1\";
    case 8: /* Memory from S register.  */
      return \"vst1.16\\t{%z1}, %A0\";
    case 9: /* ARM register from constant.  */
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			       HFmode);
	ops[0] = operands[0];
	ops[1] = GEN_INT (bits);
	ops[2] = GEN_INT (bits & 0xff00);
	ops[3] = GEN_INT (bits & 0x00ff);

	if (arm_arch_thumb2)
	  output_asm_insn (\"movw\\t%0, %1\", ops);
	else
	  output_asm_insn (\"mov\\t%0, %2\;orr\\t%0, %0, %3\", ops);
	return \"\";
       }
    default:
      gcc_unreachable ();
    }
 }
  [(set_attr "predicable" "yes, yes, no, yes, no, no, no, no, no, no")
   (set_attr "predicable_short_it" "no, no, no, yes,\
				    no, no, no, no,\
				    no, no")
   (set_attr_alternative "type"
    [(const_string "load1") (const_string "store1")
     (const_string "fmov") (const_string "mov_reg")
     (const_string "f_mcr") (const_string "f_mrc")
     (const_string "fconsts") (const_string "neon_load1_1reg")
     (const_string "neon_store1_1reg")
     (if_then_else (match_test "arm_arch_thumb2")
      (const_string "mov_imm")
      (const_string "multiple"))])
   (set_attr_alternative "length"
    [(const_int 4) (const_int 4)
     (const_int 4) (const_int 4)
     (const_int 4) (const_int 4)
     (const_int 4) (const_int 4)
     (const_int 4)
     (if_then_else (match_test "arm_arch_thumb2")
      (const_int 4)
      (const_int 8))])]
)

(define_insn "*movhf_vfp_neon"
  [(set (match_operand:HF 0 "nonimmediate_operand" "= t,Um,r,m,t,r,t,r,r")
	(match_operand:HF 1 "general_operand"	   " Um, t,m,r,t,r,r,t,F"))]
  "TARGET_32BIT
   && TARGET_HARD_FLOAT && TARGET_NEON_FP16
   && !TARGET_VFP_FP16INST
   && (   s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 0:     /* S register from memory */
      return \"vld1.16\\t{%z0}, %A1\";
    case 1:     /* memory from S register */
      return \"vst1.16\\t{%z1}, %A0\";
    case 2:     /* ARM register from memory */
      return \"ldrh\\t%0, %1\\t%@ __fp16\";
    case 3:     /* memory from ARM register */
      return \"strh\\t%1, %0\\t%@ __fp16\";
    case 4:	/* S register from S register */
      return \"vmov.f32\\t%0, %1\";
    case 5:	/* ARM register from ARM register */
      return \"mov\\t%0, %1\\t%@ __fp16\";
    case 6:	/* S register from ARM register */
      return \"vmov\\t%0, %1\";
    case 7:	/* ARM register from S register */
      return \"vmov\\t%0, %1\";
    case 8:	/* ARM register from constant */
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			       HFmode);
	ops[0] = operands[0];
	ops[1] = GEN_INT (bits);
	ops[2] = GEN_INT (bits & 0xff00);
	ops[3] = GEN_INT (bits & 0x00ff);

	if (arm_arch_thumb2)
	  output_asm_insn (\"movw\\t%0, %1\", ops);
	else
	  output_asm_insn (\"mov\\t%0, %2\;orr\\t%0, %0, %3\", ops);
	return \"\";
       }
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "conds" "unconditional")
   (set_attr "type" "neon_load1_1reg,neon_store1_1reg,\
                     load1,store1,fmov,mov_reg,f_mcr,f_mrc,multiple")
   (set_attr "length" "4,4,4,4,4,4,4,4,8")]
)

;; FP16 without element load/store instructions.
(define_insn "*movhf_vfp"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=r,m,t,r,t,r,r")
	(match_operand:HF 1 "general_operand"	   " m,r,t,r,r,t,F"))]
  "TARGET_32BIT
   && TARGET_HARD_FLOAT && TARGET_VFP
   && !TARGET_NEON_FP16
   && !TARGET_VFP_FP16INST
   && (   s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 0:     /* ARM register from memory */
      return \"ldrh\\t%0, %1\\t%@ __fp16\";
    case 1:     /* memory from ARM register */
      return \"strh\\t%1, %0\\t%@ __fp16\";
    case 2:	/* S register from S register */
      return \"vmov.f32\\t%0, %1\";
    case 3:	/* ARM register from ARM register */
      return \"mov\\t%0, %1\\t%@ __fp16\";
    case 4:	/* S register from ARM register */
      return \"vmov\\t%0, %1\";
    case 5:	/* ARM register from S register */
      return \"vmov\\t%0, %1\";
    case 6:	/* ARM register from constant */
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			       HFmode);
	ops[0] = operands[0];
	ops[1] = GEN_INT (bits);
	ops[2] = GEN_INT (bits & 0xff00);
	ops[3] = GEN_INT (bits & 0x00ff);

	if (arm_arch_thumb2)
	  output_asm_insn (\"movw\\t%0, %1\", ops);
	else
	  output_asm_insn (\"mov\\t%0, %2\;orr\\t%0, %0, %3\", ops);
	return \"\";
       }
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "conds" "unconditional")
   (set_attr "type" "load1,store1,fmov,mov_reg,f_mcr,f_mrc,multiple")
   (set_attr "length" "4,4,4,4,4,4,8")]
)


;; SFmode moves
;; Disparage the w<->r cases because reloading an invalid address is
;; preferable to loading the value via integer registers.

(define_insn "*movsf_vfp"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=t,?r,t ,t  ,Uv,r ,m,t,r")
        (match_operand:SF 1 "general_operand"	   " ?r,t,Dv,UvE,t, mE,r,t,r"))]
  "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP
   && (   s_register_operand (operands[0], SFmode)
       || s_register_operand (operands[1], SFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"vmov%?\\t%0, %1\";
    case 1:
      return \"vmov%?\\t%0, %1\";
    case 2:
      return \"vmov%?.f32\\t%0, %1\";
    case 3: case 4:
      return output_move_vfp (operands);
    case 5:
      return \"ldr%?\\t%0, %1\\t%@ float\";
    case 6:
      return \"str%?\\t%1, %0\\t%@ float\";
    case 7:
      return \"vmov%?.f32\\t%0, %1\";
    case 8:
      return \"mov%?\\t%0, %1\\t%@ float\";
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "type"
     "f_mcr,f_mrc,fconsts,f_loads,f_stores,load1,store1,fmov,mov_reg")
   (set_attr "pool_range" "*,*,*,1020,*,4096,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,1008,*,4080,*,*,*")]
)

(define_insn "*thumb2_movsf_vfp"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=t,?r,t, t  ,Uv,r ,m,t,r")
	(match_operand:SF 1 "general_operand"	   " ?r,t,Dv,UvE,t, mE,r,t,r"))]
  "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP
   && (   s_register_operand (operands[0], SFmode)
       || s_register_operand (operands[1], SFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"vmov%?\\t%0, %1\";
    case 1:
      return \"vmov%?\\t%0, %1\";
    case 2:
      return \"vmov%?.f32\\t%0, %1\";
    case 3: case 4:
      return output_move_vfp (operands);
    case 5:
      return \"ldr%?\\t%0, %1\\t%@ float\";
    case 6:
      return \"str%?\\t%1, %0\\t%@ float\";
    case 7:
      return \"vmov%?.f32\\t%0, %1\";
    case 8:
      return \"mov%?\\t%0, %1\\t%@ float\";
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type"
     "f_mcr,f_mrc,fconsts,f_loads,f_stores,load1,store1,fmov,mov_reg")
   (set_attr "pool_range" "*,*,*,1018,*,4090,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,1008,*,0,*,*,*")]
)

;; DFmode moves

(define_insn "*movdf_vfp"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=w,?r,w ,w,w  ,Uv,r, m,w,r")
	(match_operand:DF 1 "soft_df_operand"		   " ?r,w,Dy,G,UvF,w ,mF,r,w,r"))]
  "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  {
    switch (which_alternative)
      {
      case 0:
	return \"vmov%?\\t%P0, %Q1, %R1\";
      case 1:
	return \"vmov%?\\t%Q0, %R0, %P1\";
      case 2:
	gcc_assert (TARGET_VFP_DOUBLE);
        return \"vmov%?.f64\\t%P0, %1\";
      case 3:
	gcc_assert (TARGET_VFP_DOUBLE);
	return \"vmov.i64\\t%P0, #0\\t%@ float\";
      case 4: case 5:
	return output_move_vfp (operands);
      case 6: case 7:
	return output_move_double (operands, true, NULL);
      case 8:
	if (TARGET_VFP_SINGLE)
	  return \"vmov%?.f32\\t%0, %1\;vmov%?.f32\\t%p0, %p1\";
	else
	  return \"vmov%?.f64\\t%P0, %P1\";
      case 9:
        return \"#\";
      default:
	gcc_unreachable ();
      }
    }
  "
  [(set_attr "type" "f_mcrr,f_mrrc,fconstd,neon_move,f_loadd,f_stored,\
                     load2,store2,ffarithd,multiple")
   (set (attr "length") (cond [(eq_attr "alternative" "6,7,9") (const_int 8)
			       (eq_attr "alternative" "8")
				(if_then_else
				 (match_test "TARGET_VFP_SINGLE")
				 (const_int 8)
				 (const_int 4))]
			      (const_int 4)))
   (set_attr "predicable" "yes,yes,yes,no,yes,yes,yes,yes,yes,yes")
   (set_attr "pool_range" "*,*,*,*,1020,*,1020,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,1004,*,1004,*,*,*")
   (set_attr "arch" "any,any,any,neon,any,any,any,any,any,any")]
)

(define_insn "*thumb2_movdf_vfp"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=w,?r,w ,w,w  ,Uv,r ,m,w,r")
	(match_operand:DF 1 "soft_df_operand"		   " ?r,w,Dy,G,UvF,w, mF,r, w,r"))]
  "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  {
    switch (which_alternative)
      {
      case 0:
	return \"vmov%?\\t%P0, %Q1, %R1\";
      case 1:
	return \"vmov%?\\t%Q0, %R0, %P1\";
      case 2:
	gcc_assert (TARGET_VFP_DOUBLE);
	return \"vmov%?.f64\\t%P0, %1\";
      case 3:
	gcc_assert (TARGET_VFP_DOUBLE);
	return \"vmov.i64\\t%P0, #0\\t%@ float\";
      case 4: case 5:
	return output_move_vfp (operands);
      case 6: case 7: case 9:
	return output_move_double (operands, true, NULL);
      case 8:
	if (TARGET_VFP_SINGLE)
	  return \"vmov%?.f32\\t%0, %1\;vmov%?.f32\\t%p0, %p1\";
	else
	  return \"vmov%?.f64\\t%P0, %P1\";
      default:
	abort ();
      }
    }
  "
  [(set_attr "type" "f_mcrr,f_mrrc,fconstd,neon_move,f_loadd,\
                     f_stored,load2,store2,ffarithd,multiple")
   (set (attr "length") (cond [(eq_attr "alternative" "6,7,9") (const_int 8)
			       (eq_attr "alternative" "8")
				(if_then_else
				 (match_test "TARGET_VFP_SINGLE")
				 (const_int 8)
				 (const_int 4))]
			      (const_int 4)))
   (set_attr "pool_range" "*,*,*,*,1018,*,4094,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,1008,*,0,*,*,*")
   (set_attr "arch" "any,any,any,neon,any,any,any,any,any,any")]
)


;; Conditional move patterns

(define_insn "*movsfcc_vfp"
  [(set (match_operand:SF   0 "s_register_operand" "=t,t,t,t,t,t,?r,?r,?r")
	(if_then_else:SF
	  (match_operator   3 "arm_comparison_operator"
	    [(match_operand 4 "cc_register" "") (const_int 0)])
	  (match_operand:SF 1 "s_register_operand" "0,t,t,0,?r,?r,0,t,t")
	  (match_operand:SF 2 "s_register_operand" "t,0,t,?r,0,?r,t,0,t")))]
  "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   vmov%D3.f32\\t%0, %2
   vmov%d3.f32\\t%0, %1
   vmov%D3.f32\\t%0, %2\;vmov%d3.f32\\t%0, %1
   vmov%D3\\t%0, %2
   vmov%d3\\t%0, %1
   vmov%D3\\t%0, %2\;vmov%d3\\t%0, %1
   vmov%D3\\t%0, %2
   vmov%d3\\t%0, %1
   vmov%D3\\t%0, %2\;vmov%d3\\t%0, %1"
   [(set_attr "conds" "use")
    (set_attr "length" "4,4,8,4,4,8,4,4,8")
    (set_attr "type" "fmov,fmov,fmov,f_mcr,f_mcr,f_mcr,f_mrc,f_mrc,f_mrc")]
)

(define_insn "*thumb2_movsfcc_vfp"
  [(set (match_operand:SF   0 "s_register_operand" "=t,t,t,t,t,t,?r,?r,?r")
	(if_then_else:SF
	  (match_operator   3 "arm_comparison_operator"
	    [(match_operand 4 "cc_register" "") (const_int 0)])
	  (match_operand:SF 1 "s_register_operand" "0,t,t,0,?r,?r,0,t,t")
	  (match_operand:SF 2 "s_register_operand" "t,0,t,?r,0,?r,t,0,t")))]
  "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP && !arm_restrict_it"
  "@
   it\\t%D3\;vmov%D3.f32\\t%0, %2
   it\\t%d3\;vmov%d3.f32\\t%0, %1
   ite\\t%D3\;vmov%D3.f32\\t%0, %2\;vmov%d3.f32\\t%0, %1
   it\\t%D3\;vmov%D3\\t%0, %2
   it\\t%d3\;vmov%d3\\t%0, %1
   ite\\t%D3\;vmov%D3\\t%0, %2\;vmov%d3\\t%0, %1
   it\\t%D3\;vmov%D3\\t%0, %2
   it\\t%d3\;vmov%d3\\t%0, %1
   ite\\t%D3\;vmov%D3\\t%0, %2\;vmov%d3\\t%0, %1"
   [(set_attr "conds" "use")
    (set_attr "length" "6,6,10,6,6,10,6,6,10")
    (set_attr "type" "fmov,fmov,fmov,f_mcr,f_mcr,f_mcr,f_mrc,f_mrc,f_mrc")]
)

(define_insn "*movdfcc_vfp"
  [(set (match_operand:DF   0 "s_register_operand" "=w,w,w,w,w,w,?r,?r,?r")
	(if_then_else:DF
	  (match_operator   3 "arm_comparison_operator"
	    [(match_operand 4 "cc_register" "") (const_int 0)])
	  (match_operand:DF 1 "s_register_operand" "0,w,w,0,?r,?r,0,w,w")
	  (match_operand:DF 2 "s_register_operand" "w,0,w,?r,0,?r,w,0,w")))]
  "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "@
   vmov%D3.f64\\t%P0, %P2
   vmov%d3.f64\\t%P0, %P1
   vmov%D3.f64\\t%P0, %P2\;vmov%d3.f64\\t%P0, %P1
   vmov%D3\\t%P0, %Q2, %R2
   vmov%d3\\t%P0, %Q1, %R1
   vmov%D3\\t%P0, %Q2, %R2\;vmov%d3\\t%P0, %Q1, %R1
   vmov%D3\\t%Q0, %R0, %P2
   vmov%d3\\t%Q0, %R0, %P1
   vmov%D3\\t%Q0, %R0, %P2\;vmov%d3\\t%Q0, %R0, %P1"
   [(set_attr "conds" "use")
    (set_attr "length" "4,4,8,4,4,8,4,4,8")
    (set_attr "type" "ffarithd,ffarithd,ffarithd,f_mcr,f_mcr,f_mcr,f_mrrc,f_mrrc,f_mrrc")]
)

(define_insn "*thumb2_movdfcc_vfp"
  [(set (match_operand:DF   0 "s_register_operand" "=w,w,w,w,w,w,?r,?r,?r")
	(if_then_else:DF
	  (match_operator   3 "arm_comparison_operator"
	    [(match_operand 4 "cc_register" "") (const_int 0)])
	  (match_operand:DF 1 "s_register_operand" "0,w,w,0,?r,?r,0,w,w")
	  (match_operand:DF 2 "s_register_operand" "w,0,w,?r,0,?r,w,0,w")))]
  "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE && !arm_restrict_it"
  "@
   it\\t%D3\;vmov%D3.f64\\t%P0, %P2
   it\\t%d3\;vmov%d3.f64\\t%P0, %P1
   ite\\t%D3\;vmov%D3.f64\\t%P0, %P2\;vmov%d3.f64\\t%P0, %P1
   it\t%D3\;vmov%D3\\t%P0, %Q2, %R2
   it\t%d3\;vmov%d3\\t%P0, %Q1, %R1
   ite\\t%D3\;vmov%D3\\t%P0, %Q2, %R2\;vmov%d3\\t%P0, %Q1, %R1
   it\t%D3\;vmov%D3\\t%Q0, %R0, %P2
   it\t%d3\;vmov%d3\\t%Q0, %R0, %P1
   ite\\t%D3\;vmov%D3\\t%Q0, %R0, %P2\;vmov%d3\\t%Q0, %R0, %P1"
   [(set_attr "conds" "use")
    (set_attr "length" "6,6,10,6,6,10,6,6,10")
    (set_attr "type" "ffarithd,ffarithd,ffarithd,f_mcr,f_mcr,f_mcrr,f_mrrc,f_mrrc,f_mrrc")]
)


;; Sign manipulation functions

(define_insn "*abssf2_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=t")
	(abs:SF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vabs%?.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffariths")]
)

(define_insn "*absdf2_vfp"
  [(set (match_operand:DF	  0 "s_register_operand" "=w")
	(abs:DF (match_operand:DF 1 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vabs%?.f64\\t%P0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffarithd")]
)

(define_insn "*negsf2_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=t,?r")
	(neg:SF (match_operand:SF 1 "s_register_operand" "t,r")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   vneg%?.f32\\t%0, %1
   eor%?\\t%0, %1, #-2147483648"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffariths")]
)

(define_insn_and_split "*negdf2_vfp"
  [(set (match_operand:DF	  0 "s_register_operand" "=w,?r,?r")
	(neg:DF (match_operand:DF 1 "s_register_operand" "w,0,r")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "@
   vneg%?.f64\\t%P0, %P1
   #
   #"
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE && reload_completed
   && arm_general_register_operand (operands[0], DFmode)"
  [(set (match_dup 0) (match_dup 1))]
  "
  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      operands[0] = gen_highpart (SImode, operands[0]);
      operands[1] = gen_rtx_XOR (SImode, operands[0], GEN_INT (0x80000000));
    }
  else
    {
      rtx in_hi, in_lo, out_hi, out_lo;

      in_hi = gen_rtx_XOR (SImode, gen_highpart (SImode, operands[1]),
			   GEN_INT (0x80000000));
      in_lo = gen_lowpart (SImode, operands[1]);
      out_hi = gen_highpart (SImode, operands[0]);
      out_lo = gen_lowpart (SImode, operands[0]);

      if (REGNO (in_lo) == REGNO (out_hi))
        {
          emit_insn (gen_rtx_SET (out_lo, in_lo));
	  operands[0] = out_hi;
          operands[1] = in_hi;
        }
      else
        {
          emit_insn (gen_rtx_SET (out_hi, in_hi));
	  operands[0] = out_lo;
          operands[1] = in_lo;
        }
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "length" "4,4,8")
   (set_attr "type" "ffarithd")]
)

;; ABS and NEG for FP16.
(define_insn "<absneg_str>hf2"
  [(set (match_operand:HF 0 "s_register_operand" "=w")
    (ABSNEG:HF (match_operand:HF 1 "s_register_operand" "w")))]
 "TARGET_VFP_FP16INST"
 "v<absneg_str>.f16\t%0, %1"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "ffariths")]
)

(define_expand "neon_vabshf"
 [(set
   (match_operand:HF 0 "s_register_operand")
   (abs:HF (match_operand:HF 1 "s_register_operand")))]
 "TARGET_VFP_FP16INST"
{
  emit_insn (gen_abshf2 (operands[0], operands[1]));
  DONE;
})

;; VRND for FP16.
(define_insn "neon_v<fp16_rnd_str>hf"
  [(set (match_operand:HF 0 "s_register_operand" "=w")
    (unspec:HF
     [(match_operand:HF 1 "s_register_operand" "w")]
     FP16_RND))]
 "TARGET_VFP_FP16INST"
 "<fp16_rnd_insn>.f16\t%0, %1"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "neon_fp_round_s")]
)

(define_insn "neon_vrndihf"
  [(set (match_operand:HF 0 "s_register_operand" "=w")
    (unspec:HF
     [(match_operand:HF 1 "s_register_operand" "w")]
     UNSPEC_VRNDI))]
  "TARGET_VFP_FP16INST"
  "vrintr.f16\t%0, %1"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "neon_fp_round_s")]
)

;; Arithmetic insns

(define_insn "addhf3"
  [(set
    (match_operand:HF 0 "s_register_operand" "=w")
    (plus:HF
     (match_operand:HF 1 "s_register_operand" "w")
     (match_operand:HF 2 "s_register_operand" "w")))]
 "TARGET_VFP_FP16INST"
 "vadd.f16\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fadds")]
)

(define_insn "*addsf3_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=t")
	(plus:SF (match_operand:SF 1 "s_register_operand" "t")
		 (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vadd%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fadds")]
)

(define_insn "*adddf3_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=w")
	(plus:DF (match_operand:DF 1 "s_register_operand" "w")
		 (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vadd%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "faddd")]
)

(define_insn "subhf3"
 [(set
   (match_operand:HF 0 "s_register_operand" "=w")
   (minus:HF
    (match_operand:HF 1 "s_register_operand" "w")
    (match_operand:HF 2 "s_register_operand" "w")))]
 "TARGET_VFP_FP16INST"
 "vsub.f16\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fadds")]
)

(define_insn "*subsf3_vfp"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(minus:SF (match_operand:SF 1 "s_register_operand" "t")
		  (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vsub%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fadds")]
)

(define_insn "*subdf3_vfp"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(minus:DF (match_operand:DF 1 "s_register_operand" "w")
		  (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vsub%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "faddd")]
)


;; Division insns

;; FP16 Division.
(define_insn "divhf3"
  [(set
    (match_operand:HF	   0 "s_register_operand" "=w")
    (div:HF
     (match_operand:HF 1 "s_register_operand" "w")
     (match_operand:HF 2 "s_register_operand" "w")))]
  "TARGET_VFP_FP16INST"
  "vdiv.f16\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fdivs")]
)

; VFP9 Erratum 760019: It's potentially unsafe to overwrite the input
; operands, so mark the output as early clobber for VFPv2 on ARMv5 or
; earlier.
(define_insn "*divsf3_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=&t,t")
	(div:SF (match_operand:SF 1 "s_register_operand" "t,t")
		(match_operand:SF 2 "s_register_operand" "t,t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vdiv%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "arch" "*,armv6_or_vfpv3")
   (set_attr "type" "fdivs")]
)

(define_insn "*divdf3_vfp"
  [(set (match_operand:DF	  0 "s_register_operand" "=&w,w")
	(div:DF (match_operand:DF 1 "s_register_operand" "w,w")
		(match_operand:DF 2 "s_register_operand" "w,w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vdiv%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "arch" "*,armv6_or_vfpv3")
   (set_attr "type" "fdivd")]
)


;; Multiplication insns

(define_insn "mulhf3"
 [(set
   (match_operand:HF 0 "s_register_operand" "=w")
   (mult:HF (match_operand:HF 1 "s_register_operand" "w")
	    (match_operand:HF 2 "s_register_operand" "w")))]
  "TARGET_VFP_FP16INST"
  "vmul.f16\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmuls")]
)

(define_insn "*mulsf3_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=t")
	(mult:SF (match_operand:SF 1 "s_register_operand" "t")
		 (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vmul%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuls")]
)

(define_insn "*muldf3_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=w")
	(mult:DF (match_operand:DF 1 "s_register_operand" "w")
		 (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vmul%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuld")]
)

(define_insn "*mulsf3neghf_vfp"
  [(set (match_operand:HF		   0 "s_register_operand" "=t")
	(mult:HF (neg:HF (match_operand:HF 1 "s_register_operand" "t"))
		 (match_operand:HF	   2 "s_register_operand" "t")))]
  "TARGET_VFP_FP16INST && !flag_rounding_math"
  "vnmul.f16\\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmuls")]
)

(define_insn "*negmulhf3_vfp"
  [(set (match_operand:HF		   0 "s_register_operand" "=t")
	(neg:HF (mult:HF (match_operand:HF 1 "s_register_operand" "t")
		 (match_operand:HF	   2 "s_register_operand" "t"))))]
  "TARGET_VFP_FP16INST"
  "vnmul.f16\\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmuls")]
)

(define_insn "*mulsf3negsf_vfp"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(mult:SF (neg:SF (match_operand:SF 1 "s_register_operand" "t"))
		 (match_operand:SF	   2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP && !flag_rounding_math"
  "vnmul%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuls")]
)

(define_insn "*negmulsf3_vfp"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(neg:SF (mult:SF (match_operand:SF 1 "s_register_operand" "t")
		 (match_operand:SF	   2 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vnmul%?.f32\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuls")]
)

(define_insn "*muldf3negdf_vfp"
  [(set (match_operand:DF		   0 "s_register_operand" "=w")
	(mult:DF (neg:DF (match_operand:DF 1 "s_register_operand" "w"))
		 (match_operand:DF	   2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE
  && !flag_rounding_math"
  "vnmul%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuld")]
)

(define_insn "*negmuldf3_vfp"
  [(set (match_operand:DF		   0 "s_register_operand" "=w")
	(neg:DF (mult:DF (match_operand:DF 1 "s_register_operand" "w")
		 (match_operand:DF	   2 "s_register_operand" "w"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vnmul%?.f64\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuld")]
)


;; Multiply-accumulate insns

;; 0 = 1 * 2 + 0
(define_insn "*mulsf3addhf_vfp"
 [(set (match_operand:HF 0 "s_register_operand" "=t")
       (plus:HF
	(mult:HF (match_operand:HF 2 "s_register_operand" "t")
		 (match_operand:HF 3 "s_register_operand" "t"))
	(match_operand:HF 1 "s_register_operand" "0")))]
  "TARGET_VFP_FP16INST"
  "vmla.f16\\t%0, %2, %3"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmacs")]
)

(define_insn "*mulsf3addsf_vfp"
  [(set (match_operand:SF		    0 "s_register_operand" "=t")
	(plus:SF (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			  (match_operand:SF 3 "s_register_operand" "t"))
		 (match_operand:SF	    1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vmla%?.f32\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacs")]
)

(define_insn "*muldf3adddf_vfp"
  [(set (match_operand:DF		    0 "s_register_operand" "=w")
	(plus:DF (mult:DF (match_operand:DF 2 "s_register_operand" "w")
			  (match_operand:DF 3 "s_register_operand" "w"))
		 (match_operand:DF	    1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vmla%?.f64\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; 0 = 1 * 2 - 0
(define_insn "*mulhf3subhf_vfp"
  [(set (match_operand:HF 0 "s_register_operand" "=t")
	(minus:HF (mult:HF (match_operand:HF 2 "s_register_operand" "t")
			   (match_operand:HF 3 "s_register_operand" "t"))
		  (match_operand:HF 1 "s_register_operand" "0")))]
  "TARGET_VFP_FP16INST"
  "vnmls.f16\\t%0, %2, %3"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmacs")]
)

(define_insn "*mulsf3subsf_vfp"
  [(set (match_operand:SF		     0 "s_register_operand" "=t")
	(minus:SF (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			   (match_operand:SF 3 "s_register_operand" "t"))
		  (match_operand:SF	     1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vnmls%?.f32\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacs")]
)

(define_insn "*muldf3subdf_vfp"
  [(set (match_operand:DF		     0 "s_register_operand" "=w")
	(minus:DF (mult:DF (match_operand:DF 2 "s_register_operand" "w")
			   (match_operand:DF 3 "s_register_operand" "w"))
		  (match_operand:DF	     1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vnmls%?.f64\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; 0 = -(1 * 2) + 0
(define_insn "*mulhf3neghfaddhf_vfp"
  [(set (match_operand:HF 0 "s_register_operand" "=t")
	(minus:HF (match_operand:HF 1 "s_register_operand" "0")
		  (mult:HF (match_operand:HF 2 "s_register_operand" "t")
			   (match_operand:HF 3 "s_register_operand" "t"))))]
  "TARGET_VFP_FP16INST"
  "vmls.f16\\t%0, %2, %3"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmacs")]
)

(define_insn "*mulsf3negsfaddsf_vfp"
  [(set (match_operand:SF		     0 "s_register_operand" "=t")
	(minus:SF (match_operand:SF	     1 "s_register_operand" "0")
		  (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			   (match_operand:SF 3 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vmls%?.f32\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacs")]
)

(define_insn "*fmuldf3negdfadddf_vfp"
  [(set (match_operand:DF		     0 "s_register_operand" "=w")
	(minus:DF (match_operand:DF	     1 "s_register_operand" "0")
		  (mult:DF (match_operand:DF 2 "s_register_operand" "w")
			   (match_operand:DF 3 "s_register_operand" "w"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vmls%?.f64\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)


;; 0 = -(1 * 2) - 0
(define_insn "*mulhf3neghfsubhf_vfp"
  [(set (match_operand:HF 0 "s_register_operand" "=t")
	(minus:HF (mult:HF
		   (neg:HF (match_operand:HF 2 "s_register_operand" "t"))
		   (match_operand:HF 3 "s_register_operand" "t"))
		  (match_operand:HF 1 "s_register_operand" "0")))]
  "TARGET_VFP_FP16INST"
  "vnmla.f16\\t%0, %2, %3"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fmacs")]
)

(define_insn "*mulsf3negsfsubsf_vfp"
  [(set (match_operand:SF		      0 "s_register_operand" "=t")
	(minus:SF (mult:SF
		    (neg:SF (match_operand:SF 2 "s_register_operand" "t"))
		    (match_operand:SF	      3 "s_register_operand" "t"))
		  (match_operand:SF	      1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vnmla%?.f32\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacs")]
)

(define_insn "*muldf3negdfsubdf_vfp"
  [(set (match_operand:DF		      0 "s_register_operand" "=w")
	(minus:DF (mult:DF
		    (neg:DF (match_operand:DF 2 "s_register_operand" "w"))
		    (match_operand:DF	      3 "s_register_operand" "w"))
		  (match_operand:DF	      1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vnmla%?.f64\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; Fused-multiply-accumulate

(define_insn "fmahf4"
  [(set (match_operand:HF 0 "register_operand" "=w")
    (fma:HF
     (match_operand:HF 1 "register_operand" "w")
     (match_operand:HF 2 "register_operand" "w")
     (match_operand:HF 3 "register_operand" "0")))]
 "TARGET_VFP_FP16INST"
 "vfma.f16\\t%0, %1, %2"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "ffmas")]
)

(define_expand "neon_vfmahf"
  [(match_operand:HF 0 "s_register_operand")
   (match_operand:HF 1 "s_register_operand")
   (match_operand:HF 2 "s_register_operand")
   (match_operand:HF 3 "s_register_operand")]
  "TARGET_VFP_FP16INST"
{
  emit_insn (gen_fmahf4 (operands[0], operands[2], operands[3],
			 operands[1]));
  DONE;
})

(define_insn "fma<SDF:mode>4"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
        (fma:SDF (match_operand:SDF 1 "register_operand" "<F_constraint>")
		 (match_operand:SDF 2 "register_operand" "<F_constraint>")
		 (match_operand:SDF 3 "register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FMA"
  "vfma%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffma<vfp_type>")]
)

(define_insn "fmsubhf4_fp16"
 [(set (match_operand:HF 0 "register_operand" "=w")
   (fma:HF
    (neg:HF (match_operand:HF 1 "register_operand" "w"))
    (match_operand:HF 2 "register_operand" "w")
    (match_operand:HF 3 "register_operand" "0")))]
 "TARGET_VFP_FP16INST"
 "vfms.f16\\t%0, %1, %2"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "ffmas")]
)

(define_expand "neon_vfmshf"
  [(match_operand:HF 0 "s_register_operand")
   (match_operand:HF 1 "s_register_operand")
   (match_operand:HF 2 "s_register_operand")
   (match_operand:HF 3 "s_register_operand")]
  "TARGET_VFP_FP16INST"
{
  emit_insn (gen_fmsubhf4_fp16 (operands[0], operands[2], operands[3],
				operands[1]));
  DONE;
})

(define_insn "*fmsub<SDF:mode>4"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
	(fma:SDF (neg:SDF (match_operand:SDF 1 "register_operand"
					     "<F_constraint>"))
		 (match_operand:SDF 2 "register_operand" "<F_constraint>")
		 (match_operand:SDF 3 "register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FMA"
  "vfms%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffma<vfp_type>")]
)

(define_insn "*fnmsubhf4"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(fma:HF (match_operand:HF 1 "register_operand" "w")
		 (match_operand:HF 2 "register_operand" "w")
		 (neg:HF (match_operand:HF 3 "register_operand" "0"))))]
  "TARGET_VFP_FP16INST"
  "vfnms.f16\\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "ffmas")]
)

(define_insn "*fnmsub<SDF:mode>4"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
	(fma:SDF (match_operand:SDF 1 "register_operand" "<F_constraint>")
		 (match_operand:SDF 2 "register_operand" "<F_constraint>")
		 (neg:SDF (match_operand:SDF 3 "register_operand" "0"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FMA"
  "vfnms%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffma<vfp_type>")]
)

(define_insn "*fnmaddhf4"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(fma:HF (neg:HF (match_operand:HF 1 "register_operand" "w"))
		 (match_operand:HF 2 "register_operand" "w")
		 (neg:HF (match_operand:HF 3 "register_operand" "0"))))]
  "TARGET_VFP_FP16INST"
  "vfnma.f16\\t%0, %1, %2"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "ffmas")]
)

(define_insn "*fnmadd<SDF:mode>4"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
	(fma:SDF (neg:SDF (match_operand:SDF 1 "register_operand"
					       "<F_constraint>"))
		 (match_operand:SDF 2 "register_operand" "<F_constraint>")
		 (neg:SDF (match_operand:SDF 3 "register_operand" "0"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FMA"
  "vfnma%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffma<vfp_type>")]
)


;; Conversion routines

(define_insn "*extendsfdf2_vfp"
  [(set (match_operand:DF		   0 "s_register_operand" "=w")
	(float_extend:DF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.f64.f32\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "*truncdfsf2_vfp"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(float_truncate:SF (match_operand:DF 1 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.f32.f64\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "extendhfsf2"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(float_extend:SF (match_operand:HF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && (TARGET_FP16 || TARGET_VFP_FP16INST)"
  "vcvtb%?.f32.f16\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "truncsfhf2"
  [(set (match_operand:HF		   0 "s_register_operand" "=t")
	(float_truncate:HF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && (TARGET_FP16 || TARGET_VFP_FP16INST)"
  "vcvtb%?.f16.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "*truncsisf2_vfp"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(fix:SI (fix:SF (match_operand:SF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vcvt%?.s32.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)

(define_insn "*truncsidf2_vfp"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(fix:SI (fix:DF (match_operand:DF 1 "s_register_operand" "w"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.s32.f64\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)


(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(unsigned_fix:SI (fix:SF (match_operand:SF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vcvt%?.u32.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)

(define_insn "fixuns_truncdfsi2"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(unsigned_fix:SI (fix:DF (match_operand:DF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.u32.f64\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)


(define_insn "*floatsisf2_vfp"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(float:SF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vcvt%?.f32.s32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)

(define_insn "*floatsidf2_vfp"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(float:DF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.f64.s32\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)


(define_insn "floatunssisf2"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(unsigned_float:SF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vcvt%?.f32.u32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)

(define_insn "floatunssidf2"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(unsigned_float:DF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vcvt%?.f64.u32\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)


;; Sqrt insns.

(define_insn "neon_vsqrthf"
  [(set (match_operand:HF 0 "s_register_operand" "=w")
	(sqrt:HF (match_operand:HF 1 "s_register_operand" "w")))]
  "TARGET_VFP_FP16INST"
  "vsqrt.f16\t%0, %1"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "fsqrts")]
)

(define_insn "neon_vrsqrtshf"
  [(set
    (match_operand:HF 0 "s_register_operand" "=w")
    (unspec:HF [(match_operand:HF 1 "s_register_operand" "w")
		(match_operand:HF 2 "s_register_operand" "w")]
     UNSPEC_VRSQRTS))]
 "TARGET_VFP_FP16INST"
 "vrsqrts.f16\t%0, %1, %2"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "fsqrts")]
)

; VFP9 Erratum 760019: It's potentially unsafe to overwrite the input
; operands, so mark the output as early clobber for VFPv2 on ARMv5 or
; earlier.
(define_insn "*sqrtsf2_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=&t,t")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "t,t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vsqrt%?.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "arch" "*,armv6_or_vfpv3")
   (set_attr "type" "fsqrts")]
)

(define_insn "*sqrtdf2_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=&w,w")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "w,w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "vsqrt%?.f64\\t%P0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "arch" "*,armv6_or_vfpv3")
   (set_attr "type" "fsqrtd")]
)


;; Patterns to split/copy vfp condition flags.

(define_insn "*movcc_vfp"
  [(set (reg CC_REGNUM)
	(reg VFPCC_REGNUM))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "vmrs%?\\tAPSR_nzcv, FPSCR"
  [(set_attr "conds" "set")
   (set_attr "type" "f_flag")]
)

(define_insn_and_split "*cmpsf_split_vfp"
  [(set (reg:CCFP CC_REGNUM)
	(compare:CCFP (match_operand:SF 0 "s_register_operand"  "t")
		      (match_operand:SF 1 "vfp_compare_operand" "tG")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "#"
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  [(set (reg:CCFP VFPCC_REGNUM)
	(compare:CCFP (match_dup 0)
		      (match_dup 1)))
   (set (reg:CCFP CC_REGNUM)
	(reg:CCFP VFPCC_REGNUM))]
  ""
)

(define_insn_and_split "*cmpsf_trap_split_vfp"
  [(set (reg:CCFPE CC_REGNUM)
	(compare:CCFPE (match_operand:SF 0 "s_register_operand"  "t")
		       (match_operand:SF 1 "vfp_compare_operand" "tG")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "#"
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  [(set (reg:CCFPE VFPCC_REGNUM)
	(compare:CCFPE (match_dup 0)
		       (match_dup 1)))
   (set (reg:CCFPE CC_REGNUM)
	(reg:CCFPE VFPCC_REGNUM))]
  ""
)

(define_insn_and_split "*cmpdf_split_vfp"
  [(set (reg:CCFP CC_REGNUM)
	(compare:CCFP (match_operand:DF 0 "s_register_operand"  "w")
		      (match_operand:DF 1 "vfp_compare_operand" "wG")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "#"
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  [(set (reg:CCFP VFPCC_REGNUM)
	(compare:CCFP (match_dup 0)
		       (match_dup 1)))
   (set (reg:CCFP CC_REGNUM)
	(reg:CCFP VFPCC_REGNUM))]
  ""
)

(define_insn_and_split "*cmpdf_trap_split_vfp"
  [(set (reg:CCFPE CC_REGNUM)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand"  "w")
		       (match_operand:DF 1 "vfp_compare_operand" "wG")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "#"
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  [(set (reg:CCFPE VFPCC_REGNUM)
	(compare:CCFPE (match_dup 0)
		       (match_dup 1)))
   (set (reg:CCFPE CC_REGNUM)
	(reg:CCFPE VFPCC_REGNUM))]
  ""
)


;; Comparison patterns

;; In the compare with FP zero case the ARM Architecture Reference Manual
;; specifies the immediate to be #0.0.  However, some buggy assemblers only
;; accept #0.  We don't want to autodetect broken assemblers, so output #0.
(define_insn "*cmpsf_vfp"
  [(set (reg:CCFP VFPCC_REGNUM)
	(compare:CCFP (match_operand:SF 0 "s_register_operand"  "t,t")
		      (match_operand:SF 1 "vfp_compare_operand" "t,G")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   vcmp%?.f32\\t%0, %1
   vcmp%?.f32\\t%0, #0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fcmps")]
)

(define_insn "*cmpsf_trap_vfp"
  [(set (reg:CCFPE VFPCC_REGNUM)
	(compare:CCFPE (match_operand:SF 0 "s_register_operand"  "t,t")
		       (match_operand:SF 1 "vfp_compare_operand" "t,G")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   vcmpe%?.f32\\t%0, %1
   vcmpe%?.f32\\t%0, #0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fcmps")]
)

(define_insn "*cmpdf_vfp"
  [(set (reg:CCFP VFPCC_REGNUM)
	(compare:CCFP (match_operand:DF 0 "s_register_operand"  "w,w")
		      (match_operand:DF 1 "vfp_compare_operand" "w,G")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "@
   vcmp%?.f64\\t%P0, %P1
   vcmp%?.f64\\t%P0, #0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fcmpd")]
)

(define_insn "*cmpdf_trap_vfp"
  [(set (reg:CCFPE VFPCC_REGNUM)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand"  "w,w")
		       (match_operand:DF 1 "vfp_compare_operand" "w,G")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "@
   vcmpe%?.f64\\t%P0, %P1
   vcmpe%?.f64\\t%P0, #0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fcmpd")]
)

;; Fixed point to floating point conversions.
(define_insn "*combine_vcvt_f32_<FCVTI32typename>"
  [(set (match_operand:SF 0 "s_register_operand" "=t")
	(mult:SF (FCVT:SF (match_operand:SI 1 "s_register_operand" "0"))
		 (match_operand 2
			"const_double_vcvt_power_of_two_reciprocal" "Dt")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP3 && !flag_rounding_math"
  "vcvt%?.f32.<FCVTI32typename>\\t%0, %1, %v2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)

;; Not the ideal way of implementing this. Ideally we would be able to split
;; this into a move to a DP register and then a vcvt.f64.i32
(define_insn "*combine_vcvt_f64_<FCVTI32typename>"
  [(set (match_operand:DF 0 "s_register_operand" "=x,x,w")
	(mult:DF (FCVT:DF (match_operand:SI 1 "s_register_operand" "r,t,r"))
		 (match_operand 2
		     "const_double_vcvt_power_of_two_reciprocal" "Dt,Dt,Dt")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP3 && !flag_rounding_math
  && !TARGET_VFP_SINGLE"
  "@
  vmov%?.f32\\t%0, %1\;vcvt%?.f64.<FCVTI32typename>\\t%P0, %P0, %v2
  vmov%?.f32\\t%0, %1\;vcvt%?.f64.<FCVTI32typename>\\t%P0, %P0, %v2
  vmov%?.f64\\t%P0, %1, %1\;vcvt%?.f64.<FCVTI32typename>\\t%P0, %P0, %v2"
  [(set_attr "predicable" "yes")
   (set_attr "ce_count" "2")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")
   (set_attr "length" "8")]
)

(define_insn "*combine_vcvtf2i"
  [(set (match_operand:SI 0 "s_register_operand" "=t")
	(fix:SI (fix:SF (mult:SF (match_operand:SF 1 "s_register_operand" "0")
				 (match_operand 2
				 "const_double_vcvt_power_of_two" "Dp")))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP3 && !flag_rounding_math"
  "vcvt%?.s32.f32\\t%0, %1, %v2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
 )

;; FP16 conversions.
(define_insn "neon_vcvth<sup>hf"
 [(set (match_operand:HF 0 "s_register_operand" "=w")
   (unspec:HF
    [(match_operand:SI 1 "s_register_operand" "w")]
    VCVTH_US))]
 "TARGET_VFP_FP16INST"
 "vcvt.f16.<sup>%#32\t%0, %1"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "f_cvti2f")]
)

(define_insn "neon_vcvth<sup>si"
 [(set (match_operand:SI 0 "s_register_operand" "=w")
   (unspec:SI
    [(match_operand:HF 1 "s_register_operand" "w")]
    VCVTH_US))]
 "TARGET_VFP_FP16INST"
 "vcvt.<sup>%#32.f16\t%0, %1"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "f_cvtf2i")]
)

;; The neon_vcvth<sup>_nhf patterns are used to generate the instruction for the
;; vcvth_n_f16_<sup>32 arm_fp16 intrinsics.  They are complicated by the
;; hardware requirement that the source and destination registers are the same
;; despite having different machine modes.  The approach is to use a temporary
;; register for the conversion and move that to the correct destination.

;; Generate an unspec pattern for the intrinsic.
(define_insn "neon_vcvth<sup>_nhf_unspec"
 [(set
   (match_operand:SI 0 "s_register_operand" "=w")
   (unspec:SI
    [(match_operand:SI 1 "s_register_operand" "0")
     (match_operand:SI 2 "immediate_operand" "i")]
    VCVT_HF_US_N))
 (set
  (match_operand:HF 3 "s_register_operand" "=w")
  (float_truncate:HF (float:SF (match_dup 0))))]
 "TARGET_VFP_FP16INST"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vcvt.f16.<sup>32\t%0, %0, %2\;vmov.f32\t%3, %0";
}
  [(set_attr "conds" "unconditional")
   (set_attr "type" "f_cvti2f")]
)

;; Generate the instruction patterns needed for vcvth_n_f16_s32 neon intrinsics.
(define_expand "neon_vcvth<sup>_nhf"
 [(match_operand:HF 0 "s_register_operand")
  (unspec:HF [(match_operand:SI 1 "s_register_operand")
	      (match_operand:SI 2 "immediate_operand")]
   VCVT_HF_US_N)]
"TARGET_VFP_FP16INST"
{
  rtx op1 = gen_reg_rtx (SImode);

  neon_const_bounds (operands[2], 1, 33);

  emit_move_insn (op1, operands[1]);
  emit_insn (gen_neon_vcvth<sup>_nhf_unspec (op1, op1, operands[2],
					     operands[0]));
  DONE;
})

;; The neon_vcvth<sup>_nsi patterns are used to generate the instruction for the
;; vcvth_n_<sup>32_f16 arm_fp16 intrinsics.  They have the same restrictions and
;; are implemented in the same way as the neon_vcvth<sup>_nhf patterns.

;; Generate an unspec pattern, constraining the registers.
(define_insn "neon_vcvth<sup>_nsi_unspec"
 [(set (match_operand:SI 0 "s_register_operand" "=w")
   (unspec:SI
    [(fix:SI
      (fix:SF
       (float_extend:SF
	(match_operand:HF 1 "s_register_operand" "w"))))
     (match_operand:SI 2 "immediate_operand" "i")]
    VCVT_SI_US_N))]
 "TARGET_VFP_FP16INST"
{
  neon_const_bounds (operands[2], 1, 33);
  return "vmov.f32\t%0, %1\;vcvt.<sup>%#32.f16\t%0, %0, %2";
}
  [(set_attr "conds" "unconditional")
   (set_attr "type" "f_cvtf2i")]
)

;; Generate the instruction patterns needed for vcvth_n_f16_s32 neon intrinsics.
(define_expand "neon_vcvth<sup>_nsi"
 [(match_operand:SI 0 "s_register_operand")
  (unspec:SI
   [(match_operand:HF 1 "s_register_operand")
    (match_operand:SI 2 "immediate_operand")]
   VCVT_SI_US_N)]
 "TARGET_VFP_FP16INST"
{
  rtx op1 = gen_reg_rtx (SImode);

  neon_const_bounds (operands[2], 1, 33);
  emit_insn (gen_neon_vcvth<sup>_nsi_unspec (op1, operands[1], operands[2]));
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_insn "neon_vcvt<vcvth_op>h<sup>si"
 [(set
   (match_operand:SI 0 "s_register_operand" "=w")
   (unspec:SI
    [(match_operand:HF 1 "s_register_operand" "w")]
    VCVT_HF_US))]
 "TARGET_VFP_FP16INST"
 "vcvt<vcvth_op>.<sup>%#32.f16\t%0, %1"
  [(set_attr "conds" "unconditional")
   (set_attr "type" "f_cvtf2i")]
)

;; Store multiple insn used in function prologue.
(define_insn "*push_multi_vfp"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:DF 1 "vfp_register_operand" "")]
		      UNSPEC_PUSH_MULT))])]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "* return vfp_output_vstmd (operands);"
  [(set_attr "type" "f_stored")]
)

;; VRINT round to integral instructions.
;; Invoked for the patterns: btruncsf2, btruncdf2, ceilsf2, ceildf2,
;; roundsf2, rounddf2, floorsf2, floordf2, nearbyintsf2, nearbyintdf2,
;; rintsf2, rintdf2.
(define_insn "<vrint_pattern><SDF:mode>2"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
        (unspec:SDF [(match_operand:SDF 1
		         "register_operand" "<F_constraint>")]
         VRINT))]
  "TARGET_HARD_FLOAT && TARGET_VFP5 <vfp_double_cond>"
  "vrint<vrint_variant>%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1"
  [(set_attr "predicable" "<vrint_predicable>")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_rint<vfp_type>")
   (set_attr "conds" "<vrint_conds>")]
)

;; Implements the lround, lfloor and lceil optabs.
(define_insn "l<vrint_pattern><su_optab><mode>si2"
  [(set (match_operand:SI 0 "register_operand" "=t")
        (FIXUORS:SI (unspec:SDF
                        [(match_operand:SDF 1
                           "register_operand" "<F_constraint>")] VCVT)))]
  "TARGET_HARD_FLOAT && TARGET_FPU_ARMV8 <vfp_double_cond>"
  "vcvt<vrint_variant>.<su>32.<V_if_elem>\\t%0, %<V_reg>1"
  [(set_attr "predicable" "no")
   (set_attr "conds" "unconditional")
   (set_attr "type" "f_cvtf2i")]
)

;; MIN_EXPR and MAX_EXPR eventually map to 'smin' and 'smax' in RTL.
;; The 'smax' and 'smin' RTL standard pattern names do not specify which
;; operand will be returned when both operands are zero (i.e. they may not
;; honour signed zeroes), or when either operand is NaN.  Therefore GCC
;; only introduces MIN_EXPR/MAX_EXPR in fast math mode or when not honouring
;; NaNs.

(define_insn "smax<mode>3"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
        (smax:SDF (match_operand:SDF 1 "register_operand" "<F_constraint>")
		  (match_operand:SDF 2 "register_operand" "<F_constraint>")))]
  "TARGET_HARD_FLOAT && TARGET_VFP5 <vfp_double_cond>"
  "vmaxnm.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "f_minmax<vfp_type>")
   (set_attr "conds" "unconditional")]
)

(define_insn "smin<mode>3"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
        (smin:SDF (match_operand:SDF 1 "register_operand" "<F_constraint>")
		  (match_operand:SDF 2 "register_operand" "<F_constraint>")))]
  "TARGET_HARD_FLOAT && TARGET_VFP5 <vfp_double_cond>"
  "vminnm.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "f_minmax<vfp_type>")
   (set_attr "conds" "unconditional")]
)

;; Scalar forms for the IEEE-754 fmax()/fmin() functions

(define_insn "neon_<fmaxmin_op>hf"
 [(set
   (match_operand:HF 0 "s_register_operand" "=w")
   (unspec:HF
    [(match_operand:HF 1 "s_register_operand" "w")
     (match_operand:HF 2 "s_register_operand" "w")]
    VMAXMINFNM))]
 "TARGET_VFP_FP16INST"
 "<fmaxmin_op>.f16\t%0, %1, %2"
 [(set_attr "conds" "unconditional")
  (set_attr "type" "f_minmaxs")]
)

(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:SDF 0 "s_register_operand" "=<F_constraint>")
	(unspec:SDF [(match_operand:SDF 1 "s_register_operand" "<F_constraint>")
		     (match_operand:SDF 2 "s_register_operand" "<F_constraint>")]
		     VMAXMINFNM))]
  "TARGET_HARD_FLOAT && TARGET_VFP5 <vfp_double_cond>"
  "<fmaxmin_op>.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "f_minmax<vfp_type>")
   (set_attr "conds" "unconditional")]
)

;; Write Floating-point Status and Control Register.
(define_insn "set_fpscr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] VUNSPEC_SET_FPSCR)]
  "TARGET_VFP && TARGET_HARD_FLOAT"
  "mcr\\tp10, 7, %0, cr1, cr0, 0\\t @SET_FPSCR"
  [(set_attr "type" "mrs")])

;; Read Floating-point Status and Control Register.
(define_insn "get_fpscr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(const_int 0)] VUNSPEC_GET_FPSCR))]
  "TARGET_VFP && TARGET_HARD_FLOAT"
  "mrc\\tp10, 7, %0, cr1, cr0, 0\\t @GET_FPSCR"
  [(set_attr "type" "mrs")])


;; Unimplemented insns:
;; fldm*
;; fstm*
;; fmdhr et al (VFPv1)
;; Support for xD (single precision only) variants.
;; fmrrs, fmsrr
