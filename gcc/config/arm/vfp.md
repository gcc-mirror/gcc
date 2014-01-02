;; ARM VFP instruction patterns
;; Copyright (C) 2003-2014 Free Software Foundation, Inc.
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
      return \"fmsr%?\\t%0, %1\\t%@ int\";
    case 7:
      return \"fmrs%?\\t%0, %1\\t%@ int\";
    case 8:
      return \"fcpys%?\\t%0, %1\\t%@ int\";
    case 9: case 10:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "type" "mov_reg,mov_reg,mvn_imm,mov_imm,load1,store1,f_mcr,f_mrc,fmov,f_loads,f_stores")
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
      return \"fmsr%?\\t%0, %1\\t%@ int\";
    case 10:
      return \"fmrs%?\\t%0, %1\\t%@ int\";
    case 11:
      return \"fcpys%?\\t%0, %1\\t%@ int\";
    case 12: case 13:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no,yes,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "type" "mov_reg,mov_reg,mov_reg,mvn_reg,mov_reg,load1,load1,store1,store1,f_mcr,f_mrc,fmov,f_loads,f_stores")
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
      return \"fmdrr%?\\t%P0, %Q1, %R1\\t%@ int\";
    case 8:
      return \"fmrrd%?\\t%Q0, %R0, %P1\\t%@ int\";
    case 9:
      if (TARGET_VFP_SINGLE)
	return \"fcpys%?\\t%0, %1\\t%@ int\;fcpys%?\\t%p0, %p1\\t%@ int\";
      else
	return \"fcpyd%?\\t%P0, %P1\\t%@ int\";
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
      return \"fmdrr%?\\t%P0, %Q1, %R1\\t%@ int\";
    case 8:
      return \"fmrrd%?\\t%Q0, %R0, %P1\\t%@ int\";
    case 9:
      return \"fcpyd%?\\t%P0, %P1\\t%@ int\";
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
(define_insn "*movhf_vfp_neon"
  [(set (match_operand:HF 0 "nonimmediate_operand" "= t,Um,r,m,t,r,t,r,r")
	(match_operand:HF 1 "general_operand"	   " Um, t,m,r,t,r,r,t,F"))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_NEON_FP16
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
      return \"fcpys\\t%0, %1\";
    case 5:	/* ARM register from ARM register */
      return \"mov\\t%0, %1\\t%@ __fp16\";
    case 6:	/* S register from ARM register */
      return \"fmsr\\t%0, %1\";
    case 7:	/* ARM register from S register */
      return \"fmrs\\t%0, %1\";
    case 8:	/* ARM register from constant */
      {
        REAL_VALUE_TYPE r;
	long bits;
	rtx ops[4];

        REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
	bits = real_to_target (NULL, &r, HFmode);
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
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FP16 && !TARGET_NEON_FP16
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
      return \"fcpys\\t%0, %1\";
    case 3:	/* ARM register from ARM register */
      return \"mov\\t%0, %1\\t%@ __fp16\";
    case 4:	/* S register from ARM register */
      return \"fmsr\\t%0, %1\";
    case 5:	/* ARM register from S register */
      return \"fmrs\\t%0, %1\";
    case 6:	/* ARM register from constant */
      {
        REAL_VALUE_TYPE r;
	long bits;
	rtx ops[4];

        REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
	bits = real_to_target (NULL, &r, HFmode);
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
      return \"fmsr%?\\t%0, %1\";
    case 1:
      return \"fmrs%?\\t%0, %1\";
    case 2:
      return \"fconsts%?\\t%0, #%G1\";
    case 3: case 4:
      return output_move_vfp (operands);
    case 5:
      return \"ldr%?\\t%0, %1\\t%@ float\";
    case 6:
      return \"str%?\\t%1, %0\\t%@ float\";
    case 7:
      return \"fcpys%?\\t%0, %1\";
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
      return \"fmsr%?\\t%0, %1\";
    case 1:
      return \"fmrs%?\\t%0, %1\";
    case 2:
      return \"fconsts%?\\t%0, #%G1\";
    case 3: case 4:
      return output_move_vfp (operands);
    case 5:
      return \"ldr%?\\t%0, %1\\t%@ float\";
    case 6:
      return \"str%?\\t%1, %0\\t%@ float\";
    case 7:
      return \"fcpys%?\\t%0, %1\";
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
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=w,?r,w ,w  ,Uv,r, m,w,r")
	(match_operand:DF 1 "soft_df_operand"		   " ?r,w,Dy,UvF,w ,mF,r,w,r"))]
  "TARGET_ARM && TARGET_HARD_FLOAT && TARGET_VFP
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  {
    switch (which_alternative)
      {
      case 0:
	return \"fmdrr%?\\t%P0, %Q1, %R1\";
      case 1:
	return \"fmrrd%?\\t%Q0, %R0, %P1\";
      case 2:
	gcc_assert (TARGET_VFP_DOUBLE);
        return \"fconstd%?\\t%P0, #%G1\";
      case 3: case 4:
	return output_move_vfp (operands);
      case 5: case 6:
	return output_move_double (operands, true, NULL);
      case 7:
	if (TARGET_VFP_SINGLE)
	  return \"fcpys%?\\t%0, %1\;fcpys%?\\t%p0, %p1\";
	else
	  return \"fcpyd%?\\t%P0, %P1\";
      case 8:
        return \"#\";
      default:
	gcc_unreachable ();
      }
    }
  "
  [(set_attr "type" "f_mcrr,f_mrrc,fconstd,f_loadd,f_stored,\
                     load2,store2,ffarithd,multiple")
   (set (attr "length") (cond [(eq_attr "alternative" "5,6,8") (const_int 8)
			       (eq_attr "alternative" "7")
				(if_then_else
				 (match_test "TARGET_VFP_SINGLE")
				 (const_int 8)
				 (const_int 4))]
			      (const_int 4)))
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,*,*,1020,*,1020,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,1004,*,1004,*,*,*")]
)

(define_insn "*thumb2_movdf_vfp"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=w,?r,w ,w  ,Uv,r ,m,w,r")
	(match_operand:DF 1 "soft_df_operand"		   " ?r,w,Dy,UvF,w, mF,r, w,r"))]
  "TARGET_THUMB2 && TARGET_HARD_FLOAT && TARGET_VFP
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  {
    switch (which_alternative)
      {
      case 0:
	return \"fmdrr%?\\t%P0, %Q1, %R1\";
      case 1:
	return \"fmrrd%?\\t%Q0, %R0, %P1\";
      case 2:
	gcc_assert (TARGET_VFP_DOUBLE);
	return \"fconstd%?\\t%P0, #%G1\";
      case 3: case 4:
	return output_move_vfp (operands);
      case 5: case 6: case 8:
	return output_move_double (operands, true, NULL);
      case 7:
	if (TARGET_VFP_SINGLE)
	  return \"fcpys%?\\t%0, %1\;fcpys%?\\t%p0, %p1\";
	else
	  return \"fcpyd%?\\t%P0, %P1\";
      default:
	abort ();
      }
    }
  "
  [(set_attr "type" "f_mcrr,f_mrrc,fconstd,f_loadd,\
                     f_stored,load2,store2,ffarithd,multiple")
   (set (attr "length") (cond [(eq_attr "alternative" "5,6,8") (const_int 8)
			       (eq_attr "alternative" "7")
				(if_then_else
				 (match_test "TARGET_VFP_SINGLE")
				 (const_int 8)
				 (const_int 4))]
			      (const_int 4)))
   (set_attr "pool_range" "*,*,*,1018,*,4094,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,1008,*,0,*,*,*")]
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
   fcpys%D3\\t%0, %2
   fcpys%d3\\t%0, %1
   fcpys%D3\\t%0, %2\;fcpys%d3\\t%0, %1
   fmsr%D3\\t%0, %2
   fmsr%d3\\t%0, %1
   fmsr%D3\\t%0, %2\;fmsr%d3\\t%0, %1
   fmrs%D3\\t%0, %2
   fmrs%d3\\t%0, %1
   fmrs%D3\\t%0, %2\;fmrs%d3\\t%0, %1"
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
   it\\t%D3\;fcpys%D3\\t%0, %2
   it\\t%d3\;fcpys%d3\\t%0, %1
   ite\\t%D3\;fcpys%D3\\t%0, %2\;fcpys%d3\\t%0, %1
   it\\t%D3\;fmsr%D3\\t%0, %2
   it\\t%d3\;fmsr%d3\\t%0, %1
   ite\\t%D3\;fmsr%D3\\t%0, %2\;fmsr%d3\\t%0, %1
   it\\t%D3\;fmrs%D3\\t%0, %2
   it\\t%d3\;fmrs%d3\\t%0, %1
   ite\\t%D3\;fmrs%D3\\t%0, %2\;fmrs%d3\\t%0, %1"
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
   fcpyd%D3\\t%P0, %P2
   fcpyd%d3\\t%P0, %P1
   fcpyd%D3\\t%P0, %P2\;fcpyd%d3\\t%P0, %P1
   fmdrr%D3\\t%P0, %Q2, %R2
   fmdrr%d3\\t%P0, %Q1, %R1
   fmdrr%D3\\t%P0, %Q2, %R2\;fmdrr%d3\\t%P0, %Q1, %R1
   fmrrd%D3\\t%Q0, %R0, %P2
   fmrrd%d3\\t%Q0, %R0, %P1
   fmrrd%D3\\t%Q0, %R0, %P2\;fmrrd%d3\\t%Q0, %R0, %P1"
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
   it\\t%D3\;fcpyd%D3\\t%P0, %P2
   it\\t%d3\;fcpyd%d3\\t%P0, %P1
   ite\\t%D3\;fcpyd%D3\\t%P0, %P2\;fcpyd%d3\\t%P0, %P1
   it\t%D3\;fmdrr%D3\\t%P0, %Q2, %R2
   it\t%d3\;fmdrr%d3\\t%P0, %Q1, %R1
   ite\\t%D3\;fmdrr%D3\\t%P0, %Q2, %R2\;fmdrr%d3\\t%P0, %Q1, %R1
   it\t%D3\;fmrrd%D3\\t%Q0, %R0, %P2
   it\t%d3\;fmrrd%d3\\t%Q0, %R0, %P1
   ite\\t%D3\;fmrrd%D3\\t%Q0, %R0, %P2\;fmrrd%d3\\t%Q0, %R0, %P1"
   [(set_attr "conds" "use")
    (set_attr "length" "6,6,10,6,6,10,6,6,10")
    (set_attr "type" "ffarithd,ffarithd,ffarithd,f_mcr,f_mcr,f_mcrr,f_mrrc,f_mrrc,f_mrrc")]
)


;; Sign manipulation functions

(define_insn "*abssf2_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=t")
	(abs:SF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fabss%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffariths")]
)

(define_insn "*absdf2_vfp"
  [(set (match_operand:DF	  0 "s_register_operand" "=w")
	(abs:DF (match_operand:DF 1 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fabsd%?\\t%P0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "ffarithd")]
)

(define_insn "*negsf2_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=t,?r")
	(neg:SF (match_operand:SF 1 "s_register_operand" "t,r")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   fnegs%?\\t%0, %1
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
   fnegd%?\\t%P0, %P1
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
          emit_insn (gen_rtx_SET (SImode, out_lo, in_lo));
	  operands[0] = out_hi;
          operands[1] = in_hi;
        }
      else
        {
          emit_insn (gen_rtx_SET (SImode, out_hi, in_hi));
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


;; Arithmetic insns

(define_insn "*addsf3_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=t")
	(plus:SF (match_operand:SF 1 "s_register_operand" "t")
		 (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fadds%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fadds")]
)

(define_insn "*adddf3_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=w")
	(plus:DF (match_operand:DF 1 "s_register_operand" "w")
		 (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "faddd%?\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "faddd")]
)


(define_insn "*subsf3_vfp"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(minus:SF (match_operand:SF 1 "s_register_operand" "t")
		  (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fsubs%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fadds")]
)

(define_insn "*subdf3_vfp"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(minus:DF (match_operand:DF 1 "s_register_operand" "w")
		  (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fsubd%?\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "faddd")]
)


;; Division insns

(define_insn "*divsf3_vfp"
  [(set (match_operand:SF	  0 "s_register_operand" "=t")
	(div:SF (match_operand:SF 1 "s_register_operand" "t")
		(match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fdivs%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fdivs")]
)

(define_insn "*divdf3_vfp"
  [(set (match_operand:DF	  0 "s_register_operand" "=w")
	(div:DF (match_operand:DF 1 "s_register_operand" "w")
		(match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fdivd%?\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fdivd")]
)


;; Multiplication insns

(define_insn "*mulsf3_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=t")
	(mult:SF (match_operand:SF 1 "s_register_operand" "t")
		 (match_operand:SF 2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fmuls%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuls")]
)

(define_insn "*muldf3_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=w")
	(mult:DF (match_operand:DF 1 "s_register_operand" "w")
		 (match_operand:DF 2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fmuld%?\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuld")]
)

(define_insn "*mulsf3negsf_vfp"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(mult:SF (neg:SF (match_operand:SF 1 "s_register_operand" "t"))
		 (match_operand:SF	   2 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fnmuls%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuls")]
)

(define_insn "*muldf3negdf_vfp"
  [(set (match_operand:DF		   0 "s_register_operand" "=w")
	(mult:DF (neg:DF (match_operand:DF 1 "s_register_operand" "w"))
		 (match_operand:DF	   2 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fnmuld%?\\t%P0, %P1, %P2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmuld")]
)


;; Multiply-accumulate insns

;; 0 = 1 * 2 + 0
(define_insn "*mulsf3addsf_vfp"
  [(set (match_operand:SF		    0 "s_register_operand" "=t")
	(plus:SF (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			  (match_operand:SF 3 "s_register_operand" "t"))
		 (match_operand:SF	    1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fmacs%?\\t%0, %2, %3"
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
  "fmacd%?\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; 0 = 1 * 2 - 0
(define_insn "*mulsf3subsf_vfp"
  [(set (match_operand:SF		     0 "s_register_operand" "=t")
	(minus:SF (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			   (match_operand:SF 3 "s_register_operand" "t"))
		  (match_operand:SF	     1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fmscs%?\\t%0, %2, %3"
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
  "fmscd%?\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; 0 = -(1 * 2) + 0
(define_insn "*mulsf3negsfaddsf_vfp"
  [(set (match_operand:SF		     0 "s_register_operand" "=t")
	(minus:SF (match_operand:SF	     1 "s_register_operand" "0")
		  (mult:SF (match_operand:SF 2 "s_register_operand" "t")
			   (match_operand:SF 3 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fnmacs%?\\t%0, %2, %3"
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
  "fnmacd%?\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)


;; 0 = -(1 * 2) - 0
(define_insn "*mulsf3negsfsubsf_vfp"
  [(set (match_operand:SF		      0 "s_register_operand" "=t")
	(minus:SF (mult:SF
		    (neg:SF (match_operand:SF 2 "s_register_operand" "t"))
		    (match_operand:SF	      3 "s_register_operand" "t"))
		  (match_operand:SF	      1 "s_register_operand" "0")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fnmscs%?\\t%0, %2, %3"
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
  "fnmscd%?\\t%P0, %P2, %P3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fmacd")]
)

;; Fused-multiply-accumulate

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
  "fcvtds%?\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "*truncdfsf2_vfp"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(float_truncate:SF (match_operand:DF 1 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fcvtsd%?\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "extendhfsf2"
  [(set (match_operand:SF		   0 "s_register_operand" "=t")
	(float_extend:SF (match_operand:HF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FP16"
  "vcvtb%?.f32.f16\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "truncsfhf2"
  [(set (match_operand:HF		   0 "s_register_operand" "=t")
	(float_truncate:HF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FP16"
  "vcvtb%?.f16.f32\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvt")]
)

(define_insn "*truncsisf2_vfp"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(fix:SI (fix:SF (match_operand:SF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "ftosizs%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)

(define_insn "*truncsidf2_vfp"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(fix:SI (fix:DF (match_operand:DF 1 "s_register_operand" "w"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "ftosizd%?\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)


(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(unsigned_fix:SI (fix:SF (match_operand:SF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "ftouizs%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)

(define_insn "fixuns_truncdfsi2"
  [(set (match_operand:SI		  0 "s_register_operand" "=t")
	(unsigned_fix:SI (fix:DF (match_operand:DF 1 "s_register_operand" "t"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "ftouizd%?\\t%0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvtf2i")]
)


(define_insn "*floatsisf2_vfp"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(float:SF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fsitos%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)

(define_insn "*floatsidf2_vfp"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(float:DF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fsitod%?\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)


(define_insn "floatunssisf2"
  [(set (match_operand:SF	    0 "s_register_operand" "=t")
	(unsigned_float:SF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fuitos%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)

(define_insn "floatunssidf2"
  [(set (match_operand:DF	    0 "s_register_operand" "=w")
	(unsigned_float:DF (match_operand:SI 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fuitod%?\\t%P0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_cvti2f")]
)


;; Sqrt insns.

(define_insn "*sqrtsf2_vfp"
  [(set (match_operand:SF	   0 "s_register_operand" "=t")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "t")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fsqrts%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fsqrts")]
)

(define_insn "*sqrtdf2_vfp"
  [(set (match_operand:DF	   0 "s_register_operand" "=w")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "w")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "fsqrtd%?\\t%P0, %P1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fsqrtd")]
)


;; Patterns to split/copy vfp condition flags.

(define_insn "*movcc_vfp"
  [(set (reg CC_REGNUM)
	(reg VFPCC_REGNUM))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "fmstat%?"
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

(define_insn "*cmpsf_vfp"
  [(set (reg:CCFP VFPCC_REGNUM)
	(compare:CCFP (match_operand:SF 0 "s_register_operand"  "t,t")
		      (match_operand:SF 1 "vfp_compare_operand" "t,G")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "@
   fcmps%?\\t%0, %1
   fcmpzs%?\\t%0"
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
   fcmpes%?\\t%0, %1
   fcmpezs%?\\t%0"
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
   fcmpd%?\\t%P0, %P1
   fcmpzd%?\\t%P0"
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
   fcmped%?\\t%P0, %P1
   fcmpezd%?\\t%P0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "fcmpd")]
)

;; Fixed point to floating point conversions.
(define_code_iterator FCVT [unsigned_float float])
(define_code_attr FCVTI32typename [(unsigned_float "u32") (float "s32")])

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
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (fix:SF (mult:SF (match_operand:SF 1 "s_register_operand" "t")
				 (match_operand 2
				 "const_double_vcvt_power_of_two" "Dp")))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP3 && !flag_rounding_math"
  "vcvt%?.s32.f32\\t%1, %1, %v2\;vmov%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "ce_count" "2")
   (set_attr "type" "f_cvtf2i")
   (set_attr "length" "8")]
 )

;; Store multiple insn used in function prologue.
(define_insn "*push_multi_vfp"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:DF 1 "vfp_register_operand" "")]
		      UNSPEC_PUSH_MULT))])]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "* return vfp_output_fstmd (operands);"
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
  "TARGET_HARD_FLOAT && TARGET_FPU_ARMV8 <vfp_double_cond>"
  "vrint<vrint_variant>%?.<V_if_elem>\\t%<V_reg>0, %<V_reg>1"
  [(set_attr "predicable" "<vrint_predicable>")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "f_rint<vfp_type>")
   (set_attr "conds" "<vrint_conds>")]
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
  "TARGET_HARD_FLOAT && TARGET_FPU_ARMV8 <vfp_double_cond>"
  "vmaxnm.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "f_minmax<vfp_type>")
   (set_attr "conds" "unconditional")]
)

(define_insn "smin<mode>3"
  [(set (match_operand:SDF 0 "register_operand" "=<F_constraint>")
        (smin:SDF (match_operand:SDF 1 "register_operand" "<F_constraint>")
		  (match_operand:SDF 2 "register_operand" "<F_constraint>")))]
  "TARGET_HARD_FLOAT && TARGET_FPU_ARMV8 <vfp_double_cond>"
  "vminnm.<V_if_elem>\\t%<V_reg>0, %<V_reg>1, %<V_reg>2"
  [(set_attr "type" "f_minmax<vfp_type>")
   (set_attr "conds" "unconditional")]
)

;; Unimplemented insns:
;; fldm*
;; fstm*
;; fmdhr et al (VFPv1)
;; Support for xD (single precision only) variants.
;; fmrrs, fmsrr
