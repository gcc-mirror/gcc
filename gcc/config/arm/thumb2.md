;; ARM Thumb-2 Machine Description
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

;; We use the '0' constraint for operand 1 because reload should
;; be smart enough to generate an appropriate move for the r/r/r case.
(define_insn_and_split "*thumb2_smaxsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=r,l,r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "%0,0,0")
		 (match_operand:SI 2 "arm_rhs_operand"    "r,Py,I")))
   (clobber (reg:CC CC_REGNUM))]
   "TARGET_THUMB2"
   "#"
   ; cmp\\t%1, %2\;it\\tlt\;movlt\\t%0, %2
  "TARGET_THUMB2 && reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (cond_exec (lt:SI (reg:CC CC_REGNUM) (const_int 0))
              (set (match_dup 0)
                   (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "length" "6,6,10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_sminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,l,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "%0,0,0")
		 (match_operand:SI 2 "arm_rhs_operand" "r,Py,I")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "#"
  ; cmp\\t%1, %2\;it\\tge\;movge\\t%0, %2
  "TARGET_THUMB2 && reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (cond_exec (ge:SI (reg:CC CC_REGNUM) (const_int 0))
              (set (match_dup 0)
                   (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "length" "6,6,10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb32_umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,l,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "%0,0,0")
		 (match_operand:SI 2 "arm_rhs_operand" "r,Py,I")))
  (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "#"
  ; cmp\\t%1, %2\;it\\tcc\;movcc\\t%0, %2
  "TARGET_THUMB2 && reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (cond_exec (ltu:SI (reg:CC CC_REGNUM) (const_int 0))
              (set (match_dup 0)
                   (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "6,6,10")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,l,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "%0,0,0")
		 (match_operand:SI 2 "arm_rhs_operand" "r,Py,I")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "#"
  ; cmp\\t%1, %2\;it\\tcs\;movcs\\t%0, %2
  "TARGET_THUMB2 && reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (cond_exec (geu:SI (reg:CC CC_REGNUM) (const_int 0))
              (set (match_dup 0)
                   (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "6,6,10")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_abssi2"
  [(set (match_operand:SI         0 "s_register_operand" "=&r,l,r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "r,0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "#"
   ; eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31
   ; cmp\\t%0, #0\;it\tlt\;rsblt\\t%0, %0, #0
   ; cmp\\t%0, #0\;it\tlt\;rsblt\\t%0, %0, #0
  "&& reload_completed"
  [(const_int 0)]
  {
    if (REGNO(operands[0]) == REGNO(operands[1]))
      {
       rtx cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);

       emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (CCmode, operands[0],
       							const0_rtx)));
       emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                    (gen_rtx_LT (SImode,
                                                 cc_reg,
                                                 const0_rtx)),
                                    (gen_rtx_SET (operands[0],
                                                  (gen_rtx_MINUS (SImode,
                                                                  const0_rtx,
                                                                  operands[1]))))));
      }
    else
      {
        emit_insn (gen_rtx_SET (operands[0],
                                gen_rtx_XOR (SImode,
                                             gen_rtx_ASHIFTRT (SImode,
                                                               operands[1],
                                                               GEN_INT (31)),
                                             operands[1])));
        emit_insn (gen_rtx_SET (operands[0],
                                gen_rtx_MINUS (SImode,
                                               operands[0],
                                               gen_rtx_ASHIFTRT (SImode,
                                                                 operands[1],
                                                                 GEN_INT (31)))));
      }
    DONE;
  }
  [(set_attr "conds" "*,clob,clob")
   (set_attr "shift" "1")
   (set_attr "predicable" "yes,no,no")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "ce_count" "2")
   (set_attr "length" "8,6,10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,l,r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "r,0,0"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "#"
   ; eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31
   ; cmp\\t%0, #0\;it\\tgt\;rsbgt\\t%0, %0, #0
   ; cmp\\t%0, #0\;it\\tgt\;rsbgt\\t%0, %0, #0
  "&& reload_completed"
  [(const_int 0)]
  {
    if (REGNO(operands[0]) == REGNO(operands[1]))
      {
       rtx cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);

       emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (CCmode, operands[0],
							const0_rtx)));
       emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                    (gen_rtx_GT (SImode,
                                                 cc_reg,
                                                 const0_rtx)),
                                    (gen_rtx_SET (operands[0],
                                                  (gen_rtx_MINUS (SImode,
                                                                  const0_rtx,
                                                                  operands[1]))))));
      }
    else
      {
        emit_insn (gen_rtx_SET (operands[0],
                                gen_rtx_XOR (SImode,
                                             gen_rtx_ASHIFTRT (SImode,
                                                               operands[1],
                                                               GEN_INT (31)),
                                             operands[1])));
        emit_insn (gen_rtx_SET (operands[0],
                                gen_rtx_MINUS (SImode,
                                               gen_rtx_ASHIFTRT (SImode,
                                                                 operands[1],
                                                                 GEN_INT (31)),
                                               operands[0])));
      }
    DONE;
  }
  [(set_attr "conds" "*,clob,clob")
   (set_attr "shift" "1")
   (set_attr "predicable" "yes,no,no")
   (set_attr "enabled_for_short_it" "yes,yes,no")
   (set_attr "ce_count" "2")
   (set_attr "length" "8,6,10")
   (set_attr "type" "multiple")]
)

;; Pop a single register as its size is preferred over a post-incremental load
(define_insn "*thumb2_pop_single"
  [(set (match_operand:SI 0 "low_register_operand" "=r")
        (mem:SI (post_inc:SI (reg:SI SP_REGNUM))))]
  "TARGET_THUMB2 && (reload_in_progress || reload_completed)"
  "pop\t{%0}"
  [(set_attr "type" "load_4")
   (set_attr "length" "2")
   (set_attr "predicable" "yes")]
)

;; We have two alternatives here for memory loads (and similarly for stores)
;; to reflect the fact that the permissible constant pool ranges differ
;; between ldr instructions taking low regs and ldr instructions taking high
;; regs.  The high register alternatives are not taken into account when
;; choosing register preferences in order to reflect their expense.
(define_insn "*thumb2_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,l,r,r,lk*r,m")
	(match_operand:SI 1 "general_operand"	   "rk,I,Py,K,j,mi,lk*r"))]
  "TARGET_THUMB2 && !TARGET_HARD_FLOAT
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
{
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"mov%?\\t%0, %1\";
    case 3: return \"mvn%?\\t%0, #%B1\";
    case 4: return \"movw%?\\t%0, %1\";
    case 5:
      /* Cannot load it directly, split to load it via MOV / MOVT.  */
      if (!MEM_P (operands[1]) && arm_disable_literal_pool)
	return \"#\";
      return \"ldr%?\\t%0, %1\";
    case 6: return \"str%?\\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "mov_reg,mov_imm,mov_imm,mvn_imm,mov_imm,load_4,store_4")
   (set_attr "length" "2,4,2,4,4,4,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no,yes,no,no,no,no")
   (set_attr "pool_range" "*,*,*,*,*,1018,*")
   (set_attr "neg_pool_range" "*,*,*,*,*,0,*")]
)

(define_insn "tls_load_dot_plus_four"
  [(set (match_operand:SI 0 "register_operand" "=l,l,r,r")
	(mem:SI (unspec:SI [(match_operand:SI 2 "register_operand" "0,1,0,1")
			    (const_int 4)
			    (match_operand 3 "" "")]
			   UNSPEC_PIC_BASE)))
   (clobber (match_scratch:SI 1 "=X,l,X,r"))]
  "TARGET_THUMB2"
  "*
  (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
			     INTVAL (operands[3]));
  return \"add\\t%2, %|pc\;ldr%?\\t%0, [%2]\";
  "
  [(set_attr "length" "4,4,6,6")
   (set_attr "type" "multiple")]
)

;; Thumb-2 always has load/store halfword instructions, so we can avoid a lot
;; of the messiness associated with the ARM patterns.
(define_insn "*thumb2_movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,l,r,m,r")
	(match_operand:HI 1 "general_operand"      "rk,I,Py,n,r,m"))]
  "TARGET_THUMB2
  && (register_operand (operands[0], HImode)
     || register_operand (operands[1], HImode))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mov%?\\t%0, %1\\t%@ movhi
   mov%?\\t%0, %1\\t%@ movhi
   movw%?\\t%0, %L1\\t%@ movhi
   strh%?\\t%1, %0\\t%@ movhi
   ldrh%?\\t%0, %1\\t%@ movhi"
  [(set_attr "type" "mov_reg,mov_imm,mov_imm,mov_imm,store_4,load_4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no,yes,no,no,no")
   (set_attr "length" "2,4,2,4,4,4")
   (set_attr "pool_range" "*,*,*,*,*,4094")
   (set_attr "neg_pool_range" "*,*,*,*,*,250")]
)

(define_insn "*thumb2_storewb_pairsi"
  [(set (match_operand:SI 0 "register_operand" "=&kr")
	(plus:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "const_int_operand" "n")))
   (set (mem:SI (plus:SI (match_dup 0) (match_dup 2)))
	(match_operand:SI 3 "register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 5 "const_int_operand" "n")))
	(match_operand:SI 4 "register_operand" "r"))]
  "TARGET_THUMB2
   && INTVAL (operands[5]) == INTVAL (operands[2]) + 4"
  "strd\\t%3, %4, [%0, %2]!"
  [(set_attr "type" "store_8")]
)

(define_insn_and_split "*thumb2_mov_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(match_operator:SI 1 "arm_comparison_operator_mode"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  "TARGET_THUMB2"
  "#"   ; "ite\\t%D1\;mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
  "TARGET_THUMB2"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (const_int 1)
                         (const_int 0)))]
  ""
  [(set_attr "conds" "use")
   (set_attr "enabled_for_short_it" "yes,no")
   (set_attr "length" "8,10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator_mode"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2
   && !arm_restrict_it
   && !arm_borrow_operation (operands[1], SImode)"
  "#"   ; "ite\\t%D1\;mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
  "&& true"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (match_dup 3)
                         (const_int 0)))]
  {
    operands[3] = GEN_INT (~0);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_mov_negscc_strict_it"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator_mode"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2
   && arm_restrict_it
   && !arm_borrow_operation (operands[1], SImode)"
  "#"   ; ";mvn\\t%0, #0 ;it\\t%D1\;mov%D1\\t%0, #0\"
  "&& reload_completed"
  [(set (match_dup 0)
        (match_dup 3))
   (cond_exec (match_dup 4)
              (set (match_dup 0)
                   (const_int 0)))]
  {
    operands[3] = GEN_INT (~0);
    machine_mode mode = GET_MODE (operands[2]);
    enum rtx_code rc = GET_CODE (operands[1]);

    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);

  }
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_mov_notscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "arm_comparison_operator_mode"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2 && !arm_restrict_it"
  "#"   ; "ite\\t%D1\;mvn%D1\\t%0, #0\;mvn%d1\\t%0, #1"
  "&& true"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (match_dup 3)
                         (match_dup 4)))]
  {
    operands[3] = GEN_INT (~1);
    operands[4] = GEN_INT (~0);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_mov_notscc_strict_it"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(not:SI (match_operator:SI 1 "arm_comparison_operator_mode"
                 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_THUMB2 && arm_restrict_it"
  "#"   ; "mvn %0, #0 ; it%d1 ; lsl%d1 %0, %0, #1"
  "&& reload_completed"
  [(set (match_dup 0)
        (match_dup 3))
   (cond_exec (match_dup 4)
              (set (match_dup 0)
                   (ashift:SI (match_dup 0)
                              (const_int 1))))]
  {
    operands[3] = GEN_INT (~0);
    operands[4] = gen_rtx_fmt_ee (GET_CODE (operands[1]),
                                  VOIDmode, operands[2], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_movsicc_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l,r,r,r,r,r,r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 3 "arm_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0 ,lPy,0 ,0,rI,K,I ,r,rI,K ,K,r")
	 (match_operand:SI 2 "arm_not_operand" "lPy,0 ,rI,K,0 ,0,rI,I,K ,rI,K,r")))]
  "TARGET_THUMB2"
  "@
   it\\t%D3\;mov%D3\\t%0, %2
   it\\t%d3\;mov%d3\\t%0, %1
   it\\t%D3\;mov%D3\\t%0, %2
   it\\t%D3\;mvn%D3\\t%0, #%B2
   it\\t%d3\;mov%d3\\t%0, %1
   it\\t%d3\;mvn%d3\\t%0, #%B1
   #
   #
   #
   #
   #
   #"
   ; alt 6: ite\\t%d3\;mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   ; alt 7: ite\\t%d3\;mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   ; alt 8: ite\\t%d3\;mov%d3\\t%0, %1\;mvn%D3\\t%0, #%B2
   ; alt 9: ite\\t%d3\;mvn%d3\\t%0, #%B1\;mov%D3\\t%0, %2
   ; alt 10: ite\\t%d3\;mvn%d3\\t%0, #%B1\;mvn%D3\\t%0, #%B2
   ; alt 11: ite\\t%d3\;mov%d3\\t%0, %1\;mov%D3\\t%0, %2
  "&& reload_completed"
  [(const_int 0)]
  {
    enum rtx_code rev_code;
    machine_mode mode;
    rtx rev_cond;

    emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                  operands[3],
                                  gen_rtx_SET (operands[0], operands[1])));
    rev_code = GET_CODE (operands[3]);
    mode = GET_MODE (operands[4]);
    if (mode == CCFPmode || mode == CCFPEmode)
      rev_code = reverse_condition_maybe_unordered (rev_code);
    else
      rev_code = reverse_condition (rev_code);

    rev_cond = gen_rtx_fmt_ee (rev_code,
                               VOIDmode,
                               gen_rtx_REG (mode, CC_REGNUM),
                               const0_rtx);
    emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                  rev_cond,
                                  gen_rtx_SET (operands[0], operands[2])));
    DONE;
  }
  [(set_attr "length" "4,4,6,6,6,6,10,8,10,10,10,6")
   (set_attr "enabled_for_short_it" "yes,yes,no,no,no,no,no,no,no,no,no,yes")
   (set_attr "conds" "use")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (if_then_else (match_operand 1 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (const_string "mvn_imm")
                          (if_then_else (match_operand 1 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (const_string "mvn_imm")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*thumb2_movsfcc_soft_insn"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(if_then_else:SF (match_operator 3 "arm_comparison_operator"
			  [(match_operand 4 "cc_register" "") (const_int 0)])
			 (match_operand:SF 1 "s_register_operand" "0,r")
			 (match_operand:SF 2 "s_register_operand" "r,0")))]
  "TARGET_THUMB2 && TARGET_SOFT_FLOAT && !TARGET_HAVE_MVE"
  "@
   it\\t%D3\;mov%D3\\t%0, %2
   it\\t%d3\;mov%d3\\t%0, %1"
  [(set_attr "length" "6,6")
   (set_attr "conds" "use")
   (set_attr "type" "multiple")]
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

(define_insn "*nonsecure_call_reg_thumb2_fpcxt"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (unspec:SI [(match_operand 3)] UNSPEC_NONSECURE_MEM)
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2 && use_cmse && TARGET_HAVE_FPCXT_CMSE"
  "blxns\\t%0"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*nonsecure_call_reg_thumb2"
  [(call (mem:SI (reg:SI R4_REGNUM))
	 (match_operand 0 "" ""))
   (use (match_operand 1 "" ""))
   (unspec:SI [(match_operand 2)] UNSPEC_NONSECURE_MEM)
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2 && use_cmse && !TARGET_HAVE_FPCXT_CMSE"
  "bl\\t__gnu_cmse_nonsecure_call"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
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

(define_insn "*nonsecure_call_value_reg_thumb2_fpcxt"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	(match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (unspec:SI [(match_operand 4)] UNSPEC_NONSECURE_MEM)
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2 && use_cmse && TARGET_HAVE_FPCXT_CMSE"
  "blxns\\t%1"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*nonsecure_call_value_reg_thumb2"
  [(set (match_operand 0 "" "")
	(call (mem:SI (reg:SI R4_REGNUM))
	      (match_operand 1 "" "")))
   (use (match_operand 2 "" ""))
   (unspec:SI [(match_operand 3)] UNSPEC_NONSECURE_MEM)
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB2 && use_cmse && !TARGET_HAVE_FPCXT_CMSE"
  "bl\\t__gnu_cmse_nonsecure_call"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*thumb2_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "l*r"))]
  "TARGET_THUMB2"
  "bx\\t%0"
  [(set_attr "conds" "clob")
   (set_attr "type" "branch")]
)
;; Don't define thumb2_load_indirect_jump because we can't guarantee label
;; addresses will have the thumb bit set correctly.


(define_insn_and_split "*thumb2_and_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts")
	(and:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])
		(match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "#"   ; "and\\t%0, %3, #1\;it\\t%D1\;mov%D1\\t%0, #0"
  "&& reload_completed"
  [(set (match_dup 0)
        (and:SI (match_dup 3) (const_int 1)))
   (cond_exec (match_dup 4) (set (match_dup 0) (const_int 0)))]
  {
    machine_mode mode = GET_MODE (operands[2]);
    enum rtx_code rc = GET_CODE (operands[1]);

    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "type" "multiple")
   (set (attr "length") (if_then_else (match_test "arm_restrict_it")
                                      (const_int 8)
                                      (const_int 10)))]
)

(define_insn_and_split "*thumb2_ior_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])
		(match_operand:SI 3 "s_register_operand" "0,?r")))]
  "TARGET_THUMB2 && !arm_restrict_it"
  "@
   it\\t%d1\;orr%d1\\t%0, %3, #1
   #"
   ; alt 1: ite\\t%D1\;mov%D1\\t%0, %3\;orr%d1\\t%0, %3, #1
   "&& reload_completed
    && REGNO (operands [0]) != REGNO (operands[3])"
   [(cond_exec (match_dup 5) (set (match_dup 0) (match_dup 3)))
    (cond_exec (match_dup 4) (set (match_dup 0)
                                  (ior:SI (match_dup 3) (const_int 1))))]
  {
    machine_mode mode = GET_MODE (operands[2]);
    enum rtx_code rc = GET_CODE (operands[1]);

    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "6,10")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_ior_scc_strict_it"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
	(ior:SI (match_operator:SI 2 "arm_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2 && arm_restrict_it"
  "#" ; orr\\t%0, %1, #1\;it\\t%D2\;mov%D2\\t%0, %1
  "&& reload_completed"
  [(set (match_dup 0) (ior:SI (match_dup 1) (const_int 1)))
   (cond_exec (match_dup 4)
     (set (match_dup 0) (match_dup 1)))]
  {
    machine_mode mode = GET_MODE (operands[3]);
    rtx_code rc = GET_CODE (operands[2]);

    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[3], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
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
	if (arm_restrict_it)
	  output_asm_insn (\"it\\t%D4\", operands);
	else
	  output_asm_insn (\"ite\\t%D4\", operands);
	break;
      default:
	abort();
      }
    if (which_alternative != 0)
      {
        output_asm_insn (\"mov%D4\\t%0, %1\", operands);
        if (arm_restrict_it && which_alternative == 2)
          output_asm_insn (\"it\\t%d4\", operands);
      }
    if (which_alternative != 1)
      output_asm_insn (\"mov%d4\\t%0, %2\", operands);
    return \"\";
  "
  [(set_attr "conds" "use")
   (set_attr "length" "6,6,10")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb2_cond_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (match_operator:SI 5 "shiftable_operator"
	 [(match_operator:SI 4 "arm_comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
          (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && !arm_restrict_it"
  "*
    if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
      return \"%i5\\t%0, %1, %2, lsr #31\";

    output_asm_insn (\"cmp\\t%2, %3\", operands);

    if (GET_CODE (operands[5]) == PLUS && TARGET_COND_ARITH)
      return \"cinc\\t%0, %1, %d4\";

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
   (set_attr "length" "14")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_cond_arith_strict_it"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
        (match_operator:SI 5 "shiftable_operator_strict_it"
	 [(match_operator:SI 4 "arm_comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI")])
          (match_operand:SI 1 "s_register_operand" "0")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && arm_restrict_it"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
      {
        /*  %i5 %0, %1, %2, lsr #31  */
        rtx shifted_op = gen_rtx_LSHIFTRT (SImode, operands[2], GEN_INT (31));
        rtx op = NULL_RTX;

        switch (GET_CODE (operands[5]))
          {
          case AND:
            op = gen_rtx_AND (SImode, shifted_op, operands[1]);
            break;
           case PLUS:
            op = gen_rtx_PLUS (SImode, shifted_op, operands[1]);
            break;
          default: gcc_unreachable ();
          }
        emit_insn (gen_rtx_SET (operands[0], op));
        DONE;
      }

    /*  "cmp  %2, %3"  */
    emit_insn (gen_rtx_SET (gen_rtx_REG (CCmode, CC_REGNUM),
                            gen_rtx_COMPARE (CCmode, operands[2],
					     operands[3])));

    if (GET_CODE (operands[5]) == AND)
      {
        /*  %i5  %0, %1, #1
            it%D4
            mov%D4  %0, #0  */
        enum rtx_code rc = reverse_condition (GET_CODE (operands[4]));
        emit_insn (gen_rtx_SET (operands[0], gen_rtx_AND (SImode, operands[1],
							  GEN_INT (1))));
        emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                      gen_rtx_fmt_ee (rc, VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM), const0_rtx),
                                      gen_rtx_SET (operands[0], const0_rtx)));
        DONE;
      }
    else
      {
        /*  it\\t%d4
            %i5%d4\\t%0, %1, #1   */
        emit_insn (gen_rtx_COND_EXEC (VOIDmode, gen_rtx_fmt_ee (GET_CODE (operands[4]),
                                                                VOIDmode,
                                                                gen_rtx_REG (CCmode, CC_REGNUM), const0_rtx),
                                                gen_rtx_SET (operands[0],
                                                            gen_rtx_PLUS (SImode,
                                                                          operands[1],
                                                                          GEN_INT (1)))));
        DONE;
      }
     FAIL;
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb2_cond_sub"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?Ts")
		  (match_operator:SI 4 "arm_comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2"
  "*
    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (which_alternative != 0)
      {
	if (arm_restrict_it)
	  {
	    output_asm_insn (\"mov\\t%0, %1\", operands);
	    output_asm_insn (\"it\\t%d4\", operands);
	  }
	else
	{
	  output_asm_insn (\"ite\\t%D4\", operands);
	  output_asm_insn (\"mov%D4\\t%0, %1\", operands);
	}
      }
    else
      output_asm_insn (\"it\\t%d4\", operands);
    return \"sub%d4\\t%0, %1, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "10,14")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*thumb2_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts")
	(neg:SI (match_operator 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && !TARGET_COND_ARITH"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    rtx cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);

    if (GET_CODE (operands[3]) == LT && operands[2] == const0_rtx)
      {
        /* Emit asr\\t%0, %1, #31 */
        emit_insn (gen_rtx_SET (operands[0],
                                gen_rtx_ASHIFTRT (SImode,
                                                  operands[1],
                                                  GEN_INT (31))));
        DONE;
      }
    else if (GET_CODE (operands[3]) == NE && !arm_restrict_it)
      {
        /* Emit subs\\t%0, %1, %2\;it\\tne\;mvnne\\t%0, #0 */
        if (CONST_INT_P (operands[2]))
          emit_insn (gen_cmpsi2_addneg (operands[0], operands[1], operands[2],
                                        gen_int_mode (-INTVAL (operands[2]),
						      SImode)));
        else
          emit_insn (gen_subsi3_compare (operands[0], operands[1], operands[2]));

        emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                      gen_rtx_NE (SImode,
                                                  cc_reg,
                                                  const0_rtx),
                                      gen_rtx_SET (operands[0],
                                                   GEN_INT (~0))));
        DONE;
      }
    else
      {
       /* Emit:  cmp\\t%1, %2\;mvn\\t%0, #0\;it\\t%D3\;mov%D3\\t%0, #0\;*/
       enum rtx_code rc = reverse_condition (GET_CODE (operands[3]));
       machine_mode mode = SELECT_CC_MODE (rc, operands[1], operands[2]);
       rtx tmp1 = gen_rtx_REG (mode, CC_REGNUM);

       emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (CCmode, operands[1],
							operands[2])));

       emit_insn (gen_rtx_SET (operands[0], GEN_INT (~0)));

       emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                     gen_rtx_fmt_ee (rc,
                                                     VOIDmode,
                                                     tmp1,
                                                     const0_rtx),
                                     gen_rtx_SET (operands[0], const0_rtx)));
       DONE;
      }
    FAIL;
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "14")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb2_csinv"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r, r")
        (if_then_else:SI
         (match_operand 1 "arm_comparison_operation" "")
         (not:SI (match_operand:SI 2 "arm_general_register_operand" "r, r"))
         (match_operand:SI 3 "reg_or_zero_operand" "r, Pz")))]
  "TARGET_COND_ARITH"
  "@
   csinv\\t%0, %3, %2, %D1
   csinv\\t%0, zr, %2, %D1"
  [(set_attr "type" "csel")
   (set_attr "predicable" "no")]
)

(define_insn "*thumb2_csinc"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r, r")
        (if_then_else:SI
         (match_operand 1 "arm_comparison_operation" "")
         (plus:SI (match_operand:SI 2 "arm_general_register_operand" "r, r")
                  (const_int 1))
         (match_operand:SI 3 "reg_or_zero_operand" "r, Pz")))]
  "TARGET_COND_ARITH"
  "@
   csinc\\t%0, %3, %2, %D1
   csinc\\t%0, zr, %2, %D1"
  [(set_attr "type" "csel")
   (set_attr "predicable" "no")]
)

(define_insn "*thumb2_csneg"
  [(set (match_operand:SI 0 "arm_general_register_operand" "=r, r")
        (if_then_else:SI
         (match_operand 1 "arm_comparison_operation" "")
         (neg:SI (match_operand:SI 2 "arm_general_register_operand" "r, r"))
         (match_operand:SI 3 "reg_or_zero_operand" "r, Pz")))]
  "TARGET_COND_ARITH"
  "@
   csneg\\t%0, %3, %2, %D1
   csneg\\t%0, zr, %2, %D1"
  [(set_attr "type" "csel")
   (set_attr "predicable" "no")]
)

(define_insn "*thumb2_movcond"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts,Ts")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL,rIL")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,TsI,?TsI")
	 (match_operand:SI 2 "arm_rhs_operand" "TsI,0,TsI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && !TARGET_COND_ARITH"
  "*
  if (GET_CODE (operands[5]) == LT
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && REG_P (operands[1]))
	{
	  if (operands[2] == const0_rtx)
	    return \"and\\t%0, %1, %3, asr #31\";
	  return \"ands\\t%0, %1, %3, asr #32\;it\\tcc\;movcc\\t%0, %2\";
	}
      else if (which_alternative != 0 && REG_P (operands[2]))
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
      if (which_alternative != 1 && REG_P (operands[1]))
	{
	  if (operands[2] == const0_rtx)
	    return \"bic\\t%0, %1, %3, asr #31\";
	  return \"bics\\t%0, %1, %3, asr #32\;it\\tcs\;movcs\\t%0, %2\";
	}
      else if (which_alternative != 0 && REG_P (operands[2]))
	{
	  if (operands[1] == const0_rtx)
	    return \"and\\t%0, %2, %3, asr #31\";
	  return \"ands\\t%0, %2, %3, asr #32\;it\tcc\;movcc\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }
  if (CONST_INT_P (operands[4])
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
      if (arm_restrict_it)
        {
          output_asm_insn (\"mov\\t%0, %1\", operands);
          output_asm_insn (\"it\\t%D5\", operands);
        }
      else
        output_asm_insn (\"ite\\t%d5\", operands);
      break;
    default:
      abort();
    }
  if (which_alternative != 0 && !(arm_restrict_it && which_alternative == 2))
    output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  if (which_alternative != 1)
    output_asm_insn (\"mov%D5\\t%0, %2\", operands);
  return \"\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "10,10,14")
   (set_attr "type" "multiple")]
)

;; Zero and sign extension instructions.

;; All supported Thumb2 implementations are armv6, so only that case is
;; provided.
(define_insn "*thumb2_extendqisi_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   sxtb%?\\t%0, %1
   ldrsb%?\\t%0, %1"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4094")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "*thumb2_zero_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   uxth%?\\t%0, %1
   ldrh%?\\t%0, %1"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4094")
   (set_attr "neg_pool_range" "*,250")]
)

(define_insn "thumb2_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_THUMB2 && arm_arch6"
  "@
   uxtb%?\\t%0, %1
   ldrb%?\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,4094")
   (set_attr "neg_pool_range" "*,250")]
)

(define_expand "thumb2_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand")
		     (match_operand:SI 1 "arm_rhs_operand"))
		(match_dup 4)
		(label_ref:SI (match_operand 3 ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 5))
	      (use (label_ref:SI (match_operand 2 "")))])]
  "TARGET_THUMB2 && !flag_pic"
{
  operands[4] = gen_rtx_MULT (SImode, operands[0], GEN_INT (4));
  operands[4] = gen_rtx_PLUS (SImode, operands[4],
			      gen_rtx_LABEL_REF (SImode, operands[2]));
  operands[4] = gen_rtx_MEM (SImode, operands[4]);
  MEM_READONLY_P (operands[4]) = 1;
  MEM_NOTRAP_P (operands[4]) = 1;
})

(define_insn "*thumb2_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref:SI (match_operand 2 "" ""))))
		(label_ref:SI (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 4 "=&r"))
	      (use (label_ref:SI (match_dup 2)))])]
  "TARGET_THUMB2 && !flag_pic"
  "* return thumb2_output_casesi(operands);"
  [(set_attr "conds" "clob")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

(define_expand "thumb2_casesi_internal_pic"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand")
		     (match_operand:SI 1 "arm_rhs_operand"))
		(match_dup 4)
		(label_ref:SI (match_operand 3 ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 5))
	      (clobber (match_scratch:SI 6))
	      (use (label_ref:SI (match_operand 2 "")))])]
  "TARGET_THUMB2 && flag_pic"
{
  operands[4] = gen_rtx_MULT (SImode, operands[0], GEN_INT (4));
  operands[4] = gen_rtx_PLUS (SImode, operands[4],
			      gen_rtx_LABEL_REF (SImode, operands[2]));
  operands[4] = gen_rtx_MEM (SImode, operands[4]);
  MEM_READONLY_P (operands[4]) = 1;
  MEM_NOTRAP_P (operands[4]) = 1;
})

(define_insn "*thumb2_casesi_internal_pic"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref:SI (match_operand 2 "" ""))))
		(label_ref:SI (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (clobber (match_scratch:SI 4 "=&r"))
	      (clobber (match_scratch:SI 5 "=r"))
	      (use (label_ref:SI (match_dup 2)))])]
  "TARGET_THUMB2 && flag_pic"
  "* return thumb2_output_casesi(operands);"
  [(set_attr "conds" "clob")
   (set_attr "length" "20")
   (set_attr "type" "multiple")]
)

(define_insn "*thumb2_return"
  [(simple_return)]
  "TARGET_THUMB2 && !IS_CMSE_ENTRY (arm_current_func_type ())"
  "* return output_return_instruction (const_true_rtx, true, false, true);"
  [(set_attr "type" "branch")
   (set_attr "length" "4")]
)

(define_insn "*thumb2_cmse_entry_return"
  [(simple_return)]
  "TARGET_THUMB2 && IS_CMSE_ENTRY (arm_current_func_type ())"
  "* return output_return_instruction (const_true_rtx, true, false, true);"
  [(set_attr "type" "branch")
   ; This is a return from a cmse_nonsecure_entry function so code will be
   ; added to clear the APSR and potentially the FPSCR if VFP is available, so
   ; we adapt the length accordingly.
   (set (attr "length")
     (if_then_else (match_test "TARGET_HARD_FLOAT")
      (const_int 34)
      (const_int 8)))
   ; We do not support predicate execution of returns from cmse_nonsecure_entry
   ; functions because we need to clear the APSR.  Since predicable has to be
   ; a constant, we had to duplicate the thumb2_return pattern for CMSE entry
   ; functions.
   (set_attr "predicable" "no")]
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
   (set_attr "length" "2")
   (set_attr "type" "alu_sreg")]
)

(define_insn "*thumb2_shiftsi3_short"
  [(set (match_operand:SI   0 "low_register_operand" "=l,l")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "low_register_operand"  "0,l")
	  (match_operand:SI 2 "low_reg_or_int_operand" "l,M")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed
   && ((GET_CODE(operands[3]) != ROTATE && GET_CODE(operands[3]) != ROTATERT)
       || REG_P (operands[2]))"
  "* return arm_output_shift(operands, 2);"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set_attr "length" "2")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
                        (if_then_else (match_operand 3 "alu_shift_operator_lsl_1_to_4")
                          (const_string "alu_shift_imm_lsl_1to4")
                          (const_string "alu_shift_imm_other"))
		      (const_string "alu_shift_reg")))]
)

(define_insn "*thumb2_mov<mode>_shortim"
  [(set (match_operand:QHSI 0 "low_register_operand" "=l")
	(match_operand:QHSI 1 "const_int_operand" "I"))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "mov%!\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")
   (set_attr "type" "mov_imm")]
)

(define_insn "*thumb2_addsi_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l,l")
	(plus:SI (match_operand:SI 1 "low_register_operand" "l,0")
		 (match_operand:SI 2 "low_reg_or_int_operand" "lPt,Ps")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "*
    HOST_WIDE_INT val;

    if (CONST_INT_P (operands[2]))
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
   (set_attr "length" "2")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "alu_imm")
                                        (const_string "alu_sreg"))
                          (const_string "alu_imm")])]
)

(define_insn "*thumb2_subsi_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(minus:SI (match_operand:SI 1 "low_register_operand" "l")
		  (match_operand:SI 2 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "sub%!\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")
   (set_attr "type" "alu_sreg")]
)

(define_peephole2
  [(set (match_operand:CC 0 "cc_register" "")
	(compare:CC (match_operand:SI 1 "low_register_operand" "")
		    (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_THUMB2
   && peep2_reg_dead_p (1, operands[1])
   && satisfies_constraint_Pw (operands[2])"
  [(parallel
    [(set (match_dup 0) (compare:CC (match_dup 1) (match_dup 2)))
     (set (match_dup 1) (plus:SI (match_dup 1) (match_dup 3)))])]
  "operands[3] = GEN_INT (- INTVAL (operands[2]));"
)

(define_peephole2
  [(match_scratch:SI 3 "l")
   (set (match_operand:CC 0 "cc_register" "")
	(compare:CC (match_operand:SI 1 "low_register_operand" "")
		    (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_THUMB2
   && satisfies_constraint_Px (operands[2])"
  [(parallel
    [(set (match_dup 0) (compare:CC (match_dup 1) (match_dup 2)))
     (set (match_dup 3) (plus:SI (match_dup 1) (match_dup 4)))])]
  "operands[4] = GEN_INT (- INTVAL (operands[2]));"
)

(define_insn "thumb2_addsi3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	  (plus:SI (match_operand:SI 1 "s_register_operand" "l,  0, r")
		   (match_operand:SI 2 "arm_add_operand"    "lPt,Ps,rIL"))
	  (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=l,l,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_THUMB2"
  "*
    HOST_WIDE_INT val;

    if (CONST_INT_P (operands[2]))
      val = INTVAL (operands[2]);
    else
      val = 0;

    if (val < 0 && const_ok_for_arm (ARM_SIGN_EXTEND (-val)))
      return \"subs\\t%0, %1, #%n2\";
    else
      return \"adds\\t%0, %1, %2\";
  "
  [(set_attr "conds" "set")
   (set_attr "length" "2,2,4")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "alus_imm")
                                        (const_string "alus_sreg"))
                          (const_string "alus_imm")
                          (if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "alus_imm")
                                        (const_string "alus_sreg"))])]
)

(define_insn "*thumb2_addsi3_compare0_scratch"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	  (plus:SI (match_operand:SI 0 "s_register_operand" "l,  r")
		   (match_operand:SI 1 "arm_add_operand"    "lPv,rIL"))
	  (const_int 0)))]
  "TARGET_THUMB2"
  "*
    HOST_WIDE_INT val;

    if (CONST_INT_P (operands[1]))
      val = INTVAL (operands[1]);
    else
      val = 0;

    if (val < 0 && const_ok_for_arm (ARM_SIGN_EXTEND (-val)))
      return \"cmp\\t%0, #%n1\";
    else
      return \"cmn\\t%0, %1\";
  "
  [(set_attr "conds" "set")
   (set_attr "length" "2,4")
   (set (attr "type") (if_then_else (match_operand 1 "const_int_operand" "")
                                    (const_string "alus_imm")
                                    (const_string "alus_sreg")))]
)

(define_insn "*thumb2_mulsi_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
        (mult:SI (match_operand:SI 1 "low_register_operand" "%0")
                 (match_operand:SI 2 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && optimize_size && reload_completed"
  "mul%!\\t%0, %2, %0"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")
   (set_attr "type" "muls")])

(define_insn "*thumb2_mulsi_short_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ
         (mult:SI (match_operand:SI 1 "register_operand" "%0")
	          (match_operand:SI 2 "register_operand" "l"))
         (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=l")
	(mult:SI (match_dup 1) (match_dup 2)))]
  "TARGET_THUMB2 && optimize_size"
  "muls\\t%0, %2, %0"
  [(set_attr "length" "2")
   (set_attr "type" "muls")])

(define_insn "*thumb2_mulsi_short_compare0_scratch"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ
         (mult:SI (match_operand:SI 1 "register_operand" "%0")
	          (match_operand:SI 2 "register_operand" "l"))
         (const_int 0)))
   (clobber (match_scratch:SI 0 "=l"))]
  "TARGET_THUMB2 && optimize_size"
  "muls\\t%0, %2, %0"
  [(set_attr "length" "2")
   (set_attr "type" "muls")])

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
	         (not (match_test "which_alternative")))
	    (const_int 2)
	    (const_int 8)))
   (set_attr "type" "branch,multiple")]
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
	         (not (match_test "which_alternative")))
	    (const_int 2)
	    (const_int 8)))
   (set_attr "type" "branch,multiple")]
)

(define_insn "*thumb2_one_cmplsi2_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(not:SI (match_operand:SI 1 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "mvn%!\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")
   (set_attr "type" "mvn_reg")]
)

(define_insn "*thumb2_negsi2_short"
  [(set (match_operand:SI 0 "low_register_operand" "=l")
	(neg:SI (match_operand:SI 1 "low_register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && reload_completed"
  "rsb%!\t%0, %1, #0"
  [(set_attr "predicable" "yes")
   (set_attr "length" "2")
   (set_attr "type" "alu_sreg")]
)

(define_insn "*orsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "orn%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "logic_reg")]
)

(define_insn "*orsi_not_shiftsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (not:SI (match_operator:SI 4 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "const_int_operand" "M")]))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_THUMB2"
  "orn%?\\t%0, %1, %2%S4"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "2")
   (set_attr "autodetect_type" "alu_shift_operator4")]
)

(define_peephole2
  [(set (match_operand:CC_NZ 0 "cc_register" "")
	(compare:CC_NZ (zero_extract:SI
			  (match_operand:SI 1 "low_register_operand" "")
			  (const_int 1)
			  (match_operand:SI 2 "const_int_operand" ""))
			 (const_int 0)))
   (match_scratch:SI 3 "l")
   (set (pc)
	(if_then_else (match_operator:CC_NZ 4 "equality_operator"
		       [(match_dup 0) (const_int 0)])
		      (match_operand 5 "" "")
		      (match_operand 6 "" "")))]
  "TARGET_THUMB2
   && (INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) < 32)
   && peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 0)
		   (compare:CC_NZ (ashift:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (clobber (match_dup 3))])
   (set (pc)
	(if_then_else (match_op_dup 4 [(match_dup 0) (const_int 0)])
		      (match_dup 5) (match_dup 6)))]
  "
  operands[2] = GEN_INT (31 - INTVAL (operands[2]));
  operands[4] = gen_rtx_fmt_ee (GET_CODE (operands[4]) == NE ? LT : GE,
				VOIDmode, operands[0], const0_rtx);
  ")

(define_peephole2
  [(set (match_operand:CC_NZ 0 "cc_register" "")
	(compare:CC_NZ (zero_extract:SI
			  (match_operand:SI 1 "low_register_operand" "")
			  (match_operand:SI 2 "const_int_operand" "")
			  (const_int 0))
			 (const_int 0)))
   (match_scratch:SI 3 "l")
   (set (pc)
	(if_then_else (match_operator:CC_NZ 4 "equality_operator"
		       [(match_dup 0) (const_int 0)])
		      (match_operand 5 "" "")
		      (match_operand 6 "" "")))]
  "TARGET_THUMB2
   && (INTVAL (operands[2]) > 0 && INTVAL (operands[2]) < 32)
   && peep2_reg_dead_p (2, operands[0])"
  [(parallel [(set (match_dup 0)
		   (compare:CC_NZ (ashift:SI (match_dup 1) (match_dup 2))
				  (const_int 0)))
	      (clobber (match_dup 3))])
   (set (pc)
	(if_then_else (match_op_dup 4 [(match_dup 0) (const_int 0)])
		      (match_dup 5) (match_dup 6)))]
  "
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));
  ")

;; Define the subtract-one-and-jump insns so loop.c
;; knows what to generate.
(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))      ; loop pseudo
   (use (match_operand 1 "" ""))]     ; label
  "TARGET_32BIT"
  "
{
   /* Currently SMS relies on the do-loop pattern to recognize loops
      where (1) the control part consists of all insns defining and/or
      using a certain 'count' register and (2) the loop count can be
      adjusted by modifying this register prior to the loop.
      ??? The possible introduction of a new block to initialize the
      new IV can potentially affect branch optimizations.

      Also used to implement the low over head loops feature, which is part of
      the Armv8.1-M Mainline Low Overhead Branch (LOB) extension.  */
  if (optimize > 0 && (flag_modulo_sched || TARGET_HAVE_LOB))
    {
      rtx s0;
      rtx bcomp;
      rtx loc_ref;
      rtx cc_reg;
      rtx insn;
      rtx cmp;
      int decrement_num;

      if (GET_MODE (operands[0]) != SImode)
	FAIL;

      s0 = operands[0];

      if (TARGET_HAVE_LOB
	  && arm_target_bb_ok_for_lob (BLOCK_FOR_INSN (operands[1])))
	{
	  /* If we have a compatible MVE target, try and analyse the loop
	     contents to determine if we can use predicated dlstp/letp
	     looping.  These patterns implicitly use LR as the loop counter.  */
	  if (TARGET_HAVE_MVE
	      && ((decrement_num = arm_attempt_dlstp_transform (operands[1]))
		  != 1))
	    {
	      loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands[1]);
	      switch (decrement_num)
		{
		  case 2:
		    insn = gen_predicated_doloop_end_internal2 (loc_ref);
		    break;
		  case 4:
		    insn = gen_predicated_doloop_end_internal4 (loc_ref);
		    break;
		  case 8:
		    insn = gen_predicated_doloop_end_internal8 (loc_ref);
		    break;
		  case 16:
		    insn = gen_predicated_doloop_end_internal16 (loc_ref);
		    break;
		  default:
		    gcc_unreachable ();
		}
	      emit_jump_insn (insn);
	      DONE;
	    }
	  /* Remaining LOB cases need to explicitly use LR.  */
	  s0 = gen_rtx_REG (SImode, LR_REGNUM);
	}

	/* Otherwise, try standard decrement-by-one dls/le looping.  */
	if (TARGET_THUMB2)
	  insn = emit_insn (gen_thumb2_addsi3_compare0 (s0, s0,
							GEN_INT (-1)));
	else
	  insn = emit_insn (gen_addsi3_compare0 (s0, s0, GEN_INT (-1)));

	cmp = XVECEXP (PATTERN (insn), 0, 0);
	cc_reg = SET_DEST (cmp);
	bcomp = gen_rtx_NE (VOIDmode, cc_reg, const0_rtx);
	loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands[1]);
	emit_jump_insn (gen_rtx_SET (pc_rtx,
				     gen_rtx_IF_THEN_ELSE (VOIDmode, bcomp,
							   loc_ref, pc_rtx)));
	DONE;
    }
  else
    FAIL;
}")

(define_insn "*clear_apsr"
  [(unspec_volatile:SI [(const_int 0)] VUNSPEC_CLRM_APSR)
  (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB2 && TARGET_HAVE_FPCXT_CMSE && use_cmse"
  "clrm%?\\t{APSR}"
  [(set_attr "predicable" "yes")]
)

;; The operands are validated through the clear_multiple_operation
;; match_parallel predicate rather than through constraints so enable it only
;; after reload.
(define_insn "*clear_multiple"
  [(match_parallel 0 "clear_multiple_operation"
     [(set (match_operand:SI 1 "register_operand" "")
	   (const_int 0))])]
  "TARGET_THUMB2 && TARGET_HAVE_FPCXT_CMSE && use_cmse && reload_completed"
  {
    char pattern[100];
    int i, num_saves = XVECLEN (operands[0], 0);

    strcpy (pattern, \"clrm%?\\t{\");
    for (i = 0; i < num_saves; i++)
      {
	if (GET_CODE (XVECEXP (operands[0], 0, i)) == UNSPEC_VOLATILE)
	  {
	    strcat (pattern, \"APSR\");
	    ++i;
	  }
	else
	  strcat (pattern,
		  reg_names[REGNO (XEXP (XVECEXP (operands[0], 0, i), 0))]);
	if (i < num_saves - 1)
	  strcat (pattern, \", %|\");
      }
    strcat (pattern, \"}\");
    output_asm_insn (pattern, operands);
    return \"\";
  }
  [(set_attr "predicable" "yes")]
)

(define_insn "thumb2_asrl"
  [(set (match_operand:DI 0 "arm_general_register_operand" "+r")
	(ashiftrt:DI (match_dup 0)
		     (match_operand:SI 1 "arm_reg_or_long_shift_imm" "rPg")))]
  "TARGET_HAVE_MVE"
  "asrl%?\\t%Q0, %R0, %1"
  [(set_attr "predicable" "yes")])

(define_insn "thumb2_lsll"
  [(set (match_operand:DI 0 "arm_general_register_operand" "+r")
	(ashift:DI (match_dup 0)
		   (match_operand:SI 1 "arm_reg_or_long_shift_imm" "rPg")))]
  "TARGET_HAVE_MVE"
  "lsll%?\\t%Q0, %R0, %1"
  [(set_attr "predicable" "yes")])

(define_insn "thumb2_lsrl"
  [(set (match_operand:DI 0 "arm_general_register_operand" "+r")
	(lshiftrt:DI (match_dup 0)
		     (match_operand:SI 1 "long_shift_imm" "Pg")))]
  "TARGET_HAVE_MVE"
  "lsrl%?\\t%Q0, %R0, %1"
  [(set_attr "predicable" "yes")])

;; Originally expanded by 'doloop_end'.
(define_insn "*doloop_end_internal"
  [(set (pc)
        (if_then_else
            (ne (reg:SI LR_REGNUM) (const_int 1))
          (label_ref (match_operand 0 "" ""))
          (pc)))
   (set (reg:SI LR_REGNUM)
        (plus:SI (reg:SI LR_REGNUM) (const_int -1)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && TARGET_HAVE_LOB"
  {
    if (get_attr_length (insn) == 4)
      return "le\t%|lr, %l0";
    else
      return "subs\t%|lr, #1;bne\t%l0";
  }
  [(set (attr "length")
        (if_then_else
            (ltu (minus (pc) (match_dup 0)) (const_int 1024))
	    (const_int 4)
	    (const_int 6)))
   (set_attr "type" "branch")])

(define_expand "doloop_begin"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")]
  "TARGET_32BIT && TARGET_HAVE_LOB"
  {
    if (REGNO (operands[0]) == LR_REGNUM)
      {
	/* Pick out the number by which we are decrementing the loop counter
	   in every iteration.  If it's > 1, then use dlstp.  */
	int const_int_dec_num
	     = abs (INTVAL (XEXP (XEXP (XVECEXP (PATTERN (operands[1]), 0, 1),
				  1),
			    1)));
	switch (const_int_dec_num)
	  {
	    case 16:
	      emit_insn (gen_dlstp8_insn (operands[0]));
	      break;

	    case 8:
	      emit_insn (gen_dlstp16_insn (operands[0]));
	      break;

	    case 4:
	      emit_insn (gen_dlstp32_insn (operands[0]));
	      break;

	    case 2:
	      emit_insn (gen_dlstp64_insn (operands[0]));
	      break;

	    case 1:
	      emit_insn (gen_dls_insn (operands[0]));
	      break;

	    default:
	      gcc_unreachable ();
	  }
	DONE;
      }
    else
      FAIL;
  })

(define_insn "dls_insn"
  [(set (reg:SI LR_REGNUM)
        (unspec:SI [(match_operand:SI 0 "s_register_operand" "r")] UNSPEC_DLS))]
  "TARGET_32BIT && TARGET_HAVE_LOB"
  "dls\t%|lr, %0")
