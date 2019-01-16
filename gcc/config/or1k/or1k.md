;; Machine description for OpenRISC
;; Copyright (C) 2018-2019 Free Software Foundation, Inc.
;; Contributed by Stafford Horne

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; OpenRISC specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

;; Register numbers
(define_constants
  [(SP_REGNUM       1)
   (HFP_REGNUM      2)
   (LR_REGNUM       9)
   (TLS_REGNUM     10)
   (RV_REGNUM      11)
   (PE_TMP_REGNUM  13)
   (AP_REGNUM      32)
   (SFP_REGNUM     33)
   (SR_F_REGNUM    34)]
)

(define_c_enum "unspec" [
  UNSPEC_SET_GOT
  UNSPEC_GOT
  UNSPEC_GOTOFF
  UNSPEC_TPOFF
  UNSPEC_GOTTPOFF
  UNSPEC_TLSGD
  UNSPEC_MSYNC
])

(define_c_enum "unspecv" [
  UNSPECV_SET_GOT
  UNSPECV_LL
  UNSPECV_SC
])

;; Instruction scheduler

; Most instructions are 4 bytes long.
(define_attr "length" "" (const_int 4))

(define_attr "type"
  "alu,st,ld,control,multi"
  (const_string "alu"))

(define_attr "insn_support" "class1,sext,sfimm,shftimm" (const_string "class1"))

(define_attr "enabled" ""
  (cond [(eq_attr "insn_support" "class1") (const_int 1)
	 (and (eq_attr "insn_support" "sext")
	      (ne (symbol_ref "TARGET_SEXT") (const_int 0))) (const_int 1)
	 (and (eq_attr "insn_support" "sfimm")
	      (ne (symbol_ref "TARGET_SFIMM") (const_int 0))) (const_int 1)
	 (and (eq_attr "insn_support" "shftimm")
	      (ne (symbol_ref "TARGET_SHFTIMM") (const_int 0))) (const_int 1)]
	(const_int 0)))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

(define_automaton "or1k")
(define_cpu_unit "cpu" "or1k")
(define_insn_reservation "alu" 1
  (eq_attr "type" "alu")
  "cpu")
(define_insn_reservation "st" 1
  (eq_attr "type" "st")
  "cpu")
(define_insn_reservation "ld" 3
  (eq_attr "type" "st")
  "cpu")
(define_insn_reservation "control" 1
  (eq_attr "type" "control")
  "cpu")

; Define delay slots for any branch
(define_delay (eq_attr "type" "control")
  [(eq_attr "type" "alu,st,ld") (nil) (nil)])

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "l.nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (plus:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  ""
  "@
  l.add\t%0, %1, %2
  l.addi\t%0, %1, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  "!TARGET_SOFT_MUL"
  "@
  l.mul\t%0, %1, %2
  l.muli\t%0, %1, %2")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (div:SI
	   (match_operand:SI 1 "register_operand" "r")
	   (match_operand:SI 2 "register_operand" "r")))]
  "!TARGET_SOFT_DIV"
  "l.div\t%0, %1, %2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (udiv:SI
	   (match_operand:SI 1 "register_operand" "r")
	   (match_operand:SI 2 "register_operand" "r")))]
  "!TARGET_SOFT_DIV"
  "l.divu\t%0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (minus:SI
	   (match_operand:SI 1 "reg_or_0_operand" "rO")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.sub\t%0, %r1, %2")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_code_iterator SHIFT  [ashift ashiftrt lshiftrt])
(define_code_attr shift_op   [(ashift "ashl") (ashiftrt "ashr")
			      (lshiftrt "lshr")])
(define_code_attr shift_asm  [(ashift "sll") (ashiftrt "sra")
			      (lshiftrt "srl")])

(define_insn "<shift_op>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(SHIFT:SI (match_operand:SI 1 "register_operand"  "r,r")
		  (match_operand:SI 2 "reg_or_u6_operand" "r,n")))]
  ""
  "@
   l.<shift_asm>\t%0, %1, %2
   l.<shift_asm>i\t%0, %1, %2"
  [(set_attr "insn_support" "*,shftimm")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "r,r")
		  (match_operand:SI 2 "reg_or_u6_operand" "r,n")))]
  "TARGET_ROR"
  "@
   l.ror\t%0, %1, %2
   l.rori\t%0, %1, %2"
  [(set_attr "insn_support" "*,shftimm")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (and:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_u16_operand" " r,K")))]
  ""
  "@
  l.and\t%0, %1, %2
  l.andi\t%0, %1, %2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (xor:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  ""
  "@
  l.xor\t%0, %1, %2
  l.xori\t%0, %1, %2")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (ior:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_u16_operand" " r,K")))]
  ""
  "@
  l.or\t%0, %1, %2
  l.ori\t%0, %1, %2")

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "") (const_int -1)))]
  ""
  "")

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_mode_iterator I [QI HI SI])
(define_mode_iterator I12 [QI HI])

(define_mode_attr ldst [(QI "b") (HI "h") (SI "w")])
(define_mode_attr zext_andi [(QI "0xff") (HI "0xffff")])

(define_expand "mov<I:mode>"
  [(set (match_operand:I 0 "nonimmediate_operand" "")
	(match_operand:I 1 "general_operand" ""))]
  ""
{
  or1k_expand_move (<MODE>mode, operands[0], operands[1]);
  DONE;
})

;; 8-bit, 16-bit and 32-bit moves

(define_insn "*mov<I:mode>_internal"
  [(set (match_operand:I 0 "nonimmediate_operand" "=r,r,r,r, m,r")
	(match_operand:I 1 "input_operand"        " r,M,K,I,rO,m"))]
  "register_operand (operands[0], <I:MODE>mode)
   || reg_or_0_operand (operands[1], <I:MODE>mode)"
  "@
   l.or\t%0, %1, %1
   l.movhi\t%0, hi(%1)
   l.ori\t%0, r0, %1
   l.xori\t%0, r0, %1
   l.s<I:ldst>\t%0, %r1
   l.l<I:ldst>z\t%0, %1"
  [(set_attr "type" "alu,alu,alu,alu,st,ld")])

;; Hi/Low moves for constant and symbol loading

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "high_operand" "")))]
  ""
  "l.movhi\t%0, %h1"
  [(set_attr "type" "alu")])

(define_insn "*movsi_lo_sum_iori"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand"  "r")
		   (match_operand:SI 2 "losum_ior_operand" "")))]
  ""
  "l.ori\t%0, %1, %L2"
  [(set_attr "type" "alu")])

(define_insn "*movsi_lo_sum_addi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand"  "r")
		   (match_operand:SI 2 "losum_add_operand" "")))]
  ""
  "l.addi\t%0, %1, %L2"
  [(set_attr "type" "alu")])

;; 64-bit moves
;; ??? The clobber that emit_move_multi_word emits is arguably incorrect.
;; Consider gcc.c-torture/execute/20030222-1.c, where a reg-reg DImode
;; move gets register allocated to a no-op move.  At which point the
;; we actively clobber the input.

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
{
  if (MEM_P (operands[0]) && !const0_operand(operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);
})

(define_insn_and_split "*movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,o,r")
	(match_operand:DI 1 "general_operand"      " r,o,rO,n"))]
  "register_operand (operands[0], DImode)
   || reg_or_0_operand (operands[1], DImode)"
  "#"
  ""
  [(const_int 0)]
{
  rtx l0 = operand_subword (operands[0], 0, 0, DImode);
  rtx l1 = operand_subword (operands[1], 0, 0, DImode);
  rtx h0 = operand_subword (operands[0], 1, 0, DImode);
  rtx h1 = operand_subword (operands[1], 1, 0, DImode);

  if (reload_completed && reg_overlap_mentioned_p (l0, h1))
    {
      gcc_assert (!reg_overlap_mentioned_p (h0, l1));
      emit_move_insn (h0, h1);
      emit_move_insn (l0, l1);
    }
  else
    {
      emit_move_insn (l0, l1);
      emit_move_insn (h0, h1);
    }
  DONE;
})

;; -------------------------------------------------------------------------
;; Sign Extending
;; -------------------------------------------------------------------------

;; Zero extension can always be done with AND and an extending load.

(define_insn "zero_extend<mode>si2"
  [(set (match_operand:SI 0 "register_operand"                     "=r,r")
	(zero_extend:SI (match_operand:I12 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.andi\t%0, %1, <zext_andi>
   l.l<ldst>z\t%0, %1")

;; Sign extension in registers is an optional extension, but the
;; extending load is always available.  If SEXT is not available,
;; force the middle-end to do the expansion to shifts.

(define_insn "extend<mode>si2"
  [(set (match_operand:SI 0 "register_operand"                      "=r,r")
	(sign_extend:SI (match_operand:I12 1 "nonimmediate_operand"  "r,m")))]
  "TARGET_SEXT"
  "@
   l.ext<ldst>s\t%0, %1
   l.l<ldst>s\t%0, %1")

(define_insn "*extend<mode>si2_mem"
  [(set (match_operand:SI 0 "register_operand"                "=r")
	(sign_extend:SI (match_operand:I12 1 "memory_operand"  "m")))]
  ""
  "l.l<ldst>s\t%0, %1")

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

;; OpenRISC supports these integer comparisons:
;;
;;     l.sfeq[i] - equality, r r or r i
;;     l.sfne[i] - not equal, r r or r i
;;     l.sflt{s,u}[i] - less than, signed or unsigned, r r or r i
;;     l.sfle{s,u}[i] - less than or equal, signed or unsigned, r r or r i
;;     l.sfgt{s,u}[i] - greater than, signed or unsigned, r r or r i
;;     l.sfge{s,u}[i] - greater than or equal, signed or unsigned, r r or r i
;;
;;  EQ,NE,LT,LTU,LE,LEU,GT,GTU,GE,GEU
;;  We iterate through all of these
;;

(define_code_iterator intcmpcc [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr insn [(ne "ne") (eq "eq") (lt "lts") (ltu "ltu")
			(gt "gts") (gtu "gtu") (ge "ges") (le "les")
			(geu "geu") (leu "leu") ])

(define_insn "*sf_insn"
  [(set (reg:BI SR_F_REGNUM)
	(intcmpcc:BI (match_operand:SI 0 "reg_or_0_operand"   "rO,rO")
		     (match_operand:SI 1 "reg_or_s16_operand" "r,I")))]
  ""
  "@
   l.sf<insn>\t%r0, %1
   l.sf<insn>i\t%r0, %1"
  [(set_attr "insn_support" "*,sfimm")])

;; -------------------------------------------------------------------------
;; Conditional Store instructions
;; -------------------------------------------------------------------------

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	  (match_operator 1 "comparison_operator"
	    [(match_operand:SI 2 "reg_or_0_operand" "")
	     (match_operand:SI 3 "reg_or_s16_operand" "")])
	  (match_dup 0)
	  (const_int 0)))]
  ""
{
  or1k_expand_compare (operands + 1);
  PUT_MODE (operands[1], SImode);
  emit_insn (gen_rtx_SET (operands[0], operands[1]));
  DONE;
})

;; Being able to "copy" SR_F to a general register is helpful for
;; the atomic insns, wherein the usual usage is to test the success
;; of the compare-and-swap.  Representing the operation in this way,
;; rather than exposing the cmov immediately, allows the optimizers
;; to propagate the use of SR_F directly into a branch.

(define_expand "sne_sr_f"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (reg:BI SR_F_REGNUM) (const_int 0)))]
  "")

(define_insn_and_split "*scc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "equality_comparison_operator"
	  [(reg:BI SR_F_REGNUM) (const_int 0)]))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (const_int 1))
   (set (match_dup 0)
	(if_then_else:SI (match_dup 1)
	  (match_dup 0)
	  (const_int 0)))]
  "")

(define_expand "mov<I:mode>cc"
  [(set (match_operand:I 0 "register_operand" "")
	(if_then_else:I (match_operand 1 "comparison_operator" "")
	  (match_operand:I 2 "reg_or_0_operand" "")
	  (match_operand:I 3 "reg_or_0_operand" "")))]
  ""
{
  rtx xops[3] = { operands[1], XEXP (operands[1], 0), XEXP (operands[1], 1) };
  or1k_expand_compare (xops);
  operands[1] = xops[0];
})

(define_insn_and_split "*cmov<I:mode>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(if_then_else:I
	  (match_operator 3 "equality_comparison_operator"
	    [(reg:BI SR_F_REGNUM) (const_int 0)])
	  (match_operand:I 1 "reg_or_0_operand" "rO")
	  (match_operand:I 2 "reg_or_0_operand" "rO")))]
  ""
{
  return (GET_CODE (operands[3]) == NE
	  ? "l.cmov\t%0, %r1, %r2"
	  : "l.cmov\t%0, %r2, %r1");
}
  "!TARGET_CMOV"
  [(const_int 0)]
{
  rtx x;
  rtx label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  /* Generated a *cbranch pattern.  */
  if (rtx_equal_p (operands[0], operands[2]))
    {
      PUT_CODE (operands[3], (GET_CODE (operands[3]) == NE) ? EQ : NE);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, operands[3], label, pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, x));
      emit_move_insn (operands[0], operands[1]);
    }
  else
    {
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, operands[3], label, pc_rtx);
      emit_move_insn (operands[0], operands[1]);
      emit_jump_insn (gen_rtx_SET (pc_rtx, x));
      emit_move_insn (operands[0], operands[2]);
    }

  emit_label (XEXP (label, 0));
  DONE;
})

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "comparison_operator"
	    [(match_operand:SI 1 "reg_or_0_operand" "")
	     (match_operand:SI 2 "reg_or_s16_operand" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
{
  or1k_expand_compare (operands);
})

(define_insn "*cbranch"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "equality_comparison_operator"
	    [(reg:BI SR_F_REGNUM) (const_int 0)])
	  (label_ref (match_operand 0 "" ""))
	  (pc)))]
  ""
{
  return (GET_CODE (operands[1]) == NE
	  ? "l.bf\t%0%#"
	  : "l.bnf\t%0%#");
}
  [(set_attr "type" "control")])

;; -------------------------------------------------------------------------
;; Jump instructions
;; -------------------------------------------------------------------------

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "l.j\t%0%#"
  [(set_attr "type" "control")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0%#"
  [(set_attr "type" "control")])

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  or1k_expand_prologue ();
  DONE;
})

;; Expand epilogue as RTL
(define_expand "epilogue"
  [(return)]
  ""
{
  or1k_expand_epilogue ();
  emit_jump_insn (gen_simple_return ());
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  or1k_expand_epilogue ();
  /* Placing a USE of LR here, rather than as a REG_USE on the
     sibcall itself, means that LR is not unnecessarily live
     within the function itself, which would force creation of
     a stack frame.  */
  emit_insn (gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM)));
  DONE;
})

(define_expand "simple_return"
  [(parallel [(simple_return) (use (match_dup 0))])]
  ""
{
  operands[0] = gen_rtx_REG (Pmode, LR_REGNUM);
})

(define_insn "*simple_return"
  [(simple_return)
   (use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0%#"
  [(set_attr "type" "control")])

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  or1k_expand_eh_return (operands[0]);
  DONE;
})

;; This is a placeholder, during RA, in order to create the PIC regiter.
;; We do this so that we don't unconditionally mark the LR register as
;; clobbered.  It is replaced during prologue generation with the proper
;; set_got pattern below.  This works because the set_got_tmp insn is the
;; first insn in the stream and that it isn't moved during RA.
(define_insn "set_got_tmp"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_SET_GOT))]
  ""
{
  gcc_unreachable ();
})

;; The insn to initialize the GOT.
(define_insn "set_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)] UNSPEC_SET_GOT))
   (clobber (reg:SI LR_REGNUM))]
  ""
{
  return ("l.jal\t8\;"
	  " l.movhi\t%0, gotpchi(_GLOBAL_OFFSET_TABLE_-4)\;"
	  "l.ori\t%0, %0, gotpclo(_GLOBAL_OFFSET_TABLE_+0)\;"
	  "l.add\t%0, %0, r9");
}
  [(set_attr "length" "16")
   (set_attr "type" "multi")])

;; Block memory operations from being scheduled across frame (de)allocation.
(define_insn "frame_addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (plus:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))
   (clobber (mem:BLK (scratch)))]
  "reload_completed"
  "@
  l.add\t%0, %1, %2
  l.addi\t%0, %1, %2")

;; -------------------------------------------------------------------------
;; Atomic Operations
;; -------------------------------------------------------------------------

;; Note that MULT stands in for the non-existant NAND rtx_code.
(define_code_iterator FETCHOP [plus minus ior xor and mult])

(define_code_attr fetchop_name
  [(plus "add")
   (minus "sub")
   (ior "or")
   (xor "xor")
   (and "and")
   (mult "nand")])

(define_code_attr fetchop_pred
  [(plus "reg_or_s16_operand")
   (minus "register_operand")
   (ior "reg_or_u16_operand")
   (xor "reg_or_s16_operand")
   (and "reg_or_u16_operand")
   (mult "reg_or_u16_operand")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]		;; model
  ""
{
  memmodel model = memmodel_base (INTVAL (operands[0]));
  if (model != MEMMODEL_RELAXED)
    emit_insn (gen_msync ());
  DONE;
})

(define_expand "msync"
  [(set (match_dup 0) (unspec:BLK [(match_dup 0)] UNSPEC_MSYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*msync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MSYNC))]
  ""
  "l.msync")

(define_insn "load_locked_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI
	  [(match_operand:SI 1 "memory_operand" "m")] UNSPECV_LL))]
  ""
  "l.lwa\t%0,%1"
  [(set_attr "type" "ld")])

(define_insn "store_conditional_si"
  [(set (reg:BI SR_F_REGNUM)
	(unspec_volatile:BI [(const_int 0)] UNSPECV_SC))
   (set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rO"))]
  ""
  "l.swa\t%0,%r1"
  [(set_attr "type" "st")])

(define_expand "atomic_compare_and_swapsi"
  [(match_operand:SI 0 "register_operand")   ;; bool output
   (match_operand:SI 1 "register_operand")   ;; val output
   (match_operand:SI 2 "memory_operand")     ;; memory
   (match_operand:SI 3 "reg_or_s16_operand") ;; expected
   (match_operand:SI 4 "reg_or_0_operand")   ;; desired
   (match_operand:SI 5 "const_int_operand")  ;; is_weak
   (match_operand:SI 6 "const_int_operand")  ;; mod_s
   (match_operand:SI 7 "const_int_operand")] ;; mod_f
  ""
{
  or1k_expand_atomic_compare_and_swap (operands);
  DONE;
})

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand")   ;; bool output
   (match_operand:I12 1 "register_operand")  ;; val output
   (match_operand:I12 2 "memory_operand")    ;; memory
   (match_operand:I12 3 "register_operand")  ;; expected
   (match_operand:I12 4 "reg_or_0_operand")  ;; desired
   (match_operand:SI 5 "const_int_operand")  ;; is_weak
   (match_operand:SI 6 "const_int_operand")  ;; mod_s
   (match_operand:SI 7 "const_int_operand")] ;; mod_f
  ""
{
  or1k_expand_atomic_compare_and_swap_qihi (operands);
  DONE;
})

(define_expand "atomic_exchangesi"
  [(match_operand:SI 0 "register_operand")	;; output
   (match_operand:SI 1 "memory_operand")	;; memory
   (match_operand:SI 2 "reg_or_0_operand")	;; input
   (match_operand:SI 3 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_exchange (operands);
  DONE;
})

(define_expand "atomic_exchange<mode>"
  [(match_operand:I12 0 "register_operand")	;; output
   (match_operand:I12 1 "memory_operand")	;; memory
   (match_operand:I12 2 "reg_or_0_operand")	;; input
   (match_operand:SI 3 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_exchange_qihi (operands);
  DONE;
})

(define_expand "atomic_<fetchop_name>si"
  [(match_operand:SI 0 "memory_operand")	;; memory
   (FETCHOP:SI (match_dup 0)
     (match_operand:SI 1 "<fetchop_pred>"))	;; operand
   (match_operand:SI 2 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[0], operands[1], NULL, NULL);
  DONE;
})

(define_expand "atomic_<fetchop_name><mode>"
  [(match_operand:I12 0 "memory_operand")	;; memory
   (FETCHOP:I12 (match_dup 0)
     (match_operand:I12 1 "register_operand"))	;; operand
   (match_operand:SI 2 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[0], operands[1], NULL, NULL);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name>si"
  [(match_operand:SI 0 "register_operand" "")		;; output
   (match_operand:SI 1 "memory_operand" "")		;; memory
   (FETCHOP:SI (match_dup 1)
     (match_operand:SI 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[1], operands[2], operands[0], NULL);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(match_operand:I12 0 "register_operand" "")		;; output
   (match_operand:I12 1 "memory_operand" "")		;; memory
   (FETCHOP:I12 (match_dup 1)
     (match_operand:I12 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[1], operands[2],
			      operands[0], NULL);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetchsi"
  [(match_operand:SI 0 "register_operand" "")		;; output
   (match_operand:SI 1 "memory_operand" "")		;; memory
   (FETCHOP:SI (match_dup 1)
     (match_operand:SI 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[1], operands[2], NULL, operands[0]);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(match_operand:I12 0 "register_operand" "")		;; output
   (match_operand:I12 1 "memory_operand" "")		;; memory
   (FETCHOP:I12 (match_dup 1)
     (match_operand:I12 2 "<fetchop_pred>" ""))	;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[1], operands[2],
			      NULL, operands[0]);
  DONE;
})

;; -------------------------------------------------------------------------
;; Call Instructions
;; -------------------------------------------------------------------------

;; Leave these to last, as the modeless operand for call_value
;; interferes with normal patterns.

(define_expand "call"
  [(call (match_operand 0) (match_operand 1))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1], false);
  DONE;
})

(define_expand "sibcall"
  [(call (match_operand 0) (match_operand 1))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1], true);
  DONE;
})

(define_expand "call_value"
  [(set (match_operand 0) (call (match_operand 1) (match_operand 2)))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2], false);
  DONE;
})

(define_expand "sibcall_value"
  [(set (match_operand 0) (call (match_operand 1) (match_operand 2)))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2], true);
  DONE;
})

(define_insn "*call"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "r,s"))
	 (match_operand 1))
   (clobber (reg:SI LR_REGNUM))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%0%#
   l.jal\t%P0%#"
  [(set_attr "type" "control")])

(define_insn "*sibcall"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "c,s"))
	 (match_operand 1))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%0%#
   l.j\t%P0%#"
  [(set_attr "type" "control")])

(define_insn "*call_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "r,s"))
	      (match_operand 2)))
   (clobber (reg:SI LR_REGNUM))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%1%#
   l.jal\t%P1%#"
  [(set_attr "type" "control")])

(define_insn "*sibcall_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "c,s"))
	      (match_operand 2)))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%1%#
   l.j\t%P1%#"
  [(set_attr "type" "control")])
