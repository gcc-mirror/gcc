;; Machine description of the Adaptiva epiphany cpu for GNU C compiler
;; Copyright (C) 1994-2025 Free Software Foundation, Inc.
;; Contributed by Embecosm on behalf of Adapteva, Inc.

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

;; See file "rtl.def" for documentation on define_insn, match_*, et. al.

(define_constants
  [(GPR_0			 0)
   (GPR_1			 1)
   (GPR_FP			11)
   (GPR_IP			12)
   (GPR_SP			13)
   (GPR_LR			14)
   (GPR_16			16)
   (GPR_18			18)
   (GPR_20			20)
   (ARG_POINTER_REGNUM		64)
   (FRAME_POINTER_REGNUM	65)
   (CC_REGNUM			66)   ;; 66 or 17
   (CCFP_REGNUM			67)   ;; 67 or 18
   (CONFIG_REGNUM		68)
   (STATUS_REGNUM		69)
   (LC_REGNUM			70)
   (LS_REGNUM			71)
   (LE_REGNUM			72)
   (IRET_REGNUM			73)
   (FP_NEAREST_REGNUM		74)
   (FP_TRUNCATE_REGNUM		75)
   (FP_ANYFP_REGNUM		76)
   (UNKNOWN_REGNUM		77) ; used for addsi3_r and friends
   ; We represent the return address as an unspec rather than a reg.
   ; If we used a reg, we could use register elimination, but eliminating
   ; to GPR_LR would make the latter visible to dataflow, thus making it
   ; harder to determine when it must be saved.
   (UNSPEC_RETURN_ADDR		 0)
   (UNSPEC_FP_MODE		 1)

   (UNSPECV_GID			 0)
   (UNSPECV_GIE			 1)])

;; Insn type.  Used to default other attribute values.

(define_attr "type"
  "move,load,store,cmove,unary,compare,shift,mul,uncond_branch,branch,call,fp,fp_int,v2fp,misc,sfunc,fp_sfunc,flow"
  (const_string "misc"))

;; Length (in # bytes)

(define_attr "length" "" (const_int 4))

;; The length here is the length of a single asm.

(define_asm_attributes
  [(set_attr "length" "4")
   (set_attr "type" "misc")])

;; pipeline model; so far we have only one.
(define_attr "pipe_model" "epiphany" (const_string "epiphany"))

(define_attr "rounding" "trunc,nearest"
  (cond [(ne (symbol_ref "TARGET_ROUND_NEAREST") (const_int 0))
	 (const_string "nearest")]
	(const_string "trunc")))

(define_attr "fp_mode" "round_unknown,round_nearest,round_trunc,int,caller,none"
  (cond [(eq_attr "type" "fp,v2fp,fp_sfunc")
	 (symbol_ref "(enum attr_fp_mode) epiphany_normal_fp_rounding")
	 (eq_attr "type" "call")
	 (symbol_ref "(enum attr_fp_mode) epiphany_normal_fp_mode")
	 (eq_attr "type" "fp_int")
	 (const_string "int")]
	(const_string "none")))

(include "epiphany-sched.md")

(include "predicates.md")
(include "constraints.md")

;; modes that are held in a single register, and hence, a word.
(define_mode_iterator WMODE [SI SF HI QI V2HI V4QI])
(define_mode_iterator WMODE2 [SI SF HI QI V2HI V4QI])

;; modes that are held in a two single registers
(define_mode_iterator DWMODE [DI DF V2SI V2SF V4HI V8QI])

;; Double-word mode made up of two single-word mode values.
(define_mode_iterator DWV2MODE [V2SI V2SF])
(define_mode_attr vmode_part [(V2SI "si") (V2SF "sf")])
(define_mode_attr vmode_PART [(V2SI "SI") (V2SF "SF")])
(define_mode_attr vmode_fp_type [(V2SI "fp_int") (V2SF "fp")])
(define_mode_attr vmode_ccmode [(V2SI "CC") (V2SF "CC_FP")])
(define_mode_attr vmode_cc [(V2SI "CC_REGNUM") (V2SF "CCFP_REGNUM")])

;; Move instructions.

(define_expand "mov<mode>"
  [(set (match_operand:WMODE 0 "general_operand" "")
	(match_operand:WMODE 1 "general_operand" ""))]
  ""
{
  if (<MODE>mode == V4QImode || <MODE>mode == V2HImode)
    {
      operands[0] = simplify_gen_subreg (SImode, operands[0], <MODE>mode, 0);
      operands[1] = simplify_gen_subreg (SImode, operands[1], <MODE>mode, 0);
      emit_insn (gen_movsi (operands[0], operands[1]));
      DONE;
    }
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (<MODE>mode, operands[1]);
  if (<MODE>mode == SImode
      && (operands[1] == frame_pointer_rtx || operands[1] == arg_pointer_rtx))
    {
      rtx reg = operands[0];

      if (!REG_P (reg))
	reg = gen_reg_rtx (SImode);
      emit_insn (gen_move_frame (reg, operands[1]));
      operands[1] = reg;
      if (operands[0] == reg)
	DONE;
    }
})

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "move_dest_operand" "=Rcs,   r,  r,r,m")
	(match_operand:QI 1 "move_src_operand"   "Rcs,rU16,Cal,m,r"))]
;; ??? Needed?
  "gpr_operand (operands[0], QImode)
   || gpr_operand (operands[1], QImode)"
  "@
   mov %0,%1
   mov %0,%1
   mov %0,%1
   ldrb %0,%1
   strb %1,%0"
  [(set_attr "type" "move,move,move,load,store")])

(define_insn_and_split "*movhi_insn"
  [(set (match_operand:HI 0 "move_dest_operand" "=r,  r,r,m")
	(match_operand:HI 1 "move_src_operand""rU16,Cal,m,r"))]
  "gpr_operand (operands[0], HImode)
   || gpr_operand (operands[1], HImode)"
  "@
   mov %0,%1
   mov %0,%%low(%1); %1
   ldrh %0,%c1
   strh %1,%c0"
  "reload_completed && CONSTANT_P (operands[1])
   && !satisfies_constraint_U16 (operands[1]) && TARGET_SPLIT_LOHI"
  [(set (match_dup 2) (match_dup 3))]
  "operands[2] = simplify_gen_subreg (SImode, operands[0], HImode, 0);
   operands[3] = simplify_gen_subreg (SImode, operands[1], HImode, 0);"
  [(set_attr "type" "move,move,load,store")])

;; We use a special pattern for a move from the frame pointer to
;; show the flag clobber that is needed when this move is changed
;; to an add by register elimination.
;; ??? A pseudo register might be equivalent to a function invariant,
;; and thus placed by reload into reg_equiv_invariant; if the pseudo
;; does not get a hard register, we then end up with the function
;; invariant in its place, i.e. an unexpected clobber of the flags
;; register.
;;
;; N.B. operand 1 is an operand so that reload will perform elimination.
;;
;; The post-reload pattern recognition and splitting is done in frame_move_1.
(define_insn "move_frame"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (clobber (reg:CC CC_REGNUM))]
  "operands[1] == frame_pointer_rtx || operands[1] == arg_pointer_rtx"
  "#")

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "gpr_operand" "+r")
	(ior:SI (and:SI (match_dup 0) (const_int 65535))
		(high:SI (match_operand:SI 1 "move_src_operand" "i"))))]
  ""
  "movt %0, %%high(%1)"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_insn "movsi_lo_sum"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(lo_sum:SI (const_int 0)
		   (match_operand:SI 1 "move_src_operand" "i")))]
  ""
  "mov %0, %%low(%1)"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_insn_and_split "*movsi_insn"
  [(set (match_operand:SI 0 "move_dest_operand"
	 "=   r,  r,  r,  r,  r,   r,   m,  r,  Rct")
	(match_operand:SI 1 "move_src_operand"
	 "rU16Rra,Cm1,Cl1,Cr1,Cal,mSra,rRra,Rct,r"))]
  "gpr_operand (operands[0], SImode)
   || gpr_operand (operands[1], SImode)
   || satisfies_constraint_Sra (operands[1])"
{
  switch (which_alternative)
    {
    case 0: return "mov %0,%1";
    case 1: return "add %0,%-,(1+%1)";
    case 2: operands[1] = GEN_INT (exact_log2 (-INTVAL (operands[1])));
      return "lsl %0,%-,%1";
    case 3: operands[1] = GEN_INT (32 - exact_log2 (INTVAL (operands[1]) + 1));
      return "lsr %0,%-,%1";
    case 4: return "mov %0,%%low(%1)\;movt %0,%%high(%1) ; %1";
    case 5: return "ldr %0,%C1";
    case 6: return "str %1,%C0";
    case 7: return "movfs %0,%1";
    case 8: return "movts %0,%1";
    default: gcc_unreachable ();
    }
}
  "reload_completed && CONSTANT_P (operands[1])
   && !satisfies_constraint_U16 (operands[1])
   && !satisfies_constraint_Cm1 (operands[1])
   && !satisfies_constraint_Cl1 (operands[1])
   && !satisfies_constraint_Cr1 (operands[1])
   && TARGET_SPLIT_LOHI"
  [(match_dup 2) (match_dup 3)]
  "operands[2] = gen_movsi_lo_sum (operands[0], operands[1]);
   operands[3] = gen_movsi_high (operands[0], operands[1]);"
  [(set_attr "type" "move,misc,misc,misc,move,load,store,flow,flow")
   (set_attr "length" "4,4,4,4,8,4,4,4,4")])

(define_split
  [(set (match_operand:SI 0 "nonimmediate_operand")
	(unspec:SI [(const_int 0)] UNSPEC_RETURN_ADDR))]
  "reload_completed && !MACHINE_FUNCTION (cfun)->lr_clobbered"
  [(set (match_dup 0) (reg:SI GPR_LR))])

(define_split
  [(set (match_operand:SI 0 "gpr_operand")
	(unspec:SI [(const_int 0)] UNSPEC_RETURN_ADDR))]
  "reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  emit_insn (gen_reload_insi_ra (operands[0], operands[1]));
  DONE;
})

(define_expand "reload_insi_ra"
  [(set (match_operand:SI 0 "gpr_operand" "r") (match_operand:SI 1 "" "Sra"))]
  ""
{
  rtx addr
    = (frame_pointer_needed ? hard_frame_pointer_rtx : stack_pointer_rtx);

  if (!MACHINE_FUNCTION (cfun)->lr_slot_known)
    {
      start_sequence ();
      epiphany_expand_prologue ();
      if (!MACHINE_FUNCTION (cfun)->lr_slot_known)
        epiphany_expand_epilogue (0);
      end_sequence ();
      gcc_assert (MACHINE_FUNCTION (cfun)->lr_slot_known);
    }
  addr = plus_constant (Pmode, addr, MACHINE_FUNCTION (cfun)->lr_slot_offset);
  operands[1] = gen_frame_mem (SImode, addr);
})

;; If the frame pointer elimination offset is zero, we'll use this pattern.
;; Note that the splitter can accept any gpr in operands[1]; this is
;; necessary, (e.g. for compile/20021015-1.c -O0,)
;; because when register elimination cannot be done with the constant
;; as an immediate operand of the add instruction, reload will resort to
;; loading the constant into a reload register, using gen_add2_insn to add
;; the stack pointer, and then use the reload register as new source in
;; the move_frame pattern.
(define_insn_and_split "*move_frame_1"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(match_operand:SI 1 "gpr_operand" "r"))
   (clobber (reg:CC CC_REGNUM))]
  "(reload_in_progress || reload_completed)
   && (operands[1] == stack_pointer_rtx
       || operands[1] == hard_frame_pointer_rtx)"
  "#"
  "reload_in_progress || reload_completed"
  [(set (match_dup 0) (match_dup 1))])

(define_expand "mov<mode>"
  [(set (match_operand:DWMODE 0 "general_operand" "")
	(match_operand:DWMODE 1 "general_operand" ""))]
  ""
  "
{
  if (GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_INT
      || GET_MODE_CLASS (<MODE>mode) == MODE_VECTOR_FLOAT)
    {
      if (epiphany_vect_align == 4 && TARGET_SPLIT_VECMOVE_EARLY)
	{
	  rtx o0l, o0h, o1l, o1h;

	  o0l = simplify_gen_subreg (SImode, operands[0], <MODE>mode, 0);
	  o0h = simplify_gen_subreg (SImode, operands[0], <MODE>mode,
				     UNITS_PER_WORD);
	  o1l = simplify_gen_subreg (SImode, operands[1], <MODE>mode, 0);
	  o1h = simplify_gen_subreg (SImode, operands[1], <MODE>mode,
				     UNITS_PER_WORD);
	  if (reg_overlap_mentioned_p (o0l, o1h))
	    {
	      emit_move_insn (o0h, o1h);
	      emit_move_insn (o0l, o1l);
	    }
	  else
	    {
	      emit_move_insn (o0l, o1l);
	      emit_move_insn (o0h, o1h);
	    }
	  DONE;
	}
      /* lower_subreg has a tendency to muck up vectorized code.
	 To protect the wide memory accesses, we must use same-size
	 subregs.  */
      if (epiphany_vect_align != 4 /* == 8 */
	  && !reload_in_progress
	  && (GET_CODE (operands[0]) == MEM || GET_CODE (operands[1]) == MEM)
	  && !misaligned_operand (operands[1], <MODE>mode)
	  && (GET_CODE (operands[0]) != SUBREG
	      || (GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[0])))
		  != GET_MODE_SIZE (<MODE>mode)
		  && GET_CODE (operands[1]) != SUBREG)))
	{
	  operands[0]
	    = simplify_gen_subreg (DImode, operands[0], <MODE>mode, 0);
	  operands[1]
	    = simplify_gen_subreg (DImode, operands[1], <MODE>mode, 0);
	  emit_insn (gen_movdi (operands[0], operands[1]));
	  DONE;
	}
    }
  /* Everything except mem = const or mem = mem can be done easily.  */

  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (<MODE>mode, operands[1]);
}")

(define_insn_and_split "*mov<mode>_insn"
  [(set (match_operand:DWMODE 0 "move_dest_operand"      "=r,   r,r,m")
	(match_operand:DWMODE 1 "move_double_src_operand" "r,CalE,m,r"))]
  "(gpr_operand (operands[0], <MODE>mode)
    || gpr_operand (operands[1], <MODE>mode))"
  "@
   #
   #
   ldrd %0,%X1
   strd %1,%X0"
  "reload_completed
   && (((!MEM_P (operands[0]) || misaligned_operand (operands[0], <MODE>mode))
	&& (!MEM_P (operands[1])
	    || misaligned_operand (operands[1], <MODE>mode)))
       || epiphany_vect_align == 4)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  int word0 = 0, word1 = UNITS_PER_WORD;

  if (post_modify_operand (operands[0], <MODE>mode)
      || post_modify_operand (operands[1], <MODE>mode))
    word0 = UNITS_PER_WORD, word1 = 0;

  operands[2] = simplify_gen_subreg (SImode, operands[0], <MODE>mode, word0);
  operands[3] = simplify_gen_subreg (SImode, operands[1], <MODE>mode, word0);
  operands[4] = simplify_gen_subreg (SImode, operands[0], <MODE>mode, word1);
  operands[5] = simplify_gen_subreg (SImode, operands[1], <MODE>mode, word1);
  if (post_modify_operand (operands[0], <MODE>mode))
    operands[2]
      = change_address (operands[2], VOIDmode,
			plus_constant (Pmode, XEXP (XEXP (operands[0], 0), 0),
				       UNITS_PER_WORD));
  if (post_modify_operand (operands[1], <MODE>mode))
    operands[3]
      = change_address (operands[3], VOIDmode,
			plus_constant (Pmode, XEXP (XEXP (operands[1], 0), 0),
				       UNITS_PER_WORD));
}
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "8,16,4,4")])


(define_insn_and_split "*movsf_insn"
  [(set (match_operand:SF 0 "move_dest_operand" "=r,r,r,m")
	(match_operand:SF 1 "move_src_operand"   "r,E,m,r"))]
  "gpr_operand (operands[0], SFmode)
   || gpr_operand (operands[1], SFmode)"
  "@
   mov %0,%1
   mov %0,%%low(%1)\;movt %0,%%high(%1) ; %1
   ldr %0,%C1
   str %1,%C0"
  "reload_completed && CONSTANT_P (operands[1]) && TARGET_SPLIT_LOHI"
  [(set (match_dup 2) (match_dup 3))]
  "operands[2] = simplify_gen_subreg (SImode, operands[0], SFmode, 0);
   operands[3] = simplify_gen_subreg (SImode, operands[1], SFmode, 0);"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "4,8,4,4")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "add_reg_operand" "")
	(plus:SI (match_operand:SI 1 "add_reg_operand" "")
		 (match_operand:SI 2 "add_operand" "")))]
  ""
  "
{
  if (reload_in_progress || reload_completed)
    emit_insn (gen_addsi3_r (operands[0], operands[1], operands[2]));
  else if (TARGET_FP_IARITH && add_reg_operand (operands[2], SImode))
    emit_insn (gen_iadd (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_addsi3_i (operands[0], operands[1], operands[2]));
  DONE;
}")

; The default case of epiphany_print_operand emits IMMEDIATE_PREFIX
; where appropriate; however, 'n' is processed by output_asm_insn
; which doesn't, so we have to explicitly emit the '# in the
; r/r/CnL output template alternative.
(define_insn "addsi3_i"
  [(set (match_operand:SI 0 "add_reg_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "add_reg_operand" "%r,r")
		 (match_operand:SI 2 "add_operand" "rL,CnL")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "@
   add %0,%1,%2
   sub %0,%1,#%n2"
[(set_attr "type" "misc")])

; We use a clobber of UNKNOWN_REGNUM here so that the peephole optimizers
; can identify the unresolved flags clobber problem, and also to
; avoid unwanted matches.
;
; At -O0 / -O1 we don't peephole all instances away.  We could get better
; debug unwinding through the emitted code if we added a splitter.
(define_insn "addsi3_r"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(plus:SI (match_operand:SI 1 "gpr_operand" "%r")
		 (match_operand:SI 2 "nonmemory_operand" "rCar")))
   (clobber (reg:CC UNKNOWN_REGNUM))]
  "reload_in_progress || reload_completed"
{
  int scratch = (0x17
		 ^ (true_regnum (operands[0]) & 1)
		 ^ (true_regnum (operands[1]) & 2)
		 ^ (true_regnum (operands[2]) & 4));
  asm_fprintf (asm_out_file, "\tstr r%d,[sp,#0]\n", scratch);
  asm_fprintf (asm_out_file, "\tmovfs r%d,status\n", scratch);
  output_asm_insn ("add %0,%1,%2", operands);
  asm_fprintf (asm_out_file, "\tmovts status,r%d\n", scratch);
  asm_fprintf (asm_out_file, "\tldr r%d,[sp,#0]\n", scratch);
  return "";
}
  [(set_attr "length" "20")
   (set_attr "type" "misc")])

;; reload uses gen_addsi2 because it doesn't understand the need for
;; the clobber.
(define_peephole2
  [(set (match_operand:SI 0 "gpr_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))
   (parallel [(set (match_dup 0)
		   (plus:SI (match_dup 0)
			    (match_operand:SI 2 "gpr_operand")))
	      (clobber (reg:CC UNKNOWN_REGNUM))])]
  "satisfies_constraint_L (operands[1])
   || ((operands[2] == stack_pointer_rtx
	|| (operands[2] == hard_frame_pointer_rtx && frame_pointer_needed))
       && !peep2_regno_dead_p (2, CC_REGNUM)
       && satisfies_constraint_Car (operands[1]))"
  [(parallel [(set (match_dup 0)
		   (plus:SI (match_dup 2) (match_dup 1)))
	      (clobber (reg:CC UNKNOWN_REGNUM))])]
  ;; FIXME:
  ;; need this patch: http://gcc.gnu.org/ml/gcc-patches/2011-10/msg02819.html
  ;; "peep2_rescan = true;"
)

(define_peephole2
  [(match_parallel 5 ""
     [(set (match_operand 3 "cc_operand" "") (match_operand 4 "" ""))])
   (parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (match_operand:SI 1 "gpr_operand" "")
			    (match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC UNKNOWN_REGNUM))])]
  "REGNO (operands[3]) == CC_REGNUM
   && (gpr_operand (operands[2], SImode)
       || satisfies_constraint_L (operands[2]))
   && !reg_overlap_mentioned_p (operands[0], operands[5])
   && !reg_set_p (operands[1], operands[5])
   && !reg_set_p (operands[2], operands[5])"
  [(parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (match_operand:SI 1 "gpr_operand" "")
			    (match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC CC_REGNUM))])
   (match_dup 5)]
  "")

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (match_operand:SI 1 "gpr_operand" "")
			    (match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC UNKNOWN_REGNUM))])]
  "peep2_regno_dead_p (1, CC_REGNUM)
   && (gpr_operand (operands[2], SImode)
       || satisfies_constraint_L (operands[2]))"
  [(parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (match_operand:SI 1 "gpr_operand" "")
			    (match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC CC_REGNUM))])]
  "")

(define_peephole2
  [(parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (reg:SI GPR_SP)
			    (match_operand:SI 1 "nonmemory_operand" "")))
	      (clobber (reg:CC UNKNOWN_REGNUM))])]
  "(REG_P (operands[1]) && !reg_overlap_mentioned_p (operands[0], operands[1]))
   || RTX_OK_FOR_OFFSET_P (<MODE>mode, operands[1])"
  [(set (match_dup 0) (reg:SI GPR_SP))
   (set (mem:WMODE (post_modify (match_dup 0)
				(plus:SI (match_dup 0) (match_dup 1))))
	(reg:WMODE GPR_SP))]
  "")



(define_peephole2
  [(parallel [(set (match_operand:SI 0 "gpr_operand" "")
		   (plus:SI (reg:SI GPR_FP)
			    (match_operand:SI 1 "nonmemory_operand" "")))
	      (clobber (reg:CC UNKNOWN_REGNUM))])
   (match_scratch:WMODE 2 "r")]
  "frame_pointer_needed
   && ((REG_P (operands[1])
	&& !reg_overlap_mentioned_p (operands[0], operands[1]))
       || RTX_OK_FOR_OFFSET_P (<MODE>mode, operands[1]))"
  [(set (match_dup 0) (reg:SI GPR_FP))
   (set (match_dup 2)
	(mem:WMODE (post_modify (match_dup 0)
				(plus:SI (match_dup 0) (match_dup 1)))))]
  "")

(define_expand "subsi3"
  [(set (match_operand:SI 0 "gpr_operand" "")
	(plus:SI (match_operand:SI 1 "add_reg_operand" "")
		 (match_operand:SI 2 "arith_operand" "")))]
  ""
  "
{
  gcc_assert (!reload_in_progress && !reload_completed);

  if (TARGET_FP_IARITH)
    emit_insn (gen_isub (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_subsi3_i (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "subsi3_i"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(minus:SI (match_operand:SI 1 "add_reg_operand" "r")
		  (match_operand:SI 2 "arith_operand" "rL")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "sub %0,%1,%2"
  [(set_attr "type" "misc")])

; After mode-switching, floating point operations, fp_sfuncs and calls
; must exhibit the use of the control register, lest the setting of the
; control register could be deleted or moved.  OTOH a use of a hard register
; greatly counfounds optimizers like the rtl loop optimizers or combine.
; Therefore, we put an extra pass immediately after the mode switching pass
; that inserts the USEs of the control registers, and sets a flag in struct
; machine_function that float_operation can henceforth only match with that
; USE.

;; Addition
(define_expand "addsf3"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (plus:SF (match_operand:SF 1 "gpr_operand" "")
		    (match_operand:SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*addsf3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (plus:SF (match_operand:SF 1 "gpr_operand" "%r")
		    (match_operand:SF 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fadd %0,%1,%2"
  [(set_attr "type" "fp")])

;; Subtraction
(define_expand "subsf3"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (minus:SF (match_operand:SF 1 "gpr_operand" "")
		     (match_operand:SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*subsf3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (minus:SF (match_operand:SF 1 "gpr_operand" "r")
		     (match_operand:SF 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fsub %0,%1,%2"
  [(set_attr "type" "fp")])

(define_expand "subsf3_f"
  [(parallel
     [(set (reg:CC_FP CCFP_REGNUM)
	   (compare:CC_FP (match_operand:SF 1 "gpr_operand" "r")
			  (match_operand:SF 2 "gpr_operand" "r")))
      (set (match_operand:SF 0 "gpr_operand" "=r")
	   (minus:SF (match_dup 1) (match_dup 2)))])]
  "!TARGET_SOFT_CMPSF")

(define_insn "*subsf3_f_i"
  [(match_parallel 3 "float_operation"
     [(set (reg:CC_FP CCFP_REGNUM)
	   (compare:CC_FP (match_operand:SF 1 "gpr_operand" "r")
			  (match_operand:SF 2 "gpr_operand" "r")))
      (set (match_operand:SF 0 "gpr_operand" "=r")
	   (minus:SF (match_dup 1) (match_dup 2)))])]
  "!TARGET_SOFT_CMPSF"
  "fsub %0,%1,%2"
  [(set_attr "type" "fp")])

; There is an fabs instruction, but it has longer latency.
(define_expand "abssf2"
  [(set (match_operand:SF 0 "gpr_operand" "")
	(abs:SF (match_operand:SF 1 "gpr_operand" "")))]
  ""
  "
{
  rtx op1 = copy_to_mode_reg (SImode, simplify_gen_subreg (SImode, operands[1],
							   SFmode, 0));
  rtx op0 = simplify_gen_subreg (SImode, operands[0], SFmode, 0);

  emit_insn (gen_ashlsi3 (op1, op1, const1_rtx));
  emit_insn (gen_lshrsi3 (op0, op1, const1_rtx));
  DONE;
}")

;; Multiplication
(define_expand "mulsf3"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (mult:SF (match_operand:SF 1 "gpr_operand" "")
		    (match_operand:SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*mulsf3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (mult:SF (match_operand:SF 1 "gpr_operand" "%r")
		    (match_operand:SF 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fmul %0,%1,%2"
  [(set_attr "type" "fp")])

;; Division
(define_expand "divsf3"
  [(set (match_operand:SF 0 "gpr_operand" "")
	(div:SF (match_operand:SF 1 "gpr_operand" "")
		(match_operand:SF 2 "gpr_operand" "")))]
  "flag_reciprocal_math"
{
  rtx one = CONST1_RTX (SFmode);
  rtx dst = operands[0];

  if (rtx_equal_p (dst, operands[1]))
    {
      emit_move_insn (dst, one);
      DONE;
    }
  else if (!register_operand (dst, SFmode) && can_create_pseudo_p ())
    dst = gen_reg_rtx (SFmode);
  emit_insn (gen_recipsf2 (dst, one, operands[2],
			   sfunc_symbol (\"__fast_recipsf2\")));
  emit_insn (gen_mulsf3 (operands[0], operands[1], dst));
  DONE;
})

;; Before reload, keep the hard reg usage to clobbers so that the loop
;; optimizers can more easily move this insn.
;; It would be nicer to use a constraint for a GPR_0 - only register class,
;; but sched1 can still cause trouble then, and there is no guarantee of
;; better register allocations.
;; Neither is there when using the opposite strategy - putting explicit
;; hard register references into pre-reload rtl.
(define_expand "recipsf2"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (div:SF (match_operand:SF 1 "const_float_1_operand" "")
		   (match_operand:SF 2 "move_src_operand" "")))
      (use (match_operand:SI 3 "move_src_operand" ""))
      (clobber (reg:SF 0))
      (clobber (reg:SI 1))
      (clobber (reg:SF GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn_and_split "*recipsf2_1"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r,r")
	   (div:SF (match_operand:SF 1 "const_float_1_operand" "")
		   (match_operand:SF 2 "move_src_operand" "rU16m,rU16mCal")))
      (use (match_operand:SI 3 "move_src_operand" "rU16m,rU16mCal"))
      (clobber (reg:SF 0))
      (clobber (reg:SI 1))
      (clobber (reg:SF GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "flag_reciprocal_math"
  "#"
  "&& reload_completed"
  [(set (reg:SI 1) (match_dup 3))
   (set (reg:SF 0) (match_dup 2))
   (parallel
     [(set (reg:SF 0)
	   (div:SF (match_dup 1)
		   (reg:SF 0)))
      (use (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 5)
      (match_dup 6)])
   (set (match_dup 0) (reg:SF 0))]
  "operands[5] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
   operands[6] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);"
  [(set_attr "type" "fp_sfunc")
   (set_attr "length" "16,24")])

(define_insn "*recipsf2_2"
  [(match_parallel 1 "float_operation"
     [(set (reg:SF 0)
	   (div:SF (match_operand:SF 0 "const_float_1_operand" "")
		   (reg:SF 0)))
      (use (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "flag_reciprocal_math"
  "jalr r1"
  [(set_attr "type" "fp_sfunc")])


;; Fused multiply-add
(define_expand "fmasf4"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (fma:SF (match_operand:SF 1 "gpr_operand" "")
		   (match_operand:SF 2 "gpr_operand" "")
		   (match_operand:SF 3 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "")

; The multiply operands are commutative, but since they have the
; same constraints, there is no point in telling reload about this.
(define_insn "*fmadd"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (fma:SF (match_operand:SF 1 "gpr_operand" "r")
		   (match_operand:SF 2 "gpr_operand" "r")
		   (match_operand:SF 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fmadd %0,%1,%2"
  [(set_attr "type" "fp")])

; Once vetorization consistently works for this port, should check
; if the fmadd / fmsub patterns still serve a purpose.  With the
; introduction of fma / fnma handling by the SSA optimizers,
; at least scalars should be handled by these optimizers, would
; have to see how well they do on vectors from auto-vectorization.
;
; combiner pattern, also used by vector combiner pattern
(define_expand "maddsf"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (plus:SF (mult:SF (match_operand:SF 1 "gpr_operand" "r")
			     (match_operand:SF 2 "gpr_operand" "r"))
		    (match_operand:SF 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "TARGET_FUSED_MADD")

(define_insn "*maddsf_combine"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (plus:SF (mult:SF (match_operand:SF 1 "gpr_operand" "r")
			     (match_operand:SF 2 "gpr_operand" "r"))
		    (match_operand:SF 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "TARGET_FUSED_MADD"
  "fmadd %0,%1,%2"
  [(set_attr "type" "fp")])

;; Fused multiply-sub
(define_expand "fnmasf4"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (fma:SF (neg:SF (match_operand:SF 1 "gpr_operand" ""))
		   (match_operand:SF 2 "gpr_operand" "")
		   (match_operand:SF 3 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "")

(define_insn "*fmsub"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (fma:SF (neg:SF (match_operand:SF 1 "gpr_operand" "r"))
		   (match_operand:SF 2 "gpr_operand" "r")
		   (match_operand:SF 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fmsub %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "*fmsub_combine"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (minus:SF  (match_operand:SF 3 "gpr_operand" "0")
		      (mult:SF (match_operand:SF 1 "gpr_operand" "r")
			       (match_operand:SF 2 "gpr_operand" "r"))))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "TARGET_FUSED_MADD"
  "fmsub %0,%1,%2"
  [(set_attr "type" "fp")])

;; float / integer conversions

(define_expand "floatsisf2"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (float:SF (match_operand:SI 1 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*floatsisf2_i"
  [(match_parallel 2 "float_operation"
     [(set (match_operand:SF 0 "gpr_operand" "=r")
	   (float:SF (match_operand:SI 1 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "float %0, %1"
  [(set_attr "type" "fp")])

(define_expand "floatsisf2_cmp"
  [(parallel
     [(set (reg:CC_FP CCFP_REGNUM)
	   (compare:CC_FP (float:SF (match_operand:SF 1 "gpr_operand" "r"))
			  (match_dup 2)))
      (set (match_operand:SF 0 "gpr_operand" "=r")
	   (float:SF (match_dup 1)))])]
  ""
  "operands[2] = CONST0_RTX (SFmode);")

(define_insn "*floatsisf2_cmp_i"
  [(match_parallel 3 "float_operation"
     [(set (reg:CC_FP CCFP_REGNUM)
	   (compare:CC_FP (float:SF (match_operand:SF 1 "gpr_operand" "r"))
			  (match_operand:SF 2 "const0_operand" "")))
      (set (match_operand:SF 0 "gpr_operand" "=r")
	   (float:SF (match_dup 1)))])]
  ""
  "float %0, %1"
  [(set_attr "type" "fp")])

(define_expand "floatunssisf2"
  [(set (match_operand:SF 0 "gpr_operand" "")
	(float:SF (match_operand:SI 1 "gpr_operand" "")))]
  "epiphany_normal_fp_rounding == /*FP_MODE_ROUND_TRUNC*/ 2"
{
  rtx cst = force_reg (SImode, gen_int_mode (0xb0800000, SImode));
  rtx tmp = gen_reg_rtx (SImode);
  rtx cmp = gen_rtx_GTU (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM), const0_rtx);

  if (reg_overlap_mentioned_p (operands[0], operands[1]))
    operands[1] = copy_to_mode_reg (SImode, operands[1]);
  emit_insn (gen_floatsisf2 (operands[0], operands[1]));
  emit_insn (gen_ashrsi3 (tmp, operands[1], GEN_INT (8)));
  emit_insn (gen_sub_f (tmp, tmp, cst));
  emit_insn (gen_movsfcc (operands[0], cmp,
			  simplify_gen_subreg (SFmode, tmp, SImode, 0),
			  operands[0]));
  DONE;
})

(define_expand "fix_truncsfsi2"
  [(parallel
     [(set (match_operand:SI 0 "gpr_operand" "")
	   (fix:SI (match_operand:SF 1 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*fix_truncsfsi2_i"
  [(match_parallel 2 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (fix:SI (match_operand:SF 1 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "fix %0, %1"
  [(set_attr "type" "fp")
   (set (attr "fp_mode")
	(cond [(match_test "TARGET_MAY_ROUND_FOR_TRUNC")
	       (const_string "round_unknown")]
	      (const_string "round_trunc")))])

(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "gpr_operand" "")
	(unsigned_fix:SI (match_operand:SF 1 "gpr_operand" "")))]
  ""
{
  if (reg_overlap_mentioned_p (operands[0], operands[1]))
    operands[1] = copy_to_mode_reg (SImode, operands[1]);
  if (TARGET_SOFT_CMPSF || optimize_function_for_speed_p (cfun))
    {
      rtx op1si;
      /* By toggling what it to be bit31 before the shift, we get a chance to
	 use a short movt insn.  */
      rtx bit31 = force_reg (SImode, GEN_INT (0x800000));
      rtx tmp = gen_reg_rtx (SImode);
      rtx limit = force_reg (SImode, gen_int_mode (0x4f000000, SImode));
      rtx cmp
	= gen_rtx_GE (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM), const0_rtx);

      op1si = simplify_gen_subreg (SImode, operands[1], SFmode, 0);
      emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
      emit_insn (gen_subsi3_i (tmp, op1si, bit31));
      emit_insn (gen_ashlsi3 (tmp, tmp, GEN_INT (8)));
      emit_insn (gen_cmpsi_cc_insn (op1si, limit));
      emit_insn (gen_movsicc (operands[0], cmp, tmp, operands[0]));
    }
  else
    {
      REAL_VALUE_TYPE offset;
      rtx limit;
      rtx tmp = gen_reg_rtx (SFmode);
      rtx_code_label *label = gen_label_rtx ();
      rtx bit31;
      rtx cc1 = gen_rtx_REG (CC_FPmode, CCFP_REGNUM);
      rtx cmp = gen_rtx_LT (VOIDmode, cc1, CONST0_RTX (SFmode));

      real_2expN (&offset, 31, SFmode);
      limit = const_double_from_real_value (offset, SFmode);
      limit = force_reg (SFmode, limit);
      emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
      emit_insn (gen_subsf3_f (tmp, operands[1], limit));
      emit_jump_insn (gen_branch_insn (label, cmp, cc1));
      bit31 = force_reg (SImode, gen_int_mode (0x80000000, SImode));
      emit_insn (gen_fix_truncsfsi2 (operands[0], tmp));
      emit_insn (gen_xorsi3 (operands[0], operands[0], bit31));
      emit_label (label);
    }
  DONE;
})

(define_expand "iadd"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (plus:SI (match_operand:SF 1 "gpr_operand" "")
		    (match_operand:SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*iadd_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (plus:SI (match_operand:SI 1 "gpr_operand" "%r")
		    (match_operand:SI 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "iadd %0, %1, %2"
  [(set_attr "type" "fp_int")])

(define_expand "isub"
  [(parallel
     [(set (match_operand:SF 0 "gpr_operand" "")
	   (minus:SI (match_operand:SF 1 "gpr_operand" "")
		     (match_operand:SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*isub_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (minus:SI (match_operand:SI 1 "gpr_operand" "r")
		     (match_operand:SI 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "isub %0, %1, %2"
  [(set_attr "type" "fp_int")])

; Try to figure out if we over-committed the FPU, and if so, move
; some insns back over to the integer pipe.

; The peephole optimizer 'consumes' the insns that are explicitly
; mentioned.  We do not want the preceding insn reconsidered, but
; we do want that for the following one, so that if we have a run
; of five fpu users, two of them get changed.  Therefore, we
; use next_active_insn to look at the 'following' insn.  That should
; exist, because peephole2 runs after reload, and there has to be
; a return after an fp_int insn.
; ??? However, we cannot even ordinarily match the preceding insn;
; there is some bug in the generators such that then it leaves out
; the check for PARALLEL before the length check for the then-second
; main insn.  Observed when compiling compatibility-atomic-c++0x.cc
; from libstdc++-v3.
(define_peephole2
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "")
	   (match_operator:SI 4 "addsub_operator"
	     [(match_operand:SI 1 "gpr_operand" "")
	      (match_operand:SI 2 "gpr_operand" "")]))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "get_attr_sched_use_fpu (prev_active_insn (peep2_next_insn (0)))
   && peep2_regno_dead_p (1, CC_REGNUM)
   && get_attr_sched_use_fpu (next_active_insn (peep2_next_insn (0)))"
  [(parallel [(set (match_dup 0) (match_dup 4))
	      (clobber (reg:CC CC_REGNUM))])]
)

(define_peephole2
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "")
	   (mult:SI
	      (match_operand:SI 1 "gpr_operand" "")
	      (match_operand:SI 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "prev_active_insn (peep2_next_insn (0))
   && get_attr_sched_use_fpu (prev_active_insn (peep2_next_insn (0)))
   && peep2_regno_dead_p (1, CC_REGNUM)
   && get_attr_sched_use_fpu (next_active_insn (peep2_next_insn (0)))
   && find_reg_note (insn, REG_EQUAL, NULL_RTX) != NULL_RTX
   && GET_CODE (XEXP (find_reg_note (insn, REG_EQUAL, NULL_RTX), 0)) == MULT
   && CONST_INT_P (XEXP (XEXP (find_reg_note (insn, REG_EQUAL, NULL_RTX), 0),
			 1))"
  [(parallel [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 4)))
	      (clobber (reg:CC CC_REGNUM))])]
{
  operands[4]
    = XEXP (XEXP (find_reg_note (curr_insn, REG_EQUAL, NULL_RTX), 0), 1);
})

(define_expand "mulsi3"
  [(parallel
     [(set (match_operand:SI 0 "gpr_operand" "")
	   (mult:SI (match_operand:SI 1 "gpr_operand" "")
		    (match_operand:SI 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn "*imul"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (mult:SI (match_operand:SI 1 "gpr_operand" "%r")
		    (match_operand:SI 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "imul %0, %1, %2"
  [(set_attr "type" "fp_int")])

; combiner pattern, also used by vector combiner pattern
(define_expand "maddsi"
  [(parallel
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (plus:SI (mult:SI (match_operand:SI 1 "gpr_operand" "r")
			     (match_operand:SI 2 "gpr_operand" "r"))
		    (match_operand:SI 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "")

(define_insn "*maddsi_combine"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (plus:SI (mult:SI (match_operand:SI 1 "gpr_operand" "r")
			     (match_operand:SI 2 "gpr_operand" "r"))
		    (match_operand:SI 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "imadd %0, %1, %2"
  [(set_attr "type" "fp_int")])

(define_insn "*imsub"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "gpr_operand" "=r")
	   (minus:SI (match_operand:SI 3 "gpr_operand" "0")
		     (mult:SI (match_operand:SI 1 "gpr_operand" "r")
			      (match_operand:SI 2 "gpr_operand" "r"))))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "imsub %0, %1, %2"
  [(set_attr "type" "fp_int")])

(define_expand "divsi3"
  [(parallel
     [(set (match_operand:SI 0 "move_dest_operand" "")
	   (div:SI (match_operand:SI 1 "move_src_operand" "")
		   (match_operand:SI 2 "move_src_operand" "")))
      (use (match_dup 3))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "operands[3] = sfunc_symbol (\"__divsi3\");")

;; Before reload, keep the hard reg usage to clobbers so that the loop
;; optimizers can more easily move this insn.
(define_insn_and_split "*divsi3_1"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "move_dest_operand" "=r,r")
	   (div:SI (match_operand:SI 1 "move_src_operand" "rU16m,rU16mCal")
		   (match_operand:SI 2 "move_src_operand" "rU16m,rU16mCal")))
      (use (match_operand:SI 3 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:SI 0) (match_dup 1))
   (set (reg:SI 1) (match_dup 2))
   (parallel
     [(set (reg:SI 0) (div:SI (reg:SI 0) (reg:SI 1)))
      (use (match_dup 3))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 5)
      (match_dup 6)])
   (set (match_dup 0) (reg:SI 0))]
  "operands[5] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
   operands[6] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);"
  [(set_attr "type" "fp_sfunc")
   (set_attr "length" "16,24")])

(define_insn "*divsi3_2"
  [(match_parallel 1 "float_operation"
     [(set (reg:SI 0) (div:SI (reg:SI 0) (reg:SI 1)))
      (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_20))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "%f0"
  [(set_attr "type" "fp_sfunc")])

(define_expand "udivsi3"
  [(parallel
     [(set (match_operand:SI 0 "move_dest_operand" "")
	   (udiv:SI (match_operand:SI 1 "move_src_operand" "")
		    (match_operand:SI 2 "move_src_operand" "")))
      (use (match_dup 3))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "operands[3] = sfunc_symbol (\"__udivsi3\");")

;; Before reload, keep the hard reg usage to clobbers so that the loop
;; optimizers can more easily move this insn.
(define_insn_and_split "*udivsi3_1"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "move_dest_operand" "=r,r")
	   (udiv:SI (match_operand:SI 1 "move_src_operand" "rU16m,rU16mCal")
		    (match_operand:SI 2 "move_src_operand" "rU16m,rU16mCal")))
      (use (match_operand:SI 3 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:SI 0) (match_dup 1))
   (set (reg:SI 1) (match_dup 2))
   (parallel
     [(set (reg:SI 0) (udiv:SI (reg:SI 0) (reg:SI 1)))
      (use (match_dup 3))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 5)
      (match_dup 6)])
   (set (match_dup 0) (reg:SI 0))]
  "operands[5] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
   operands[6] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);"
  [(set_attr "type" "fp_sfunc")
   (set_attr "length" "16,24")])

(define_insn "*udivsi3_2"
  [(match_parallel 1 "float_operation"
     [(set (reg:SI 0) (udiv:SI (reg:SI 0) (reg:SI 1)))
      (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 1))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "%f0"
  [(set_attr "type" "fp_sfunc")])

(define_expand "modsi3"
  [(parallel
     [(set (match_operand:SI 0 "move_dest_operand" "")
	   (mod:SI (match_operand:SI 1 "move_src_operand" "")
		   (match_operand:SI 2 "move_src_operand" "")))
      (use (match_dup 3))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "operands[3] = sfunc_symbol (\"__modsi3\");")

;; Before reload, keep the hard reg usage to clobbers so that the loop
;; optimizers can more easily move this insn.
(define_insn_and_split "*modsi3_1"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "move_dest_operand" "=r,r")
	   (mod:SI (match_operand:SI 1 "move_src_operand" "rU16m,rU16mCal")
		   (match_operand:SI 2 "move_src_operand" "rU16m,rU16mCal")))
      (use (match_operand:SI 3 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:SI 0) (match_dup 1))
   (set (reg:SI 1) (match_dup 2))
   (parallel
     [(set (reg:SI 0) (mod:SI (reg:SI 0) (reg:SI 1)))
      (use (match_dup 3))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 5)
      (match_dup 6)])
   (set (match_dup 0) (reg:SI 0))]
  "operands[5] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
   operands[6] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);"
  [(set_attr "type" "fp_sfunc")
   (set_attr "length" "16,24")])

(define_insn "*modsi3_2"
  [(match_parallel 1 "float_operation"
     [(set (reg:SI 0) (mod:SI (reg:SI 0) (reg:SI 1)))
      (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:DI GPR_18))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "%f0"
  [(set_attr "type" "fp_sfunc")])

(define_expand "umodsi3"
  [(parallel
     [(set (match_operand:SI 0 "move_dest_operand" "")
	   (umod:SI (match_operand:SI 1 "move_src_operand" "")
		    (match_operand:SI 2 "move_src_operand" "")))
      (use (match_dup 3))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "operands[3] = sfunc_symbol (\"__umodsi3\");")

;; Before reload, keep the hard reg usage to clobbers so that the loop
;; optimizers can more easily move this insn.
(define_insn_and_split "*umodsi3_1"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:SI 0 "move_dest_operand" "=r,r")
	   (umod:SI (match_operand:SI 1 "move_src_operand" "rU16m,rU16mCal")
		    (match_operand:SI 2 "move_src_operand" "rU16m,rU16mCal")))
      (use (match_operand:SI 3 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:SI 0) (match_dup 1))
   (set (reg:SI 1) (match_dup 2))
   (parallel
     [(set (reg:SI 0) (umod:SI (reg:SI 0) (reg:SI 1)))
      (use (match_dup 3))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 5)
      (match_dup 6)])
   (set (match_dup 0) (reg:SI 0))]
  "operands[5] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
   operands[6] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);"
  [(set_attr "type" "fp_sfunc")
   (set_attr "length" "16,24")])

(define_insn "*umodsi3_2"
  [(match_parallel 1 "float_operation"
     [(set (reg:SI 0) (umod:SI (reg:SI 0) (reg:SI 1)))
      (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
      (clobber (reg:SI 2))
      (clobber (reg:SI GPR_IP))
      (clobber (reg:DI GPR_16))
      (clobber (reg:SI GPR_LR))
      (clobber (reg:CC CC_REGNUM))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "%f0"
  [(set_attr "type" "fp_sfunc")])

; Disable interrupts.
; Any earlier values read from CONFIG_REGNUM are out of date, since interrupts
; might have changed settings that we do not want to mess with.
(define_insn "gid"
  [(set (reg:SI CONFIG_REGNUM)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_GID))]
  ""
  "gid"
  [(set_attr "type" "flow")])

; Enable interrupts.
; Present CONTROL_REGNUM here to make sure it is live before the
; actual uses in floating point insns / calls are inserted.
; FWIW, interrupts also do mind what is in the control register.
(define_insn "gie"
  [(unspec_volatile [(reg:SI CONFIG_REGNUM)] UNSPECV_GIE)]
  ""
  "gie"
  [(set_attr "type" "flow")])

; Floating point instructions require manipulating the control register.
; Manipulating the control register needs arithmetic.
; Arithmetic clobbers flags.
; The flags are in the status register, which also contains the alternate
; flag and the interrupt enable/disable bits.
; saving/restoring status and mixing up the order with gid/gie could
; lead to disaster.
; Usually, saving/restoring the status is unnecessary, and will be optimized
; away.  But when we really need it, we must make sure that we don't change
; anything but the flags.
; N.B.: We could make the constant easier to load by inverting it, but
; then we'd need to clobber the saved value - and that would make optimizing
; away unneeded saves/restores harder / less likely.
(define_expand "movcc"
  [(parallel [(set (match_operand:CC 0 "cc_move_operand"  "")
		   (match_operand:CC 1 "cc_move_operand" ""))
	      (use (match_dup 2))
	      (clobber (match_scratch:SI 3                 "=X, &r"))])]
  ""
  "operands[2] = gen_int_mode (~0x10f0, SImode);")

(define_insn "*movcc_i"
  [(set (match_operand:CC 0 "cc_move_operand"  "=r,Rcc")
	(match_operand:CC 1 "cc_move_operand" "Rcc,  r"))
   (use (match_operand:SI 2 "nonmemory_operand"  "X,  r"))
   (clobber (match_scratch:SI 3                 "=X, &r"))]
  ""
  "@
   movfs %0,status
   movfs %3,status\;eor %3,%3,%1\;and %3,%3,%2\;eor %3,%3,%1\;movts status,%3"
  [(set_attr "type" "flow")
   (set_attr "length" "20,4")])

(define_insn_and_split "save_config"
  [(set (match_operand:SI 0 "gpr_operand" "=r") (reg:SI CONFIG_REGNUM))
   (use (reg:SI FP_NEAREST_REGNUM))
   (use (reg:SI FP_TRUNCATE_REGNUM))
   (use (reg:SI FP_ANYFP_REGNUM))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (reg:SI CONFIG_REGNUM))])

(define_insn_and_split "set_fp_mode"
  [(set (reg:SI FP_NEAREST_REGNUM)
	(match_operand:SI 0 "set_fp_mode_operand" "rCfm"))
   (set (reg:SI FP_TRUNCATE_REGNUM) (match_dup 0))
   (set (reg:SI FP_ANYFP_REGNUM)
	(match_operand:SI 1 "set_fp_mode_operand" "rCfm"))
   (use (match_operand:SI 2 "gpr_operand" "r"))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "#"
  "reload_completed || !rtx_equal_p (operands[0], operands[1])"
  [(const_int 0)]
{
  if (!reload_completed)
    emit_note (NOTE_INSN_DELETED);
  else
    epiphany_expand_set_fp_mode (operands);
  DONE;
})


;; Boolean instructions.
;;
;; We don't define the DImode versions as expand_binop does a good enough job.

(define_insn "andsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(and:SI (match_operand:SI 1 "gpr_operand" "r")
		(match_operand:SI 2 "gpr_operand" "r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "and %0,%1,%2")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(ior:SI (match_operand:SI 1 "gpr_operand" "r")
		(match_operand:SI 2 "gpr_operand" "r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "orr %0,%1,%2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(xor:SI (match_operand:SI 1 "gpr_operand" "r")
		(match_operand:SI 2 "gpr_operand" "r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "eor %0,%1,%2")

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "gpr_operand" "")
	(xor:SI (match_operand:SI 1 "gpr_operand" "")
		(match_dup 2)))]
  ""
{
  if (epiphany_m1reg >= 0)
    emit_insn (gen_one_cmplsi2_i (operands[0], operands[1]));
  else
    emit_insn (gen_xorsi3 (operands[0], operands[1],
			   force_reg (SImode, GEN_INT (-1))));
  DONE;
})

; Note that folding this pattern into the xorsi3 pattern would make combine
; less effective.
(define_insn "one_cmplsi2_i"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
        (not:SI (match_operand:SI 1 "gpr_operand" "r")))
   (clobber (reg:CC CC_REGNUM))]
  "epiphany_m1reg >= 0"
  "eor %0,%1,%-")

;; Shift instructions.
;; In principle we could support arbitrary symbolic values as shift constant
;; (truncating the value appropriately), but that would require a suitable
;; relocation and assembler & linker support.
(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "gpr_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "r,K")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "asr %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "shift")])

(define_insn "ashrsi3_tst"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (ashiftrt:SI (match_operand:SI 1 "gpr_operand" "r,r")
		       (match_operand:SI 2 "arith_operand" "r,K"))
	(const_int 0)))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
	(ashiftrt:SI (match_dup 1) (match_dup 2)))]
  ""
  "asr %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "shift")])

;; Logical Shift Right
(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "gpr_operand" "r,r")
		     (match_operand:SI 2 "arith_operand" "r,K")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "lsr %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "shift")])

(define_insn "lshrsi3_tst"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (lshiftrt:SI (match_operand:SI 1 "gpr_operand" "r,r")
		       (match_operand:SI 2 "arith_operand" "r,K"))
	(const_int 0)))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
	(lshiftrt:SI (match_dup 1) (match_dup 2)))]
  ""
  "lsr %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "shift")])

;; Logical/Arithmetic Shift Left
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "gpr_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "gpr_operand" "r,r")
		   (match_operand:SI 2 "arith_operand" "r,K")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "lsl %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "shift")])

(define_insn "*ashlsi_btst"
  [(set (reg:CC_N_NE CC_REGNUM)
	(compare:CC_N_NE
	  (zero_extract:SI (match_operand:SI 1 "gpr_operand" "r")
			   (const_int 1)
			   (match_operand 2 "const_int_operand" "K"))
	  (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
{
  rtx xop[3];

  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = GEN_INT (31-INTVAL (operands[2]));
  output_asm_insn ("lsl %0,%1,%2", xop);
  return "";
})

;; zero extensions
(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "@
   #
   ldrb %0,%1"
  "reload_completed
   ? true_regnum (operands[1]) >= 0
   : REG_P (operands[1]) && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER"
  [(parallel [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 24)))
	      (clobber (reg:CC CC_REGNUM))])
   (parallel [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 24)))
	      (clobber (reg:CC CC_REGNUM))])]
  "operands[2] = simplify_gen_subreg (SImode, operands[1], QImode, 0);")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0,m")))]
  ""
  "@
   movt %0, 0
   ldrh %0,%c1")


;; Compare instructions.

(define_insn "cmpsi_cc_insn"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "add_reg_operand" "r,r")
		    (match_operand:SI 1 "arith_operand" "r,L")))
   (clobber (match_scratch:SI 2 "=r,r"))]
  ""
  "sub %2,%0,%1"
  [(set_attr "type" "compare")])

(define_insn "sub_f"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 1 "gpr_operand"  "r,r")
                    (match_operand:SI 2 "arith_operand" "r,L")))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
        (minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "sub %0,%1,%2"
  [(set_attr "type" "compare")])

(define_insn "*sub_f_add_imm"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 1 "gpr_operand"  "r")
                    (match_operand:SI 2 "arith_int_operand" "L")))
   (set (match_operand:SI 0 "gpr_operand" "=r")
        (plus:SI (match_dup 1) (match_operand:SI 3 "const_int_operand" "CnL")))]
  "INTVAL (operands[2]) == -INTVAL (operands[3])"
  "sub %0,%1,%2"
  [(set_attr "type" "compare")])

(define_expand "abssi2"
  [(set (match_dup 2) (const_int 0))
   (parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 2)
			       (match_operand:SI 1 "nonmemory_operand" "")))
	      (set (match_dup 3)
		   (minus:SI (match_dup 2) (match_dup 1)))])
   (set (match_operand:SI 0 "gpr_operand" "=r")
	(if_then_else:SI (gt:SI (reg:CC CC_REGNUM) (const_int 0))
			 (match_dup 3)
			 (match_dup 1)))]
  "TARGET_CMOVE"
  "operands[2] = gen_reg_rtx (SImode); operands[3] = gen_reg_rtx (SImode);")

(define_insn "*add_c"
  [(set (reg:CC_C_LTU CC_REGNUM)
        (compare:CC_C_LTU
	  (plus:SI (match_operand:SI 1 "gpr_operand" "%r,r")
		   (match_operand:SI 2 "arith_operand" "r,L"))
	  (match_dup 1)))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
        (plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "add %0,%1,%2"
  [(set_attr "type" "compare")])

(define_insn "*add_c_rev"
  [(set (reg:CC_C_LTU CC_REGNUM)
        (compare:CC_C_LTU
	  (plus:SI (match_operand:SI 1 "gpr_operand" "%r,r")
		   (match_operand:SI 2 "arith_operand" "r,L"))
	  (match_dup 1)))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
        (plus:SI (match_dup 2) (match_dup 1)))]
  ""
  "add %0,%1,%2"
  [(set_attr "type" "compare")])

(define_insn "*sub_c"
  [(set (reg:CC_C_GTU CC_REGNUM)
        (compare:CC_C_GTU
	  (minus:SI (match_operand:SI 1 "gpr_operand"  "r,r")
		    (match_operand:SI 2 "arith_operand" "r,L"))
	  (match_dup 1)))
   (set (match_operand:SI 0 "gpr_operand" "=r,r")
        (minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "sub %0,%1,%2"
  [(set_attr "type" "compare")])

(define_insn "*sub_c_void"
  [(set (reg:CC_C_GTU CC_REGNUM)
        (compare:CC_C_GTU
	  (minus:SI (match_operand:SI 1 "gpr_operand"  "r,r")
		    (match_operand:SI 2 "arith_operand" "r,L"))
	  (match_dup 1)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  ""
  "sub %0,%1,%2"
  [(set_attr "type" "compare")])

(define_code_iterator logical_op
  [and ior xor])

(define_code_attr op_mnc
  [(plus "add") (minus "sub") (and "and") (ior "orr") (xor "eor")])

(define_insn "*<op_mnc>_f"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (logical_op:SI (match_operand:SI 1 "gpr_operand" "%r")
				   (match_operand:SI 2 "gpr_operand"  "r"))
                    (const_int 0)))
   (set (match_operand:SI 0 "gpr_operand" "=r")
        (logical_op:SI (match_dup 1) (match_dup 2)))]
  ""
  "<op_mnc> %0,%1,%2"
  [(set_attr "type" "compare")])

(define_insn_and_split "*mov_f"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 1 "gpr_operand"  "r") (const_int 0)))
   (set (match_operand:SI 0 "gpr_operand" "=r") (match_dup 1))]
  ""
  "#"
  "reload_completed"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (and:SI (match_dup 1) (match_dup 1)) (const_int 0)))
     (set (match_operand:SI 0 "gpr_operand" "=r")
	  (and:SI (match_dup 1) (match_dup 1)))])]
  ""
  [(set_attr "type" "compare")])

(define_peephole2
  [(parallel
    [(set (match_operand:SI 0 "gpr_operand")
	  (logical_op:SI (match_operand:SI 1 "gpr_operand")
			 (match_operand:SI 2 "gpr_operand")))
     (clobber (reg:CC CC_REGNUM))])
   (parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (and:SI (match_dup 0) (match_dup 0)) (const_int 0)))
     (set (match_operand:SI 3 "gpr_operand")
	  (and:SI (match_dup 0) (match_dup 0)))])]
  "peep2_reg_dead_p (2, operands[0])"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (logical_op:SI (match_dup 1) (match_dup 2))
		      (const_int 0)))
     (set (match_dup 3) (logical_op:SI (match_dup 1) (match_dup 2)))])])

(define_peephole2
  [(parallel
    [(set (match_operand:SI 0 "gpr_operand")
	  (logical_op:SI (match_operand:SI 1 "gpr_operand")
			 (match_operand:SI 2 "gpr_operand")))
     (clobber (reg:CC CC_REGNUM))])
   (parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (and:SI (match_dup 0) (match_dup 0)) (const_int 0)))
     (set (match_operand:SI 3 "gpr_operand")
	  (and:SI (match_dup 0) (match_dup 0)))])]
  "peep2_reg_dead_p (2, operands[3])"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (logical_op:SI (match_dup 1) (match_dup 2))
		      (const_int 0)))
     (set (match_dup 0) (logical_op:SI (match_dup 1) (match_dup 2)))])])

(define_peephole2
  [(parallel
    [(set (match_operand:SI 0 "gpr_operand")
	  (logical_op:SI (match_operand:SI 1 "gpr_operand")
			 (match_operand:SI 2 "gpr_operand")))
     (clobber (reg:CC CC_REGNUM))])
   (parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (match_dup 0) (const_int 0)))
     (clobber (match_operand:SI 3 "gpr_operand"))])]
  ""
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (logical_op:SI (match_dup 1) (match_dup 2))
		      (const_int 0)))
     (set (match_dup 0) (logical_op:SI (match_dup 1) (match_dup 2)))])])

(define_expand "cstoresi4"
  [(parallel
    [(set (reg:CC CC_REGNUM)
          (match_operand:SI 1 "comparison_operator"))
     (match_operand:SI 2 "" "")])
   (set (match_dup 0) (match_operand:SI 3 "arith_operand" ""))
   (set (match_operand:SI 0 "gpr_operand" "=r")
	(if_then_else:SI (match_dup 4) (match_dup 5) (match_dup 0)))]
  ""
{
  enum rtx_code o2_code = GET_CODE (operands[2]);
  enum rtx_code cmp_code = GET_CODE (operands[1]);

  if ((o2_code == AND || o2_code == IOR || o2_code == XOR)
      && operands[3] == const0_rtx)
    {
      operands[2] = copy_rtx(operands[2]);
      XEXP (operands[2], 0) = force_reg (SImode, XEXP (operands[2], 0));
      XEXP (operands[2], 1) = force_reg (SImode, XEXP (operands[2], 1));
    }
  else
    operands[2] = force_reg (SImode, operands[2]);
  operands[1] = gen_rtx_COMPARE (CCmode, operands[2], operands[3]);
  if (cmp_code != NE)
    {
      operands[2] = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (SImode));
      operands[3] = const0_rtx;
    }
  else
    {
      if (operands[3] != const0_rtx)
	operands[2] = gen_rtx_MINUS (SImode, operands[2], operands[3]);
      operands[2] = gen_rtx_SET (operands[0], operands[2]);
      operands[3] = operands[0];
    }
  operands[4] = gen_rtx_fmt_ee (cmp_code, SImode,
				gen_rtx_REG (CCmode, CC_REGNUM), const0_rtx);
  operands[5] = force_reg (SImode, GEN_INT (STORE_FLAG_VALUE));
})


; floating point comparisons

(define_insn "*cmpsf_cc_insn"
  [(match_parallel 3 "float_operation"
     [(set (reg:CC_FP CCFP_REGNUM)
	   (compare:CC_FP (match_operand:SF 0 "gpr_operand" "r")
			  (match_operand:SF 1 "gpr_operand" "r")))
      (clobber (match_scratch:SF 2 "=r"))])]
  "!TARGET_SOFT_CMPSF"
  "fsub %2,%0,%1"
  [(set_attr "type" "fp")
   (set_attr "fp_mode" "round_unknown")])

;; ??? do we have to relax the operand0 predicate to immediate_operand
;; to allow the rtl loop optimizer to generate comparisons?  OTOH
;; we want call_address_operand to enforce valid operands so that
;; combine won't do silly things, allowing instruction scheduling to do
;; a proper job.
(define_insn "*cmpsf_eq"
  [(set (reg:CC_FP_EQ CC_REGNUM) (compare:CC_FP_EQ (reg:SF 0) (reg:SF 1)))
   (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
   (clobber (reg:SI GPR_IP))
   (clobber (reg:SI GPR_LR))]
  "TARGET_SOFT_CMPSF"
  "%f0"
  [(set_attr "type" "sfunc")])

(define_insn "*cmpsf_gte"
  [(set (reg:CC_FP_GTE CC_REGNUM) (compare:CC_FP_GTE (reg:SF 0) (reg:SF 1)))
   (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
   (clobber (reg:SI GPR_IP))
   (clobber (reg:SI GPR_LR))]
  "TARGET_SOFT_CMPSF"
  "%f0"
  [(set_attr "type" "sfunc")])

(define_insn "*cmpsf_ord"
  [(set (reg:CC_FP_ORD CC_REGNUM) (compare:CC_FP_ORD (reg:SF 0) (reg:SF 1)))
   (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
   (clobber (reg:SI GPR_IP))
   (clobber (reg:SI GPR_16))
   (clobber (reg:SI GPR_LR))]
  ""
  "%f0"
  [(set_attr "type" "sfunc")])

(define_insn "*cmpsf_uneq"
  [(set (reg:CC_FP_UNEQ CC_REGNUM) (compare:CC_FP_UNEQ (reg:SF 0) (reg:SF 1)))
   (use (match_operand:SI 0 "call_address_operand" "Csy,r"))
   (clobber (reg:SI GPR_IP))
   (clobber (reg:SI GPR_16))
   (clobber (reg:SI GPR_LR))]
  "TARGET_SOFT_CMPSF"
  "%f0"
  [(set_attr "type" "sfunc")])

;; conditional moves

(define_expand "mov<mode>cc"
  [(set (match_operand:WMODE 0 "gpr_operand" "")
	(if_then_else:WMODE (match_operand 1 "comparison_operator" "")
			    (match_operand:WMODE 2 "gpr_operand" "")
			    (match_operand:WMODE 3 "gpr_operand" "")))]
  "TARGET_CMOVE"
{
  rtx cmp_op0 = XEXP (operands[1], 0);
  rtx cmp_op1 = XEXP (operands[1], 1);
  machine_mode cmp_in_mode;
  enum rtx_code code = GET_CODE (operands[1]);

  cmp_in_mode = GET_MODE (cmp_op0);
  if (cmp_in_mode == VOIDmode)
    cmp_in_mode = GET_MODE (cmp_op1);
  if (cmp_in_mode == VOIDmode)
    cmp_in_mode = SImode;
  /* If the operands are a better match when reversed, swap them now.
     This allows combine to see the proper comparison codes.  */
  if (rtx_equal_p (operands[0], operands[2])
      && !rtx_equal_p (operands[0], operands[3]))
    {
      rtx tmp = operands[2]; operands[2] = operands[3]; operands[3] = tmp;
      code = (FLOAT_MODE_P (GET_MODE (cmp_op0)) && !flag_finite_math_only
	      ? reverse_condition_maybe_unordered (code)
	      : reverse_condition (code));
    }

  if (proper_comparison_operator (operands[1], VOIDmode))
    operands[1] = gen_rtx_fmt_ee (code, cmp_in_mode, cmp_op0, cmp_op1);
  else
    {
      if (!currently_expanding_to_rtl)
	{
	  /* ???  It would seem safest to FAIL here, but that would defeat
	     the purpose of having an if-conversion pass; its logic currently
	     assumes that the backend should be safe to insert condition code
	     setting instructions, as the same condition codes were presumably
	     set by the if-conversion input code.  */
	}
      /* What mode to give as first operand to gen_compare_reg here is
	 debatable.  VOIDmode would be minimalist; telling gen_compare_reg
	 to use the mode of CC_REGNUM (or putting it on the comparison
	 operator afterwards) is also a logical choice.  OTOH, by using
	 <MODE>mode, we have mode combine opportunities with flag setting
	 operations - if we get some.  */
      operands[1]
	= gen_compare_reg (<MODE>mode, code, cmp_in_mode, cmp_op0, cmp_op1);
      if (!operands[1])
	FAIL;
    }
})

(define_insn "*mov<mode>cc_insn"
  [(set (match_operand:WMODE 0 "gpr_operand" "=r")
	(if_then_else:WMODE (match_operator 3 "proper_comparison_operator"
			      [(match_operand 4 "cc_operand") (const_int 0)])
			    (match_operand:WMODE 1 "gpr_operand" "r")
			    (match_operand:WMODE 2 "gpr_operand" "0")))]
  "TARGET_CMOVE"
  "mov%d3 %0,%1"
  [(set_attr "type" "cmove")])

(define_peephole2
  [(parallel [(set (match_operand:WMODE 0 "gpr_operand" "")
		   (match_operand:WMODE 1 "" ""))
	      (clobber (match_operand 8 "cc_operand"))])
   (match_operand 2 "" "")
   (set (match_operand:WMODE2 3 "gpr_operand" "")
	(match_operand:WMODE2 9 "gpr_operand" ""))
   (set (match_dup 3)
	(if_then_else:WMODE2 (match_operator 5 "proper_comparison_operator"
			       [(match_operand 6 "cc_operand")
				(match_operand 7 "const0_operand")])
			     (match_operand:WMODE2 4 "nonmemory_operand" "")
			     (match_dup 3)))]
  "REGNO (operands[0]) == REGNO (operands[9])
   && peep2_reg_dead_p (3, operands[0])
   && !reg_set_p (operands[0], operands[2])
   && !reg_set_p (operands[3], operands[2])
   && !reg_overlap_mentioned_p (operands[3], operands[2])"
  [(parallel [(set (match_dup 10) (match_dup 1))
	      (clobber (match_dup 8))])
   (match_dup 2)
   (set (match_dup 3)
	(if_then_else:WMODE2 (match_dup 5) (match_dup 4) (match_dup 3)))]
{
  operands[10] = simplify_gen_subreg (<WMODE:MODE>mode, operands[3],
				      <WMODE2:MODE>mode, 0);
  replace_rtx (operands[2], operands[9], operands[3], true);
  replace_rtx (operands[2], operands[0], operands[10], true);
  gcc_assert (!reg_overlap_mentioned_p (operands[0], operands[2]));
})

(define_peephole2
  [(parallel [(set (match_operand 6 "cc_operand") (match_operand 2 "" ""))
	      (set (match_operand:WMODE 0 "gpr_operand" "")
		   (match_operand:WMODE 1 "" ""))])
   (set (match_operand:WMODE2 3 "gpr_operand" "")
	(match_operand:WMODE2 4 "gpr_operand"))
   (set (match_dup 3)
	(if_then_else:WMODE2 (match_operator 5 "proper_comparison_operator"
			       [(match_dup 6)
			       (match_operand:WMODE 7 "const0_operand")])
			    (match_operand:WMODE2 8 "gpr_operand")
			    (match_dup 3)))]
  "REGNO (operands[0]) == REGNO (operands[8])
   && REVERSIBLE_CC_MODE (GET_MODE (operands[6]))
   && peep2_reg_dead_p (3, operands[6])
   && peep2_reg_dead_p (3, operands[0])
   && !reg_overlap_mentioned_p (operands[4], operands[3])"
  [(parallel [(set (match_dup 6) (match_dup 2))
	      (set (match_dup 9) (match_dup 1))])
   (set (match_dup 3)
	(if_then_else:WMODE2 (match_dup 5) (match_dup 4) (match_dup 3)))]
  "
{
  operands[5]
    = gen_rtx_fmt_ee (REVERSE_CONDITION (GET_CODE (operands[5]),
					 GET_MODE (operands[6])),
		      GET_MODE (operands[5]), operands[6], operands[7]);
  operands[9] = simplify_gen_subreg (<WMODE:MODE>mode, operands[3],
				     <WMODE2:MODE>mode, 0);
}")

;; These control RTL generation for conditional jump insns

;; To signal to can_compare_p that the cbranchs?4 patterns work,
;; they must allow const0_rtx for both comparison operands
(define_expand "cbranchsi4"
  [(set (reg CC_REGNUM)
	(compare (match_operand:SI 1 "add_operand" "")
		 (match_operand:SI 2 "arith_operand" "")))
   (set (pc)
	(if_then_else
	      (match_operator 0 "ordered_comparison_operator" [(reg CC_REGNUM)
							       (const_int 0)])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
{
  rtx cmp = gen_compare_reg (VOIDmode, GET_CODE (operands[0]), SImode,
			     operands[1], operands[2]);
  emit_jump_insn (gen_branch_insn (operands[3], cmp, XEXP (cmp, 0)));
  DONE;
})

(define_expand "cbranchsf4"
  [(set (reg CC_REGNUM)
	(compare (match_operand:SF 1 "arith_operand" "")
		 (match_operand:SF 2 "arith_operand" "")))
   (set (pc)
	(if_then_else
	      (match_operator 0 "comparison_operator" [(reg CC_REGNUM)
						       (const_int 0)])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
{
  rtx cmp = gen_compare_reg (VOIDmode, GET_CODE (operands[0]), SFmode,
			     operands[1], operands[2]);
  emit_jump_insn (gen_branch_insn (operands[3], cmp, XEXP (cmp, 0)));
  DONE;
})

;; Now match both normal and inverted jump.

(define_insn "branch_insn"
  [(set (pc)
	(if_then_else (match_operator 1 "proper_comparison_operator"
				      [(match_operand 2 "cc_operand")
				       (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "b%d1 %l0"
  [(set_attr "type" "branch")])

(define_insn "*rev_branch_insn"
  [(set (pc)
	(if_then_else (match_operator 1 "proper_comparison_operator"
				      [(reg CC_REGNUM) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "b%D1 %l0"
  [(set_attr "type" "branch")])

;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "b %l0"
  [(set_attr "type" "uncond_branch")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "gpr_operand" "r"))]
  ""
  "jr %0"
  [(set_attr "type" "uncond_branch")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "gpr_operand" ""))
              (use (label_ref (match_operand 1 "" "")))])]
  ""
{
  /* In PIC mode, the table entries are stored PC relative.
     Convert the relative address to an absolute address.  */
  if (flag_pic)
    {
      rtx op1 = gen_rtx_LABEL_REF (Pmode, operands[1]);

      operands[0] = expand_simple_binop (Pmode, PLUS, operands[0],
					 op1, NULL_RTX, 0, OPTAB_DIRECT);
    }
})

(define_insn "*tablejump_internal"
  [(set (pc) (match_operand:SI 0 "gpr_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jr %0;"
  [(set_attr "type" "uncond_branch")])

(define_insn "*tablejump_hi_internal"
  [(set (pc) (match_operand:HI 0 "gpr_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  "optimize_size && TARGET_SMALL16"
  "jr %0;"
  [(set_attr "type" "uncond_branch")])


(define_expand "call"
  ;; operands[1] is stack_size_rtx
  ;; operands[2] is next_arg_register
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	     (clobber (reg:SI GPR_LR))])]
  ""
{
  bool target_uninterruptible = epiphany_call_uninterruptible_p (operands[0]);

  if (!call_operand (operands[1], VOIDmode))
    operands[0]
      = change_address (operands[0], VOIDmode,
			copy_to_mode_reg (Pmode, XEXP (operands[0], 0)));
  if (epiphany_uninterruptible_p (current_function_decl)
      != target_uninterruptible)
    {
      emit_insn (target_uninterruptible ? gen_gid () : gen_gie ());
      emit_call_insn
	(gen_rtx_PARALLEL
	  (VOIDmode,
	   gen_rtvec (2, gen_rtx_CALL (VOIDmode, operands[0], operands[1]),
			 gen_rtx_CLOBBER (VOIDmode,
					  gen_rtx_REG (SImode, GPR_LR)))));
      emit_insn (target_uninterruptible ? gen_gie () : gen_gid ());
      DONE;
    }
})

(define_insn "*call_i"
  [(match_parallel 2 "float_operation"
     [(call (mem:SI (match_operand:SI 0 "call_address_operand" "Csy,r"))
	    (match_operand 1 "" ""))
      (clobber (reg:SI GPR_LR))])]
  ""
  "%f0"
  [(set_attr "type" "call")])

(define_expand "sibcall"
  ;; operands[1] is stack_size_rtx
  ;; operands[2] is next_arg_register
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	     (return)])]
  ""
{
  bool target_uninterruptible = epiphany_call_uninterruptible_p (operands[0]);

  if (!call_operand (operands[1], VOIDmode))
    operands[0]
      = change_address (operands[0], VOIDmode,
			copy_to_mode_reg (Pmode, XEXP (operands[0], 0)));
  if (epiphany_uninterruptible_p (current_function_decl)
      != target_uninterruptible)
    {
      emit_insn (target_uninterruptible ? gen_gid () : gen_gie ());
      emit_call_insn
	(gen_rtx_PARALLEL
	  (VOIDmode,
	   gen_rtvec (2, gen_rtx_CALL (VOIDmode, operands[0], operands[1]),
			 ret_rtx)));
      emit_insn (target_uninterruptible ? gen_gie () : gen_gid ());
      DONE;
    }
})

(define_insn "*sibcall_i"
  [(call (mem:SI (match_operand:SI 0 "call_address_operand" "Csy,Rsc"))
	 (match_operand 1 "" ""))
   (return)]
  ""
  "@
   b %0
   jr %0"
  [(set_attr "type" "call")])

(define_expand "call_value"
  ;; operand 2 is stack_size_rtx
  ;; operand 3 is next_arg_register
  [(parallel [(set (match_operand 0 "gpr_operand" "=r")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand 2 "" "")))
	     (clobber (reg:SI GPR_LR))])]
  ""
{
  bool target_uninterruptible = epiphany_call_uninterruptible_p (operands[1]);

  if (!call_operand (operands[1], VOIDmode))
    operands[1]
      = change_address (operands[1], VOIDmode,
			copy_to_mode_reg (Pmode, XEXP (operands[1], 0)));
  if (epiphany_uninterruptible_p (current_function_decl)
      != target_uninterruptible)
    {
      emit_insn (target_uninterruptible ? gen_gid () : gen_gie ());
      emit_call_insn
	(gen_rtx_PARALLEL
	  (VOIDmode,
	   gen_rtvec (2, gen_rtx_SET
			   (operands[0],
			    gen_rtx_CALL (VOIDmode, operands[1], operands[2])),
			 gen_rtx_CLOBBER (VOIDmode,
					  gen_rtx_REG (SImode, GPR_LR)))));
      emit_insn (target_uninterruptible ? gen_gie () : gen_gid ());
      DONE;
    }
})

(define_insn "*call_value_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand 0 "gpr_operand" "=r,r")
	   (call (mem:SI (match_operand:SI 1 "call_address_operand" "Csy,r"))
	         (match_operand 2 "" "")))
      (clobber (reg:SI GPR_LR))])]
  ""
  "%f1"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_expand "sibcall_value"
  ;; operand 2 is stack_size_rtx
  ;; operand 3 is next_arg_register
  [(parallel [(set (match_operand 0 "gpr_operand" "=r")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand 2 "" "")))
	     (return)])]
  ""
{
  bool target_uninterruptible = epiphany_call_uninterruptible_p (operands[1]);

  if (!call_operand (operands[1], VOIDmode))
    operands[1]
      = change_address (operands[1], VOIDmode,
			copy_to_mode_reg (Pmode, XEXP (operands[1], 0)));
  if (epiphany_uninterruptible_p (current_function_decl)
      != target_uninterruptible)
    {
      emit_insn (target_uninterruptible ? gen_gid () : gen_gie ());
      emit_call_insn
	(gen_rtx_PARALLEL
	  (VOIDmode,
	   gen_rtvec (2, gen_rtx_SET
			   (operands[0],
			    gen_rtx_CALL (VOIDmode, operands[1], operands[2])),
			 ret_rtx)));
      emit_insn (target_uninterruptible ? gen_gie () : gen_gid ());
      DONE;
    }
})

(define_insn "*sibcall_value_i"
  [(set (match_operand 0 "gpr_operand" "=r,r")
	(call (mem:SI (match_operand:SI 1 "call_address_operand" "Csy,Rsc"))
	      (match_operand 2 "" "")))
   (return)]
  ""
  "@
   b %1
   jr %1"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_expand "prologue"
  [(pc)]
  ""
{
  epiphany_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(pc)]
  ""
{
  epiphany_expand_epilogue (0);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(pc)]
  ""
{
  epiphany_expand_epilogue (1);
  DONE;
})

; Since the demise of REG_N_SETS, it is no longer possible to find out
; in the prologue / epilogue expanders how many times lr is set.
; Using df_regs_ever_live_p to decide if lr needs saving means that
; any explicit use of lr will cause it to be saved; hence we cannot
; represent the blink use in return / sibcall instructions themselves, and
; instead have to show it in EPILOGUE_USES.
(define_insn "return_i"
  [(return)]
  "reload_completed"
  "rts"
  [(set_attr "type" "uncond_branch")])

(define_insn "return_internal_interrupt"
  [(return)
   (unspec_volatile [(const_int 0)] 1)]
  ""
  "rti"
  [(set_attr "type" "uncond_branch")])

(define_insn "stack_adjust_add"
  [(set (reg:SI GPR_SP)
	(plus:SI (reg:SI GPR_SP) (match_operand:SI 0 "arith_operand" "rL")))
   (clobber (reg:CC CC_REGNUM))
   (clobber (reg:SI STATUS_REGNUM))
   (clobber (match_operand:BLK 1 "memclob_operand" "=X"))]
  "reload_completed"
  "add sp,sp,%0")

(define_insn "stack_adjust_mov"
  [(set (reg:SI GPR_SP) (reg:SI GPR_FP))
   (clobber (match_operand:BLK 0 "memory_operand" "=m"))]
  "reload_completed"
  "mov sp,fp"
  [(set_attr "type" "move")])

(define_insn "stack_adjust_str"
  [(set (match_operand 0 "stacktop_operand" "=m")
	(match_operand 1 "any_gpr_operand" "r"))
   (set (reg:SI GPR_SP)
	(plus:SI (reg:SI GPR_SP) (match_operand:SI 2 "nonmemory_operand" "rn")))
   (clobber (match_operand:BLK 3 "memclob_operand" "=X"))]
  "reload_completed"
{
  return (GET_MODE_SIZE (GET_MODE (operands[0])) <= 4
	  ? \"str %1,%0,%C2\" : \"strd %1,%0,%X2\");
}
  [(set_attr "type" "store")])

(define_insn "stack_adjust_ldr"
  [(set (match_operand:SI 0 "gpr_operand" "=r")
	(match_operand:SI 1 "stacktop_operand" "m"))
   (set (reg:SI GPR_SP)
	(plus:SI (reg:SI GPR_SP) (match_operand:SI 2 "nonmemory_operand" "rn")))
   (clobber (match_operand:BLK 3 "memory_operand" "=m"))]
  "reload_completed"
  "ldr %0,%1,%C2"
  [(set_attr "type" "load")])

;; Define some fake vector operations so that the vectorizer is happy to use
;; 64 bit loads/stores.
(define_expand "vec_unpacks_lo_v4hi"
  [(match_operand:V2SI 0 "gpr_operand")
   (match_operand:V4HI 1 "gpr_operand")]
  ""
{
  rtx in = simplify_gen_subreg (SImode, operands[1], V4HImode, 0);
  rtx outl = simplify_gen_subreg (SImode, operands[0], V2SImode, 0);
  rtx outh
    = simplify_gen_subreg (SImode, operands[0], V2SImode, UNITS_PER_WORD);

  if (reg_overlap_mentioned_p (outl, in))
    in = copy_to_mode_reg (SImode, in);
  emit_insn (gen_ashlsi3 (outl, in, GEN_INT (16)));
  emit_insn (gen_ashrsi3 (outl, outl, GEN_INT (16)));
  emit_insn (gen_ashrsi3 (outh, in, GEN_INT (16)));
  DONE;
})

(define_expand "vec_unpacks_hi_v4hi"
  [(match_operand:V2SI 0 "gpr_operand")
   (match_operand:V4HI 1 "gpr_operand")]
  ""
{
  rtx in = simplify_gen_subreg (SImode, operands[1], V4HImode, UNITS_PER_WORD);
  rtx outl = simplify_gen_subreg (SImode, operands[0], V2SImode, 0);
  rtx outh
    = simplify_gen_subreg (SImode, operands[0], V2SImode, UNITS_PER_WORD);

  if (reg_overlap_mentioned_p (outl, in))
    in = copy_to_mode_reg (SImode, in);
  emit_insn (gen_ashlsi3 (outl, in, GEN_INT (16)));
  emit_insn (gen_ashrsi3 (outl, outl, GEN_INT (16)));
  emit_insn (gen_ashrsi3 (outh, in, GEN_INT (16)));
  DONE;
})

(define_code_iterator addsub [plus minus])

(define_code_iterator alu_binop
  [plus minus and ior xor])

(define_code_attr insn_opname
  [(plus "add") (minus "sub") (mult "mul") (div "div")
   (and "and") (ior "ior") (xor "xor")])

; The addsi3 / subsi3 do checks that we don't want when splitting V2SImode
; operations into two SImode operations.
(define_code_attr si_pattern_suffix
  [(plus "_i") (minus "_i") (and "") (ior "") (xor "")])

; You might think that this would work better as a define_expand, but
; again lower_subreg pessimizes the code if it sees indiviudual operations.
; We need to keep inputs and outputs as register pairs if we want to
; get sensible register allocation for double-word load and store operations.
(define_insn_and_split "<insn_opname>v2si3"
  [(set (match_operand:V2SI 0 "gpr_operand" "=r")
	(alu_binop:V2SI (match_operand:V2SI 1 "gpr_operand" "r")
			(match_operand:V2SI 2 "gpr_operand" "r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "#"
  "reload_completed || (epiphany_vect_align == 4 && TARGET_SPLIT_VECMOVE_EARLY)"
  [(const_int 0)]
{
  rtx o0l, o0h, o1l, o1h, o2l, o2h;

  o0l = simplify_gen_subreg (SImode, operands[0], V2SImode, 0);
  o0h = simplify_gen_subreg (SImode, operands[0], V2SImode, UNITS_PER_WORD);
  o1l = simplify_gen_subreg (SImode, operands[1], V2SImode, 0);
  o1h = simplify_gen_subreg (SImode, operands[1], V2SImode, UNITS_PER_WORD);
  o2l = simplify_gen_subreg (SImode, operands[2], V2SImode, 0);
  o2h = simplify_gen_subreg (SImode, operands[2], V2SImode, UNITS_PER_WORD);
  if (reg_overlap_mentioned_p (o0l, o1h))
    o1h = copy_to_mode_reg (SImode, o1h);
  if (reg_overlap_mentioned_p (o0l, o2h))
    o2h = copy_to_mode_reg (SImode, o2h);
  emit_insn (gen_<insn_opname>si3<si_pattern_suffix> (o0l, o1l, o2l));
  emit_insn (gen_<insn_opname>si3<si_pattern_suffix> (o0h, o1h, o2h));
  DONE;
}
  [(set_attr "length" "8")])

(define_expand "<insn_opname>v2sf3"
  [(parallel
     [(set (match_operand:V2SF 0 "gpr_operand" "")
	   (addsub:V2SF (match_operand:V2SF 1 "gpr_operand" "")
			(match_operand:V2SF 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn_and_split "<insn_opname>v2sf3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:V2SF 0 "gpr_operand" "=r")
	   (addsub:V2SF (match_operand:V2SF 1 "gpr_operand" "r")
			(match_operand:V2SF 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "reload_completed || (epiphany_vect_align == 4 && TARGET_SPLIT_VECMOVE_EARLY)"
  [(parallel
     [(set (match_dup 4) (addsub:SF (match_dup 5) (match_dup 6)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 10)
      (match_dup 11)])
   (parallel
     [(set (match_dup 7) (addsub:SF (match_dup 8) (match_dup 9)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 10)
      (match_dup 11)])]
{
  operands[4] = simplify_gen_subreg (SFmode, operands[0], V2SFmode, 0);
  operands[5] = simplify_gen_subreg (SFmode, operands[1], V2SFmode, 0);
  operands[6] = simplify_gen_subreg (SFmode, operands[2], V2SFmode, 0);
  operands[7]
    = simplify_gen_subreg (SFmode, operands[0], V2SFmode, UNITS_PER_WORD);
  operands[8]
    = simplify_gen_subreg (SFmode, operands[1], V2SFmode, UNITS_PER_WORD);
  operands[9]
    = simplify_gen_subreg (SFmode, operands[2], V2SFmode, UNITS_PER_WORD);
  if (!reload_completed)
    {
      if (reg_overlap_mentioned_p (operands[4], operands[8]))
	operands[8] = copy_to_mode_reg (SFmode, operands[8]);
      if (reg_overlap_mentioned_p (operands[4], operands[9]))
	operands[9] = copy_to_mode_reg (SFmode, operands[9]);
      emit_insn (gen_<insn_opname>sf3 (operands[4], operands[5], operands[6]));
      emit_insn (gen_<insn_opname>sf3 (operands[7], operands[8], operands[9]));
      DONE;
    }
  gcc_assert (!reg_overlap_mentioned_p (operands[4], operands[8]));
  gcc_assert (!reg_overlap_mentioned_p (operands[4], operands[9]));
  operands[10] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 2);
  operands[11] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 1);
}
  [(set_attr "length" "8")
   (set_attr "type" "v2fp")])

(define_expand "ashlv2si3"
  [(parallel
     [(set (match_operand:V2SI 0 "gpr_operand" "")
	   (ashift:V2SI (match_operand:V2SI 1 "gpr_operand" "")
			(match_operand:SI 2 "general_operand")))
      (use (match_dup 3))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
{
  if (const_int_operand (operands[2], VOIDmode))
    operands[3]
      = copy_to_mode_reg (SImode, GEN_INT (1 << INTVAL (operands[2])));
  else
    {
      int o, i;
      rtx xop[2], last_out = pc_rtx;

      for (o = 0; o <= UNITS_PER_WORD; o += UNITS_PER_WORD)
	{
	  for (i = 0; i < 2; i++)
	    {
	      xop[i]
		= (i == 2 ? operands[2]
		   : simplify_gen_subreg (SImode, operands[i], V2SImode, o));
	      gcc_assert (!reg_overlap_mentioned_p (last_out, xop[i])
			  /* ??? reg_overlap_mentioned_p doesn't understand
			     about multi-word SUBREGs.  */
			  || (GET_CODE (last_out) == SUBREG
			      && GET_CODE (xop[i]) == SUBREG
			      && SUBREG_REG (last_out) == SUBREG_REG (xop[i])
			      && ((SUBREG_BYTE (last_out) & -UNITS_PER_WORD)
				  != (SUBREG_BYTE (xop[i]) & -UNITS_PER_WORD))));
	    }
	  emit_insn (gen_ashlsi3 (xop[0], xop[1], operands[2]));
	  last_out = xop[0];
	}
      DONE;
    }
})

(define_insn_and_split "*ashlv2si3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:V2SI 0 "gpr_operand" "=&r,*1*2")
	   (ashift:V2SI (match_operand:V2SI 1 "gpr_operand" "r,r")
			(match_operand 2 "const_int_operand" "n,n")))
      (use (match_operand:SI 4 "gpr_operand" "r,r"))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 5) (mult:SI (match_dup 6) (match_dup 4)))
	   (clobber (reg:CC_FP CCFP_REGNUM))
	   (match_dup 9)
	   (match_dup 10)])
   (parallel
     [(set (match_dup 7) (mult:SI (match_dup 8) (match_dup 4)))
	   (clobber (reg:CC_FP CCFP_REGNUM))
	   (match_dup 9)
	   (match_dup 10)])]
{
  operands[5] = simplify_gen_subreg (SImode, operands[0], V2SImode, 0);
  operands[6] = simplify_gen_subreg (SImode, operands[1], V2SImode, 0);
  operands[7] = simplify_gen_subreg (SImode, operands[0],
				     V2SImode, UNITS_PER_WORD);
  operands[8] = simplify_gen_subreg (SImode, operands[1],
				     V2SImode, UNITS_PER_WORD);
  gcc_assert (!reg_overlap_mentioned_p (operands[5], operands[8]));
  gcc_assert (!reg_overlap_mentioned_p (operands[5], operands[4]));
  operands[9] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 2);
  operands[10] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 1);
  rtx insn
    = (gen_rtx_PARALLEL
	(VOIDmode,
	 gen_rtvec
	  (4,
	   gen_rtx_SET (operands[5],
			gen_rtx_MULT (SImode, operands[6], operands[4])),
	   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CC_FPmode, CCFP_REGNUM)),
	   operands[9], operands[10])));
  insn = emit_insn (insn);
  add_reg_note (insn, REG_EQUAL,
		gen_rtx_ASHIFT (SImode, operands[6], operands[2]));
  insn
    = (gen_rtx_PARALLEL
	(VOIDmode,
	 gen_rtvec
	  (4,
	   gen_rtx_SET (operands[7],
			gen_rtx_MULT (SImode, operands[8], operands[4])),
	   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CC_FPmode, CCFP_REGNUM)),
	   operands[9], operands[10])));
  insn = emit_insn (insn);
  add_reg_note (insn, REG_EQUAL,
		gen_rtx_ASHIFT (SImode, operands[7], operands[2]));
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "fp_int")])

(define_expand "mul<mode>3"
  [(parallel
     [(set (match_operand:DWV2MODE 0 "gpr_operand" "")
	   (mult:DWV2MODE (match_operand:DWV2MODE 1 "gpr_operand" "")
			  (match_operand:DWV2MODE 2 "gpr_operand" "")))
      (clobber (reg:CC_FP CCFP_REGNUM))])])

(define_insn_and_split "mul<mode>3_i"
  [(match_parallel 3 "float_operation"
     [(set (match_operand:DWV2MODE 0 "gpr_operand" "=r")
	   (mult:DWV2MODE (match_operand:DWV2MODE 1 "gpr_operand" "r")
			  (match_operand:DWV2MODE 2 "gpr_operand" "r")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  ""
  "#"
  "reload_completed || (epiphany_vect_align == 4 && TARGET_SPLIT_VECMOVE_EARLY)"
  [(parallel
     [(set (match_dup 4) (mult:<vmode_PART> (match_dup 5) (match_dup 6)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 10)
      (match_dup 11)])
   (parallel
     [(set (match_dup 7) (mult:<vmode_PART> (match_dup 8) (match_dup 9)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 10)
      (match_dup 11)])]
{
  operands[4]
    = simplify_gen_subreg (<vmode_PART>mode, operands[0], <MODE>mode, 0);
  operands[5]
    = simplify_gen_subreg (<vmode_PART>mode, operands[1], <MODE>mode, 0);
  operands[6]
    = simplify_gen_subreg (<vmode_PART>mode, operands[2], <MODE>mode, 0);
  operands[7] = simplify_gen_subreg (<vmode_PART>mode, operands[0],
				     <MODE>mode, UNITS_PER_WORD);
  operands[8] = simplify_gen_subreg (<vmode_PART>mode, operands[1],
				     <MODE>mode, UNITS_PER_WORD);
  operands[9] = simplify_gen_subreg (<vmode_PART>mode, operands[2],
				     <MODE>mode, UNITS_PER_WORD);
  if (!reload_completed)
    {
      if (reg_overlap_mentioned_p (operands[4], operands[8]))
	operands[8] = copy_to_mode_reg (<vmode_PART>mode, operands[8]);
      if (reg_overlap_mentioned_p (operands[4], operands[9]))
	operands[9] = copy_to_mode_reg (<vmode_PART>mode, operands[9]);
      emit_insn (gen_mul<vmode_part>3 (operands[4], operands[5], operands[6]));
      emit_insn (gen_mul<vmode_part>3 (operands[7], operands[8], operands[9]));
      DONE;
    }
  gcc_assert (!reg_overlap_mentioned_p (operands[4], operands[8]));
  gcc_assert (!reg_overlap_mentioned_p (operands[4], operands[9]));
  operands[10] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 2);
  operands[11] = XVECEXP (operands[3], 0, XVECLEN (operands[3], 0) - 1);
}
  [(set_attr "length" "8")
   (set_attr "type" "<vmode_fp_type>")])

(define_insn_and_split "*fmadd<mode>_combine"
  [(match_parallel 4 "float_operation"
     [(set (match_operand:DWV2MODE 0 "gpr_operand" "=r")
	   (plus:DWV2MODE (mult:<MODE>
				(match_operand:<MODE> 1 "gpr_operand" "r")
				(match_operand:<MODE> 2 "gpr_operand" "r"))
			  (match_operand:<MODE> 3 "gpr_operand" "0")))
      (clobber (reg:CC_FP CCFP_REGNUM))])]
  "TARGET_FUSED_MADD || <MODE>mode == V2SImode"
  "#"
  "reload_completed || (epiphany_vect_align == 4 && TARGET_SPLIT_VECMOVE_EARLY)"
  [(parallel
     [(set (match_dup 5)
	   (plus:<vmode_PART> (mult:<vmode_PART> (match_dup 6) (match_dup 7))
			      (match_dup 8)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 13)
      (match_dup 14)])
   (parallel
     [(set (match_dup 9)
	   (plus:<vmode_PART> (mult:<vmode_PART> (match_dup 10) (match_dup 11))
			      (match_dup 12)))
      (clobber (reg:CC_FP CCFP_REGNUM))
      (match_dup 13)
      (match_dup 14)])]
{
  operands[5]
    = simplify_gen_subreg (<vmode_PART>mode, operands[0], <MODE>mode, 0);
  operands[6]
    = simplify_gen_subreg (<vmode_PART>mode, operands[1], <MODE>mode, 0);
  operands[7]
    = simplify_gen_subreg (<vmode_PART>mode, operands[2], <MODE>mode, 0);
  operands[8]
    = simplify_gen_subreg (<vmode_PART>mode, operands[3], <MODE>mode, 0);
  operands[9] = simplify_gen_subreg (<vmode_PART>mode, operands[0],
				     <MODE>mode, UNITS_PER_WORD);
  operands[10] = simplify_gen_subreg (<vmode_PART>mode, operands[1],
				      <MODE>mode, UNITS_PER_WORD);
  operands[11] = simplify_gen_subreg (<vmode_PART>mode, operands[2],
				      <MODE>mode, UNITS_PER_WORD);
  operands[12] = simplify_gen_subreg (<vmode_PART>mode, operands[3],
				      <MODE>mode, UNITS_PER_WORD);
  if (!reload_completed)
    {
      if (reg_overlap_mentioned_p (operands[5], operands[10]))
	operands[10] = copy_to_mode_reg (<vmode_PART>mode, operands[10]);
      if (reg_overlap_mentioned_p (operands[5], operands[11]))
	operands[11] = copy_to_mode_reg (<vmode_PART>mode, operands[11]);
      if (reg_overlap_mentioned_p (operands[5], operands[12]))
	operands[12] = copy_to_mode_reg (<vmode_PART>mode, operands[12]);
      emit_insn (gen_madd<vmode_part> (operands[5], operands[6], operands[7],
				       operands[8]));
      emit_insn (gen_madd<vmode_part> (operands[9], operands[10], operands[11],
				       operands[12]));
      DONE;
    }
  gcc_assert (!reg_overlap_mentioned_p (operands[5], operands[10]));
  gcc_assert (!reg_overlap_mentioned_p (operands[5], operands[11]));
  gcc_assert (!reg_overlap_mentioned_p (operands[5], operands[12]));
  operands[13] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 2);
  operands[14] = XVECEXP (operands[4], 0, XVECLEN (operands[4], 0) - 1);
}
  [(set_attr "length" "8")
   (set_attr "type" "<vmode_fp_type>")])

(define_expand "vec_set<mode>"
  [(match_operand:DWV2MODE 0 "register_operand")
   (match_operand:<vmode_PART> 1 "register_operand")
   (match_operand 2 "const_int_operand" "")]
  ""
{
  operands[0]
    = simplify_gen_subreg (<vmode_PART>mode, operands[0], <MODE>mode,
			   UNITS_PER_WORD * INTVAL (operands[2]));
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "movmisalign<mode>"
 [(set (match_operand:DWV2MODE 0 "nonimmediate_operand" "")
       (match_operand:DWV2MODE 1 "general_operand" ""))]
 ""
{
  rtx op00, op01, op10, op11;

  op00 = simplify_gen_subreg (<vmode_PART>mode, operands[0], <MODE>mode, 0);
  op01 = simplify_gen_subreg (<vmode_PART>mode, operands[0], <MODE>mode,
			      UNITS_PER_WORD);
  op10 = simplify_gen_subreg (<vmode_PART>mode, operands[1], <MODE>mode, 0);
  op11 = simplify_gen_subreg (<vmode_PART>mode, operands[1], <MODE>mode,
			      UNITS_PER_WORD);
  emit_move_insn (op00, op10);
  emit_move_insn (op01, op11);
  DONE;
})

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "flow")])
