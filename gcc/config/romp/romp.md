;;- Machine description for ROMP chip for GNU C compiler
;;   Copyright (C) 1988, 1991, 1993, 1994, 1995, 1998, 1999, 2000
;;   Free Software Foundation, Inc.
;;   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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


;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Define the attributes for the ROMP.

;; Insn type.  Used to default other attribute values.

(define_attr "type"
  "branch,ibranch,return,fp,load,loadz,store,call,address,arith,compare,multi,misc"
  (const_string "arith"))

;; Length in bytes.

(define_attr "length" ""
  (cond [(eq_attr "type" "branch")
	 (if_then_else (and (ge (minus (pc) (match_dup 0))
				(const_int -256))
			    (le (minus (pc) (match_dup 0))
				(const_int 254)))
		       (const_int 2)
		       (const_int 4))
	 (eq_attr "type" "return,ibranch") (const_int 2)
	 (eq_attr "type" "fp")		(const_int 10)
	 (eq_attr "type" "call")	(const_int 4)
	 (eq_attr "type" "load")
	   (cond [(match_operand 1 "short_memory_operand" "") (const_int 2)
		  (match_operand 1 "symbolic_memory_operand" "") (const_int 8)]
		 (const_int 4))
         (eq_attr "type" "loadz")
	   (cond [(match_operand 1 "zero_memory_operand" "") (const_int 2)
		  (match_operand 1 "symbolic_memory_operand" "") (const_int 8)]
		 (const_string "4"))
	 (eq_attr "type" "store")
	   (cond [(match_operand 0 "short_memory_operand" "") (const_int 2)
		  (match_operand 0 "symbolic_memory_operand" "") (const_int 8)]
		 (const_int 4))]
	(const_int 4)))

;; Whether insn can be placed in a delay slot.

(define_attr "in_delay_slot" "yes,no" 
  (cond [(eq_attr "length" "8,10,38")			(const_string "no")
	 (eq_attr "type" "branch,ibranch,return,call,multi")
	 (const_string "no")]
	(const_string "yes")))

;; Whether insn needs a delay slot.  We have to say that two-byte
;; branches do not need a delay slot.  Otherwise, branch shortening will
;; try to do something with delay slot insns (we want it to on the PA).
;; This is a kludge, which should be cleaned up at some point.

(define_attr "needs_delay_slot" "yes,no"
  (if_then_else (ior (and (eq_attr "type" "branch")
			  (eq_attr "length" "4"))
		     (eq_attr "type" "ibranch,return,call"))
		(const_string "yes") (const_string "no")))

;; What insn does to the condition code.

(define_attr "cc"
  "clobber,none,sets,change0,copy1to0,compare,tbit"
  (cond [(eq_attr "type" "load,loadz")		(const_string "change0")
	 (eq_attr "type" "store")		(const_string "none")
	 (eq_attr "type" "fp,call")		(const_string "clobber")
	 (eq_attr "type" "branch,ibranch,return") (const_string "none")
	 (eq_attr "type" "address")		(const_string "change0")
	 (eq_attr "type" "compare")		(const_string "compare")
	 (eq_attr "type" "arith")		(const_string "sets")]
	(const_string "clobber")))

;; Define attributes for `asm' insns.

(define_asm_attributes [(set_attr "type" "misc")
			(set_attr "length" "8")
			(set_attr "in_delay_slot" "no")
			(set_attr "cc" "clobber")])

;; Define the delay slot requirements for branches and calls.  We don't have
;; any annulled insns.
;;
(define_delay (eq_attr "needs_delay_slot" "yes")
  [(eq_attr "in_delay_slot" "yes") (nil) (nil)])

;; We cannot give a floating-point comparison a delay slot, even though it
;; could make use of it.  This is because it would confuse next_cc0_user
;; to do so.  Other fp insns can't get a delay slow because they set their
;; result and use their input after the delay slot insn is executed.  This
;; isn't what reorg.c expects.  

;; Define load & store delays.  These were obtained by measurements done by
;; jfc@athena.mit.edu.
;;
;; In general, the memory unit can support at most two simultaneous operations.
;;
;; Loads take 5 cycles to return the data and can be pipelined up to the
;; limit of two simultaneous operations.
(define_function_unit "memory" 1 2 (eq_attr "type" "load,loadz") 5 0)

;; Stores do not return data, but tie up the memory unit for 2 cycles if the
;; next insn is also a store.
(define_function_unit "memory" 1 2 (eq_attr "type" "store") 1 2
  [(eq_attr "type" "store")])

;; Move word instructions.
;;
;; If destination is memory but source is not register, force source to
;; register.
;;
;; If source is a constant that is too large to load in a single insn, build
;; it in two pieces.
;;
;; If destination is memory and source is a register, a temporary register
;; will be needed.  In that case, make a PARALLEL of the SET and a
;; CLOBBER of a SCRATCH to allocate the required temporary.
;;
;; This temporary is ACTUALLY only needed when the destination is a
;; relocatable expression.  For generating RTL, however, we always
;; place the CLOBBER.  In insns where it is not needed, the SCRATCH will
;; not be allocated to a register.
;;
;; Also, avoid creating pseudo-registers or SCRATCH rtx's during reload as
;; they will not be correctly handled.  We never need pseudos for that
;; case anyway.
;;
;; We do not use DEFINE_SPLIT for loading constants because the number
;; of cases in the resulting unsplit insn would be too high to deal
;; with practically.
(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{ rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (GET_CODE (op1) == REG && REGNO (op1) == 16)
    DONE;

  if (GET_CODE (op0) == REG && REGNO (op0) == 16)
    DONE;

  if (GET_CODE (op0) == MEM && ! reload_in_progress)
    {
      emit_insn (gen_storesi (operands[0], force_reg (SImode, operands[1])));
      DONE;
    }
  else if (GET_CODE (op1) == CONST_INT)
    {
      int const_val = INTVAL (op1);

      /* Try a number of cases to see how to best load the constant.  */
      if ((const_val & 0xffff) == 0
	  || (const_val & 0xffff0000) == 0
	  || (unsigned) (const_val + 0x8000) < 0x10000)
	/* Can do this in one insn, so generate it.  */
	;
      else if (((- const_val) & 0xffff) == 0
	       || ((- const_val) & 0xffff0000) == 0
	       || (unsigned) ((- const_val) + 0x8000) < 0x10000)
	{
	  /* Can do this by loading the negative constant and then negating. */
	  emit_move_insn (operands[0], GEN_INT (- const_val));
	  emit_insn (gen_negsi2 (operands[0], operands[0]));
	  DONE;
	}
      else
	/* Do this the long way.  */
	{
	  unsigned int high_part = const_val & 0xffff0000;
	  unsigned int low_part = const_val & 0xffff;
	  int i;

	  if (low_part >= 0x10 && exact_log2 (low_part) >= 0)
	    i = high_part, high_part = low_part, low_part = i;

	  emit_move_insn (operands[0], GEN_INT (low_part));
	  emit_insn (gen_iorsi3 (operands[0], operands[0],
				 GEN_INT (high_part)));
	  DONE;
	}
    }
}")

;; Move from a symbolic memory location to a register is special.  In this
;; case, we know in advance that the register cannot be r0, so we can improve
;; register allocation by treating it separately.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=b")
	(match_operand:SI 1 "symbolic_memory_operand" "m"))]
  ""
  "load %0,%1"
  [(set_attr "type" "load")])

;; Generic single-word move insn.  We avoid the case where the destination is
;; a symbolic address, as that needs a temporary register.

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,r,r,r,b,Q")
	(match_operand:SI 1 "romp_operand" "rR,I,K,L,M,S,s,Q,m,r"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   cas %0,%1,r0
   lis %0,%1
   cal %0,%1(r0)
   cal16 %0,%1(r0)
   cau %0,%H1(r0)
   ail %0,r14,%C1
   get %0,$%1
   l%M1 %0,%1
   load %0,%1
   st%M0 %1,%0"
  [(set_attr "type" "address,address,address,address,address,arith,misc,load,load,store")
   (set_attr "length" "2,2,4,4,4,4,8,*,*,*")])

(define_insn "storesi"
  [(set (match_operand:SI 0 "memory_operand" "=Q,m")
	(match_operand:SI 1 "register_operand" "r,r"))
   (clobber (match_scratch:SI 2 "=X,&b"))]
  ""
  "@
   st%M0 %1,%0
   store %1,%0,%2"
  [(set_attr "type" "store")])

;; This pattern is used by reload when we store into a symbolic address.  It
;; provides the temporary register required.  This pattern is only used
;; when SECONDARY_OUTPUT_RELOAD_CLASS returns something other than
;; NO_REGS, so we need not have any predicates here.

(define_expand "reload_outsi"
  [(parallel [(set (match_operand:SI 0 "symbolic_memory_operand" "=m")
		   (match_operand:SI 1 "" "r"))
	      (clobber (match_operand:SI 2 "" "=&b"))])]
  ""
  "")

;; Now do the same for the QI move instructions.
(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{ rtx op0 = operands[0];

  if (GET_CODE (op0) == MEM && ! reload_in_progress)
    {
      emit_insn (gen_storeqi (operands[0], force_reg (QImode, operands[1])));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=b")
	(match_operand:QI 1 "symbolic_memory_operand" "m"))]
  "" 
  "loadc %0,%1"
  [(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,b,Q")
	(match_operand:QI 1 "romp_operand" "r,I,n,s,Q,m,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   cas %0,%1,r0
   lis %0,%1
   cal %0,%L1(r0)
   get %0,$%1
   lc%M1 %0,%1
   loadc %0,%1
   stc%M0 %1,%0"
  [(set_attr "type" "address,address,address,misc,load,load,store")
   (set_attr "length" "2,2,4,8,*,*,*")])

(define_insn "storeqi"
  [(set (match_operand:QI 0 "memory_operand" "=Q,m")
	(match_operand:QI 1 "register_operand" "r,r"))
   (clobber (match_scratch:SI 2 "=X,&b"))]
  ""
  "@
   stc%M0 %1,%0
   storec %1,%0,%2"
  [(set_attr "type" "store")])

(define_expand "reload_outqi"
  [(parallel [(set (match_operand:QI 0 "symbolic_memory_operand" "=m")
		   (match_operand:QI 1 "" "r"))
	      (clobber (match_operand:SI 2 "" "=&b"))])]
  ""
  "")

;; Finally, the HI instructions.
(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{ rtx op0 = operands[0];

  if (GET_CODE (op0) == MEM && ! reload_in_progress)
    {
      emit_insn (gen_storehi (operands[0], force_reg (HImode, operands[1])));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=b")
	(match_operand:HI 1 "symbolic_memory_operand" "m"))]
  ""
  "loadha %0,%1"
  [(set_attr "type" "load")])


;; use cal16 instead of cal for constant source because combine requires
;; the high bits of the register to be 0 after a HImode load of a constant

(define_insn ""
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,b,Q")
	(match_operand:HI 1 "romp_operand" "r,I,n,s,Q,m,r"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   cas %0,%1,r0
   lis %0,%1
   cal16 %0,%L1(r0)
   get %0,$%1
   lh%N1 %0,%1
   loadh %0,%1
   sth%M0 %1,%0"
  [(set_attr "type" "address,address,address,misc,loadz,loadz,store")
   (set_attr "length" "2,2,4,8,*,*,*")])

(define_insn "storehi"
  [(set (match_operand:HI 0 "memory_operand" "=Q,m")
	(match_operand:HI 1 "register_operand" "r,r"))
   (clobber (match_scratch:SI 2 "=X,&b"))]
  ""
  "@
   sth%M0 %1,%0
   storeh %1,%0,%2"
  [(set_attr "type" "store")])

(define_expand "reload_outhi"
  [(parallel [(set (match_operand:HI 0 "symbolic_memory_operand" "=m")
		   (match_operand:HI 1 "" "r"))
	      (clobber (match_operand:SI 2 "" "=&b"))])]
  ""
  "")

;; For DI move, if we have a constant, break the operation apart into
;; two SImode moves because the optimizer may be able to do a better job
;; with the resulting code.
;;
;; For memory stores, make the required pseudo for a temporary in case we
;; are storing into an absolute address.
;;
;; We need to be careful about the cases where the output is a register that is
;; the second register of the input.

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{ rtx op0 = operands[0];
  rtx op1 = operands[1];
 
  if (CONSTANT_P (op1))
    {
      rtx insns;

      start_sequence ();
      emit_move_insn (operand_subword (op0, 0, 1, DImode),
		      operand_subword (op1, 0, 1, DImode));
      emit_move_insn (operand_subword (op0, 1, 1, DImode),
		      operand_subword (op1, 1, 1, DImode));
      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, op0, op1, 0, op1);
      DONE;
    }

  if (GET_CODE (op0) == MEM && ! reload_in_progress)
    {
      emit_insn (gen_storedi (operands[0], force_reg (DImode, operands[1])));
      DONE;
    }
}")

(define_insn ""
 [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q")
       (match_operand:DI 1 "reg_or_mem_operand" "r,Q,m,r"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)"
  "*
{
  switch (which_alternative)
    {
    case 0:
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return \"cas %O0,%O1,r0\;cas %0,%1,r0\";
      else
	return \"cas %0,%1,r0\;cas %O0,%O1,r0\";
    case 1:
      /* Here we must see which word to load first.  We default to the
	 low-order word unless it occurs in the address.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return \"l%M1 %O0,%O1\;l%M1 %0,%1\";
      else
	return \"l%M1 %0,%1\;l%M1 %O0,%O1\";
    case 2:
      return \"get %O0,$%1\;ls %0,0(%O0)\;ls %O0,4(%O0)\";
    case 3:
      return \"st%M0 %1,%0\;st%M0 %O1,%O0\";
    default:
      abort();
    }
}"
  [(set_attr "type" "multi")
   (set_attr "cc" "change0,change0,change0,none")
   (set_attr "length" "4,12,8,8")])

(define_insn "storedi"
  [(set (match_operand:DI 0 "memory_operand" "=Q,m")
	(match_operand:DI 1 "register_operand" "r,r"))
   (clobber (match_scratch:SI 2 "=X,&b"))]
  ""
  "@
   st%M0 %1,%0\;st%M0 %O1,%O0
   get %2,$%0\;sts %1,0(%2)\;sts %O1,4(%2)"
  [(set_attr "type" "multi,multi")
   (set_attr "cc" "none,none")
   (set_attr "length" "8,12")])

(define_expand "reload_outdi"
  [(parallel [(set (match_operand:DI 0 "symbolic_memory_operand" "=m")
		   (match_operand:DI 1 "" "r"))
	      (clobber (match_operand:SI 2 "" "=&b"))])]
  ""
  "")

;; Split symbolic memory operands differently.  We first load the address
;; into a register and then do the two loads or stores.  We can only do
;; this if operand_subword won't produce a SUBREG, which is only when
;; operands[0] is a hard register.  Thus, these won't be used during the
;; first insn scheduling pass.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "symbolic_memory_operand" ""))]
  "GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
  "
{ operands[2] = operand_subword (operands[0], 1, 0, DImode);
  operands[3] = XEXP (operands[1], 0);
  operands[4] = operand_subword (operands[0], 0, 0, DImode);
  operands[5] = gen_rtx_MEM (SImode, operands[2]);
  operands[6] = operands[2];
  operands[7] = gen_rtx_MEM (SImode, plus_constant (operands[2], 4));

  if (operands[2] == 0 || operands[4] == 0)
    FAIL;
}")

(define_split
  [(set (match_operand:DI 0 "symbolic_memory_operand" "")
	(match_operand:DI 1 "register_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" ""))] 
  "GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
  "
{ operands[3] = XEXP (operands[0], 0);
  operands[4] = gen_rtx_MEM (SImode, operands[2]);
  operands[5] = operand_subword (operands[1], 0, 0, DImode);
  operands[6] = gen_rtx_MEM (SImode, plus_constant (operands[4], 4));
  operands[7] = operand_subword (operands[1], 1, 0, DImode);

  if (operands[5] == 0 || operands[7] == 0)
    FAIL;
}")

;; If the output is a register and the input is memory, we have to be careful
;; and see which word needs to be loaded first.
;;
;; Note that this case doesn't have a CLOBBER.   Therefore, we must either
;; be after reload or operand[0] must not be a MEM.  So we don't need a
;; CLOBBER on the new insns either.
;;
;; Due to a bug in sched.c, we do not want to split this insn if both
;; operands are registers and they overlap unless reload has completed.
(define_split
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  "! symbolic_memory_operand (operands[0], DImode)
   && ! symbolic_memory_operand (operands[1], DImode)
   && ! (GET_CODE (operands[0]) == REG
         && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[1]) == REG
         && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
	 && ! reload_completed
	 && reg_overlap_mentioned_p (operands[0], operands[1]))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{ if (GET_CODE (operands[0]) != REG
      || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			      operands[1], 0))
    {
      operands[2] = operand_subword (operands[0], 0, 0, DImode);
      operands[3] = operand_subword (operands[1], 0, 0, DImode);
      operands[4] = operand_subword (operands[0], 1, 0, DImode);
      operands[5] = operand_subword (operands[1], 1, 0, DImode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 1, 0, DImode);
      operands[3] = operand_subword (operands[1], 1, 0, DImode);
      operands[4] = operand_subword (operands[0], 0, 0, DImode);
      operands[5] = operand_subword (operands[1], 0, 0, DImode);
    }

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;
}")

(define_split
 [(set (match_operand:DI 0 "general_operand" "")
       (match_operand:DI 1 "general_operand" ""))
  (clobber (match_operand:SI 6 "register_operand" ""))]
  "! symbolic_memory_operand (operands[0], DImode)
   && ! symbolic_memory_operand (operands[1], DImode)
   && ! (GET_CODE (operands[0]) == REG
         && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[1]) == REG
         && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
	 && ! reload_completed
	 && reg_overlap_mentioned_p (operands[0], operands[1]))"
 [(parallel [(set (match_dup 2) (match_dup 3))
	     (clobber (match_dup 7))])
  (parallel [(set (match_dup 4) (match_dup 5))
	     (clobber (match_dup 8))])]
 "
{ if (GET_CODE (operands[0]) != REG
      || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			      operands[1], 0))
    {
      operands[2] = operand_subword (operands[0], 0, 0, DImode);
      operands[3] = operand_subword (operands[1], 0, 0, DImode);
      operands[4] = operand_subword (operands[0], 1, 0, DImode);
      operands[5] = operand_subword (operands[1], 1, 0, DImode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 1, 0, DImode);
      operands[3] = operand_subword (operands[1], 1, 0, DImode);
      operands[4] = operand_subword (operands[0], 0, 0, DImode);
      operands[5] = operand_subword (operands[1], 0, 0, DImode);
    }

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;

  /* We must be sure to make two different SCRATCH operands, since they
     are not allowed to be shared.  After reload, however, we only have
     a SCRATCH if we won't use the operand, so it is allowed to share it
     then.  */
  if (reload_completed || GET_CODE (operands[6]) != SCRATCH)
    operands[7] = operands[8] = operands[6];
  else
    {
      operands[7] = gen_rtx_SCRATCH (SImode);
      operands[8] = gen_rtx_SCRATCH (SImode);
    }
}")

;; Define move insns for SF, and DF.
;;
;; For register-register copies or a copy of something to itself, emit a
;; single SET insn since it will likely be optimized away.
;;
;; Otherwise, emit a floating-point move operation unless both input and
;; output are either constant, memory, or a non-floating-point hard register.
(define_expand "movdf"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (match_operand:DF 1 "general_operand" ""))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "
{ rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (op0 == op1)
    {
      emit_insn (gen_rtx_SET (VOIDmode, op0, op1));
      DONE;
    }

  if ((GET_CODE (op0) == MEM
       || (GET_CODE (op0) == REG && REGNO (op0) < FIRST_PSEUDO_REGISTER
	   && ! FP_REGNO_P (REGNO (op0))))
      && (GET_CODE (op1) == MEM
	  || GET_CODE (op1) == CONST_DOUBLE
	  || (GET_CODE (op1) == REG && REGNO (op1) < FIRST_PSEUDO_REGISTER
	      && ! FP_REGNO_P (REGNO (op1)) && ! rtx_equal_p (op0, op1))))
    {
      rtx insns;

      if (GET_CODE (op1) == CONST_DOUBLE)
	op1 = force_const_mem (DFmode, op1);

      start_sequence ();
      if (GET_CODE (operands[0]) != REG
	  || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
				  operands[1], 0))
	{
	  emit_move_insn (operand_subword (op0, 0, 1, DFmode),
			  operand_subword_force (op1, 0, DFmode));
	  emit_move_insn (operand_subword (op0, 1, 1, DFmode),
			  operand_subword_force (op1, 1, DFmode));
	}
      else
	{
	  emit_move_insn (operand_subword (op0, 1, 1, DFmode),
			  operand_subword_force (op1, 1, DFmode));
	  emit_move_insn (operand_subword (op0, 0, 1, DFmode),
			  operand_subword_force (op1, 0, DFmode));
	}

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, op0, op1, 0, op1);
      DONE;
    }
}")

(define_expand "movsf"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (match_operand:SF 1 "general_operand" ""))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "
{ rtx op0 = operands[0];
  rtx op1 = operands[1];
  
  if (op0 == op1)
    {
      emit_insn (gen_rtx_SET (VOIDmode, op0, op1));
      DONE;
    }

  if ((GET_CODE (op0) == MEM
       || (GET_CODE (op0) == REG && REGNO (op0) < FIRST_PSEUDO_REGISTER
	   && ! FP_REGNO_P (REGNO (op0))))
       && (GET_CODE (op1) == MEM
	   || GET_CODE (op1) == CONST_DOUBLE
	   || (GET_CODE (op1) == REG && REGNO (op1) < FIRST_PSEUDO_REGISTER
	       && ! FP_REGNO_P (REGNO (op1)))))
    {
      rtx last;

      if (GET_CODE (op1) == CONST_DOUBLE)
	op1 = force_const_mem (SFmode, op1);

      last = emit_move_insn (operand_subword (op0, 0, 1, SFmode),
			     operand_subword_force (op1, 0, SFmode));

      REG_NOTES (last) = gen_rtx_EXPR_LIST (REG_EQUAL, op1, REG_NOTES (last));
      DONE;
    }
}")

;; Define the move insns for SF and DF.  Check for all general regs
;; in the FP insns and make them non-FP if so.  Do the same if the input and
;; output are the same (the insn will be deleted in this case and we don't
;; want to think there are FP insns when there might not be).
(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=*frg")
	(match_dup 0))]
  ""
  "nopr r0"
  [(set_attr "type" "address")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=r,*fr,r,r,Q,m,frg")
	(match_operand:SF 1 "general_operand" "r,0,Q,m,r,r,frg"))
   (clobber (match_operand:SI 2 "reg_0_operand" "=&z,z,z,z,z,z,z"))
   (clobber (match_operand:SI 3 "reg_15_operand" "=&t,t,t,t,t,t,t"))]
  ""
  "*
{ switch (which_alternative)
    {
    case 0:
      return \"cas %0,%1,r0\";
    case 1:
      return \"nopr r0\";
    case 2:
      return \"l%M1 %0,%1\";
    case 3:
      return \"load %0,%1\";
    case 4:
      return \"st%M0 %1,%0\";
    case 5:
      return \"store %1,%0,%3\";
    default:
      return output_fpop (SET, operands[0], operands[1], 0, insn);
    }
}"
  [(set_attr "type" "address,address,load,load,store,store,fp")
   (set_attr "length" "2,2,*,*,*,*,*")])

(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=*frg")
	(match_dup 0))]
  ""
  "nopr r0"
  [(set_attr "type" "address")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=r,*fr,r,r,Q,m,frg")
	(match_operand:DF 1 "general_operand" "r,0,Q,m,r,r,*frg"))
   (clobber (match_operand:SI 2 "reg_0_operand" "=&z,z,z,z,z,z,z"))
   (clobber (match_operand:SI 3 "reg_15_operand" "=&t,t,t,t,t,t,t"))]
  ""
  "*
{ switch (which_alternative)
    {
    case 0:
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return \"cas %O0,%O1,r0\;cas %0,%1,r0\";
      else
	return \"cas %0,%1,r0\;cas %O0,%O1,r0\";
    case 1:
      return \"nopr r0\";
    case 2:
      /* Here we must see which word to load first.  We default to the
	 low-order word unless it occurs in the address.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return \"l%M1 %O0,%O1\;l%M1 %0,%1\";
      else
	return \"l%M1 %0,%1\;l%M1 %O0,%O1\";
    case 3:
      return \"get %3,$%1\;ls %0,0(%3)\;ls %O0,4(%3)\";
    case 4:
      return \"st%M0 %1,%0\;st%M0 %O1,%O0\";
    case 5:
      return \"get %3,$%0\;sts %1,0(%3)\;sts %O1,4(%3)\";
    default:
      return output_fpop (SET, operands[0], operands[1], 0, insn);
    }
}"
  [(set_attr "type" "address,multi,multi,multi,multi,multi,fp")
   (set_attr "length" "2,4,*,*,*,*,*")])

;; Split all the above cases that involve multiple insns and no floating-point
;; data block.  If before reload, we can make a SCRATCH.  Otherwise, use
;; register 15.

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "symbolic_memory_operand" ""))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  "GET_CODE (operands[0]) == REG && REGNO (operands[0]) < 16"
  [(set (reg:SI 15) (match_dup 2))
   (set (match_dup 3) (match_dup 4))
   (set (match_dup 5) (match_dup 6))]
  "
{ operands[2] = XEXP (operands[1], 0);
  operands[3] = operand_subword (operands[0], 0, 0, DFmode);
  operands[4] = gen_rtx_MEM (SImode, gen_rtx (REG, SImode, 15));
  operands[5] = operand_subword (operands[0], 1, 0, DFmode);
  operands[6] = gen_rtx_MEM (SImode,
			     plus_constant (gen_rtx (REG, SImode, 15), 4));

  if (operands[3] == 0 || operands[5] == 0)
    FAIL;
}")

(define_split
  [(set (match_operand:DF 0 "symbolic_memory_operand" "")
	(match_operand:DF 1 "register_operand" ""))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  "GET_CODE (operands[1]) == REG && REGNO (operands[1]) < 16"
  [(set (reg:SI 15) (match_dup 2))
   (set (match_dup 3) (match_dup 4))
   (set (match_dup 5) (match_dup 6))]
  "
{ operands[2] = XEXP (operands[0], 0);
  operands[3] = gen_rtx_MEM (SImode, gen_rtx (REG, SImode, 15));
  operands[4] = operand_subword (operands[1], 0, 0, DFmode);
  operands[5] = gen_rtx_MEM (SImode,
			     plus_constant (gen_rtx_REG (SImode, 15), 4));
  operands[6] = operand_subword (operands[1], 1, 0, DFmode);

  if (operands[4] == 0 || operands[6] == 0)
    FAIL;
}")

;; If the output is a register and the input is memory, we have to be careful
;; and see which word needs to be loaded first.  We also cannot to the
;; split if the input is a constant because it would result in invalid
;; insns.  When the output is a MEM, we must put a CLOBBER on each of the
;; resulting insn, when it is not a MEM, we must not.
(define_split
  [(set (match_operand:DF 0 "memory_operand" "")
	(match_operand:DF 1 "register_operand" ""))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  "GET_CODE (operands[1]) == REG && REGNO (operands[1]) < 15"
  [(parallel [(set (match_dup 2) (match_dup 3))
	      (clobber (match_dup 6))])
   (parallel [(set (match_dup 4) (match_dup 5))
	      (clobber (match_dup 7))])]
  "
{ operands[2] = operand_subword (operands[0], 0, 0, DFmode);
  operands[3] = operand_subword (operands[1], 0, 0, DFmode);
  operands[4] = operand_subword (operands[0], 1, 0, DFmode);
  operands[5] = operand_subword (operands[1], 1, 0, DFmode);

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;

  if (reload_completed)
    operands[6] = operands[7] = gen_rtx_REG (SImode, 15);
  else
    {
      operands[6] = gen_rtx_SCRATCH (SImode);
      operands[7] = gen_rtx_SCRATCH (SImode);
    }
}")

(define_split
  [(set (match_operand:DF 0 "nonmemory_operand" "")
	(match_operand:DF 1 "general_operand" ""))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  "! symbolic_memory_operand (operands[1], DFmode)
   && GET_CODE (operands[1]) != CONST_DOUBLE
   && (GET_CODE (operands[0]) != REG || REGNO (operands[0]) < 15)
   && (GET_CODE (operands[1]) != REG || REGNO (operands[1]) < 15)
   && (GET_CODE (operands[0]) == REG || GET_CODE (operands[1]) == REG)
   && ! (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
	 && ! reload_completed
	 && reg_overlap_mentioned_p (operands[0], operands[1]))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{ if (GET_CODE (operands[0]) != REG
      || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			      operands[1], 0))
    {
      operands[2] = operand_subword (operands[0], 0, 0, DFmode);
      operands[3] = operand_subword (operands[1], 0, 0, DFmode);
      operands[4] = operand_subword (operands[0], 1, 0, DFmode);
      operands[5] = operand_subword (operands[1], 1, 0, DFmode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 1, 0, DFmode);
      operands[3] = operand_subword (operands[1], 1, 0, DFmode);
      operands[4] = operand_subword (operands[0], 0, 0, DFmode);
      operands[5] = operand_subword (operands[1], 0, 0, DFmode);
    }

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;
}")

;; Conversions from one integer mode to another.
;; It is possible sometimes to sign- or zero-extend while fetching from memory.
;;
;; First, sign-extensions:
(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=b")
	(sign_extend:SI (match_operand:HI 1 "symbolic_memory_operand" "m")))]
  ""
  "loadha %0,%1"
  [(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,b")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,Q,m")))]
  ""
  "@
   exts %0,%1
   lha%M1 %0,%1
   loadha %0,%1"
  [(set_attr "type" "arith,load,load")
   (set_attr "length" "2,*,*")])

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

;; Define peepholes to eliminate an instruction when we are doing a sign
;; extension but cannot clobber the input.
;;
;; In this case we will shift left 24 bits, but need a copy first.  The shift
;; can be replaced by a "mc03" instruction, but this can only be done if
;; followed by the right shift of 24 or more bits.
(define_peephole
  [(set (match_operand:SI 0 "register_operand" "")
	(subreg:SI (match_operand:QI 1 "register_operand" "") 0))
   (set (match_dup 0)
	(ashift:SI (match_dup 0)
		   (const_int 24)))
   (set (match_dup 0)
	(ashiftrt:SI (match_dup 0)
		     (match_operand:SI 2 "const_int_operand" "")))]
  "INTVAL (operands[2]) >= 24"
  "mc03 %0,%1\;sari16 %0,%S2"
 [(set_attr "type" "multi")
  (set_attr "length" "4")
  (set_attr "cc" "sets")])

;; Now zero extensions:
(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=b")
	(zero_extend:SI (match_operand:HI 1 "symbolic_memory_operand" "m")))]
  ""
  "loadh %0,%1"
  [(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,b")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,Q,m")))]
  ""
  "@
   nilz %0,%1,65535
   lh%N1 %0,%1
   loadh %0,%1"
  [(set_attr "type" "arith,loadz,load")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=b")
	(zero_extend:SI (match_operand:QI 1 "symbolic_memory_operand" "m")))]
  ""
  "loadc %0,%1"
  [(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,b")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,Q,m")))]
  ""
  "@
   nilz %0,%1,255
   lc%M1 %0,%1
   loadc %0,%1"
  [(set_attr "type" "arith,load,load")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=b")
	(zero_extend:HI (match_operand:QI 1 "symbolic_memory_operand" "m")))]
  ""
  "loadc %0,%1"
  [(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r,r,b")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,Q,m")))]
  ""
  "@
   nilz %0,%1,255
   lc%M1 %0,%1
   loadc %0,%1"
  [(set_attr "type" "arith,load,load")])

;; Various extract and insertion operations.
(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 8)
    FAIL;

  if (GET_CODE (operands[3]) != CONST_INT)
    FAIL;

  if (INTVAL (operands[3]) != 0 && INTVAL (operands[3]) != 8
      && INTVAL (operands[3]) != 16 && INTVAL (operands[3]) != 24)
    FAIL;
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (match_operand:SI 2 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) & 7) == 0"
  "lis %0,0\;mc3%B2 %0,%1"
  [(set_attr "type" "multi")
   (set_attr "cc" "change0")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (match_operand:SI 2 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) & 7) == 0"
  [(set (match_dup 0) (const_int 0))
   (set (zero_extract:SI (match_dup 0) (const_int 8) (const_int 24))
	(zero_extract:SI (match_dup 1) (const_int 8) (match_dup 2)))]
  "")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (const_int 24))
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 8)
			 (match_operand:SI 2 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) & 7) == 0"
  "mc3%B2 %0,%1"
  [(set_attr "type" "address")
   (set_attr "length" "2")])

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			 (match_operand:SI 1 "const_int_operand" "")
			 (match_operand:SI 2 "const_int_operand" ""))
	(match_operand:SI 3 "register_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    FAIL;

  if (GET_CODE (operands[1]) != CONST_INT)
    FAIL;

  if (INTVAL (operands[1]) == 1)
    {
      emit_insn (gen_bit_insv (operands[0], operands[1], operands[2],
			       operands[3]));
      DONE;
    }
  else if (INTVAL (operands[1]) == 8
	   && (INTVAL (operands[2]) % 8 == 0))
    ;				/* Accept aligned byte-wide field. */
  else
    FAIL;
}")

;; For a single-bit insert, it is better to explicitly generate references
;; to the T bit.  We will call the T bit "CC0" because it can be clobbered
;; by some CC0 sets (single-bit tests).

(define_expand "bit_insv"
  [(set (cc0)
	(zero_extract:SI (match_operand:SI 3 "register_operand" "")
			 (const_int 1)
			 (const_int 31)))
   (parallel [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
				    (match_operand:SI 1 "const_int_operand" "")
				    (match_operand:SI 2 "const_int_operand" ""))
		   (ne (cc0) (const_int 0)))
	      (clobber (match_scratch:SI 4 ""))])]
  ""
  "")
	
(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 8)
			 (match_operand:SI 1 "const_int_operand" "n"))
	(match_operand:SI 2 "register_operand" "r"))]
  "(INTVAL (operands[1]) & 7) == 0"
  "mc%B1%.3 %0,%2"
  [(set_attr "type" "address")
   (set_attr "length" "2")])

;; This pattern cannot have any input reloads since if references CC0.
;; So we have to add code to support memory, which is the only other
;; thing that a "register_operand" can become.  There is still a problem
;; if the address isn't valid and *it* needs a reload, but there is no
;; way to solve that problem, so let's hope it never happens.

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r,m")
			 (const_int 1)
			 (match_operand:SI 1 "const_int_operand" "n,m"))
	(ne (cc0) (const_int 0)))
   (clobber (match_scratch:SI 2 "=X,b"))]
  ""
  "@
   mftbi%t1 %0,%S1
   l%M0 %2,%0\;mftb%t1 %2,%S1\;st%M0 %2,%0"
  [(set_attr "type" "*,multi")
   (set_attr "cc" "none,none")
   (set_attr "length" "2,10")])

;; Arithmetic instructions.  First, add and subtract.
;;
;; It may be that the second input is either large or small enough that
;; the operation cannot be done in a single insn.  In that case, emit two.
(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (INTVAL (operands[2]) + 0x8000) >= 0x10000
      && (INTVAL (operands[2]) & 0xffff) != 0)
    {
      int low = INTVAL (operands[2]) & 0xffff;
      int high = (unsigned) INTVAL (operands[2]) >> 16;

      if (low & 0x8000)
	high++, low |= 0xffff0000;

      emit_insn (gen_addsi3 (operands[0], operands[1], GEN_INT (high << 16)));
      operands[1] = operands[0];
      operands[2] = GEN_INT (low);
    }
}")

;; Put the insn to add a symbolic constant to a register separately to
;; improve register allocation since it has different register requirements.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=b")
	(plus:SI (match_operand:SI 1 "register_operand" "%b")
		 (match_operand:SI 2 "romp_symbolic_operand" "s")))]
   ""
   "get %0,$%2(%1)"
   [(set_attr "type" "address")
    (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r,r,r,b")
	(plus:SI (match_operand:SI 1 "reg_or_add_operand" "%0,0,r,b,0,r,b")
		 (match_operand:SI 2 "reg_or_add_operand" "I,J,K,M,r,b,s")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   ais %0,%2
   sis %0,%n2
   ail %0,%1,%2
   cau %0,%H2(%1)
   a %0,%2
   cas %0,%1,%2
   get %0,$%2(%1)"
  [(set_attr "type" "arith,arith,arith,address,arith,address,misc")
   (set_attr "length" "2,2,4,4,2,2,8")])

;; Now subtract.
;;
;; 1.	If third operand is constant integer, convert it to add of the negative
;;	of that integer.
;; 2.	If the second operand is not a valid constant integer, force it into a
;;	register.
(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_any_cint_operand" "")
		  (match_operand:SI 2 "reg_or_any_cint_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands [2]) == CONST_INT)
    {
      emit_insn (gen_addsi3 (operands[0], operands[1], 
			     GEN_INT (- INTVAL (operands[2]))));
      DONE;
    }
  else
    operands[2] = force_reg (SImode, operands[2]);

  if (GET_CODE (operands[1]) != CONST_INT
      || (unsigned) (INTVAL (operands[1]) + 0x8000) >= 0x10000)
    operands[1] = force_reg (SImode, operands[1]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(minus:SI (match_operand:SI 1 "reg_or_D_operand" "K,0,r")
		  (match_operand:SI 2 "register_operand" "r,r,0")))]
  ""
  "@
   sfi %0,%2,%1
   s %0,%2
   sf %0,%1"
  [(set_attr "length" "4,2,2")])

;; Multiply either calls a special RT routine or is done in-line, depending
;; on the value of a -m flag.
;;
;; First define the way we call the subroutine.
(define_expand "mulsi3_subr"
  [(set (reg:SI 2) (match_operand:SI 1 "register_operand" ""))
   (set (reg:SI 3) (match_operand:SI 2 "register_operand" ""))
   (parallel [(set (reg:SI 2) (mult:SI (reg:SI 2) (reg:SI 3)))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])
   (set (match_operand:SI 0 "register_operand" "")
	(reg:SI 2))]
  ""
  "")

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
  "
{
  if (! TARGET_IN_LINE_MUL)
    {
      emit_insn (gen_mulsi3_subr (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

;; Define the patterns to match.
;; We would like to provide a delay slot for the insns that call internal
;; routines, but doing so is risky since reorg will think that the use of
;; r2 and r3 is completed in the insn needing the delay slot.  Also, it
;; won't know that the cc will be clobbered.  So take the safe approach
;; and don't give them delay slots.
(define_insn ""
  [(set (reg:SI 2)
	(mult:SI (reg:SI 2) (reg:SI 3)))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  "! TARGET_IN_LINE_MUL"
  "bali%# r15,lmul$$"
  [(set_attr "type" "misc")
   (set_attr "in_delay_slot" "no")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_IN_LINE_MUL"
  "*
{ return output_in_line_mul (); }"
  [(set_attr "length" "38")
   (set_attr "type" "multi")])

;; Handle divide and modulus.  The same function returns both values,
;; so use divmodsi4.  This divides arg 1 by arg 2 with quotient to go
;; into arg 0 and remainder in arg 3.
;;
;; We want to put REG_EQUAL notes for the two outputs.  So we need a
;; function to do everything else.
(define_expand "divmodsi4_doit"
  [(set (reg:SI 2)
	(match_operand:SI 0 "register_operand" ""))
   (set (reg:SI 3)
	(match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:SI 2) (div:SI (reg:SI 2) (reg:SI 3)))
	      (set (reg:SI 3) (mod:SI (reg:SI 2) (reg:SI 3)))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "divmodsi4"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (div:SI (match_operand:SI 1 "register_operand" "")
			   (match_operand:SI 2 "register_operand" "")))
	      (set (match_operand:SI 3 "register_operand" "")
		   (mod:SI (match_dup 1) (match_dup 2)))])]
  ""
  "
{
  rtx insn;

  emit_insn (gen_divmodsi4_doit (operands[1], operands[2]));
  insn = emit_move_insn (operands[0], gen_rtx_REG (SImode, 2));
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL,
					gen_rtx_DIV (SImode, operands[1],
						     operands[2]),
					REG_NOTES (insn));
  insn = emit_move_insn (operands[3], gen_rtx_REG (SImode, 3));
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL,
					gen_rtx_MOD (SImode, operands[1],
						     operands[2]),
					REG_NOTES (insn));
  DONE;
}")

(define_insn ""
  [(set (reg:SI 2)
	(div:SI (reg:SI 2) (reg:SI 3)))
   (set (reg:SI 3)
	(mod:SI (reg:SI 2) (reg:SI 3)))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  ""
  "bali%# r15,ldiv$$"
  [(set_attr "type" "misc")
   (set_attr "in_delay_slot" "no")])

;; Similarly for unsigned divide.
(define_expand "udivmodsi4_doit"
  [(set (reg:SI 2)
	(match_operand:SI 0 "register_operand" ""))
   (set (reg:SI 3)
	(match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:SI 2) (udiv:SI (reg:SI 2) (reg:SI 3)))
	      (set (reg:SI 3) (umod:SI (reg:SI 2) (reg:SI 3)))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "udivmodsi4"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (udiv:SI (match_operand:SI 1 "register_operand" "")
			    (match_operand:SI 2 "register_operand" "")))
	      (set (match_operand:SI 3 "register_operand" "")
		   (umod:SI (match_dup 1) (match_dup 2)))])]
  ""
  "
{
  rtx insn;

  emit_insn (gen_udivmodsi4_doit (operands[1], operands[2]));
  insn = emit_move_insn (operands[0], gen_rtx_REG (SImode, 2));
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL,
					gen_rtx_UDIV (SImode, operands[1],
						      operands[2]),
					REG_NOTES (insn));
  insn = emit_move_insn (operands[3], gen_rtx_REG (SImode, 3));
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL,
					gen_rtx_UMOD (SImode, operands[1],
						      operands[2]),
					REG_NOTES (insn));
  DONE;
}")

(define_insn ""
  [(set (reg:SI 2)
	(udiv:SI (reg:SI 2) (reg:SI 3)))
   (set (reg:SI 3)
	(umod:SI (reg:SI 2) (reg:SI 3)))
   (clobber (reg:SI 0))
   (clobber (reg:SI 15))]
  ""
  "bali%# r15,uldiv$$"
  [(set_attr "type" "misc")
   (set_attr "in_delay_slot" "no")])

;; Define DImode arithmetic operations.
;;
;; It is possible to do certain adds and subtracts with constants in a single
;; insn, but it doesn't seem worth the trouble.
;;
;; Don't use DEFINE_SPLIT on these because the dependency on CC can't be
;; easily tracked in that case!
(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "r")))]
  ""
  "a %O0,%O2\;ae %0,%2"
  [(set_attr "type" "multi")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		  (match_operand:DI 2 "register_operand" "r")))]
  ""
  "s %O0,%O2\;se %0,%2"
  [(set_attr "type" "multi")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r,&r")
	(neg:DI (match_operand:DI 1 "register_operand" "0,r")))]
  ""
  "twoc %O0,%O1\;onec %0,%1\;aei %0,%0,0"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

;; Unary arithmetic operations.
(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "abs %0,%1"
  [(set_attr "length" "2")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "twoc %0,%1"
  [(set_attr "length" "2")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "onec %0,%1"
  [(set_attr "length" "2")])


;; Logical insns: AND, IOR, and XOR
;;
;; If the operation is being performed on a 32-bit constant such that
;; it cannot be done in one insn, do it in two.  We may lose a bit on
;; CSE in pathological cases, but it seems better doing it this way.
(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "reg_or_any_cint_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int top = (unsigned) INTVAL (operands[2]) >> 16;
      int bottom = INTVAL (operands[2]) & 0xffff;

      if (top != 0 && top != 0xffff && bottom != 0 && bottom != 0xffff)
	{
	  emit_insn (gen_andsi3 (operands[0], operands[1],
				 GEN_INT ((top << 16) | 0xffff)));
	  operands[1] = operands[0];
	  operands[2] = GEN_INT (0xffff0000 | bottom);
	}
    }
}");

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(and:SI (match_operand:SI 1 "reg_or_and_operand" "%0,r,0")
		(match_operand:SI 2 "reg_or_and_operand" "P,LMO,r")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   clrb%k2 %0,%b2
   ni%z2 %0,%1,%Z2
   n %0,%2"
  [(set_attr "length" "2,4,2")])

;; logical OR (IOR)
(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "reg_or_any_cint_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int top = (unsigned) INTVAL (operands[2]) >> 16;
      int bottom = INTVAL (operands[2]) & 0xffff;

      if (top != 0 && bottom != 0)
	{
	  emit_insn (gen_iorsi3 (operands[0], operands[1],
				 GEN_INT (top << 16)));
	  operands[1] = operands[0];
	  operands[2] = GEN_INT (bottom);
	}
    }
}");

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(ior:SI (match_operand:SI 1 "reg_or_cint_operand" "%0,r,0")
		(match_operand:SI 2 "reg_or_cint_operand" "N,LM,r")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   setb%h2 %0,%b2
   oi%h2 %0,%1,%H2
   o %0,%2"
  [(set_attr "length" "2,4,2")])

;; exclusive-or (XOR)
(define_expand "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "reg_or_any_cint_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int top = (unsigned) INTVAL (operands[2]) >> 16;
      int bottom = INTVAL (operands[2]) & 0xffff;

      if (top == 0xffff && bottom == 0xffff)
	{
	  emit_insn (gen_one_cmplsi2 (operands[0], operands[1]));
	  DONE;
	}
      else if (top != 0 && bottom != 0)
	{
	  emit_insn (gen_xorsi3 (operands[0], operands[1],
				 GEN_INT (top << 16)));
	  operands[1] = operands[0];
	  operands[2] = GEN_INT (bottom);
	}
    }
}");

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "reg_or_cint_operand" "%r,0")
		(match_operand:SI 2 "reg_or_cint_operand" "LM,r")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
   xi%h2 %0,%1,%H2
   x %0,%2"
  [(set_attr "length" "4,2")])

;; Various shift insns
(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "reg_or_cint_operand" "r,n")))]
  ""
  "@
   sar %0,%2
   sari%s2 %0,%S2"
  [(set_attr "length" "2")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "reg_or_cint_operand" "r,n")))]
  ""
  "@
   sr %0,%2
   sri%s2 %0,%S2"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "b")
		   (const_int 1)))]
  ""
  "cas %0,%1,%1"
  [(set_attr "length" "2")
   (set_attr "type" "address")])

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "reg_or_cint_operand" "r,n")))]
  ""
  "@
   sl %0,%2
   sli%s2 %0,%S2"
  [(set_attr "length" "2")])

;; Function call insns:
;;
;; On the ROMP, &fcn is actually a pointer to the data area, which is passed
;; to the function in r0.  &.fcn is the actual starting address of the
;; function.  Also, the word at &fcn contains &.fcn.
;;
;; For both functions that do and don't return values, there are two cases:
;; where the function's address is a constant, and where it isn't.
;;
;; Operand 1 (2 for `call_value') is the number of arguments and is not used.
(define_expand "call"
  [(use (match_operand:SI 0 "address_operand" ""))
   (use (match_operand 1 "" ""))]
  ""
  "
{
  rtx reg0 = gen_rtx_REG (SImode, 0);
  rtx call_insn;

  if (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != CONST_INT)
    abort();

  operands[0] = XEXP (operands[0], 0);
  if (GET_CODE (operands[0]) == SYMBOL_REF)
    {
      extern rtx get_symref ();
      char *real_fcnname
	= (char *) alloca (strlen (XSTR (operands[0], 0)) + 2);

      /* Copy the data area address to r0.  */
      emit_move_insn (reg0, force_reg (SImode, operands[0]));
      strcpy (real_fcnname, \".\");
      strcat (real_fcnname, XSTR (operands[0], 0));
      operands[0] = get_symref (real_fcnname);
    }
  else
    {
      rtx data_access;

      emit_move_insn (reg0, force_reg (SImode, operands[0]));
      data_access = gen_rtx_MEM (SImode, operands[0]);
      RTX_UNCHANGING_P (data_access) = 1;
      operands[0] = copy_to_reg (data_access);
    }

  call_insn = emit_call_insn (gen_call_internal (operands[0], operands[1]));
  use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), reg0);
  DONE;
}")

(define_insn "call_internal"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "b"))
	 (match_operand 1 "" "g"))
   (clobber (reg:SI 15))]
  ""
  "balr%# r15,%0"
  [(set_attr "type" "call")
   (set_attr "length" "2")])

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "romp_symbolic_operand" "i"))
	 (match_operand 1 "" "g"))
   (clobber (reg:SI 15))]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "bali%# r15,%0"
  [(set_attr "type" "call")])

;; Call a function and return a value.
(define_expand "call_value"
  [(use (match_operand 0 "" ""))
   (use (match_operand:SI 1 "address_operand" ""))
   (use (match_operand 2 "" ""))]
  ""
  "
{
  rtx reg0 = gen_rtx_REG (SImode, 0);
  rtx call_insn;

  if (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != CONST_INT)
    abort();

  operands[1] = XEXP (operands[1], 0);
  if (GET_CODE (operands[1]) == SYMBOL_REF)
    {
      extern rtx get_symref ();
      char *real_fcnname =
		(char *) alloca (strlen (XSTR (operands[1], 0)) + 2);

      /* Copy the data area address to r0.  */
      emit_move_insn (reg0,force_reg (SImode, operands[1]));
      strcpy (real_fcnname, \".\");
      strcat (real_fcnname, XSTR (operands[1], 0));
      operands[1] = get_symref (real_fcnname);
    }
  else
    {
      rtx data_access;

      emit_move_insn (reg0,force_reg (SImode, operands[1]));
      data_access = gen_rtx_MEM (SImode, operands[1]);
      RTX_UNCHANGING_P (data_access) = 1;
      operands[1] = copy_to_reg (data_access);
    }

  call_insn = emit_call_insn (gen_call_value_internal (operands[0],
						       operands[1],
						       operands[2]));
  use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), reg0);
  DONE;
}")

(define_insn "call_value_internal"
  [(set (match_operand 0 "" "=fg")
	(call (mem:SI (match_operand:SI 1 "register_operand" "b"))
	      (match_operand 2 "" "g")))
   (clobber (reg:SI 15))]
  ""
  "balr%# r15,%1"
  [(set_attr "length" "2")
   (set_attr "type" "call")])

(define_insn ""
  [(set (match_operand 0 "" "=fg")
	(call (mem:SI (match_operand:SI 1 "romp_symbolic_operand" "i"))
	      (match_operand 2 "" "g")))
   (clobber (reg:SI 15))]
  "GET_CODE (operands[1]) == SYMBOL_REF"
  "bali%# r15,%1"
  [(set_attr "type" "call")])

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

;; No operation insn.
(define_insn "nop"
  [(const_int 0)]
  ""
  "nopr r0"
  [(set_attr "type" "address")
   (set_attr "length" "2")
   (set_attr "cc" "none")])

;; Here are the floating-point operations.
;;
;; Start by providing DEFINE_EXPAND for each operation.
;; The insns will be handled with MATCH_OPERATOR; the methodology will be
;; discussed below.

;; First the conversion operations.

(define_expand "truncdfsf2"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (float_truncate:SF (match_operand:DF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "extendsfdf2"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (float_extend:DF (match_operand:SF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "floatsisf2"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (float:SF (match_operand:SI 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "floatsidf2"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (float:DF (match_operand:SI 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "fix_truncsfsi2"
  [(parallel [(set (match_operand:SI 0 "general_operand" "")
		   (fix:SI (match_operand:SF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "fix_truncdfsi2"
  [(parallel [(set (match_operand:SI 0 "general_operand" "")
		   (fix:SI (match_operand:DF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

;; Now the binary operations.

(define_expand "addsf3"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (plus:SF (match_operand:SF 1 "general_operand" "")
			    (match_operand:SF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "adddf3"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (plus:DF (match_operand:DF 1 "general_operand" "")
			    (match_operand:DF 2 "general_operand" "")))
	       (clobber (reg:SI 0))
	       (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "subsf3"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (minus:SF (match_operand:SF 1 "general_operand" "")
			     (match_operand:SF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "subdf3"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (minus:DF (match_operand:DF 1 "general_operand" "")
			     (match_operand:DF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "mulsf3"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (mult:SF (match_operand:SF 1 "general_operand" "")
			    (match_operand:SF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "muldf3"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (mult:DF (match_operand:DF 1 "general_operand" "")
			    (match_operand:DF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "divsf3"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (div:SF (match_operand:SF 1 "general_operand" "")
			   (match_operand:SF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "divdf3"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (div:DF (match_operand:DF 1 "general_operand" "")
			   (match_operand:DF 2 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

;; Unary floating-point operations.
;;
;; Negations can be done without floating-point, since this is IEEE.
;; But we cannot do this if an operand is a hard FP register, since
;; the SUBREG we create would not be valid.
(define_expand "negsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(neg:SF (match_operand:SF 1 "register_operand" "")))]
  ""
  "
{
  if (! (GET_CODE (operands[0]) == REG
	 && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
	 && FP_REGNO_P (REGNO (operands[0])))
      && ! (GET_CODE (operands[1]) == REG
	    && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
	    && FP_REGNO_P (REGNO (operands[1]))))
    {
      rtx result;
      rtx target = operand_subword (operands[0], 0, 1, SFmode);

      result = expand_binop (SImode, xor_optab,
			     operand_subword_force (operands[1], 0, SFmode),
			     GEN_INT (0x80000000), target, 0, OPTAB_WIDEN);
      if (result == 0)
	abort ();

      if (result != target)
	emit_move_insn (result, target);

      /* Make a place for REG_EQUAL.  */
      emit_move_insn (operands[0], operands[0]);
      DONE;
    }
}")

(define_expand "negdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(neg:DF (match_operand:DF 1 "register_operand" "")))]
  ""
  "
{
  if (! (GET_CODE (operands[0]) == REG
	 && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
	 && FP_REGNO_P (REGNO (operands[0])))
      && ! (GET_CODE (operands[1]) == REG
	    && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
	    && FP_REGNO_P (REGNO (operands[1]))))
    {
      rtx result;
      rtx target = operand_subword (operands[0], 0, 1, DFmode);
      rtx insns;

      start_sequence ();
      result = expand_binop (SImode, xor_optab,
			     operand_subword_force (operands[1], 0, DFmode),
			     GEN_INT (0x80000000), target, 0, OPTAB_WIDEN);
      if (result == 0)
	abort ();

      if (result != target)
	emit_move_insn (result, target);
  
      emit_move_insn (operand_subword (operands[0], 1, 1, DFmode),
		      operand_subword_force (operands[1], 1, DFmode));

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, operands[0], operands[1], 0, 0);
      DONE;
    }
}")

(define_expand "abssf2"
  [(parallel [(set (match_operand:SF 0 "general_operand" "")
		   (abs:SF (match_operand:SF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "absdf2"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (abs:DF (match_operand:DF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

;; Any floating-point operation can be either SFmode or DFmode, and each
;; operand (including the output) can be either a normal operand or a
;; conversion from a normal operand.
;;
;; We use MATCH_OPERATOR to match a floating-point binary or unary operator
;; and input and output conversions.  So we need 2^N patterns for each type
;; of operation, where N is the number of operands, including the output.
;; There are thus a total of 14 patterns, 8 for binary operations, 4 for
;; unary operations and two for conversion/move operations (only one
;; operand can have a conversion for move operations).  In addition, we have
;; to be careful that a floating-point reload register doesn't get allocated
;; for an integer.  We take care of this for inputs with PREFERRED_RELOAD_CLASS
;; but need to have two different constraints for outputs.  This means that
;; we have to duplicate each pattern where the output could be an integer.
;; This adds another 7 patterns, for a total of 21.

;; Start with conversion operations (moves are done above).

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operand 2 "general_operand" "frg")]))
   (clobber (match_operand:SI 3 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 4 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (SET, operands[0], operands[2], 0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operand 2 "general_operand" "frg")]))
   (clobber (match_operand:SI 3 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 4 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (SET, operands[0], operands[2], 0, insn);
}"
  [(set_attr "type" "fp")])

;; Next, binary floating-point operations.

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_binary"
		[(match_operand 2 "general_operand" "frg")
		 (match_operand 3 "general_operand" "frg")]))
   (clobber (match_operand:SI 4 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 5 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[2], operands[3])"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], 
		      operands[2], operands[3], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_binary"
		[(match_operand 2 "general_operand" "frg")
		 (match_operator 3 "float_conversion"
			[(match_operand 4 "general_operand" "frg")])]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[2], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], 
		      operands[2], operands[4], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_binary"
		[(match_operator 2 "float_conversion"
			[(match_operand 3 "general_operand" "frg")])
		 (match_operand 4 "general_operand" "frg")]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[3], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], 
		      operands[3], operands[4], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_binary"
		[(match_operator 2 "float_conversion"
			[(match_operand 3 "general_operand" "frg")])
		 (match_operator 4 "float_conversion"
			[(match_operand 5 "general_operand" "frg")])]))
   (clobber (match_operand:SI 6 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 7 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[3], operands[5])"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], 
		      operands[3], operands[5], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operand 3 "general_operand" "frg")
			 (match_operand 4 "general_operand" "frg")])]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[3], operands[4], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operand 3 "general_operand" "frg")
			 (match_operand 4 "general_operand" "frg")])]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[3], operands[4], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operand 3 "general_operand" "frg")
			 (match_operator 4 "float_conversion"
			 	[(match_operand 5 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 6 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 7 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[3], operands[5], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operand 3 "general_operand" "frg")
			 (match_operator 4 "float_conversion"
			 	[(match_operand 5 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 6 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 7 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], operands[4])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[3], operands[5], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])
			 (match_operand 5 "general_operand" "frg")])]))
   (clobber (match_operand:SI 6 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 7 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], operands[5])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[4], operands[5], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])
			 (match_operand 5 "general_operand" "frg")])]))
   (clobber (match_operand:SI 6 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 7 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], operands[5])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[4], operands[5], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])
			 (match_operator 5 "float_conversion"
			 	[(match_operand 6 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 7 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 8 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], operands[6])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[4], operands[6], insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_binary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])
			 (match_operator 5 "float_conversion"
			 	[(match_operand 6 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 7 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 8 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], operands[6])"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], 
		      operands[4], operands[6], insn);
}"
  [(set_attr "type" "fp")])

;; Unary floating-point operations.

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_unary"
		[(match_operand 2 "general_operand" "frg")]))
   (clobber (match_operand:SI 3 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 4 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[2], 0)"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], operands[2],
		      0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_unary"
		[(match_operator 2 "float_conversion"
			[(match_operand 3 "general_operand" "frg")])]))
   (clobber (match_operand:SI 4 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 5 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[1]), operands[3], 0)"
  "*
{ return output_fpop (GET_CODE (operands[1]), operands[0], operands[3],
		      0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_unary"
			[(match_operand 3 "general_operand" "frg")])]))
   (clobber (match_operand:SI 4 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 5 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], 0)"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], operands[3],
		      0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_unary"
			[(match_operand 3 "general_operand" "frg")])]))
   (clobber (match_operand:SI 4 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 5 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[3], 0)"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], operands[3],
		      0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_unary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], 0)"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], operands[4],
		      0, insn);
}"
  [(set_attr "type" "fp")])

(define_insn ""
  [(set (match_operand 0 "general_operand" "=frg")
	(match_operator 1 "float_conversion"
		[(match_operator 2 "float_unary"
			[(match_operator 3 "float_conversion"
			 	[(match_operand 4 "general_operand" "frg")])])]))
   (clobber (match_operand:SI 5 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 6 "reg_15_operand" "=&t"))]
  "check_precision (GET_MODE (operands[2]), operands[4], 0)"
  "*
{ return output_fpop (GET_CODE (operands[2]), operands[0], operands[4],
		      0, insn);
}"
  [(set_attr "type" "fp")])

;; Compare insns are next.  Note that the ROMP has two types of compares,
;; signed & unsigned, and one type of branch.  Use the routine
;; `next_insn_tests_no_unsigned' to see which type to use.
(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "")

(define_expand "cmpsi"
  [(set (cc0)
        (compare (match_operand:SI 0 "register_operand" "")
  		 (match_operand:SI 1 "reg_or_cint_operand" "")))]
  ""
  "")

;; Signed compare, `test' first.

(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  "next_insn_tests_no_unsigned (insn)"
  "cis %0,0"
  [(set_attr "length" "2")
   (set_attr "type" "compare")])

(define_insn ""
  [(set (cc0) (match_operand:SI 0 "register_operand" "r,r,r"))
   (set (match_operand:SI 1 "reg_or_nonsymb_mem_operand" "=0,r,Q")
	(match_dup 0))]
  "next_insn_tests_no_unsigned (insn)"
  "@
   cis %1,0
   nilo %1,%0,65535
   st%M1 %0,%1\;cis %0,0"
  [(set_attr "type" "compare,compare,store")
   (set_attr "length" "2,4,6")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0)
        (compare (match_operand:SI 0 "register_operand" "r,r,r")
		 (match_operand:SI 1 "reg_or_cint_operand" "I,K,r")))]
  "next_insn_tests_no_unsigned (insn)"
  "@
   cis %0,%1
   cil %0,%1
   c %0,%1"
  [(set_attr "length" "2,4,2")
   (set_attr "type" "compare")])

;; Unsigned comparisons, `test' first, again.
(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  "! next_insn_tests_no_unsigned (insn)"
  "clil %0,0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (cc0)
        (compare (match_operand:SI 0 "register_operand" "r,r")
		 (match_operand:SI 1 "reg_or_cint_operand" "K,r")))]
  "! next_insn_tests_no_unsigned (insn)"
  "@
   clil %0,%1
   cl %0,%1"
  [(set_attr "length" "4,2")
   (set_attr "type" "compare")])

;; Bit test insn.  Many cases are converted into this by combine.  This
;; uses the ROMP test bit.

(define_insn ""
  [(set (cc0)
	(zero_extract (match_operand:SI 0 "register_operand" "r,r")
		      (const_int 1)
		      (match_operand:SI 1 "reg_or_any_cint_operand" "r,n")))]
  "next_insn_tests_no_inequality (insn)"
  "@
   mttb %0,%1
   mttbi%t1 %0,%S1"
  [(set_attr "length" "2")
   (set_attr "type" "compare")
   (set_attr "cc" "tbit")])

;; Floating-point comparisons.  There are two, equality and order.
;; The difference will be that a trap for NaN will be given on the orderr
;; comparisons only.

(define_expand "cmpsf"
  [(parallel [(set (cc0) (compare (match_operand:SF 0 "general_operand" "")
				  (match_operand:SF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "cmpdf"
  [(parallel [(set (cc0) (compare (match_operand:DF 0 "general_operand" "")
				  (match_operand:DF 1 "general_operand" "")))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "tstsf"
  [(parallel [(set (cc0) (match_operand:SF 0 "general_operand" ""))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

(define_expand "tstdf"
  [(parallel [(set (cc0) (match_operand:DF 0 "general_operand" ""))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 15))])]
  ""
  "")

;; There are four cases for compare and two for test.  These correspond
;; to each input having a floating-point conversion or not.

(define_insn ""
  [(set (cc0) (compare (match_operand 0 "general_operand" "frg")
		       (match_operand 1 "general_operand" "frg")))
   (clobber (match_operand:SI 2 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 3 "reg_15_operand" "=&t"))]
  "GET_MODE (operands[1]) == SFmode || GET_MODE (operands[1]) == DFmode"
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[0], operands[1], 0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0) (compare (match_operand 0 "general_operand" "frg")
		       (match_operator 1 "float_conversion"
			      [(match_operand 2 "general_operand" "frg")])))
   (clobber (match_operand:SI 3 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 4 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[0], operands[2], 0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0) (compare (match_operator 0 "float_conversion"
			      [(match_operand 1 "general_operand" "frg")])
		       (match_operand 2 "general_operand" "frg")))
   (clobber (match_operand:SI 3 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 4 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[1], operands[2], 0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0) (compare (match_operator 0 "float_conversion"
			      [(match_operand 1 "general_operand" "frg")])
		       (match_operator 2 "float_conversion"
			      [(match_operand 3 "general_operand" "frg")])))
   (clobber (match_operand:SI 4 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 5 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[1], operands[3], 0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0) (match_operand 0 "general_operand" "frg"))
   (clobber (match_operand:SI 1 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 2 "reg_15_operand" "=&t"))]
  "GET_MODE (operands[0]) == SFmode || GET_MODE (operands[0]) == DFmode"
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[0], CONST0_RTX (GET_MODE (operands[0])),
		      0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

(define_insn ""
  [(set (cc0) (match_operator 0 "float_conversion"
			[(match_operand 1 "general_operand" "frg")]))
   (clobber (match_operand:SI 2 "reg_0_operand" "=&z"))
   (clobber (match_operand:SI 3 "reg_15_operand" "=&t"))]
  ""
  "*
{ return output_fpop (next_insn_tests_no_inequality (insn) ? EQ : GE,
		      operands[1], CONST0_RTX (GET_MODE (operands[1])),
		      0, insn);
}"
  [(set_attr "type" "fp")
   (set_attr "cc" "compare")])

;; Branch insns.  Unsigned vs. signed have already
;; been taken care of.  The only insns that need to be concerned about the
;; test bit are beq and bne because the rest are either always true,
;; always false, or converted to EQ or NE.

;; For conditional branches, we use `define_expand' and just have two patterns
;; that match them.  Operand printing does most of the work.

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")

;; Define both directions of branch and return.

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (restore_compare_p (operands[1]))
    return 0;
  else if (get_attr_length (insn) == 2)
    return \"j%j1 %l0\";
  else
    return \"b%j1%# %l0\";
}"
  [(set_attr "type" "branch")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (return)
		      (pc)))]
  "null_epilogue ()"
  "*
{
  if (restore_compare_p (operands[0]))
    return 0;
  else
    return \"b%j0r%# r15\";
}"
  [(set_attr "type" "return")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				[(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  if (restore_compare_p (operands[1]))
    return 0;
  else if (get_attr_length (insn) == 2)
    return \"j%J1 %l0\";
  else
    return \"b%J1%# %l0\";
}"
  [(set_attr "type" "branch")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (pc)
		      (return)))]
  "null_epilogue ()"
  "*
{
  if (restore_compare_p (operands[0]))
    return 0;
  else
    return \"b%J0r%# r15\";
}"
  [(set_attr "type" "return")])

;; Unconditional branch and return.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (get_attr_length (insn) == 2)
    return \"j %l0\";
  else
    return \"b%# %l0\";
}"
  [(set_attr "type" "branch")])

(define_insn "return"
  [(return)]
  "null_epilogue ()"
  "br%# r15"
  [(set_attr "type" "return")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "br%# %0"
  [(set_attr "type" "ibranch")])

;; Table jump for switch statements:
(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "br%# %0"
  [(set_attr "type" "ibranch")])
