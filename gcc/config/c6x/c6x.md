;; Machine description for TI C6X.
;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
;; Contributed by Andrew Jenner <andrew@codesourcery.com>
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Register names

(define_constants
  [(REG_A0 0)
   (REG_A1 1)
   (REG_A2 2)
   (REG_A3 3)
   (REG_A4 4)
   (REG_A5 5)
   (REG_A6 6)
   (REG_A7 7)
   (REG_A8 8)
   (REG_A9 9)
   (REG_A10 10)
   (REG_A11 11)
   (REG_A12 12)
   (REG_A13 13)
   (REG_A14 14)
   (REG_A15 15)
   (REG_A16 16)
   (REG_A17 17)
   (REG_A18 18)
   (REG_A19 19)
   (REG_A20 20)
   (REG_A21 21)
   (REG_A22 22)
   (REG_A23 23)
   (REG_A24 24)
   (REG_A25 25)
   (REG_A26 26)
   (REG_A27 27)
   (REG_A28 28)
   (REG_A29 29)
   (REG_A30 30)
   (REG_A31 31)
   (REG_B0 32)
   (REG_B1 33)
   (REG_B2 34)
   (REG_B3 35)
   (REG_B4 36)
   (REG_B5 37)
   (REG_B6 38)
   (REG_B7 39)
   (REG_B8 40)
   (REG_B9 41)
   (REG_B10 42)
   (REG_B11 43)
   (REG_B12 44)
   (REG_B13 45)
   (REG_B14 46)
   (REG_SP 47)
   (REG_B15 47)
   (REG_B16 48)
   (REG_B17 49)
   (REG_B18 50)
   (REG_B19 51)
   (REG_B20 52)
   (REG_B21 53)
   (REG_B22 54)
   (REG_B23 55)
   (REG_B24 56)
   (REG_B25 57)
   (REG_B26 58)
   (REG_B27 59)
   (REG_B28 60)
   (REG_B29 61)
   (REG_B30 62)
   (REG_B31 63)
   (REG_FRAME 64)
   (REG_ARGP 65)
   (REG_ILC 66)])

(define_c_enum "unspec" [
   UNSPEC_NOP
   UNSPEC_RCP
   UNSPEC_MISALIGNED_ACCESS
   UNSPEC_ADDKPC
   UNSPEC_SETUP_DSBT
   UNSPEC_LOAD_GOT
   UNSPEC_LOAD_SDATA
   UNSPEC_BITREV
   UNSPEC_GOTOFF
   UNSPEC_MVILC
   UNSPEC_REAL_JUMP
   UNSPEC_REAL_LOAD
   UNSPEC_REAL_MULT
   UNSPEC_JUMP_SHADOW
   UNSPEC_LOAD_SHADOW
   UNSPEC_MULT_SHADOW
   UNSPEC_EPILOGUE_BARRIER
   UNSPEC_ATOMIC
   UNSPEC_CLR
   UNSPEC_EXT
   UNSPEC_EXTU
   UNSPEC_SUBC
   UNSPEC_AVG
])

(define_c_enum "unspecv" [
   UNSPECV_BLOCKAGE
   UNSPECV_SPLOOP
   UNSPECV_SPKERNEL
   UNSPECV_EH_RETURN
   UNSPECV_CAS
])

;; -------------------------------------------------------------------------
;; Instruction attributes
;; -------------------------------------------------------------------------

(define_attr "cpu"
  "c62x,c64x,c64xp,c67x,c67xp,c674x"
  (const (symbol_ref "(enum attr_cpu)c6x_arch")))

;; Define a type for each insn which is used in the scheduling description.
;; These correspond to the types defined in chapter 4 of the C674x manual.
(define_attr "type"
  "unknown,single,mpy2,store,storen,mpy4,load,loadn,branch,call,callp,dp2,fp4,
   intdp,cmpdp,adddp,mpy,mpyi,mpyid,mpydp,mpyspdp,mpysp2dp,spkernel,sploop,
   mvilc,blockage,shadow,load_shadow,mult_shadow,atomic"
  (const_string "single"))

;; The register file used by an instruction's destination register.
;; The function destreg_file computes this; instructions can override the
;; attribute if they aren't a single_set.
(define_attr "dest_regfile"
  "unknown,any,a,b"
  (cond [(eq_attr "type" "single,load,mpy2,mpy4,dp2,fp4,intdp,cmpdp,adddp,mpy,mpyi,mpyid,mpydp,mpyspdp,mpysp2dp")
	 (cond [(match_operand 0 "a_register" "") (const_string "a")
		(match_operand 0 "b_register" "") (const_string "b")]
	       (const_string "unknown"))
	 (eq_attr "type" "store")
		(cond [(match_operand 1 "a_register" "") (const_string "a")
		       (match_operand 1 "b_register" "") (const_string "b")]
	       (const_string "unknown"))]
	(const_string "unknown")))

(define_attr "addr_regfile"
  "unknown,a,b"
  (const_string "unknown"))

(define_attr "cross"
  "n,y"
  (const_string "n"))

;; This describes the relationship between operands and register files.
;; For example, "sxs" means that operands 0 and 2 determine the side of
;; the machine, and operand 1 can optionally use the cross path.  "dt" and
;; "td" are used to describe loads and stores.
;; Used for register renaming in loops for improving modulo scheduling.
(define_attr "op_pattern"
  "unknown,dt,td,sx,sxs,ssx"
  (cond [(eq_attr "type" "load") (const_string "td")
	 (eq_attr "type" "store") (const_string "dt")]
	(const_string "unknown")))

(define_attr "has_shadow"
  "n,y"
  (const_string "n"))

;; The number of cycles the instruction takes to finish.  Any cycles above
;; the first are delay slots.
(define_attr "cycles" ""
  (cond [(eq_attr "type" "branch,call") (const_int 6)
	 (eq_attr "type" "load,loadn") (const_int 5)
	 (eq_attr "type" "dp2") (const_int 2)
	 (eq_attr "type" "mpy2") (const_int 2)
	 (eq_attr "type" "mpy4") (const_int 4)
	 (eq_attr "type" "fp4") (const_int 4)
	 (eq_attr "type" "mvilc") (const_int 4)
	 (eq_attr "type" "cmpdp") (const_int 2)
	 (eq_attr "type" "intdp") (const_int 5)
	 (eq_attr "type" "adddp") (const_int 7)
	 (eq_attr "type" "mpydp") (const_int 10)
	 (eq_attr "type" "mpyi") (const_int 9)
	 (eq_attr "type" "mpyid") (const_int 10)
	 (eq_attr "type" "mpyspdp") (const_int 7)
	 (eq_attr "type" "mpysp2dp") (const_int 5)]
	(const_int 1)))

(define_attr "predicable" "no,yes"
  (const_string "yes"))

(define_attr "enabled" "no,yes"
  (const_string "yes"))

;; Specify which units can be used by a given instruction.  Normally,
;; dest_regfile is used to select between the two halves of the machine.
;; D_ADDR is for load/store instructions; they use the D unit and use
;; addr_regfile to choose between D1 and D2.

(define_attr "units62"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (const_string "unknown"))

(define_attr "units64"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (const_string "unknown"))

(define_attr "units64p"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (attr "units64"))

(define_attr "units67"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (attr "units62"))

(define_attr "units67p"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (attr "units67"))

(define_attr "units674"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (attr "units64"))

(define_attr "units"
  "unknown,d,d_addr,l,m,s,dl,ds,dls,ls"
  (cond [(eq_attr "cpu" "c62x")
	   (attr "units62")
	 (eq_attr "cpu" "c67x")
	   (attr "units67")
	 (eq_attr "cpu" "c67xp")
	   (attr "units67p")
	 (eq_attr "cpu" "c64x")
	   (attr "units64")
	 (eq_attr "cpu" "c64xp")
	   (attr "units64p")
	 (eq_attr "cpu" "c674x")
	   (attr "units674")
	]
	(const_string "unknown")))

(define_automaton "c6x_1,c6x_2,c6x_m1,c6x_m2,c6x_t1,c6x_t2,c6x_branch")
(automata_option "no-comb-vect")
(automata_option "ndfa")
(automata_option "collapse-ndfa")

(define_query_cpu_unit "d1,l1,s1" "c6x_1")
(define_cpu_unit "x1" "c6x_1")
(define_cpu_unit "l1w,s1w" "c6x_1")
(define_query_cpu_unit "m1" "c6x_m1")
(define_cpu_unit "m1w" "c6x_m1")
(define_cpu_unit "t1" "c6x_t1")
(define_query_cpu_unit "d2,l2,s2" "c6x_2")
(define_cpu_unit "x2" "c6x_2")
(define_cpu_unit "l2w,s2w" "c6x_2")
(define_query_cpu_unit "m2" "c6x_m2")
(define_cpu_unit "m2w" "c6x_m2")
(define_cpu_unit "t2" "c6x_t2")
;; A special set of units used to identify specific reservations, rather than
;; just units.
(define_query_cpu_unit "fps1,fpl1,adddps1,adddpl1" "c6x_1")
(define_query_cpu_unit "fps2,fpl2,adddps2,adddpl2" "c6x_2")

;; There can be up to two branches in one cycle (on the .s1 and .s2
;; units), but some instructions must not be scheduled in parallel
;; with a branch.  We model this by reserving either br0 or br1 for a
;; normal branch, and both of them for an insn such as callp.
;; Another constraint is that two branches may only execute in parallel
;; if one uses an offset, and the other a register.  We can distinguish
;; these by the dest_regfile attribute; it is "any" iff the branch uses
;; an offset.  br0 is reserved for these, while br1 is reserved for
;; branches using a register.
(define_cpu_unit "br0,br1" "c6x_branch")

(include "c6x-sched.md")

;; Some reservations which aren't generated from c6x-sched.md.in

(define_insn_reservation "branch_s1any" 6
  (and (eq_attr "type" "branch")
       (and (eq_attr "cross" "n")
	    (and (eq_attr "units" "s")
		 (eq_attr "dest_regfile" "any"))))
  "s1+s1w+br0")

;; For calls, we also reserve the units needed in the following cycles
;; to load the return address.  There are two options; using addkpc or
;; mvkh/mvkl.  The code in c6x_reorg knows whether to use one of these
;; or whether to use callp.  The actual insns are emitted only after
;; the final scheduling pass is complete.
;; We always reserve S2 for PC-relative call insns, since that allows
;; us to turn them into callp insns later on.
(define_insn_reservation "call_addkpc_s1any" 6
  (and (eq_attr "type" "call")
       (and (ne (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "any")))))
  "s2+s2w+br0,s2+s2w+br0+br1")

(define_insn_reservation "call_mvk_s1any" 6
  (and (eq_attr "type" "call")
       (and (eq (symbol_ref "TARGET_INSNS_64") (const_int 0))
	    (and (eq_attr "cross" "n")
		 (and (eq_attr "units" "s")
		      (eq_attr "dest_regfile" "any")))))
  "s2+s2w+br0,s2+s2w,s2+s2w")

(define_reservation "all" "s1+s2+d1+d2+l1+l2+m1+m2")

(define_insn_reservation "callp_s1" 1
  (and (eq_attr "type" "callp") (eq_attr "dest_regfile" "a"))
  "s1+s1w,all*5")

(define_insn_reservation "callp_s2" 1
  (and (eq_attr "type" "callp") (eq_attr "dest_regfile" "b"))
  "s2+s2w,all*5")

;; Constraints

(include "constraints.md")

;; Predicates

(include "predicates.md")

;; General predication pattern.

(define_cond_exec
  [(match_operator 0 "eqne_operator"
    [(match_operand 1 "predicate_register" "AB")
     (const_int 0)])]
  ""
  "")

;; -------------------------------------------------------------------------
;; NOP instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "nop_count"
  [(unspec [(match_operand 0 "const_int_operand" "n")] UNSPEC_NOP)]
  ""
  "%|%.\\tnop\\t%0")

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_mode_iterator QIHIM [QI HI])
(define_mode_iterator SIDIM [SI DI])
(define_mode_iterator SIDIVM [SI DI V2HI V4QI])
(define_mode_iterator VEC4M [V2HI V4QI])
(define_mode_iterator VEC8M [V2SI V4HI V8QI])
(define_mode_iterator SISFVM [SI SF V2HI V4QI])
(define_mode_iterator DIDFM [DI DF])
(define_mode_iterator DIDFVM [DI DF V2SI V4HI V8QI])
(define_mode_iterator SFDFM [SF DF])
(define_mode_iterator M32 [QI HI SI SF V2HI V4QI])

;; The C6X LO_SUM and HIGH are backwards - HIGH sets the low bits, and
;; LO_SUM adds in the high bits.  Fortunately these are opaque operations
;; so this does not matter.
(define_insn "movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=ab")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "const_int_or_symbolic_operand" "i")))]
  "reload_completed"
  "%|%.\\tmvkh\\t%$\\t%2, %0"
  [(set_attr "units" "s")])

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=ab")
	(high:SI (match_operand:SI 1 "const_int_or_symbolic_operand" "i")))]
  "reload_completed"
  "%|%.\\tmvkl\\t%$\\t%1, %0"
  [(set_attr "units" "s")])

(define_insn "movsi_gotoff_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=ab")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (unspec:SI [(match_operand:SI 2 "symbolic_operand" "S2")]
			      UNSPEC_GOTOFF)))]
  "flag_pic == 2"
  "%|%.\\tmvkh\\t%$\\t$dpr_got%2, %0"
  [(set_attr "units" "s")])

(define_insn "movsi_gotoff_high"
  [(set (match_operand:SI 0 "register_operand" "=ab")
	(high:SI (unspec:SI [(match_operand:SI 1 "symbolic_operand" "S2")]
			    UNSPEC_GOTOFF)))]
  "flag_pic == 2"
  "%|%.\\tmvkl\\t%$\\t$dpr_got%1, %0"
  [(set_attr "units" "s")])

;; Normally we'd represent this as a normal load insn, but we can't currently
;; represent the addressing mode.
(define_insn "load_got_gotoff"
  [(set (match_operand:SI 0 "register_operand" "=a,b")
	(unspec:SI [(match_operand:SI 1 "register_operand" "Z,Z")
		    (match_operand:SI 2 "register_operand" "b,b")]
		   UNSPEC_GOTOFF))]
  "flag_pic == 2"
  "%|%.\\tldw\\t%$\\t*+%1[%2], %0"
  [(set_attr "type" "load")
   (set_attr "units" "d_addr")
   (set_attr "dest_regfile" "a,b")
   (set_attr "addr_regfile" "b")])

(define_insn "*movstricthi_high"
  [(set (match_operand:SI 0 "register_operand" "+ab")
	(ior:SI (and:SI (match_dup 0) (const_int 65535))
		(ashift:SI (match_operand:SI 1 "const_int_operand" "IuB")
			   (const_int 16))))]
  "reload_completed"
  "%|%.\\tmvklh\\t%$\\t%1, %0"
  [(set_attr "units" "s")])

;; Break up SImode loads of immediate operands.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "reload_completed
   && !satisfies_constraint_IsB (operands[1])"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (ior:SI (and:SI (match_dup 0) (const_int 65535))
			      (ashift:SI (match_dup 3) (const_int 16))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);
  operands[2] = GEN_INT (trunc_int_for_mode (val, HImode));
  operands[3] = GEN_INT ((val >> 16) & 65535);
})

(define_split
  [(set (match_operand:VEC4M 0 "register_operand" "")
	(match_operand:VEC4M 1 "const_vector_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))]
{
  unsigned HOST_WIDE_INT mask, val;
  enum machine_mode inner_mode = GET_MODE_INNER (<MODE>mode);
  int i;

  val = 0;
  mask = GET_MODE_MASK (inner_mode);
  if (TARGET_BIG_ENDIAN)
    {
      for (i = 0; i < GET_MODE_NUNITS (<MODE>mode); i++)
	{
	  val <<= GET_MODE_BITSIZE (inner_mode);
	  val |= INTVAL (CONST_VECTOR_ELT (operands[1], i)) & mask;
	}
    }
  else
    {
      i = GET_MODE_NUNITS (<MODE>mode);
      while (i-- > 0)
	{
	  val <<= GET_MODE_BITSIZE (inner_mode);
	  val |= INTVAL (CONST_VECTOR_ELT (operands[1], i)) & mask;
	}
    }
  operands[2] = gen_rtx_REG (SImode, REGNO (operands[0]));
  operands[3] = GEN_INT (trunc_int_for_mode (val, SImode));
})

(define_split
  [(set (match_operand:VEC8M 0 "register_operand" "")
	(match_operand:VEC8M 1 "const_vector_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  unsigned HOST_WIDE_INT mask;
  unsigned HOST_WIDE_INT val[2];
  rtx lo_half, hi_half;
  enum machine_mode inner_mode = GET_MODE_INNER (<MODE>mode);
  int i, j;

  split_di (operands, 1, &lo_half, &hi_half);

  val[0] = val[1] = 0;
  mask = GET_MODE_MASK (inner_mode);
  if (TARGET_BIG_ENDIAN)
    {
      for (i = 0, j = 1; i < GET_MODE_NUNITS (<MODE>mode); i++)
	{
	  if (i * 2 == GET_MODE_NUNITS (<MODE>mode))
	    j--;
	  val[j] <<= GET_MODE_BITSIZE (inner_mode);
	  val[j] |= INTVAL (CONST_VECTOR_ELT (operands[1], i)) & mask;
	}
    }
  else
    {
      i = GET_MODE_NUNITS (<MODE>mode);
      j = 1;
      while (i-- > 0)
        {
	  val[j] <<= GET_MODE_BITSIZE (inner_mode);
	  val[j] |= INTVAL (CONST_VECTOR_ELT (operands[1], i)) & mask;
	  if (i * 2 == GET_MODE_NUNITS (<MODE>mode))
	    j--;
	}
    }
  operands[2] = lo_half;
  operands[3] = GEN_INT (trunc_int_for_mode (val[0], SImode));
  operands[4] = hi_half;
  operands[5] = GEN_INT (trunc_int_for_mode (val[1], SImode));
})

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
	(match_operand:SF 1 "immediate_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 2) (ior:SI (and:SI (match_dup 2) (const_int 65535))
			      (ashift:SI (match_dup 4) (const_int 16))))]
{
  long values;
  REAL_VALUE_TYPE value;

  gcc_assert (GET_CODE (operands[1]) == CONST_DOUBLE);

  REAL_VALUE_FROM_CONST_DOUBLE (value, operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE (value, values);

  operands[2] = gen_rtx_REG (SImode, true_regnum (operands[0]));
  operands[3] = GEN_INT (trunc_int_for_mode (values, HImode));
  if (values >= -32768 && values < 32768)
    {
      emit_move_insn (operands[2], operands[3]);
      DONE;
    }
  operands[4] = GEN_INT ((values >> 16) & 65535);
})

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "symbolic_operand" ""))]
  "reload_completed
   && (!TARGET_INSNS_64PLUS
       || !sdata_symbolic_operand (operands[1], SImode))"
  [(set (match_dup 0) (high:SI (match_dup 1)))
   (set (match_dup 0) (lo_sum:SI (match_dup 0) (match_dup 1)))]
  "")

;; Normally, we represent the load of an sdata address as a normal
;; move of a SYMBOL_REF.  In DSBT mode, B14 is not constant, so we
;; should show the dependency.
(define_insn "load_sdata_pic"
  [(set (match_operand:SI 0 "register_operand" "=a,b")
	(plus:SI (match_operand:SI 1 "pic_register_operand" "Z,Z")
		 (unspec:SI [(match_operand:SI 2 "sdata_symbolic_operand" "S0,S0")]
			    UNSPEC_LOAD_SDATA)))]
  "flag_pic"
  "@
   %|%.\\tadda%D2\\t%$\\t%1, %2, %0
   %|%.\\tadda%D2\\t%$\\t%1, %2, %0"
  [(set_attr "units" "d")
   (set_attr "cross" "y,n")
   (set_attr "op_pattern" "unknown")
   (set_attr "predicable" "no")])

;; Move instruction patterns

(define_mode_attr LDST_SUFFIX [(QI "b") (HI "h")
			       (SI "w") (SF "w") (V2HI "w") (V4QI "w")
			       (DI "dw") (V2SI "dw") (V4HI "dw") (V8QI "dw")])

(define_insn "mov<mode>_insn"
 [(set (match_operand:QIHIM 0 "nonimmediate_operand"
        "=a,b, a, b, ab, ab,a,?a, b,?b, Q, R, R, Q")
       (match_operand:QIHIM 1 "general_operand"
         "a,b,?b,?a,Is5,IsB,Q, R, R, Q, a,?a, b,?b"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
 "@
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmvk\\t%$\\t%1, %0
  %|%.\\tmvk\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tst<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tst<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tst<LDST_SUFFIX>\\t%$\\t%1, %0
  %|%.\\tst<LDST_SUFFIX>\\t%$\\t%1, %0"
  [(set_attr "type" "*,*,*,*,*,*,load,load,load,load,store,store,store,store")
   (set_attr "units62" "dls,dls,ls,ls,s,s,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr")
   (set_attr "units64" "dls,dls,ls,ls,dl,s,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr")
   (set_attr "op_pattern" "sx,sx,sx,sx,*,*,*,*,*,*,*,*,*,*")
   (set_attr "addr_regfile" "*,*,*,*,*,*,a,b,b,a,a,b,b,a")
   (set_attr "dest_regfile" "*,*,*,*,*,*,a,a,b,b,a,a,b,b")
   (set_attr "cross" "n,n,y,y,n,n,n,y,n,y,n,y,n,y")])

(define_insn "mov<mode>_insn"
 [(set (match_operand:SISFVM 0 "nonimmediate_operand"
        "=a,b, a, b, ab, ab,a,b,ab,a,?a, b,?b, Q, R, R, Q")
       (match_operand:SISFVM 1 "general_operand"
         "a,b,?b,?a,Is5,IsB,S0,S0,Si,Q, R, R, Q, a,?a, b,?b"))]
  "(GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG
    || (GET_CODE (operands[1]) == SUBREG && REG_P (SUBREG_REG (operands[1]))))"
 "@
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmv\\t%$\\t%1, %0
  %|%.\\tmvk\\t%$\\t%1, %0
  %|%.\\tmvk\\t%$\\t%1, %0
  %|%.\\tadda%D1\\t%$\\tB14, %1, %0
  %|%.\\tadda%D1\\t%$\\tB14, %1, %0
  #
  %|%.\\tldw\\t%$\\t%1, %0
  %|%.\\tldw\\t%$\\t%1, %0
  %|%.\\tldw\\t%$\\t%1, %0
  %|%.\\tldw\\t%$\\t%1, %0
  %|%.\\tstw\\t%$\\t%1, %0
  %|%.\\tstw\\t%$\\t%1, %0
  %|%.\\tstw\\t%$\\t%1, %0
  %|%.\\tstw\\t%$\\t%1, %0"
  [(set_attr "type" "*,*,*,*,*,*,*,*,*,load,load,load,load,store,store,store,store")
   (set_attr "units62" "dls,dls,ls,ls,s,s,d,d,*,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr")
   (set_attr "units64" "dls,dls,ls,ls,dl,s,d,d,*,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr")
   (set_attr "op_pattern" "sx,sx,sx,sx,*,*,*,*,*,*,*,*,*,*,*,*,*")
   (set_attr "addr_regfile" "*,*,*,*,*,*,*,*,*,a,b,b,a,a,b,b,a")
   (set_attr "dest_regfile" "*,*,*,*,*,*,*,*,*,a,a,b,b,a,a,b,b")
   (set_attr "cross" "n,n,y,y,n,n,y,n,*,n,y,n,y,n,y,n,y")
   (set_attr "predicable" "yes,yes,yes,yes,yes,yes,no,no,yes,yes,yes,yes,yes,yes,yes,yes,yes")])

(define_insn "*mov<mode>_insn"
  [(set (match_operand:DIDFVM 0 "nonimmediate_operand"
         "=a,b, a, b,ab,a,?a, b,?b, Q, R, R, Q")
        (match_operand:DIDFVM 1 "general_operand"
          "a,b,?b,?a,iF,Q, R, R, Q, a,?a, b,?b"))]
  "(!MEM_P (operands[0]) || REG_P (operands[1])
    || (GET_CODE (operands[1]) == SUBREG && REG_P (SUBREG_REG (operands[1]))))"
{
  if (MEM_P (operands[1]) && TARGET_LDDW)
    return "%|%.\\tlddw\\t%$\\t%1, %0";
  if (MEM_P (operands[0]) && TARGET_STDW)
    return "%|%.\\tstdw\\t%$\\t%1, %0";
  if (TARGET_INSNS_64PLUS && REG_P (operands[0]) && REG_P (operands[1])
      && A_REGNO_P (REGNO (operands[0])) == A_REGNO_P (REGNO (operands[1])))
    return "%|%.\\tdmv\\t%$\\t%P1, %p1, %0";
  return "#";
}
  [(set_attr "units" "s,s,*,*,*,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr,d_addr")
   (set_attr "addr_regfile" "*,*,*,*,*,a,b,b,a,a,b,b,a")
   (set_attr "dest_regfile" "*,*,*,*,*,a,a,b,b,a,a,b,b")
   (set_attr "type" "*,*,*,*,*,load,load,load,load,store,store,store,store")
   (set_attr "cross" "n,n,y,y,*,n,y,n,y,n,y,n,y")])

(define_split
  [(set (match_operand:DIDFVM 0 "nonimmediate_operand" "")
  	(match_operand:DIDFVM 1 "general_operand" ""))]
  "reload_completed
   && !((MEM_P (operands[0]) && TARGET_STDW)
 	|| (MEM_P (operands[1]) && TARGET_LDDW))
   && !const_vector_operand (operands[1], <MODE>mode)
   && !(TARGET_INSNS_64PLUS && REG_P (operands[0]) && REG_P (operands[1])
	&& A_REGNO_P (REGNO (operands[0])) == A_REGNO_P (REGNO (operands[1])))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  rtx lo_half[2], hi_half[2];
  split_di (operands, 2, lo_half, hi_half);

  /* We can't have overlap for a register-register move, but if
     memory is involved, we have to make sure we don't clobber the
     address.  */
  if (reg_overlap_mentioned_p (lo_half[0], hi_half[1]))
    {
      operands[2] = hi_half[0];
      operands[3] = hi_half[1];
      operands[4] = lo_half[0];
      operands[5] = lo_half[1];
    }
  else
    {
      operands[2] = lo_half[0];
      operands[3] = lo_half[1];
      operands[4] = hi_half[0];
      operands[5] = hi_half[1];
    }
})

(define_insn "real_load<mode>"
  [(unspec [(match_operand 0 "const_int_operand" "JA,JA,JB,JB")
	    (match_operand:M32 1 "memory_operand" "Q,R,R,Q")]
	   UNSPEC_REAL_LOAD)]
  ""
  "%|%.\\tld<LDST_SUFFIX>\\t%$\\t%1, %k0"
  [(set_attr "type" "load")
   (set_attr "units" "d_addr")
   (set_attr "addr_regfile" "a,b,b,a")
   (set_attr "dest_regfile" "a,a,b,b")
   (set_attr "cross" "n,y,n,y")])

(define_insn "real_load<mode>"
  [(unspec [(match_operand 0 "const_int_operand" "JA,JA,JB,JB")
	    (match_operand:DIDFVM 1 "memory_operand" "Q,R,R,Q")]
	   UNSPEC_REAL_LOAD)]
  "TARGET_LDDW"
  "%|%.\\tlddw\\t%$\\t%1, %K0"
  [(set_attr "type" "load")
   (set_attr "units" "d_addr")
   (set_attr "addr_regfile" "a,b,b,a")
   (set_attr "dest_regfile" "a,a,b,b")
   (set_attr "cross" "n,y,n,y")])

(define_insn "load_shadow"
  [(set (match_operand 0 "register_operand" "=ab")
 	(unspec [(pc)] UNSPEC_LOAD_SHADOW))]
  ""
  ";; load to %0 occurs"
  [(set_attr "type" "load_shadow")])

(define_insn "mult_shadow"
  [(set (match_operand 0 "register_operand" "=ab")
 	(unspec [(pc)] UNSPEC_MULT_SHADOW))]
  ""
  ";; multiplication occurs and stores to %0"
  [(set_attr "type" "mult_shadow")])


(define_mode_iterator MOV [QI HI SI SF DI DF V2HI V4QI V2SI V4HI V8QI])

(define_expand "mov<mode>"
  [(set (match_operand:MOV 0 "nonimmediate_operand" "")
	(match_operand:MOV 1 "general_operand" ""))]
  ""
{
  if (expand_move (operands, <MODE>mode))
    DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:SIDIVM 0 "nonimmediate_operand"	      "")
	(unspec:SIDIVM [(match_operand:SIDIVM 1 "nonimmediate_operand" "")]
		       UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_INSNS_64"
{
  if (memory_operand (operands[0], <MODE>mode))
    {
      emit_insn (gen_movmisalign<mode>_store (operands[0], operands[1]));
      DONE;
    }
})

(define_insn_and_split "movmisalign<mode>_store"
  [(set (match_operand:SIDIVM 0 "memory_operand" "=W,Q,T,Q,T")
	(unspec:SIDIVM [(match_operand:SIDIVM 1 "register_operand" "r,a,b,b,a")]
		       UNSPEC_MISALIGNED_ACCESS))
   (clobber (match_scratch:SI 2 "=r,X,X,X,X"))]
  "TARGET_INSNS_64"
  "@
   #
   %|%.\\tstn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tstn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tstn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tstn<LDST_SUFFIX>\\t%$\\t%1, %0"
  "&& reload_completed && satisfies_constraint_W (operands[0])"
  [(parallel
    [(set (match_dup 3) (unspec:SIDIVM [(match_dup 1)] UNSPEC_MISALIGNED_ACCESS))
     (clobber (match_dup 4))])]
{
  rtx addr = XEXP (operands[0], 0);
  rtx tmpreg = operands[2];

  if (GET_CODE (addr) == PLUS && XEXP (addr, 0) == stack_pointer_rtx
      && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    {
      unsigned HOST_WIDE_INT val = INTVAL (XEXP (addr, 1));
      val &= GET_MODE_SIZE (<MODE>mode) - 1;
      if (val == 0)
	{
	  emit_move_insn (operands[0], operands[1]);
	  DONE;
	}
    }
  operands[3] = change_address (operands[0], <MODE>mode, tmpreg);
  emit_move_insn (tmpreg, addr);
  operands[4] = gen_rtx_SCRATCH (SImode);
}
  [(set_attr "type" "storen")
   (set_attr "units" "d_addr")
   (set_attr "addr_regfile" "*,a,b,a,b")
   (set_attr "dest_regfile" "*,a,b,b,a")
   (set_attr "cross" "*,n,n,y,y")])

(define_insn_and_split "movmisalign<mode>_load"
  [(set (match_operand:SIDIVM 0 "register_operand" "=ab,a,b,b,a")
	(unspec:SIDIVM [(match_operand:SIDIVM 1 "memory_operand" "W,Q,T,Q,T")]
		       UNSPEC_MISALIGNED_ACCESS))]
  "TARGET_INSNS_64"
  "@
   #
   %|%.\\tldn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tldn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tldn<LDST_SUFFIX>\\t%$\\t%1, %0
   %|%.\\tldn<LDST_SUFFIX>\\t%$\\t%1, %0"
  "&& reload_completed && satisfies_constraint_W (operands[1])"
  [(set (match_dup 0) (unspec:SIDIVM [(match_dup 2)] UNSPEC_MISALIGNED_ACCESS))]
{
  rtx addr = XEXP (operands[1], 0);
  rtx tmpreg = (GET_MODE (operands[0]) == SImode ? operands[0]
		: operand_subword_force (operands[0], 0, DImode));

  if (GET_CODE (addr) == PLUS && XEXP (addr, 0) == stack_pointer_rtx
      && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    {
      unsigned HOST_WIDE_INT val = INTVAL (XEXP (addr, 1));
      val &= GET_MODE_SIZE (<MODE>mode) - 1;
      if (val == 0)
	{
	  emit_move_insn (operands[0], operands[1]);
	  DONE;
	}
    }
  operands[2] = change_address (operands[1], <MODE>mode, tmpreg);
  emit_move_insn (tmpreg, addr);
}
  [(set_attr "type" "loadn")
   (set_attr "units" "d_addr")
   (set_attr "addr_regfile" "*,a,b,a,b")
   (set_attr "dest_regfile" "*,a,b,b,a")
   (set_attr "cross" "*,n,n,y,y")])

;;

;; -------------------------------------------------------------------------
;; Extensions/extractions
;; -------------------------------------------------------------------------

(define_code_iterator any_extract [zero_extract sign_extract])
(define_code_iterator any_ext [zero_extend sign_extend])

(define_code_attr ext_name [(zero_extend "zero_extend") (sign_extend "sign_extend")])

(define_code_attr u [(zero_extend "u") (sign_extend "")])

(define_code_attr z [(zero_extract "z") (sign_extract "")])
(define_code_attr zu [(zero_extract "u") (sign_extract "")])

(define_mode_attr ext_shift [(QI "24") (HI "16")])

(define_insn "<ext_name><mode>si2"
 [(set (match_operand:SI 0 "register_operand" "=a,b,a,?a, b,?b")
       (any_ext:SI (match_operand:QIHIM 1 "nonimmediate_operand" "a,b,Q, R, R, Q")))]
  ""
 "@
  %|%.\\text<u>\\t%$\\t%1, <ext_shift>, <ext_shift>, %0
  %|%.\\text<u>\\t%$\\t%1, <ext_shift>, <ext_shift>, %0
  %|%.\\tld<LDST_SUFFIX><u>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX><u>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX><u>\\t%$\\t%1, %0
  %|%.\\tld<LDST_SUFFIX><u>\\t%$\\t%1, %0"
  [(set_attr "type" "*,*,load,load,load,load")
   (set_attr "units" "s,s,d_addr,d_addr,d_addr,d_addr")
   (set_attr "addr_regfile" "*,*,a,b,b,a")
   (set_attr "dest_regfile" "*,*,a,a,b,b")
   (set_attr "cross" "n,n,n,y,n,y")])

(define_insn "*ext<z>v_const"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=a,b")
	(any_extract:SI (match_operand:SI 1 "register_operand" "a,b")
			(match_operand:SI 2 "const_int_operand" "n,n")
			(match_operand:SI 3 "const_int_operand" "n,n")))]
  "INTVAL (operands[3]) >= 0
   && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32"
{
  int pos = INTVAL (operands[3]);
  int len = INTVAL (operands[2]);
  rtx xop[4];
  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = GEN_INT (32 - pos - len);
  xop[3] = GEN_INT (32 - len);

  output_asm_insn ("%|%.\\text<zu>\\t%$\\t%1, %2, %3, %0", xop);
  return "";
}
  [(set_attr "units" "s")
   (set_attr "cross" "n")])

(define_expand "ext<z>v"
  [(set (match_operand:SI 0 "register_operand" "")
	(any_extract:SI (match_operand:SI 1 "register_operand" "")
			(match_operand:SI 2 "const_int_operand" "")
			(match_operand:SI 3 "const_int_operand" "")))]
  ""
{
   if (INTVAL (operands[2]) < 0
       || INTVAL (operands[2]) + INTVAL (operands[3]) > 32)
     FAIL;
})

(define_insn "real_<ext_name><mode>"
  [(unspec [(match_operand 0 "const_int_operand" "JA,JA,JB,JB")
	    (any_ext:SI (match_operand:QIHIM 1 "memory_operand" "Q,R,R,Q"))]
	   UNSPEC_REAL_LOAD)]
  ""
  "%|%.\\tld<LDST_SUFFIX><u>\\t%$\\t%1, %k0"
  [(set_attr "type" "load")
   (set_attr "units" "d_addr")
   (set_attr "addr_regfile" "a,b,b,a")
   (set_attr "dest_regfile" "a,a,b,b")
   (set_attr "cross" "n,y,n,y")])

(define_insn "clrr"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0,0,0,0")
		    (match_operand:SI 2 "register_operand" "a,b,?b,?a")
		    (match_operand:SI 3 "reg_or_const_int_operand" "ai,bi,a,b")]
		   UNSPEC_CLR))]
  ""
{
  if (CONST_INT_P (operands[2]))
    {
      rtx xops[4];
      int v1 = INTVAL (operands[2]);
      int v2 = (v1 >> 5) & 0x1f;
      v1 &= 0x1f;
      xops[0] = operands[0];
      xops[1] = operands[1];
      xops[2] = GEN_INT (v1);
      xops[3] = GEN_INT (v2);
      output_asm_insn ("%|%.\\tclr\\t%$\\t%1, %3, %2, %0", xops);
      return "";
    }
  return "%|%.\\tclr\\t%$\\t%2, %3, %0";
}
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

(define_insn "extr"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(unspec:SI [(match_operand:SI 1 "register_operand" "a,b,?b,?a")
		    (match_operand:SI 2 "reg_or_const_int_operand" "ai,bi,a,b")]
		   UNSPEC_EXT))]
  ""
{
  if (CONST_INT_P (operands[2]))
    {
      rtx xops[4];
      int v1 = INTVAL (operands[2]);
      int v2 = (v1 >> 5) & 0x1f;
      v1 &= 0x1f;
      xops[0] = operands[0];
      xops[1] = operands[1];
      xops[2] = GEN_INT (v1);
      xops[3] = GEN_INT (v2);
      output_asm_insn ("%|%.\\text\\t%$\\t%1, %3, %2, %0", xops);
      return "";
    }
  return "%|%.\\text\\t%$\\t%1, %2, %0";
}
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

(define_insn "extru"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(unspec:SI [(match_operand:SI 1 "register_operand" "a,b,?b,?a")
		    (match_operand:SI 2 "reg_or_const_int_operand" "ai,bi,a,b")]
		   UNSPEC_EXTU))]
  ""
{
  if (CONST_INT_P (operands[2]))
    {
      rtx xops[4];
      int v1 = INTVAL (operands[2]);
      int v2 = (v1 >> 5) & 0x1f;
      v1 &= 0x1f;
      xops[0] = operands[0];
      xops[1] = operands[1];
      xops[2] = GEN_INT (v1);
      xops[3] = GEN_INT (v2);
      output_asm_insn ("%|%.\\textu\\t%$\\t%1, %3, %2, %0", xops);
      return "";
    }
  return "%|%.\\textu\\t%$\\t%1, %2, %0";
}
  [(set_attr "units" "s")
   (set_attr "cross" "n,y,n,y")])

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_insn "scmpsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=ab,a,b,a,b")
	(match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:SI 2 "register_operand" "ab,a,b,?b,?a")
	    (match_operand:SI 3 "reg_or_scst5_operand" "Is5,aIs5,bIs5,aIs5,bIs5")]))]
  ""
  "%|%.\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set (attr "cross")
	(symbol_ref "CROSS_OPERANDS (operands[0], operands[2])"))])

(define_insn "*ucmpsi_insn_64"
  [(set (match_operand:SI 0 "register_operand" "=ab,a,b,a,b")
	(match_operator:SI 1 "ltugtu_operator"
	   [(match_operand:SI 2 "register_operand" "ab,a,b,?b,?a")
	    (match_operand:SI 3 "reg_or_ucst5_operand" "Iu5,aIu5,bIu5,aIu5,bIu5")]))]
  "TARGET_INSNS_64"
  "%|%.\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set (attr "cross")
	(symbol_ref "CROSS_OPERANDS (operands[0], operands[2])"))])

(define_insn "*ucmpsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=ab,a,b,a,b")
	(match_operator:SI 1 "ltugtu_operator"
	   [(match_operand:SI 2 "register_operand" "ab,a,b,?b,?a")
	    (match_operand:SI 3 "reg_or_ucst4_operand" "Iu4,aIu4,bIu4,aIu4,bIu4")]))]
  "!TARGET_INSNS_64"
  "%|%.\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set (attr "cross")
	(symbol_ref "CROSS_OPERANDS (operands[0], operands[2])"))])

(define_code_iterator andior_eqne [eq ne])
(define_code_attr andior_name [(eq "and") (ne "ior")])
(define_code_attr andior_condmod [(eq "") (ne "!")])

(define_insn "*scmpsi_<andior_name>_insn"
  [(set (match_operand:SI 0 "register_operand" "=A,B,A,B")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "eqltgt_operator"
	  [(match_operand:SI 2 "register_operand" "a,b,?b,?a")
	   (match_operand:SI 3 "reg_or_scst5_operand" "aIs5,bIs5,aIs5,bIs5")])))]
  ""
  "%|[<andior_condmod>%4]\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")
   (set_attr "predicable" "no")])

(define_insn "*ucmpsi_<andior_name>_insn_64"
  [(set (match_operand:SI 0 "register_operand" "=A,B,A,B")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "ltugtu_operator"
	  [(match_operand:SI 2 "register_operand" "a,b,?b,?a")
	   (match_operand:SI 3 "reg_or_ucst5_operand" "aIu5,bIu5,aIu5,bIu5")])))]
  "TARGET_INSNS_64"
  "%|[<andior_condmod>%4]\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")
   (set_attr "predicable" "no")])

(define_insn "*ucmpsi_<andior_name>_insn"
  [(set (match_operand:SI 0 "register_operand" "=A,B,A,B")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "ltugtu_operator"
	  [(match_operand:SI 2 "register_operand" "a,b,?b,?a")
	   (match_operand:SI 3 "reg_or_ucst4_operand" "aIu4,bIu4,aIu4,bIu4")])))]
  "!TARGET_INSNS_64"
  "%|[<andior_condmod>%4]\\tcmp%C1\\t%$\\t%3, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")
   (set_attr "predicable" "no")])

(define_expand "cmpsi_<andior_name>"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "c6x_comparison_operator"
	  [(match_operand:SI 2 "register_operand" "")
	   (match_operand:SI 3 "reg_or_const_int_operand" "")])))]
  ""
{
  if (c6x_force_op_for_comparison_p (GET_CODE (operands[1]), operands[3]))
    operands[3] = force_reg (SImode, operands[3]);
})

(define_insn "*cmpsf_insn"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:SF 2 "register_operand" "a,b,a,b")
	    (match_operand:SF 3 "register_operand" "a,b,?b,?a")]))]
  "TARGET_FP"
  "%|%.\\tcmp%c1sp\\t%$\\t%2, %3, %0"
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

(define_insn "*cmpdf_insn"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:DF 2 "register_operand" "a,b,a,b")
	    (match_operand:DF 3 "register_operand" "a,b,?b,?a")]))]
  "TARGET_FP"
  "%|%.\\tcmp%c1dp\\t%$\\t%2, %3, %0"
  [(set_attr "type" "cmpdp")
   (set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

(define_expand "cmp<mode>_<andior_name>"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:SFDFM 2 "register_operand" "")
	    (match_operand:SFDFM 3 "register_operand" "")])))]
  "TARGET_FP")

(define_insn "*cmpsf_<andior_name>_insn"
  [(set (match_operand:SI 0 "register_operand" "=A,B,A,B")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:SF 2 "register_operand" "a,b,a,b")
	    (match_operand:SF 3 "register_operand" "a,b,?b,?a")])))]
  "TARGET_FP"
  "%|[<andior_condmod>%4]\\tcmp%c1sp\\t%$\\t%2, %3, %0"
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")
   (set_attr "predicable" "no")])

;; reload_reg_class_lower will ensure that two-word reloads are allocated first,
;; which could exhaust the predicate registers if we used just "a" and "b"
;; constraints on operands 2 and 3.
(define_insn "*cmpdf_<andior_name>_insn"
  [(set (match_operand:SI 0 "register_operand" "=A,B,A,B")
	(if_then_else:SI
	 (andior_eqne:SI (match_operand:SI 4 "register_operand" "0,0,0,0")
			 (const_int 0))
	 (match_dup 4)
	 (match_operator:SI 1 "eqltgt_operator"
	   [(match_operand:DF 2 "register_operand" "Da,Db,Da,Db")
	    (match_operand:DF 3 "register_operand" "Da,Db,?Db,?Da")])))]
  "TARGET_FP"
  "%|[<andior_condmod>%4]\\tcmp%c1dp\\t%$\\t%2, %3, %0"
  [(set_attr "type" "cmpdp")
   (set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")
   (set_attr "predicable" "no")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand 1 "c6x_any_comparison_operand" "")
		(match_operand 2 "c6x_any_comparison_operand" "")))]
  "!reg_overlap_mentioned_p (operands[0], operands[2])"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0)
	(if_then_else:SI (ne:SI (match_dup 0) (const_int 0))
			 (match_dup 0)
			 (match_dup 2)))])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand 1 "c6x_any_comparison_operand" "")
		(match_operand 2 "c6x_any_comparison_operand" "")))]
  "!reg_overlap_mentioned_p (operands[0], operands[2])"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0)
	(if_then_else:SI (eq:SI (match_dup 0) (const_int 0))
			 (match_dup 0)
			 (match_dup 2)))])


;; -------------------------------------------------------------------------
;; setcc instructions
;; -------------------------------------------------------------------------

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "comparison_operator"
	 [(match_operand:SI 2 "register_operand" "")
	  (match_operand:SI 3 "reg_or_ucst4_operand" "")]))]
  ""
{
  if (!c6x_comparison_operator (operands[1], SImode))
    {
      rtx tmpreg = gen_reg_rtx (SImode);
      rtx t = gen_rtx_fmt_ee (reverse_condition (GET_CODE (operands[1])),
			      SImode, operands[2], operands[3]);
      emit_insn (gen_rtx_SET (VOIDmode, tmpreg, t));
      emit_insn (gen_scmpsi_insn (operands[0],
				  gen_rtx_fmt_ee (EQ, SImode, tmpreg, const0_rtx),
				  tmpreg, const0_rtx));
      DONE;
    }
})

;; -------------------------------------------------------------------------
;; Jump instructions
;; -------------------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "a,b"))]
  ""
  "%|%.\\tb\\t%$\\t%0"
  [(set_attr "type" "branch")
   (set_attr "units" "s")
   (set_attr "cross" "y,n")
   (set_attr "dest_regfile" "b")])

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "%|%.\\tb\\t%$\\t%l0"
  [(set_attr "type" "branch")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand" ""))
              (use (label_ref (match_operand 1 "" "")))])]
  "!flag_pic || !TARGET_INSNS_64"
{
})

(define_insn "*tablejump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "b"))
   (use (label_ref (match_operand 1 "" "")))]
  "!flag_pic || !TARGET_INSNS_64"
  "%|\\tb\\t%$\\t%0"
  [(set_attr "type" "branch")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "b")])

;; Implement switch statements when generating PIC code.  Switches are
;; implemented by `tablejump' when not using -fpic.

;; Emit code here to do the range checking and make the index zero based.
;; operand 0 is the index
;; operand 1 is the lower bound
;; operand 2 is the range of indices (highest - lowest + 1)
;; operand 3 is the label that precedes the table itself
;; operand 4 is the fall through label

(define_expand "casesi"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "const_int_operand" ""))
   (use (match_operand:SI 2 "const_int_operand" ""))
   (use (match_operand 3 "" ""))
   (use (match_operand 4 "" ""))]
  "flag_pic && TARGET_INSNS_64"
{
  rtx indx;
  rtx low = operands[1];
  rtx range = operands[2];
  rtx table = operands[3];
  rtx fail = operands[4];

  gcc_assert (GET_CODE (operands[1]) == CONST_INT);
  gcc_assert (GET_CODE (operands[2]) == CONST_INT);

  if (!reg_or_ucst4_operand (range, SImode))
    range = force_reg (SImode, range);

  /* If low bound is 0, we don't have to subtract it.  */
  if (INTVAL (operands[1]) == 0)
    indx = operands[0];
  else
    {
      rtx offset = GEN_INT (-INTVAL (low));
      indx = gen_reg_rtx (SImode);
      if (!addsi_operand (offset, SImode))
        offset = force_reg (SImode, offset);
      emit_insn (gen_addsi3 (indx, operands[0], offset));
    }
  emit_cmp_and_jump_insns (indx, range, GTU, NULL_RTX, SImode, 1, fail);

  emit_jump_insn (gen_casesi_internal (indx, table));
  DONE;
})

;; This is the only instance in this file where a pattern emits more than
;; one instruction.  The concern here is that the addkpc insn could otherwise
;; be scheduled too far away from the label.  A tablejump always ends an
;; extended basic block, so it shouldn't happen that the scheduler places
;; something in the delay slots.
(define_insn "casesi_internal"
  [(set (pc)
	(mem:SI (plus:SI (mult:SI (match_operand:SI 0 "register_operand" "b")
				  (const_int 4))
			 (label_ref (match_operand 1 "" "")))))
   (clobber (match_scratch:SI 2 "=&b"))
   (clobber (match_scratch:SI 3 "=b"))]
  "flag_pic && TARGET_INSNS_64"
  "addkpc\t.s2\t%l1,%2, 0\n\t\tldw\t.d2t2\t*+%2[%0], %3\n\t\tnop\t\t4\n\t\tadd\t.l2\t%2, %3, %3\n\t\tb\t.s2\t%3"
  [(set_attr "type" "branch")
   (set_attr "predicable" "no")
   (set_attr "dest_regfile" "b")])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:SIDIM 1 "register_operand" "")
			(match_operand:SIDIM 2 "reg_or_const_int_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  rtx t = c6x_expand_compare (operands[0], VOIDmode);
  operands[0] = t;
  operands[1] = XEXP (t, 0);
  operands[2] = XEXP (t, 1);
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "c6x_fp_comparison_operator"
		       [(match_operand:SFDFM 1 "register_operand" "")
			(match_operand:SFDFM 2 "register_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  rtx t = c6x_expand_compare (operands[0], VOIDmode);
  operands[0] = t;
  operands[1] = XEXP (t, 0);
  operands[2] = XEXP (t, 1);
})

(define_insn "br_true"
  [(set (pc)
	(if_then_else (match_operator 0 "predicate_operator"
			[(match_operand:SI 1 "register_operand" "AB")
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "%|[%J0]\\tb\\t%$\\t%l2"
  [(set_attr "type" "branch")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any")])

(define_insn "br_false"
  [(set (pc)
	(if_then_else (match_operator 0 "predicate_operator"
			[(match_operand:SI 1 "register_operand" "AB")
			 (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
  "%|[%j0]\\tb\\t%$\\t%l2"
  [(set_attr "type" "branch")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any")])

(define_expand "return"
  [(parallel
    [(return)
     (use (reg:SI REG_B3))])]
  "reload_completed && get_frame_size () == 0 && c6x_nsaved_regs () == 0")

;; We can't expand this before we know where the link register is stored.
(define_insn_and_split "eh_return"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "ab")]
		    UNSPECV_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&ab"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  {
    c6x_set_return_address (operands[0], operands[1]);
    DONE;
  }"
)

;; -------------------------------------------------------------------------
;; Doloop
;; -------------------------------------------------------------------------

; operand 0 is the loop count pseudo register
; operand 1 is the number of loop iterations or 0 if it is unknown
; operand 2 is the maximum number of loop iterations
; operand 3 is the number of levels of enclosed loops
; operand 4 is the label to jump to at the top of the loop
(define_expand "doloop_end"
  [(parallel [(set (pc) (if_then_else
			  (ne (match_operand:SI 0 "" "")
			      (const_int 1))
			  (label_ref (match_operand 4 "" ""))
			  (pc)))
	      (set (match_dup 0)
		   (plus:SI (match_dup 0)
			    (const_int -1)))
	      (clobber (match_scratch:SI 5 ""))])]
  "TARGET_INSNS_64PLUS && optimize"
{
  /* The loop optimizer doesn't check the predicates... */
  if (GET_MODE (operands[0]) != SImode)
    FAIL;
})

(define_insn "mvilc"
  [(set (reg:SI REG_ILC)
	(unspec [(match_operand:SI 0 "register_operand" "a,b")] UNSPEC_MVILC))]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tmvc\\t%$\\t%0, ILC"
  [(set_attr "predicable" "no")
   (set_attr "cross" "y,n")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "b")
   (set_attr "type" "mvilc")])
  
(define_insn "sploop"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "i")
		     (reg:SI REG_ILC)]
		    UNSPECV_SPLOOP)]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tsploop\t%0"
  [(set_attr "predicable" "no")
   (set_attr "type" "sploop")])
  
(define_insn "spkernel"
  [(set (pc)
	(if_then_else
	 (ne (unspec_volatile:SI
	      [(match_operand:SI 0 "const_int_operand" "i")
	       (match_operand:SI 1 "const_int_operand" "i")]
	      UNSPECV_SPKERNEL)
	     (const_int 1))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tspkernel\t%0, %1"
  [(set_attr "predicable" "no")
   (set_attr "type" "spkernel")])
  
(define_insn "loop_end"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 3 "nonimmediate_operand" "0,0,0,*r")
			  (const_int 1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=AB,*r,m,m")
	(plus:SI (match_dup 3)
		 (const_int -1)))
   (clobber (match_scratch:SI 2 "=X,&AB,&AB,&AB"))]
  "TARGET_INSNS_64PLUS && optimize"
  "#"
  [(set_attr "type" "spkernel")])

(define_split
  [(set (pc)
	(if_then_else (ne (match_operand:SI 3 "nonimmediate_operand" "")
			  (const_int 1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "memory_operand" "")
	(plus:SI (match_dup 3)
		 (const_int -1)))
   (clobber (match_scratch 2))]
  ""
  [(set (match_dup 2) (plus:SI (match_dup 3) (const_int -1)))
   (set (match_dup 0) (match_dup 2))
   (set (pc)
	(if_then_else (ne (match_dup 2) (const_int 0))
		      (label_ref (match_dup 1))
		      (pc)))]
{
  if (!REG_P (operands[3]))
    {
      emit_move_insn (operands[2], operands[3]);
      operands[3] = operands[2];
    }
})

;; -------------------------------------------------------------------------
;; Delayed-branch real jumps and shadows
;; -------------------------------------------------------------------------

(define_insn "real_jump"
  [(unspec [(match_operand 0 "c6x_jump_operand" "a,b,s") (const_int 0)]
	   UNSPEC_REAL_JUMP)]
  ""
{
  if (GET_CODE (operands[0]) == LABEL_REF)
    return "%|%.\\tb\\t%$\\t%l0";
  return "%|%.\\tb\\t%$\\t%0";
}
  [(set_attr "type" "branch")
   (set_attr "has_shadow" "y")
   (set_attr "units" "s")
   (set_attr "cross" "y,n,n")
   (set_attr "dest_regfile" "b,b,any")])

(define_insn "real_call"
  [(unspec [(match_operand 0 "c6x_call_operand" "a,b,S1") (const_int 1)]
	   UNSPEC_REAL_JUMP)
   (clobber (reg:SI REG_B3))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "has_shadow" "y")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "cross" "y,n,n")
   (set_attr "dest_regfile" "b,b,any")])

(define_insn "real_ret"
  [(unspec [(match_operand 0 "register_operand" "a,b") (const_int 2)]
	   UNSPEC_REAL_JUMP)]
  ""
  "%|%.\\tret\\t%$\\t%0"
  [(set_attr "type" "branch")
   (set_attr "has_shadow" "y")
   (set_attr "units" "s")
   (set_attr "cross" "y,n")
   (set_attr "dest_regfile" "b")])

;; computed_jump_p returns true if it finds a constant; so use one in the
;; unspec.
(define_insn "indirect_jump_shadow"
  [(set (pc) (unspec [(const_int 1)] UNSPEC_JUMP_SHADOW))]
  ""
  ";; indirect jump occurs"
  [(set_attr "type" "shadow")])

;; Operand 0 may be a PARALLEL which isn't handled by output_operand, so
;; we don't try to print it.
(define_insn "indirect_call_value_shadow"
  [(set (match_operand 0 "" "")
	(call (unspec [(pc)] UNSPEC_JUMP_SHADOW)
	      (const_int 0)))]
  ""
  ";; indirect call occurs, with return value"
  [(set_attr "type" "shadow")])

(define_insn "indirect_sibcall_shadow"
  [(call (unspec [(pc)] UNSPEC_JUMP_SHADOW)
	 (const_int 0))]
  "SIBLING_CALL_P (insn)"
  ";; indirect sibcall occurs"
  [(set_attr "type" "shadow")])

(define_insn "indirect_call_shadow"
  [(call (unspec [(pc)] UNSPEC_JUMP_SHADOW)
	 (const_int 0))]
  ""
  ";; indirect call occurs"
  [(set_attr "type" "shadow")])

(define_insn "call_value_shadow"
  [(set (match_operand 0 "" "")
	(call (unspec [(match_operand 1 "" "")] UNSPEC_JUMP_SHADOW)
	      (const_int 0)))]
  ""
  ";; call to %1 occurs, with return value"
  [(set_attr "type" "shadow")])

(define_insn "call_shadow"
  [(call (unspec [(match_operand 0 "" "")] UNSPEC_JUMP_SHADOW)
	 (const_int 0))]
  "!SIBLING_CALL_P (insn)"
  ";; call to %0 occurs"
  [(set_attr "type" "shadow")])

(define_insn "sibcall_shadow"
  [(call (unspec [(match_operand 0 "" "")] UNSPEC_JUMP_SHADOW)
	 (const_int 0))]
  "SIBLING_CALL_P (insn)"
  ";; sibcall to %0 occurs"
  [(set_attr "type" "shadow")])

(define_insn "jump_shadow"
  [(set (pc) (unspec [(match_operand 0 "" "")] UNSPEC_JUMP_SHADOW))]
  ""
  ";; jump to %0 occurs"
  [(set_attr "type" "shadow")])

(define_insn "condjump_shadow"
  [(set (pc)
	(if_then_else (eq (unspec [(const_int 0)] UNSPEC_JUMP_SHADOW)
			  (const_int 0))
		      (match_operand 0 "" "")
		      (pc)))]
  ""
  ";; condjump to %0 occurs"
  [(set_attr "type" "shadow")])

(define_insn "return_shadow"
  [(unspec [(const_int 0)] UNSPEC_JUMP_SHADOW)
   (return)]
  ""
  ";; return occurs"
  [(set_attr "type" "shadow")])

;; -------------------------------------------------------------------------
;; Add instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"
              "=a   ,b   , a, b, a, b,    a,    b, ab,  a,  b,  a,  b,ab")
    (plus:SI (match_operand:SI 1 "register_operand"
              "%a   ,b   , a, b, b, a,    b,    a,  0,  a,  b,  z,  z,0")
  	     (match_operand:SI 2 "addsi_operand"
               "aIs5,bIs5,?b,?a,?a,?b,?aIs5,?bIs5,I5x,I5x,I5x,Iux,Iux,IsB")))]
  ""
{
  if (CONSTANT_P (operands[2]))
    {
      HOST_WIDE_INT val = INTVAL (operands[2]);

      if (c6x_get_unit_specifier (insn) == 'd')
	{
	  bool issp = (TARGET_INSNS_64PLUS
		       && operands[1] == stack_pointer_rtx
		       && GET_CODE (PATTERN (insn)) != COND_EXEC);

	  if (get_attr_cross (insn) == CROSS_N)
	    {
	      if (satisfies_constraint_Iu5 (operands[2]))
		return "%|%.\\tadd\\t%$\\t%1, %2, %0";
	      else if (satisfies_constraint_In5 (operands[2]))
		return "%|%.\\tsub\\t%$\\t%1, %n2, %0";
	    }

	  if (issp && val > 0 && val < 32768)
	    {
	      return "%|%.\\taddab\\t%$\\t%1, %2, %0";
	    }
	  if ((val & 1) == 0 && ((val >= -62 && val <= 62)
				 || (issp && val > 0 && val < 65536)))
	    {
	      if (val < 0)
		return "%|%.\\tsubah\\t%$\\t%1, %r2, %0";
	      else
		return "%|%.\\taddah\\t%$\\t%1, %r2, %0";
	    }
	  else if ((val & 3) == 0 && ((val >= -124 && val <= 124)
				       || (issp && val > 0 && val < 131072)))
	    {
	      if (val < 0)
		return "%|%.\\tsubaw\\t%$\\t%1, %R2, %0";
	      else
		return "%|%.\\taddaw\\t%$\\t%1, %R2, %0";
	    }
	  else if ((val & 7) == 0 && val > 0 && val <= 248)
	    {
	      rtx xop[3];
	      xop[0] = operands[0];
	      xop[1] = operands[1];
	      xop[2] = GEN_INT (val >> 3);
	      output_asm_insn ("%|%.\\taddad\\t%$\\t%1, %2, %0", xop);
	      return "";
	    }
	}
      else
        {
	  if (satisfies_constraint_Is5 (operands[2]))
	    return "%|%.\\tadd\\t%$\\t%2, %1, %0";
	}
      gcc_assert (rtx_equal_p (operands[0], operands[1]));
      return "%|%.\\taddk\\t%$\\t%2, %0";
    }
  if (which_alternative == 4 || which_alternative == 5)
    return "%|%.\\tadd\\t%$\\t%2, %1, %0";
  else
    return "%|%.\\tadd\\t%$\\t%1, %2, %0";
}
  [(set_attr "units62" "dls,dls,ls,ls,ls,ls,ls,ls,s,d,d,*,*,s")
   (set_attr "units67" "dls,dls,ls,ls,ls,ls,ls,ls,ds,d,d,*,*,s")
   (set_attr "units64" "dls,dls,dls,dls,dls,dls,ls,ls,ds,d,d,d,d,s")
   (set_attr "cross" "n,n,y,y,y,y,y,y,n,n,n,y,n,n")
   (set_attr "predicable" "yes,yes,yes,yes,yes,yes,yes,yes,yes,yes,yes,no,no,yes")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b,a,b")
	(minus:SI (match_operand:SI 1 "reg_or_scst5_operand" "a,b,aIs5,bIs5,bIs5,aIs5")
		  (match_operand:SI 2 "register_operand" "a,b,a,b,?a,?b")))]
  ""
  "%|%.\\tsub\\t%$\\t%1, %2, %0"
  [(set_attr "units62" "dls,dls,ls,ls,l,l")
   (set_attr "units64" "dls,dls,ls,ls,ls,ls")
   (set_attr "cross" "n,n,n,n,y,y")])

(define_insn "*addshiftsi"
  [(set (match_operand:SI 0 "register_operand" "=a,b")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "a,b")
			  (match_operand:SI 3 "adda_scale_operand" "n,n"))
		 (match_operand:SI 1 "register_operand" "a,b")))]
  ""
  "%|%.\\tadda%d3\\t%$\\t%1, %2, %0"
  [(set_attr "units" "d")])

(define_insn "*subshiftsi"
  [(set (match_operand:SI 0 "register_operand" "=a,b")
	(minus:SI (match_operand:SI 1 "register_operand" "a,b")
		  (mult:SI (match_operand:SI 2 "register_operand" "a,b")
			   (match_operand:SI 3 "suba_scale_operand" "n,n"))))]
  ""
  "%|%.\\tsuba%d3\\t%$\\t%1, %2, %0"
  [(set_attr "units" "d")])

(define_insn "addsidi3_widen"
  [(set (match_operand:DI 0 "register_operand" "=a,b,a,b")
	(plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "%a,b,a,b"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "a,b,?b,?a"))))]
  ""
  "%|%.\\taddu\\t%$\\t%1, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "register_operand" "")))]
  ""
{
  rtx tmp;
  rtx lo_half[3], hi_half[3];
  split_di (operands + 1, 2, lo_half + 1, hi_half + 1);
  if (reg_overlap_mentioned_p (operands[0], hi_half[1])
      || reg_overlap_mentioned_p (operands[0], hi_half[2]))
    tmp = gen_reg_rtx (DImode);
  else
    tmp = operands[0];
  split_di (&tmp, 1, lo_half, hi_half);
  emit_insn (gen_addsidi3_widen (tmp, lo_half[1], lo_half[2]));
  emit_insn (gen_addsi3 (hi_half[0], copy_rtx (hi_half[0]), hi_half[1]));
  emit_insn (gen_addsi3 (copy_rtx (hi_half[0]),
			 copy_rtx (hi_half[0]), hi_half[2]));
  if (tmp != operands[0])
    emit_move_insn (operands[0], tmp);
  DONE;
})

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=a,b,a,b")
	(plus:SF (match_operand:SF 1 "register_operand" "%a,b,a,b")
		 (match_operand:SF 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\taddsp\\t%$\\t%1, %2, %0"
  [(set_attr "type" "fp4")
   (set_attr "units67" "l")
   (set_attr "units67p" "ls")
   (set_attr "units674" "ls")
   (set_attr "cross" "n,n,y,y")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(plus:DF (match_operand:DF 1 "register_operand" "%a,b,a,b")
		 (match_operand:DF 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tadddp\\t%$\\t%1, %2, %0"
  [(set_attr "type" "adddp")
   (set_attr "units67" "l")
   (set_attr "units67p" "ls")
   (set_attr "units674" "ls")
   (set_attr "cross" "n,n,y,y")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=a,b, a, b, a, b")
	(minus:SF (match_operand:SF 1 "register_operand" "a,b, b, a, a, b")
		  (match_operand:SF 2 "register_operand" "a,b,?a,?b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tsubsp\\t%$\\t%1, %2, %0"
  [(set_attr "type" "fp4")
   (set_attr "units67" "l")
   (set_attr "units67p" "ls")
   (set_attr "units674" "ls")
   (set_attr "cross" "n,n,y,y,y,y")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=a,b, a, b, a, b")
	(minus:DF (match_operand:DF 1 "register_operand" "a,b, b, a, a, b")
		  (match_operand:DF 2 "register_operand" "a,b,?a,?b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tsubdp\\t%$\\t%1, %2, %0"
  [(set_attr "type" "adddp")
   (set_attr "units67" "l")
   (set_attr "units67p" "ls")
   (set_attr "units674" "ls")
   (set_attr "cross" "n,n,y,y,y,y")])

;; -------------------------------------------------------------------------
;; Logical instructions
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b,a,b")
	(and:SI (match_operand:SI 1 "register_operand" "%a,b,b,a,a,b")
		(match_operand:SI 2 "andsi_operand" "aIs5,bIs5,?aIs5,?bIs5,aJc,bJc")))]
  ""
{
  if (which_alternative < 4)
    return "%|%.\\tand\\t%$\\t%2, %1, %0";
  else
    return "%|%.\\tclr\\t%$\\t%1, %f2, %F2, %0";
}
  [(set_attr "units62" "ls,ls,ls,ls,s,s")
   (set_attr "units64" "dls,dls,dls,dls,s,s")
   (set_attr "cross" "n,n,y,y,n,n")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b,a,b")
	(ior:SI (match_operand:SI 1 "register_operand" "%a,b,b,a,a,b")
		(match_operand:SI 2 "iorsi_operand" "aIs5,bIs5,?aIs5,?bIs5,aJs,bJs")))]
  ""
{
  if (which_alternative < 4)
    return "%|%.\\tor\\t%$\\t%2, %1, %0";
  else
    return "%|%.\\tset\\t%$\\t%1, %s2, %S2, %0";
}
  [(set_attr "units62" "ls,ls,ls,ls,s,s")
   (set_attr "units64" "dls,dls,dls,dls,s,s")
   (set_attr "cross" "n,n,y,y,n,n")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(xor:SI (match_operand:SI 1 "register_operand" "%a,b,b,a")
		(match_operand:SI 2 "reg_or_scst5_operand" "aIs5,bIs5,?aIs5,?bIs5")))]
  ""
  "%|%.\\txor\\t%$\\t%2, %1, %0"
  [(set_attr "units62" "ls")
   (set_attr "units64" "dls")
   (set_attr "cross" "n,n,y,y")])

;; -------------------------------------------------------------------------
;; Conversions
;; -------------------------------------------------------------------------

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(float_extend:DF (match_operand:SF 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tspdp\\t%$\\t%1,%0"
  [(set_attr "type" "dp2")
   (set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=a,b")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "a,b")))]
  "TARGET_FP"
  "%|%.\\tdpsp\\t%$\\t%1,%0"
  [(set_attr "type" "fp4")
   (set_attr "units" "l")
   (set_attr "cross" "n")])

;;;; Convert between signed integer types and floating point.
(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=a,b,a,b")
	(float:SF (match_operand:SI 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tintsp\\t%$\\t%1,%0"
  [(set_attr "type" "fp4")
   (set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=a,b,a,b")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tintspu\\t%$\\t%1,%0"
  [(set_attr "type" "fp4")
   (set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(float:DF (match_operand:SI 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tintdp\\t%$\\t%1,%0"
  [(set_attr "type" "intdp")
   (set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "floatunssidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(unsigned_float:DF (match_operand:SI 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tintdpu\\t%$\\t%1,%0"
  [(set_attr "type" "intdp")
   (set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(fix:SI (match_operand:SF 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP"
  "%|%.\\tsptrunc\\t%$\\t%1,%0"
  [(set_attr "type" "fp4")
   (set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=a,b")
	(fix:SI (match_operand:DF 1 "register_operand" "a,b")))]
  "TARGET_FP"
  "%|%.\\tdptrunc\\t%$\\t%1,%0"
  [(set_attr "type" "fp4")
   (set_attr "units" "l")
   (set_attr "cross" "n")])

;; -------------------------------------------------------------------------
;; Saturating arithmetic
;; -------------------------------------------------------------------------

(define_insn "saddsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b,a,b,a,b")
	(ss_plus:SI (match_operand:SI 1 "register_operand" "a,b,?b,?a,a,b,?b,?a")
		    (match_operand:SI 2 "reg_or_const_int_operand" "a,b,a,b,aIs5,bIs5,aIs5,bIs5")))]
  ""
  "%|%.\\tsadd\\t%$\\t%2, %1, %0"
  [(set_attr "units" "ls,ls,ls,ls,l,l,l,l")
   (set_attr "cross" "n,n,y,y,n,n,y,y")])

(define_insn "ssubsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(ss_minus:SI (match_operand:SI 1 "reg_or_scst5_operand" "aIs5,bIs5,?bIs5,?aIs5")
		     (match_operand:SI 2 "register_operand" "a,b,a,b")))]
  ""
  "%|%.\\tssub\\t%$\\t%1, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "subcsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(unspec:SI
	 [(match_operand:SI 1 "register_operand" "a,b,a,b")
	  (match_operand:SI 2 "register_operand" "a,b,?b,?a")]
	 UNSPEC_SUBC))]
  ""
  "%|%.\\tsubc\\t%$\\t%1, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

;; -------------------------------------------------------------------------
;; Call instructions
;; -------------------------------------------------------------------------

(define_expand "call"
 [(match_operand 0 "" "")]
 ""
{
  c6x_expand_call (NULL_RTX, operands[0], false);
  DONE;
})

(define_expand "call_value"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")]
 ""
{
  c6x_expand_call (operands[0], operands[1], false);
  DONE;
})

(define_expand "sibcall"
 [(match_operand 0 "" "")]
 ""
{
  c6x_expand_call (NULL_RTX, operands[0], true);
  cfun->machine->contains_sibcall = true;
  DONE;
})

(define_expand "sibcall_value"
  [(match_operand 0 "" "")
   (match_operand 1 "" "")]
 ""
{
  c6x_expand_call (operands[0], operands[1], true);
  cfun->machine->contains_sibcall = true;
  DONE;
})

(define_insn "call_internal"
  [(call (mem (match_operand:SI 0 "c6x_call_operand" "S1,a,b"))
	 (const_int 0))]
  "!SIBLING_CALL_P (insn)"
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any,b,b")
   (set_attr "cross" "n,y,n")])

(define_insn "call_value_internal"
  [(set (match_operand 0 "" "")
	(call (mem (match_operand:SI 1 "c6x_call_operand" "S1,a,b"))
	      (const_int 0)))]
  ""
  "%|%.\\tcall\\t%$\\t%1"
  [(set_attr "type" "call")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any,b,b")
   (set_attr "cross" "n,y,n")])

(define_insn "sibcall_internal"
  [(call (mem (match_operand:SI 0 "c6x_call_operand" "S1,C"))
	 (const_int 0))]
  "SIBLING_CALL_P (insn)"
  "%|%.\\tb\\t%$\\t%0"
  [(set_attr "type" "branch")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "any,b")])

(define_insn "callp"
  [(call (mem (match_operand:SI 0 "c6x_call_operand" "S1"))
	 (const_int 0))
   (unspec [(const_int 6)] UNSPEC_NOP)]
  "!SIBLING_CALL_P (insn)"
  "%|%.\\tcallp\\t%$\\t%0, B3"
  [(set_attr "type" "callp")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "b")
   (set_attr "cross" "n")])

(define_insn "callp_value"
  [(set (match_operand:SI 0 "register_operand" "")
	(call (mem (match_operand:SI 1 "c6x_call_operand" "S1"))
	      (const_int 0)))
   (unspec [(const_int 6)] UNSPEC_NOP)]
  "!SIBLING_CALL_P (insn)"
  "%|%.\\tcallp\\t%$\\t%1, B3"
  [(set_attr "type" "callp")
   (set_attr "predicable" "no")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "b")
   (set_attr "cross" "n")])

(define_insn "return_internal"
  [(return)
   (use (match_operand:SI 0 "register_operand" "b"))]
  "reload_completed"
  "%|%.\\tret\\t%$\\t%0"
  [(set_attr "type" "branch")
   (set_attr "units" "s")
   (set_attr "dest_regfile" "b")])

(define_insn "addkpc"
  [(set (match_operand:SI 0 "register_operand" "=b")
	(unspec:SI [(match_operand 1 "" "")] UNSPEC_ADDKPC))
   (unspec [(match_operand 2 "const_int_operand" "n")] UNSPEC_NOP)]
  "TARGET_INSNS_64"
  "%|%.\\taddkpc\\t%$\\t%l1, %0, %2"
  [(set_attr "units" "s")
   (set_attr "dest_regfile" "b")])

;; -------------------------------------------------------------------------
;; Unary operations
;; -------------------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=a, a, b, b")
        (neg:SI (match_operand:SI 1 "register_operand" "a,?b, b,?a")))]
  ""
  "%|%.\\tneg\\t%$\\t%1, %0"
  [(set_attr "units" "ls")
   (set_attr "cross" "n,y,n,y")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=a, a, b, b")
	(not:SI (match_operand:SI 1 "register_operand" "a,?b, b,?a")))]
  ""
  "%|%.\\tnot\\t%$\\t%1, %0"
  [(set_attr "units" "ls")
   (set_attr "cross" "n,y,n,y")])

(define_insn "clrsbsi2"
  [(set (match_operand:SI 0 "register_operand" "=a, a, b, b")
	(clrsb:SI (match_operand:SI 1 "register_operand" "a,?b, b,?a")))]
  ""
  "%|%.\\tnorm\\t%$\\t%1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,y,n,y")])

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=a, a, b, b")
	(clz:SI (match_operand:SI 1 "register_operand" "a,?b, b,?a")))]
  ""
  "%|%.\\tlmbd\\t%$\\t1, %1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,y,n,y")])

;; bitrevsi2 is defined in c6x-mult.md.in.

(define_expand "ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(ctz:SI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_INSNS_64"
{
  rtx tmpreg = gen_reg_rtx (SImode);
  emit_insn (gen_bitrevsi2 (tmpreg, operands[1]));
  emit_insn (gen_clzsi2 (operands[0], tmpreg));
  DONE;
})

(define_expand "ctzdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(ctz:DI (match_operand:DI 1 "register_operand" "")))]
  "TARGET_INSNS_64"
{
  rtx tmpreg = gen_reg_rtx (DImode);
  rtx out;
  emit_insn (gen_bitrevsi2 (gen_highpart (SImode, tmpreg),
			    gen_lowpart (SImode, operands[1])));
  emit_insn (gen_bitrevsi2 (gen_lowpart (SImode, tmpreg),
			    gen_highpart (SImode, operands[1])));
  out = expand_unop (DImode, clz_optab, tmpreg, operands[0], 1);
  if (!rtx_equal_p (out, operands[0]))
    emit_move_insn (operands[0], out);
  DONE;
})

(define_insn "ssabssi2"
  [(set (match_operand:SI 0 "register_operand" "=a, a, b, b")
        (ss_abs:SI (match_operand:SI 1 "register_operand" "a,?b, b,?a")))]
  ""
  "%|%.\\tabs\\t%$\\t%1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,y,n,y")])

;; -------------------------------------------------------------------------
;; Shift instructions
;; -------------------------------------------------------------------------

(define_code_iterator any_shift [ss_ashift ashift ashiftrt lshiftrt])
(define_code_iterator any_rshift [ashiftrt lshiftrt])
(define_code_attr shift_code [(ss_ashift "ss_ashl") (ashift "ashl")
			      (ashiftrt "ashr") (lshiftrt "lshr")])
(define_code_attr shift_insn [(ss_ashift "sshl") (ashift "shl")
			      (ashiftrt "shr") (lshiftrt "shru")])

(define_insn "<shift_code>si3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (any_shift:SI (match_operand:SI 1 "register_operand" "a,b,?b,?a")
		      (match_operand:SI 2 "reg_or_ucst5_operand" "aIu5,bIu5,aIu5,bIu5")))]
  ""
  "%|%.\\t<shift_insn>\\t%$\\t%1, %2, %0"
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

;; See c6x-mult.md.in for the rotlsi3 pattern.

(define_insn "rotrdi3_16"
  [(set (match_operand:DI 0 "register_operand" "=a,b")
        (rotatert:DI (match_operand:DI 1 "register_operand" "a,b")
		     (const_int 16)))]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tdpackx2\\t%$\\t%P1, %p1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n")])

(define_insn "shlmbsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "a,b,?b,?a")
			   (const_int 8))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "a,b,a,b")
			     (const_int 24))))]
  "TARGET_INSNS_64"
  "%|%.\\tshlmb\\t%$\\t%2, %1, %0"
  [(set_attr "units" "ls")
   (set_attr "cross" "n,n,y,y")])

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "")
        (ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_INSNS_64"
{
  if (CONST_INT_P (operands[2]) && INTVAL (operands[2]) == 8)
    {
      rtx lo0, lo1, hi0, hi1, tmp;
      lo0 = gen_lowpart (SImode, operands[0]);
      hi0 = gen_highpart (SImode, operands[0]);
      lo1 = gen_lowpart (SImode, operands[1]);
      hi1 = gen_highpart (SImode, operands[1]);
      if (reg_overlap_mentioned_p (hi0, lo1))
        tmp = gen_reg_rtx (SImode);
      else
        tmp = hi0;
      emit_insn (gen_shlmbsi3 (tmp, hi1, lo1));
      emit_insn (gen_ashlsi3 (lo0, lo1, operands[2]));
      if (tmp != hi0)
        emit_move_insn (hi0, tmp);
      DONE;
    }
  FAIL;
})

(define_expand "rotrdi3"
  [(set (match_operand:DI 0 "register_operand" "")
        (rotatert:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_INSNS_64PLUS"
{
  if (CONST_INT_P (operands[2]) && INTVAL (operands[2]) == 16)
    {
      emit_insn (gen_rotrdi3_16 (operands[0], operands[1]));
      DONE;
    }
  FAIL;
})

(define_insn "bswapv2hi2"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
        (bswap:V2HI (match_operand:V2HI 1 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tswap4\\t%$\\t%1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(bswap:SI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_INSNS_64"
{
  rtx tmpreg = gen_reg_rtx (SImode);
  rtx tmpv2 = gen_lowpart (V2HImode, tmpreg);
  rtx op0v2 = gen_lowpart (V2HImode, operands[0]);
  emit_insn (gen_rotlsi3 (tmpreg, operands[1], GEN_INT (16)));
  emit_insn (gen_bswapv2hi2 (op0v2, tmpv2));
  DONE;
})

;; -------------------------------------------------------------------------
;; Division
;; -------------------------------------------------------------------------

(define_insn "divsi3_insn"
  [(set (reg:SI REG_A4) (div:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B5))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_divi"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "divsi3_insn_indcall"
  [(set (reg:SI REG_A4) (div:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B5))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "udivsi3_insn"
  [(set (reg:SI REG_A4) (udiv:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_divu"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "udivsi3_insn_indcall"
  [(set (reg:SI REG_A4) (udiv:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "modsi3_insn"
  [(set (reg:SI REG_A4) (mod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_remi"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "modsi3_insn_indcall"
  [(set (reg:SI REG_A4) (mod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "divmodsi4_insn"
  [(set (reg:SI REG_A4) (div:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (set (reg:SI REG_A5) (mod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_divremi"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "divmodsi4_insn_indcall"
  [(set (reg:SI REG_A4) (div:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (set (reg:SI REG_A5) (mod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "umodsi3_insn"
  [(set (reg:SI REG_A4) (umod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A7))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_remu"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "umodsi3_insn_indcall"
  [(set (reg:SI REG_A4) (umod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A7))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "udivmodsi4_insn"
  [(set (reg:SI REG_A4) (udiv:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (set (reg:SI REG_A5) (umod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t__c6xabi_divremu"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "udivmodsi4_insn_indcall"
  [(set (reg:SI REG_A4) (udiv:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (set (reg:SI REG_A5) (umod:SI (reg:SI REG_A4) (reg:SI REG_B4)))
   (use (match_operand:SI 0 "register_operand" "b"))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "%|%.\\tcall\\t%$\\t%0"
  [(set_attr "type" "call")
   (set_attr "dest_regfile" "any")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn_and_split "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(div:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "register_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(mod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A4))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B5))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "#"
  ""
  [(const_int 0)]
{
  rtx reg = NULL_RTX;

  if (TARGET_LONG_CALLS)
    {
      if (reload_completed)
	reg = gen_rtx_REG (SImode, REG_A6);
      else
        reg = gen_reg_rtx (SImode);
    }
  emit_move_insn (gen_rtx_REG (SImode, REG_A4), operands[1]);
  emit_move_insn (gen_rtx_REG (SImode, REG_B4), operands[2]);
  if (find_reg_note (curr_insn, REG_UNUSED, operands[3]))
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (sdiv_optab, SImode));
	  emit_insn (gen_divsi3_insn_indcall (reg));
	}
      else
        emit_insn (gen_divsi3_insn ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, REG_A4));
    }
  else if (find_reg_note (curr_insn, REG_UNUSED, operands[0]))
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (smod_optab, SImode));
	  emit_insn (gen_modsi3_insn_indcall (reg));
	}
      else
        emit_insn (gen_modsi3_insn ());
      emit_move_insn (operands[3], gen_rtx_REG (SImode, REG_A4));
    }
  else
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (sdivmod_optab, SImode));
	  emit_insn (gen_divmodsi4_insn_indcall (reg));
	}
      else
        emit_insn (gen_divmodsi4_insn ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, REG_A4));
      emit_move_insn (operands[3], gen_rtx_REG (SImode, REG_A5));
    }
  DONE;
})

(define_insn_and_split "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(udiv:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "register_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(umod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI REG_A0))
   (clobber (reg:SI REG_A1))
   (clobber (reg:SI REG_A2))
   (clobber (reg:SI REG_A4))
   (clobber (reg:SI REG_A5))
   (clobber (reg:SI REG_A6))
   (clobber (reg:SI REG_A7))
   (clobber (reg:SI REG_B0))
   (clobber (reg:SI REG_B1))
   (clobber (reg:SI REG_B2))
   (clobber (reg:SI REG_B3))
   (clobber (reg:SI REG_B4))
   (clobber (reg:SI REG_B30))
   (clobber (reg:SI REG_B31))]
  ""
  "#"
  ""
  [(const_int 0)]
{
  rtx reg = NULL_RTX;

  if (TARGET_LONG_CALLS)
    {
      if (reload_completed)
	reg = gen_rtx_REG (SImode, REG_A6);
      else
        reg = gen_reg_rtx (SImode);
    }

  emit_move_insn (gen_rtx_REG (SImode, REG_A4), operands[1]);
  emit_move_insn (gen_rtx_REG (SImode, REG_B4), operands[2]);
  if (find_reg_note (curr_insn, REG_UNUSED, operands[3]))
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (udiv_optab, SImode));
	  emit_insn (gen_udivsi3_insn_indcall (reg));
	}
      else
        emit_insn (gen_udivsi3_insn ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, REG_A4));
    }
  else if (find_reg_note (curr_insn, REG_UNUSED, operands[0]))
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (umod_optab, SImode));
	  emit_insn (gen_umodsi3_insn_indcall (reg));
	}
      else
        emit_insn (gen_umodsi3_insn ());
      emit_move_insn (operands[3], gen_rtx_REG (SImode, REG_A4));
    }
  else
    {
      if (TARGET_LONG_CALLS)
	{
	  emit_move_insn (reg, optab_libfunc (udivmod_optab, SImode));
	  emit_insn (gen_udivmodsi4_insn_indcall (reg));
	}
      else
        emit_insn (gen_udivmodsi4_insn ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, REG_A4));
      emit_move_insn (operands[3], gen_rtx_REG (SImode, REG_A5));
    }
  DONE;
})

;; -------------------------------------------------------------------------
;; Multiplication
;; See c6x-mult.md.in for define_insn patterns.
;; -------------------------------------------------------------------------

(define_expand "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" ""))
                 (sign_extend:SI (match_operand:HI 2 "reg_or_scst5_operand" ""))))]
  ""
{
  if (CONSTANT_P (operands[2]))
    {
      emit_insn (gen_mulhisi3_const (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_expand "usmulhisi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" ""))
                 (sign_extend:SI (match_operand:HI 2 "reg_or_scst5_operand" ""))))]
 ""
{
  if (CONSTANT_P (operands[2]))
    {
      emit_insn (gen_usmulhisi3_const (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
{
  if (!TARGET_MPY32)
    {
      rtx lo1 = gen_lowpart (HImode, operands[1]);
      rtx lo2 = gen_lowpart (HImode, operands[2]);
      /*   (N * AH + AL) * (N * BH + BL)
         = N*(AH * BL + BH * AL) + AL*BL  */
      rtx tmp1 = gen_reg_rtx (SImode);
      rtx tmp2 = gen_reg_rtx (SImode);
      rtx tmp3 = gen_reg_rtx (SImode);
      emit_insn (gen_umulhisi3 (tmp1, lo1, lo2));
      emit_insn (gen_umulhisi3_lh (tmp2, lo1, operands[2]));
      emit_insn (gen_umulhisi3_hl (tmp3, operands[1], lo2));
      emit_insn (gen_addsi3 (tmp2, tmp2, tmp3));
      emit_insn (gen_ashlsi3 (tmp2, tmp2, GEN_INT (16)));
      emit_insn (gen_addsi3 (operands[0], tmp1, tmp2));
      DONE;
    }
})

;; -------------------------------------------------------------------------
;; Floating point multiplication
;; -------------------------------------------------------------------------

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=a,b,a,b")
	(mult:SF (match_operand:SF 1 "register_operand" "%a,b,?a,?b")
		 (match_operand:SF 2 "register_operand" "a,b,b,a")))]
  "TARGET_FP"
  "%|%.\\tmpysp\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpy4")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=a,b")
	(mult:DF (match_operand:DF 1 "register_operand" "%a,b")
		 (match_operand:DF 2 "register_operand" "a,b")))]
  "TARGET_FP"
  "%|%.\\tmpydp\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpydp")
  (set_attr "units" "m")
  (set_attr "cross" "n")])

;; Note that mpyspdp and mpysp2dp are available on C67x, despite what the
;; manual says.
(define_insn "*muldf_ext1"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(mult:DF (float_extend:DF (match_operand:SF 1 "register_operand" "a,b,a,b"))
		 (match_operand:DF 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_FP_EXT"
  "%|%.\\tmpyspdp\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpyspdp")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "*muldf_ext2"
  [(set (match_operand:DF 0 "register_operand" "=a,b,a,b")
	(mult:DF (float_extend:DF (match_operand:SF 1 "register_operand" "%a,b,a,b"))
		 (float_extend:DF (match_operand:SF 2 "register_operand" "a,b,?b,?a"))))]
  "TARGET_FP_EXT"
  "%|%.\\tmpysp2dp\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpysp2dp")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

;; -------------------------------------------------------------------------
;; Floating point division
;; -------------------------------------------------------------------------

(define_insn "rcpsf2"
  [(set (match_operand:SF 0 "register_operand" "=a,b,a,b")
	(unspec:SF [(match_operand:SF 1 "register_operand" "a,b,?b,?a")]
		   UNSPEC_RCP))]
  "TARGET_FP"
  "%|%.\\trcpsp\\t%$\\t%1, %0"
 [(set_attr "units" "s")
  (set_attr "cross" "n,n,y,y")])

(define_insn "rcpdf2"
  [(set (match_operand:DF 0 "register_operand" "=a,b")
	(unspec:DF [(match_operand:DF 1 "register_operand" "a,b")]
		   UNSPEC_RCP))]
  "TARGET_FP"
  "%|%.\\trcpdp\\t%$\\t%1, %0"
 [(set_attr "type" "dp2")
  (set_attr "units" "s")
  (set_attr "cross" "n")])

(define_expand "divsf3"
  [(set (match_dup 4)
	(unspec:SF [(match_operand:SF 2 "register_operand" "")]
		   UNSPEC_RCP))
   (set (match_dup 5) (mult:SF (match_dup 2) (match_dup 4)))
   (set (match_dup 6) (minus:SF (match_dup 3) (match_dup 5)))
   (set (match_dup 4) (mult:SF (match_dup 4) (match_dup 6)))
   (set (match_dup 5) (mult:SF (match_dup 2) (match_dup 4)))
   (set (match_dup 6) (minus:SF (match_dup 3) (match_dup 5)))
   (set (match_dup 4) (mult:SF (match_dup 4) (match_dup 6)))
   (set (match_operand:SF 0 "register_operand" "")
	(mult:SF (match_operand:SF 1 "register_operand")
		 (match_dup 4)))]
  "TARGET_FP && flag_reciprocal_math"
{
  operands[3] = force_reg (SFmode,
			   CONST_DOUBLE_FROM_REAL_VALUE (dconst2, SFmode));
  operands[4] = gen_reg_rtx (SFmode);
  operands[5] = gen_reg_rtx (SFmode);
  operands[6] = gen_reg_rtx (SFmode);
})

(define_expand "divdf3"
  [(set (match_dup 4)
	(unspec:DF [(match_operand:DF 2 "register_operand" "")]
		   UNSPEC_RCP))
   (set (match_dup 5) (mult:DF (match_dup 2) (match_dup 4)))
   (set (match_dup 6) (minus:DF (match_dup 3) (match_dup 5)))
   (set (match_dup 4) (mult:DF (match_dup 4) (match_dup 6)))
   (set (match_dup 5) (mult:DF (match_dup 2) (match_dup 4)))
   (set (match_dup 6) (minus:DF (match_dup 3) (match_dup 5)))
   (set (match_dup 4) (mult:DF (match_dup 4) (match_dup 6)))
   (set (match_dup 5) (mult:DF (match_dup 2) (match_dup 4)))
   (set (match_dup 6) (minus:DF (match_dup 3) (match_dup 5)))
   (set (match_dup 4) (mult:DF (match_dup 4) (match_dup 6)))
   (set (match_operand:DF 0 "register_operand" "")
	(mult:DF (match_operand:DF 1 "register_operand")
		 (match_dup 4)))]
  "TARGET_FP && flag_reciprocal_math"
{
  operands[3] = force_reg (DFmode,
			   CONST_DOUBLE_FROM_REAL_VALUE (dconst2, DFmode));
  operands[4] = gen_reg_rtx (DFmode);
  operands[5] = gen_reg_rtx (DFmode);
  operands[6] = gen_reg_rtx (DFmode);
})

;; -------------------------------------------------------------------------
;; Block moves
;; -------------------------------------------------------------------------

(define_expand "movmemsi"
  [(use (match_operand:BLK 0 "memory_operand" ""))
   (use (match_operand:BLK 1 "memory_operand" ""))
   (use (match_operand:SI 2 "nonmemory_operand" ""))
   (use (match_operand:SI 3 "const_int_operand" ""))
   (use (match_operand:SI 4 "const_int_operand" ""))
   (use (match_operand:SI 5 "const_int_operand" ""))]
  ""
{
 if (c6x_expand_movmem (operands[0], operands[1], operands[2], operands[3],
			operands[4], operands[5]))
   DONE;
 else
   FAIL;
})

;; -------------------------------------------------------------------------
;; Prologue and epilogue.
;; -------------------------------------------------------------------------

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "blockage")])

(define_insn "push_rts"
  [(set (mem:SI (reg:SI REG_SP)) (reg:SI REG_B14))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -8))) (reg:DI REG_A14))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -16))) (reg:DI REG_B12))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -24))) (reg:DI REG_A12))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -32))) (reg:DI REG_B10))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -40))) (reg:DI REG_A10))
   (set (mem:DI (plus:SI (reg:SI REG_SP) (const_int -48))) (reg:DI REG_B2))
   (set (reg:SI REG_SP) (plus:SI (reg:SI REG_SP) (const_int -56)))
   (unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)
   (clobber (reg:SI REG_A3))]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tcallp\\t%$\\t__c6xabi_push_rts, a3"
  [(set_attr "type" "callp")
   (set_attr "dest_regfile" "a")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_insn "pop_rts"
  [(set (reg:SI REG_B14) (mem:SI (plus:SI (reg:SI REG_SP) (const_int 56))))
   (set (reg:DI REG_A14) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 48))))
   (set (reg:DI REG_B12) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 40))))
   (set (reg:DI REG_A12) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 32))))
   (set (reg:DI REG_B10) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 24))))
   (set (reg:DI REG_A10) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 16))))
   (set (reg:DI REG_B2) (mem:DI (plus:SI (reg:SI REG_SP) (const_int 8))))
   (set (reg:SI REG_SP) (plus:SI (reg:SI REG_SP) (const_int 56)))
   (clobber (reg:SI REG_A3))
   (return)]
  "TARGET_INSNS_64PLUS"
  "%|%.\\tretp\\t%$\\t__c6xabi_pop_rts, a3"
  [(set_attr "type" "callp")
   (set_attr "dest_regfile" "a")
   (set_attr "units" "s")
   (set_attr "cross" "n")])

(define_expand "prologue"
  [(const_int 1)]
  ""
  "c6x_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(const_int 1)]
  ""
  "c6x_expand_epilogue (false); DONE;")

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  c6x_expand_epilogue (true);
  DONE;
})

(define_insn "setup_dsbt"
  [(set (match_operand:SI 0 "pic_register_operand" "+Z")
	(unspec:SI [(match_dup 0)
		    (match_operand:SI 1 "symbolic_operand" "")]
		   UNSPEC_SETUP_DSBT))]
  "TARGET_DSBT"
  "%|%.\\tldw\\t%$\\t*+%0($DSBT_index%1), %0"
  [(set_attr "type" "load")
   (set_attr "units" "d_addr")
   (set_attr "dest_regfile" "b")
   (set_attr "addr_regfile" "b")])


;; A dummy use/set to prevent prologue and epiloge overlapping.
;; This can be caused by sched-ebb in the presence of multiple
;; exit sequences, and causes the unwinding table generation to explode.
(define_insn "epilogue_barrier"
 [(set (match_operand:SI 0 "register_operand" "")
       (unspec:SI [(match_operand:SI 1 "register_operand" "")]
		  UNSPEC_EPILOGUE_BARRIER))]
 ""
 ""
 [(set_attr "type" "blockage")])

;; -------------------------------------------------------------------------
;; Vector insns
;; -------------------------------------------------------------------------

(define_code_iterator logical [and ior xor])
(define_code_attr logical_insn [(and "and") (ior "ior") (xor "xor")])
(define_code_attr logical_opcode [(and "and") (ior "or") (xor "xor")])
(define_code_iterator plusminus [plus minus])
(define_code_attr plusminus_insn [(plus "add") (minus "sub")])
(define_code_iterator ss_plusminus [ss_plus ss_minus])
(define_code_attr ss_plusminus_insn [(ss_plus "add") (ss_minus "sub")])

;; Vector logical insns

(define_insn "<logical_insn><mode>3"
  [(set (match_operand:VEC4M 0 "register_operand" "=a,b,a,b")
	(logical:VEC4M (match_operand:VEC4M 1 "register_operand" "a,b,a,b")
		      (match_operand:VEC4M 2 "register_operand" "a,b,?b,?a")))]
  ""
  "%|%.\\t<logical_opcode>\\t%$\\t%1, %2, %0"
  [(set_attr "units62" "ls")
   (set_attr "units64" "dls")
   (set_attr "cross" "n,n,y,y")])

;; Vector add/subtract

(define_insn "<plusminus_insn>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
	(plusminus:V2HI (match_operand:V2HI 1 "register_operand" "a,b,a,b")
			(match_operand:V2HI 2 "register_operand" "a,b,?b,?a")))]
  ""
  "%|%.\\t<plusminus_insn>2\\t%$\\t%1, %2, %0"
 [(set_attr "units62" "l")
  (set_attr "units64" "dls")
  (set_attr "cross" "n,n,y,y")])

(define_insn "<plusminus_insn>v4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=a,b,a,b")
	(plusminus:V4QI (match_operand:V4QI 1 "register_operand" "a,b,a,b")
			(match_operand:V4QI 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\t<plusminus_insn>4\\t%$\\t%1, %2, %0"
 [(set_attr "units" "l")
  (set_attr "cross" "n,n,y,y")])

(define_insn "ss_addv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
	(ss_plus:V2HI (match_operand:V2HI 1 "register_operand" "a,b,a,b")
		      (match_operand:V2HI 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tsadd2\\t%$\\t%1, %2, %0"
 [(set_attr "units" "s")
  (set_attr "cross" "n,n,y,y")])

(define_insn "ss_subv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
	(ss_minus:V2HI (match_operand:V2HI 1 "register_operand" "a,b,a,b")
		       (match_operand:V2HI 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tssub2\\t%$\\t%1, %2, %0"
 [(set_attr "units" "l")
  (set_attr "cross" "n,n,y,y")])

(define_insn "us_addv4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=a,b,a,b")
	(ss_plus:V4QI (match_operand:V4QI 1 "register_operand" "a,b,a,b")
		      (match_operand:V4QI 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tsaddu4\\t%$\\t%1, %2, %0"
 [(set_attr "units" "s")
  (set_attr "cross" "n,n,y,y")])

;; Vector/scalar min/max

(define_mode_iterator SMINMAX [HI V2HI])
(define_mode_iterator UMINMAX [QI V4QI])

(define_insn "smax<mode>3"
  [(set (match_operand:SMINMAX 0 "register_operand" "=a,b,a,b")
	(smax:SMINMAX (match_operand:SMINMAX 1 "register_operand" "a,b,a,b")
		      (match_operand:SMINMAX 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tmax2\\t%$\\t%1, %2, %0"
  [(set_attr "units64" "l")
   (set_attr "units64p" "ls")
   (set_attr "cross" "n,n,y,y")])

(define_insn "smin<mode>3"
  [(set (match_operand:SMINMAX 0 "register_operand" "=a,b,a,b")
	(smin:SMINMAX (match_operand:SMINMAX 1 "register_operand" "a,b,a,b")
		      (match_operand:SMINMAX 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tmin2\\t%$\\t%1, %2, %0"
  [(set_attr "units64" "l")
   (set_attr "units64p" "ls")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umax<mode>3"
  [(set (match_operand:UMINMAX 0 "register_operand" "=a,b,a,b")
	(umax:UMINMAX (match_operand:UMINMAX 1 "register_operand" "a,b,a,b")
		      (match_operand:UMINMAX 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tmaxu4\\t%$\\t%1, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umin<mode>3"
  [(set (match_operand:UMINMAX 0 "register_operand" "=a,b,a,b")
	(umin:UMINMAX (match_operand:UMINMAX 1 "register_operand" "a,b,a,b")
		      (match_operand:UMINMAX 2 "register_operand" "a,b,?b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tminu4\\t%$\\t%1, %2, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,n,y,y")])

;; Vector shifts

(define_insn "<shift_code>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
        (any_rshift:V2HI (match_operand:V2HI 1 "register_operand" "a,b,?b,?a")
			 (match_operand:SI 2 "reg_or_ucst5_operand" "aIu5,bIu5,aIu5,bIu5")))]
  "TARGET_INSNS_64"
  "%|%.\\t<shift_insn>2\\t%$\\t%1, %2, %0"
  [(set_attr "units" "s")
   (set_attr "cross" "n,n,y,y")])

;; See c6x-mult.md.in for avg2/avgu4

;; Widening vector multiply and dot product.
;; See c6x-mult.md.in for the define_insn patterns

(define_expand "sdot_prodv2hi"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:SI 3 "register_operand" "")]
  "TARGET_INSNS_64"
{
  rtx t = gen_reg_rtx (SImode);
  emit_insn (gen_dotv2hi (t, operands[1], operands[2]));
  emit_insn (gen_addsi3 (operands[0], operands[3], t));
  DONE;
})

;; Unary vector operations

(define_insn "ssabsv2hi2"
  [(set (match_operand:V2HI 0 "register_operand" "=a, a, b, b")
        (ss_abs:V2HI (match_operand:V2HI 1 "register_operand" "a,?b, b,?a")))]
  "TARGET_INSNS_64"
  "%|%.\\tabs2\\t%$\\t%1, %0"
  [(set_attr "units" "l")
   (set_attr "cross" "n,y,n,y")])

;; Pack insns

(define_insn "*packv2hi_insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+a,b,a,b,ab")
			 (const_int 16)
			 (const_int 16))
	(match_operand:SI 1 "nonmemory_operand" "a,b,?b,?a,n"))]
  "TARGET_INSNS_64"
  "@
   %|%.\\tpack2\\t%$\\t%1, %0, %0
   %|%.\\tpack2\\t%$\\t%1, %0, %0
   %|%.\\tpack2\\t%$\\t%1, %0, %0
   %|%.\\tpack2\\t%$\\t%1, %0, %0
   %|%.\\tmvklh\\t%$\\t%1, %0"
  [(set_attr "units" "ls")
   (set_attr "cross" "n,n,y,y,n")])

(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+a,b,a,b"))
	(match_operand:HI 1 "register_operand" "a,b,?b,?a"))]
  "TARGET_INSNS_64"
  "%|%.\\tpackhl2\\t%$\\t%0, %1, %0"
  [(set_attr "units" "ls")
   (set_attr "cross" "n,n,y,y")])

(include "c6x-mult.md")
(include "sync.md")
