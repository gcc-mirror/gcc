;;  Machine Description for Renesas RX processors
;;  Copyright (C) 2008-2013 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

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


;; This code iterator is used for sign- and zero- extensions.
(define_mode_iterator small_int_modes [(HI "") (QI "")])

;; This code iterator is used for max and min operations.
(define_mode_iterator int_modes [(SI "") (HI "") (QI "")])

;; We do not handle DFmode here because it is either
;; the same as SFmode, or if -m64bit-doubles is active
;; then all operations on doubles have to be handled by
;; library functions.
(define_mode_iterator register_modes
  [(SF "ALLOW_RX_FPU_INSNS") (SI "") (HI "") (QI "")])

(define_constants
  [
   (SP_REG 0)
   (CC_REG 		   16)

   (UNSPEC_LOW_REG         0)
   (UNSPEC_HIGH_REG        1)

   (UNSPEC_RTE             10)
   (UNSPEC_RTFI            11)
   (UNSPEC_NAKED           12)
   (UNSPEC_CONST           13)
   
   (UNSPEC_MOVSTR          20)
   (UNSPEC_MOVMEM          21)
   (UNSPEC_SETMEM          22)
   (UNSPEC_STRLEN          23)
   (UNSPEC_CMPSTRN         24)

   (UNSPEC_BUILTIN_BRK     30)
   (UNSPEC_BUILTIN_CLRPSW  31)
   (UNSPEC_BUILTIN_INT     32)
   (UNSPEC_BUILTIN_MACHI   33)
   (UNSPEC_BUILTIN_MACLO   34)
   (UNSPEC_BUILTIN_MULHI   35)
   (UNSPEC_BUILTIN_MULLO   36)
   (UNSPEC_BUILTIN_MVFACHI 37)
   (UNSPEC_BUILTIN_MVFACMI 38)
   (UNSPEC_BUILTIN_MVFC    39)
   (UNSPEC_BUILTIN_MVFCP   40)
   (UNSPEC_BUILTIN_MVTACHI 41)
   (UNSPEC_BUILTIN_MVTACLO 42)
   (UNSPEC_BUILTIN_MVTC    43)
   (UNSPEC_BUILTIN_MVTIPL  44)
   (UNSPEC_BUILTIN_RACW	   45)
   (UNSPEC_BUILTIN_REVW    46)
   (UNSPEC_BUILTIN_RMPA	   47)
   (UNSPEC_BUILTIN_ROUND   48)
   (UNSPEC_BUILTIN_SAT     49)
   (UNSPEC_BUILTIN_SETPSW  50)
   (UNSPEC_BUILTIN_WAIT	   51)

   (UNSPEC_PID_ADDR	   52)
  ]
)

(define_attr "length" "" (const_int 8))

(include "predicates.md")
(include "constraints.md")

;; Pipeline description.

;; The RX only has a single pipeline.  It has five stages (fetch,
;; decode, execute, memory access, writeback) each of which normally
;; takes a single CPU clock cycle.

;; The timings attribute consists of two numbers, the first is the
;; throughput, which is the number of cycles the instruction takes
;; to execute and generate a result.  The second is the latency
;; which is the effective number of cycles the instruction takes to
;; execute if its result is used by the following instruction.  The
;; latency is always greater than or equal to the throughput.
;; These values were taken from tables 2.13 and 2.14 in section 2.8
;; of the RX610 Group Hardware Manual v0.11

;; Note - it would be nice to use strings rather than integers for
;; the possible values of this attribute, so that we can have the
;; gcc build mechanism check for values that are not supported by
;; the reservations below.  But this will not work because the code
;; in rx_adjust_sched_cost() needs integers not strings.

(define_attr "timings" "" (const_int 11))

(define_automaton "pipelining")
(define_cpu_unit "throughput" "pipelining")

(define_insn_reservation "throughput__1_latency__1"  1
  (eq_attr "timings" "11") "throughput")
(define_insn_reservation "throughput__1_latency__2"  2
  (eq_attr "timings" "12") "throughput,nothing")
(define_insn_reservation "throughput__2_latency__2"  1
  (eq_attr "timings" "22") "throughput*2")
(define_insn_reservation "throughput__3_latency__3"  1
  (eq_attr "timings" "33") "throughput*3")
(define_insn_reservation "throughput__3_latency__4"  2
  (eq_attr "timings" "34") "throughput*3,nothing")
(define_insn_reservation "throughput__4_latency__4"  1
  (eq_attr "timings" "44") "throughput*4")
(define_insn_reservation "throughput__4_latency__5"  2
  (eq_attr "timings" "45") "throughput*4,nothing")
(define_insn_reservation "throughput__5_latency__5"  1
  (eq_attr "timings" "55") "throughput*5")
(define_insn_reservation "throughput__5_latency__6"  2
  (eq_attr "timings" "56") "throughput*5,nothing")
(define_insn_reservation "throughput__6_latency__6"  1
  (eq_attr "timings" "66") "throughput*6")
(define_insn_reservation "throughput_10_latency_10"  1
  (eq_attr "timings" "1010") "throughput*10")
(define_insn_reservation "throughput_11_latency_11"  1
  (eq_attr "timings" "1111") "throughput*11")
(define_insn_reservation "throughput_16_latency_16"  1
  (eq_attr "timings" "1616") "throughput*16")
(define_insn_reservation "throughput_18_latency_18"  1
  (eq_attr "timings" "1818") "throughput*18")

;; ----------------------------------------------------------------------------

;; Comparisons

;; Note - we do not specify the two instructions necessary to perform
;; a compare-and-branch in the cbranchsi4 pattern because that would
;; allow the comparison to be moved away from the jump before the reload
;; pass has completed.  That would be problematical because reload can
;; generate ADDSI3 instructions which would corrupt the PSW flags.

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "comparison_operator"
	    [(match_operand:SI 1 "register_operand")
	     (match_operand:SI 2 "rx_source_operand")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
  ""
)

(define_insn_and_split "*cbranchsi4"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "comparison_operator"
	    [(match_operand:SI  0 "register_operand"  "r")
	     (match_operand:SI  1 "rx_source_operand" "riQ")])
	  (match_operand        2 "label_ref_operand" "")
	  (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rx_split_cbranch (CCmode, GET_CODE (operands[3]),
		    operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "*cmpsi"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 0 "register_operand"  "r,r,r,r,r,r,r")
		    (match_operand:SI 1 "rx_source_operand" "r,Uint04,Int08,Sint16,Sint24,i,Q")))]
  "reload_completed"
  "cmp\t%Q1, %0"
  [(set_attr "timings" "11,11,11,11,11,11,33")
   (set_attr "length"  "2,2,3,4,5,6,5")]
)

;; Canonical method for representing TST.
(define_insn_and_split "*cbranchsi4_tst"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "rx_zs_comparison_operator"
	    [(and:SI (match_operand:SI  0 "register_operand"  "r")
		     (match_operand:SI  1 "rx_source_operand" "riQ"))
	     (const_int 0)])
	  (match_operand 2 "label_ref_operand" "")
	  (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rx_split_cbranch (CC_ZSmode, GET_CODE (operands[3]),
		    XEXP (operands[3], 0), XEXP (operands[3], 1),
		    operands[2]);
  DONE;
})

;; Various other ways that GCC codes "var & const"
(define_insn_and_split "*cbranchsi4_tst_ext"
  [(set (pc)
	(if_then_else
	  (match_operator 4 "rx_z_comparison_operator"
	    [(zero_extract:SI
		(match_operand:SI 0 "register_operand" "r")
		(match_operand:SI 1 "rx_constshift_operand" "")
		(match_operand:SI 2 "rx_constshift_operand" ""))
	     (const_int 0)])
	  (match_operand 3 "label_ref_operand" "")
	  (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  HOST_WIDE_INT mask;
  rtx x;

  mask = 1;
  mask <<= INTVAL (operands[1]);
  mask -= 1;
  mask <<= INTVAL (operands[2]);
  x = gen_rtx_AND (SImode, operands[0], gen_int_mode (mask, SImode));

  rx_split_cbranch (CC_ZSmode, GET_CODE (operands[4]),
		    x, const0_rtx, operands[3]);
  DONE;
})

(define_insn "*tstsi"
  [(set (reg:CC_ZS CC_REG)
	(compare:CC_ZS
	  (and:SI (match_operand:SI 0 "register_operand"  "r,r,r")
		  (match_operand:SI 1 "rx_source_operand" "r,i,Q"))
	  (const_int 0)))]
  "reload_completed"
  "tst\t%Q1, %0"
  [(set_attr "timings" "11,11,33")
   (set_attr "length"  "3,7,6")]
)

(define_expand "cbranchsf4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "rx_fp_comparison_operator"
	    [(match_operand:SF 1 "register_operand")
	     (match_operand:SF 2 "rx_source_operand")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
  "ALLOW_RX_FPU_INSNS"
)

(define_insn_and_split "*cbranchsf4"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "rx_fp_comparison_operator"
	    [(match_operand:SF  0 "register_operand"  "r")
	     (match_operand:SF  1 "rx_source_operand" "rFQ")])
	  (match_operand        2 "label_ref_operand" "")
	  (pc)))]
  "ALLOW_RX_FPU_INSNS"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rx_split_cbranch (CC_Fmode, GET_CODE (operands[3]),
		    operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "*cmpsf"
  [(set (reg:CC_F CC_REG)
	(compare:CC_F
	  (match_operand:SF 0 "register_operand"  "r,r,r")
	  (match_operand:SF 1 "rx_source_operand" "r,F,Q")))]
  "ALLOW_RX_FPU_INSNS && reload_completed"
  "fcmp\t%1, %0"
  [(set_attr "timings" "11,11,33")
   (set_attr "length" "3,7,5")]
)

;; Flow Control Instructions:

(define_insn "*conditional_branch"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "comparison_operator"
	    [(reg CC_REG) (const_int 0)])
	  (label_ref (match_operand 0 "" ""))
	  (pc)))]
  "reload_completed"
  "b%B1\t%0"
  [(set_attr "length" "8")    ;; This length is wrong, but it is
                              ;; too hard to compute statically.
   (set_attr "timings" "33")] ;; The timing assumes that the branch is taken.
)

;; ----------------------------------------------------------------------------

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "bra\t%0"
  [(set_attr "length" "4")
   (set_attr "timings" "33")]
)

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "33")]
)

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI          0 "register_operand" "r"))
   (use (label_ref (match_operand  1 "" "")))]
  ""
  { return TARGET_PID ? (TARGET_AS100_SYNTAX ? "\n?:\tbra\t%0"
					     : "\n1:\tbra\t%0")
	                                     : "\n1:jmp\t%0";
  }
  [(set_attr "timings" "33")
   (set_attr "length" "2")]
)

(define_expand "return"
  [(return)]
  "rx_can_use_simple_return ()"
  "rx_expand_epilogue (false); DONE;"
)

(define_insn "simple_return"
  [(simple_return)]
  ""
  "rts"
  [(set_attr "length" "1")
   (set_attr "timings" "55")]
)

;; Unspec used so that the constant will not be invalid
;; if -mmax-constant-size has been specified.
(define_insn "deallocate_and_return"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const:SI (unspec:SI [(match_operand 0 "const_int_operand" "n")] UNSPEC_CONST))))
   (return)]
  ""
  "rtsd\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "55")]
)

(define_insn "pop_and_return"
  [(match_parallel 1 "rx_rtsd_vector"
     [(set (reg:SI SP_REG)
	   (plus:SI (reg:SI SP_REG)
		    (match_operand:SI 0 "const_int_operand" "n")))])
   (return)]
  "reload_completed"
  {
    rx_emit_stack_popm (operands, false);
    return "";
  }
  [(set_attr "length" "3")
   (set_attr "timings" "56")]
)

(define_insn "fast_interrupt_return"
  [(unspec_volatile [(return)] UNSPEC_RTFI) ]
  ""
  "rtfi"
  [(set_attr "length" "2")
   (set_attr "timings" "33")]
)

(define_insn "exception_return"
  [(unspec_volatile [(return)] UNSPEC_RTE) ]
  ""
  "rte"
  [(set_attr "length" "2")
   (set_attr "timings" "66")]
)

(define_insn "naked_return"
  [(unspec_volatile [(return)] UNSPEC_NAKED) ]
  ""
  "; Naked function: epilogue provided by programmer."
)


;; Note - the following set of patterns do not use the "memory_operand"
;; predicate or an "m" constraint because we do not allow symbol_refs
;; or label_refs as legitimate memory addresses.  This matches the
;; behaviour of most of the RX instructions.  Only the call/branch
;; instructions are allowed to refer to symbols/labels directly.
;; The call operands are in QImode because that is the value of
;; FUNCTION_MODE

(define_expand "call"
  [(call (match_operand:QI 0 "general_operand")
	 (match_operand:SI 1 "general_operand"))]
  ""
  {
    rtx dest = XEXP (operands[0], 0);

    if (! rx_call_operand (dest, Pmode))
      dest = force_reg (Pmode, dest);
    emit_call_insn (gen_call_internal (dest));
    DONE;
  }
)

(define_insn "call_internal"
  [(call (mem:QI (match_operand:SI 0 "rx_call_operand" "r,Symbol"))
	 (const_int 0))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  jsr\t%0
  bsr\t%A0"
  [(set_attr "length" "2,4")
   (set_attr "timings" "33")]
)

(define_expand "call_value"
  [(set (match_operand          0 "register_operand")
	(call (match_operand:QI 1 "general_operand")
	      (match_operand:SI 2 "general_operand")))]
  ""
  {
    rtx dest = XEXP (operands[1], 0);

    if (! rx_call_operand (dest, Pmode))
      dest = force_reg (Pmode, dest);
    emit_call_insn (gen_call_value_internal (operands[0], dest));
    DONE;
  }
)

(define_insn "call_value_internal"
  [(set (match_operand                  0 "register_operand" "=r,r")
	(call (mem:QI (match_operand:SI 1 "rx_call_operand"   "r,Symbol"))
	      (const_int 0)))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  jsr\t%1
  bsr\t%A1"
  [(set_attr "length" "2,4")
   (set_attr "timings" "33")]
)

;; Note - we do not allow indirect sibcalls (with the address
;; held in a register) because we cannot guarantee that the register
;; chosen will be a call-used one.  If it is a call-saved register,
;; then the epilogue code will corrupt it by popping the saved value
;; off of the stack.
(define_expand "sibcall"
  [(parallel
    [(call (mem:QI (match_operand:SI 0 "rx_symbolic_call_operand"))
	   (match_operand:SI         1 "general_operand"))
     (return)])]
  ""
  {
    if (MEM_P (operands[0]))
      operands[0] = XEXP (operands[0], 0);
    emit_call_insn (gen_sibcall_internal (operands[0]));
    DONE;
  }
)

(define_insn "sibcall_internal"
  [(call (mem:QI (match_operand:SI 0 "rx_symbolic_call_operand" "Symbol"))
	 (const_int 0))
   (return)]
  ""
  "bra\t%A0"
  [(set_attr "length"  "4")
   (set_attr "timings" "33")]
)

(define_expand "sibcall_value"
 [(parallel
   [(set (match_operand                  0 "register_operand")
	 (call (mem:QI (match_operand:SI 1 "rx_symbolic_call_operand"))
	       (match_operand:SI         2 "general_operand")))
    (return)])]
  ""
  {
    if (MEM_P (operands[1]))
      operands[1] = XEXP (operands[1], 0);
    emit_call_insn (gen_sibcall_value_internal (operands[0], operands[1]));
    DONE;
  }
)

(define_insn "sibcall_value_internal"
 [(set (match_operand                  0 "register_operand"         "=r")
       (call (mem:QI (match_operand:SI 1 "rx_symbolic_call_operand" "Symbol"))
	     (const_int 0)))
  (return)]
  ""
  "bra\t%A1"
  [(set_attr "length"  "4")
   (set_attr "timings" "33")]
)

;; Function Prologue/Epilogue Instructions

(define_expand "prologue"
  [(const_int 0)]
  ""
  "rx_expand_prologue (); DONE;"
)

(define_expand "epilogue"
  [(return)]
  ""
  "rx_expand_epilogue (false); DONE;"
)

(define_expand "sibcall_epilogue"
  [(return)]
  ""
  "rx_expand_epilogue (true); DONE;"
)

;; Move Instructions

;; Note - we do not allow memory to memory moves, even though the ISA
;; supports them.  The reason is that the conditions on such moves are
;; too restrictive, specifically the source addressing mode is limited
;; by the destination addressing mode and vice versa.  (For example it
;; is not possible to use indexed register indirect addressing for one
;; of the operands if the other operand is anything other than a register,
;; but it is possible to use register relative addressing when the other
;; operand also uses register relative or register indirect addressing).
;;
;; GCC does not support computing legitimate addresses based on the
;; nature of other operands involved in the instruction, and reload is
;; not smart enough to cope with a whole variety of different memory
;; addressing constraints, so it is simpler and safer to just refuse
;; to support memory to memory moves.

(define_expand "mov<register_modes:mode>"
  [(set (match_operand:register_modes 0 "general_operand")
	(match_operand:register_modes 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && MEM_P (operands[1]))
      operands[1] = copy_to_mode_reg (<register_modes:MODE>mode, operands[1]);
    operands[0] = rx_maybe_pidify_operand (operands[0], 0);
    operands[1] = rx_maybe_pidify_operand (operands[1], 0);
    if (GET_CODE (operands[0]) != REG
	&& GET_CODE (operands[1]) == PLUS)
      operands[1] = copy_to_mode_reg (<register_modes:MODE>mode, operands[1]);
    if (GET_CODE (operands[1]) == PLUS && GET_MODE (operands[1]) == SImode)
      {
        emit_insn (gen_addsi3 (operands[0], XEXP (operands[1], 0), XEXP (operands[1], 1)));
        DONE;
      }
    if (CONST_INT_P (operand1)
        && ! rx_is_legitimate_constant (<register_modes:MODE>mode, operand1))
      FAIL;
  }
)

(define_insn "*mov<register_modes:mode>_internal"
  [(set (match_operand:register_modes
	 0 "nonimmediate_operand" "=r,r,r,r,r,r,m,Q,Q,Q,Q,r")
	(match_operand:register_modes
	 1 "general_operand" "Int08,Sint16,Sint24,i,r,m,r,Int08,Sint16,Sint24,i,RpdaRpid"))]
  ""
  { return rx_gen_move_template (operands, false); }
  [(set_attr "length" "3,4,5,6,2,4,6,5,6,7,8,8")
   (set_attr "timings" "11,11,11,11,11,12,11,11,11,11,11,11")]
)

(define_insn "extend<small_int_modes:mode>si2"
  [(set (match_operand:SI 0 "register_operand"    "=r,r")
        (sign_extend:SI (match_operand:small_int_modes
			  1 "nonimmediate_operand" "r,m")))]
  ""
  { return rx_gen_move_template (operands, false); }
  [(set_attr "length" "2,6")
   (set_attr "timings" "11,12")]
)

(define_insn "zero_extend<small_int_modes:mode>si2"
  [(set (match_operand:SI 0 "register_operand"     "=r,r")
        (zero_extend:SI (match_operand:small_int_modes
			  1 "nonimmediate_operand"  "r,m")))]
  ""
  { return rx_gen_move_template (operands, true); }
  [(set_attr "length" "2,4")
   (set_attr "timings" "11,12")]
)

(define_insn "stack_push"
  [(set (reg:SI SP_REG)
	(minus:SI (reg:SI SP_REG)
		  (const_int 4)))
   (set (mem:SI (reg:SI SP_REG))
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push.l\t%0"
  [(set_attr "length" "2")]
)

(define_insn "stack_pushm"
  [(match_parallel 1 "rx_store_multiple_vector"
     [(set (reg:SI SP_REG)
	   (minus:SI (reg:SI SP_REG)
		     (match_operand:SI 0 "const_int_operand" "n")))])]
  "reload_completed"
  {
    rx_emit_stack_pushm (operands);
    return "";
  }
  [(set_attr "length" "2")
   (set_attr "timings" "44")] ;; The timing is a guesstimate average timing.
)

(define_insn "stack_pop"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (reg:SI SP_REG)))
   (set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (const_int 4)))]
  ""
  "pop\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "12")]
)

(define_insn "stack_popm"
  [(match_parallel 1 "rx_load_multiple_vector"
     [(set (reg:SI SP_REG)
	   (plus:SI (reg:SI SP_REG)
		    (match_operand:SI 0 "const_int_operand" "n")))])]
  "reload_completed"
  {
    rx_emit_stack_popm (operands, true);
    return "";
  }
  [(set_attr "length" "2")
   (set_attr "timings" "45")] ;; The timing is a guesstimate average timing.
)

(define_insn_and_split "cstoresi4"
  [(set (match_operand:SI   0 "register_operand" "=r")
	(match_operator:SI  1 "comparison_operator"
	  [(match_operand:SI 2 "register_operand"  "r")
	   (match_operand:SI 3 "rx_source_operand" "riQ")]))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx flags, x;

  flags = gen_rtx_REG (CCmode, CC_REG);
  x = gen_rtx_COMPARE (CCmode, operands[2], operands[3]);
  x = gen_rtx_SET (VOIDmode, flags, x);
  emit_insn (x);

  x = gen_rtx_fmt_ee (GET_CODE (operands[1]), SImode, flags, const0_rtx);
  x = gen_rtx_SET (VOIDmode, operands[0], x);
  emit_insn (x);
  DONE;
})

(define_insn "*sccc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "comparison_operator"
	  [(reg CC_REG) (const_int 0)]))]
  "reload_completed"
  "sc%B1.L\t%0"
  [(set_attr "length" "3")]
)

(define_insn_and_split "cstoresf4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "rx_fp_comparison_operator"
	 [(match_operand:SF 2 "register_operand" "r")
	  (match_operand:SF 3 "rx_source_operand" "rFQ")]))]
  "ALLOW_RX_FPU_INSNS"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx flags, x;

  flags = gen_rtx_REG (CC_Fmode, CC_REG);
  x = gen_rtx_COMPARE (CC_Fmode, operands[2], operands[3]);
  x = gen_rtx_SET (VOIDmode, flags, x);
  emit_insn (x);

  x = gen_rtx_fmt_ee (GET_CODE (operands[1]), SImode, flags, const0_rtx);
  x = gen_rtx_SET (VOIDmode, operands[0], x);
  emit_insn (x);
  DONE;
})

(define_expand "movsicc"
  [(parallel
    [(set (match_operand:SI                  0 "register_operand")
	  (if_then_else:SI (match_operand:SI 1 "comparison_operator")
			   (match_operand:SI 2 "nonmemory_operand")
			   (match_operand:SI 3 "nonmemory_operand")))
     (clobber (reg:CC CC_REG))])]
  ""
{
  /* One operand must be a constant or a register, the other must be a register.  */
  if (   ! CONSTANT_P (operands[2])
      && ! CONSTANT_P (operands[3])
      && ! (REG_P (operands[2]) && REG_P (operands[3])))
    FAIL;
})

(define_insn_and_split "*movsicc"
  [(set (match_operand:SI     0 "register_operand" "=r,r,r")
	(if_then_else:SI
	  (match_operator     5 "comparison_operator"
	   [(match_operand:SI 3 "register_operand"  "r,r,r")
	    (match_operand:SI 4 "rx_source_operand" "riQ,riQ,riQ")])
	  (match_operand:SI   1 "nonmemory_operand" "i,ri,r")
	  (match_operand:SI   2 "nonmemory_operand" "ri,i,r")))
   (clobber (reg:CC CC_REG))]
  "(CONSTANT_P (operands[1]) || CONSTANT_P (operands[2]))
    || (REG_P (operands[1]) && REG_P (operands[2]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx x, flags, op0, op1, op2;
  enum rtx_code cmp_code;

  flags = gen_rtx_REG (CCmode, CC_REG);
  x = gen_rtx_COMPARE (CCmode, operands[3], operands[4]);
  emit_insn (gen_rtx_SET (VOIDmode, flags, x));

  cmp_code = GET_CODE (operands[5]);
  op0 = operands[0];
  op1 = operands[1];
  op2 = operands[2];

  /* If OP2 is the constant, reverse the sense of the move.
     Likewise if both operands are registers but OP1 == OP0.  */
  if ((! CONSTANT_P (operands[1]) && CONSTANT_P (operands[2]))
      || (REG_P (operands[1]) && REG_P (operands[2])
          && rtx_equal_p (op0, op1)))
    {
      x = op1, op1 = op2, op2 = x;
      cmp_code = reverse_condition (cmp_code);
    }

  /* If OP2 does not match the output, copy it into place.  We have allowed
     these alternatives so that the destination can legitimately be one of
     the comparison operands without increasing register pressure.  */
  if (! rtx_equal_p (op0, op2))
    emit_move_insn (op0, op2);

  x = gen_rtx_fmt_ee (cmp_code, VOIDmode, flags, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (SImode, x, op1, op0);
  emit_insn (gen_rtx_SET (VOIDmode, op0, x));
  DONE;
})

(define_insn "*stcc"
  [(set (match_operand:SI 0 "register_operand" "+r,r,r,r")
	(if_then_else:SI
	  (match_operator 2 "rx_z_comparison_operator"
	    [(reg CC_REG) (const_int 0)])
	  (match_operand:SI 1 "immediate_operand" "Sint08,Sint16,Sint24,i")
	  (match_dup 0)))]
  "reload_completed
   && ((GET_CODE (operands[2]) == EQ) || (GET_CODE (operands[2]) == NE))"
  {
    if (GET_CODE (operands[2]) == EQ)
      return "stz\t%1, %0";
    else
     return "stnz\t%1, %0";
  }
  [(set_attr "length" "4,5,6,7")]
)

(define_insn "*stcc_reg"
  [(set (match_operand:SI 0 "register_operand" "+r,r,r,r,r,r")
	(if_then_else:SI
	  (match_operator 2 "comparison_operator"
	    [(reg CC_REG) (const_int 0)])
	  (match_operand:SI 1 "nonmemory_operand"
		              "r,Uint04,Sint08,Sint16,Sint24,i")
	  (match_dup 0)))]
  "reload_completed"
  {
    PUT_CODE (operands[2], reverse_condition (GET_CODE (operands[2])));
    return "b%B2 1f\n\tmov %1, %0\n1:";
  }
  [(set_attr "length" "3,3,4,5,6,7")]
)

;; Arithmetic Instructions

(define_insn "abssi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (abs:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  abs\t%0
  abs\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_insn "*abssi2_flags"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (abs:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (set (reg CC_REG)
	(compare (abs:SI (match_dup 1))
		 (const_int 0)))]
  ;; Note - although the ABS instruction does set the O bit in the processor
  ;; status word, it does not do so in a way that is comparable with the CMP
  ;; instruction.  Hence we use CC_ZSmode rather than CC_ZSOmode.
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  abs\t%0
  abs\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_expand "addsi3"
  [(parallel [(set (match_operand:SI          0 "register_operand"  "")
	(plus:SI (match_operand:SI 1 "register_operand"  "")
		 (match_operand:SI 2 "rx_source_operand" "")))
    (clobber (reg:CC CC_REG))])]
  ""
  "
      operands[0] = rx_maybe_pidify_operand (operands[0], 1);
      operands[1] = rx_maybe_pidify_operand (operands[1], 1);
      operands[2] = rx_maybe_pidify_operand (operands[2], 1);
  "
)

(define_insn "addsi3_internal"
  [(set (match_operand:SI          0 "register_operand"  "=r,r,r,r,r,r,r,r,r,r,r,r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0,0,r,r,r,r,r,r,0")
		 (match_operand:SI 2 "rx_source_operand" "r,Uint04,NEGint4,Sint08,Sint16,Sint24,i,0,r,Sint08,Sint16,Sint24,i,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  add\t%2, %0
  add\t%2, %0
  sub\t%N2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,11,11,11,11,11,33")
   (set_attr "length"   "2,2,2,3,4,5,6,2,3,3,4,5,6,5")]
)

(define_insn "*addsi3_flags"
  [(set (match_operand:SI          0 "register_operand"  "=r,r,r,r,r,r,r,r,r,r,r,r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0,0,r,r,r,r,r,r,0")
		 (match_operand:SI 2 "rx_source_operand" "r,Uint04,NEGint4,Sint08,Sint16,Sint24,i,0,r,Sint08,Sint16,Sint24,i,Q")))
   (set (reg CC_REG)
	(compare (plus:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSCmode)"
  "@
  add\t%2, %0
  add\t%2, %0
  sub\t%N2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,11,11,11,11,11,33")
   (set_attr "length"   "2,2,2,3,4,5,6,2,3,3,4,5,6,5")]
)

;; A helper to expand the above with the CC_MODE filled in.
(define_expand "addsi3_flags"
  [(parallel [(set (match_operand:SI 0 "register_operand")
		   (plus:SI (match_operand:SI 1 "register_operand")
			    (match_operand:SI 2 "rx_source_operand")))
	      (set (reg:CC_ZSC CC_REG)
		   (compare:CC_ZSC (plus:SI (match_dup 1) (match_dup 2))
				   (const_int 0)))])]
)

(define_insn "adc_internal"
  [(set (match_operand:SI     0 "register_operand"  "=r,r,r,r,r,r")
	(plus:SI
	  (plus:SI
	    (ltu:SI (reg:CC CC_REG) (const_int 0))
	    (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0"))
	  (match_operand:SI   2 "rx_source_operand" "r,Sint08,Sint16,Sint24,i,Q")))
    (clobber (reg:CC CC_REG))]
  "reload_completed"
  "adc\t%2, %0"
  [(set_attr "timings" "11,11,11,11,11,33")
   (set_attr "length"   "3,4,5,6,7,6")]
)

(define_insn "*adc_flags"
  [(set (match_operand:SI     0 "register_operand"  "=r,r,r,r,r,r")
	(plus:SI
	  (plus:SI
	    (ltu:SI (reg:CC CC_REG) (const_int 0))
	    (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0"))
	  (match_operand:SI   2 "rx_source_operand" "r,Sint08,Sint16,Sint24,i,Q")))
   (set (reg CC_REG)
	(compare 
	  (plus:SI
	    (plus:SI
	      (ltu:SI (reg:CC CC_REG) (const_int 0))
	      (match_dup 1))
	    (match_dup 2))
	  (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSCmode)"
  "adc\t%2, %0"
  [(set_attr "timings" "11,11,11,11,11,33")
   (set_attr "length"   "3,4,5,6,7,6")]
)

;; Peepholes to match:
;;   (set (reg A) (reg B))
;;   (set (CC) (compare:CC (reg A/reg B) (const_int 0)))
;; and replace them with the addsi3_flags pattern, using an add
;; of zero to copy the register and set the condition code bits.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (match_operand:SI 1 "register_operand"))
   (set (reg:CC CC_REG)
        (compare:CC (match_dup 0)
                    (const_int 0)))]
  ""
  [(parallel [(set (match_dup 0)
		   (plus:SI (match_dup 1) (const_int 0)))
	      (set (reg:CC_ZSC CC_REG)
		   (compare:CC_ZSC (plus:SI (match_dup 1) (const_int 0))
				   (const_int 0)))])]
)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (match_operand:SI 1 "register_operand"))
   (set (reg:CC CC_REG)
        (compare:CC (match_dup 1)
                    (const_int 0)))]
  ""
  [(parallel [(set (match_dup 0)
		   (plus:SI (match_dup 1) (const_int 0)))
	      (set (reg:CC_ZSC CC_REG)
		   (compare:CC_ZSC (plus:SI (match_dup 1) (const_int 0))
				   (const_int 0)))])]
)

(define_expand "adddi3"
  [(set (match_operand:DI          0 "register_operand")
	(plus:DI (match_operand:DI 1 "register_operand")
		 (match_operand:DI 2 "rx_source_operand")))]
  ""
{
  rtx op0l, op0h, op1l, op1h, op2l, op2h;

  op0l = gen_lowpart (SImode, operands[0]);
  op1l = gen_lowpart (SImode, operands[1]);
  op2l = gen_lowpart (SImode, operands[2]);
  op0h = gen_highpart (SImode, operands[0]);
  op1h = gen_highpart (SImode, operands[1]);
  op2h = gen_highpart_mode (SImode, DImode, operands[2]);

  emit_insn (gen_adddi3_internal (op0l, op0h, op1l, op2l, op1h, op2h));
  DONE;
})

(define_insn_and_split "adddi3_internal"
  [(set (match_operand:SI          0 "register_operand"  "=&r")
	(plus:SI (match_operand:SI 2 "register_operand"  "r")
		 (match_operand:SI 3 "rx_source_operand" "riQ")))
   (set (match_operand:SI          1 "register_operand"  "=r")
	(plus:SI
	  (plus:SI
	    (ltu:SI (plus:SI (match_dup 2) (match_dup 3)) (match_dup 2))
	    (match_operand:SI      4 "register_operand"  "%1"))
	  (match_operand:SI        5 "rx_source_operand" "riQ")))
   (clobber (match_scratch:SI      6                     "=&r"))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx op0l = operands[0];
  rtx op0h = operands[1];
  rtx op1l = operands[2];
  rtx op2l = operands[3];
  rtx op1h = operands[4];
  rtx op2h = operands[5];
  rtx scratch = operands[6];
  rtx x;

  if (reg_overlap_mentioned_p (op0l, op1h))
    {
      emit_move_insn (scratch, op0l);
      op1h = scratch;
      if (reg_overlap_mentioned_p (op0l, op2h))
	op2h = scratch;
    }
  else if (reg_overlap_mentioned_p (op0l, op2h))
    {
      emit_move_insn (scratch, op0l);
      op2h = scratch;
    }

  if (rtx_equal_p (op0l, op1l))
    ;
  /* It is preferable that op0l == op1l...  */
  else if (rtx_equal_p (op0l, op2l))
    x = op1l, op1l = op2l, op2l = x;
  /* ... but it is only a requirement if op2l == MEM.  */
  else if (MEM_P (op2l))
    {
      /* Let's hope that we still have a scratch register free.  */
      gcc_assert (op1h != scratch);
      emit_move_insn (scratch, op2l);
      op2l = scratch;
    }

  emit_insn (gen_addsi3_flags (op0l, op1l, op2l));

  if (rtx_equal_p (op0h, op1h))
    ;
  else if (rtx_equal_p (op0h, op2h))
    x = op1h, op1h = op2h, op2h = x;
  else
    {
      emit_move_insn (op0h, op1h);
      op1h = op0h;
    }
  emit_insn (gen_adc_internal (op0h, op1h, op2h));
  DONE;
})

(define_insn "andsi3"
  [(set (match_operand:SI         0 "register_operand"  "=r,r,r,r,r,r,r,r,r")
	(and:SI (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0,r,r,0")
		(match_operand:SI 2 "rx_source_operand" "r,Uint04,Sint08,Sint16,Sint24,i,0,r,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%1, %0
  and\t%2, %1, %0
  and\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,33")
   (set_attr "length" "2,2,3,4,5,6,2,5,5")]
)

(define_insn "*andsi3_flags"
  [(set (match_operand:SI         0 "register_operand"  "=r,r,r,r,r,r,r,r,r")
	(and:SI (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0,r,r,0")
		(match_operand:SI 2 "rx_source_operand" "r,Uint04,Sint08,Sint16,Sint24,i,0,r,Q")))
   (set (reg CC_REG)
	(compare (and:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%1, %0
  and\t%2, %1, %0
  and\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,33")
   (set_attr "length" "2,2,3,4,5,6,2,5,5")]
)

;; Byte swap (single 32-bit value).
(define_insn "bswapsi2"
  [(set (match_operand:SI           0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand"  "r")))]
  ""
  "revl\t%1, %0"
  [(set_attr "length" "3")]
)

;; Byte swap (single 16-bit value).  Note - we ignore the swapping of the high 16-bits.
(define_insn "bswaphi2"
  [(set (match_operand:HI           0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand"  "r")))]
  ""
  "revw\t%1, %0"
  [(set_attr "length" "3")]
)

(define_insn "divsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r")
	(div:SI (match_operand:SI 1 "register_operand"  "0,0,0,0,0,0")
		(match_operand:SI 2 "rx_source_operand" "r,Sint08,Sint16,Sint24,i,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "div\t%Q2, %0"
  [(set_attr "timings" "1111") ;; Strictly speaking the timing should be
                               ;; 2222, but that is a worst case sceanario.
   (set_attr "length" "3,4,5,6,7,6")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI          0 "register_operand"  "=r,r,r,r,r,r")
	(udiv:SI (match_operand:SI 1 "register_operand"   "0,0,0,0,0,0")
		 (match_operand:SI 2 "rx_source_operand"  "r,Sint08,Sint16,Sint24,i,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "divu\t%Q2, %0"
  [(set_attr "timings" "1010") ;; Strictly speaking the timing should be
                               ;; 2020, but that is a worst case sceanario.
   (set_attr "length" "3,4,5,6,7,6")]
)

;; Note - these patterns are suppressed in big-endian mode because they
;; generate a little endian result.  ie the most significant word of the
;; result is placed in the higher numbered register of the destination
;; register pair.

(define_insn "mulsidi3"
  [(set (match_operand:DI          0 "register_operand"  "=r,r,r,r,r,r")
        (mult:DI (sign_extend:DI (match_operand:SI
				  1 "register_operand"  "%0,0,0,0,0,0"))
                 (sign_extend:DI (match_operand:SI
				  2 "rx_source_operand"
				  "r,Sint08,Sint16,Sint24,i,Q"))))]
  "! TARGET_BIG_ENDIAN_DATA"
  "emul\t%Q2, %0"
  [(set_attr "length" "3,4,5,6,7,6")   
   (set_attr "timings" "22,22,22,22,22,44")]
)

;; See comment for mulsidi3.
;; Note - the zero_extends are to distinguish this pattern from the
;; mulsidi3 pattern.  Immediate mode addressing is not supported
;; because gcc cannot handle the expression: (zero_extend (const_int)).
(define_insn "umulsidi3"
  [(set (match_operand:DI                          0 "register_operand"	 "=r,r")
        (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "%0,0"))
                 (zero_extend:DI (match_operand:SI 2 "rx_compare_operand" "r,Q"))))]
  "! TARGET_BIG_ENDIAN_DATA"
  "emulu\t%Q2, %0"
  [(set_attr "length" "3,6")
   (set_attr "timings" "22,44")]
)

(define_insn "smaxsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r")
	(smax:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
		 (match_operand:SI 2 "rx_source_operand"
				   "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "max\t%Q2, %0"
  [(set_attr "length" "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_insn "sminsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r")
	(smin:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
		 (match_operand:SI 2 "rx_source_operand"
				   "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "min\t%Q2, %0"
  [(set_attr "length"  "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_insn "umax<small_int_modes:mode>3_u"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r")
	(smax:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
		 (zero_extend:SI (match_operand:small_int_modes 2 "rx_minmaxex_operand"
								"r,Sint08,Sint16,Sint24,i,Q"))))]
  ""
  "max\t%R2, %0"
  [(set_attr "length"  "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_insn "umin<small_int_modes:mode>3_ur"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r")
	(smin:SI (zero_extend:SI (match_operand:small_int_modes 2 "rx_minmaxex_operand"
								"r,Sint08,Sint16,Sint24,i,Q"))
		 (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")))]
  ""
  "min\t%R2, %0"
  [(set_attr "length"  "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_insn "umax<small_int_modes:mode>3_ur"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r")
	(smax:SI (zero_extend:SI (match_operand:small_int_modes 2 "rx_minmaxex_operand"
								"r,Sint08,Sint16,Sint24,i,Q"))
		 (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")))]
  ""
  "max\t%R2, %0"
  [(set_attr "length"  "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_expand "umax<small_int_modes:mode>3"
  [(set (match_dup 4)
	(zero_extend:SI (match_operand:small_int_modes 1 "register_operand" "%0,0,0,0,0,0")))
   (set (match_dup 3)
	(smax:SI (match_dup 4)
		 (match_operand:small_int_modes 2 "rx_source_operand"
						"r,Sint08,Sint16,Sint24,i,Q")))
   (set (match_operand:small_int_modes          0 "register_operand" "=r,r,r,r,r,r")
	(match_dup 6))
   ]
  ""
  "operands[3] = gen_reg_rtx (SImode);
   operands[4] = gen_reg_rtx (SImode);
   operands[5] = gen_reg_rtx (SImode);
   operands[6] = gen_rtx_SUBREG (GET_MODE (operands[0]), operands[3],
     TARGET_BIG_ENDIAN_DATA ? (GET_MODE (operands[0]) == HImode ? 2 : 3) : 0);
   if (GET_CODE (operands[2]) != CONST_INT)
     {
       emit_move_insn (operands[5], gen_rtx_ZERO_EXTEND (SImode, operands[2]));
       operands[2] = operands[5];
     }
  "
)

(define_expand "umin<small_int_modes:mode>3"
  [(set (match_dup 4)
	(zero_extend:SI (match_operand:small_int_modes 1 "register_operand" "%0,0,0,0,0,0")))
   (set (match_dup 3)
	(smin:SI (match_dup 4)
		 (match_operand:small_int_modes 2 "rx_source_operand"
						"r,Sint08,Sint16,Sint24,i,Q")))
   (set (match_operand:small_int_modes          0 "register_operand" "=r,r,r,r,r,r")
	(match_dup 6))
   ]
  ""
  "operands[3] = gen_reg_rtx (SImode);
   operands[4] = gen_reg_rtx (SImode);
   operands[5] = gen_reg_rtx (SImode);
   operands[6] = gen_rtx_SUBREG (GET_MODE (operands[0]), operands[3],
     TARGET_BIG_ENDIAN_DATA ? (GET_MODE (operands[0]) == HImode ? 2 : 3) : 0);
   if (GET_CODE (operands[2]) != CONST_INT)
     {
       emit_move_insn (operands[5], gen_rtx_ZERO_EXTEND (SImode, operands[2]));
       operands[2] = operands[5];
     }
   "
)

(define_insn "mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r,r,r,r")
        (mult:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,0,r,r")
                 (match_operand:SI 2 "rx_source_operand"
				   "r,Uint04,Sint08,Sint16,Sint24,i,Q,0,r")))]
  ""
  "@
  mul\t%2, %0
  mul\t%2, %0
  mul\t%2, %0
  mul\t%2, %0
  mul\t%2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%1, %0
  mul\t%2, %1, %0"
  [(set_attr "length"  "2,2,3,4,5,6,5,2,3")
   (set_attr "timings" "11,11,11,11,11,11,33,11,11")]
)

(define_insn "negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (neg:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  neg\t%0
  neg\t%1, %0"
  [(set_attr "length" "2,3")]
)

;; Note that the O and C flags are not set as per a normal compare,
;; and thus are unusable in that context.
(define_insn "*negsi2_flags"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (neg:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (set (reg CC_REG)
	(compare (neg:SI (match_dup 1))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  neg\t%0
  neg\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_insn "one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
	(not:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  not\t%0
  not\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_insn "*one_cmplsi2_flags"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
	(not:SI (match_operand:SI 1 "register_operand"  "0,r")))
   (set (reg CC_REG)
	(compare (not:SI (match_dup 1))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  not\t%0
  not\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_insn "iorsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r,r,r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,r,r,0")
	        (match_operand:SI 2 "rx_source_operand" "r,Uint04,Sint08,Sint16,Sint24,i,0,r,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%Q2, %0
  or\t%1, %0
  or\t%2, %1, %0
  or\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,33")
   (set_attr "length"  "2,2,3,4,5,6,2,3,5")]
)

(define_insn "*iorsi3_flags"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r,r,r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,r,r,0")
	        (match_operand:SI 2 "rx_source_operand" "r,Uint04,Sint08,Sint16,Sint24,i,0,r,Q")))
   (set (reg CC_REG)
	(compare (ior:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%Q2, %0
  or\t%1, %0
  or\t%2, %1, %0
  or\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,11,11,11,33")
   (set_attr "length"  "2,2,3,4,5,6,2,3,5")]
)

(define_insn "rotlsi3"
  [(set (match_operand:SI            0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand"  "0")
		   (match_operand:SI 2 "rx_shift_operand" "rn")))
   (clobber (reg:CC CC_REG))]
  ""
  "rotl\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "*rotlsi3_flags"
  [(set (match_operand:SI            0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand"  "0")
		   (match_operand:SI 2 "rx_shift_operand" "rn")))
   (set (reg CC_REG)
	(compare (rotate:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "rotl\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "0")
		     (match_operand:SI 2 "rx_shift_operand" "rn")))
   (clobber (reg:CC CC_REG))]
  ""
  "rotr\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "*rotrsi3_flags"
  [(set (match_operand:SI              0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "0")
		     (match_operand:SI 2 "rx_shift_operand" "rn")))
   (set (reg CC_REG)
	(compare (rotatert:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "rotr\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "ashrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  shar\t%2, %0
  shar\t%2, %0
  shar\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

(define_insn "*ashrsi3_flags"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (set (reg CC_REG)
	(compare (ashiftrt:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  shar\t%2, %0
  shar\t%2, %0
  shar\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

(define_insn "lshrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  shlr\t%2, %0
  shlr\t%2, %0
  shlr\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

(define_insn "*lshrsi3_flags"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (set (reg CC_REG)
	(compare (lshiftrt:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  shlr\t%2, %0
  shlr\t%2, %0
  shlr\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

(define_insn "ashlsi3"
  [(set (match_operand:SI            0 "register_operand" "=r,r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,0,r")
	           (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  shll\t%2, %0
  shll\t%2, %0
  shll\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

(define_insn "*ashlsi3_flags"
  [(set (match_operand:SI            0 "register_operand" "=r,r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,0,r")
	           (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))
   (set (reg CC_REG)
	(compare (ashift:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "@
  shll\t%2, %0
  shll\t%2, %0
  shll\t%2, %1, %0"
  [(set_attr "length" "3,2,3")]
)

;; Saturate to 32-bits
(define_insn_and_split "ssaddsi3"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(ss_plus:SI (match_operand:SI 1 "register_operand"  "r")
		    (match_operand:SI 2 "rx_source_operand" "riQ")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:SI (match_dup 1) (match_dup 2)))
	      (set (reg:CC_ZSC CC_REG)
		   (compare:CC_ZSC
		     (plus:SI (match_dup 1) (match_dup 2))
		     (const_int 0)))])
   (set (match_dup 0)
	(unspec:SI [(match_dup 0) (reg:CC CC_REG)] 
		   UNSPEC_BUILTIN_SAT))]
   ""
)

(define_insn "*sat"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand"  "0")
		    (reg:CC CC_REG)]
		   UNSPEC_BUILTIN_SAT))]
  "reload_completed"
  "sat\t%0"
  [(set_attr "length" "2")]
)

(define_insn "subsi3"
  [(set (match_operand:SI           0 "register_operand" "=r,r,r,r,r")
	(minus:SI (match_operand:SI 1 "register_operand"  "0,0,0,r,0")
		  (match_operand:SI 2 "rx_source_operand" "r,Uint04,n,r,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "@
  sub\t%2, %0
  sub\t%2, %0
  add\t%N2, %0
  sub\t%2, %1, %0
  sub\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,33")
   (set_attr "length" "2,2,6,3,5")]
)

;; Note that the O flag is set as if (compare op1 op2) not for
;; what is described here, (compare op0 0).
(define_insn "*subsi3_flags"
  [(set (match_operand:SI           0 "register_operand" "=r,r,r,r,r")
	(minus:SI (match_operand:SI 1 "register_operand"  "0,0,0,r,0")
		  (match_operand:SI 2 "rx_source_operand" "r,Uint04,n,r,Q")))
   (set (reg CC_REG)
	(compare (minus:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSCmode)"
  "@
  sub\t%2, %0
  sub\t%2, %0
  add\t%N2, %0
  sub\t%2, %1, %0
  sub\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,33")
   (set_attr "length" "2,2,6,3,5")]
)

;; A helper to expand the above with the CC_MODE filled in.
(define_expand "subsi3_flags"
  [(parallel [(set (match_operand:SI 0 "register_operand")
		   (minus:SI (match_operand:SI 1 "register_operand")
			     (match_operand:SI 2 "rx_source_operand")))
	      (set (reg:CC_ZSC CC_REG)
		   (compare:CC_ZSC (minus:SI (match_dup 1) (match_dup 2))
				   (const_int 0)))])]
)

(define_insn "sbb_internal"
  [(set (match_operand:SI     0 "register_operand"   "=r,r")
	(minus:SI
	  (minus:SI
	    (match_operand:SI 1 "register_operand"   " 0,0")
	    (match_operand:SI 2 "rx_compare_operand" " r,Q"))
	  (geu:SI (reg:CC CC_REG) (const_int 0))))
    (clobber (reg:CC CC_REG))]
  "reload_completed"
  "sbb\t%2, %0"
  [(set_attr "timings" "11,33")
   (set_attr "length"  "3,6")]
)

(define_insn "*sbb_flags"
  [(set (match_operand:SI     0 "register_operand"   "=r,r")
	(minus:SI
	  (minus:SI
	    (match_operand:SI 1 "register_operand"   " 0,0")
	    (match_operand:SI 2 "rx_compare_operand" " r,Q"))
	  (geu:SI (reg:CC CC_REG) (const_int 0))))
   (set (reg CC_REG)
	(compare
	  (minus:SI
	    (minus:SI (match_dup 1) (match_dup 2))
	    (geu:SI (reg:CC CC_REG) (const_int 0)))
	  (const_int 0)))]
  "reload_completed"
  "sbb\t%2, %0"
  [(set_attr "timings" "11,33")
   (set_attr "length"  "3,6")]
)

(define_expand "subdi3"
  [(set (match_operand:DI           0 "register_operand")
	(minus:DI (match_operand:DI 1 "register_operand")
		  (match_operand:DI 2 "register_operand")))]
  ""
{
  rtx op0l, op0h, op1l, op1h, op2l, op2h;

  op0l = gen_lowpart (SImode, operands[0]);
  op1l = gen_lowpart (SImode, operands[1]);
  op2l = gen_lowpart (SImode, operands[2]);
  op0h = gen_highpart (SImode, operands[0]);
  op1h = gen_highpart (SImode, operands[1]);
  op2h = gen_highpart_mode (SImode, DImode, operands[2]);

  emit_insn (gen_subdi3_internal (op0l, op0h, op1l, op2l, op1h, op2h));
  DONE;
})

(define_insn_and_split "subdi3_internal"
  [(set (match_operand:SI          0 "register_operand"   "=&r,&r")
	(minus:SI (match_operand:SI 2 "register_operand"  "  0, r")
		  (match_operand:SI 3 "rx_compare_operand" "rQ, r")))
   (set (match_operand:SI          1 "register_operand"   "= r, r")
	(minus:SI
	  (minus:SI
	    (match_operand:SI      4 "register_operand"   "  1, 1")
	    (match_operand:SI      5 "rx_compare_operand" " rQ,rQ"))
	  (geu:SI (match_dup 2) (match_dup 3))))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi3_flags (operands[0], operands[2], operands[3]));
  emit_insn (gen_sbb_internal (operands[1], operands[4], operands[5]));
  DONE;
})

(define_insn "xorsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
	        (match_operand:SI 2 "rx_source_operand"
				  "r,Sint08,Sint16,Sint24,i,Q")))
   (clobber (reg:CC CC_REG))]
  ""
  "xor\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,33")
   (set_attr "length" "3,4,5,6,7,6")]
)

(define_insn "*xorsi3_flags"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
	        (match_operand:SI 2 "rx_source_operand"
				  "r,Sint08,Sint16,Sint24,i,Q")))
   (set (reg CC_REG)
	(compare (xor:SI (match_dup 1) (match_dup 2))
		 (const_int 0)))]
  "reload_completed && rx_match_ccmode (insn, CC_ZSmode)"
  "xor\t%Q2, %0"
  [(set_attr "timings" "11,11,11,11,11,33")
   (set_attr "length" "3,4,5,6,7,6")]
)

;; A set of peepholes to catch extending loads followed by arithmetic operations.
;; We use iterators where possible to reduce the amount of typing and hence the
;; possibilities for typos.

(define_code_iterator extend_types [(zero_extend "") (sign_extend "")])
(define_code_attr     letter       [(zero_extend "R") (sign_extend "Q")])

(define_code_iterator memex_commutative [(plus "") (and "") (ior "") (xor "")])
(define_code_iterator memex_noncomm     [(div "") (udiv "") (minus "")])
(define_code_iterator memex_nocc        [(smax "") (smin "") (mult "")])

(define_code_attr     op                [(plus "add") (and "and") (div "div") (udiv "divu") (smax "max") (smin "min") (mult "mul") (ior "or") (minus "sub") (xor "xor")])

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (parallel [(set (match_operand:SI                    2 "register_operand")
		   (memex_commutative:SI (match_dup 0)
					 (match_dup 2)))
	      (clobber (reg:CC CC_REG))])]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(parallel [(set:SI (match_dup 2)
		      (memex_commutative:SI (match_dup 2)
					    (extend_types:SI (match_dup 1))))
	      (clobber (reg:CC CC_REG))])]
)

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (parallel [(set (match_operand:SI                    2 "register_operand")
		   (memex_commutative:SI (match_dup 2)
					 (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(parallel [(set:SI (match_dup 2)
		      (memex_commutative:SI (match_dup 2)
					    (extend_types:SI (match_dup 1))))
	      (clobber (reg:CC CC_REG))])]
)

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (parallel [(set (match_operand:SI                    2 "register_operand")
		   (memex_noncomm:SI (match_dup 2)
				     (match_dup 0)))
	      (clobber (reg:CC CC_REG))])]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(parallel [(set:SI (match_dup 2)
		      (memex_noncomm:SI (match_dup 2)
					(extend_types:SI (match_dup 1))))
	      (clobber (reg:CC CC_REG))])]
)

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (set (match_operand:SI                               2 "register_operand")
	(memex_nocc:SI (match_dup 0)
		       (match_dup 2)))]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(set:SI (match_dup 2)
	   (memex_nocc:SI (match_dup 2)
			  (extend_types:SI (match_dup 1))))]
)

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (set (match_operand:SI                               2 "register_operand")
	(memex_nocc:SI (match_dup 2)
		       (match_dup 0)))]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(set:SI (match_dup 2)
	   (memex_nocc:SI (match_dup 2)
			  (extend_types:SI (match_dup 1))))]
)

(define_insn "<memex_commutative:code>si3_<extend_types:code><small_int_modes:mode>"
  [(set (match_operand:SI                                                     0 "register_operand" "=r")
	(memex_commutative:SI (match_operand:SI                               1 "register_operand" "%0")
 		              (extend_types:SI (match_operand:small_int_modes 2 "rx_restricted_mem_operand" "Q"))))
   (clobber (reg:CC CC_REG))]
  "(optimize < 3 || optimize_size)"
  "<memex_commutative:op>\t%<extend_types:letter>2, %0"
  [(set_attr "timings" "33")
   (set_attr "length"  "5")] ;; This length is corrected in rx_adjust_insn_length
)

(define_insn "<memex_noncomm:code>si3_<extend_types:code><small_int_modes:mode>"
  [(set (match_operand:SI                                                 0 "register_operand" "=r")
	(memex_noncomm:SI (match_operand:SI                               1 "register_operand" "0")
                          (extend_types:SI (match_operand:small_int_modes 2 "rx_restricted_mem_operand" "Q"))))
   (clobber (reg:CC CC_REG))]
  "(optimize < 3 || optimize_size)"
  "<memex_noncomm:op>\t%<extend_types:letter>2, %0"
  [(set_attr "timings" "33")
   (set_attr "length"  "5")] ;; This length is corrected in rx_adjust_insn_length
)

(define_insn "<memex_nocc:code>si3_<extend_types:code><small_int_modes:mode>"
  [(set (match_operand:SI                                              0 "register_operand" "=r")
	(memex_nocc:SI (match_operand:SI                               1 "register_operand" "%0")
		       (extend_types:SI (match_operand:small_int_modes 2 "rx_restricted_mem_operand" "Q"))))]
  "(optimize < 3 || optimize_size)"
  "<memex_nocc:op>\t%<extend_types:letter>2, %0"
  [(set_attr "timings" "33")
   (set_attr "length"  "5")] ;; This length is corrected in rx_adjust_insn_length
)

(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand")))
   (set (reg:CC CC_REG)
	(compare:CC (match_operand:SI                   2 "register_operand")
		    (match_dup 0)))]
  "peep2_regno_dead_p (2, REGNO (operands[0])) && (optimize < 3 || optimize_size)"
  [(set (reg:CC CC_REG)
	(compare:CC (match_dup 2)
		    (extend_types:SI (match_dup 1))))]
)

;; Convert:
;;   (set (reg1) (sign_extend (mem))
;;   (set (reg2) (zero_extend (reg1))
;; into
;;   (set (reg2) (zero_extend (mem)))
(define_peephole2
  [(set (match_operand:SI                              0 "register_operand")
	(sign_extend:SI (match_operand:small_int_modes 1 "memory_operand")))
   (set (match_operand:SI                              2 "register_operand")
	(zero_extend:SI (match_operand:small_int_modes 3 "register_operand")))]
  "REGNO (operands[0]) == REGNO (operands[3])
   && (REGNO (operands[0]) == REGNO (operands[2])
       || peep2_regno_dead_p (2, REGNO (operands[0])))"
  [(set (match_dup 2)
	(zero_extend:SI (match_dup 1)))]
)

;; Remove the redundant sign extension from:
;;   (set (reg) (extend (mem)))
;;   (set (reg) (extend (reg)))
(define_peephole2
  [(set (match_operand:SI                               0 "register_operand")
	(extend_types:SI (match_operand:small_int_modes 1 "memory_operand")))
   (set (match_dup 0)
	(extend_types:SI (match_operand:small_int_modes 2 "register_operand")))]
  "REGNO (operands[0]) == REGNO (operands[2])"
  [(set (match_dup 0) (extend_types:SI (match_dup 1)))]
)

(define_insn "comparesi3_<extend_types:code><small_int_modes:mode>"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI                               0 "register_operand" "r")
		    (extend_types:SI (match_operand:small_int_modes 1 "rx_restricted_mem_operand" "Q"))))]
  "(optimize < 3 || optimize_size)"
  "cmp\t%<extend_types:letter>1, %0"
  [(set_attr "timings" "33")
   (set_attr "length"  "5")] ;; This length is corrected in rx_adjust_insn_length
)

;; Floating Point Instructions

(define_insn "addsf3"
  [(set (match_operand:SF          0 "register_operand"  "=r,r,r")
	(plus:SF (match_operand:SF 1 "register_operand"  "%0,0,0")
		 (match_operand:SF 2 "rx_source_operand"  "r,F,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "fadd\t%2, %0"
  [(set_attr "timings" "44,44,66")
   (set_attr "length" "3,7,5")]
)

(define_insn "divsf3"
  [(set (match_operand:SF         0 "register_operand" "=r,r,r")
	(div:SF (match_operand:SF 1 "register_operand"  "0,0,0")
		(match_operand:SF 2 "rx_source_operand" "r,F,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "fdiv\t%2, %0"
  [(set_attr "timings" "1616,1616,1818")
   (set_attr "length" "3,7,5")]
)

(define_insn "mulsf3"
  [(set (match_operand:SF          0 "register_operand" "=r,r,r")
	(mult:SF (match_operand:SF 1 "register_operand" "%0,0,0")
		(match_operand:SF  2 "rx_source_operand" "r,F,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "fmul\t%2, %0"
  [(set_attr "timings" "33,33,55")
   (set_attr "length"  "3,7,5")]
)

(define_insn "subsf3"
  [(set (match_operand:SF           0 "register_operand" "=r,r,r")
	(minus:SF (match_operand:SF 1 "register_operand"  "0,0,0")
		  (match_operand:SF 2 "rx_source_operand" "r,F,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "fsub\t%Q2, %0"
  [(set_attr "timings" "44,44,66")
   (set_attr "length" "3,7,5")]
)

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI         0 "register_operand"  "=r,r")
	(fix:SI (match_operand:SF 1 "rx_compare_operand" "r,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "ftoi\t%Q1, %0"
  [(set_attr "timings" "22,44")
   (set_attr "length" "3,5")]
)

(define_insn "floatsisf2"
  [(set (match_operand:SF           0 "register_operand"  "=r,r")
	(float:SF (match_operand:SI 1 "rx_compare_operand" "r,Q")))
   (clobber (reg:CC CC_REG))]
  "ALLOW_RX_FPU_INSNS"
  "itof\t%Q1, %0"
  [(set_attr "timings" "22,44")
   (set_attr "length" "3,6")]
)

;; Bit manipulation instructions.

;; ??? The *_in_memory patterns will not be matched without further help.
;; At one time we had the insv expander generate them, but I suspect that
;; in general we get better performance by exposing the register load to
;; the optimizers.
;;
;; An alternate solution would be to re-organize these patterns such
;; that allow both register and memory operands.  This would allow the
;; register allocator to spill and not load the register operand.  This
;; would be possible only for operations for which we have a constant
;; bit offset, so that we can adjust the address by ofs/8 and replace
;; the offset in the insn by ofs%8.

(define_insn "*bitset"
  [(set (match_operand:SI                    0 "register_operand" "=r")
	(ior:SI (ashift:SI (const_int 1)
			   (match_operand:SI 1 "rx_shift_operand" "ri"))
		(match_operand:SI            2 "register_operand" "0")))]
  ""
  "bset\t%1, %0"
  [(set_attr "length" "3")]
)

(define_insn "*bitset_in_memory"
  [(set (match_operand:QI                    0 "rx_restricted_mem_operand" "+Q")
	(ior:QI (ashift:QI (const_int 1)
			   (match_operand:QI 1 "nonmemory_operand" "ri"))
		(match_dup 0)))]
  ""
  "bset\t%1, %0.B"
  [(set_attr "length" "3")
   (set_attr "timings" "33")]
)

(define_insn "*bitinvert"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (ashift:SI (const_int 1)
			   (match_operand:SI 1 "rx_shift_operand" "ri"))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "bnot\t%1, %0"
  [(set_attr "length" "3")]
)

(define_insn "*bitinvert_in_memory"
  [(set (match_operand:QI 0 "rx_restricted_mem_operand" "+Q")
	(xor:QI (ashift:QI (const_int 1)
			   (match_operand:QI 1 "nonmemory_operand" "ri"))
		(match_dup 0)))]
  ""
  "bnot\t%1, %0.B"
  [(set_attr "length" "5")
   (set_attr "timings" "33")]
)

(define_insn "*bitclr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI
		  (ashift:SI
		    (const_int 1)
		    (match_operand:SI 1 "rx_shift_operand" "ri")))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "bclr\t%1, %0"
  [(set_attr "length" "3")]
)

(define_insn "*bitclr_in_memory"
  [(set (match_operand:QI 0 "rx_restricted_mem_operand" "+Q")
	(and:QI (not:QI
		  (ashift:QI
		    (const_int 1)
		    (match_operand:QI 1 "nonmemory_operand" "ri")))
		(match_dup 0)))]
  ""
  "bclr\t%1, %0.B"
  [(set_attr "length" "3")
   (set_attr "timings" "33")]
)

(define_insn "*insv_imm"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "+r")
	  (const_int 1)
	  (match_operand:SI 1 "rx_shift_operand" "ri"))
	(match_operand:SI 2 "const_int_operand" ""))]
  ""
{
  if (INTVAL (operands[2]) & 1)
    return "bset\t%1, %0";
  else
    return "bclr\t%1, %0";
}
  [(set_attr "length" "3")]
)

(define_insn_and_split "rx_insv_reg"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "+r")
	  (const_int 1)
	  (match_operand:SI 1 "const_int_operand" ""))
	(match_operand:SI 2 "register_operand" "r"))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
	(match_dup 3))]
{
  rtx flags, x;

  /* Emit tst #1, op2.  */
  flags = gen_rtx_REG (CC_ZSmode, CC_REG);
  x = gen_rtx_AND (SImode, operands[2], const1_rtx);
  x = gen_rtx_COMPARE (CC_ZSmode, x, const0_rtx);
  x = gen_rtx_SET (VOIDmode, flags, x);
  emit_insn (x);

  /* Emit bmne.  */
  operands[3] = gen_rtx_NE (SImode, flags, const0_rtx);
})

(define_insn_and_split "*insv_cond"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "+r")
	  (const_int 1)
	  (match_operand:SI 1 "const_int_operand" ""))
	(match_operator:SI 4 "comparison_operator"
	  [(match_operand:SI 2 "register_operand" "r")
	   (match_operand:SI 3 "rx_source_operand" "riQ")]))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
	(match_dup 4))]
{
  rtx flags, x;

  flags = gen_rtx_REG (CCmode, CC_REG);
  x = gen_rtx_COMPARE (CCmode, operands[2], operands[3]);
  x = gen_rtx_SET (VOIDmode, flags, x);
  emit_insn (x);

  operands[4] = gen_rtx_fmt_ee (GET_CODE (operands[4]), SImode,
			        flags, const0_rtx);
})

(define_insn "*bmcc"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "+r")
	  (const_int 1)
	  (match_operand:SI 1 "const_int_operand" ""))
	(match_operator:SI 2 "comparison_operator"
	  [(reg CC_REG) (const_int 0)]))]
  "reload_completed"
  "bm%B2\t%1, %0"
  [(set_attr "length" "3")]
)

;; Work around the fact that X=Y<0 is preferentially expanded as a shift.
(define_insn_and_split "*insv_cond_lt"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand" "+r")
	  (const_int 1)
	  (match_operand:SI 1 "const_int_operand" ""))
	(match_operator:SI 3 "rshift_operator"
	  [(match_operand:SI 2 "register_operand" "r")
	   (const_int 31)]))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(parallel [(set (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
		   (lt:SI (match_dup 2) (const_int 0)))
	      (clobber (reg:CC CC_REG))])]
  ""
)

(define_expand "insv"
  [(set (zero_extract:SI
	  (match_operand:SI 0 "register_operand")	;; Destination
	  (match_operand:SI 1 "const_int_operand")	;; # of bits to set
	  (match_operand:SI 2 "nonmemory_operand"))	;; Starting bit
	(match_operand:SI   3 "nonmemory_operand"))]	;; Bits to insert
  ""
{
  /* We only handle single-bit inserts.  */
  if (!CONST_INT_P (operands[1]) || INTVAL (operands[1]) != 1)
    FAIL;

  /* Either the bit to insert or the position must be constant.  */
  if (CONST_INT_P (operands[3]))
    operands[3] = GEN_INT (INTVAL (operands[3]) & 1);
  else if (CONST_INT_P (operands[2]))
    {
      emit_insn (gen_rx_insv_reg (operands[0], operands[2], operands[3]));
      DONE;
    }
  else
    FAIL;
})

;; Atomic exchange operation.

(define_insn "sync_lock_test_and_setsi"
  [(set (match_operand:SI 0 "register_operand"   "=r,r")
	(match_operand:SI 1 "rx_compare_operand" "=r,Q"))
   (set (match_dup 1)
	(match_operand:SI 2 "register_operand"    "0,0"))]
  ""
  "xchg\t%1, %0"
  [(set_attr "length" "3,6")
   (set_attr "timings" "22")]
)

;; Block move functions.

(define_expand "movstr"
  [(set (match_operand:BLK 1 "memory_operand")    ;; Dest
	(match_operand:BLK 2 "memory_operand"))   ;; Source
   (use (match_operand:SI  0 "register_operand")) ;; Updated Dest
  ]
  ""
  {
    rtx addr1 = gen_rtx_REG (SImode, 1);
    rtx addr2 = gen_rtx_REG (SImode, 2);
    rtx len   = gen_rtx_REG (SImode, 3);
    rtx dest_copy = gen_reg_rtx (SImode);

    emit_move_insn (len, GEN_INT (-1));
    emit_move_insn (addr1, force_operand (XEXP (operands[1], 0), NULL_RTX));
    emit_move_insn (addr2, force_operand (XEXP (operands[2], 0), NULL_RTX));
    operands[1] = replace_equiv_address_nv (operands[1], addr1);
    operands[2] = replace_equiv_address_nv (operands[2], addr2);
    emit_move_insn (dest_copy, addr1);
    emit_insn (gen_rx_movstr ());
    emit_move_insn (len, GEN_INT (-1));
    emit_insn (gen_rx_strend (operands[0], dest_copy));
    DONE;
  }
)

(define_insn "rx_movstr"
  [(set (mem:BLK (reg:SI 1))
	(mem:BLK (reg:SI 2)))
   (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_MOVSTR)
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))]
  ""
  "smovu"
  [(set_attr "length" "2")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_insn "rx_strend"
  [(set (match_operand:SI                      0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand"  "r")
				(reg:SI 3)] UNSPEC_STRLEN))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:CC CC_REG))
   ]
  ""
  "mov\t%1, r1\n\tmov\t#0, r2\n\tsuntil.b\n\tmov\tr1, %0\n\tsub\t#1, %0"
  [(set_attr "length" "10")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_expand "movmemsi"
  [(parallel
    [(set (match_operand:BLK 0 "memory_operand")    ;; Dest
	  (match_operand:BLK 1 "memory_operand"))   ;; Source
     (use (match_operand:SI  2 "register_operand")) ;; Length in bytes
     (match_operand          3 "immediate_operand") ;; Align
     (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_MOVMEM)]
    )]
  ""
  {
    rtx addr1 = gen_rtx_REG (SImode, 1);
    rtx addr2 = gen_rtx_REG (SImode, 2);
    rtx len   = gen_rtx_REG (SImode, 3);

    /* Do not use when the source or destination are volatile - the SMOVF
       instruction will read and write in word sized blocks, which may be
       outside of the valid address range.  */
    if (MEM_P (operands[0]) && MEM_VOLATILE_P (operands[0]))
      FAIL;
    if (MEM_P (operands[1]) && MEM_VOLATILE_P (operands[1]))
      FAIL;

    if (REG_P (operands[0]) && (REGNO (operands[0]) == 2
				      || REGNO (operands[0]) == 3))
      FAIL;
    if (REG_P (operands[1]) && (REGNO (operands[1]) == 1
				      || REGNO (operands[1]) == 3))
      FAIL;
    if (REG_P (operands[2]) && (REGNO (operands[2]) == 1
				      || REGNO (operands[2]) == 2))
      FAIL;

    emit_move_insn (addr1, force_operand (XEXP (operands[0], 0), NULL_RTX));
    emit_move_insn (addr2, force_operand (XEXP (operands[1], 0), NULL_RTX));
    emit_move_insn (len, force_operand (operands[2], NULL_RTX));
    operands[0] = replace_equiv_address_nv (operands[0], addr1);
    operands[1] = replace_equiv_address_nv (operands[1], addr2);
    emit_insn (gen_rx_movmem ());
    DONE;
  }
)

(define_insn "rx_movmem"
  [(set (mem:BLK (reg:SI 1))
	(mem:BLK (reg:SI 2)))
   (use (reg:SI 3))
   (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_MOVMEM)
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))]
  ""
  "smovf"
  [(set_attr "length" "2")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_expand "setmemsi"
  [(set (match_operand:BLK 0 "memory_operand")     ;; Dest
        (match_operand:QI  2 "nonmemory_operand")) ;; Value
   (use (match_operand:SI  1 "nonmemory_operand")) ;; Length
   (match_operand          3 "immediate_operand")  ;; Align
   (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_SETMEM)]
  ""
  {
    rtx addr = gen_rtx_REG (SImode, 1);
    rtx val  = gen_rtx_REG (QImode, 2);
    rtx len  = gen_rtx_REG (SImode, 3);

    emit_move_insn (addr, force_operand (XEXP (operands[0], 0), NULL_RTX));
    emit_move_insn (len, force_operand (operands[1], NULL_RTX));
    emit_move_insn (val, operands[2]);
    emit_insn (gen_rx_setmem ());
    DONE;
  }
)

(define_insn "rx_setmem"
  [(set (mem:BLK (reg:SI 1))
	(unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_SETMEM))
   (clobber (reg:SI 1))
   (clobber (reg:SI 3))]
  ""
  "sstr.b"
  [(set_attr "length" "2")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_expand "cmpstrnsi"
  [(set (match_operand:SI                       0 "register_operand")   ;; Result
	(unspec_volatile:SI [(match_operand:BLK 1 "memory_operand")     ;; String1
			     (match_operand:BLK 2 "memory_operand")]    ;; String2
			    UNSPEC_CMPSTRN))
   (use (match_operand:SI                       3 "register_operand"))  ;; Max Length
   (match_operand:SI                            4 "immediate_operand")] ;; Known Align
  ""
  {
    rtx str1 = gen_rtx_REG (SImode, 1);
    rtx str2 = gen_rtx_REG (SImode, 2);
    rtx len  = gen_rtx_REG (SImode, 3);
  
    emit_move_insn (str1, force_operand (XEXP (operands[1], 0), NULL_RTX));
    emit_move_insn (str2, force_operand (XEXP (operands[2], 0), NULL_RTX));
    emit_move_insn (len, force_operand (operands[3], NULL_RTX));

    emit_insn (gen_rx_cmpstrn (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_expand "cmpstrsi"
  [(set (match_operand:SI                       0 "register_operand")   ;; Result
	(unspec_volatile:SI [(match_operand:BLK 1 "memory_operand")     ;; String1
			     (match_operand:BLK 2 "memory_operand")]    ;; String2
			    UNSPEC_CMPSTRN))
   (match_operand:SI                            3 "immediate_operand")] ;; Known Align
  ""
  {
    rtx str1 = gen_rtx_REG (SImode, 1);
    rtx str2 = gen_rtx_REG (SImode, 2);
    rtx len  = gen_rtx_REG (SImode, 3);
  
    emit_move_insn (str1, force_reg (SImode, XEXP (operands[1], 0)));
    emit_move_insn (str2, force_reg (SImode, XEXP (operands[2], 0)));
    emit_move_insn (len, GEN_INT (-1));

    emit_insn (gen_rx_cmpstrn (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_insn "rx_cmpstrn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(reg:SI 1) (reg:SI 2) (reg:SI 3)]
			    UNSPEC_CMPSTRN))
   (use (match_operand:BLK 1 "memory_operand" "m"))
   (use (match_operand:BLK 2 "memory_operand" "m"))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:CC CC_REG))]
  ""
  "scmpu		; Perform the string comparison
   mov     #-1, %0      ; Set up -1 result (which cannot be created
                        ; by the SC insn)
   bnc	   ?+		; If Carry is not set skip over
   scne.L  %0		; Set result based on Z flag
?:              	
"
  [(set_attr "length" "9")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

;;   Builtin Functions
;;
;; GCC does not have the ability to generate the following instructions
;; on its own so they are provided as builtins instead.  To use them from
;; a program for example invoke them as __builtin_rx_<insn_name>.  For
;; example:
;;
;;    int short_byte_swap (int arg) { return __builtin_rx_revw (arg); }

;;---------- Accumulator Support ------------------------

;; Multiply & Accumulate (high)
(define_insn "machi"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")]
	      UNSPEC_BUILTIN_MACHI)]
  ""
  "machi\t%0, %1"
  [(set_attr "length" "3")]
)

;; Multiply & Accumulate (low)
(define_insn "maclo"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")]
	      UNSPEC_BUILTIN_MACLO)]
  ""
  "maclo\t%0, %1"
  [(set_attr "length" "3")]
)

;; Multiply (high)
(define_insn "mulhi"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")]
	      UNSPEC_BUILTIN_MULHI)]
  ""
  "mulhi\t%0, %1"
  [(set_attr "length" "3")]
)

;; Multiply (low)
(define_insn "mullo"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")]
	      UNSPEC_BUILTIN_MULLO)]
  ""
  "mullo\t%0, %1"
  [(set_attr "length" "3")]
)

;; Move from Accumulator (high)
(define_insn "mvfachi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)]
		   UNSPEC_BUILTIN_MVFACHI))]
  ""
  "mvfachi\t%0"
  [(set_attr "length" "3")]
)

;; Move from Accumulator (middle)
(define_insn "mvfacmi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)]
		   UNSPEC_BUILTIN_MVFACMI))]
  ""
  "mvfacmi\t%0"
  [(set_attr "length" "3")]
)

;; Move to Accumulator (high)
(define_insn "mvtachi"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")]
		       UNSPEC_BUILTIN_MVTACHI)]
  ""
  "mvtachi\t%0"
  [(set_attr "length" "3")]
)

;; Move to Accumulator (low)
(define_insn "mvtaclo"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")]
		       UNSPEC_BUILTIN_MVTACLO)]
  ""
  "mvtaclo\t%0"
  [(set_attr "length" "3")]
)

;; Round Accumulator
(define_insn "racw"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "i")]
		       UNSPEC_BUILTIN_RACW)]
  ""
  "racw\t%0"
  [(set_attr "length" "3")]
)

;; Repeat multiply and accumulate
(define_insn "rmpa"
  [(unspec:SI [(const_int 0) (reg:SI 1) (reg:SI 2) (reg:SI 3)
	       (reg:SI 4) (reg:SI 5) (reg:SI 6)]
	      UNSPEC_BUILTIN_RMPA)
  (clobber (reg:SI 1))
  (clobber (reg:SI 2))
  (clobber (reg:SI 3))]
  ""
  "rmpa"
  [(set_attr "length" "2")
   (set_attr "timings" "1010")]
)

;;---------- Arithmetic ------------------------

;; Byte swap (two 16-bit values).
(define_insn "revw"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand"  "r")]
		   UNSPEC_BUILTIN_REVW))]
  ""
  "revw\t%1, %0"
  [(set_attr "length" "3")]
)

;; Round to integer.
(define_insn "lrintsf2"
  [(set (match_operand:SI             0 "register_operand"  "=r,r")
	(unspec:SI [(match_operand:SF 1 "rx_compare_operand" "r,Q")]
		   UNSPEC_BUILTIN_ROUND))
   (clobber (reg:CC CC_REG))]
  ""
  "round\t%1, %0"
  [(set_attr "timings" "22,44")   
   (set_attr "length" "3,5")]
)

;;---------- Control Registers ------------------------

;; Clear Processor Status Word
(define_insn "clrpsw"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "i")]
	      UNSPEC_BUILTIN_CLRPSW)
   (clobber (reg:CC CC_REG))]
  ""
  "clrpsw\t%F0"
  [(set_attr "length" "2")]
)

;; Set Processor Status Word
(define_insn "setpsw"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "i")]
	      UNSPEC_BUILTIN_SETPSW)
   (clobber (reg:CC CC_REG))]
  ""
  "setpsw\t%F0"
  [(set_attr "length" "2")]
)

;; Move from control register
(define_insn "mvfc"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")]
		   UNSPEC_BUILTIN_MVFC))]
  ""
  "mvfc\t%C1, %0"
  [(set_attr "length" "3")]
)

;; Move to control register
(define_insn "mvtc"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "i,i")
	       (match_operand:SI 1 "nonmemory_operand" "r,i")]
	      UNSPEC_BUILTIN_MVTC)]
  ""
  "mvtc\t%1, %C0"
  [(set_attr "length" "3,7")]
  ;; Ignore possible clobbering of the comparison flags in the
  ;; PSW register.  This is a cc0 target so any cc0 setting
  ;; instruction will always be paired with a cc0 user, without
  ;; the possibility of this instruction being placed in between
  ;; them.
)

;; Move to interrupt priority level
(define_insn "mvtipl"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "Uint04")]
	      UNSPEC_BUILTIN_MVTIPL)]
  ""
  "mvtipl\t%0"
  [(set_attr "length" "3")]
)

;;---------- Interrupts ------------------------

;; Break
(define_insn "brk"
  [(unspec_volatile [(const_int 0)]
		    UNSPEC_BUILTIN_BRK)]
  ""
  "brk"
  [(set_attr "length" "1")
   (set_attr "timings" "66")]
)

;; Interrupt
(define_insn "int"
  [(unspec_volatile:SI [(match_operand:SI 0 "immediate_operand" "i")]
		       UNSPEC_BUILTIN_INT)]
  ""
  "int\t%0"
  [(set_attr "length" "3")]
)

;; Wait
(define_insn "wait"
  [(unspec_volatile [(const_int 0)]
		    UNSPEC_BUILTIN_WAIT)]
  ""
  "wait"
  [(set_attr "length" "2")]
)

;;---------- CoProcessor Support ------------------------

;; FIXME: The instructions are currently commented out because
;; the bit patterns have not been finalized, so the assembler
;; does not support them.  Once they are decided and the assembler
;; supports them, enable the instructions here.

;; Move from co-processor register
(define_insn "mvfcp"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")
		    (match_operand:SI 2 "immediate_operand" "i")]
		   UNSPEC_BUILTIN_MVFCP))]
  ""
  "; mvfcp\t%1, %0, %2"
  [(set_attr "length" "5")]
)

;;---------- Misc ------------------------

;; Required by cfglayout.c...
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "length" "1")]
)

(define_expand "pid_addr"
  [(plus:SI (match_operand:SI 0)
	    (const:SI (unspec:SI [(match_operand:SI 1)] UNSPEC_PID_ADDR)))]
  ""
  ""
)
