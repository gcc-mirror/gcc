;;  Machine Description for Renesas RX processors
;;  Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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


;; This code iterator allows all branch instructions to
;; be generated from a single define_expand template.
(define_code_iterator most_cond [eq ne gt ge lt le gtu geu ltu leu
				 unordered ordered ])

;; This code iterator is used for sign- and zero- extensions.
(define_mode_iterator small_int_modes [(HI "") (QI "")])

;; We do not handle DFmode here because it is either
;; the same as SFmode, or if -m64bit-doubles is active
;; then all operations on doubles have to be handled by
;; library functions.
(define_mode_iterator register_modes
  [(SF "ALLOW_RX_FPU_INSNS") (SI "") (HI "") (QI "")])


;; Used to map RX condition names to GCC
;; condition names for builtin instructions.
(define_code_iterator gcc_conds [eq ne gt ge lt le gtu geu ltu leu
				unge unlt uneq ltgt])
(define_code_attr rx_conds [(eq "eq") (ne "ne") (gt "gt") (ge "ge") (lt "lt")
			    (le "le") (gtu "gtu") (geu "geu") (ltu "ltu")
			    (leu "leu") (unge "pz") (unlt "n") (uneq "o")
			    (ltgt "no")])

(define_constants
  [
   (SP_REG 0)

   (UNSPEC_LOW_REG         0)
   (UNSPEC_HIGH_REG        1)

   (UNSPEC_RTE             10)
   (UNSPEC_RTFI            11)
   (UNSPEC_NAKED           12)
   
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
  ]
)

;; Condition code settings:
;;   none     - insn does not affect the condition code bits
;;   set_zs   - insn sets z,s to usable values;
;;   set_zso  - insn sets z,s,o to usable values;
;;   set_zsoc - insn sets z,s,o,c to usable values;
;;   clobber  - value of cc0 is unknown
(define_attr "cc" "none,set_zs,set_zso,set_zsoc,clobber" (const_string "none"))

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

;; Comparisons

(define_expand "cbranchsi4"
  [(set (cc0) (compare:CC (match_operand:SI 1 "register_operand")
			  (match_operand:SI 2 "rx_source_operand")))
   (set (pc)
	(if_then_else (match_operator:SI  0 "comparison_operator"
					  [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  ""
)

(define_expand "cbranchsf4"
  [(set (cc0) (compare:CC (match_operand:SF 1 "register_operand")
			  (match_operand:SF 2 "rx_source_operand")))
   (set (pc)
	(if_then_else (match_operator:SI  0 "comparison_operator"
					  [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "ALLOW_RX_FPU_INSNS && ! flag_non_call_exceptions"
  ""
)

;; The TST instruction is not used as it does not set the Carry flag,
;; so for example, the LessThan comparison cannot be tested.
;;
;; (define_insn "tstsi"
;;   [(set (cc0)
;;         (match_operand:SI 0 "rx_source_operand"  "r,i,Q")))]
;;   ""
;;   {
;;     rx_float_compare_mode = false;
;;     return "tst\t%Q0";
;;   }
;;   [(set_attr "cc" "set_zs")
;;    (set_attr "timings" "11,11,33")
;;    (set_attr "length" "3,7,6")]
;; )

(define_insn "cmpsi"
  [(set (cc0) (compare:CC
	       (match_operand:SI 0 "register_operand"  "r,r,r,r,r,r,r")
	       (match_operand:SI 1 "rx_source_operand"
				 "r,Uint04,Int08,Sint16,Sint24,i,Q")))]
  ""
  {
    rx_float_compare_mode = false;
    return "cmp\t%Q1, %Q0";
  }
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "11,11,11,11,11,11,33")
   (set_attr "length"  "2,2,3,4,5,6,5")]
)

;; This pattern is disabled when -fnon-call-exceptions is active because
;; it could generate a floating point exception, which would introduce an
;; edge into the flow graph between this insn and the conditional branch
;; insn to follow, thus breaking the cc0 relationship.  Run the g++ test
;; g++.dg/eh/080514-1.C to see this happen.
(define_insn "cmpsf"
  [(set (cc0)
	(compare:CC (match_operand:SF 0 "register_operand"  "r,r,r")
		    (match_operand:SF 1 "rx_source_operand" "r,i,Q")))]
  "ALLOW_RX_FPU_INSNS && ! flag_non_call_exceptions"
  {
    rx_float_compare_mode = true;
    return "fcmp\t%1, %0";
  }
  [(set_attr "cc" "set_zso")
   (set_attr "timings" "11,11,33")
   (set_attr "length" "3,7,5")]
)

;; Flow Control Instructions:

(define_expand "b<code>"
  [(set (pc)
        (if_then_else (most_cond (cc0) (const_int 0))
                      (label_ref (match_operand 0))
                      (pc)))]
  ""
  ""
)

(define_insn "*conditional_branch"
  [(set (pc)
	(if_then_else (match_operator           1 "comparison_operator"
						[(cc0) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  {
    return rx_gen_cond_branch_template (operands[1], false);
  }
  [(set_attr "length" "8")    ;; This length is wrong, but it is
                              ;; too hard to compute statically.
   (set_attr "timings" "33")  ;; The timing assumes that the branch is taken.
   (set_attr "cc" "clobber")] ;; FIXME: This clobber is wrong.
)

(define_insn "*reveresed_conditional_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  {
    return rx_gen_cond_branch_template (operands[1], true);
  }
  [(set_attr "length" "8")    ;; This length is wrong, but it is
                              ;; too hard to compute statically.
   (set_attr "timings" "33")  ;; The timing assumes that the branch is taken.
   (set_attr "cc" "clobber")] ;; FIXME: This clobber is wrong.
)

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bra\t%0"
  [(set_attr "length" "4")
   (set_attr "timings" "33")
   (set_attr "cc" "clobber")] ;; FIXME: This clobber is wrong.
)

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "33")
   (set_attr "cc" "clobber")] ;; FIXME: This clobber is wrong.
)

(define_insn "tablejump"
  [(set (pc) (match_operand:SI     0 "register_operand" "r"))
   (use (label_ref (match_operand  1 "" "")))]
  ""
  { return flag_pic ? (TARGET_AS100_SYNTAX ? "\n?:\tbra\t%0"
					   : "\n1:\tbra\t%0")
	                                   : "jmp\t%0";
  }
  [(set_attr "cc" "clobber") ;; FIXME: This clobber is wrong.
   (set_attr "timings" "33")
   (set_attr "length" "2")]
)

(define_insn "simple_return"
  [(return)]
  ""
  "rts"
  [(set_attr "length" "1")
   (set_attr "timings" "55")]
)

(define_insn "deallocate_and_return"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (match_operand:SI 0 "immediate_operand" "i")))
   (return)]
  ""
  "rtsd\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "55")]
)

(define_insn "pop_and_return"
  [(match_parallel                    1 "rx_rtsd_vector"
		   [(set:SI (reg:SI SP_REG)
			    (plus:SI (reg:SI SP_REG)
				     (match_operand:SI
				      0 "const_int_operand" "n")))])]
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
;; or label_refs as legitmate memory addresses.  This matches the
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
    emit_call_insn (gen_call_internal (dest, operands[1]));
    DONE;
  }
)

(define_insn "call_internal"
  [(call (mem:QI (match_operand:SI 0 "rx_call_operand" "r,Symbol"))
	 (match_operand:SI         1 "general_operand" "g,g"))]
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
    emit_call_insn (gen_call_value_internal (operands[0], dest, operands[2]));
    DONE;
  }
)

(define_insn "call_value_internal"
  [(set (match_operand                  0 "register_operand" "=r,r")
	(call (mem:QI (match_operand:SI 1 "rx_call_operand"   "r,Symbol"))
	      (match_operand:SI         2 "general_operand"   "g,g")))]
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
  }
)

(define_insn "sibcall_internal"
  [(call (mem:QI (match_operand:SI 0 "rx_symbolic_call_operand" "Symbol"))
	 (match_operand:SI         1 "general_operand"          "g"))
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
  }
)

(define_insn "sibcall_value_internal"
 [(set (match_operand                  0 "register_operand"         "=r")
       (call (mem:QI (match_operand:SI 1 "rx_symbolic_call_operand" "Symbol"))
	     (match_operand:SI         2 "general_operand"          "g")))
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
    if (MEM_P (operand0) && MEM_P (operand1))
      operands[1] = copy_to_mode_reg (<register_modes:MODE>mode, operand1);
  }
)

(define_insn "*mov<register_modes:mode>_internal"
  [(set (match_operand:register_modes
	 0 "nonimmediate_operand" "=r,r,r,r,r,r,m,Q,Q,Q,Q")
	(match_operand:register_modes
	 1 "general_operand" "Int08,Sint16,Sint24,i,r,m,r,Int08,Sint16,Sint24,i"))]
  ""
  { return rx_gen_move_template (operands, false); }
  [(set_attr "length" "3,4,5,6,2,4,6,5,6,7,8")
   (set_attr "timings" "11,11,11,11,11,12,11,11,11,11,11")]
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
  [(set:SI (reg:SI SP_REG)
	   (minus:SI (reg:SI SP_REG)
		     (const_int 4)))
   (set:SI (mem:SI (reg:SI SP_REG))
	   (match_operand:SI 0 "register_operand" "r"))]
  ""
  "push.l\t%0"
  [(set_attr "length" "2")]
)

(define_insn "stack_pushm"
  [(match_parallel                     1 "rx_store_multiple_vector"
		   [(set:SI (reg:SI SP_REG)
			    (minus:SI (reg:SI SP_REG)
				      (match_operand:SI
				       0 "const_int_operand" "n")))])]
  "reload_completed"
  {
    rx_emit_stack_pushm (operands);
    return "";
  }
  [(set_attr "length" "2")
   (set_attr "timings" "44")] ;; The timing is a guesstimate average timing.
)

(define_insn "stack_pop"
  [(set:SI (match_operand:SI 0 "register_operand" "=r")
	   (mem:SI (reg:SI SP_REG)))
   (set:SI (reg:SI SP_REG)
	   (plus:SI (reg:SI SP_REG)
		    (const_int 4)))]
  ""
  "pop\t%0"
  [(set_attr "length" "2")
   (set_attr "timings" "12")]
)

(define_insn "stack_popm"
  [(match_parallel                     1 "rx_load_multiple_vector"
		   [(set:SI (reg:SI SP_REG)
			    (plus:SI (reg:SI SP_REG)
				     (match_operand:SI
				      0 "const_int_operand" "n")))])]
  "reload_completed"
  {
    rx_emit_stack_popm (operands, true);
    return "";
  }
  [(set_attr "length" "2")
   (set_attr "timings" "45")] ;; The timing is a guesstimate average timing.
)

(define_insn "cstoresi4"
  [(set (match_operand:SI  0 "register_operand" "=r,r,r,r,r,r,r")
	(match_operator:SI
	 1 "comparison_operator"
	 [(match_operand:SI
	   2 "register_operand"  "r,r,r,r,r,r,r")
	  (match_operand:SI
	   3 "rx_source_operand" "r,Uint04,Int08,Sint16,Sint24,i,Q")]))]
  ""
  {
    rx_float_compare_mode = false;
    return "cmp\t%Q3, %Q2\n\tsc%B1.L\t%0";
  }
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "22,22,22,22,22,22,44")
   (set_attr "length"  "5,5,6,7,8,9,8")]
)

(define_expand "movsicc"
  [(set (match_operand:SI                   0 "register_operand")
        (if_then_else:SI (match_operand:SI 1 "comparison_operator")
			 (match_operand:SI  2 "nonmemory_operand")
			 (match_operand:SI  3 "immediate_operand")))]
  ""
  {
    if (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE)
      FAIL;
    if (! CONST_INT_P (operands[3]))
      FAIL;
  }
)

(define_insn "*movsieq"
  [(set (match_operand:SI     0 "register_operand" "=r,r,r")
	(if_then_else:SI (eq (match_operand:SI
			      3 "register_operand"  "r,r,r")
			     (match_operand:SI
			      4 "rx_source_operand" "riQ,riQ,riQ"))
			 (match_operand:SI
			  1 "nonmemory_operand"     "0,i,r")
			 (match_operand:SI
			  2 "immediate_operand"     "i,i,i")))]
  ""
  "@
  cmp\t%Q4, %Q3\n\tstnz\t%2, %0
  cmp\t%Q4, %Q3\n\tmov.l\t%2, %0\n\tstz\t%1, %0
  cmp\t%Q4, %Q3\n\tmov.l\t%1, %0\n\tstnz\t%2, %0"
  [(set_attr "cc"      "set_zsoc")
   (set_attr "length"  "13,19,15")
   (set_attr "timings" "22,33,33")]
)

(define_insn "*movsine"
  [(set (match_operand:SI                      0 "register_operand" "=r,r,r")
	(if_then_else:SI (ne (match_operand:SI 3 "register_operand"  "r,r,r")
			     (match_operand:SI 4 "rx_source_operand" "riQ,riQ,riQ"))
			 (match_operand:SI     1 "nonmemory_operand" "0,i,r")
			 (match_operand:SI     2 "immediate_operand" "i,i,i")))]
  ""
  "@
  cmp\t%Q4, %Q3\n\tstz\t%2, %0
  cmp\t%Q4, %Q3\n\tmov.l\t%2, %0\n\tstnz\t%1, %0
  cmp\t%Q4, %Q3\n\tmov.l\t%1, %0\n\tstz\t%2, %0"
  [(set_attr "cc"      "set_zsoc")
   (set_attr "length"  "13,19,15")
   (set_attr "timings" "22,33,33")]
)

;; Arithmetic Instructions

(define_insn "abssi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (abs:SI (match_operand:SI 1 "register_operand"  "0,r")))]
  ""
  "@
  abs\t%0
  abs\t%1, %0"
  [(set_attr "cc" "set_zso")
   (set_attr "length" "2,3")]
)

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"
			  "=r,r,r,r,r,r,r,r,r,r,r,r")
	(plus:SI (match_operand:SI
		  1 "register_operand"
		  "%0,0,0,0,0,0,r,r,r,r,r,0")
		 (match_operand:SI
		  2 "rx_source_operand"
		  "r,Uint04,Sint08,Sint16,Sint24,i,r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "@
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%2, %1, %0
  add\t%Q2, %0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "11,11,11,11,11,11,11,11,11,11,11,33")
   (set_attr "length" "2,2,3,4,5,6,3,3,4,5,6,5")]
)

(define_insn "adddi3"
  [(set (match_operand:DI          0 "register_operand" "=r,r,r,r,r,r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0,0,0,0,0,0")
		 (match_operand:DI 2 "rx_source_operand"
				   "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "add\t%L2, %L0\n\tadc\t%H2, %H0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "22,22,22,22,22,44")
   (set_attr "length" "5,7,9,11,13,11")]
)

(define_insn "andsi3"
  [(set (match_operand:SI         0 "register_operand"  "=r,r,r,r,r,r,r,r,r")
	(and:SI (match_operand:SI 1 "register_operand"  "%0,0,0,0,0,0,r,0,Q")
		(match_operand:SI
		 2 "rx_source_operand"
		 "r,Uint04,Sint08,Sint16,Sint24,i,r,Q,0")))]
  ""
  "@
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %0
  and\t%2, %1, %0
  and\t%Q2, %0
  and\t%Q1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "11,11,11,11,11,11,11,33,33")
   (set_attr "length" "2,2,3,4,5,6,3,5,5")]
)

;; Byte swap (single 32-bit value).
(define_insn "bswapsi2"
  [(set (match_operand:SI           0 "register_operand" "+r")
	(bswap:SI (match_operand:SI 1 "register_operand"  "r")))]
  ""
  "revl\t%1, %0"
  [(set_attr "length" "3")]
)

;; Byte swap (single 16-bit value).  Note - we ignore the swapping of the high 16-bits.
(define_insn "bswaphi2"
  [(set (match_operand:HI           0 "register_operand" "+r")
	(bswap:HI (match_operand:HI 1 "register_operand"  "r")))]
  ""
  "revw\t%1, %0"
  [(set_attr "length" "3")]
)

(define_insn "divsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r")
	(div:SI (match_operand:SI 1 "register_operand"  "0,0,0,0,0,0")
		(match_operand:SI
		 2 "rx_source_operand" "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "div\t%Q2, %0"
  [(set_attr "cc" "clobber")
   (set_attr "timings" "1111") ;; Strictly speaking the timing should be
                               ;; 2222, but that is a worst case sceanario.
   (set_attr "length" "3,4,5,6,7,6")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI          0 "register_operand"  "=r,r,r,r,r,r")
	(udiv:SI (match_operand:SI 1 "register_operand"   "0,0,0,0,0,0")
		 (match_operand:SI
		  2 "rx_source_operand"  "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "divu\t%Q2, %0"
  [(set_attr "cc" "clobber")
   (set_attr "timings" "1010") ;; Strictly speaking the timing should be
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
  "@
  emul\t%Q2, %0
  emul\t%Q2, %0
  emul\t%Q2, %0
  emul\t%Q2, %0
  emul\t%Q2, %0
  emul\t%Q2, %0"
  [(set_attr "length" "3,4,5,6,7,6")   
   (set_attr "timings" "22,22,22,22,22,44")]
)

;; See comment for mulsidi3.
;; Note - the zero_extends are to distinguish this pattern from the
;; mulsidi3 pattern.  Immediate mode addressing is not supported
;; because gcc cannot handle the expression: (zero_extend (const_int)).
(define_insn "umulsidi3"
  [(set (match_operand:DI                          0 "register_operand"
						   "=r,r")
        (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"
						   "%0,0"))
                 (zero_extend:DI (match_operand:SI 2 "rx_compare_operand"
						   "r,Q"))))]
  "! TARGET_BIG_ENDIAN_DATA"
  "@
  emulu\t%Q2, %0
  emulu\t%Q2, %0"
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
  "@
  min\t%Q2, %0
  min\t%Q2, %0
  min\t%Q2, %0
  min\t%Q2, %0
  min\t%Q2, %0
  min\t%Q2, %0"
  [(set_attr "length"  "3,4,5,6,7,6")
   (set_attr "timings" "11,11,11,11,11,33")]
)

(define_insn "mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,r,r,r,r,r,r")
        (mult:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,0,Q,r")
                 (match_operand:SI 2 "rx_source_operand"
				   "r,Uint04,Sint08,Sint16,Sint24,i,Q,0,r")))]
  ""
  "@
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q2, %0
  mul\t%Q1, %0
  mul\t%Q2, %1, %0"
  [(set_attr "length"  "2,2,3,4,5,6,5,5,3")
   (set_attr "timings" "11,11,11,11,11,11,33,33,11")]
)

(define_insn "negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
        (neg:SI (match_operand:SI 1 "register_operand"  "0,r")))]
  ;; The NEG instruction does not comply with -fwrapv semantics.
  ;; See gcc.c-torture/execute/pr22493-1.c for an example of this.
  "! flag_wrapv"
  "@
  neg\t%0
  neg\t%1, %0"
  [(set_attr "length" "2,3")]
)

(define_insn "one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=r,r")
	(not:SI (match_operand:SI 1 "register_operand"  "0,r")))]
  ""
  "@
  not\t%0
  not\t%1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "length" "2,3")]
)

(define_insn "iorsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r,r,r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,r,0,Q")
	        (match_operand:SI 2 "rx_source_operand"
				  "r,Uint04,Sint08,Sint16,Sint24,i,r,Q,0")))]
  ""
  "@
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %0
  or\t%2, %1, %0
  or\t%Q2, %0
  or\t%Q1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "11,11,11,11,11,11,11,33,33")
   (set_attr "length"  "2,2,3,4,5,6,3,5,5")]
)

(define_insn "rotlsi3"
  [(set (match_operand:SI            0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand"  "0")
		   (match_operand:SI 2 "rx_shift_operand" "rn")))]
  ""
  "rotl\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "length" "3")]
)

(define_insn "rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "0")
		     (match_operand:SI 2 "rx_shift_operand" "rn")))]
  ""
  "rotr\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "length" "3")]
)

(define_insn "ashrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))]
  ""
  "@
  shar\t%2, %0
  shar\t%2, %0
  shar\t%2, %1, %0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "length" "3,2,3")]
)

(define_insn "lshrsi3"
  [(set (match_operand:SI              0 "register_operand" "=r,r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,0,r")
		     (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))]
  ""
  "@
  shlr\t%2, %0
  shlr\t%2, %0
  shlr\t%2, %1, %0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "length" "3,2,3")]
)

(define_insn "ashlsi3"
  [(set (match_operand:SI            0 "register_operand" "=r,r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,0,r")
	           (match_operand:SI 2 "rx_shift_operand"  "r,n,n")))]
  ""
  "@
  shll\t%2, %0
  shll\t%2, %0
  shll\t%2, %1, %0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "length" "3,2,3")]
)

(define_insn "subsi3"
  [(set (match_operand:SI           0 "register_operand" "=r,r,r,r,r")
	(minus:SI (match_operand:SI 1 "register_operand"  "0,0,0,r,0")
		  (match_operand:SI 2 "rx_source_operand" "r,Uint04,n,r,Q")))]
  ""
  "@
  sub\t%2, %0
  sub\t%2, %0
  add\t%N2, %0
  sub\t%2, %1, %0
  sub\t%Q2, %0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "11,11,11,11,33")
   (set_attr "length" "2,2,6,3,5")]
)

(define_insn "subdi3"
  [(set (match_operand:DI           0 "register_operand" "=r,r")
	(minus:DI (match_operand:DI 1 "register_operand"  "0,0")
		  (match_operand:DI 2 "rx_source_operand" "r,Q")))]
  ""
  "sub\t%L2, %L0\n\tsbb\t%H2, %H0"
  [(set_attr "cc" "set_zsoc")
   (set_attr "timings" "22,44")
   (set_attr "length" "5,11")]
)

(define_insn "xorsi3"
  [(set (match_operand:SI         0 "register_operand" "=r,r,r,r,r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0")
	        (match_operand:SI 2 "rx_source_operand"
				  "r,Sint08,Sint16,Sint24,i,Q")))]
  ""
  "@
  xor\t%Q2, %0
  xor\t%Q2, %0
  xor\t%Q2, %0
  xor\t%Q2, %0
  xor\t%Q2, %0
  xor\t%Q2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "11,11,11,11,11,33")
   (set_attr "length" "3,4,5,6,7,6")]
)

;; Floating Point Instructions

(define_insn "addsf3"
  [(set (match_operand:SF          0 "register_operand"  "=r,r,r")
	(plus:SF (match_operand:SF 1 "register_operand"  "%0,0,0")
		 (match_operand:SF 2 "rx_source_operand"  "r,F,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "@
  fadd\t%2, %0
  fadd\t%2, %0
  fadd\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "44,44,66")
   (set_attr "length" "3,7,5")]
)

(define_insn "divsf3"
  [(set (match_operand:SF         0 "register_operand" "=r,r,r")
	(div:SF (match_operand:SF 1 "register_operand"  "0,0,0")
		(match_operand:SF 2 "rx_source_operand" "r,F,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "fdiv\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "1616,1616,1818")
   (set_attr "length" "3,7,5")]
)

(define_insn "mulsf3"
  [(set (match_operand:SF          0 "register_operand" "=r,r,r")
	(mult:SF (match_operand:SF 1 "register_operand" "%0,0,0")
		(match_operand:SF  2 "rx_source_operand" "r,F,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "@
  fmul\t%2, %0
  fmul\t%2, %0
  fmul\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "33,33,55")
   (set_attr "length"  "3,7,5")]
)

(define_insn "subsf3"
  [(set (match_operand:SF           0 "register_operand" "=r,r,r")
	(minus:SF (match_operand:SF 1 "register_operand"  "0,0,0")
		  (match_operand:SF 2 "rx_source_operand" "r,F,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "fsub\t%2, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "44,44,66")
   (set_attr "length" "3,7,5")]
)

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI         0 "register_operand"  "=r,r")
	(fix:SI (match_operand:SF 1 "rx_compare_operand" "r,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "ftoi\t%1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "22,44")
   (set_attr "length" "3,5")]
)

(define_insn "floatsisf2"
  [(set (match_operand:SF           0 "register_operand"  "=r,r")
	(float:SF (match_operand:SI 1 "rx_compare_operand" "r,Q")))]
  "ALLOW_RX_FPU_INSNS"
  "itof\t%1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "22,44")
   (set_attr "length" "3,6")]
)

;; Bit manipulation instructions.
;; Note - there are two versions of each pattern because the memory
;; accessing versions use QImode whilst the register accessing
;; versions use SImode.
;; The peephole are here because the combiner only looks at a maximum
;; of three instructions at a time.

(define_insn "bitset"
  [(set:SI (match_operand:SI 0 "register_operand" "+r")
	   (ior:SI (match_operand:SI 1 "register_operand" "0")
		   (ashift:SI (const_int 1)
			      (match_operand:SI 2 "nonmemory_operand" "ri"))))]
  ""
  "bset\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "bitset_in_memory"
  [(set:QI (match_operand:QI 0 "memory_operand" "+m")
	   (ior:QI (match_operand:QI 1 "memory_operand" "0")
		   (ashift:QI (const_int 1)
			      (match_operand:QI 2 "nonmemory_operand" "ri"))))]
  ""
  "bset\t%2, %0.B"
  [(set_attr "length" "3")
   (set_attr "timings" "34")]
)

;; (set (reg A) (const_int 1))
;; (set (reg A) (ashift (reg A) (reg B)))
;; (set (reg C) (ior (reg A) (reg C)))
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int 1))
   (set:SI (match_dup 0)
	   (ashift:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_operand:SI 2 "register_operand" "")
	   (ior:SI (match_dup 0)
		   (match_dup 2)))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (ior:SI (match_dup 2)
		   (ashift:SI (const_int 1)
			      (match_dup 1))))]
)
  
;; (set (reg A) (const_int 1))
;; (set (reg A) (ashift (reg A) (reg B)))
;; (set (reg A) (ior (reg A) (reg C)))
;; (set (reg C) (reg A)
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int 1))
   (set:SI (match_dup 0)
	   (ashift:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_dup 0)
	   (ior:SI (match_dup 0)
		   (match_operand:SI 2 "register_operand" "")))
   (set:SI (match_dup 2) (match_dup 0))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (ior:SI (match_dup 2)
		   (ashift:SI (const_int 1)
			      (match_dup 1))))]
)
  
(define_insn "bitinvert"
  [(set:SI (match_operand:SI 0 "register_operand" "+r")
	   (xor:SI (match_operand:SI 1 "register_operand" "0")
		   (ashift:SI (const_int 1)
			      (match_operand:SI 2 "nonmemory_operand" "ri"))))]
  ""
  "bnot\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "bitinvert_in_memory"
  [(set:QI (match_operand:QI 0 "memory_operand" "+m")
	   (xor:QI (match_operand:QI 1 "register_operand" "0")
		   (ashift:QI (const_int 1)
			      (match_operand:QI 2 "nonmemory_operand" "ri"))))]
  ""
  "bnot\t%2, %0.B"
  [(set_attr "length" "5")
   (set_attr "timings" "33")]
)

;; (set (reg A) (const_int 1))
;; (set (reg A) (ashift (reg A) (reg B)))
;; (set (reg C) (xor (reg A) (reg C)))
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int 1))
   (set:SI (match_dup 0)
	   (ashift:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_operand:SI 2 "register_operand" "")
	   (xor:SI (match_dup 0)
		   (match_dup 2)))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (xor:SI (match_dup 2)
		   (ashift:SI (const_int 1)
			      (match_dup 1))))]
  ""
)
  
;; (set (reg A) (const_int 1))
;; (set (reg A) (ashift (reg A) (reg B)))
;; (set (reg A) (xor (reg A) (reg C)))
;; (set (reg C) (reg A))
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int 1))
   (set:SI (match_dup 0)
	   (ashift:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_dup 0)
	   (xor:SI (match_dup 0)
		   (match_operand:SI 2 "register_operand" "")))
   (set:SI (match_dup 2) (match_dup 0))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (xor:SI (match_dup 2)
		   (ashift:SI (const_int 1)
			      (match_dup 1))))]
  ""
)

(define_insn "bitclr"
  [(set:SI (match_operand:SI 0 "register_operand" "+r")
	   (and:SI (match_operand:SI 1 "register_operand" "0")
		   (not:SI (ashift:SI (const_int 1)
				      (match_operand:SI 2 "nonmemory_operand" "ri")))))]
  ""
  "bclr\t%2, %0"
  [(set_attr "length" "3")]
)

(define_insn "bitclr_in_memory"
  [(set:QI (match_operand:QI 0 "memory_operand" "+m")
	   (and:QI (match_operand:QI 1 "memory_operand" "0")
		   (not:QI (ashift:QI (const_int 1)
				      (match_operand:QI 2 "nonmemory_operand" "ri")))))]
  ""
  "bclr\t%2, %0.B"
  [(set_attr "length" "3")
   (set_attr "timings" "34")]
)

;; (set (reg A) (const_int -2))
;; (set (reg A) (rotate (reg A) (reg B)))
;; (set (reg C) (and (reg A) (reg C)))
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int -2))
   (set:SI (match_dup 0)
	   (rotate:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_operand:SI 2 "register_operand" "")
	   (and:SI (match_dup 0)
		   (match_dup 2)))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (and:SI (match_dup 2)
		   (not:SI (ashift:SI (const_int 1)
				      (match_dup 1)))))]
)
  
;; (set (reg A) (const_int -2))
;; (set (reg A) (rotate (reg A) (reg B)))
;; (set (reg A) (and (reg A) (reg C)))
;; (set (reg C) (reg A)
(define_peephole2
  [(set:SI (match_operand:SI 0 "register_operand" "")
	   (const_int -2))
   (set:SI (match_dup 0)
	   (rotate:SI (match_dup 0)
		      (match_operand:SI 1 "register_operand" "")))
   (set:SI (match_dup 0)
	   (and:SI (match_dup 0)
		   (match_operand:SI 2 "register_operand" "")))
   (set:SI (match_dup 2) (match_dup 0))]
  "dead_or_set_p (insn, operands[0])"
  [(set:SI (match_dup 2)
	   (and:SI (match_dup 2)
		   (not:SI (ashift:SI (const_int 1)
				      (match_dup 1)))))]
)

(define_expand "insv"
  [(set:SI (zero_extract:SI (match_operand:SI
			     0 "nonimmediate_operand") ;; Destination
		            (match_operand
			     1 "immediate_operand")    ;; # of bits to set
			    (match_operand
			     2 "immediate_operand"))   ;; Starting bit
	   (match_operand
	    3 "immediate_operand"))]  ;; Bits to insert
  ""
  {
    if (rx_expand_insv (operands))
      DONE;
    FAIL;
  }
)   

;; Atomic exchange operation.

(define_insn "sync_lock_test_and_setsi"
  [(set:SI (match_operand:SI 0 "register_operand"   "=r,r")
	   (match_operand:SI 1 "rx_compare_operand" "=r,Q"))
   (set:SI (match_dup 1)
	   (match_operand:SI 2 "register_operand"    "0,0"))]
  ""
  "xchg\t%1, %0"
  [(set_attr "length" "3,6")
   (set_attr "timings" "22")]
)

;; Block move functions.

(define_expand "movstr"
  [(set:SI (match_operand:BLK 1 "memory_operand")    ;; Dest
	   (match_operand:BLK 2 "memory_operand"))   ;; Source
   (use (match_operand:SI     0 "register_operand")) ;; Updated Dest
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
  [(set:SI (mem:BLK (reg:SI 1))
	   (mem:BLK (reg:SI 2)))
   (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_MOVSTR)
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
  ]
  ""
  "smovu"
  [(set_attr "length" "2")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_insn "rx_strend"
  [(set:SI (match_operand:SI                      0 "register_operand" "=r")
	   (unspec_volatile:SI [(match_operand:SI 1 "register_operand"  "r")
				(reg:SI 3)] UNSPEC_STRLEN))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
  ]
  ""
  "mov\t%1, r1\n\tmov\t#0, r2\n\tsuntil.b\n\tmov\tr1, %0\n\tsub\t#1, %0"
  [(set_attr "length" "10")
   (set_attr "cc" "clobber")
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
  [(set:BLK (mem:BLK (reg:SI 1)) (reg 2))
   (unspec_volatile:BLK [(reg:SI 1) (reg:SI 2) (reg:SI 3)] UNSPEC_SETMEM)
   (clobber (reg:SI 1))
   (clobber (reg:SI 3))]
  ""
  "sstr.b"
  [(set_attr "length" "2")
   (set_attr "timings" "1111")] ;; The timing is a guesstimate.
)

(define_expand "cmpstrnsi"
  [(set (match_operand:SI
	 0 "register_operand") ;; Result
	(unspec_volatile:SI [(match_operand:BLK
			      1 "memory_operand") ;; String1
			     (match_operand:BLK
			      2 "memory_operand")] ;; String2
			    UNSPEC_CMPSTRN))
   (use (match_operand:SI
	 3 "register_operand")) ;; Max Length
   (match_operand:SI
    4 "immediate_operand")] ;; Known Align
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
  [(set (match_operand:SI
	 0 "register_operand") ;; Result
	(unspec_volatile:SI [(match_operand:BLK
			      1 "memory_operand")  ;; String1
			     (match_operand:BLK
			      2 "memory_operand")] ;; String2
			    UNSPEC_CMPSTRN))
   (match_operand:SI
    3 "immediate_operand")] ;; Known Align
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
  [(set:SI (match_operand:SI 0 "register_operand" "=r")
	   (unspec_volatile:SI [(reg:SI 1) (reg:SI 2) (reg:SI 3)]
			       UNSPEC_CMPSTRN))
   (use (match_operand:BLK   1 "memory_operand" "m"))
   (use (match_operand:BLK   2 "memory_operand" "m"))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))]
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
  [(set (match_operand:SI             0 "register_operand" "+r")
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
		   UNSPEC_BUILTIN_ROUND))]
  ""
  "round\t%1, %0"
  [(set_attr "cc" "set_zs")
   (set_attr "timings" "22,44")   
   (set_attr "length" "3,5")]
)

;; Saturate to 32-bits
(define_insn "sat"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand"  "0")]
		   UNSPEC_BUILTIN_SAT))]
  ""
  "sat\t%0"
  [(set_attr "length" "2")]
)

;;---------- Control Registers ------------------------

;; Clear Processor Status Word
(define_insn "clrpsw"
  [(unspec:SI [(match_operand:SI 0 "immediate_operand" "i")]
	      UNSPEC_BUILTIN_CLRPSW)
   (clobber (cc0))]
  ""
  "clrpsw\t%F0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")]
)

;; Set Processor Status Word
(define_insn "setpsw"
  [(unspec:SI [(match_operand:SI 0 "immediate_operand" "i")]
	      UNSPEC_BUILTIN_SETPSW)
   (clobber (cc0))]
  ""
  "setpsw\t%F0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")]
)

;; Move from control register
(define_insn "mvfc"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")]
		   UNSPEC_BUILTIN_MVFC))]
  ""
  "mvfc\t%C1, %0"
  [(set_attr "length" "3")]
)

;; Move to control register
(define_insn "mvtc"
  [(unspec:SI [(match_operand:SI 0 "immediate_operand" "i,i")
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
  [(unspec:SI [(match_operand:SI 0 "immediate_operand" "Uint04")]
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
