;; DFA scheduling description for EPIPHANY
;; Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

;; Two automata are defined to reduce number of states
;; which a single large automaton will have. (Factoring)

(define_automaton "inst_pipeline,fpu_pipe")

;; This unit is basically the decode unit of the processor.
;; Since epiphany is a dual issue machine, it is as if there are two
;; units so that any insn can be processed by either one
;; of the decoding unit.

(define_cpu_unit "pipe_01,pipe_02" "inst_pipeline")

;; The fixed point arithmetic unit.

(define_cpu_unit  "int" "inst_pipeline")

;; The floating point unit.

(define_cpu_unit "F0" "fpu_pipe")

;; ----------------------------------------------------
;; This reservation is to simplify the dual issue description.

(define_reservation  "issue"  "pipe_01|pipe_02")

;; This is to express instructions that cannot be paired.

(define_reservation  "d_lock" "pipe_01+pipe_02")

;; We don't model all pipeline stages; we model the issue stage
;; inasmuch as we allow only two instructions to issue simultaneously,
;; and flow instructions prevent any simultaneous issue of another instruction.
;; (This uses pipe_01 and pipe_02).
;; Double issue of 'other' insns is prevented by using the int unit in the
;; E1 stage.
;; Double issue of float instructions is prevented by using F0 in the E1 stage.

(define_insn_reservation "simple_arith" 2
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "move,cmove,compare,shift,misc,mul")
       (eq_attr "length" "4"))
  "issue,int")

; anything but fp / fp_int / v2fp has a bypass
(define_bypass 1 "simple_arith" "simple_arith,simple_arith_2,simple_arith_4,load,store,branch,call,flow")

(define_insn_reservation "simple_arith_2" 2
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "move,cmove,compare,shift,misc,mul")
       (eq_attr "length" "8"))
  "issue,issue+int,int")

(define_insn_reservation "simple_arith_4" 4
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "move,compare,shift,misc,mul")
       (eq_attr "length" "12,16,20,24"))
  "issue,issue+int,issue+int,issue+int,int")

;; Loads have a latency of two.
;; Note that we fix up the latency of post_modify in epiphany.c:epiphany_adjust_cost

(define_insn_reservation "load" 3
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "load"))
  "issue,int")

; anything but fp / fp_int / v2fp has a bypass
(define_bypass 2 "load" "simple_arith,simple_arith_2,simple_arith_4,load,store,branch,call,flow")

(define_insn_reservation "store" 1
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "store"))
  "issue,int")

;; Branch
;; Latency when taken: 	3
;; Issue Rate: 	1
;; The latency is 1 when the branch is not taken.
;; We can't really do much with the latency, even if we could express it,
;; but the pairing restrictions are useful to take into account.

(define_insn_reservation "branch"  1
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "branch,uncond_branch"))
  "d_lock")

;; calls introduce a longisch delay that is likely to flush the pipelines
;; of the caller's instructions.  Both the call instruction itself and
;; the rts at the end of the call / sfunc incurs a three cycle penalty,
;; thus also isolating the scheduling of caller and callee.

(define_insn_reservation "call" 8
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "call,sfunc,fp_sfunc"))
  "d_lock*8")

(define_insn_reservation "flow" 1
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "flow"))
  "d_lock")

(define_insn_reservation "fp_arith"  5
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "fp,fp_int"))
  "issue,F0")

(define_bypass 4 "fp_arith" "store")

; There are two main consumers for v2fp:
; - other v2fp operation - in that case, the latencies can dovetail to
;   save one cycle of latency.
; - 64 bit store operations - we need both registers, but OTOH the latency is
; one lower to start with.
; of the bypass saving one cyles then.
(define_insn_reservation "v2fp_arith"  5
  (and (eq_attr "pipe_model" "epiphany")
       (eq_attr "type" "v2fp"))
  "issue,issue+F0,F0")

; A boolean attribute for use by peephole2 patterns that try to figure out
; if we overcommitted the FPU.
; This is notionally a numeric attribute to avoid dependency problems.
(define_attr "sched_use_fpu" ""
  (cond [(eq_attr "type" "fp,fp_int,v2fp") (const_int 1)]
	(const_int 0)))
