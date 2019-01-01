;; Pipeline model for ST Microelectronics Loongson-2E/2F cores.

;; Copyright (C) 2008-2019 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.
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

(define_c_enum "unspec" [
  UNSPEC_LOONGSON_ALU1_TURN_ENABLED_INSN
  UNSPEC_LOONGSON_ALU2_TURN_ENABLED_INSN
  UNSPEC_LOONGSON_FALU1_TURN_ENABLED_INSN
  UNSPEC_LOONGSON_FALU2_TURN_ENABLED_INSN
])

;; Automaton for integer instructions.
(define_automaton "ls2_alu")

;; ALU1 and ALU2.
;; We need to query these units to adjust round-robin counter.
(define_query_cpu_unit "ls2_alu1_core,ls2_alu2_core" "ls2_alu")

;; Pseudo units to help modeling of ALU1/2 round-robin dispatch strategy.
(define_cpu_unit "ls2_alu1_turn,ls2_alu2_turn" "ls2_alu")

;; Pseudo units to enable/disable ls2_alu[12]_turn units.
;; ls2_alu[12]_turn unit can be subscribed only after ls2_alu[12]_turn_enabled
;; unit is subscribed.
(define_cpu_unit "ls2_alu1_turn_enabled,ls2_alu2_turn_enabled" "ls2_alu")
(presence_set "ls2_alu1_turn" "ls2_alu1_turn_enabled")
(presence_set "ls2_alu2_turn" "ls2_alu2_turn_enabled")

;; Reservations for ALU1 (ALU2) instructions.
;; Instruction goes to ALU1 (ALU2) and makes next ALU1/2 instruction to
;; be dispatched to ALU2 (ALU1).
(define_reservation "ls2_alu1"
  "(ls2_alu1_core+ls2_alu2_turn_enabled)|ls2_alu1_core")
(define_reservation "ls2_alu2"
  "(ls2_alu2_core+ls2_alu1_turn_enabled)|ls2_alu2_core")

;; Reservation for ALU1/2 instructions.
;; Instruction will go to ALU1 iff ls2_alu1_turn_enabled is subscribed and
;; switch the turn to ALU2 by subscribing ls2_alu2_turn_enabled.
;; Or to ALU2 otherwise.
(define_reservation "ls2_alu"
  "(ls2_alu1_core+ls2_alu1_turn+ls2_alu2_turn_enabled)
   |(ls2_alu1_core+ls2_alu1_turn)
   |(ls2_alu2_core+ls2_alu2_turn+ls2_alu1_turn_enabled)
   |(ls2_alu2_core+ls2_alu2_turn)")

;; Automaton for floating-point instructions.
(define_automaton "ls2_falu")

;; FALU1 and FALU2.
;; We need to query these units to adjust round-robin counter.
(define_query_cpu_unit "ls2_falu1_core,ls2_falu2_core" "ls2_falu")

;; Pseudo units to help modeling of FALU1/2 round-robin dispatch strategy.
(define_cpu_unit "ls2_falu1_turn,ls2_falu2_turn" "ls2_falu")

;; Pseudo units to enable/disable ls2_falu[12]_turn units.
;; ls2_falu[12]_turn unit can be subscribed only after
;; ls2_falu[12]_turn_enabled unit is subscribed.
(define_cpu_unit "ls2_falu1_turn_enabled,ls2_falu2_turn_enabled" "ls2_falu")
(presence_set "ls2_falu1_turn" "ls2_falu1_turn_enabled")
(presence_set "ls2_falu2_turn" "ls2_falu2_turn_enabled")

;; Reservations for FALU1 (FALU2) instructions.
;; Instruction goes to FALU1 (FALU2) and makes next FALU1/2 instruction to
;; be dispatched to FALU2 (FALU1).
(define_reservation "ls2_falu1"
  "(ls2_falu1_core+ls2_falu2_turn_enabled)|ls2_falu1_core")
(define_reservation "ls2_falu2"
  "(ls2_falu2_core+ls2_falu1_turn_enabled)|ls2_falu2_core")

;; Reservation for FALU1/2 instructions.
;; Instruction will go to FALU1 iff ls2_falu1_turn_enabled is subscribed and
;; switch the turn to FALU2 by subscribing ls2_falu2_turn_enabled.
;; Or to FALU2 otherwise.
(define_reservation "ls2_falu"
  "(ls2_falu1+ls2_falu1_turn+ls2_falu2_turn_enabled)
   |(ls2_falu1+ls2_falu1_turn)
   |(ls2_falu2+ls2_falu2_turn+ls2_falu1_turn_enabled)
   |(ls2_falu2+ls2_falu2_turn)")

;; The following 4 instructions each subscribe one of
;; ls2_[f]alu{1,2}_turn_enabled units according to this attribute.
;; These instructions are used in mips.c: sched_ls2_dfa_post_advance_cycle.

(define_attr "ls2_turn_type" "alu1,alu2,falu1,falu2,unknown,atomic,syncloop"
  (const_string "unknown"))

;; Subscribe ls2_alu1_turn_enabled.
(define_insn "ls2_alu1_turn_enabled_insn"
  [(unspec [(const_int 0)] UNSPEC_LOONGSON_ALU1_TURN_ENABLED_INSN)]
  "TUNE_LOONGSON_2EF"
  { gcc_unreachable (); }
  [(set_attr "ls2_turn_type" "alu1")])

(define_insn_reservation "ls2_alu1_turn_enabled" 0
  (eq_attr "ls2_turn_type" "alu1")
  "ls2_alu1_turn_enabled")

;; Subscribe ls2_alu2_turn_enabled.
(define_insn "ls2_alu2_turn_enabled_insn"
  [(unspec [(const_int 0)] UNSPEC_LOONGSON_ALU2_TURN_ENABLED_INSN)]
  "TUNE_LOONGSON_2EF"
  { gcc_unreachable (); }
  [(set_attr "ls2_turn_type" "alu2")])

(define_insn_reservation "ls2_alu2_turn_enabled" 0
  (eq_attr "ls2_turn_type" "alu2")
  "ls2_alu2_turn_enabled")

;; Subscribe ls2_falu1_turn_enabled.
(define_insn "ls2_falu1_turn_enabled_insn"
  [(unspec [(const_int 0)] UNSPEC_LOONGSON_FALU1_TURN_ENABLED_INSN)]
  "TUNE_LOONGSON_2EF"
  { gcc_unreachable (); }
  [(set_attr "ls2_turn_type" "falu1")])

(define_insn_reservation "ls2_falu1_turn_enabled" 0
  (eq_attr "ls2_turn_type" "falu1")
  "ls2_falu1_turn_enabled")

;; Subscribe ls2_falu2_turn_enabled.
(define_insn "ls2_falu2_turn_enabled_insn"
  [(unspec [(const_int 0)] UNSPEC_LOONGSON_FALU2_TURN_ENABLED_INSN)]
  "TUNE_LOONGSON_2EF"
  { gcc_unreachable (); }
  [(set_attr "ls2_turn_type" "falu2")])

(define_insn_reservation "ls2_falu2_turn_enabled" 0
  (eq_attr "ls2_turn_type" "falu2")
  "ls2_falu2_turn_enabled")

;; Automaton for memory operations.
(define_automaton "ls2_mem")

;; Memory unit.
(define_query_cpu_unit "ls2_mem" "ls2_mem")

;; Reservation for integer instructions.
(define_insn_reservation "ls2_alu" 2
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "arith,condmove,const,logical,mfhi,mflo,move,
                        mthi,mtlo,nop,shift,signext,slt"))
  "ls2_alu")

;; Reservation for branch instructions.
(define_insn_reservation "ls2_branch" 2
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "branch,jump,call,trap"))
  "ls2_alu1")

;; Reservation for integer multiplication instructions.
(define_insn_reservation "ls2_imult" 5
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "imul,imul3nc"))
  "ls2_alu2,ls2_alu2_core")

;; Reservation for integer division / remainder instructions.
;; These instructions use the SRT algorithm and hence take 2-38 cycles.
(define_insn_reservation "ls2_idiv" 20
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "idiv,idiv3"))
  "ls2_alu2,ls2_alu2_core*18")

;; Reservation for memory load instructions.
(define_insn_reservation "ls2_load" 5
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "load,fpload,mfc,mtc"))
  "ls2_mem")

(define_insn_reservation "ls2_prefetch" 0
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "prefetch,prefetchx"))
  "ls2_mem")

;; Reservation for memory store instructions.
;; With stores we assume they don't alias with dependent loads.
;; Therefore we set the latency to zero.
(define_insn_reservation "ls2_store" 0
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "store,fpstore"))
  "ls2_mem")

;; Reservation for floating-point instructions of latency 3.
(define_insn_reservation "ls2_fp3" 3
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "fabs,fneg,fcmp,fmove"))
  "ls2_falu1")

;; Reservation for floating-point instructions of latency 5.
(define_insn_reservation "ls2_fp5" 5
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "fcvt"))
  "ls2_falu1")

;; Reservation for floating-point instructions that can go
;; to either of FALU1/2 units.
(define_insn_reservation "ls2_falu" 7
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "fadd,fmul,fmadd"))
  "ls2_falu")

;; Reservation for floating-point division / remainder instructions.
;; These instructions use the SRT algorithm and hence take a variable amount
;; of cycles:
;; div.s takes 5-11 cycles
;; div.d takes 5-18 cycles
(define_insn_reservation "ls2_fdiv" 9
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "fdiv"))
  "ls2_falu2,ls2_falu2_core*7")

;; Reservation for floating-point sqrt instructions.
;; These instructions use the SRT algorithm and hence take a variable amount
;; of cycles:
;; sqrt.s takes 5-17 cycles
;; sqrt.d takes 5-32 cycles
(define_insn_reservation "ls2_fsqrt" 15
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "fsqrt"))
  "ls2_falu2,ls2_falu2_core*13")

;; Two consecutive ALU instructions.
(define_insn_reservation "ls2_multi" 4
  (and (eq_attr "cpu" "loongson_2e,loongson_2f")
       (eq_attr "type" "multi"))
  "(ls2_alu1,ls2_alu2_core)|(ls2_alu2,ls2_alu1_core)")

;; Reservation for everything else.  Normally, this reservation
;; will only be used to handle cases like compiling for non-loongson
;; CPUs with -mtune=loongson2?.
;;
;; This reservation depends upon the fact that DFA will check
;; reservations in the same order as they appear in the file.
(define_insn_reservation "ls2_unknown" 1
  (eq_attr "cpu" "loongson_2e,loongson_2f")
  "ls2_alu1_core+ls2_alu2_core+ls2_falu1_core+ls2_falu2_core+ls2_mem")
