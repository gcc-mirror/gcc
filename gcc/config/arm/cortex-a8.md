;; ARM Cortex-A8 scheduling description.
;; Copyright (C) 2007 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.

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

(define_automaton "cortex_a8")

;; Only one load/store instruction can be issued per cycle
;; (although reservation of this unit is only required for single
;; loads and stores -- see below).
(define_cpu_unit "cortex_a8_issue_ls" "cortex_a8")

;; Only one branch instruction can be issued per cycle.
(define_cpu_unit "cortex_a8_issue_branch" "cortex_a8")

;; The two ALU pipelines.
(define_cpu_unit "cortex_a8_alu0" "cortex_a8")
(define_cpu_unit "cortex_a8_alu1" "cortex_a8")

;; The usual flow of an instruction through the pipelines.
(define_reservation "cortex_a8_default"
                    "cortex_a8_alu0|cortex_a8_alu1")

;; The flow of a branch instruction through the pipelines.
(define_reservation "cortex_a8_branch"
                    "(cortex_a8_alu0+cortex_a8_issue_branch)|\
                     (cortex_a8_alu1+cortex_a8_issue_branch)")

;; The flow of a load or store instruction through the pipeline in
;; the case where that instruction consists of only one micro-op...
(define_reservation "cortex_a8_load_store_1"
                    "(cortex_a8_alu0+cortex_a8_issue_ls)|\
                     (cortex_a8_alu1+cortex_a8_issue_ls)")

;; ...and in the case of two micro-ops.  Dual issue is altogether forbidden
;; during the issue cycle of the first micro-op.  (Instead of modelling
;; a separate issue unit, we instead reserve alu0 and alu1 to
;; prevent any other instructions from being issued upon that first cycle.)
;; Even though the load/store pipeline is usually available in either
;; ALU pipe, multi-cycle instructions always issue in pipeline 0.
(define_reservation "cortex_a8_load_store_2"
                    "cortex_a8_alu0+cortex_a8_alu1+cortex_a8_issue_ls,\
                     cortex_a8_alu0+cortex_a8_issue_ls")

;; The flow of a single-cycle multiplication.
(define_reservation "cortex_a8_multiply"
                    "cortex_a8_alu0")

;; The flow of a multiplication instruction that gets decomposed into
;; two micro-ops.  The two micro-ops will be issued to pipeline 0 on
;; successive cycles.  Dual issue cannot happen at the same time as the
;; first of the micro-ops.
(define_reservation "cortex_a8_multiply_2"
                    "cortex_a8_alu0+cortex_a8_alu1,\
                     cortex_a8_alu0")

;; Similarly, the flow of a multiplication instruction that gets
;; decomposed into three micro-ops.  Dual issue cannot occur except on
;; the cycle upon which the third micro-op is issued.
(define_reservation "cortex_a8_multiply_3"
                    "cortex_a8_alu0+cortex_a8_alu1,\
                     cortex_a8_alu0+cortex_a8_alu1,\
                     cortex_a8_alu0")

;; The model given here assumes that all instructions are unconditional.

;; Data processing instructions, but not move instructions.

;; We include CLZ with these since it has the same execution pattern
;; (source read in E2 and destination available at the end of that cycle).
(define_insn_reservation "cortex_a8_alu" 2
  (and (eq_attr "tune" "cortexa8")
       (ior (and (and (eq_attr "type" "alu")
		      (eq_attr "neon_type" "none"))
		 (not (eq_attr "insn" "mov,mvn")))
            (eq_attr "insn" "clz")))
  "cortex_a8_default")

(define_insn_reservation "cortex_a8_alu_shift" 2
  (and (eq_attr "tune" "cortexa8")
       (and (eq_attr "type" "alu_shift")
            (not (eq_attr "insn" "mov,mvn"))))
  "cortex_a8_default")

(define_insn_reservation "cortex_a8_alu_shift_reg" 2
  (and (eq_attr "tune" "cortexa8")
       (and (eq_attr "type" "alu_shift_reg")
            (not (eq_attr "insn" "mov,mvn"))))
  "cortex_a8_default")

;; Move instructions.

(define_insn_reservation "cortex_a8_mov" 1
  (and (eq_attr "tune" "cortexa8")
       (and (eq_attr "type" "alu,alu_shift,alu_shift_reg")
            (eq_attr "insn" "mov,mvn")))
  "cortex_a8_default")

;; Exceptions to the default latencies for data processing instructions.

;; A move followed by an ALU instruction with no early dep.
;; (Such a pair can be issued in parallel, hence latency zero.)
(define_bypass 0 "cortex_a8_mov" "cortex_a8_alu")
(define_bypass 0 "cortex_a8_mov" "cortex_a8_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 0 "cortex_a8_mov" "cortex_a8_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; An ALU instruction followed by an ALU instruction with no early dep.
(define_bypass 1 "cortex_a8_alu,cortex_a8_alu_shift,cortex_a8_alu_shift_reg"
               "cortex_a8_alu")
(define_bypass 1 "cortex_a8_alu,cortex_a8_alu_shift,cortex_a8_alu_shift_reg"
               "cortex_a8_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 1 "cortex_a8_alu,cortex_a8_alu_shift,cortex_a8_alu_shift_reg"
               "cortex_a8_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; Multiplication instructions.  These are categorized according to their
;; reservation behavior and the need below to distinguish certain
;; varieties for bypasses.  Results are available at the E5 stage
;; (but some of these are multi-cycle instructions which explains the
;; latencies below).

(define_insn_reservation "cortex_a8_mul" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "insn" "mul,smulxy,smmul"))
  "cortex_a8_multiply_2")

(define_insn_reservation "cortex_a8_mla" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "insn" "mla,smlaxy,smlawy,smmla,smlad,smlsd"))
  "cortex_a8_multiply_2")

(define_insn_reservation "cortex_a8_mull" 7
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "insn" "smull,umull,smlal,umlal,umaal,smlalxy"))
  "cortex_a8_multiply_3")

(define_insn_reservation "cortex_a8_smulwy" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "insn" "smulwy,smuad,smusd"))
  "cortex_a8_multiply")

;; smlald and smlsld are multiply-accumulate instructions but do not
;; received bypassed data from other multiplication results; thus, they
;; cannot go in cortex_a8_mla above.  (See below for bypass details.)
(define_insn_reservation "cortex_a8_smlald" 6
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "insn" "smlald,smlsld"))
  "cortex_a8_multiply_2")

;; A multiply with a single-register result or an MLA, followed by an
;; MLA with an accumulator dependency, has its result forwarded so two
;; such instructions can issue back-to-back.
(define_bypass 1 "cortex_a8_mul,cortex_a8_mla,cortex_a8_smulwy"
               "cortex_a8_mla"
               "arm_mac_accumulator_is_mul_result")

;; A multiply followed by an ALU instruction needing the multiply
;; result only at E2 has lower latency than one needing it at E1.
(define_bypass 4 "cortex_a8_mul,cortex_a8_mla,cortex_a8_mull,\
                  cortex_a8_smulwy,cortex_a8_smlald"
               "cortex_a8_alu")
(define_bypass 4 "cortex_a8_mul,cortex_a8_mla,cortex_a8_mull,\
                  cortex_a8_smulwy,cortex_a8_smlald"
               "cortex_a8_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 4 "cortex_a8_mul,cortex_a8_mla,cortex_a8_mull,\
                  cortex_a8_smulwy,cortex_a8_smlald"
               "cortex_a8_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; Load instructions.
;; The presence of any register writeback is ignored here.

;; A load result has latency 3 unless the dependent instruction has
;; no early dep, in which case it is only latency two.
;; We assume 64-bit alignment for doubleword loads.
(define_insn_reservation "cortex_a8_load1_2" 3
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "load1,load2,load_byte"))
  "cortex_a8_load_store_1")

(define_bypass 2 "cortex_a8_load1_2"
               "cortex_a8_alu")
(define_bypass 2 "cortex_a8_load1_2"
               "cortex_a8_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 2 "cortex_a8_load1_2"
               "cortex_a8_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; We do not currently model the fact that loads with scaled register
;; offsets that are not LSL #2 have an extra cycle latency (they issue
;; as two micro-ops).

;; A load multiple of three registers is usually issued as two micro-ops.
;; The first register will be available at E3 of the first iteration,
;; the second at E3 of the second iteration, and the third at E4 of
;; the second iteration.  A load multiple of four registers is usually
;; issued as two micro-ops.
(define_insn_reservation "cortex_a8_load3_4" 5
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "load3,load4"))
  "cortex_a8_load_store_2")

(define_bypass 4 "cortex_a8_load3_4"
               "cortex_a8_alu")
(define_bypass 4 "cortex_a8_load3_4"
               "cortex_a8_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 4 "cortex_a8_load3_4"
               "cortex_a8_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; Store instructions.
;; Writeback is again ignored.

(define_insn_reservation "cortex_a8_store1_2" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "store1,store2"))
  "cortex_a8_load_store_1")

(define_insn_reservation "cortex_a8_store3_4" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "store3,store4"))
  "cortex_a8_load_store_2")

;; An ALU instruction acting as a producer for a store instruction
;; that only uses the result as the value to be stored (as opposed to
;; using it to calculate the address) has latency zero; the store
;; reads the value to be stored at the start of E3 and the ALU insn
;; writes it at the end of E2.  Move instructions actually produce the
;; result at the end of E1, but since we don't have delay slots, the
;; scheduling behavior will be the same.
(define_bypass 0 "cortex_a8_alu,cortex_a8_alu_shift,\
                  cortex_a8_alu_shift_reg,cortex_a8_mov"
               "cortex_a8_store1_2,cortex_a8_store3_4"
               "arm_no_early_store_addr_dep")

;; Branch instructions

(define_insn_reservation "cortex_a8_branch" 0
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "branch"))
  "cortex_a8_branch")

;; Call latencies are not predictable.  A semi-arbitrary very large
;; number is used as "positive infinity" so that everything should be
;; finished by the time of return.
(define_insn_reservation "cortex_a8_call" 32
  (and (eq_attr "tune" "cortexa8")
       (eq_attr "type" "call"))
  "cortex_a8_issue_branch")

;; NEON (including VFP) instructions.

(include "cortex-a8-neon.md")

