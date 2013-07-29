;; ARM Cortex-R4 scheduling description.
;; Copyright (C) 2007-2013 Free Software Foundation, Inc.
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

(define_automaton "cortex_r4")

;; We approximate the dual-issue constraints of this core using four
;; "issue units" and a reservation matrix as follows.  The numbers indicate
;; the instruction groups' preferences in order.  Multiple entries for
;; the same numbered preference indicate units that must be reserved
;; together.
;;
;; Issue unit:		A	B	C	ALU
;;
;; ALU w/o reg shift	1st	2nd		1st and 2nd
;; ALU w/ reg shift	1st	2nd	2nd	1st and 2nd
;; Moves		1st	2nd		2nd
;; Multiplication	1st			1st
;; Division		1st			1st
;; Load/store single	1st		1st
;; Other load/store	1st	1st
;; Branches			1st

(define_cpu_unit "cortex_r4_issue_a" "cortex_r4")
(define_cpu_unit "cortex_r4_issue_b" "cortex_r4")
(define_cpu_unit "cortex_r4_issue_c" "cortex_r4")
(define_cpu_unit "cortex_r4_issue_alu" "cortex_r4")

(define_reservation "cortex_r4_alu"
                    "(cortex_r4_issue_a+cortex_r4_issue_alu)|\
                     (cortex_r4_issue_b+cortex_r4_issue_alu)")
(define_reservation "cortex_r4_alu_shift_reg"
                    "(cortex_r4_issue_a+cortex_r4_issue_alu)|\
                     (cortex_r4_issue_b+cortex_r4_issue_c+\
                      cortex_r4_issue_alu)")
(define_reservation "cortex_r4_mov"
                    "cortex_r4_issue_a|(cortex_r4_issue_b+\
                     cortex_r4_issue_alu)")
(define_reservation "cortex_r4_mul" "cortex_r4_issue_a+cortex_r4_issue_alu")
(define_reservation "cortex_r4_mul_2"
                    "(cortex_r4_issue_a+cortex_r4_issue_alu)*2")
;; Division instructions execute out-of-order with respect to the
;; rest of the pipeline and only require reservations on their first and
;; final cycles.
(define_reservation "cortex_r4_div_9"
                    "cortex_r4_issue_a+cortex_r4_issue_alu,\
                     nothing*7,\
                     cortex_r4_issue_a+cortex_r4_issue_alu")
(define_reservation "cortex_r4_div_10"
                    "cortex_r4_issue_a+cortex_r4_issue_alu,\
                     nothing*8,\
                     cortex_r4_issue_a+cortex_r4_issue_alu")
(define_reservation "cortex_r4_load_store"
                    "cortex_r4_issue_a+cortex_r4_issue_c")
(define_reservation "cortex_r4_load_store_2"
                    "(cortex_r4_issue_a+cortex_r4_issue_b)*2")
(define_reservation "cortex_r4_branch" "cortex_r4_issue_b")

;; We assume that all instructions are unconditional.

;; Data processing instructions.  Moves without shifts are kept separate
;; for the purposes of the dual-issue constraints above.
(define_insn_reservation "cortex_r4_alu" 2
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "arlo_imm,arlo_reg,shift,shift_reg,mvn_imm,mvn_reg"))
  "cortex_r4_alu")

(define_insn_reservation "cortex_r4_mov" 2
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "mov_imm,mov_reg"))
  "cortex_r4_mov")

(define_insn_reservation "cortex_r4_alu_shift" 2
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "extend,arlo_shift,mov_shift,mvn_shift"))
  "cortex_r4_alu")

(define_insn_reservation "cortex_r4_alu_shift_reg" 2
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "arlo_shift_reg,mov_shift_reg,mvn_shift_reg"))
  "cortex_r4_alu_shift_reg")

;; An ALU instruction followed by an ALU instruction with no early dep.
(define_bypass 1 "cortex_r4_alu,cortex_r4_alu_shift,cortex_r4_alu_shift_reg,\
                  cortex_r4_mov"
               "cortex_r4_alu")
(define_bypass 1 "cortex_r4_alu,cortex_r4_alu_shift,cortex_r4_alu_shift_reg,\
                  cortex_r4_mov"
               "cortex_r4_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 1 "cortex_r4_alu,cortex_r4_alu_shift,cortex_r4_alu_shift_reg,\
                  cortex_r4_mov"
               "cortex_r4_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; In terms of availabilities, a consumer mov could theoretically be
;; issued together with a producer ALU instruction, without stalls.
;; In practice this cannot happen because mov;add (in that order) is not
;; eligible for dual issue and furthermore dual issue is not permitted
;; when a dependency is involved.  We therefore note it as latency one.
;; A mov followed by another of the same is also latency one.
(define_bypass 1 "cortex_r4_alu,cortex_r4_alu_shift,cortex_r4_alu_shift_reg,\
                  cortex_r4_mov"
               "cortex_r4_mov")

;; qadd, qdadd, qsub and qdsub are not currently emitted, and neither are
;; media data processing instructions nor sad instructions.

;; Multiplication instructions.

(define_insn_reservation "cortex_r4_mul_4" 4
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "mul,smmul"))
  "cortex_r4_mul_2")

(define_insn_reservation "cortex_r4_mul_3" 3
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "smulxy,smulwy,smuad,smusd"))
  "cortex_r4_mul")

(define_insn_reservation "cortex_r4_mla_4" 4
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "mla,smmla"))
  "cortex_r4_mul_2")

(define_insn_reservation "cortex_r4_mla_3" 3
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "smlaxy,smlawy,smlad,smlsd"))
  "cortex_r4_mul")

(define_insn_reservation "cortex_r4_smlald" 3
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "smlald,smlsld"))
  "cortex_r4_mul")

(define_insn_reservation "cortex_r4_mull" 4
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "smull,umull,umlal,umaal"))
  "cortex_r4_mul_2")

;; A multiply or an MLA with a single-register result, followed by an
;; MLA with an accumulator dependency, has its result forwarded.
(define_bypass 2 "cortex_r4_mul_3,cortex_r4_mla_3"
               "cortex_r4_mla_3,cortex_r4_mla_4"
               "arm_mac_accumulator_is_mul_result")

(define_bypass 3 "cortex_r4_mul_4,cortex_r4_mla_4"
               "cortex_r4_mla_3,cortex_r4_mla_4"
               "arm_mac_accumulator_is_mul_result")

;; A multiply followed by an ALU instruction needing the multiply
;; result only at ALU has lower latency than one needing it at Shift.
(define_bypass 2 "cortex_r4_mul_3,cortex_r4_mla_3,cortex_r4_smlald"
               "cortex_r4_alu")
(define_bypass 2 "cortex_r4_mul_3,cortex_r4_mla_3,cortex_r4_smlald"
               "cortex_r4_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 2 "cortex_r4_mul_3,cortex_r4_mla_3,cortex_r4_smlald"
               "cortex_r4_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")
(define_bypass 3 "cortex_r4_mul_4,cortex_r4_mla_4,cortex_r4_mull"
               "cortex_r4_alu")
(define_bypass 3 "cortex_r4_mul_4,cortex_r4_mla_4,cortex_r4_mull"
               "cortex_r4_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 3 "cortex_r4_mul_4,cortex_r4_mla_4,cortex_r4_mull"
               "cortex_r4_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; A multiply followed by a mov has one cycle lower latency again.
(define_bypass 1 "cortex_r4_mul_3,cortex_r4_mla_3,cortex_r4_smlald"
               "cortex_r4_mov")
(define_bypass 2 "cortex_r4_mul_4,cortex_r4_mla_4,cortex_r4_mull"
               "cortex_r4_mov")

;; We guess that division of A/B using sdiv or udiv, on average, 
;; is performed with B having ten more leading zeros than A.
;; This gives a latency of nine for udiv and ten for sdiv.
(define_insn_reservation "cortex_r4_udiv" 9
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "udiv"))
  "cortex_r4_div_9")

(define_insn_reservation "cortex_r4_sdiv" 10
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "sdiv"))
  "cortex_r4_div_10")

;; Branches.  We assume correct prediction.

(define_insn_reservation "cortex_r4_branch" 0
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "branch"))
  "cortex_r4_branch")

;; Call latencies are not predictable.  A semi-arbitrary very large
;; number is used as "positive infinity" so that everything should be
;; finished by the time of return.
(define_insn_reservation "cortex_r4_call" 32
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "call"))
  "nothing")

;; Status register access instructions are not currently emitted.

;; Load instructions.
;; We do not model the "addr_md_3cycle" cases and assume that
;; accesses following are correctly aligned.

(define_insn_reservation "cortex_r4_load_1_2" 3
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "load1,load2"))
  "cortex_r4_load_store")

(define_insn_reservation "cortex_r4_load_3_4" 4
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "load3,load4"))
  "cortex_r4_load_store_2")

;; If a producing load is followed by an instruction consuming only
;; as a Normal Reg, there is one fewer cycle of latency.

(define_bypass 2 "cortex_r4_load_1_2"
               "cortex_r4_alu")
(define_bypass 2 "cortex_r4_load_1_2"
               "cortex_r4_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 2 "cortex_r4_load_1_2"
               "cortex_r4_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

(define_bypass 3 "cortex_r4_load_3_4"
               "cortex_r4_alu")
(define_bypass 3 "cortex_r4_load_3_4"
               "cortex_r4_alu_shift"
               "arm_no_early_alu_shift_dep")
(define_bypass 3 "cortex_r4_load_3_4"
               "cortex_r4_alu_shift_reg"
               "arm_no_early_alu_shift_value_dep")

;; If a producing load is followed by an instruction consuming only
;; as a Late Reg, there are two fewer cycles of latency.  Such consumer
;; instructions are moves and stores.

(define_bypass 1 "cortex_r4_load_1_2"
               "cortex_r4_mov,cortex_r4_store_1_2,cortex_r4_store_3_4")
(define_bypass 2 "cortex_r4_load_3_4"
               "cortex_r4_mov,cortex_r4_store_1_2,cortex_r4_store_3_4")

;; If a producer's result is required as the base or offset of a load,
;; there is an extra cycle latency.

(define_bypass 3 "cortex_r4_alu,cortex_r4_mov,cortex_r4_alu_shift,\
                  cortex_r4_alu_shift_reg"
               "cortex_r4_load_1_2,cortex_r4_load_3_4")

(define_bypass 4 "cortex_r4_mul_3,cortex_r4_mla_3,cortex_r4_smlald"
               "cortex_r4_load_1_2,cortex_r4_load_3_4")

(define_bypass 5 "cortex_r4_mul_4,cortex_r4_mla_4,cortex_r4_mull"
               "cortex_r4_load_1_2,cortex_r4_load_3_4")

;; Store instructions.

(define_insn_reservation "cortex_r4_store_1_2" 0
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "store1,store2"))
  "cortex_r4_load_store")

(define_insn_reservation "cortex_r4_store_3_4" 0
  (and (eq_attr "tune_cortexr4" "yes")
       (eq_attr "type" "store3,store4"))
  "cortex_r4_load_store_2")

