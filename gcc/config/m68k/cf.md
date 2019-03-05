;; ColdFire V1, V2, V3 and V4/V4e DFA description.
;; Copyright (C) 2007-2019 Free Software Foundation, Inc.
;; Contributed by CodeSourcery Inc., www.codesourcery.com
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
;; along with this program; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Instruction Buffer
(define_automaton "cfv123_ib")

;; These pseudo units are used to model instruction buffer of ColdFire cores.
;; Instruction of size N can be issued only when cf_ib_wN is available.
(define_cpu_unit "cf_ib_w1, cf_ib_w2, cf_ib_w3" "cfv123_ib")

;; Instruction occupies 1 word in the instruction buffer.
(define_reservation "cf_ib1" "cf_ib_w1")
;; Instruction occupies 2 words in the instruction buffer.
(define_reservation "cf_ib2" "cf_ib_w1+cf_ib_w2")
;; Instruction occupies 3 words in the instruction buffer.
(define_reservation "cf_ib3" "cf_ib_w1+cf_ib_w2+cf_ib_w3")

;; This reservation is used at the start of each cycle to setup the maximal
;; length of instruction that can be issued on current cycle.
;; E.g., when this reservation is applied for the first time, cf_ib_w3
;; resource is marked busy, thus filtering out all 3-word insns.
;;
;; This reservation requires deterministic automaton.
;;
;; At each cycle, given that memory bus is available (i.e., there is no
;; pending memory operation), instruction fetch pipeline (IFP) prefetches
;; two instruction words into instruction buffer (IB).
(define_insn_reservation "cf_ib1" 0
  (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
       (eq_attr "type" "ib"))
  "cf_ib_w3|cf_ib_w2|cf_ib_w1")

;; Operand Execution Pipeline
(define_automaton "cfv123_oep")

(define_cpu_unit "cf_dsoc,cf_agex" "cfv123_oep")

;; A memory unit that is referred to as 'certain hardware resources' in
;; ColdFire reference manuals.  This unit remains occupied for two cycles
;; after last dsoc cycle of a store - hence there is a 2 cycle delay between
;; two consecutive stores.
(define_automaton "cfv123_chr")

(define_cpu_unit "cf_chr" "cfv123_chr")

;; Memory bus
(define_automaton "cfv123_mem")

;; When memory bus is subscribed, that implies that instruction buffer won't
;; get its portion this cycle.  To model that we query if cf_mem unit is
;; subscribed and adjust number of prefetched instruction words accordingly.
;; 
(define_query_cpu_unit "cf_mem1, cf_mem2" "cfv123_mem")

(define_reservation "cf_mem" "cf_mem1+cf_mem2")

(define_automaton "cf_mac")

(define_cpu_unit "cf_mac1,cf_mac2,cf_mac3,cf_mac4"
  "cf_mac")

(define_automaton "cfv123_guess")

(define_query_cpu_unit "cfv123_guess" "cfv123_guess")

;; Register to register move.
;; Takes 1 cycle.
(define_reservation "cfv123_alu_00"
  "cf_dsoc,cf_agex")

;; Load from a memory location.
;; Takes 3 cycles.
(define_reservation "cfv12_alu_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex")
;; Takes 2 cycles.
(define_reservation "cfv12_omove_10"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem,cf_agex")
;; Takes 4 cycles.
(define_reservation "cfv3_alu_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex")
;; Takes 3 cycles.
(define_reservation "cfv3_omove_10"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex")

;; Load from an indexed location.
;; Takes 4 cycles.
(define_reservation "cfv12_alu_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex")
;; Takes 3 cycles.
(define_reservation "cfv12_omove_i0"
  "cf_dsoc+cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex")
;; Takes 5 cycles.
(define_reservation "cfv3_alu_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex")
;; Takes 4 cycles.
(define_reservation "cfv3_omove_i0"
  "cf_dsoc+cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex")

;; Store to a memory location.
;; Takes 1 cycle.
(define_reservation "cfv12_alu_01"
  "cf_dsoc+cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 1 cycle.
(define_reservation "cfv3_alu_01"
  "cf_dsoc+cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Store to an indexed location.
;; Takes 2 cycles.
(define_reservation "cfv12_alu_0i"
  "cf_dsoc+cf_agex,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 2 cycles.
(define_reservation "cfv3_alu_0i"
  "cf_dsoc+cf_agex,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Load from a memory location and store to a memory location.
;; Takes 3 cycles
(define_reservation "cfv12_alu_11"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 2 cycles.
(define_reservation "cfv12_omove_11"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 4 cycles
(define_reservation "cfv3_alu_11"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")
;; Takes 3 cycles.
(define_reservation "cfv3_omove_11"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Load from an indexed location and store to a memory location.
;; Takes 4 cycles.
(define_reservation "cfv12_alu_i1"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 3 cycles.
(define_reservation "cfv12_omove_i1"
  "cf_dsoc+cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 5 cycles.
(define_reservation "cfv3_alu_i1"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")
;; Takes 4 cycles.
(define_reservation "cfv3_omove_i1"
  "cf_dsoc+cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Load from a memory location and store to an indexed location.
;; Takes 4 cycles.
(define_reservation "cfv12_alu_1i"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 3 cycles.
(define_reservation "cfv12_omove_1i"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem,cf_agex,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 5 cycles.
(define_reservation "cfv3_alu_1i"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")
;; Takes 4 cycles.
(define_reservation "cfv3_omove_1i"
  "cf_dsoc+cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Lea operation for a memory location.
;; Takes 1 cycle.
(define_reservation "cfv123_lea_10"
  "cf_dsoc,cf_agex")

;; Lea operation for an indexed location.
;; Takes 2 cycles.
(define_reservation "cfv123_lea_i0"
  "cf_dsoc,cf_agex,cf_agex")

;; Pea operation for a memory location.
;; Takes 2 cycles.
(define_reservation "cfv12_pea_11"
  "cf_dsoc,cf_agex,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 2 cycles.
(define_reservation "cfv3_pea_11"
  "cf_dsoc,cf_agex,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Pea operation for an indexed location.
;; Takes 3 cycles.
(define_reservation "cfv12_pea_i1"
  "cf_dsoc,cf_agex,cf_agex,cf_agex+cf_chr,cf_mem+cf_chr,cf_chr")
;; Takes 3 cycles.
(define_reservation "cfv3_pea_i1"
  "cf_dsoc,cf_agex,cf_agex,cf_agex+cf_chr,cf_mem1+cf_chr,cf_mem2+cf_chr")

;; Long multiplication with no mac.
;; Takes 9-18 cycles.
(define_reservation "cfv123_mul_l_00"
  "cf_dsoc,(cf_agex+cf_dsoc)*17,cf_agex")

;; Word multiplication with no mac.
;; Takes 9 cycles.
(define_reservation "cfv123_mul_w_00"
  "cf_dsoc,(cf_agex+cf_dsoc)*8,cf_agex")

;; Long multiplication with no mac.
;; Takes 11-20 cycles.
(define_reservation "cfv12_mul_l_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,(cf_agex+cf_dsoc)*17,cf_agex")
;; Takes 12-21 cycles.
(define_reservation "cfv3_mul_l_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,(cf_agex+cf_dsoc)*17,cf_agex")

;; Word multiplication with no mac.
;; Takes 11 cycles.
(define_reservation "cfv12_mul_w_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,(cf_agex+cf_dsoc)*8,cf_agex")
;; Takes 12 cycles.
(define_reservation "cfv3_mul_w_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,(cf_agex+cf_dsoc)*8,cf_agex")

;; Word multiplication with no mac.
;; Takes 12 cycles.
(define_reservation "cfv12_mul_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem,(cf_agex+cf_dsoc)*8,cf_agex")
;; Takes 13 cycles.
(define_reservation "cfv3_mul_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,(cf_agex+cf_dsoc)*8,cf_agex")

;; Long multiplication with mac.
;; Takes 5 cycles.
(define_reservation "cfv123_mac_l_00"
  "cf_dsoc,cf_agex,cf_mac1,cf_mac2,cf_mac3,cf_mac4")

;; Word multiplication with mac.
;; Takes 3 cycles.
(define_reservation "cfv123_mac_w_00"
  "cf_dsoc,cf_agex,cf_mac1,cf_mac2")

;; Long multiplication with mac.
;; Takes 7 cycles.
(define_reservation "cfv12_mac_l_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex,cf_mac1,cf_mac2,cf_mac3,cf_mac4")
;; Takes 8 cycles.
(define_reservation "cfv3_mac_l_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex,cf_mac1,cf_mac2,cf_mac3,cf_mac4")

;; Word multiplication with mac.
;; Takes 5 cycles.
(define_reservation "cfv12_mac_w_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex,cf_mac1,cf_mac2")
;; Takes 6 cycles.
(define_reservation "cfv3_mac_w_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex,cf_mac1,cf_mac2")

;; Word multiplication with mac.
;; Takes 6 cycles.
(define_reservation "cfv12_mac_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex,cf_mac1,cf_mac2")
;; Takes 7 cycles.
(define_reservation "cfv3_mac_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex,cf_mac1,cf_mac2")

;; Multiplication with emac.
;; Takes 4 cycles.
(define_reservation "cfv123_emac_00"
  "cf_dsoc,cf_agex+cf_mac1,cf_mac2,cf_mac3,cf_mac4")

;; Multiplication with emac.
;; Takes 6 cycles.
(define_reservation "cfv12_emac_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem,cf_agex+cf_mac1,cf_mac2,cf_mac3,cf_mac4")
;; Takes 7 cycles.
(define_reservation "cfv3_emac_10"
  "cf_dsoc,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_mac1,cf_mac2,cf_mac3,cf_mac4")

;; Word multiplication with emac.
;; Takes 7 cycles.
(define_reservation "cfv12_emac_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem,cf_agex+cf_mac1,cf_mac2,cf_mac3,cf_mac4")
;; Takes 8 cycles.
(define_reservation "cfv3_emac_w_i0"
  "cf_dsoc,cf_agex,cf_agex,cf_dsoc+cf_mem1,cf_dsoc+cf_mem2,cf_agex+cf_mac1,cf_mac2,cf_mac3,cf_mac4")

;; Return instruction.
;; ??? As return reads target address from stack, use a mem-read reservation
;; ??? for it.
;; ??? It's not clear what the core does during these 5 cycles.
;; ??? Luckily, we don't care that much about an insn that won't be moved.
;; Takes 5 cycles.
(define_reservation "cfv12_rts" "cfv12_alu_10")
;; Takes 8 cycles.
(define_reservation "cfv3_rts" "cfv3_alu_10")

;; Call instruction.
;; ??? It's not clear what reservation is best to use for calls.
;; ??? For now we use mem-write + return reservations to reflect the fact of
;; ??? pushing and poping return address to and from the stack.
;; Takes 3 cycles.
(define_reservation "cfv12_call" "cfv12_alu_01,cfv12_rts")
;; Takes 1/5 cycles.
(define_reservation "cfv3_call" "cfv3_alu_01,cfv3_rts")

;; Conditional branch instruction.
;; ??? Branch reservations are unclear to me so far.  Luckily, we don't care
;; ??? that much about branches.
;; Takes 2 cycles.
(define_reservation "cfv12_bcc" "cfv123_alu_00")
;; Takes 1 cycles.
(define_reservation "cfv3_bcc" "cfv123_alu_00")

;; Unconditional branch instruciton.
;; Takes 2 cycles.
(define_reservation "cfv12_bra" "cfv12_alu_01")
;; Takes 1 cycles.
(define_reservation "cfv3_bra" "cfv3_alu_01")

;; Computed jump instruction.
;; Takes 3 cycles.
(define_reservation "cfv12_jmp"
  "(cf_dsoc+cf_agex)*3")
;; Takes 5 cycles.
(define_reservation "cfv3_jmp"
  "(cf_dsoc+cf_agex)*5")

;; Instruction reservations.

;; Below reservations are simple derivation from the above reservations.
;; Each reservation from the above expands into 3 reservations below - one
;; for each instruction size.
;; A number in the end of reservation's name is the size of the instruction.

(define_insn_reservation "cfv123_alu_00_1" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "00"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_alu_00")

(define_insn_reservation "cfv123_alu_00_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "00"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_alu_00")

(define_insn_reservation "cfv123_alu_00_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "00"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_alu_00")

(define_insn_reservation "cfv1_alu_10_1" 3
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_alu_10")

(define_insn_reservation "cfv1_alu_10_2" 3
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_alu_10")

(define_insn_reservation "cfv1_alu_10_3" 3
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_10")

(define_insn_reservation "cfv1_omove_10_1" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_omove_10")

(define_insn_reservation "cfv1_omove_10_2" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_omove_10")

(define_insn_reservation "cfv1_omove_10_3" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_10")

(define_insn_reservation "cfv2_alu_10_1" 3
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_alu_10")

(define_insn_reservation "cfv2_alu_10_2" 3
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_alu_10")

(define_insn_reservation "cfv2_alu_10_3" 3
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_10")

(define_insn_reservation "cfv2_omove_10_1" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_omove_10")

(define_insn_reservation "cfv2_omove_10_2" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_omove_10")

(define_insn_reservation "cfv2_omove_10_3" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_10")

(define_insn_reservation "cfv3_alu_10_1" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_alu_10")

(define_insn_reservation "cfv3_alu_10_2" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_alu_10")

(define_insn_reservation "cfv3_alu_10_3" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_10")

(define_insn_reservation "cfv3_omove_10_1" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_omove_10")

(define_insn_reservation "cfv3_omove_10_2" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_omove_10")

(define_insn_reservation "cfv3_omove_10_3" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "10"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_omove_10")

(define_insn_reservation "cfv1_alu_i0_2" 4
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_i0")

(define_insn_reservation "cfv1_alu_i0_3" 4
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_i0")

(define_insn_reservation "cfv1_omove_i0_2" 3
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_i0")

(define_insn_reservation "cfv1_omove_i0_3" 3
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_i0")

(define_insn_reservation "cfv2_alu_i0_2" 4
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_i0")

(define_insn_reservation "cfv2_alu_i0_3" 4
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_i0")

(define_insn_reservation "cfv2_omove_i0_2" 3
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_i0")

(define_insn_reservation "cfv2_omove_i0_3" 3
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_i0")

(define_insn_reservation "cfv3_alu_i0_2" 5
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_alu_i0")

(define_insn_reservation "cfv3_alu_i0_3" 5
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_i0")

(define_insn_reservation "cfv3_omove_i0_2" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_omove_i0")

(define_insn_reservation "cfv3_omove_i0_3" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i0"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_omove_i0")

(define_insn_reservation "cfv12_alu_01_1" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_alu_01")

(define_insn_reservation "cfv12_alu_01_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_alu_01")

(define_insn_reservation "cfv12_alu_01_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_01")

(define_insn_reservation "cfv3_alu_01_1" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_alu_01")

(define_insn_reservation "cfv3_alu_01_2" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_alu_01")

(define_insn_reservation "cfv3_alu_01_3" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "01"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_01")

(define_insn_reservation "cfv12_alu_0i_2" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "0i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_0i")

(define_insn_reservation "cfv12_alu_0i_3" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_0i")

(define_insn_reservation "cfv3_alu_0i_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "0i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_alu_0i")

(define_insn_reservation "cfv3_alu_0i_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_0i")

(define_insn_reservation "cfv1_alu_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_alu_11")

(define_insn_reservation "cfv1_alu_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_alu_11")

(define_insn_reservation "cfv1_alu_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_11")

(define_insn_reservation "cfv1_omove_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_omove_11")

(define_insn_reservation "cfv1_omove_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_omove_11")

(define_insn_reservation "cfv1_omove_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_11")

(define_insn_reservation "cfv2_alu_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_alu_11")

(define_insn_reservation "cfv2_alu_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_alu_11")

(define_insn_reservation "cfv2_alu_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_11")

(define_insn_reservation "cfv2_omove_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_omove_11")

(define_insn_reservation "cfv2_omove_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_omove_11")

(define_insn_reservation "cfv2_omove_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_11")

(define_insn_reservation "cfv3_alu_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_alu_11")

(define_insn_reservation "cfv3_alu_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "size" "2"))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv3_alu_11")

(define_insn_reservation "cfv3_alu_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_11")

(define_insn_reservation "cfv3_omove_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_omove_11")

(define_insn_reservation "cfv3_omove_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "size" "2"))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv3_omove_11")

(define_insn_reservation "cfv3_omove_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_omove_11")

(define_insn_reservation "cfv1_alu_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_i1")

(define_insn_reservation "cfv1_alu_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_i1")

(define_insn_reservation "cfv1_omove_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_i1")

(define_insn_reservation "cfv1_omove_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_i1")

(define_insn_reservation "cfv2_alu_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_i1")

(define_insn_reservation "cfv2_alu_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_i1")

(define_insn_reservation "cfv2_omove_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_i1")

(define_insn_reservation "cfv2_omove_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_i1")

(define_insn_reservation "cfv3_alu_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_alu_i1")

(define_insn_reservation "cfv3_alu_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_i1")

(define_insn_reservation "cfv3_omove_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_omove_i1")

(define_insn_reservation "cfv3_omove_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_omove_i1")

(define_insn_reservation "cfv1_alu_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_1i")

(define_insn_reservation "cfv1_alu_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_1i")

(define_insn_reservation "cfv1_omove_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_1i")

(define_insn_reservation "cfv1_omove_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv1")
		 (eq_attr "type" "
clr,clr_l,mov3q_l,move,moveq_l,tst,
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_1i")

(define_insn_reservation "cfv2_alu_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_alu_1i")

(define_insn_reservation "cfv2_alu_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_alu_1i")

(define_insn_reservation "cfv2_omove_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_omove_1i")

(define_insn_reservation "cfv2_omove_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv2")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_omove_1i")

(define_insn_reservation "cfv3_alu_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_alu_1i")

(define_insn_reservation "cfv3_alu_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
alu_l,aluq_l,bitr,bitrw,cmp,cmp_l,alux_l,ext,neg_l,scc,shift,
clr,clr_l,mov3q_l,move,moveq_l,tst"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_alu_1i")

(define_insn_reservation "cfv3_omove_1i_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_omove_1i")

(define_insn_reservation "cfv3_omove_1i_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "
move_l,tst_l"))
	    (eq_attr "op_mem" "1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_omove_1i")

(define_insn_reservation "cfv123_lea_10_1" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "lea"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_10_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "lea"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_10_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "lea"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_i0_2" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "lea"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv123_lea_i0")

(define_insn_reservation "cfv123_lea_i0_3" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type" "lea"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_lea_i0")

(define_insn_reservation "cfv12_pea_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_pea_11")

(define_insn_reservation "cfv12_pea_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_pea_11")

(define_insn_reservation "cfv12_pea_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_pea_11")

(define_insn_reservation "cfv3_pea_11_1" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_pea_11")

(define_insn_reservation "cfv3_pea_11_2" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_pea_11")

(define_insn_reservation "cfv3_pea_11_3" 1
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "11"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_pea_11")

(define_insn_reservation "cfv12_pea_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_pea_i1")

(define_insn_reservation "cfv12_pea_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_pea_i1")

(define_insn_reservation "cfv3_pea_i1_2" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_pea_i1")

(define_insn_reservation "cfv3_pea_i1_3" 2
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type" "pea"))
	    (eq_attr "op_mem" "i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_pea_i1")

(define_insn_reservation "cfv123_mul_l_00_1" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_l_00_2" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_l_00_3" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_w_00_1" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_mul_w_00")

(define_insn_reservation "cfv123_mul_w_00_2" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_mul_w_00")

(define_insn_reservation "cfv123_mul_w_00_3" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_mul_w_00")

(define_insn_reservation "cfv12_mul_l_10_1" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_mul_l_10")

(define_insn_reservation "cfv12_mul_l_10_2" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_mul_l_10")

(define_insn_reservation "cfv12_mul_l_10_3" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_1" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_2" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_3" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mul_l_10")

(define_insn_reservation "cfv12_mul_w_10_1" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_mul_w_10")

(define_insn_reservation "cfv12_mul_w_10_2" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_mul_w_10")

(define_insn_reservation "cfv12_mul_w_10_3" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_1" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_2" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_3" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mul_w_10")

(define_insn_reservation "cfv12_mul_w_i0_2" 12
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_mul_w_i0")

(define_insn_reservation "cfv12_mul_w_i0_3" 12
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mul_w_i0")

(define_insn_reservation "cfv3_mul_w_i0_2" 13
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_mul_w_i0")

(define_insn_reservation "cfv3_mul_w_i0_3" 13
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mul_w_i0")

(define_insn_reservation "cfv123_mac_l_00_1" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_l_00_2" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_l_00_3" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_w_00_1" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_mac_w_00")

(define_insn_reservation "cfv123_mac_w_00_2" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_mac_w_00")

(define_insn_reservation "cfv123_mac_w_00_3" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_mac_w_00")

(define_insn_reservation "cfv12_mac_l_10_1" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_mac_l_10")

(define_insn_reservation "cfv12_mac_l_10_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_mac_l_10")

(define_insn_reservation "cfv12_mac_l_10_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_1" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_2" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_3" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mac_l_10")

(define_insn_reservation "cfv12_mac_w_10_1" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_mac_w_10")

(define_insn_reservation "cfv12_mac_w_10_2" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_mac_w_10")

(define_insn_reservation "cfv12_mac_w_10_3" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_1" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mac_w_10")

(define_insn_reservation "cfv12_mac_w_i0_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_mac_w_i0")

(define_insn_reservation "cfv12_mac_w_i0_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_mac_w_i0")

(define_insn_reservation "cfv3_mac_w_i0_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_mac_w_i0")

(define_insn_reservation "cfv3_mac_w_i0_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_mac_w_i0")

(define_insn_reservation "cfv123_emac_00_1" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l,mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv123_emac_00")

(define_insn_reservation "cfv123_emac_00_2" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l,mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv123_emac_00")

(define_insn_reservation "cfv123_emac_00_3" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l,mul_w"))
	    (eq_attr "op_mem" "00,01,0i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv123_emac_00")

(define_insn_reservation "cfv12_emac_l_10_1" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_l_10_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_l_10_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_emac_10")

(define_insn_reservation "cfv3_emac_l_10_1" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_l_10_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_l_10_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_l"))
	    (eq_attr "op_mem" "10,i0,i1,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_emac_10")

(define_insn_reservation "cfv12_emac_w_10_1" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_w_10_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_w_10_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_emac_10")

(define_insn_reservation "cfv3_emac_w_10_1" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_w_10_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_w_10_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "10,11,1i"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_emac_10")

(define_insn_reservation "cfv12_emac_w_i0_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv12_emac_w_i0")

(define_insn_reservation "cfv12_emac_w_i0_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_emac_w_i0")

(define_insn_reservation "cfv3_emac_w_i0_2" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "1,2"))
  "cf_ib2+cfv3_emac_w_i0")

(define_insn_reservation "cfv3_emac_w_i0_3" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type" "mul_w"))
	    (eq_attr "op_mem" "i0,i1"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_emac_w_i0")

(define_insn_reservation "cfv12_rts" 5
  (and (eq_attr "cpu" "cfv1,cfv2")
       (eq_attr "type" "rts"))
  "cf_ib1+cfv12_rts")

(define_insn_reservation "cfv3_rts" 8
  (and (eq_attr "cpu" "cfv3")
       (eq_attr "type" "rts"))
  "cf_ib1+cfv3_rts")

(define_insn_reservation "cfv12_call_1" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_call")

(define_insn_reservation "cfv12_call_2" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_call")

(define_insn_reservation "cfv12_call_3" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_call")

(define_insn_reservation "cfv3_call_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_call")

(define_insn_reservation "cfv3_call_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_call")

(define_insn_reservation "cfv3_call_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bsr,jsr"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_call")

(define_insn_reservation "cfv12_bcc_1" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_bcc")

(define_insn_reservation "cfv12_bcc_2" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_bcc")

(define_insn_reservation "cfv12_bcc_3" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_bcc")

(define_insn_reservation "cfv3_bcc_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_bcc")

(define_insn_reservation "cfv3_bcc_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_bcc")

(define_insn_reservation "cfv3_bcc_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bcc"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_bcc")

(define_insn_reservation "cfv12_bra_1" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_bra")

(define_insn_reservation "cfv12_bra_2" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_bra")

(define_insn_reservation "cfv12_bra_3" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_bra")

(define_insn_reservation "cfv3_bra_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_bra")

(define_insn_reservation "cfv3_bra_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_bra")

(define_insn_reservation "cfv3_bra_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "bra"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_bra")

(define_insn_reservation "cfv12_jmp_1" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv12_jmp")

(define_insn_reservation "cfv12_jmp_2" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv12_jmp")

(define_insn_reservation "cfv12_jmp_3" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv12_jmp")

(define_insn_reservation "cfv3_jmp_1" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "1"))
  "cf_ib1+cfv3_jmp")

(define_insn_reservation "cfv3_jmp_2" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "2"))
  "cf_ib2+cfv3_jmp")

(define_insn_reservation "cfv3_jmp_3" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type" "jmp"))
       (eq_attr "size" "3"))
  "cf_ib3+cfv3_jmp")

(define_insn_reservation "cfv12_unlk" 2
  (and (eq_attr "cpu" "cfv1,cfv2")
       (eq_attr "type" "unlk"))
  "cf_ib1+cfv12_alu_10")

(define_insn_reservation "cfv3_unlk" 3
  (and (eq_attr "cpu" "cfv3")
       (eq_attr "type" "unlk"))
  "cf_ib1+cfv3_alu_10")

;; Dummy reservation for instructions that are not handled.
(define_insn_reservation "cfv123_guess" 3
  (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
       (eq_attr "type" "falu,fbcc,fcmp,fdiv,fmove,fmul,fneg,fsqrt,ftst,
                        div_w,div_l,link,mvsz,nop,trap,unknown"))
  "cf_ib3+cfv123_guess+cf_dsoc+cf_agex+cf_mem")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Below is pipeline description of ColdFire V4 core.
;; It is substantially different from the description of V1, V2 or V3 cores,
;; primarily due to no need to model the instruction buffer.
;;
;; V4 pipeline model uses a completely separate set of cpu units.

;; Operand Execution Pipeline.
(define_automaton "cfv4_oep")

(define_cpu_unit "cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_da"
  "cfv4_oep")

;; V4 has 3 cases of dual-issue.
;; After issuing a cfv4_pOEPx instruction, it'll be possible to issue
;; a cfv4_sOEPx instruction on the same cycle (see final_presence_sets below).
(define_cpu_unit "cfv4_pOEP1,cfv4_sOEP1,
                  cfv4_pOEP2,cfv4_sOEP2,
                  cfv4_pOEP3,cfv4_sOEP3" "cfv4_oep")

(final_presence_set "cfv4_sOEP1" "cfv4_pOEP1")
(final_presence_set "cfv4_sOEP2" "cfv4_pOEP2")
(final_presence_set "cfv4_sOEP3" "cfv4_pOEP3")

;; Reservation for instructions that don't allow dual-issue.
(define_reservation "cfv4_ds" "cfv4_pOEP1+cfv4_sOEP1+
                               cfv4_pOEP2+cfv4_sOEP2+
                               cfv4_pOEP3+cfv4_sOEP3")

;; Memory access resource.
(define_automaton "cfv4_mem")

(define_cpu_unit "cfv4_mem" "cfv4_mem")

;; EMAC.
(define_automaton "cfv4_emac")

(define_cpu_unit "cfv4_emac" "cfv4_emac")

;; FPU.
(define_automaton "cfv4_fp")

(define_cpu_unit "cfv4_fp" "cfv4_fp")

;; Automaton for unknown instruction.
(define_automaton "cfv4_guess")

(define_query_cpu_unit "cfv4_guess" "cfv4_guess")

;; This bypass allows 1st case of dual-issue.
(define_bypass 0 "cfv4_00_oag_pOEP1,cfv4_10_pOEP1,cfv4_i0_pOEP1"
  "cfv4_00_oag,cfv4_00_oag_pOEP3_sOEP12,cfv4_00_oag_pOEP1,
   cfv4_00_oag_moveql,cfv4_00_ex_sOEP13")

;; The following bypasses decrease the latency of producers if it modifies
;; a target register in the EX stage and the consumer also uses
;; that register in the EX stage.
(define_bypass 1 "cfv4_00_ex" "cfv4_00_ex,cfv4_00_ex_sOEP13")
(define_bypass 1 "cfv4_00_ex" "cfv4_10,cfv4_10_pOEP1,cfv4_i0,cfv4_i0_pOEP1"
  "!m68k_sched_address_bypass_p")

;; Indexed loads with scale factors 2 and 4 require an update of the index
;; register in the register file.  Considering that the index register is
;; only needed at the second cycle of address generation, we get
;; a latency of 4.
;; Producers for indexed loads with scale factor 1 should have
;; a latency of 3.  Since we're only allowed one bypass, we handle it
;; in the adjust_cost hook.
(define_bypass 4
  "cfv4_00_oag,cfv4_00_oag_pOEP3_sOEP12,cfv4_00_oag_lea,cfv4_00_oag_pOEP1,
   cfv4_00_oag_moveql"
  "cfv4_i0,cfv4_i0_pOEP1"
  "m68k_sched_indexed_address_bypass_p")

;; First part of cfv4_00.
;; If issued in pairs with cfv4_movel_?0, the cost should be increased.
;; ??? Is it possible that combined cfv4_movel_00 and cfv4_oag_00 instructions
;; have longer latency than the two instructions emitted sequentially?
;; Due to register renaming, the result of the sequence would be available
;; after 3 cycles, instead of 4 for combined instruction?
(define_insn_reservation "cfv4_00_oag" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,clr_l,cmp_l,mov3q_l,neg_l"))
       (eq_attr "op_mem" "00"))
  "cfv4_sOEP1|cfv4_sOEP3|(cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex)")

(define_insn_reservation "cfv4_00_oag_pOEP3_sOEP12" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "move_l,mov3q_l,clr_l"))
       (and (eq_attr "op_mem" "00")
	    (and (eq_attr "opx_type" "Rn")
		 (eq_attr "opy_type" "none,imm_q,imm_w,imm_l"))))
  "cfv4_sOEP1|cfv4_sOEP2|(cfv4_pOEP3,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex)")

(define_insn_reservation "cfv4_00_oag_lea" 1
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "lea"))
  "cfv4_pOEP3,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_00_oag_pOEP1" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "move_l,mov3q_l,clr_l"))
       (and (eq_attr "op_mem" "00")
	    (ior (eq_attr "opx_type" "!Rn")
		 (eq_attr "opy_type" "!none,imm_q,imm_w,imm_l"))))
  "cfv4_sOEP1|(cfv4_pOEP1,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex)")

(define_insn_reservation "cfv4_00_oag_moveql" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "moveq_l"))
       (eq_attr "op_mem" "00"))
  "cfv4_sOEP1|cfv4_sOEP2|cfv4_sOEP3|(cfv4_pOEP3,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex)")

;; Second part of cfv4_00.
;; Latency is either 1 or 4 depending on which stage the consumer
;; will need the data.

(define_insn_reservation "cfv4_00_ex" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "bitr,bitrw,clr,cmp,move,mvsz,scc,tst"))
       (eq_attr "op_mem" "00"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_00_ex_sOEP13" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alux_l,ext,shift,tst_l"))
       (eq_attr "op_mem" "00"))
  "cfv4_sOEP1|cfv4_sOEP3|(cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex)")

;; Several types mentioned in this reservation (e.g., ext and shift) don't
;; support implicit load.  But we handle them anyway due to first scheduling
;; pass, which handles non-strict rtl.
;;
;; Latency is either 1 or 4 depending in which stage the consumer
;; will need the data.
(define_insn_reservation "cfv4_10" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,moveq_l,mvsz,neg_l,
                             shift,tst,tst_l"))
       (eq_attr "op_mem" "10"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

;; Specialization of cfv4_10.
;; move.l has OC2-to-DS forwarding path, that saves one cycle of latency.
(define_insn_reservation "cfv4_10_pOEP1" 3
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "move_l"))
       (eq_attr "op_mem" "10"))
  "cfv4_pOEP1,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

;; Same here.  But +1 to latency due to longer OAG.
(define_insn_reservation "cfv4_i0" 5
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,moveq_l,mvsz,neg_l,
                             shift,tst,tst_l"))
       (eq_attr "op_mem" "i0"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

;; ??? Does indexed load trigger dual-issue?
;; ??? Does OC2-to-DS forwarding path saves a cycle?
(define_insn_reservation "cfv4_i0_pOEP1" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "move_l"))
       (eq_attr "op_mem" "i0"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

;; This reservation is for moves and clr.  Arithmetic instructions
;; don't write to memory unless they also read from it.
;; But, before reload we can have all sorts of things.
;; With cfv4_pOEP2 allow dual-issue for type 2 cases.
(define_insn_reservation "cfv4_01" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,move_l,moveq_l,mvsz,neg_l,
                             shift"))
       (eq_attr "op_mem" "01"))
  "cfv4_pOEP2,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

;; ??? Does indexed store trigger dual-issue?
(define_insn_reservation "cfv4_0i" 2
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,move_l,moveq_l,mvsz,neg_l,
                             shift"))
       (eq_attr "op_mem" "0i"))
  "cfv4_pOEP2,cfv4_oag,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

(define_insn_reservation "cfv4_11" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,move_l,moveq_l,mvsz,neg_l,
                             shift"))
       (eq_attr "op_mem" "11"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

;; Latency is 2 due to long OAG stage.
(define_insn_reservation "cfv4_i1" 2
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,move_l,moveq_l,mvsz,neg_l,
                             shift"))
       (eq_attr "op_mem" "i1"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

;; This one is the same as cfv4_i1.
;; ??? Should it be different?
(define_insn_reservation "cfv4_1i" 2
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "alu_l,aluq_l,alux_l,bitr,bitrw,
                             clr,clr_l,cmp,cmp_l,ext,
                             mov3q_l,move,move_l,moveq_l,mvsz,neg_l,
                             shift"))
       (eq_attr "op_mem" "1i"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

;; ??? Does pea indeed support case 2 of dual-issue?
(define_insn_reservation "cfv4_11_pea" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "pea"))
       (eq_attr "op_mem" "11,00,01,0i,10"))
  "cfv4_pOEP2,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

;; ??? Does pea indeed support case 2 of dual-issue?
;; ??? Does indexed store trigger dual-issue?
(define_insn_reservation "cfv4_i1_pea" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "pea"))
       (eq_attr "op_mem" "i1,1i"))
  "cfv4_pOEP2,cfv4_oag,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_da,cfv4_mem")

(define_insn_reservation "cfv4_link" 2
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "link"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_ex,cfv4_da,cfv4_mem")

(define_insn_reservation "cfv4_unlink" 2
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "unlk"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_divw_00" 20
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "div_w"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex*15")

(define_insn_reservation "cfv4_divw_10" 20
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "div_w"))
       (eq_attr "op_mem" "10,11,1i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex*15")

(define_insn_reservation "cfv4_divw_i0" 21
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "div_w"))
       (eq_attr "op_mem" "i0,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex*15")

(define_insn_reservation "cfv4_divl_00" 35
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "div_l"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex*30")

(define_insn_reservation "cfv4_divl_10" 35
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "div_l"))
       (eq_attr "op_mem" "10,11,1i,i0,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex*30")

(define_insn_reservation "cfv4_emac_mul_00" 7
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "mul_w,mul_l"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_emac")

(define_insn_reservation "cfv4_emac_mul_10" 7
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "mul_w,mul_l"))
       (eq_attr "op_mem" "10,11,1i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_emac")

(define_insn_reservation "cfv4_emac_mul_i0" 8
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "mul_w,mul_l"))
       (eq_attr "op_mem" "i0,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_emac")

(define_insn_reservation "cfv4_falu_00" 7
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "falu,fcmp,fmul"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp")

(define_insn_reservation "cfv4_falu_10" 7
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "falu,fcmp,fmul"))
       (eq_attr "op_mem" "10,i0,11,1i,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_fp")

(define_insn_reservation "cfv4_fneg_00" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fmove,fneg,ftst"))
       (eq_attr "op_mem" "00"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp")

(define_insn_reservation "cfv4_fmove_fneg_10" 4
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fmove,fneg,ftst"))
       (eq_attr "op_mem" "10,i0,11,1i,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_fp")

(define_insn_reservation "cfv4_fmove_01" 1
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fmove,fneg,ftst"))
       (eq_attr "op_mem" "01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp,cfv4_da,cfv4_mem")

(define_insn_reservation "cfv4_fdiv_00" 23
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fdiv"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp*17")

(define_insn_reservation "cfv4_fdiv_10" 23
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fdiv"))
       (eq_attr "op_mem" "10,i0,11,1i,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_fp*17")

(define_insn_reservation "cfv4_fsqrt_00" 56
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fsqrt"))
       (eq_attr "op_mem" "00,01,0i"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp*50")

(define_insn_reservation "cfv4_fsqrt_10" 56
  (and (and (eq_attr "cpu" "cfv4")
	    (eq_attr "type" "fsqrt"))
       (eq_attr "op_mem" "10,i0,11,1i,i1"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_fp*50")

(define_insn_reservation "cfv4_bcc" 0
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "bcc"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_fbcc" 2
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "fbcc"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex,cfv4_fp")

;; ??? Why is bra said to write to memory: 1(0/1) ?
(define_insn_reservation "cfv4_bra_bsr" 1
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "bra,bsr"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_jmp_jsr" 5
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "jmp,jsr"))
  "cfv4_ds,cfv4_oag,cfv4_oc1,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_rts" 2
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "rts"))
  "cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex")

(define_insn_reservation "cfv4_nop" 1
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "nop"))
  "cfv4_ds+cfv4_oag+cfv4_oc1+cfv4_mem+cfv4_oc2+cfv4_ex")

(define_insn_reservation "cfv4_guess" 10
  (and (eq_attr "cpu" "cfv4")
       (eq_attr "type" "trap,unknown"))
  "cfv4_guess+cfv4_ds,cfv4_oag,cfv4_oc1+cfv4_mem,cfv4_oc2,cfv4_ex,cfv4_emac+cfv4_fp")

(define_insn_reservation "ignore" 0
  (eq_attr "type" "ignore")
  "nothing")
