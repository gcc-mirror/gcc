;; ColdFire V1, V2 and V3 DFA description.
;; Copyright (C) 2007 Free Software Foundation, Inc.
;; Contributed by CodeSourcery Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Intruction types recognized by DFA.
;; This attribute correspond to type1 attribute with the exceptions below.
;; omove - optimized move.  All explicit loads on cfv1 and long explicit
;;         loads on cfv2 execute one cycle faster then they should.
;;         Supposedly, that is due to combined instruction decoding
;;         and address generation phases.
;; ??? To let genattrtab live, implement this attribute in C.
(define_attr "type2"
  "alu, alu_reg, bcc, bra, call, jmp, lea, mul_l, mul_w, omove, pea,
   rts, unlk, unknown"
  (symbol_ref "m68k_sched_attr_type2 (insn)"))

;; Instruction Buffer
(define_automaton "cf_ib")

;; These pseudo units are used to model instruction buffer of ColdFire cores.
;; Instruction of size N can be issued only when cf_ib_wN is available.
(define_cpu_unit "cf_ib_w1, cf_ib_w2, cf_ib_w3" "cf_ib")

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
(define_automaton "cf_oep")

(define_cpu_unit "cf_dsoc, cf_agex" "cf_oep")

;; A memory unit that is reffered to as 'certain hardware resources' in
;; ColdFire reference manuals.  This unit remains occupied for two cycles
;; after last dsoc cycle of a store - hence there is a 2 cycle delay between
;; two consecutive stores.
(define_automaton "cf_chr")

(define_cpu_unit "cf_chr" "cf_chr")

;; Memory bus
(define_automaton "cf_mem")

;; When memory bus is subscribed, that implies that instruction buffer won't
;; get its portion this cycle.  To model that we query if cf_mem unit is
;; subscribed and adjust number of prefetched instruction words accordingly.
;; 
(define_query_cpu_unit "cf_mem1, cf_mem2" "cf_mem")

(define_reservation "cf_mem" "cf_mem1+cf_mem2")

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

(define_automaton "cf_mac")

(define_cpu_unit "cf_mac1,cf_mac2,cf_mac3,cf_mac4"
  "cf_mac")

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
		 (eq_attr "type2" "alu,alu_reg,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_alu_00")

(define_insn_reservation "cfv123_alu_00_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "alu,alu_reg,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_alu_00")

(define_insn_reservation "cfv123_alu_00_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "alu,alu_reg,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_alu_00")

(define_insn_reservation "cfv12_alu_10_1" 3
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_alu_10")

(define_insn_reservation "cfv12_alu_10_2" 3
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_alu_10")

(define_insn_reservation "cfv12_alu_10_3" 3
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_alu_10")

(define_insn_reservation "cfv12_omove_10_1" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_omove_10")

(define_insn_reservation "cfv12_omove_10_2" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_omove_10")

(define_insn_reservation "cfv12_omove_10_3" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_omove_10")

(define_insn_reservation "cfv3_alu_10_1" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_alu_10")

(define_insn_reservation "cfv3_alu_10_2" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_alu_10")

(define_insn_reservation "cfv3_alu_10_3" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_alu_10")

(define_insn_reservation "cfv3_omove_10_1" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_omove_10")

(define_insn_reservation "cfv3_omove_10_2" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_omove_10")

(define_insn_reservation "cfv3_omove_10_3" 3
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_omove_10")

(define_insn_reservation "cfv12_alu_i0_2" 4
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv12_alu_i0")

(define_insn_reservation "cfv12_alu_i0_3" 4
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv12_alu_i0")

(define_insn_reservation "cfv12_omove_i0_2" 3
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv12_omove_i0")

(define_insn_reservation "cfv12_omove_i0_3" 3
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv12_omove_i0")

(define_insn_reservation "cfv3_alu_i0_2" 5
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv3_alu_i0")

(define_insn_reservation "cfv3_alu_i0_3" 5
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv3_alu_i0")

(define_insn_reservation "cfv3_omove_i0_2" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv3_omove_i0")

(define_insn_reservation "cfv3_omove_i0_3" 4
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv3_omove_i0")

(define_insn_reservation "cfv12_alu_01_1" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "01"))
  "cf_ib1+cfv12_alu_01")

(define_insn_reservation "cfv12_alu_01_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "01"))
  "cf_ib2+cfv12_alu_01")

(define_insn_reservation "cfv12_alu_01_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "01"))
  "cf_ib3+cfv12_alu_01")

(define_insn_reservation "cfv3_alu_01_1" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "01"))
  "cf_ib1+cfv3_alu_01")

(define_insn_reservation "cfv3_alu_01_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "01"))
  "cf_ib2+cfv3_alu_01")

(define_insn_reservation "cfv3_alu_01_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "01"))
  "cf_ib3+cfv3_alu_01")

(define_insn_reservation "cfv12_alu_0i_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "0i"))
  "cf_ib2+cfv12_alu_0i")

(define_insn_reservation "cfv12_alu_0i_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "0i"))
  "cf_ib3+cfv12_alu_0i")

(define_insn_reservation "cfv3_alu_0i_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "0i"))
  "cf_ib2+cfv3_alu_0i")

(define_insn_reservation "cfv3_alu_0i_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu,omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "0i"))
  "cf_ib3+cfv3_alu_0i")

(define_insn_reservation "cfv12_alu_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv12_alu_11")

(define_insn_reservation "cfv12_alu_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv12_alu_11")

(define_insn_reservation "cfv12_alu_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv12_alu_11")

(define_insn_reservation "cfv12_omove_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv12_omove_11")

(define_insn_reservation "cfv12_omove_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv12_omove_11")

(define_insn_reservation "cfv12_omove_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv12_omove_11")

(define_insn_reservation "cfv3_alu_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv3_alu_11")

(define_insn_reservation "cfv3_alu_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv3_alu_11")

(define_insn_reservation "cfv3_alu_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv3_alu_11")

(define_insn_reservation "cfv3_omove_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv3_omove_11")

(define_insn_reservation "cfv3_omove_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv3_omove_11")

(define_insn_reservation "cfv3_omove_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv3_omove_11")

(define_insn_reservation "cfv12_alu_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv12_alu_i1")

(define_insn_reservation "cfv12_alu_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv12_alu_i1")

(define_insn_reservation "cfv12_omove_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv12_omove_i1")

(define_insn_reservation "cfv12_omove_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv12_omove_i1")

(define_insn_reservation "cfv3_alu_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv3_alu_i1")

(define_insn_reservation "cfv3_alu_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv3_alu_i1")

(define_insn_reservation "cfv3_omove_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv3_omove_i1")

(define_insn_reservation "cfv3_omove_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv3_omove_i1")

(define_insn_reservation "cfv12_alu_1i_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_ib2+cfv12_alu_1i")

(define_insn_reservation "cfv12_alu_1i_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_ib3+cfv12_alu_1i")

(define_insn_reservation "cfv12_omove_1i_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_ib2+cfv12_omove_1i")

(define_insn_reservation "cfv12_omove_1i_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_ib3+cfv12_omove_1i")

(define_insn_reservation "cfv3_alu_1i_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_ib2+cfv3_alu_1i")

(define_insn_reservation "cfv3_alu_1i_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "alu"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_ib3+cfv3_alu_1i")

(define_insn_reservation "cfv3_omove_1i_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_ib2+cfv3_omove_1i")

(define_insn_reservation "cfv3_omove_1i_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "omove"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_ib3+cfv3_omove_1i")

(define_insn_reservation "cfv123_lea_10_1" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_10_2" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_10_3" 1
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv123_lea_10")

(define_insn_reservation "cfv123_lea_i0_2" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv123_lea_i0")

(define_insn_reservation "cfv123_lea_i0_3" 2
  (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv123_lea_i0")

(define_insn_reservation "cfv12_pea_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv12_pea_11")

(define_insn_reservation "cfv12_pea_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv12_pea_11")

(define_insn_reservation "cfv12_pea_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv12_pea_11")

(define_insn_reservation "cfv3_pea_11_1" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_ib1+cfv3_pea_11")

(define_insn_reservation "cfv3_pea_11_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_ib2+cfv3_pea_11")

(define_insn_reservation "cfv3_pea_11_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_ib3+cfv3_pea_11")

(define_insn_reservation "cfv12_pea_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv12_pea_i1")

(define_insn_reservation "cfv12_pea_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv12_pea_i1")

(define_insn_reservation "cfv3_pea_i1_2" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_ib2+cfv3_pea_i1")

(define_insn_reservation "cfv3_pea_i1_3" 0
  (and (and (and (eq_attr "cpu" "cfv3")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_ib3+cfv3_pea_i1")

(define_insn_reservation "cfv123_mul_l_00_1" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_l_00_2" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_l_00_3" 18
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_mul_l_00")

(define_insn_reservation "cfv123_mul_w_00_1" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_mul_w_00")

(define_insn_reservation "cfv123_mul_w_00_2" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_mul_w_00")

(define_insn_reservation "cfv123_mul_w_00_3" 9
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_mul_w_00")

(define_insn_reservation "cfv12_mul_l_10_1" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_mul_l_10")

(define_insn_reservation "cfv12_mul_l_10_2" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_mul_l_10")

(define_insn_reservation "cfv12_mul_l_10_3" 20
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_1" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_2" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_mul_l_10")

(define_insn_reservation "cfv3_mul_l_10_3" 21
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_mul_l_10")

(define_insn_reservation "cfv12_mul_w_10_1" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_mul_w_10")

(define_insn_reservation "cfv12_mul_w_10_2" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_mul_w_10")

(define_insn_reservation "cfv12_mul_w_10_3" 11
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_1" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_2" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_mul_w_10")

(define_insn_reservation "cfv3_mul_w_10_3" 12
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_mul_w_10")

(define_insn_reservation "cfv12_mul_w_i0_2" 12
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv12_mul_w_i0")

(define_insn_reservation "cfv12_mul_w_i0_3" 12
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv12_mul_w_i0")


(define_insn_reservation "cfv3_mul_w_i0_2" 13
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv3_mul_w_i0")

(define_insn_reservation "cfv3_mul_w_i0_3" 13
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "no"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv3_mul_w_i0")

(define_insn_reservation "cfv123_mac_l_00_1" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_l_00_2" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_l_00_3" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_mac_l_00")

(define_insn_reservation "cfv123_mac_w_00_1" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_mac_w_00")

(define_insn_reservation "cfv123_mac_w_00_2" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_mac_w_00")

(define_insn_reservation "cfv123_mac_w_00_3" 3
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_mac_w_00")

(define_insn_reservation "cfv12_mac_l_10_1" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_mac_l_10")

(define_insn_reservation "cfv12_mac_l_10_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_mac_l_10")

(define_insn_reservation "cfv12_mac_l_10_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_1" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_2" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_mac_l_10")

(define_insn_reservation "cfv3_mac_l_10_3" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_mac_l_10")

(define_insn_reservation "cfv12_mac_w_10_1" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_mac_w_10")

(define_insn_reservation "cfv12_mac_w_10_2" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_mac_w_10")

(define_insn_reservation "cfv12_mac_w_10_3" 5
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_1" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_mac_w_10")

(define_insn_reservation "cfv3_mac_w_10_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_mac_w_10")

(define_insn_reservation "cfv12_mac_w_i0_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv12_mac_w_i0")

(define_insn_reservation "cfv12_mac_w_i0_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv12_mac_w_i0")

(define_insn_reservation "cfv3_mac_w_i0_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv3_mac_w_i0")

(define_insn_reservation "cfv3_mac_w_i0_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_mac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv3_mac_w_i0")

(define_insn_reservation "cfv123_emac_00_1" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_ib1+cfv123_emac_00")

(define_insn_reservation "cfv123_emac_00_2" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_ib2+cfv123_emac_00")

(define_insn_reservation "cfv123_emac_00_3" 4
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_ib3+cfv123_emac_00")

(define_insn_reservation "cfv12_emac_10_1" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_10_2" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv12_emac_10")

(define_insn_reservation "cfv12_emac_10_3" 6
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv12_emac_10")

(define_insn_reservation "cfv3_emac_10_1" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_ib1+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_10_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_ib2+cfv3_emac_10")

(define_insn_reservation "cfv3_emac_10_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_l,mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_ib3+cfv3_emac_10")

(define_insn_reservation "cfv12_emac_w_i0_2" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv12_emac_w_i0")

(define_insn_reservation "cfv12_emac_w_i0_3" 7
  (and (and (and (and (eq_attr "cpu" "cfv1,cfv2")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv12_emac_w_i0")

(define_insn_reservation "cfv3_emac_w_i0_2" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_ib2+cfv3_emac_w_i0")

(define_insn_reservation "cfv3_emac_w_i0_3" 8
  (and (and (and (and (eq_attr "cpu" "cfv3")
		      (eq_attr "mac" "cf_emac"))
		 (eq_attr "type2" "mul_w"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_ib3+cfv3_emac_w_i0")

(define_insn_reservation "cfv12_rts_1" 5
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "rts"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_rts")

(define_insn_reservation "cfv3_rts_1" 8
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "rts"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_rts")

(define_insn_reservation "cfv12_call_1" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_call")

(define_insn_reservation "cfv12_call_2" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv12_call")

(define_insn_reservation "cfv12_call_3" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv12_call")

(define_insn_reservation "cfv3_call_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_call")

(define_insn_reservation "cfv3_call_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv3_call")

(define_insn_reservation "cfv3_call_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv3_call")

(define_insn_reservation "cfv12_bcc_1" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_bcc")

(define_insn_reservation "cfv12_bcc_2" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv12_bcc")

(define_insn_reservation "cfv12_bcc_3" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv12_bcc")

(define_insn_reservation "cfv3_bcc_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_bcc")

(define_insn_reservation "cfv3_bcc_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv3_bcc")

(define_insn_reservation "cfv3_bcc_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv3_bcc")

(define_insn_reservation "cfv12_bra_1" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_bra")

(define_insn_reservation "cfv12_bra_2" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv12_bra")

(define_insn_reservation "cfv12_bra_3" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv12_bra")

(define_insn_reservation "cfv3_bra_1" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_bra")

(define_insn_reservation "cfv3_bra_2" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv3_bra")

(define_insn_reservation "cfv3_bra_3" 1
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv3_bra")

(define_insn_reservation "cfv12_jmp_1" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_jmp")

(define_insn_reservation "cfv12_jmp_2" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv12_jmp")

(define_insn_reservation "cfv12_jmp_3" 3
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv12_jmp")

(define_insn_reservation "cfv3_jmp_1" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_jmp")

(define_insn_reservation "cfv3_jmp_2" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cfv3_jmp")

(define_insn_reservation "cfv3_jmp_3" 5
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cfv3_jmp")

;; Misc reservations.

(define_insn_reservation "cfv12_unlk_1" 2
  (and (and (eq_attr "cpu" "cfv1,cfv2")
	    (eq_attr "type2" "unlk"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv12_alu_10")

(define_insn_reservation "cfv3_unlk_1" 3
  (and (and (eq_attr "cpu" "cfv3")
	    (eq_attr "type2" "unlk"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cfv3_alu_10")

;; This automaton is used to gather statistics on insns that need reservations.
(define_automaton "cf_guess")

(define_query_cpu_unit "cf_guess" "cf_guess")

;; Dummy reservation for instructions that are not handled yet.

(define_insn_reservation "cf_guess_1" 1
  (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_ib1+cf_guess+cf_dsoc+cf_agex")

(define_insn_reservation "cf_guess_2" 1
  (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_ib2+cf_guess+cf_dsoc+cf_agex")

(define_insn_reservation "cf_guess_3" 1
  (and (and (eq_attr "cpu" "cfv1,cfv2,cfv3")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_ib3+cf_guess+cf_dsoc+cf_agex")
