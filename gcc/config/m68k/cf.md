;; ColdFire V2 DFA description.
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

;; ??? To let genattrtab live, implement this attribute in C.
(define_attr "type2"
  "alu, alu_l, bcc, bra, call, jmp, lea, move, move_l, mul, pea, rts, unlk,
   unknown"
  (symbol_ref "m68k_sched_attr_type2 (insn)"))

;; Instruction Buffer
(define_automaton "cf_v2_ib")

;; If one of these cpu units is occupied, that means that corresponding
;; word in the buffer is empty.
(define_cpu_unit "cf_v2_ib_w0, cf_v2_ib_w1, cf_v2_ib_w2, cf_v2_ib_w3, cf_v2_ib_w4, cf_v2_ib_w5" "cf_v2_ib")

(final_presence_set "cf_v2_ib_w1, cf_v2_ib_w2, cf_v2_ib_w3, cf_v2_ib_w4, cf_v2_ib_w5" "cf_v2_ib_w0")
(final_presence_set "cf_v2_ib_w2, cf_v2_ib_w3, cf_v2_ib_w4, cf_v2_ib_w5" "cf_v2_ib_w1")
(final_presence_set "cf_v2_ib_w3, cf_v2_ib_w4, cf_v2_ib_w5" "cf_v2_ib_w2")
(final_presence_set "cf_v2_ib_w4, cf_v2_ib_w5" "cf_v2_ib_w3")
(final_presence_set "cf_v2_ib_w5" "cf_v2_ib_w4")

;; Occupy 1 word.
(define_reservation "cf_v2_ib1" "cf_v2_ib_w0|cf_v2_ib_w1|cf_v2_ib_w2|cf_v2_ib_w3|cf_v2_ib_w4|cf_v2_ib_w5")

;; Occupy 2 words.
(define_reservation "cf_v2_ib2" "(cf_v2_ib_w0+cf_v2_ib_w1)|(cf_v2_ib_w1+cf_v2_ib_w2)|(cf_v2_ib_w2+cf_v2_ib_w3)|(cf_v2_ib_w3+cf_v2_ib_w4)|(cf_v2_ib_w4+cf_v2_ib_w5)")

;; Occupy 3 words.
(define_reservation "cf_v2_ib3" "(cf_v2_ib_w0+cf_v2_ib_w1+cf_v2_ib_w2)|(cf_v2_ib_w1+cf_v2_ib_w2+cf_v2_ib_w3)|(cf_v2_ib_w2+cf_v2_ib_w3+cf_v2_ib_w4)|(cf_v2_ib_w3+cf_v2_ib_w4+cf_v2_ib_w5)")

;; Reservation to subscribe 1 word in the instruction buffer.  If a given
;; word in the instruction buffer is subscribed, that means it is empty.
;; This reservation is used at the start of each cycle to setup the number
;; of prefetched instruction words in the instruction buffer.
;; At each cycle, given that memory bus is available (i.e. there is no
;; pending memory operation), IFP prefetches two instruction words into IB.
(define_insn_reservation "cf_v2_ib" 0
  (and (eq_attr "cpu" "cf_v2")
       (eq_attr "type" "ib"))
  "cf_v2_ib1")

;; Operand Execution Pipeline
(define_automaton "cf_v2_oep")

(define_cpu_unit "cf_v2_dsoc, cf_v2_agex" "cf_v2_oep")

;; A memory unit that is reffered to as 'certain hardware resources' in
;; ColdFire reference manuals.  This unit remains occupied for two cycles
;; after last dsoc cycle of a store - hence there is a 2 cycle delay between
;; two consecutive stores.
(define_automaton "cf_v2_chr")

(define_cpu_unit "cf_v2_chr" "cf_v2_chr")

;; Memory bus
(define_automaton "cf_v2_mem")

;; When memory bus is subscribed, that implies that instruction buffer won't
;; get its portion this cycle.  To model that we query if cf_v2_mem unit is
;; subscribed and adjust number of prefetched instruction words accordingly.
;; 
(define_query_cpu_unit "cf_v2_mem" "cf_v2_mem")

;; Register to register move.
;; Takes 1 cycle.
(define_reservation "cf_v2_move_00"
  "cf_v2_dsoc+cf_v2_agex")

;; Load from a memory location.
;; Takes 3 cycles.
(define_reservation "cf_v2_move_10"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex")

;; Long load from a memory location.
;; Takes 2 cycles.
(define_reservation "cf_v2_move_l_10"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex")

;; Load from an indexed location.
;; Takes 4 cycles.
(define_reservation "cf_v2_move_i0"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex")

;; Long load from an indexed location.
;; Takes 3 cycles.
(define_reservation "cf_v2_move_l_i0"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex")

;; Store to a memory location.
;; Takes 1 cycle.
(define_reservation "cf_v2_move_01"
  "cf_v2_dsoc+cf_v2_agex+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Store to an indexed location.
;; Takes 2 cycle.
(define_reservation "cf_v2_move_0i"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Load from a memory location and store to a memory location.
;; Takes 3 cycles
(define_reservation "cf_v2_move_11"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Long load from a memory location and store to a memory location.
;; Takes 2 cycles.
(define_reservation "cf_v2_move_l_11"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Load from an indexed location and store to a memory location.
;; Takes 4 cycles.
(define_reservation "cf_v2_move_i1"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Long load from an indexed location and store to a memory location.
;; Takes 3 cycles.
(define_reservation "cf_v2_move_l_i1"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Load from a memory location and store to an indexed location.
;; Takes 4 cycles.
(define_reservation "cf_v2_move_1i"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem,cf_v2_agex,cf_v2_mem")

;; Long load from a memory location and store to an indexed location.
;; Takes 3 cycles.
(define_reservation "cf_v2_move_l_1i"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_dsoc+cf_v2_agex+cf_v2_mem,cf_v2_agex,cf_v2_mem")

;; Lea operation for a memory location.
;; Takes 1 cycle.
(define_reservation "cf_v2_lea_10"
  "cf_v2_dsoc+cf_v2_agex")

;; Lea operation for an indexed location.
;; Takes 2 cycles.
(define_reservation "cf_v2_lea_i0"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex")

;; Pea operation for a memory location.
;; Takes 2 cycle.
(define_reservation "cf_v2_pea_11"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

;; Pea operation for an indexed location.
;; Takes 3 cycles.
(define_reservation "cf_v2_pea_i1"
  "cf_v2_dsoc+cf_v2_agex,cf_v2_agex,cf_v2_agex+cf_v2_chr,cf_v2_mem+cf_v2_chr,cf_v2_chr")

(define_automaton "cf_v2_emac")

(define_cpu_unit "cf_v2_emac1,cf_v2_emac2,cf_v2_emac3,cf_v2_emac4"
  "cf_v2_emac")

;; Mul operation with register operands.
;; Takes 4 cycles.
(define_reservation "cf_v2_mul_00"
  "cf_v2_dsoc,cf_v2_agex+cf_v2_emac1,cf_v2_emac2,cf_v2_emac3,cf_v2_emac4")

;; Mul operation with implicit load from a memory location.
;; Takes 6 cycles.
(define_reservation "cf_v2_mul_10"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex+cf_v2_emac1,cf_v2_emac2,cf_v2_emac3,cf_v2_emac4")

;; Mul operation with implicit load from an indexed location.
;; Takes 7 cycles.
(define_reservation "cf_v2_mul_i0"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_agex,cf_v2_dsoc+cf_v2_mem,cf_v2_agex+cf_v2_emac1,cf_v2_emac2,cf_v2_emac3,cf_v2_emac4")

;; Instruction reservations.

;; Below reservations are simple derivation from the above reservations.
;; Each reservation from the above expands into 3 reservations below - one
;; for each instruction size.
;; A number in the end of reservation's name is the size of the instruction.

(define_insn_reservation "cf_v2_move_00_1" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu,alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib1+cf_v2_move_00")

(define_insn_reservation "cf_v2_move_00_2" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu,alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib2+cf_v2_move_00")

(define_insn_reservation "cf_v2_move_00_3" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu,alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib3+cf_v2_move_00")

(define_insn_reservation "cf_v2_move_10_1" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib1+cf_v2_move_10")

(define_insn_reservation "cf_v2_move_10_2" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib2+cf_v2_move_10")

(define_insn_reservation "cf_v2_move_10_3" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib3+cf_v2_move_10")

(define_insn_reservation "cf_v2_move_l_10_1" 3
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib1+cf_v2_move_l_10")

(define_insn_reservation "cf_v2_move_l_10_2" 3
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib2+cf_v2_move_l_10")

(define_insn_reservation "cf_v2_move_l_10_3" 3
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib3+cf_v2_move_l_10")

(define_insn_reservation "cf_v2_move_i0_2" 5
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib2+cf_v2_move_i0")

(define_insn_reservation "cf_v2_move_i0_3" 5
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib3+cf_v2_move_i0")

(define_insn_reservation "cf_v2_move_l_i0_2" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib2+cf_v2_move_l_i0")

(define_insn_reservation "cf_v2_move_l_i0_3" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib3+cf_v2_move_l_i0")

(define_insn_reservation "cf_v2_move_01_1" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "01"))
  "cf_v2_ib1+cf_v2_move_01")

(define_insn_reservation "cf_v2_move_01_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "01"))
  "cf_v2_ib2+cf_v2_move_01")

(define_insn_reservation "cf_v2_move_01_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "01"))
  "cf_v2_ib3+cf_v2_move_01")

(define_insn_reservation "cf_v2_move_0i_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "0i"))
  "cf_v2_ib2+cf_v2_move_0i")

(define_insn_reservation "cf_v2_move_0i_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move,move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "0i"))
  "cf_v2_ib3+cf_v2_move_0i")

(define_insn_reservation "cf_v2_move_11_1" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib1+cf_v2_move_11")

(define_insn_reservation "cf_v2_move_11_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib2+cf_v2_move_11")

(define_insn_reservation "cf_v2_move_11_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib3+cf_v2_move_11")

(define_insn_reservation "cf_v2_move_l_11_1" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib1+cf_v2_move_l_11")

(define_insn_reservation "cf_v2_move_l_11_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib2+cf_v2_move_l_11")

(define_insn_reservation "cf_v2_move_l_11_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib3+cf_v2_move_l_11")

(define_insn_reservation "cf_v2_move_i1_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib2+cf_v2_move_i1")

(define_insn_reservation "cf_v2_move_i1_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib3+cf_v2_move_i1")

(define_insn_reservation "cf_v2_move_l_i1_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib2+cf_v2_move_l_i1")

(define_insn_reservation "cf_v2_move_l_i1_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib3+cf_v2_move_l_i1")

(define_insn_reservation "cf_v2_move_1i_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_v2_ib2+cf_v2_move_1i")

(define_insn_reservation "cf_v2_move_1i_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "alu_l,move"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_v2_ib3+cf_v2_move_1i")

(define_insn_reservation "cf_v2_move_l_1i_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "1i"))
  "cf_v2_ib2+cf_v2_move_l_1i")

(define_insn_reservation "cf_v2_move_l_1i_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "move_l"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "1i"))
  "cf_v2_ib3+cf_v2_move_l_1i")

(define_insn_reservation "cf_v2_lea_10_1" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib1+cf_v2_lea_10")

(define_insn_reservation "cf_v2_lea_10_2" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib2+cf_v2_lea_10")

(define_insn_reservation "cf_v2_lea_10_3" 1
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib3+cf_v2_lea_10")

(define_insn_reservation "cf_v2_lea_i0_2" 2
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib2+cf_v2_lea_i0")

(define_insn_reservation "cf_v2_lea_i0_3" 2
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "lea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib3+cf_v2_lea_i0")

(define_insn_reservation "cf_v2_pea_11_1" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib1+cf_v2_pea_11")

(define_insn_reservation "cf_v2_pea_11_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib2+cf_v2_pea_11")

(define_insn_reservation "cf_v2_pea_11_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "11"))
  "cf_v2_ib3+cf_v2_pea_11")

(define_insn_reservation "cf_v2_pea_i1_2" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib2+cf_v2_pea_i1")

(define_insn_reservation "cf_v2_pea_i1_3" 0
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "pea"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i1"))
  "cf_v2_ib3+cf_v2_pea_i1")

(define_insn_reservation "cf_v2_mul_00_1" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib1+cf_v2_mul_00")

(define_insn_reservation "cf_v2_mul_00_2" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib2+cf_v2_mul_00")

(define_insn_reservation "cf_v2_mul_00_3" 4
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "00"))
  "cf_v2_ib3+cf_v2_mul_00")

(define_insn_reservation "cf_v2_mul_10_1" 6
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib1+cf_v2_mul_10")

(define_insn_reservation "cf_v2_mul_10_2" 6
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib2+cf_v2_mul_10")

(define_insn_reservation "cf_v2_mul_10_3" 6
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "10"))
  "cf_v2_ib3+cf_v2_mul_10")

(define_insn_reservation "cf_v2_mul_i0_2" 7
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib2+cf_v2_mul_i0")

(define_insn_reservation "cf_v2_mul_i0_3" 7
  (and (and (and (eq_attr "cpu" "cf_v2")
		 (eq_attr "type2" "mul"))
	    (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
       (eq_attr "op_mem" "i0"))
  "cf_v2_ib3+cf_v2_mul_i0")

;; ??? As return reads target address from stack, use a mem-read reservation
;; for it.
(define_reservation "cf_v2_rts" "cf_v2_move_10")

;; ??? It's not clear what the core does during these 5 cycles.
;; Luckily, we don't care that much about an insn that won't be moved.
(define_insn_reservation "cf_v2_rts_1" 5
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "rts"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_rts")

;; Call instructions reservations.

;; ??? It's not clear what reservation is best to use for calls.
;; For now we use mem-write + return reservations to reflect the fact of
;; pushing and poping return address to and from the stack.

(define_insn_reservation "cf_v2_call_1" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_move_10,cf_v2_rts")

(define_insn_reservation "cf_v2_call_2" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_v2_ib2+cf_v2_move_10,cf_v2_rts")

(define_insn_reservation "cf_v2_call_3" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "call"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_v2_ib3+cf_v2_move_10,cf_v2_rts")

;; Branch reservations.

;; ??? Branch reservations are unclear to me so far.  Luckily, we don't care
;; ??? that much about branches.
(define_reservation "cf_v2_bcc" "cf_v2_move_00")

(define_insn_reservation "cf_v2_bcc_1" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_bcc")

(define_insn_reservation "cf_v2_bcc_2" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_v2_ib2+cf_v2_bcc")

(define_insn_reservation "cf_v2_bcc_3" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bcc"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_v2_ib3+cf_v2_bcc")

(define_reservation "cf_v2_bra" "cf_v2_move_01")

(define_insn_reservation "cf_v2_bra_1" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_bra")

(define_insn_reservation "cf_v2_bra_2" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_v2_ib2+cf_v2_bra")

(define_insn_reservation "cf_v2_bra_3" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "bra"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_v2_ib3+cf_v2_bra")

;; Computed jump.
;; Takes 3 cycles.
(define_reservation "cf_v2_jmp"
  "cf_v2_dsoc,cf_v2_agex,cf_v2_dsoc,cf_v2_agex")

(define_insn_reservation "cf_v2_jmp_1" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_jmp")

(define_insn_reservation "cf_v2_jmp_2" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_v2_ib2+cf_v2_jmp")

(define_insn_reservation "cf_v2_jmp_3" 3
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "jmp"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_v2_ib3+cf_v2_jmp")

;; Misc reservations.

(define_insn_reservation "cf_v2_unlk_1" 2
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "type2" "unlk"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_move_l_10")

;; This automaton is used to gather statistics on insns that need reservations.
(define_automaton "cf_v2_guess")

(define_query_cpu_unit "cf_v2_guess" "cf_v2_guess")

;; Dummy reservation for instructions that are not handled yet.

(define_insn_reservation "cf_v2_guess_1" 1
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 1)))
  "cf_v2_ib1+cf_v2_guess+cf_v2_dsoc+cf_v2_agex")

(define_insn_reservation "cf_v2_guess_2" 1
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 2)))
  "cf_v2_ib2+cf_v2_guess+cf_v2_dsoc+cf_v2_agex")

(define_insn_reservation "cf_v2_guess_3" 1
  (and (and (eq_attr "cpu" "cf_v2")
	    (eq_attr "guess" "yes"))
       (eq (symbol_ref "get_attr_size (insn)") (const_int 3)))
  "cf_v2_ib3+cf_v2_guess+cf_v2_dsoc+cf_v2_agex")
