;; Scheduling description for Niagara-4
;;   Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

(define_automaton "niagara4_0")

(define_cpu_unit "n4_slot0,n4_slot1,n4_slot2" "niagara4_0")
(define_reservation "n4_single_issue" "n4_slot0 + n4_slot1 + n4_slot2")

(define_cpu_unit "n4_load_store" "niagara4_0")

(define_insn_reservation "n4_single" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "multi,savew,flushw,iflush,trap"))
  "n4_single_issue")

(define_insn_reservation "n4_integer" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "ialu,ialuX,shift,cmove,compare"))
  "(n4_slot0 | n4_slot1)")

(define_insn_reservation "n4_imul" 12
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "imul"))
  "n4_slot1, nothing*11")

(define_insn_reservation "n4_idiv" 35
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "idiv"))
  "n4_slot1, nothing*34")

(define_insn_reservation "n4_load" 5
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "load,fpload,sload"))
  "(n4_slot0 + n4_load_store), nothing*4")

(define_insn_reservation "n4_store" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "store,fpstore"))
  "(n4_slot0 | n4_slot2) + n4_load_store")

(define_insn_reservation "n4_cti" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "cbcond,uncond_cbcond,branch,call,sibcall,call_no_delay_slot,uncond_branch,return"))
  "n4_slot1")

(define_insn_reservation "n4_fp" 11
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpmove,fpcmove,fpcrmove,fp,fpcmp,fpmul"))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_array" 12
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "array,edge,edgen"))
  "n4_slot1, nothing*11")

(define_insn_reservation "n4_vis_move_1cycle" 1
  (and (eq_attr "cpu" "niagara4")
    (and (eq_attr "type" "vismv")
      (eq_attr "fptype" "double")))
  "n4_slot1")

;; The latency numbers for VIS instructions in the reservations below
;; reflect empirical results, and don't match with the documented
;; latency numbers in the T4 Processor Supplement.  This is because
;; the HW chaps didn't feel it necessary to document the complexity in
;; the PRM, and just assigned a latency of 11 to all/most of the VIS
;; instructions.

(define_insn_reservation "n4_vis_move_11cycle" 11
  (and (eq_attr "cpu" "niagara4")
    (and (eq_attr "type" "vismv")
      (eq_attr "fptype" "single")))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_vis_logical" 3
  (and (eq_attr "cpu" "niagara4")
    (and (eq_attr "type" "visl,pdistn")
      (eq_attr "fptype" "double")))
  "n4_slot1, nothing*2")

(define_insn_reservation "n4_vis_logical_11cycle" 11
  (and (eq_attr "cpu" "niagara4")
    (and (eq_attr "type" "visl")
      (eq_attr "fptype" "single")))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_vis_fga" 11
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fga,gsr"))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_vis_fgm" 11
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fgm_pack,fgm_mul,pdist"))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_fpdivs" 24
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpdivs,fpsqrts"))
  "n4_slot1, nothing*23")

(define_insn_reservation "n4_fpdivd" 37
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpdivd,fpsqrtd"))
  "n4_slot1, nothing*36")
