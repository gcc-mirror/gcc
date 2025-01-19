;; Scheduling description for Niagara-7
;;   Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

(define_automaton "niagara7_0")

;; The S4 core has a dual-issue queue.  This queue is divided into two
;; slots.  One instruction can be issued each cycle to each slot, and
;; up to 2 instructions are committed each cycle.  Each slot serves
;; several execution units, as depicted below:
;;
;;
;;                 m7_slot0 - Integer unit.
;;                          - Load/Store unit.
;; === QUEUE ==>
;;
;;                 m7_slot1 - Integer unit.
;;                          - Branch unit.
;;                          - Floating-point and graphics unit.
;;                          - 3-cycles crypto unit.

(define_cpu_unit "n7_slot0,n7_slot1" "niagara7_0")

;; Some instructions stall the pipeline and avoid any other
;; instruction to be issued in the same cycle.  We assume the same for
;; multi-instruction insns.

(define_reservation "n7_single_issue" "n7_slot0 + n7_slot1")

(define_insn_reservation "n7_single" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "multi,savew,flushw,trap"))
  "n7_single_issue")

;; Most of the instructions executing in the integer unit have a
;; latency of 1.

(define_insn_reservation "n7_integer" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "ialu,ialuX,shift,cmove,compare"))
  "(n7_slot0 | n7_slot1)")

;; Flushing the instruction memory takes 27 cycles.

(define_insn_reservation "n7_iflush" 27
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "iflush"))
  "(n7_slot0 | n7_slot1), nothing*26")

;; The integer multiplication instructions have a latency of 12 cycles
;; and execute in the integer unit.
;;
;; Likewise for array*, edge* and pdistn instructions.

(define_insn_reservation "n7_imul" 12
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "imul,array,edge,edgen,pdistn"))
  "(n7_slot0 | n7_slot1), nothing*11")

;; The integer division instructions have a latency of 35 cycles and
;; execute in the integer unit.

(define_insn_reservation "n7_idiv" 35
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "idiv"))
  "(n7_slot0 | n7_slot1), nothing*34")

;; Both integer and floating-point load instructions have a latency of
;; 5 cycles, and execute in the slot0.
;;
;; The prefetch instruction also executes in the load/store unit, but
;; its latency is only 1 cycle.

(define_insn_reservation "n7_load" 5
  (and (eq_attr "cpu" "niagara7")
       (ior (eq_attr "type" "fpload,sload")
            (and (eq_attr "type" "load")
                 (eq_attr "subtype" "regular"))))
  "n7_slot0, nothing*4")

(define_insn_reservation "n7_prefetch" 1
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "load")
       (eq_attr "subtype" "prefetch"))
  "n7_slot0")

;; Both integer and floating-point store instructions have a latency
;; of 1 cycle, and execute in the load/store unit in slot0.

(define_insn_reservation "n7_store" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "store,fpstore"))
  "n7_slot0")

;; Control-transfer instructions execute in the Branch Unit in the
;; slot1.

(define_insn_reservation "n7_cti" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "cbcond,uncond_cbcond,branch,call,sibcall,call_no_delay_slot,uncond_branch,return"))
  "n7_slot1")

;; Many instructions executing in the Floating-point and Graphics unit
;; in the slot1 feature a latency of 11 cycles.

(define_insn_reservation "n7_fp" 11
  (and (eq_attr "cpu" "niagara7")
       (ior (eq_attr "type" "fpmove,fpcmove,fpcrmove,fp,fpcmp,fpmul,fgm_pack,fgm_mul,pdist")
            (and (eq_attr "type" "fga")
                 (eq_attr "subtype" "fpu,maxmin"))))
  "n7_slot1, nothing*10")

;; Floating-point division and floating-point square-root instructions
;; have high latencies.  They execute in the floating-point and
;; graphics unit in the slot1.


(define_insn_reservation "n7_fpdivs" 24
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "fpdivs,fpsqrts"))
  "n7_slot1, nothing*23")

(define_insn_reservation "n7_fpdivd" 37
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fpdivd,fpsqrtd"))
  "n7_slot1, nothing*36")

;; SIMD VIS instructions executing in the Floating-point and graphics
;; unit (FPG) in slot1 usually have a latency of either 11 or 12
;; cycles.
;;
;; However, the latency for many instructions is only 3 cycles if the
;; consumer can also be executed in 3 cycles.  We model this with a
;; bypass.  In these cases the instructions are executed in the
;; 3-cycle crypto unit which also serves slot1.

(define_insn_reservation "n7_vis_11cycles" 11
  (and (eq_attr "cpu" "niagara7")
       (ior (and (eq_attr "type" "fga")
                 (eq_attr "subtype" "addsub64,other"))
            (and (eq_attr "type" "vismv")
                 (eq_attr "subtype" "double,single"))
            (and (eq_attr "type" "visl")
                 (eq_attr "subtype" "double,single"))))
  "n7_slot1, nothing*10")

(define_insn_reservation "n7_vis_12cycles" 12
  (and (eq_attr "cpu" "niagara7")
       (ior (eq_attr "type" "bmask,viscmp")
            (and (eq_attr "type" "fga")
                 (eq_attr "subtype" "cmask"))
            (and (eq_attr "type" "vismv")
                 (eq_attr "subtype" "movstouw"))))
  "n7_slot1, nothing*11")

(define_bypass 3 "n7_vis_*" "n7_vis_*")

;; Some other VIS instructions have a latency of 12 cycles, and won't
;; be executed in the 3-cycle crypto pipe.

(define_insn_reservation "n7_lzd" 12
  (and (eq_attr "cpu" "niagara7")
       (ior (eq_attr "type" "lzd,")
            (and (eq_attr "type" "gsr")
                 (eq_attr "subtype" "alignaddr"))))
  "n7_slot1, nothing*11")

;; A couple of VIS instructions feature very low latencies in the M7.

(define_insn_reservation "n7_single_vis" 1
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "vismv")
       (eq_attr "subtype" "movxtod"))
  "n7_slot1")

(define_insn_reservation "n7_double_vis" 2
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "vismv")
       (eq_attr "subtype" "movdtox"))
  "n7_slot1, nothing")

;; Reading and writing to the gsr register takes a high number of
;; cycles that is not documented in the PRM.  Let's use the same value
;; than the M8.

(define_insn_reservation "n7_gsr_reg" 70
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "gsr")
       (eq_attr "subtype" "reg"))
  "n7_slot1, nothing*70")
