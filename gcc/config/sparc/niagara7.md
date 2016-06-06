;; Scheduling description for Niagara-7
;;   Copyright (C) 2016 Free Software Foundation, Inc.
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

(define_cpu_unit "n7_slot0,n7_slot1,n7_slot2" "niagara7_0")
(define_reservation "n7_single_issue" "n7_slot0 + n7_slot1 + n7_slot2")

(define_cpu_unit "n7_load_store" "niagara7_0")

(define_insn_reservation "n7_single" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "multi,savew,flushw,trap"))
  "n7_single_issue")

(define_insn_reservation "n7_iflush" 27
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "iflush"))
  "(n7_slot0 | n7_slot1), nothing*26")

(define_insn_reservation "n7_integer" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "ialu,ialuX,shift,cmove,compare"))
  "(n7_slot0 | n7_slot1)")

(define_insn_reservation "n7_imul" 12
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "imul"))
  "n7_slot1, nothing*11")

(define_insn_reservation "n7_idiv" 35
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "idiv"))
  "n7_slot1, nothing*34")

(define_insn_reservation "n7_load" 5
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "load,fpload,sload"))
  "(n7_slot0 + n7_load_store), nothing*4")

(define_insn_reservation "n7_store" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "store,fpstore"))
  "(n7_slot0 | n7_slot2) + n7_load_store")

(define_insn_reservation "n7_cti" 1
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "cbcond,uncond_cbcond,branch,call,sibcall,call_no_delay_slot,uncond_branch,return"))
  "n7_slot1")

(define_insn_reservation "n7_fp" 11
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fpmove,fpcmove,fpcrmove,fp,fpcmp,fpmul"))
  "n7_slot1, nothing*10")

(define_insn_reservation "n7_array" 12
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "array,edge,edgen"))
  "n7_slot1, nothing*11")

(define_insn_reservation "n7_fpdivs" 24
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fpdivs,fpsqrts"))
  "n7_slot1, nothing*23")

(define_insn_reservation "n7_fpdivd" 37
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fpdivd,fpsqrtd"))
  "n7_slot1, nothing*36")

(define_insn_reservation "n7_lzd" 12
  (and (eq_attr "cpu" "niagara7")
       (eq_attr "type" "lzd"))
  "(n7_slot0 | n7_slot1), nothing*11")

;; There is an internal unit called the "V3 pipe", that was originally
;; intended to process some of the short cryptographic instructions.
;; However, as soon as in the T4 several of the VIS instructions
;; (notably non-FP instructions) have been moved to the V3 pipe.
;; Consequently, these instructions feature a latency of 3 instead of
;; 11 or 12 cycles, provided their consumers also execute in the V3
;; pipe.
;;
;; This is modelled here with a bypass.

(define_insn_reservation "n7_vis_fga" 11
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fga,gsr"))
  "n7_slot1, nothing*10")

(define_insn_reservation "n7_vis_fgm" 11
  (and (eq_attr "cpu" "niagara7")
    (eq_attr "type" "fgm_pack,fgm_mul,pdist"))
  "n7_slot1, nothing*10")

(define_insn_reservation "n7_vis_move_v3pipe" 11
  (and (eq_attr "cpu" "niagara7")
    (and (eq_attr "type" "vismv")
         (eq_attr "v3pipe" "true")))
  "n7_slot1")

(define_insn_reservation "n7_vis_move_11cycle" 11
  (and (eq_attr "cpu" "niagara7")
    (and (eq_attr "type" "vismv")
         (eq_attr "v3pipe" "false")))
  "n7_slot1, nothing*10")

(define_insn_reservation "n7_vis_logical_v3pipe" 11
  (and (eq_attr "cpu" "niagara7")
    (and (eq_attr "type" "visl,pdistn")
         (eq_attr "v3pipe" "true")))
  "n7_slot1, nothing*2")

(define_insn_reservation "n7_vis_logical_11cycle" 11
  (and (eq_attr "cpu" "niagara7")
    (and (eq_attr "type" "visl")
      (eq_attr "v3pipe" "false")))
  "n7_slot1, nothing*10")

(define_bypass 3 "*_v3pipe" "*_v3pipe")
