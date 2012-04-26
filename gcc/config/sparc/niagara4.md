;; Scheduling description for Niagara-4
;;   Copyright (C) 2012 Free Software Foundation, Inc.
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

(define_cpu_unit "n4_slot0,n4_slot1" "niagara4_0")
(define_reservation "n4_single_issue" "n4_slot0 + n4_slot1")

(define_insn_reservation "n4_single" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "multi,savew,flushw,iflush,trap,gsr"))
  "n4_single_issue")

(define_insn_reservation "n4_integer" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "ialu,ialuX,shift,cmove,compare"))
  "(n4_slot0 | n4_slot1)")

(define_insn_reservation "n4_imul" 12
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "imul"))
  "(n4_slot0 | n4_slot1), nothing*11")

(define_insn_reservation "n4_idiv" 35
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "idiv"))
  "(n4_slot0 | n4_slot1), nothing*34")

(define_insn_reservation "n4_load" 5
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "load,fpload,sload"))
  "n4_slot0, nothing*4")

(define_insn_reservation "n4_store" 1
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "store,fpstore"))
  "n4_slot0")

(define_insn_reservation "n4_cti" 2
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "branch,call,sibcall,call_no_delay_slot,uncond_branch,return"))
  "n4_slot1, nothing")

(define_insn_reservation "n4_fp" 11
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpmove,fpcmove,fpcrmove,fp,fpcmp,fpmul"))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_array" 12
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "array,edge,edgen"))
  "n4_slot1, nothing*11")

(define_insn_reservation "n4_vis" 11
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fga,fgm_pack,fgm_mul,fgm_pdist"))
  "n4_slot1, nothing*10")

(define_insn_reservation "n4_fpdivs" 24
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpdivs,fpsqrts"))
  "n4_slot1, nothing*23")

(define_insn_reservation "n4_fpdivd" 37
  (and (eq_attr "cpu" "niagara4")
    (eq_attr "type" "fpdivd,fpsqrtd"))
  "n4_slot1, nothing*36")
