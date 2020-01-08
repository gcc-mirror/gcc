;; Scheduling description for Tilera TILEPro chip.
;; Copyright (C) 2011-2020 Free Software Foundation, Inc.
;; Contributed by Walter Lee (walt@tilera.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "tile")

; Make the scheduling automaton an ndfa.
(automata_option "ndfa")

; Name the three pipes.
(define_cpu_unit "X0" "tile")
(define_cpu_unit "X1" "tile")
(define_cpu_unit "Y0" "tile")
(define_cpu_unit "Y1" "tile")
(define_cpu_unit "Y2" "tile")

(define_insn_reservation "X0" 1
  (eq_attr "type" "X0")
  "X0")

(define_insn_reservation "X0_2cycle" 2
  (eq_attr "type" "X0_2cycle")
  "X0,nothing")

(define_insn_reservation "X1" 1
  (eq_attr "type" "X1,X1_branch")
  "X1")

(define_insn_reservation "X1_2cycle" 2
  (eq_attr "type" "X1_2cycle")
  "X1,nothing")

(define_insn_reservation "X1_L2" 8
  (eq_attr "type" "X1_L2")
  "X1")

(define_insn_reservation "X1_miss" 80
  (eq_attr "type" "X1_miss")
  "X1")

(define_insn_reservation "X01" 1
  (eq_attr "type" "X01")
  "X0|X1")

(define_insn_reservation "Y0" 1
  (eq_attr "type" "Y0")
  "Y0|X0")

(define_insn_reservation "Y0_2cycle" 2
  (eq_attr "type" "Y0_2cycle")
  "Y0|X0,nothing")

(define_insn_reservation "Y2" 1
  (eq_attr "type" "Y2")
  "Y2|X1")

(define_insn_reservation "Y2_2cycle" 2
  (eq_attr "type" "Y2_2cycle")
  "Y2|X1,nothing")

(define_insn_reservation "Y2_L2" 8
  (eq_attr "type" "Y2_L2")
  "Y2|X1")

(define_insn_reservation "Y2_miss" 80
  (eq_attr "type" "Y2_miss")
  "Y2|X1")

(define_insn_reservation "Y01" 1
  (eq_attr "type" "Y01")
  "Y0|Y1|X0|X1")

(define_insn_reservation "nothing" 0
  (eq_attr "type" "nothing")
  "nothing")

(define_insn_reservation "cannot_bundle" 1
  (eq_attr "type" "cannot_bundle")
  "X0+X1")

(define_insn_reservation "cannot_bundle_3cycle" 3
  (eq_attr "type" "cannot_bundle_3cycle")
  "X0+X1")

(define_insn_reservation "cannot_bundle_4cycle" 4
  (eq_attr "type" "cannot_bundle_4cycle")
  "X0+X1")


; A bundle must be in either X format or Y format.
(exclusion_set "X0,X1" "Y0,Y1,Y2")
