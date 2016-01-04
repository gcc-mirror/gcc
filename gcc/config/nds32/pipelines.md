;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
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

(define_automaton "nds32_machine")

(define_cpu_unit "general_unit" "nds32_machine")

(define_insn_reservation "simple_insn" 1
			 (eq_attr "type" "unknown,load,store,move,alu,compare,branch,call,misc")
			 "general_unit")

;; ------------------------------------------------------------------------
