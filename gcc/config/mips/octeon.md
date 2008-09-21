;;  Octeon pipeline description.
;;  Copyright (C) 2008
;;  Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;;   Copyright (C) 2004, 2005, 2006 Cavium Networks.


;; Octeon is a dual-issue processor that can issue all instructions on
;; pipe0 and a subset on pipe1.

(define_automaton "octeon_main, octeon_mult")

(define_cpu_unit "octeon_pipe0" "octeon_main")
(define_cpu_unit "octeon_pipe1" "octeon_main")
(define_cpu_unit "octeon_mult" "octeon_mult")

(define_insn_reservation "octeon_arith" 1
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "arith,const,logical,move,shift,signext,slt,nop"))
  "octeon_pipe0 | octeon_pipe1")

(define_insn_reservation "octeon_condmove" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "condmove"))
  "octeon_pipe0 | octeon_pipe1")

(define_insn_reservation "octeon_load" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "load,prefetch,mtc,mfc"))
  "octeon_pipe0")

(define_insn_reservation "octeon_store" 1
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "store"))
  "octeon_pipe0")

(define_insn_reservation "octeon_brj" 1
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "branch,jump,call,trap"))
  "octeon_pipe0")

(define_insn_reservation "octeon_imul3" 5
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imul3,pop,clz"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult")

(define_insn_reservation "octeon_imul" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imul,mthilo"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult")

(define_insn_reservation "octeon_mfhilo" 5
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "mfhilo"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult")

(define_insn_reservation "octeon_imadd" 4
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imadd"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult*3")

(define_insn_reservation "octeon_idiv" 72
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "idiv"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult*71")

;; Assume both pipes are needed for unknown and multiple-instruction
;; patterns.

(define_insn_reservation "octeon_unknown" 1
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "unknown,multi"))
  "octeon_pipe0 + octeon_pipe1")
