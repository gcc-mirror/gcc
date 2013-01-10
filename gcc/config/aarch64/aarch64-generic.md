;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Generic scheduler

(define_automaton "aarch64")

(define_cpu_unit "core" "aarch64")

(define_attr "is_load" "yes,no"
  (if_then_else (eq_attr "v8type" "fpsimd_load,fpsimd_load2,load1,load2")
	(const_string "yes")
	(const_string "no")))

(define_insn_reservation "load" 2
  (eq_attr "is_load" "yes")
  "core")

(define_insn_reservation "nonload" 1
  (eq_attr "is_load" "no")
  "core")
