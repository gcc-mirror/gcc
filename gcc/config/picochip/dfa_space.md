;; GCC machine description for picochip
;; Copyright (C) 2008 Free Software Foundation, Inc.
;; Contributed by picoChip Designs Ltd (http://www.picochip.com)
;; Maintained by Daniel Towner (dant@picochip.com) and Hariharan
;; Sandanagobalane (hariharan@picochip.com)
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
;; along with GCC; see the file COPYING3.  If not, see
;; <http://www.gnu.org/licenses/>.

;; The following DFA description schedules instructions for space.  The
;; schedule seeks to avoid stall cycles (e.g., memory load), but the
;; instructions are not VLIW packed (whenever instructions are packed
;; together, an additional byte is used to denote this, which
;; increases the code size).

;; No special handling of the long constants is necessary (as in
;; dfa_speed.md), since VLIW packing is not used.

;; Memory instructions stall for one cycle.  All other instructions
;; complete ready for the next cycle.

(define_insn_reservation "nonStallInsn" 1
  (and (eq_attr "schedType" "space")
       (eq_attr "type" "!mem"))
  "slot0+slot1+slot2")

(define_insn_reservation "stallInsn" 2
  (and (eq_attr "schedType" "space")
       (eq_attr "type" "mem"))
  "slot0+slot1+slot2")
