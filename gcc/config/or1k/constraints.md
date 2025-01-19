;; Constraint definitions for OpenRISC
;; Copyright (C) 2018-2025 Free Software Foundation, Inc.
;; Contributed by Stafford Horne

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

; We use:
;  c - sibcall registers
;  d - double pair base registers (excludes r0, r30 and r31 which overflow)
;  t - got address registers (excludes LR (r9) which is clobbered by set_got)
;  I - constant signed 16-bit
;  K - constant unsigned 16-bit
;  M - constant signed 16-bit shifted left 16-bits (l.movhi)
;  O - constant zero

(define_register_constraint "c" "SIBCALL_REGS"
  "Registers which can hold a sibling call address")

(define_register_constraint "d" "DOUBLE_REGS"
  "Registers which can be used for double reg pairs.")

(define_register_constraint "t" "GOT_REGS"
  "Registers which can be used to store the Global Offset Table (GOT) address.")

;; Immediates
(define_constraint "I"
  "A signed 16-bit immediate in the range -32768 to 32767."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "K"
  "An unsigned 16-bit immediate in the range 0 to 0xffff."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "M"
  "A shifted signed 16-bit constant suitable for l.movhi."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0
		    && (ival >> 31 == -1 || ival >> 31 == 0)")))

(define_constraint "O"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))
