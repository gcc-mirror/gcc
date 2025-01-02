;; Machine description for T-Head vendor extensions
;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

;; XTheadMemPair: merge two SI or DI loads
(define_peephole2
  [(set (match_operand:GPR 0 "register_operand" "")
	(match_operand:GPR 1 "memory_operand" ""))
   (set (match_operand:GPR 2 "register_operand" "")
	(match_operand:GPR 3 "memory_operand" ""))]
  "TARGET_XTHEADMEMPAIR
  && th_mempair_operands_p (operands, true, <GPR:MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	          (set (match_dup 2) (match_dup 3))])]
{
  th_mempair_order_operands (operands, true, <GPR:MODE>mode);
})

;; XTheadMemPair: merge two SI or DI stores
(define_peephole2
  [(set (match_operand:GPR 0 "memory_operand" "")
	(match_operand:GPR 1 "register_operand" ""))
   (set (match_operand:GPR 2 "memory_operand" "")
	(match_operand:GPR 3 "register_operand" ""))]
  "TARGET_XTHEADMEMPAIR
  && th_mempair_operands_p (operands, false, <GPR:MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
              (set (match_dup 2) (match_dup 3))])]
{
  th_mempair_order_operands (operands, false, <GPR:MODE>mode);
})

;; XTheadMemPair: merge two SI loads with sign-extension
(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "TARGET_XTHEADMEMPAIR && TARGET_64BIT
  && th_mempair_operands_p (operands, true, SImode)"
  [(parallel [(set (match_dup 0) (sign_extend:DI (match_dup 1)))
              (set (match_dup 2) (sign_extend:DI (match_dup 3)))])]
{
  th_mempair_order_operands (operands, true, SImode);
})

;; XTheadMemPair: merge two SI loads with zero-extension
(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "TARGET_XTHEADMEMPAIR && TARGET_64BIT
  && th_mempair_operands_p (operands, true, SImode)"
  [(parallel [(set (match_dup 0) (zero_extend:DI (match_dup 1)))
              (set (match_dup 2) (zero_extend:DI (match_dup 3)))])]
{
  th_mempair_order_operands (operands, true, SImode);
})
