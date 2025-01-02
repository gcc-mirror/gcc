;; AArch64 ldp/stp peephole optimizations.
;; Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

(define_peephole2
  [(set (match_operand:GPI 0 "register_operand" "")
	(match_operand:GPI 1 "memory_operand" ""))
   (set (match_operand:GPI 2 "register_operand" "")
	(match_operand:GPI 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true);
  DONE;
})

(define_peephole2
  [(set (match_operand:GPI 0 "memory_operand" "")
	(match_operand:GPI 1 "aarch64_reg_or_zero" ""))
   (set (match_operand:GPI 2 "memory_operand" "")
	(match_operand:GPI 3 "aarch64_reg_or_zero" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, false);
  DONE;
})

(define_peephole2
  [(set (match_operand:GPF 0 "register_operand" "")
	(match_operand:GPF 1 "memory_operand" ""))
   (set (match_operand:GPF 2 "register_operand" "")
	(match_operand:GPF 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true);
  DONE;
})

(define_peephole2
  [(set (match_operand:GPF 0 "memory_operand" "")
	(match_operand:GPF 1 "aarch64_reg_or_fp_zero" ""))
   (set (match_operand:GPF 2 "memory_operand" "")
	(match_operand:GPF 3 "aarch64_reg_or_fp_zero" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, false);
  DONE;
})

(define_peephole2
  [(set (match_operand:DREG 0 "register_operand" "")
	(match_operand:DREG 1 "memory_operand" ""))
   (set (match_operand:DREG2 2 "register_operand" "")
	(match_operand:DREG2 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true);
  DONE;
})

(define_peephole2
  [(set (match_operand:DREG 0 "memory_operand" "")
	(match_operand:DREG 1 "register_operand" ""))
   (set (match_operand:DREG2 2 "memory_operand" "")
	(match_operand:DREG2 3 "register_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, false);
  DONE;
})

(define_peephole2
  [(set (match_operand:VQ 0 "register_operand" "")
	(match_operand:VQ 1 "memory_operand" ""))
   (set (match_operand:VQ2 2 "register_operand" "")
	(match_operand:VQ2 3 "memory_operand" ""))]
  "TARGET_FLOAT
   && aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true);
  DONE;
})

(define_peephole2
  [(set (match_operand:VQ 0 "memory_operand" "")
	(match_operand:VQ 1 "register_operand" ""))
   (set (match_operand:VQ2 2 "memory_operand" "")
	(match_operand:VQ2 3 "register_operand" ""))]
  "TARGET_FLOAT
   && aarch64_operands_ok_for_ldpstp (operands, false)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, false);
  DONE;
})


;; Handle sign/zero extended consecutive load/store.

(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true, SIGN_EXTEND);
  DONE;
})

(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "aarch64_operands_ok_for_ldpstp (operands, true)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, true, ZERO_EXTEND);
  DONE;
})

;; Handle storing of a floating point zero with integer data.
;; This handles cases like:
;;   struct pair { int a; float b; }
;;
;;   p->a = 1;
;;   p->b = 0.0;
;;
;; We can match modes that won't work for a stp instruction
;; as aarch64_operands_ok_for_ldpstp checks that the modes are
;; compatible.
(define_peephole2
  [(set (match_operand:DSX 0 "memory_operand" "")
	(match_operand:DSX 1 "aarch64_reg_zero_or_fp_zero" ""))
   (set (match_operand:<FCVT_TARGET> 2 "memory_operand" "")
	(match_operand:<FCVT_TARGET> 3 "aarch64_reg_zero_or_fp_zero" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false)"
  [(const_int 0)]
{
  aarch64_finish_ldpstp_peephole (operands, false);
  DONE;
})

;; Handle consecutive load/store whose offset is out of the range
;; supported by ldp/ldpsw/stp.  We firstly adjust offset in a scratch
;; register, then merge them into ldp/ldpsw/stp by using the adjusted
;; offset.

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:GPI 0 "register_operand" "")
	(match_operand:GPI 1 "memory_operand" ""))
   (set (match_operand:GPI 2 "register_operand" "")
	(match_operand:GPI 3 "memory_operand" ""))
   (set (match_operand:GPI 4 "register_operand" "")
	(match_operand:GPI 5 "memory_operand" ""))
   (set (match_operand:GPI 6 "register_operand" "")
	(match_operand:GPI 7 "memory_operand" ""))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, true, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:GPF 0 "register_operand" "")
	(match_operand:GPF 1 "memory_operand" ""))
   (set (match_operand:GPF 2 "register_operand" "")
	(match_operand:GPF 3 "memory_operand" ""))
   (set (match_operand:GPF 4 "register_operand" "")
	(match_operand:GPF 5 "memory_operand" ""))
   (set (match_operand:GPF 6 "register_operand" "")
	(match_operand:GPF 7 "memory_operand" ""))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, true, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "")))
   (set (match_operand:DI 4 "register_operand" "")
	(sign_extend:DI (match_operand:SI 5 "memory_operand" "")))
   (set (match_operand:DI 6 "register_operand" "")
	(sign_extend:DI (match_operand:SI 7 "memory_operand" "")))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, true, SImode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, true, SImode, SIGN_EXTEND))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "memory_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "")))
   (set (match_operand:DI 4 "register_operand" "")
	(zero_extend:DI (match_operand:SI 5 "memory_operand" "")))
   (set (match_operand:DI 6 "register_operand" "")
	(zero_extend:DI (match_operand:SI 7 "memory_operand" "")))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, true, SImode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, true, SImode, ZERO_EXTEND))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:GPI 0 "memory_operand" "")
	(match_operand:GPI 1 "aarch64_reg_or_zero" ""))
   (set (match_operand:GPI 2 "memory_operand" "")
	(match_operand:GPI 3 "aarch64_reg_or_zero" ""))
   (set (match_operand:GPI 4 "memory_operand" "")
	(match_operand:GPI 5 "aarch64_reg_or_zero" ""))
   (set (match_operand:GPI 6 "memory_operand" "")
	(match_operand:GPI 7 "aarch64_reg_or_zero" ""))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, false, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:GPF 0 "memory_operand" "")
	(match_operand:GPF 1 "aarch64_reg_or_fp_zero" ""))
   (set (match_operand:GPF 2 "memory_operand" "")
	(match_operand:GPF 3 "aarch64_reg_or_fp_zero" ""))
   (set (match_operand:GPF 4 "memory_operand" "")
	(match_operand:GPF 5 "aarch64_reg_or_fp_zero" ""))
   (set (match_operand:GPF 6 "memory_operand" "")
	(match_operand:GPF 7 "aarch64_reg_or_fp_zero" ""))
   (match_dup 8)]
  "aarch64_operands_adjust_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, false, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:VP_2E 0 "memory_operand" "")
        (match_operand:VP_2E 1 "aarch64_reg_or_zero" ""))
   (set (match_operand:VP_2E 2 "memory_operand" "")
        (match_operand:VP_2E 3 "aarch64_reg_or_zero" ""))
   (set (match_operand:VP_2E 4 "memory_operand" "")
        (match_operand:VP_2E 5 "aarch64_reg_or_zero" ""))
   (set (match_operand:VP_2E 6 "memory_operand" "")
        (match_operand:VP_2E 7 "aarch64_reg_or_zero" ""))
   (match_dup 8)]
  "TARGET_FLOAT
   && aarch64_operands_adjust_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, false, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:DI 8 "r")
   (set (match_operand:VP_2E 0 "register_operand" "")
        (match_operand:VP_2E 1 "memory_operand" ""))
   (set (match_operand:VP_2E 2 "register_operand" "")
        (match_operand:VP_2E 3 "memory_operand" ""))
   (set (match_operand:VP_2E 4 "register_operand" "")
        (match_operand:VP_2E 5 "memory_operand" ""))
   (set (match_operand:VP_2E 6 "register_operand" "")
        (match_operand:VP_2E 7 "memory_operand" ""))
   (match_dup 8)]
  "TARGET_FLOAT
   && aarch64_operands_adjust_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(const_int 0)]
{
  if (aarch64_gen_adjusted_ldpstp (operands, true, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})
