;; AArch64 ldp/stp peephole optimizations.
;; Copyright (C) 2014-2016 Free Software Foundation, Inc.
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
	(match_operand:GPI 1 "aarch64_mem_pair_operand" ""))
   (set (match_operand:GPI 2 "register_operand" "")
	(match_operand:GPI 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:GPI 0 "aarch64_mem_pair_operand" "")
	(match_operand:GPI 1 "aarch64_reg_or_zero" ""))
   (set (match_operand:GPI 2 "memory_operand" "")
	(match_operand:GPI 3 "aarch64_reg_or_zero" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[0], &base, &offset_1);
  extract_base_offset_in_addr (operands[2], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:GPF 0 "register_operand" "")
	(match_operand:GPF 1 "aarch64_mem_pair_operand" ""))
   (set (match_operand:GPF 2 "register_operand" "")
	(match_operand:GPF 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:GPF 0 "aarch64_mem_pair_operand" "")
	(match_operand:GPF 1 "aarch64_reg_or_fp_zero" ""))
   (set (match_operand:GPF 2 "memory_operand" "")
	(match_operand:GPF 3 "aarch64_reg_or_fp_zero" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[0], &base, &offset_1);
  extract_base_offset_in_addr (operands[2], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:VD 0 "register_operand" "")
	(match_operand:VD 1 "aarch64_mem_pair_operand" ""))
   (set (match_operand:VD 2 "register_operand" "")
	(match_operand:VD 3 "memory_operand" ""))]
  "aarch64_operands_ok_for_ldpstp (operands, true, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:VD 0 "aarch64_mem_pair_operand" "")
	(match_operand:VD 1 "register_operand" ""))
   (set (match_operand:VD 2 "memory_operand" "")
	(match_operand:VD 3 "register_operand" ""))]
  "TARGET_SIMD && aarch64_operands_ok_for_ldpstp (operands, false, <MODE>mode)"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (match_dup 2) (match_dup 3))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[0], &base, &offset_1);
  extract_base_offset_in_addr (operands[2], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})


;; Handle sign/zero extended consecutive load/store.

(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "aarch64_mem_pair_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "aarch64_operands_ok_for_ldpstp (operands, true, SImode)"
  [(parallel [(set (match_dup 0) (sign_extend:DI (match_dup 1)))
	      (set (match_dup 2) (sign_extend:DI (match_dup 3)))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
})

(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "aarch64_mem_pair_operand" "")))
   (set (match_operand:DI 2 "register_operand" "")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "")))]
  "aarch64_operands_ok_for_ldpstp (operands, true, SImode)"
  [(parallel [(set (match_dup 0) (zero_extend:DI (match_dup 1)))
	      (set (match_dup 2) (zero_extend:DI (match_dup 3)))])]
{
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[1], &base, &offset_1);
  extract_base_offset_in_addr (operands[3], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[0], &base, &offset_1);
  extract_base_offset_in_addr (operands[2], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

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
  rtx base, offset_1, offset_2;

  extract_base_offset_in_addr (operands[0], &base, &offset_1);
  extract_base_offset_in_addr (operands[2], &base, &offset_2);
  if (INTVAL (offset_1) > INTVAL (offset_2))
    {
      std::swap (operands[0], operands[6]);
      std::swap (operands[1], operands[7]);
      std::swap (operands[2], operands[4]);
      std::swap (operands[3], operands[5]);
    }

  if (aarch64_gen_adjusted_ldpstp (operands, false, <MODE>mode, UNKNOWN))
    DONE;
  else
    FAIL;
})
