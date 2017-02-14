/* Nios II R2 CDX ldwm/stwm/push.h/pop.n instruction patterns.
   This file was automatically generated using nios2-ldstwm.sml.
   Please do not edit manually.

   Copyright (C) 2014-2017 Free Software Foundation, Inc.
   Contributed by Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

(define_insn "*cdx_push_ra_fp"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 8) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 8);
  return "push.n\\t{ra, fp}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 4) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 4);
  return "push.n\\t{ra}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
{
  rtx x = XEXP (operands[3], 0);
  operands[3] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{fp, ra}, %3";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
{
  rtx x = XEXP (operands[2], 0);
  operands[2] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{ra}, %2";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 12) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 12);
  return "push.n\\t{ra, fp, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 8) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 8);
  return "push.n\\t{ra, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 4 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
{
  rtx x = XEXP (operands[4], 0);
  operands[4] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, fp, ra}, %4";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 3 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
{
  rtx x = XEXP (operands[3], 0);
  operands[3] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, ra}, %3";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 16) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 16);
  return "push.n\\t{ra, fp, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 12) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 12);
  return "push.n\\t{ra, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 5 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
{
  rtx x = XEXP (operands[5], 0);
  operands[5] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, fp, ra}, %5";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 4 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
{
  rtx x = XEXP (operands[4], 0);
  operands[4] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, ra}, %4";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 20) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 20);
  return "push.n\\t{ra, fp, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 16) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 16);
  return "push.n\\t{ra, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 6 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
{
  rtx x = XEXP (operands[6], 0);
  operands[6] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, fp, ra}, %6";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 5 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
{
  rtx x = XEXP (operands[5], 0);
  operands[5] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, ra}, %5";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 24) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 24);
  return "push.n\\t{ra, fp, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 20) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 20);
  return "push.n\\t{ra, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_r19_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 7 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
{
  rtx x = XEXP (operands[7], 0);
  operands[7] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, fp, ra}, %7";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_r19_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 6 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
{
  rtx x = XEXP (operands[6], 0);
  operands[6] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, ra}, %6";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 28) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 28);
  return "push.n\\t{ra, fp, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 24) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 24);
  return "push.n\\t{ra, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 8 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
{
  rtx x = XEXP (operands[8], 0);
  operands[8] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, fp, ra}, %8";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 7 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
{
  rtx x = XEXP (operands[7], 0);
  operands[7] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, ra}, %7";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -32))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 32) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 32);
  return "push.n\\t{ra, fp, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 28) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 28);
  return "push.n\\t{ra, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 8 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 9 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
{
  rtx x = XEXP (operands[9], 0);
  operands[9] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, fp, ra}, %9";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 8 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
{
  rtx x = XEXP (operands[8], 0);
  operands[8] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, ra}, %8";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r22_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 22))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -32))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -36))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 36) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 36);
  return "push.n\\t{ra, fp, r22, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r22_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 22))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -32))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 32) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 32);
  return "push.n\\t{ra, r22, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_r22_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 22) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 8 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 9 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 10 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
{
  rtx x = XEXP (operands[10], 0);
  operands[10] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, r22, fp, ra}, %10";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_r22_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 22) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 8 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 9 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
{
  rtx x = XEXP (operands[9], 0);
  operands[9] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, r22, ra}, %9";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_push_ra_fp_r23_r22_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI FP_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 23))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 22))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -32))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -36))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -40))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 40) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 40);
  return "push.n\\t{ra, fp, r23, r22, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_push_ra_r23_r22_r21_r20_r19_r18_r17_r16"
  [(match_parallel 0 ""
    [(set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -4))) (reg:SI RA_REGNO))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -8))) (reg:SI 23))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -12))) (reg:SI 22))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -16))) (reg:SI 21))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -20))) (reg:SI 20))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -24))) (reg:SI 19))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -28))) (reg:SI 18))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -32))) (reg:SI 17))
     (set (mem:SI (plus:SI (reg:SI SP_REGNO) (const_int -36))) (reg:SI 16))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10
    && (-INTVAL (operands[1]) & 3) == 0
    && (-INTVAL (operands[1]) - 36) <= 60"
{
  operands[2] = GEN_INT (-INTVAL (operands[1]) - 36);
  return "push.n\\t{ra, r23, r22, r21, r20, r19, r18, r17, r16}, %2";
}
  [(set_attr "type" "push")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_r22_r23_fp_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI FP_REGNO) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 23) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 22) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 8 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 9 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 10 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 11 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
{
  rtx x = XEXP (operands[11], 0);
  operands[11] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, r22, r23, fp, ra}, %11";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_pop_r16_r17_r18_r19_r20_r21_r22_r23_ra"
  [(match_parallel 0 "pop_operation"
    [(return)
     (set (reg:SI SP_REGNO)
          (plus:SI (reg:SI SP_REGNO) (match_operand 1 "const_int_operand" "")))
     (set (reg:SI RA_REGNO) (match_operand:SI 2 "stack_memory_operand" ""))
     (set (reg:SI 23) (match_operand:SI 3 "stack_memory_operand" ""))
     (set (reg:SI 22) (match_operand:SI 4 "stack_memory_operand" ""))
     (set (reg:SI 21) (match_operand:SI 5 "stack_memory_operand" ""))
     (set (reg:SI 20) (match_operand:SI 6 "stack_memory_operand" ""))
     (set (reg:SI 19) (match_operand:SI 7 "stack_memory_operand" ""))
     (set (reg:SI 18) (match_operand:SI 8 "stack_memory_operand" ""))
     (set (reg:SI 17) (match_operand:SI 9 "stack_memory_operand" ""))
     (set (reg:SI 16) (match_operand:SI 10 "stack_memory_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
{
  rtx x = XEXP (operands[10], 0);
  operands[10] = REG_P (x) ? const0_rtx : XEXP (x, 1);
  return "pop.n\\t{r16, r17, r18, r19, r20, r21, r22, r23, ra}, %10";
}
  [(set_attr "type" "pop")])

(define_insn "*cdx_ldwm1_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int 4)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 2)))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1}, (%2)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int 4)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 2)))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1}, (%2)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 2 "register_operand" "r")))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1}, (%2)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 2 "register_operand" "r")))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 1"
   "ldwm\\t{%1}, (%2)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int -4)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 2) (const_int -4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1}, --(%2), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int -4)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 2) (const_int -4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1}, --(%2), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 2 "register_operand" "r") (const_int -4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1}, --(%2), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm1_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 2 "register_operand" "r") (const_int -4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 1"
   "ldwm\\t{%1}, --(%2)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 3)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int 4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2}, (%3)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 3)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int 4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2}, (%3)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 3 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int 4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2}, (%3)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 3 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int 4))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1, %2}, (%3)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2}, --(%3), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2}, --(%3), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 3 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2}, --(%3), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm2_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 3 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 3) (const_int -8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "ldwm\\t{%1, %2}, --(%3)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 4)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3}, (%4)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 4)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3}, (%4)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 4 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3}, (%4)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 4 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int 8))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2, %3}, (%4)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3}, --(%4), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3}, --(%4), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 4 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3}, --(%4), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm3_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 4 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 4) (const_int -12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "ldwm\\t{%1, %2, %3}, --(%4)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 5)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4}, (%5)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 5)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4}, (%5)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 5 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4}, (%5)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 5 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int 12))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3, %4}, (%5)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4}, --(%5), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4}, --(%5), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 5 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4}, --(%5), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm4_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 5 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 5) (const_int -16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "ldwm\\t{%1, %2, %3, %4}, --(%5)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int 20)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 6)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5}, (%6)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int 20)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 6)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5}, (%6)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 6 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5}, (%6)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 6 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int 16))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4, %5}, (%6)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int -20)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5}, --(%6), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int -20)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5}, --(%6), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 6 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5}, --(%6), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm5_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 6 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 6) (const_int -20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "ldwm\\t{%1, %2, %3, %4, %5}, --(%6)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int 24)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 7)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int 24)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 7)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 7 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 7 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int 20))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int -24)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, --(%7), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int -24)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, --(%7), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 7 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, --(%7), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm6_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 7 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 7) (const_int -24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "ldwm\\t{%1, %2, %3, %4, %5, %6}, --(%7)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int 28)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 8)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int 28)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 8)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 8 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 8 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int 24))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int -28)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int -28)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 8 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm7_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 8 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 8) (const_int -28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int 32)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 9)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int 32)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 9)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 9 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 9 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int 28))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int -32)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int -32)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 9 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm8_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 9 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 9) (const_int -32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int 36)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 10)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int 36)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 10)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 10 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 10 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int 32))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int -36)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int -36)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 10 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm9_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 10 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 10) (const_int -36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int 40)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 11)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int 40)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 11)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 11 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 11 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int 36))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int -40)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int -40)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 11 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm10_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 11 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 11) (const_int -40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int 44)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 12)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int 44)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 12)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 12 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 12 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int 40))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int -44)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int -44)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 12 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm11_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 12 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 12) (const_int -44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_inc_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int 48)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 13)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 40))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 14"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++, writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_inc_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int 48)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_dup 13)))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 40))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++, writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_inc_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 13 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 40))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_inc"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (match_operand:SI 13 "register_operand" "r")))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 4))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 8))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 12))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 16))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 20))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 24))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 28))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 32))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 36))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 40))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int 44))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_dec_wb_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int -48)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -44))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -48))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 14"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13), writeback, ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_dec_wb"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int -48)))
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -44))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -48))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13), writeback"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_dec_ret"
  [(match_parallel 0 "ldwm_operation"
    [(return)
     (set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 13 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -44))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -48))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13), ret"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_ldwm12_dec"
  [(match_parallel 0 "ldwm_operation"
    [(set (match_operand:SI 1 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 13 "register_operand" "r") (const_int -4))))
     (set (match_operand:SI 2 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -8))))
     (set (match_operand:SI 3 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -12))))
     (set (match_operand:SI 4 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -16))))
     (set (match_operand:SI 5 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -20))))
     (set (match_operand:SI 6 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -24))))
     (set (match_operand:SI 7 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -28))))
     (set (match_operand:SI 8 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -32))))
     (set (match_operand:SI 9 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -36))))
     (set (match_operand:SI 10 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -40))))
     (set (match_operand:SI 11 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -44))))
     (set (match_operand:SI 12 "nios2_hard_register_operand" "")
          (mem:SI (plus:SI (match_dup 13) (const_int -48))))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "ldwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13)"
  [(set_attr "type" "ldwm")])

(define_insn "*cdx_stwm1_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int 4)))
     (set (mem:SI (match_dup 2))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "stwm\\t{%1}, (%2)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm1_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 2 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 1"
   "stwm\\t{%1}, (%2)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm1_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 2 "register_operand" "+&r")
          (plus:SI (match_dup 2) (const_int -4)))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "stwm\\t{%1}, --(%2), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm1_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 2 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 1"
   "stwm\\t{%1}, --(%2)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm2_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (mem:SI (match_dup 3))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "stwm\\t{%1, %2}, (%3)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm2_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 3 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "stwm\\t{%1, %2}, (%3)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm2_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 3 "register_operand" "+&r")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "stwm\\t{%1, %2}, --(%3), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm2_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 3 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 2"
   "stwm\\t{%1, %2}, --(%3)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm3_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (mem:SI (match_dup 4))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "stwm\\t{%1, %2, %3}, (%4)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm3_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 4 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "stwm\\t{%1, %2, %3}, (%4)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm3_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 4 "register_operand" "+&r")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "stwm\\t{%1, %2, %3}, --(%4), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm3_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 4 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 3"
   "stwm\\t{%1, %2, %3}, --(%4)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm4_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (mem:SI (match_dup 5))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "stwm\\t{%1, %2, %3, %4}, (%5)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm4_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 5 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "stwm\\t{%1, %2, %3, %4}, (%5)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm4_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 5 "register_operand" "+&r")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "stwm\\t{%1, %2, %3, %4}, --(%5), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm4_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 5 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 4"
   "stwm\\t{%1, %2, %3, %4}, --(%5)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm5_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int 20)))
     (set (mem:SI (match_dup 6))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "stwm\\t{%1, %2, %3, %4, %5}, (%6)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm5_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 6 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "stwm\\t{%1, %2, %3, %4, %5}, (%6)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm5_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 6 "register_operand" "+&r")
          (plus:SI (match_dup 6) (const_int -20)))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "stwm\\t{%1, %2, %3, %4, %5}, --(%6), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm5_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 6 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 6) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 5"
   "stwm\\t{%1, %2, %3, %4, %5}, --(%6)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm6_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int 24)))
     (set (mem:SI (match_dup 7))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "stwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm6_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 7 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "stwm\\t{%1, %2, %3, %4, %5, %6}, (%7)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm6_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 7 "register_operand" "+&r")
          (plus:SI (match_dup 7) (const_int -24)))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "stwm\\t{%1, %2, %3, %4, %5, %6}, --(%7), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm6_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 7 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 7) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 6"
   "stwm\\t{%1, %2, %3, %4, %5, %6}, --(%7)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm7_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int 28)))
     (set (mem:SI (match_dup 8))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm7_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 8 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7}, (%8)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm7_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 8 "register_operand" "+&r")
          (plus:SI (match_dup 8) (const_int -28)))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm7_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 8 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 8) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 7"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7}, --(%8)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm8_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int 32)))
     (set (mem:SI (match_dup 9))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm8_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 9 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, (%9)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm8_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 9 "register_operand" "+&r")
          (plus:SI (match_dup 9) (const_int -32)))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm8_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 9 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 9) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 8"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8}, --(%9)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm9_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int 36)))
     (set (mem:SI (match_dup 10))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm9_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 10 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, (%10)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm9_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 10 "register_operand" "+&r")
          (plus:SI (match_dup 10) (const_int -36)))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm9_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 10 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 10) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 9"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9}, --(%10)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm10_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int 40)))
     (set (mem:SI (match_dup 11))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm10_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 11 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, (%11)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm10_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 11 "register_operand" "+&r")
          (plus:SI (match_dup 11) (const_int -40)))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm10_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 11 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 11) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 10"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10}, --(%11)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm11_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int 44)))
     (set (mem:SI (match_dup 12))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 40)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm11_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 12 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int 40)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, (%12)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm11_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 12 "register_operand" "+&r")
          (plus:SI (match_dup 12) (const_int -44)))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -44)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm11_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 12 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 12) (const_int -44)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 11"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11}, --(%12)"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm12_inc_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int 48)))
     (set (mem:SI (match_dup 13))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 40)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 44)))
          (match_operand:SI 12 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++, writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm12_inc"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (match_operand:SI 13 "register_operand" "r"))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 4)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 8)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 12)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 16)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 20)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 24)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 28)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 32)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 36)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 40)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int 44)))
          (match_operand:SI 12 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, (%13)++"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm12_dec_wb"
  [(match_parallel 0 "stwm_operation"
    [(set (match_operand:SI 13 "register_operand" "+&r")
          (plus:SI (match_dup 13) (const_int -48)))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -44)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -48)))
          (match_operand:SI 12 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 13"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13), writeback"
  [(set_attr "type" "stwm")])

(define_insn "*cdx_stwm12_dec"
  [(match_parallel 0 "stwm_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 13 "register_operand" "r") (const_int -4)))
          (match_operand:SI 1 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -8)))
          (match_operand:SI 2 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -12)))
          (match_operand:SI 3 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -16)))
          (match_operand:SI 4 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -20)))
          (match_operand:SI 5 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -24)))
          (match_operand:SI 6 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -28)))
          (match_operand:SI 7 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -32)))
          (match_operand:SI 8 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -36)))
          (match_operand:SI 9 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -40)))
          (match_operand:SI 10 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -44)))
          (match_operand:SI 11 "nios2_hard_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 13) (const_int -48)))
          (match_operand:SI 12 "nios2_hard_register_operand" ""))])]
   "TARGET_HAS_CDX && XVECLEN (operands[0], 0) == 12"
   "stwm\\t{%1, %2, %3, %4, %5, %6, %7, %8, %9, %10, %11, %12}, --(%13)"
  [(set_attr "type" "stwm")])

(define_peephole2
  [(match_scratch:SI 24 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 14 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 15 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 16 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 17 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 18 "memory_operand" ""))
   (set (match_operand:SI 7 "register_operand" "")
        (match_operand:SI 19 "memory_operand" ""))
   (set (match_operand:SI 8 "register_operand" "")
        (match_operand:SI 20 "memory_operand" ""))
   (set (match_operand:SI 9 "register_operand" "")
        (match_operand:SI 21 "memory_operand" ""))
   (set (match_operand:SI 10 "register_operand" "")
        (match_operand:SI 22 "memory_operand" ""))
   (set (match_operand:SI 11 "register_operand" "")
        (match_operand:SI 23 "memory_operand" ""))
   (match_dup 24)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 12, operands[24], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 22 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 14 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 15 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 16 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 17 "memory_operand" ""))
   (set (match_operand:SI 7 "register_operand" "")
        (match_operand:SI 18 "memory_operand" ""))
   (set (match_operand:SI 8 "register_operand" "")
        (match_operand:SI 19 "memory_operand" ""))
   (set (match_operand:SI 9 "register_operand" "")
        (match_operand:SI 20 "memory_operand" ""))
   (set (match_operand:SI 10 "register_operand" "")
        (match_operand:SI 21 "memory_operand" ""))
   (match_dup 22)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 11, operands[22], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 20 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 10 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 14 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 15 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 16 "memory_operand" ""))
   (set (match_operand:SI 7 "register_operand" "")
        (match_operand:SI 17 "memory_operand" ""))
   (set (match_operand:SI 8 "register_operand" "")
        (match_operand:SI 18 "memory_operand" ""))
   (set (match_operand:SI 9 "register_operand" "")
        (match_operand:SI 19 "memory_operand" ""))
   (match_dup 20)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 10, operands[20], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 18 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 9 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 10 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 14 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 15 "memory_operand" ""))
   (set (match_operand:SI 7 "register_operand" "")
        (match_operand:SI 16 "memory_operand" ""))
   (set (match_operand:SI 8 "register_operand" "")
        (match_operand:SI 17 "memory_operand" ""))
   (match_dup 18)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 9, operands[18], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 16 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 8 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 9 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 10 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 14 "memory_operand" ""))
   (set (match_operand:SI 7 "register_operand" "")
        (match_operand:SI 15 "memory_operand" ""))
   (match_dup 16)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 8, operands[16], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 14 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 7 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 8 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 9 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 10 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 12 "memory_operand" ""))
   (set (match_operand:SI 6 "register_operand" "")
        (match_operand:SI 13 "memory_operand" ""))
   (match_dup 14)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 7, operands[14], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 12 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 6 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 7 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 8 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 9 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 10 "memory_operand" ""))
   (set (match_operand:SI 5 "register_operand" "")
        (match_operand:SI 11 "memory_operand" ""))
   (match_dup 12)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 6, operands[12], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 10 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 5 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 6 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 7 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 8 "memory_operand" ""))
   (set (match_operand:SI 4 "register_operand" "")
        (match_operand:SI 9 "memory_operand" ""))
   (match_dup 10)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 5, operands[10], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 8 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 4 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 5 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 6 "memory_operand" ""))
   (set (match_operand:SI 3 "register_operand" "")
        (match_operand:SI 7 "memory_operand" ""))
   (match_dup 8)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 4, operands[8], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 6 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 4 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 5 "memory_operand" ""))
   (match_dup 6)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 3, operands[6], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 4 "r")
   (set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (match_dup 4)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (true, 2, operands[4], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 24 "r")
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 14 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 15 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 16 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 17 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 18 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (set (match_operand:SI 19 "memory_operand" "")
        (match_operand:SI 7 "register_operand" ""))
   (set (match_operand:SI 20 "memory_operand" "")
        (match_operand:SI 8 "register_operand" ""))
   (set (match_operand:SI 21 "memory_operand" "")
        (match_operand:SI 9 "register_operand" ""))
   (set (match_operand:SI 22 "memory_operand" "")
        (match_operand:SI 10 "register_operand" ""))
   (set (match_operand:SI 23 "memory_operand" "")
        (match_operand:SI 11 "register_operand" ""))
   (match_dup 24)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 12, operands[24], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 22 "r")
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 14 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 15 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 16 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 17 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (set (match_operand:SI 18 "memory_operand" "")
        (match_operand:SI 7 "register_operand" ""))
   (set (match_operand:SI 19 "memory_operand" "")
        (match_operand:SI 8 "register_operand" ""))
   (set (match_operand:SI 20 "memory_operand" "")
        (match_operand:SI 9 "register_operand" ""))
   (set (match_operand:SI 21 "memory_operand" "")
        (match_operand:SI 10 "register_operand" ""))
   (match_dup 22)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 11, operands[22], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 20 "r")
   (set (match_operand:SI 10 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 14 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 15 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 16 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (set (match_operand:SI 17 "memory_operand" "")
        (match_operand:SI 7 "register_operand" ""))
   (set (match_operand:SI 18 "memory_operand" "")
        (match_operand:SI 8 "register_operand" ""))
   (set (match_operand:SI 19 "memory_operand" "")
        (match_operand:SI 9 "register_operand" ""))
   (match_dup 20)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 10, operands[20], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 18 "r")
   (set (match_operand:SI 9 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 10 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 14 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 15 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (set (match_operand:SI 16 "memory_operand" "")
        (match_operand:SI 7 "register_operand" ""))
   (set (match_operand:SI 17 "memory_operand" "")
        (match_operand:SI 8 "register_operand" ""))
   (match_dup 18)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 9, operands[18], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 16 "r")
   (set (match_operand:SI 8 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 9 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 10 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 14 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (set (match_operand:SI 15 "memory_operand" "")
        (match_operand:SI 7 "register_operand" ""))
   (match_dup 16)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 8, operands[16], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 14 "r")
   (set (match_operand:SI 7 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 8 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 9 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 10 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 12 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (set (match_operand:SI 13 "memory_operand" "")
        (match_operand:SI 6 "register_operand" ""))
   (match_dup 14)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 7, operands[14], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 12 "r")
   (set (match_operand:SI 6 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 8 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 9 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 10 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (set (match_operand:SI 11 "memory_operand" "")
        (match_operand:SI 5 "register_operand" ""))
   (match_dup 12)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 6, operands[12], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 10 "r")
   (set (match_operand:SI 5 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 6 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 8 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (set (match_operand:SI 9 "memory_operand" "")
        (match_operand:SI 4 "register_operand" ""))
   (match_dup 10)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 5, operands[10], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 8 "r")
   (set (match_operand:SI 4 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 6 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))
   (match_dup 8)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 4, operands[8], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 6 "r")
   (set (match_operand:SI 3 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_operand:SI 2 "register_operand" ""))
   (match_dup 6)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 3, operands[6], operands))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(match_scratch:SI 4 "r")
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 0 "register_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (match_dup 4)]
  "TARGET_HAS_CDX"
  [(const_int 0)]
{
  if (gen_ldstwm_peep (false, 2, operands[4], operands))
    DONE;
  else
    FAIL;
})

