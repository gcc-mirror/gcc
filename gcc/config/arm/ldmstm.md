/* ARM ldm/stm instruction patterns.  This file was automatically generated
   using arm-ldmstm.ml.  Please do not edit manually.

   Copyright (C) 2010-2016 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

(define_insn "*ldm4_"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_operand:SI 5 "s_register_operand" "rk")))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "ldm%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm4_ia"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_operand:SI 5 "s_register_operand" "l")))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 4))))
     (set (match_operand:SI 3 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 4 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 4"
  "ldmia\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")])

(define_insn "*ldm4_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 5)))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 5"
  "ldmia%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm4_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&l")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_dup 5)))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 4))))
     (set (match_operand:SI 3 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 4 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 5"
  "ldmia\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")])

(define_insn "*stm4_"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 5 "s_register_operand" "rk"))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "stm%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm4_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (mem:SI (match_dup 5))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 5"
  "stmia%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_stm4_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&l")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (mem:SI (match_dup 5))
          (match_operand:SI 1 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 2 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 3 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 4 "low_register_operand" ""))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 5"
  "stmia\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")])

(define_insn "*ldm4_ib"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk")
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 16))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "ldmib%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")])

(define_insn "*ldm4_ib_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 12))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int 16))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 5"
  "ldmib%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")])

(define_insn "*stm4_ib"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk") (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 16)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "stmib%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")])

(define_insn "*stm4_ib_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int 16)))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 12)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int 16)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 5"
  "stmib%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")])

(define_insn "*ldm4_da"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk")
                  (const_int -12))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -4))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 5)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "ldmda%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")])

(define_insn "*ldm4_da_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -12))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -4))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 5)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 5"
  "ldmda%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")])

(define_insn "*stm4_da"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk") (const_int -12)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -4)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 5))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "stmda%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")])

(define_insn "*stm4_da_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -12)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -4)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 5))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 5"
  "stmda%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")])

(define_insn "*ldm4_db"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk")
                  (const_int -16))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -12))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -8))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "ldmdb%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*ldm4_db_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -16))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -12))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -8))))
     (set (match_operand:SI 4 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 5)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 5"
  "ldmdb%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm4_db"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 5 "s_register_operand" "rk") (const_int -16)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -12)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -4)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "stmdb%?\t%5, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm4_db_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 5 "s_register_operand" "+&rk")
          (plus:SI (match_dup 5) (const_int -16)))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -16)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -12)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 5) (const_int -4)))
          (match_operand:SI 4 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 5"
  "stmdb%?\t%5!, {%1, %2, %3, %4}"
  [(set_attr "type" "store4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 4 "memory_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 5 "memory_operand" ""))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 6 "memory_operand" ""))
   (set (match_operand:SI 3 "s_register_operand" "")
        (match_operand:SI 7 "memory_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_ldm_seq (operands, 4, false))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 4 "memory_operand" ""))
   (parallel
    [(set (match_operand:SI 1 "s_register_operand" "")
          (match_operand:SI 5 "memory_operand" ""))
     (set (match_operand:SI 2 "s_register_operand" "")
          (match_operand:SI 6 "memory_operand" ""))
     (set (match_operand:SI 3 "s_register_operand" "")
          (match_operand:SI 7 "memory_operand" ""))])]
  ""
  [(const_int 0)]
{
  if (gen_ldm_seq (operands, 4, false))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 8 "const_int_operand" ""))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 9 "const_int_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_dup 1))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 10 "const_int_operand" ""))
   (set (match_operand:SI 6 "memory_operand" "")
        (match_dup 2))
   (set (match_operand:SI 3 "s_register_operand" "")
        (match_operand:SI 11 "const_int_operand" ""))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_dup 3))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 4))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 8 "const_int_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 9 "const_int_operand" ""))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 10 "const_int_operand" ""))
   (set (match_operand:SI 3 "s_register_operand" "")
        (match_operand:SI 11 "const_int_operand" ""))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_dup 1))
   (set (match_operand:SI 6 "memory_operand" "")
        (match_dup 2))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_dup 3))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 4))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 4 "memory_operand" "")
        (match_operand:SI 0 "s_register_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_operand:SI 1 "s_register_operand" ""))
   (set (match_operand:SI 6 "memory_operand" "")
        (match_operand:SI 2 "s_register_operand" ""))
   (set (match_operand:SI 7 "memory_operand" "")
        (match_operand:SI 3 "s_register_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_stm_seq (operands, 4))
    DONE;
  else
    FAIL;
})

(define_insn "*ldm3_"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_operand:SI 4 "s_register_operand" "rk")))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "ldm%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm3_ia"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_operand:SI 4 "s_register_operand" "l")))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 4))))
     (set (match_operand:SI 3 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 3"
  "ldmia\t%4, {%1, %2, %3}"
  [(set_attr "type" "load3")])

(define_insn "*ldm3_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 4)))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "ldmia%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm3_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&l")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_dup 4)))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 4))))
     (set (match_operand:SI 3 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 4"
  "ldmia\t%4!, {%1, %2, %3}"
  [(set_attr "type" "load3")])

(define_insn "*stm3_"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 4 "s_register_operand" "rk"))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "stm%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm3_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (mem:SI (match_dup 4))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "stmia%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_stm3_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&l")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (mem:SI (match_dup 4))
          (match_operand:SI 1 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 2 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 3 "low_register_operand" ""))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 4"
  "stmia\t%4!, {%1, %2, %3}"
  [(set_attr "type" "store3")])

(define_insn "*ldm3_ib"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk")
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 12))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "ldmib%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")])

(define_insn "*ldm3_ib_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int 12))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "ldmib%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")])

(define_insn "*stm3_ib"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk") (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 12)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "stmib%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")])

(define_insn "*stm3_ib_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int 12)))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int 12)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "stmib%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")])

(define_insn "*ldm3_da"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk")
                  (const_int -8))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 4)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "ldmda%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")])

(define_insn "*ldm3_da_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -8))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -4))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 4)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "ldmda%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")])

(define_insn "*stm3_da"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk") (const_int -8)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 4))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "stmda%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")])

(define_insn "*stm3_da_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -8)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 4))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 4"
  "stmda%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")])

(define_insn "*ldm3_db"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk")
                  (const_int -12))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "ldmdb%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*ldm3_db_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -12))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -8))))
     (set (match_operand:SI 3 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 4)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "ldmdb%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "load3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm3_db"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 4 "s_register_operand" "rk") (const_int -12)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -4)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "stmdb%?\t%4, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm3_db_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 4 "s_register_operand" "+&rk")
          (plus:SI (match_dup 4) (const_int -12)))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -12)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 4) (const_int -4)))
          (match_operand:SI 3 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 4"
  "stmdb%?\t%4!, {%1, %2, %3}"
  [(set_attr "type" "store3")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 4 "memory_operand" ""))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 5 "memory_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_ldm_seq (operands, 3, false))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (parallel
    [(set (match_operand:SI 1 "s_register_operand" "")
          (match_operand:SI 4 "memory_operand" ""))
     (set (match_operand:SI 2 "s_register_operand" "")
          (match_operand:SI 5 "memory_operand" ""))])]
  ""
  [(const_int 0)]
{
  if (gen_ldm_seq (operands, 3, false))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 6 "const_int_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 7 "const_int_operand" ""))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_dup 1))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 8 "const_int_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_dup 2))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 3))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 6 "const_int_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 7 "const_int_operand" ""))
   (set (match_operand:SI 2 "s_register_operand" "")
        (match_operand:SI 8 "const_int_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_dup 1))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_dup 2))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 3))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 3 "memory_operand" "")
        (match_operand:SI 0 "s_register_operand" ""))
   (set (match_operand:SI 4 "memory_operand" "")
        (match_operand:SI 1 "s_register_operand" ""))
   (set (match_operand:SI 5 "memory_operand" "")
        (match_operand:SI 2 "s_register_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_stm_seq (operands, 3))
    DONE;
  else
    FAIL;
})

(define_insn "*ldm2_"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_operand:SI 3 "s_register_operand" "rk")))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 2"
  "ldm%?\t%3, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm2_ia"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_operand:SI 3 "s_register_operand" "l")))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 4))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 2"
  "ldmia\t%3, {%1, %2}"
  [(set_attr "type" "load2")])

(define_insn "*ldm2_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 3)))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "ldmia%?\t%3!, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_ldm2_ia_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&l")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (match_operand:SI 1 "low_register_operand" "")
          (mem:SI (match_dup 3)))
     (set (match_operand:SI 2 "low_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 4))))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 3"
  "ldmia\t%3!, {%1, %2}"
  [(set_attr "type" "load2")])

(define_insn "*stm2_"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 3 "s_register_operand" "rk"))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 2"
  "stm%?\t%3, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm2_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (mem:SI (match_dup 3))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "stmia%?\t%3!, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb_stm2_ia_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&l")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (mem:SI (match_dup 3))
          (match_operand:SI 1 "low_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 2 "low_register_operand" ""))])]
  "TARGET_THUMB1 && XVECLEN (operands[0], 0) == 3"
  "stmia\t%3!, {%1, %2}"
  [(set_attr "type" "store2")])

(define_insn "*ldm2_ib"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk")
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 8))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 2"
  "ldmib%?\t%3, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")])

(define_insn "*ldm2_ib_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int 8))))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "ldmib%?\t%3!, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")])

(define_insn "*stm2_ib"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk") (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 2"
  "stmib%?\t%3, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")])

(define_insn "*stm2_ib_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int 8)))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int 8)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "stmib%?\t%3!, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")])

(define_insn "*ldm2_da"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk")
                  (const_int -4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 3)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 2"
  "ldmda%?\t%3, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")])

(define_insn "*ldm2_da_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int -4))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (match_dup 3)))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "ldmda%?\t%3!, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")])

(define_insn "*stm2_da"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk") (const_int -4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 3))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 2"
  "stmda%?\t%3, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")])

(define_insn "*stm2_da_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -4)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (match_dup 3))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_ARM && XVECLEN (operands[0], 0) == 3"
  "stmda%?\t%3!, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")])

(define_insn "*ldm2_db"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk")
                  (const_int -8))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 2"
  "ldmdb%?\t%3, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*ldm2_db_update"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (match_operand:SI 1 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int -8))))
     (set (match_operand:SI 2 "arm_hard_general_register_operand" "")
          (mem:SI (plus:SI (match_dup 3)
                  (const_int -4))))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "ldmdb%?\t%3!, {%1, %2}"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm2_db"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 3 "s_register_operand" "rk") (const_int -8)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 2"
  "stmdb%?\t%3, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*stm2_db_update"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 3 "s_register_operand" "+&rk")
          (plus:SI (match_dup 3) (const_int -8)))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -8)))
          (match_operand:SI 1 "arm_hard_general_register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 3) (const_int -4)))
          (match_operand:SI 2 "arm_hard_general_register_operand" ""))])]
  "TARGET_32BIT && XVECLEN (operands[0], 0) == 3"
  "stmdb%?\t%3!, {%1, %2}"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_ldm_seq (operands, 2, false))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 4 "const_int_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 5 "const_int_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_dup 1))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 2))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 4 "const_int_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 5 "const_int_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_dup 0))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_dup 1))]
  ""
  [(const_int 0)]
{
  if (gen_const_stm_seq (operands, 2))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 0 "s_register_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
        (match_operand:SI 1 "s_register_operand" ""))]
  ""
  [(const_int 0)]
{
  if (gen_stm_seq (operands, 2))
    DONE;
  else
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (parallel
     [(set (match_operand:SI 4 "s_register_operand" "")
           (match_operator:SI 5 "commutative_binary_operator"
            [(match_operand:SI 6 "s_register_operand" "")
             (match_operand:SI 7 "s_register_operand" "")]))
      (clobber (reg:CC CC_REGNUM))])]
  "((((REGNO (operands[6]) == REGNO (operands[0]))
         && (REGNO (operands[7]) == REGNO (operands[1])))
      || ((REGNO (operands[7]) == REGNO (operands[0]))
         && (REGNO (operands[6]) == REGNO (operands[1]))))
    && (peep2_regno_dead_p (3, REGNO (operands[0]))
      || (REGNO (operands[0]) == REGNO (operands[4])))
    && (peep2_regno_dead_p (3, REGNO (operands[1]))
      || (REGNO (operands[1]) == REGNO (operands[4]))))"
  [(parallel
    [(set (match_dup 4) (match_op_dup 5 [(match_dup 6) (match_dup 7)]))
     (clobber (reg:CC CC_REGNUM))])]
{
  if (!gen_ldm_seq (operands, 2, true))
    FAIL;
})

(define_peephole2
  [(set (match_operand:SI 0 "s_register_operand" "")
        (match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "s_register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))
   (set (match_operand:SI 4 "s_register_operand" "")
        (match_operator:SI 5 "commutative_binary_operator"
         [(match_operand:SI 6 "s_register_operand" "")
          (match_operand:SI 7 "s_register_operand" "")]))]
  "((((REGNO (operands[6]) == REGNO (operands[0]))
         && (REGNO (operands[7]) == REGNO (operands[1])))
      || ((REGNO (operands[7]) == REGNO (operands[0]))
         && (REGNO (operands[6]) == REGNO (operands[1]))))
    && (peep2_regno_dead_p (3, REGNO (operands[0]))
      || (REGNO (operands[0]) == REGNO (operands[4])))
    && (peep2_regno_dead_p (3, REGNO (operands[1]))
      || (REGNO (operands[1]) == REGNO (operands[4]))))"
  [(set (match_dup 4) (match_op_dup 5 [(match_dup 6) (match_dup 7)]))]
{
  if (!gen_ldm_seq (operands, 2, true))
    FAIL;
})

