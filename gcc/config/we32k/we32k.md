;;- Machine description for GNU compiler
;;- AT&T we32000 Version
;;  Contributed by John Wehle (john@feith1.uucp)
;;   Copyright (C) 1991-1992 Free Software Foundation, Inc.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;- instruction definitions

;;- @@The original PO technology requires these to be ordered by speed,
;;- @@    so that assigner will pick the fastest.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;- When naming insn's (operand 0 of define_insn) be careful about using
;;- names from other targets machine descriptions.

;; move instructions

(define_insn ""
  [(set (match_operand:DF 0 "push_operand" "=m")
        (match_operand:DF 1 "general_operand" "mrF"))]
  ""
  "*
  {
  output_push_double(&operands[1]);

  return \"\";
  }")

(define_insn "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=mr")
        (match_operand:DF 1 "general_operand" "mrF"))]
  ""
  "*
  {
  output_move_double(operands);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SF 0 "push_operand" "=m")
        (match_operand:SF 1 "general_operand" "mrF"))]
  ""
  "pushw %1")

(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=mr")
        (match_operand:SF 1 "general_operand" "mrF"))]
  ""
  "movw %1, %0")

(define_insn ""
  [(set (match_operand:DI 0 "push_operand" "=m")
        (match_operand:DI 1 "general_operand" "mriF"))]
  ""
  "*
  {
  output_push_double(&operands[1]);

  return \"\";
  }")

(define_insn "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=mr")
        (match_operand:DI 1 "general_operand" "mriF"))]
  ""
  "*
  {
  output_move_double(operands);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=m")
        (match_operand:SI 1 "general_operand" "mri"))]
  ""
  "pushw %1")

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (match_operand:SI 1 "general_operand" "mri"))]
  ""
  "movw %1, %0")

(define_insn "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (match_operand:HI 1 "general_operand" "mri"))]
  ""
  "movh %1, %0")

(define_insn "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (match_operand:QI 1 "general_operand" "mri"))]
  ""
  "movb %1, %0")

;; add instructions

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&or")
        (plus:DI (match_operand:DI 1 "nonimmediate_operand" "0")
                 (match_operand:DI 2 "general_operand" "oriF")))]
  ""
  "*
  {
  rtx label[1];
  rtx lsw_operands[3];

  if (GET_CODE (operands[0]) == REG)
    lsw_operands[0] = gen_rtx(REG, SImode, REGNO (operands[0]) + 1);
  else
    if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
      lsw_operands[0] = adj_offsettable_operand(operands[0], 4);
    else
      abort();

  if (GET_CODE (operands[2]) == REG)
    lsw_operands[2] = gen_rtx(REG, SImode, REGNO (operands[2]) + 1);
  else
    if (GET_CODE (operands[2]) == MEM && offsettable_memref_p (operands[2]))
      lsw_operands[2] = adj_offsettable_operand(operands[2], 4);
    else
      if (GET_CODE (operands[2]) == CONST_DOUBLE)
        {
        lsw_operands[2] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[2]));
        operands[2] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[2]));
        }
      else
        if (GET_CODE (operands[2]) == CONST_INT)
          {
          lsw_operands[2] = operands[2];
          operands[2] = const0_rtx;
          }
        else
          abort();

  label[0] = gen_label_rtx();
  LABEL_NUSES(label[0]) = 1;

  output_asm_insn(\"addw2 %2, %0\", operands);
  output_asm_insn(\"addw2 %2, %0\", lsw_operands);
  output_asm_insn(\"BCCB %l0\", label);
  output_asm_insn(\"INCW %0\", operands);
  output_asm_insn(\"%l0:\", label);

  return \"\";
  }")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&or")
        (plus:DI (match_operand:DI 1 "general_operand" "oriF")
                 (match_operand:DI 2 "general_operand" "oriF")))]
  ""
  "*
  {
  rtx label[1];
  rtx lsw_operands[3];

  if (GET_CODE (operands[0]) == REG)
    lsw_operands[0] = gen_rtx(REG, SImode, REGNO (operands[0]) + 1);
  else
    if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
      lsw_operands[0] = adj_offsettable_operand(operands[0], 4);
    else
      abort();

  if (GET_CODE (operands[1]) == REG)
    lsw_operands[1] = gen_rtx(REG, SImode, REGNO (operands[1]) + 1);
  else
    if (GET_CODE (operands[1]) == MEM && offsettable_memref_p (operands[1]))
      lsw_operands[1] = adj_offsettable_operand(operands[1], 4);
    else
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
        {
        lsw_operands[1] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[1]));
        operands[1] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[1]));
        }
      else
        if (GET_CODE (operands[1]) == CONST_INT)
          {
          lsw_operands[1] = operands[1];
          operands[1] = const0_rtx;
          }
        else
          abort();

  if (GET_CODE (operands[2]) == REG)
    lsw_operands[2] = gen_rtx(REG, SImode, REGNO (operands[2]) + 1);
  else
    if (GET_CODE (operands[2]) == MEM && offsettable_memref_p (operands[2]))
      lsw_operands[2] = adj_offsettable_operand(operands[2], 4);
    else
      if (GET_CODE (operands[2]) == CONST_DOUBLE)
        {
        lsw_operands[2] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[2]));
        operands[2] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[2]));
        }
      else
        if (GET_CODE (operands[2]) == CONST_INT)
          {
          lsw_operands[2] = operands[2];
          operands[2] = const0_rtx;
          }
        else
          abort();

  label[0] = gen_label_rtx();
  LABEL_NUSES(label[0]) = 1;

  output_asm_insn(\"addw3 %2, %1, %0\", operands);
  output_asm_insn(\"addw3 %2, %1, %0\", lsw_operands);
  output_asm_insn(\"BCCB %l0\", label);
  output_asm_insn(\"INCW %0\", operands);
  output_asm_insn(\"%l0:\", label);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (plus:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "addw2 %2, %0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (plus:SI (match_operand:SI 1 "general_operand" "mri")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "addw3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (plus:HI (match_operand:HI 1 "nonimmediate_operand" "0")
                 (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "addh2 %2, %0")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (plus:HI (match_operand:HI 1 "general_operand" "mri")
                 (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "addh3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (plus:QI (match_operand:QI 1 "nonimmediate_operand" "0")
                 (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "addb2 %2, %0")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (plus:QI (match_operand:QI 1 "general_operand" "mri")
                 (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "addb3 %2, %1, %0")

;; subtract instructions

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&or")
        (minus:DI (match_operand:DI 1 "nonimmediate_operand" "0")
                 (match_operand:DI 2 "general_operand" "oriF")))]
  ""
  "*
  {
  rtx label[1];
  rtx lsw_operands[3];

  if (GET_CODE (operands[0]) == REG)
    lsw_operands[0] = gen_rtx(REG, SImode, REGNO (operands[0]) + 1);
  else
    if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
      lsw_operands[0] = adj_offsettable_operand(operands[0], 4);
    else
      abort();

  if (GET_CODE (operands[2]) == REG)
    lsw_operands[2] = gen_rtx(REG, SImode, REGNO (operands[2]) + 1);
  else
    if (GET_CODE (operands[2]) == MEM && offsettable_memref_p (operands[2]))
      lsw_operands[2] = adj_offsettable_operand(operands[2], 4);
    else
      if (GET_CODE (operands[2]) == CONST_DOUBLE)
        {
        lsw_operands[2] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[2]));
        operands[2] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[2]));
        }
      else
        if (GET_CODE (operands[2]) == CONST_INT)
          {
          lsw_operands[2] = operands[2];
          operands[2] = const0_rtx;
          }
        else
          abort();

  label[0] = gen_label_rtx();
  LABEL_NUSES(label[0]) = 1;

  output_asm_insn(\"subw2 %2, %0\", operands);
  output_asm_insn(\"subw2 %2, %0\", lsw_operands);
  output_asm_insn(\"BCCB %l0\", label);
  output_asm_insn(\"DECW %0\", operands);
  output_asm_insn(\"%l0:\", label);

  return \"\";
  }")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=&or")
        (minus:DI (match_operand:DI 1 "general_operand" "oriF")
                 (match_operand:DI 2 "general_operand" "oriF")))]
  ""
  "*
  {
  rtx label[1];
  rtx lsw_operands[3];

  if (GET_CODE (operands[0]) == REG)
    lsw_operands[0] = gen_rtx(REG, SImode, REGNO (operands[0]) + 1);
  else
    if (GET_CODE (operands[0]) == MEM && offsettable_memref_p (operands[0]))
      lsw_operands[0] = adj_offsettable_operand(operands[0], 4);
    else
      abort();

  if (GET_CODE (operands[1]) == REG)
    lsw_operands[1] = gen_rtx(REG, SImode, REGNO (operands[1]) + 1);
  else
    if (GET_CODE (operands[1]) == MEM && offsettable_memref_p (operands[1]))
      lsw_operands[1] = adj_offsettable_operand(operands[1], 4);
    else
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
        {
        lsw_operands[1] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[1]));
        operands[1] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[1]));
        }
      else
        if (GET_CODE (operands[1]) == CONST_INT)
          {
          lsw_operands[1] = operands[1];
          operands[1] = const0_rtx;
          }
        else
          abort();

  if (GET_CODE (operands[2]) == REG)
    lsw_operands[2] = gen_rtx(REG, SImode, REGNO (operands[2]) + 1);
  else
    if (GET_CODE (operands[2]) == MEM && offsettable_memref_p (operands[2]))
      lsw_operands[2] = adj_offsettable_operand(operands[2], 4);
    else
      if (GET_CODE (operands[2]) == CONST_DOUBLE)
        {
        lsw_operands[2] = gen_rtx(CONST_INT, SImode,
                                  CONST_DOUBLE_HIGH(operands[2]));
        operands[2] = gen_rtx(CONST_INT, SImode,
                              CONST_DOUBLE_LOW(operands[2]));
        }
      else
        if (GET_CODE (operands[2]) == CONST_INT)
          {
          lsw_operands[2] = operands[2];
          operands[2] = const0_rtx;
          }
        else
          abort();

  label[0] = gen_label_rtx();
  LABEL_NUSES(label[0]) = 1;

  output_asm_insn(\"subw3 %2, %1, %0\", operands);
  output_asm_insn(\"subw3 %2, %1, %0\", lsw_operands);
  output_asm_insn(\"BCCB %l0\", label);
  output_asm_insn(\"DECW %0\", operands);
  output_asm_insn(\"%l0:\", label);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (minus:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                  (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "subw2 %2, %0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (minus:SI (match_operand:SI 1 "general_operand" "mri")
                  (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "subw3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (minus:HI (match_operand:HI 1 "nonimmediate_operand" "0")
                  (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "subh2 %2, %0")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (minus:HI (match_operand:HI 1 "general_operand" "mri")
                  (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "subh3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (minus:QI (match_operand:QI 1 "nonimmediate_operand" "0")
                  (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "subb2 %2, %0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (minus:QI (match_operand:QI 1 "general_operand" "mri")
                  (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "subb3 %2, %1, %0")

;; signed multiply instructions

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (mult:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "mulw2 %2, %0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (mult:SI (match_operand:SI 1 "general_operand" "mri")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "mulw3 %2, %1, %0")

;; signed divide instructions

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (div:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "divw2 %2, %0")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (div:SI (match_operand:SI 1 "general_operand" "mri")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "divw3 %2, %1, %0")

;; signed modulus instruction

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (mod:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "modw2 %2, %0")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (mod:SI (match_operand:SI 1 "general_operand" "mri")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "modw3 %2, %1, %0")

;; unsigned divide instruction

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (udiv:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "udivw2 %2, %0")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (udiv:SI (match_operand:SI 1 "general_operand" "mri")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "udivw3 %2, %1, %0")

;; unsigned modulus instruction

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (umod:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "umodw2 %2, %0")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (umod:SI (match_operand:SI 1 "general_operand" "mri")
                 (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "umodw3 %2, %1, %0")

;; logical-and instructions

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (and:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "andw2 %2, %0")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (and:SI (match_operand:SI 1 "general_operand" "mri")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "andw3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (and:HI (match_operand:HI 1 "nonimmediate_operand" "0")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "andh2 %2, %0")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (and:HI (match_operand:HI 1 "general_operand" "mri")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "andh3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (and:QI (match_operand:QI 1 "nonimmediate_operand" "0")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "andb2 %2, %0")

(define_insn "andqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (and:QI (match_operand:QI 1 "general_operand" "mri")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "andb3 %2, %1, %0")

;; inclusive-or instructions

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (ior:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "orw2 %2, %0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (ior:SI (match_operand:SI 1 "general_operand" "mri")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "orw3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (ior:HI (match_operand:HI 1 "nonimmediate_operand" "0")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "orh2 %2, %0")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (ior:HI (match_operand:HI 1 "general_operand" "mri")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "orh3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (ior:QI (match_operand:QI 1 "nonimmediate_operand" "0")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "orb2 %2, %0")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (ior:QI (match_operand:QI 1 "general_operand" "mri")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "orb3 %2, %1, %0")

;; exclusive-or instructions

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (xor:SI (match_operand:SI 1 "nonimmediate_operand" "0")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "xorw2 %2, %0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (xor:SI (match_operand:SI 1 "general_operand" "mri")
                (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "xorw3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (xor:HI (match_operand:HI 1 "nonimmediate_operand" "0")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "xorh2 %2, %0")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (xor:HI (match_operand:HI 1 "general_operand" "mri")
                (match_operand:HI 2 "general_operand" "mri")))]
  ""
  "xorh3 %2, %1, %0")

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (xor:QI (match_operand:QI 1 "nonimmediate_operand" "0")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "xorb2 %2, %0")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (xor:QI (match_operand:QI 1 "general_operand" "mri")
                (match_operand:QI 2 "general_operand" "mri")))]
  ""
  "xorb3 %2, %1, %0")

;; arithmetic shift instructions

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (ashift:SI (match_operand:SI 1 "general_operand" "mri")
                   (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "LLSW3 %2, %1, %0")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (ashiftrt:SI (match_operand:SI 1 "general_operand" "mri")
                     (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "ARSW3 %2, %1, %0")

;; logical shift instructions

;; (define_insn "lshlsi3"
;;   [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
;;         (lshift:SI (match_operand:SI 1 "general_operand" "mri")
;;                    (match_operand:SI 2 "general_operand" "mri")))]
;;   ""
;;   "LLSW3 %2, %1, %0")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (lshiftrt:SI (match_operand:SI 1 "general_operand" "mri")
                     (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "LRSW3 %2, %1, %0")

;; rotate instruction

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (rotatert: SI (match_operand:SI 1 "general_operand" "mri")
                      (match_operand:SI 2 "general_operand" "mri")))]
  ""
  "ROTW %2, %1, %0")

;; negate instructions

(define_insn "negsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (neg:SI (match_operand:SI 1 "general_operand" "mri")))]
  ""
  "mnegw %1, %0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (neg:HI (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "mnegh %1, %0")

;; complement instructions

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (not:SI (match_operand:SI 1 "general_operand" "mri")))]
  ""
  "mcomw %1, %0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (not:HI (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "mcomh %1, %0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (not:QI (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "mcomb %1, %0")

;; test instruction

;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.

(define_insn "tstsi"
  [(set (cc0) (match_operand:SI 0 "nonimmediate_operand" "mr"))]
  ""
  "TSTW %0")

(define_insn "tsthi"
  [(set (cc0) (match_operand:HI 0 "nonimmediate_operand" "mr"))]
  ""
  "TSTH %0")

(define_insn "tstqi"
  [(set (cc0) (match_operand:QI 0 "nonimmediate_operand" "mr"))]
  ""
  "TSTB {sbyte}%0")

;; compare instruction

(define_insn "cmpsi"
  [(set (cc0) (compare (match_operand:SI 0 "nonimmediate_operand" "mr")
                       (match_operand:SI 1 "general_operand" "mri")))]
  ""
  "CMPW %1, %0")

(define_insn "cmphi"
  [(set (cc0) (compare (match_operand:HI 0 "nonimmediate_operand" "mr")
                       (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "*
  {

  if (GET_CODE (operands[1]) == CONST_INT &&
    ((unsigned long)INTVAL (operands[1]) & 0x8000L))
    operands[1] = gen_rtx(CONST_INT, SImode, INTVAL(operands[1]) | 0xffff0000L);

  output_asm_insn(\"CMPH %1, %0\",operands);

  return \"\";
  }")

(define_insn "cmpqi"
  [(set (cc0) (compare (match_operand:QI 0 "nonimmediate_operand" "mr")
                       (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "*
  {

  if (GET_CODE (operands[1]) == CONST_INT &&
    ((unsigned long)INTVAL (operands[1]) & 0x80L))
    operands[1] = gen_rtx(CONST_INT, SImode, INTVAL(operands[1]) | 0xffffff00L);

  output_asm_insn(\"CMPB {sbyte}%1, {sbyte}%0\",operands);

  return \"\";
  }")

;; truncate instructions

(define_insn "truncdfsf2"
  [(clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (set (match_operand:SF 0 "nonimmediate_operand" "=mr")
        (float_truncate:SF (match_operand:DF 1 "general_operand" "orF")))]
  ""
  "*
  {
  output_push_double(&operands[1]);
  output_asm_insn(\"call &2, _fdtos\");

  if (GET_CODE (operands[0]) != REG || REGNO (operands[0]) != 0)
    output_asm_insn(\"movw %%r0, %0\", operands);

  return \"\";
  }")


(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (truncate:HI (match_operand:SI 1 "general_operand" "mri")))]
  ""
  "movtwh %1, %0")

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (truncate:QI (match_operand:SI 1 "general_operand" "mri")))]
  ""
  "movtwb %1, %0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=mr")
        (truncate:QI (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "movthb %1, %0")

;; sign-extend move instructions

(define_insn "extendsfdf2"
  [(clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (set (match_operand:DF 0 "nonimmediate_operand" "=or")
        (float_extend:DF (match_operand:SF 1 "general_operand" "mrF")))]
  ""
  "*
  {
  rtx xoperands[2];

  output_asm_insn(\"pushw %1\", operands);
  output_asm_insn(\"call &1, _fstod\");

  if (GET_CODE (operands[0]) != REG || REGNO (operands[0]) != 0) {
    xoperands[0] = operands[0];
    xoperands[1] = gen_rtx(REG, DFmode, 0);
    output_move_double(xoperands);
    }

  return \"\";
  }")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "movbhw %1, %0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "movbbw %1, %0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (sign_extend:HI (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "movbbh %1, %0")

;; zero-extend move instructions

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (zero_extend:SI (match_operand:HI 1 "general_operand" "mri")))]
  ""
  "movzhw %1, %0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (zero_extend:SI (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "movzbw %1, %0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=mr")
        (zero_extend:HI (match_operand:QI 1 "general_operand" "mri")))]
  ""
  "movzbh %1, %0")

;; bit field instructions

(define_insn "extzv"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (zero_extract:SI (match_operand:SI 1 "general_operand" "mri")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "general_operand" "mri")))]
  ""
  "*
  {

  operands[2] = gen_rtx(CONST_INT, SImode, INTVAL(operands[2]) - 1);
  output_asm_insn(\"EXTFW %2, %3, %1, %0\",operands);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (zero_extract:SI (match_operand:HI 1 "general_operand" "mri")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "general_operand" "mri")))]
  ""
  "*
  {

  operands[2] = gen_rtx(CONST_INT, SImode, INTVAL(operands[2]) - 1);
  output_asm_insn(\"EXTFH %2, %3, {uhalf}%1, {uword}%0\",operands);

  return \"\";
  }")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mr")
        (zero_extract:SI (match_operand:QI 1 "general_operand" "mri")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "general_operand" "mri")))]
  ""
  "*
  {

  operands[2] = gen_rtx(CONST_INT, SImode, INTVAL(operands[2]) - 1);
  output_asm_insn(\"EXTFB %2, %3, {ubyte}%1, {uword}%0\",operands);

  return \"\";
  }")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+mr")
                         (match_operand:SI 1 "immediate_operand" "i")
                         (match_operand:SI 2 "general_operand" "mri"))
        (match_operand:SI 3 "general_operand" "mri"))]
  ""
  "*
  {

  operands[1] = gen_rtx(CONST_INT, SImode, INTVAL(operands[1]) - 1);
  output_asm_insn(\"INSFW %1, %2, %3, %0\",operands);

  return \"\";
  }")

(define_insn ""
  [(set (zero_extract:SI (match_operand:HI 0 "nonimmediate_operand" "+mr")
                         (match_operand:SI 1 "immediate_operand" "i")
                         (match_operand:SI 2 "general_operand" "mri"))
        (match_operand:SI 3 "general_operand" "mri"))]
  ""
  "*
  {

  operands[1] = gen_rtx(CONST_INT, SImode, INTVAL(operands[1]) - 1);
  output_asm_insn(\"INSFH %1, %2, {uword}%3, {uhalf}%0\",operands);

  return \"\";
  }")

(define_insn ""
  [(set (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "+mr")
                         (match_operand:SI 1 "immediate_operand" "i")
                         (match_operand:SI 2 "general_operand" "mri"))
        (match_operand:SI 3 "general_operand" "mri"))]
  ""
  "*
  {

  operands[1] = gen_rtx(CONST_INT, SImode, INTVAL(operands[1]) - 1);
  output_asm_insn(\"INSFB %1, %2, {uword}%3, {ubyte}%0\",operands);

  return \"\";
  }")

;; conditional branch instructions

(define_insn "beq"
  [(set (pc) (if_then_else (eq (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "je %l0")

(define_insn "bne"
  [(set (pc) (if_then_else (ne (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jne %l0")

(define_insn "bgt"
  [(set (pc) (if_then_else (gt (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jg %l0")

(define_insn "bgtu"
  [(set (pc) (if_then_else (gtu (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jgu %l0")

(define_insn "blt"
  [(set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jl %l0")

(define_insn "bltu"
  [(set (pc) (if_then_else (ltu (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jlu %l0")

(define_insn "bge"
  [(set (pc) (if_then_else (ge (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jge %l0")

(define_insn "bgeu"
  [(set (pc) (if_then_else (geu (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jgeu %l0")

(define_insn "ble"
  [(set (pc) (if_then_else (le (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jle %l0")

(define_insn "bleu"
  [(set (pc) (if_then_else (leu (cc0) (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "jleu %l0")

;; reverse-conditional branch instructions

(define_insn ""
  [(set (pc) (if_then_else (eq (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jne %l0")

(define_insn ""
  [(set (pc) (if_then_else (ne (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "je %l0")

(define_insn ""
  [(set (pc) (if_then_else (gt (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jle %l0")

(define_insn ""
  [(set (pc) (if_then_else (gtu (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jleu %l0")

(define_insn ""
  [(set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jge %l0")

(define_insn ""
  [(set (pc) (if_then_else (ltu (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jgeu %l0")

(define_insn ""
  [(set (pc) (if_then_else (ge (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jl %l0")

(define_insn ""
  [(set (pc) (if_then_else (geu (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jlu %l0")

(define_insn ""
  [(set (pc) (if_then_else (le (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jg %l0")

(define_insn ""
  [(set (pc) (if_then_else (leu (cc0) (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "jgu %l0")

;; call instructions

(define_insn "call"
  [(call (match_operand:QI 0 "memory_operand" "m")
         (match_operand:SI 1 "immediate_operand" "i"))]
  ""
  "call %1/4, %0")

(define_insn "call_value"
  [(set (match_operand 0 "register_operand" "=r")
        (call (match_operand:QI 1 "memory_operand" "m")
              (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "call %2/4, %1")

;; No-op instruction

(define_insn "nop"
  [(const_int 0)]
  ""
  "NOP")

;; jump through a dispatch table instruction

(define_expand "casesi"
  [(use (match_operand:SI 0 "general_operand" "mri"))
   (set (cc0) (compare (match_dup 5)
                       (match_operand:SI 1 "general_operand" "mri")))
   (set (pc) (if_then_else (lt (cc0) (const_int 0))
                           (label_ref (match_operand 4 "" ""))
                           (pc)))
   (set (match_dup 5) (minus:SI (match_dup 5)
                                (match_dup 1)))
   (set (cc0) (compare (match_dup 5)
                       (match_operand:SI 2 "general_operand" "mri")))
   (set (pc) (if_then_else (gtu (cc0) (const_int 0))
                           (label_ref (match_dup 4))
                           (pc)))
   (set (match_dup 5) (ashift:SI (match_dup 5)
                                 (const_int 2)))
   (set (pc) (mem:SI (plus:SI (label_ref (match_operand 3 "" ""))
                              (match_dup 5))))]
  ""
  "
  {
  operands[5] = gen_reg_rtx(GET_MODE (operands[0]));
  emit_move_insn(operands[5], operands[0]);
  }")

;; jump instructions

(define_insn ""
  [(set (pc) (mem:SI (match_operand:SI 0 "address_operand" "p")))]
  "GET_CODE (operands[0]) != MEM"
  "jmp *%a0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  ""
  "jmp %a0")

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "jmp %l0")

;; peephole optimizations

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "nonimmediate_operand" "or"))
   (set (match_operand:SI 2 "register_operand" "=r")
        (mem:SI (match_dup 0)))]
  "REGNO (operands[0]) == REGNO (operands[2]) && (REG_P (operands[1]) || offsettable_memref_p (operands[1]))"
  "movw %a1, %0")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "nonimmediate_operand" "or"))
   (set (match_operand:HI 2 "register_operand" "=r")
        (mem:HI (match_dup 0)))]
  "REGNO (operands[0]) == REGNO (operands[2]) && (REG_P (operands[1]) || offsettable_memref_p (operands[1]))"
  "movh %a1, %0")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "nonimmediate_operand" "or"))
   (set (match_operand:QI 2 "register_operand" "=r")
        (mem:QI (match_dup 0)))]
  "REGNO (operands[0]) == REGNO (operands[2]) && (REG_P (operands[1]) || offsettable_memref_p (operands[1]))"
  "movb %a1, %0")
