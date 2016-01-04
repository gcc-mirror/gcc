;; Load/Store Multiple patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.for NDS32.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Load Multiple Insns.
;;
;; operands[0] is the first of the consecutive registers.
;; operands[1] is the first memory location.
;; operands[2] is the number of consecutive registers.

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
			  (match_operand:SI 1 "" ""))
		     (use (match_operand:SI 2 "" ""))])]
  ""
{
  int maximum;

  /* Because reduced-set regsiters has few registers
     (r0~r5, r6~10, r15, r28~r31, where 'r15' and 'r28~r31' cannot
     be used for register allocation),
     using 8 registers for load_multiple may easily consume all of them.
     It makes register allocation/spilling hard to work.
     So we only allow maximum=4 registers for load_multiple
     under reduced-set registers.  */
  if (TARGET_REDUCED_REGS)
    maximum = 4;
  else
    maximum = 8;

  /* Here are the conditions that must be all passed,
     otherwise we have to FAIL this rtx generation:
       1. The number of consecutive registers must be integer.
       2. Maximum 4 or 8 registers for lmw.bi instruction
          (based on this nds32-multiple.md design).
       3. Minimum 2 registers for lmw.bi instruction
          (based on this nds32-multiple.md design).
       4. operands[0] must be register for sure.
       5. operands[1] must be memory for sure.
       6. Do not cross $r15 register because it is not allocatable.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > maximum
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[0]) != REG
      || GET_CODE (operands[1]) != MEM
      || REGNO (operands[0]) + INTVAL (operands[2]) > TA_REGNUM)
    FAIL;

  /* For (mem addr), we force_reg on addr here,
     so that nds32_expand_load_multiple can easily use it.  */
  operands[3] = nds32_expand_load_multiple (REGNO (operands[0]),
					    INTVAL (operands[2]),
					    force_reg (SImode,
						       XEXP (operands[1], 0)),
					    operands[1]);
})

;; Ordinary Load Multiple.

(define_insn "*lmwsi8"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 12))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 16))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 20))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 24))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))])]
  "(XVECLEN (operands[0], 0) == 8)"
  "lmw.bi\t%2, [%1], %9, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi7"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 12))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 16))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 20))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 24))))])]
  "(XVECLEN (operands[0], 0) == 7)"
  "lmw.bi\t%2, [%1], %8, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi6"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 12))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 16))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 20))))])]
  "(XVECLEN (operands[0], 0) == 6)"
  "lmw.bi\t%2, [%1], %7, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi5"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 12))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 16))))])]
  "(XVECLEN (operands[0], 0) == 5)"
  "lmw.bi\t%2, [%1], %6, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi4"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 12))))])]
  "(XVECLEN (operands[0], 0) == 4)"
  "lmw.bi\t%2, [%1], %5, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi3"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 8))))])]
  "(XVECLEN (operands[0], 0) == 3)"
  "lmw.bi\t%2, [%1], %4, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_insn "*lmwsi2"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))])]
  "(XVECLEN (operands[0], 0) == 2)"
  "lmw.bi\t%2, [%1], %3, 0x0"
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)


;; Store Multiple Insns.
;;
;; operands[0] is the first memory location.
;; opernads[1] is the first of the consecutive registers.
;; operands[2] is the number of consecutive registers.

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
			  (match_operand:SI 1 "" ""))
		     (use (match_operand:SI 2 "" ""))])]
  ""
{
  int maximum;

  /* Because reduced-set regsiters has few registers
     (r0~r5, r6~10, r15, r28~r31, where 'r15' and 'r28~r31' cannot
     be used for register allocation),
     using 8 registers for store_multiple may easily consume all of them.
     It makes register allocation/spilling hard to work.
     So we only allow maximum=4 registers for store_multiple
     under reduced-set registers.  */
  if (TARGET_REDUCED_REGS)
    maximum = 4;
  else
    maximum = 8;

  /* Here are the conditions that must be all passed,
     otherwise we have to FAIL this rtx generation:
       1. The number of consecutive registers must be integer.
       2. Maximum 4 or 8 registers for smw.bi instruction
          (based on this nds32-multiple.md design).
       3. Minimum 2 registers for smw.bi instruction
          (based on this nds32-multiple.md design).
       4. operands[0] must be memory for sure.
       5. operands[1] must be register for sure.
       6. Do not cross $r15 register because it is not allocatable.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > maximum
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[0]) != MEM
      || GET_CODE (operands[1]) != REG
      || REGNO (operands[1]) + INTVAL (operands[2]) > TA_REGNUM)
    FAIL;

  /* For (mem addr), we force_reg on addr here,
     so that nds32_expand_store_multiple can easily use it.  */
  operands[3] = nds32_expand_store_multiple (REGNO (operands[1]),
					     INTVAL (operands[2]),
					     force_reg (SImode,
							XEXP (operands[0], 0)),
					     operands[0]);
})

;; Ordinary Store Multiple.

(define_insn "*stmsi8"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 16)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 20)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 24)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 28)))
	  (match_operand:SI 9 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 8)"
  "smw.bi\t%2, [%1], %9, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi7"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 16)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 20)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 24)))
	  (match_operand:SI 8 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 7)"
  "smw.bi\t%2, [%1], %8, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi6"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 16)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 20)))
	  (match_operand:SI 7 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 6)"
  "smw.bi\t%2, [%1], %7, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi5"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 16)))
	  (match_operand:SI 6 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 5)"
  "smw.bi\t%2, [%1], %6, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi4"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 4)"
  "smw.bi\t%2, [%1], %5, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi3"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 3)"
  "smw.bi\t%2, [%1], %4, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "*stmsi2"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 2)"
  "smw.bi\t%2, [%1], %3, 0x0"
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.
;;
;; operands[0] is the destination block of memory.
;; operands[1] is the source block of memory.
;; operands[2] is the number of bytes to move.
;; operands[3] is the known shared alignment.

(define_expand "movmemqi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
{
  if (nds32_expand_movmemqi (operands[0],
			     operands[1],
			     operands[2],
			     operands[3]))
    DONE;

  FAIL;
})

;; ------------------------------------------------------------------------
