;; Load/Store Multiple patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
       6. operands[1] is not volatile memory access.
       7. Do not cross $r15 register because it is not allocatable.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > maximum
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[0]) != REG
      || GET_CODE (operands[1]) != MEM
      || MEM_VOLATILE_P (operands[1])
      || REGNO (operands[0]) + INTVAL (operands[2]) > TA_REGNUM)
    FAIL;

  /* For (mem addr), we force_reg on addr here,
     so that nds32_expand_load_multiple can easily use it.  */
  operands[3] = nds32_expand_load_multiple (REGNO (operands[0]),
					    INTVAL (operands[2]),
					    force_reg (SImode,
						       XEXP (operands[1], 0)),
					    operands[1],
					    false, NULL);
})

;; Ordinary Load Multiple.
(define_insn "*lmw_bim_si25"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 100)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 80))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 84))))
     (set (match_operand:SI 25 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 88))))
     (set (match_operand:SI 26 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 92))))
     (set (match_operand:SI 27 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 96))))])]
  "(XVECLEN (operands[0], 0) == 26)"
  "lmw.bim\t%3, [%1], %27, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "25")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si24"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 96)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 80))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 84))))
     (set (match_operand:SI 25 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 88))))
     (set (match_operand:SI 26 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 92))))])]
  "(XVECLEN (operands[0], 0) == 25)"
  "lmw.bim\t%3, [%1], %26, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "24")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si23"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 92)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 80))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 84))))
     (set (match_operand:SI 25 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 88))))])]
  "(XVECLEN (operands[0], 0) == 24)"
  "lmw.bim\t%3, [%1], %25, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "23")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si22"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 88)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 80))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 84))))])]
  "(XVECLEN (operands[0], 0) == 23)"
  "lmw.bim\t%3, [%1], %24, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "22")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si21"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 84)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 80))))])]
  "(XVECLEN (operands[0], 0) == 22)"
  "lmw.bim\t%3, [%1], %23, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "21")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si20"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 80)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 76))))])]
  "(XVECLEN (operands[0], 0) == 21)"
  "lmw.bim\t%3, [%1], %22, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "20")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si19"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 76)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 72))))])]
  "(XVECLEN (operands[0], 0) == 20)"
  "lmw.bim\t%3, [%1], %21, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "19")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si18"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 72)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 68))))])]
  "(XVECLEN (operands[0], 0) == 19)"
  "lmw.bim\t%3, [%1], %20, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "18")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si17"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 68)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 64))))])]
  "(XVECLEN (operands[0], 0) == 18)"
  "lmw.bim\t%3, [%1], %19, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "17")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si16"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 64)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 60))))])]
  "(XVECLEN (operands[0], 0) == 17)"
  "lmw.bim\t%3, [%1], %18, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "16")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si15"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 60)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 56))))])]
  "(XVECLEN (operands[0], 0) == 16)"
  "lmw.bim\t%3, [%1], %17, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "15")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si14"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 56)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 52))))])]
  "(XVECLEN (operands[0], 0) == 15)"
  "lmw.bim\t%3, [%1], %16, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "14")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si13"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 52)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 48))))])]
  "(XVECLEN (operands[0], 0) == 14)"
  "lmw.bim\t%3, [%1], %15, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "13")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si12"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 48)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 44))))])]
  "(XVECLEN (operands[0], 0) == 13)"
  "lmw.bim\t%3, [%1], %14, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "12")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si11"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 44)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 40))))])]
  "(XVECLEN (operands[0], 0) == 12)"
  "lmw.bim\t%3, [%1], %13, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "11")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si10"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 40)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 36))))])]
  "(XVECLEN (operands[0], 0) == 11)"
  "lmw.bim\t%3, [%1], %12, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "10")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si9"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 36)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 32))))])]
  "(XVECLEN (operands[0], 0) == 10)"
  "lmw.bim\t%3, [%1], %11, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "9")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si8"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 32)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 28))))])]
  "(XVECLEN (operands[0], 0) == 9)"
  "lmw.bim\t%3, [%1], %10, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "8")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si7"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 28)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))
     (set (match_operand:SI 9 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 24))))])]
  "(XVECLEN (operands[0], 0) == 8)"
  "lmw.bim\t%3, [%1], %9, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "7")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si6"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 24)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))
     (set (match_operand:SI 8 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 20))))])]
  "(XVECLEN (operands[0], 0) == 7)"
  "lmw.bim\t%3, [%1], %8, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "6")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si5"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 20)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))
     (set (match_operand:SI 7 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 16))))])]
  "(XVECLEN (operands[0], 0) == 6)"
  "lmw.bim\t%3, [%1], %7, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "5")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si4"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 16)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))
     (set (match_operand:SI 6 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 12))))])]
  "(XVECLEN (operands[0], 0) == 5)"
  "lmw.bim\t%3, [%1], %6, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "4")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si3"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 12)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 8))))])]
  "(XVECLEN (operands[0], 0) == 4)"
  "lmw.bim\t%3, [%1], %5, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "3")
   (set_attr "length"             "4")]
)

(define_insn "*lmw_bim_si2"
  [(match_parallel 0 "nds32_load_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 8)))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 2) (const_int 4))))])]
  "(XVECLEN (operands[0], 0) == 3)"
  "lmw.bim\t%3, [%1], %4, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "2")
   (set_attr "length"             "4")]
)

(define_expand "unaligned_load_update_base_w"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (plus:SI (match_operand:SI 2 "register_operand" "") (const_int 4)))
	      (set (match_operand:SI 1 "register_operand" "")
		   (unspec:SI [(mem:SI (match_dup 2))] UNSPEC_UALOAD_W))])]
  ""
{
  emit_insn (gen_unaligned_load_w (operands[1], gen_rtx_MEM (SImode, operands[2])));
  emit_insn (gen_addsi3 (operands[0], operands[2], gen_int_mode (4, Pmode)));
  DONE;
}
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "1")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi25"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 80))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 84))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 88))))
     (set (match_operand:SI 25 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 92))))
     (set (match_operand:SI 26 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 96))))])]
  "(XVECLEN (operands[0], 0) == 25)"
  "lmw.bi\t%2, [%1], %26, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "25")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi24"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 80))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 84))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 88))))
     (set (match_operand:SI 25 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 92))))])]
  "(XVECLEN (operands[0], 0) == 24)"
  "lmw.bi\t%2, [%1], %25, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "24")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi23"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 80))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 84))))
     (set (match_operand:SI 24 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 88))))])]
  "(XVECLEN (operands[0], 0) == 23)"
  "lmw.bi\t%2, [%1], %24, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "23")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi22"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 80))))
     (set (match_operand:SI 23 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 84))))])]
  "(XVECLEN (operands[0], 0) == 22)"
  "lmw.bi\t%2, [%1], %23, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "22")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi21"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))
     (set (match_operand:SI 22 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 80))))])]
  "(XVECLEN (operands[0], 0) == 21)"
  "lmw.bi\t%2, [%1], %22, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "21")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi20"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))
     (set (match_operand:SI 21 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 76))))])]
  "(XVECLEN (operands[0], 0) == 20)"
  "lmw.bi\t%2, [%1], %21, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "20")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi19"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))
     (set (match_operand:SI 20 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 72))))])]
  "(XVECLEN (operands[0], 0) == 19)"
  "lmw.bi\t%2, [%1], %20, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "19")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi18"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))
     (set (match_operand:SI 19 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 68))))])]
  "(XVECLEN (operands[0], 0) == 18)"
  "lmw.bi\t%2, [%1], %19, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "18")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi17"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))
     (set (match_operand:SI 18 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 64))))])]
  "(XVECLEN (operands[0], 0) == 17)"
  "lmw.bi\t%2, [%1], %18, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "17")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi16"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))
     (set (match_operand:SI 17 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 60))))])]
  "(XVECLEN (operands[0], 0) == 16)"
  "lmw.bi\t%2, [%1], %17, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "16")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi15"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))
     (set (match_operand:SI 16 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 56))))])]
  "(XVECLEN (operands[0], 0) == 15)"
  "lmw.bi\t%2, [%1], %16, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "15")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi14"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))
     (set (match_operand:SI 15 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 52))))])]
  "(XVECLEN (operands[0], 0) == 14)"
  "lmw.bi\t%2, [%1], %15, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "14")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi13"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))
     (set (match_operand:SI 14 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 48))))])]
  "(XVECLEN (operands[0], 0) == 13)"
  "lmw.bi\t%2, [%1], %14, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "13")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi12"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))
     (set (match_operand:SI 13 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 44))))])]
  "(XVECLEN (operands[0], 0) == 12)"
  "lmw.bi\t%2, [%1], %13, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "12")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi11"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))
     (set (match_operand:SI 12 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 40))))])]
  "(XVECLEN (operands[0], 0) == 11)"
  "lmw.bi\t%2, [%1], %12, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "11")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi10"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))
     (set (match_operand:SI 11 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 36))))])]
  "(XVECLEN (operands[0], 0) == 10)"
  "lmw.bi\t%2, [%1], %11, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"             "10")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi9"
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
	  (mem:SI (plus:SI (match_dup 1) (const_int 28))))
     (set (match_operand:SI 10 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 32))))])]
  "(XVECLEN (operands[0], 0) == 9)"
  "lmw.bi\t%2, [%1], %10, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "9")
   (set_attr "length"             "4")]
)

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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "8")
   (set_attr "length"             "4")]
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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "7")
   (set_attr "length"             "4")]
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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "6")
   (set_attr "length"             "4")]
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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "5")
   (set_attr "length"             "4")]
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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "4")
   (set_attr "length"             "4")]
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
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "3")
   (set_attr "length"             "4")]
)

(define_insn "*lmwsi2"
  [(match_parallel 0 "nds32_load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "")
	  (mem:SI (match_operand:SI 1 "register_operand" "r")))
     (set (match_operand:SI 3 "register_operand" "")
	  (mem:SI (plus:SI (match_dup 1) (const_int 4))))])]
  "(XVECLEN (operands[0], 0) == 2)"
  "lmw.bi\t%2, [%1], %3, 0x0"
  [(set_attr "type"   "load_multiple")
   (set_attr "combo"              "2")
   (set_attr "length"             "4")]
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
       6. operands[0] is not volatile memory access.
       7. Do not cross $r15 register because it is not allocatable.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > maximum
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[0]) != MEM
      || GET_CODE (operands[1]) != REG
      || MEM_VOLATILE_P (operands[0])
      || REGNO (operands[1]) + INTVAL (operands[2]) > TA_REGNUM)
    FAIL;

  /* For (mem addr), we force_reg on addr here,
     so that nds32_expand_store_multiple can easily use it.  */
  operands[3] = nds32_expand_store_multiple (REGNO (operands[1]),
					     INTVAL (operands[2]),
					     force_reg (SImode,
							XEXP (operands[0], 0)),
					     operands[0],
					     false, NULL);
})

;; Ordinary Store Multiple.
(define_insn "*stm_bim_si25"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 100)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 80)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 84)))
	  (match_operand:SI 24 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 88)))
	  (match_operand:SI 25 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 92)))
	  (match_operand:SI 26 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 96)))
	  (match_operand:SI 27 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 26)"
  "smw.bim\t%3, [%1], %27, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "25")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si24"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 96)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 80)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 84)))
	  (match_operand:SI 24 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 88)))
	  (match_operand:SI 25 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 92)))
	  (match_operand:SI 26 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 25)"
  "smw.bim\t%3, [%1], %26, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "24")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si23"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 92)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 80)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 84)))
	  (match_operand:SI 24 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 88)))
	  (match_operand:SI 25 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 24)"
  "smw.bim\t%3, [%1], %25, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "23")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si22"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 88)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 80)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 84)))
	  (match_operand:SI 24 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 23)"
  "smw.bim\t%3, [%1], %24, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "22")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si21"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 84)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 80)))
	  (match_operand:SI 23 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 22)"
  "smw.bim\t%3, [%1], %23, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "21")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si20"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 80)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 76)))
	  (match_operand:SI 22 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 21)"
  "smw.bim\t%3, [%1], %22, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "20")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si19"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 76)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 72)))
	  (match_operand:SI 21 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 20)"
  "smw.bim\t%3, [%1], %21, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "19")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si18"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 72)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 68)))
	  (match_operand:SI 20 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 19)"
  "smw.bim\t%3, [%1], %20, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "18")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si17"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 68)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 64)))
	  (match_operand:SI 19 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 18)"
  "smw.bim\t%3, [%1], %19, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "17")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si16"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 64)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 60)))
	  (match_operand:SI 18 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 17)"
  "smw.bim\t%3, [%1], %18, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "16")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si15"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 60)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 56)))
	  (match_operand:SI 17 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 16)"
  "smw.bim\t%3, [%1], %17, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "15")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si14"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 56)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 52)))
	  (match_operand:SI 16 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 15)"
  "smw.bim\t%3, [%1], %16, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "14")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si13"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 52)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 48)))
	  (match_operand:SI 15 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 14)"
  "smw.bim\t%3, [%1], %15, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "13")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si12"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 48)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 44)))
	  (match_operand:SI 14 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 13)"
  "smw.bim\t%3, [%1], %14, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "12")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si11"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 44)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 40)))
	  (match_operand:SI 13 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 12)"
  "smw.bim\t%3, [%1], %13, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "11")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si10"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 40)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 36)))
	  (match_operand:SI 12 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 11)"
  "smw.bim\t%3, [%1], %12, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "10")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si9"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 36)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 32)))
	  (match_operand:SI 11 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 10)"
  "smw.bim\t%3, [%1], %11, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "9")
   (set_attr "length"              "4")]
)


(define_insn "*stm_bim_si8"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 32)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 28)))
	  (match_operand:SI 10 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 9)"
  "smw.bim\t%3, [%1], %10, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "8")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si7"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 28)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 24)))
	  (match_operand:SI 9 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 8)"
  "smw.bim\t%3, [%1], %9, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "7")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si6"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 24)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 20)))
	  (match_operand:SI 8 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 7)"
  "smw.bim\t%3, [%1], %8, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "6")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si5"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 20)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 16)))
	  (match_operand:SI 7 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 6)"
  "smw.bim\t%3, [%1], %7, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "5")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si4"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 16)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 12)))
	  (match_operand:SI 6 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 5)"
  "smw.bim\t%3, [%1], %6, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "4")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si3"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 12)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	  (match_operand:SI 5 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 4)"
  "smw.bim\t%3, [%1], %5, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "3")
   (set_attr "length"              "4")]
)

(define_insn "*stm_bim_si2"
  [(match_parallel 0 "nds32_store_multiple_and_update_address_operation"
    [(set (match_operand:SI 1 "register_operand" "=r")
	  (plus:SI (match_operand:SI 2 "register_operand" "1") (const_int 8)))
     (set (mem:SI (match_dup 2))
	  (match_operand:SI 3 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	  (match_operand:SI 4 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 3)"
  "smw.bim\t%3, [%1], %4, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "2")
   (set_attr "length"              "4")]
)

(define_expand "unaligned_store_update_base_w"
  [(parallel [(set (match_operand:SI 0 "register_operand" "=r")
		   (plus:SI (match_operand:SI 1 "register_operand" "0") (const_int 4)))
	      (set (mem:SI (match_dup 1))
		   (unspec:SI [(match_operand:SI 2 "register_operand" "r")] UNSPEC_UASTORE_W))])]
  ""
{
  emit_insn (gen_unaligned_store_w (gen_rtx_MEM (SImode, operands[1]), operands[2]));
  emit_insn (gen_addsi3 (operands[0], operands[1], gen_int_mode (4, Pmode)));
  DONE;
}
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "1")
   (set_attr "length"              "4")]
)

(define_expand "unaligned_store_update_base_dw"
  [(parallel [(set (match_operand:SI 0 "register_operand" "=r")
		   (plus:SI (match_operand:SI 1 "register_operand" "0") (const_int 8)))
	      (set (mem:DI (match_dup 1))
		   (unspec:DI [(match_operand:DI 2 "register_operand" "r")] UNSPEC_UASTORE_DW))])]
  ""
{
  /* DO NOT emit unaligned_store_w_m immediately since web pass don't
     recognize post_inc, try it again after GCC 5.0.
     REF: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63156  */
  emit_insn (gen_unaligned_store_dw (gen_rtx_MEM (DImode, operands[1]), operands[2]));
  emit_insn (gen_addsi3 (operands[0], operands[1], gen_int_mode (8, Pmode)));
  DONE;
}
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "2")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi25"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 80)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 84)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 88)))
	  (match_operand:SI 24 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 92)))
	  (match_operand:SI 25 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 96)))
	  (match_operand:SI 26 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 25)"
  "smw.bi\t%2, [%1], %26, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "25")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi24"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 80)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 84)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 88)))
	  (match_operand:SI 24 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 92)))
	  (match_operand:SI 25 "register_operand" ""))
])]
  "(XVECLEN (operands[0], 0) == 24)"
  "smw.bi\t%2, [%1], %25, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "24")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi23"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 80)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 84)))
	  (match_operand:SI 23 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 88)))
	  (match_operand:SI 24 "register_operand" ""))
])]
  "(XVECLEN (operands[0], 0) == 23)"
  "smw.bi\t%2, [%1], %24, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "23")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi22"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 80)))
	  (match_operand:SI 22 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 84)))
	  (match_operand:SI 23 "register_operand" ""))
])]
  "(XVECLEN (operands[0], 0) == 22)"
  "smw.bi\t%2, [%1], %23, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "22")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi21"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 80)))
	  (match_operand:SI 22 "register_operand" ""))
])]
  "(XVECLEN (operands[0], 0) == 21)"
  "smw.bi\t%2, [%1], %22, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "21")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi20"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 76)))
	  (match_operand:SI 21 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 20)"
  "smw.bi\t%2, [%1], %21, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "20")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi19"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 72)))
	  (match_operand:SI 20 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 19)"
  "smw.bi\t%2, [%1], %20, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "19")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi18"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 68)))
	  (match_operand:SI 19 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 18)"
  "smw.bi\t%2, [%1], %19, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "18")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi17"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 64)))
	  (match_operand:SI 18 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 17)"
  "smw.bi\t%2, [%1], %18, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "17")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi16"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 60)))
	  (match_operand:SI 17 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 16)"
  "smw.bi\t%2, [%1], %17, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "16")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi15"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 56)))
	  (match_operand:SI 16 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 15)"
  "smw.bi\t%2, [%1], %16, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "15")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi14"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 52)))
	  (match_operand:SI 15 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 14)"
  "smw.bi\t%2, [%1], %15, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "14")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi13"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 48)))
	  (match_operand:SI 14 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 13)"
  "smw.bi\t%2, [%1], %14, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "13")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi12"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 44)))
	  (match_operand:SI 13 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 12)"
  "smw.bi\t%2, [%1], %13, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "12")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi11"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 40)))
	  (match_operand:SI 12 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 11)"
  "smw.bi\t%2, [%1], %12, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "11")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi10"
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
	  (match_operand:SI 9 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 36)))
	  (match_operand:SI 11 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 10)"
  "smw.bi\t%2, [%1], %11, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"              "10")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi9"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
	  (match_operand:SI 4  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 12)))
	  (match_operand:SI 5  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 16)))
	  (match_operand:SI 6  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 20)))
	  (match_operand:SI 7  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 24)))
	  (match_operand:SI 8  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 28)))
	  (match_operand:SI 9  "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 32)))
	  (match_operand:SI 10 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 9)"
  "smw.bi\t%2, [%1], %10, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "9")
   (set_attr "length"              "4")]
)

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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "8")
   (set_attr "length"              "4")]
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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "7")
   (set_attr "length"              "4")]
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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "6")
   (set_attr "length"              "4")]
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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "5")
   (set_attr "length"              "4")]
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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "4")
   (set_attr "length"              "4")]
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
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "3")
   (set_attr "length"              "4")]
)

(define_insn "*stmsi2"
  [(match_parallel 0 "nds32_store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "register_operand" ""))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
	  (match_operand:SI 3 "register_operand" ""))])]
  "(XVECLEN (operands[0], 0) == 2)"
  "smw.bi\t%2, [%1], %3, 0x0"
  [(set_attr "type"   "store_multiple")
   (set_attr "combo"               "2")
   (set_attr "length"              "4")]
)

;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.
;;
;; operands[0] is the destination block of memory.
;; operands[1] is the source block of memory.
;; operands[2] is the number of bytes to move.
;; operands[3] is the known shared alignment.


(define_expand "cpymemsi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "nds32_reg_constant_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
{
  if (nds32_expand_cpymemsi (operands[0],
			     operands[1],
			     operands[2],
			     operands[3]))
    DONE;

  FAIL;
})

;; ------------------------------------------------------------------------

(define_expand "strlensi"
  [(match_operand:SI  0 "register_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:QI  2 "nds32_reg_constant_operand")
   (match_operand     3 "const_int_operand")]
  "TARGET_EXT_STRING"
{
  if (nds32_expand_strlen (operands[0], operands[1], operands[2], operands[3]))
    DONE;

  FAIL;
})

(define_expand "setmemsi"
   [(use (match_operand:BLK 0 "memory_operand"))
    (use (match_operand:SI 1 "nds32_reg_constant_operand"))
    (use (match_operand:QI 2 "nonmemory_operand"))
    (use (match_operand 3 "const_int_operand"))
    (use (match_operand:SI 4 "const_int_operand"))
    (use (match_operand:SI 5 "const_int_operand"))]
  ""
{
 if (nds32_expand_setmem (operands[0], operands[1],
			  operands[2], operands[3],
			  operands[4], operands[5]))
   DONE;

 FAIL;
})

;; ------------------------------------------------------------------------
