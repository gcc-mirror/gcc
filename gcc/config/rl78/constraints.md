;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2020 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

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

; Constraints in use:

; core:
; V X g i m n o p r s < >
; 0..9
; I..Q - integers
;   Int8 = 0..255
;   Int3 = 1..7
;   J = -255..0
;   K = 1
;   L = -1
;   M = 0
;   N = 2
;   O = -2
;   P = 1..15

; E..H - float constants

; RL78-specific
; a x b c d e h l w - 8-bit regs
; A B D T S - 16-bit regs
; R = all regular registers (A-L)
; Y - any valid memory
; Wxx - various memory addressing modes
; Qxx - conditionals
; U = usual memory references mov-able to/from AX
; v = virtual registers
; Zxx = specific virtual registers

(define_constraint "Int8"
  "Integer constant in the range 0 @dots{} 255."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 255)")))

(define_constraint "Int3"
  "Integer constant in the range 1 @dots{} 7."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 7)")))

(define_constraint "Iv08"
  "@internal
   Integer constant equal to 8."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 8, 8)")))

(define_constraint "Iv16"
  "@internal
   Integer constant equal to 16."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 16, 16)")))

(define_constraint "Iv24"
  "@internal
   Integer constant equal to 24."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 24, 24)")))

(define_constraint "Is09"
  "@internal
   Integer constant in the range 9 @dots{} 15 (for shifts)."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 9, 15)")))

(define_constraint "Is17"
  "@internal
   Integer constant in the range 17 @dots{} 23 (for shifts)."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 17, 23)")))

(define_constraint "Is25"
  "@internal
   Integer constant in the range 25 @dots{} 31 (for shifts)."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 25, 31)")))

(define_constraint "ISsi"
  "@internal
   Integer constant with bit 31 set."
  (and (match_code "const_int")
       (match_test "(ival & 0x80000000) != 0")))

(define_constraint "IShi"
  "@internal
   Integer constant with bit 15 set."
  (and (match_code "const_int")
       (match_test "(ival & 0x8000) != 0")))

(define_constraint "ISqi"
  "@internal
   Integer constant with bit 7 set."
  (and (match_code "const_int")
       (match_test "(ival & 0x80) != 0")))

(define_constraint "Ibqi"
  "@internal
   Integer constant with one bit in 0..7 set."
  (and (match_code "const_int")
       (match_test "(ival & 0xff) && (exact_log2 (ival & 0xff) >= 0)")))
(define_constraint "IBqi"
  "@internal
   Integer constant with one bit in 0..7 clear."
  (and (match_code "const_int")
       (match_test "(~ival & 0xff) && (exact_log2 (~ival & 0xff) >= 0)")))

(define_constraint "J"
  "Integer constant in the range -255 @dots{} 0"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -255, 0)")))

(define_constraint "K"
  "Integer constant 1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 1)")))

(define_constraint "L"
  "Integer constant -1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -1, -1)")))

(define_constraint "M"
  "Integer constant 0."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 0)")))

(define_constraint "N"
  "Integer constant 2."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 2, 2)")))

(define_constraint "O"
  "Integer constant -2."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -2, -2)")))

(define_constraint "P"
  "Integer constant 1..15"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 15)")))

(define_register_constraint "R" "QI_REGS"
 "@code{A} through @code{L} registers.")

(define_register_constraint "a" "AREG"
 "The @code{A} register.")

(define_register_constraint "x" "XREG"
 "The @code{X} register.")

(define_register_constraint "b" "BREG"
 "The @code{B} register.")

(define_register_constraint "c" "CREG"
 "The @code{C} register.")

(define_register_constraint "d" "DREG"
 "The @code{D} register.")

(define_register_constraint "e" "EREG"
 "The @code{E} register.")

(define_register_constraint "h" "HREG"
 "The @code{H} register.")

(define_register_constraint "l" "LREG"
 "The @code{L} register.")

(define_register_constraint "w" "PSWREG"
 "The @code{PSW} register.")

(define_register_constraint "A" "AXREG"
 "The @code{AX} register.")

(define_register_constraint "B" "BCREG"
 "The @code{BC} register.")

(define_register_constraint "D" "DEREG"
 "The @code{DE} register.")

; because H + L = T, assuming A=1.
(define_register_constraint "T" "HLREG"
 "The @code{HL} register.")

(define_register_constraint "S" "SPREG"
 "The @code{SP} register.")

(define_register_constraint "v" "V_REGS"
 "The virtual registers.")

(define_register_constraint "Z08W" "R8W_REGS"
 "The R8 register, HImode.")

(define_register_constraint "Z10W" "R10W_REGS"
 "The R10 register, HImode.")

(define_register_constraint "Zint" "INT_REGS"
 "The interrupt registers.")

; All the memory addressing schemes the RL78 supports
; of the form W {register} {bytes of offset}
;          or W {register} {register}
; Additionally, the Cxx forms are the same as the Wxx forms, but without
; the ES: override.

; absolute address
(define_memory_constraint "Cab"
  "[addr]"
  (and (match_code "mem")
       (ior (match_test "CONSTANT_P (XEXP (op, 0))")
	    (match_test "GET_CODE (XEXP (op, 0)) == PLUS && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF"))
	    )
  )
(define_memory_constraint "Wab"
  "es:[addr]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cab (rl78_es_base (op)))
               || satisfies_constraint_Cab (op)")
  )

(define_memory_constraint "Cbc"
  "word16[BC]"
  (and (match_code "mem")
       (ior
	(and (match_code "reg" "0")
	     (match_test "REGNO (XEXP (op, 0)) == BC_REG"))
	(and (match_code "plus" "0")
	     (and (and (match_code "reg" "00")
		       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == BC_REG"))
		       (match_test "uword_operand (XEXP (XEXP (op, 0), 1), VOIDmode)"))))
       )
  )
(define_memory_constraint "Wbc"
  "es:word16[BC]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cbc (rl78_es_base (op)))
               || satisfies_constraint_Cbc (op)")
  )

(define_memory_constraint "Cde"
  "[DE]"
  (and (match_code "mem")
       (and (match_code "reg" "0")
	    (match_test "REGNO (XEXP (op, 0)) == DE_REG")))
  )
(define_memory_constraint "Wde"
  "es:[DE]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cde (rl78_es_base (op)))
               || satisfies_constraint_Cde (op)")
  )

(define_memory_constraint "Cca"
  "[AX..HL] for calls"
  (and (match_code "mem")
       (and (match_code "reg" "0")
	    (match_test "REGNO (XEXP (op, 0)) <= HL_REG")))
  )
(define_memory_constraint "Wca"
  "es:[AX..HL] for calls"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cca (rl78_es_base (op)))
               || satisfies_constraint_Cca (op)")
  )

(define_memory_constraint "Ccv"
  "[AX..HL,r8-r31] for calls"
  (and (match_code "mem")
       (and (match_code "reg" "0")
	    (match_test "REGNO (XEXP (op, 0)) < 32")))
  )
(define_memory_constraint "Wcv"
  "es:[AX..HL,r8-r31] for calls"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Ccv (rl78_es_base (op)))
               || satisfies_constraint_Ccv (op)")
  )

(define_memory_constraint "Cd2"
  "word16[DE]"
  (and (match_code "mem")
       (ior
	(and (match_code "reg" "0")
	     (match_test "REGNO (XEXP (op, 0)) == DE_REG"))
	(and (match_code "plus" "0")
	     (and (and (match_code "reg" "00")
		       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == DE_REG"))
		       (match_test "uword_operand (XEXP (XEXP (op, 0), 1), VOIDmode)"))))
       )
  )
(define_memory_constraint "Wd2"
  "es:word16[DE]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cd2 (rl78_es_base (op)))
               || satisfies_constraint_Cd2 (op)")
  )

(define_memory_constraint "Chl"
  "[HL]"
  (and (match_code "mem")
       (and (match_code "reg" "0")
	    (match_test "REGNO (XEXP (op, 0)) == HL_REG")))
  )
(define_memory_constraint "Whl"
  "es:[HL]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Chl (rl78_es_base (op)))
               || satisfies_constraint_Chl (op)")
  )

(define_memory_constraint "Ch1"
  "byte8[HL]"
  (and (match_code "mem")
       (and (match_code "plus" "0")
	    (and (and (match_code "reg" "00")
		      (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == HL_REG"))
		      (match_test "ubyte_operand (XEXP (XEXP (op, 0), 1), VOIDmode)"))))
  )
(define_memory_constraint "Wh1"
  "es:byte8[HL]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Ch1 (rl78_es_base (op)))
               || satisfies_constraint_Ch1 (op)")
  )

(define_memory_constraint "Chb"
  "[HL+B]"
  (and (match_code "mem")
       (match_test "rl78_hl_b_c_addr_p (XEXP (op, 0))"))
  )
(define_memory_constraint "Whb"
  "es:[HL+B]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Chb (rl78_es_base (op)))
               || satisfies_constraint_Chb (op)")
  )

(define_memory_constraint "Cs1"
  "word8[SP]"
  (and (match_code "mem")
       (ior
	(and (match_code "reg" "0")
	     (match_test "REGNO (XEXP (op, 0)) == SP_REG"))
	(and (match_code "plus" "0")
	     (and (and (match_code "reg" "00")
		       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REG"))
		       (and (match_code "const_int" "01")
		            (match_test "IN_RANGE (INTVAL (XEXP (XEXP (op, 0), 1)), 0, 256 - GET_MODE_SIZE (GET_MODE (op)))")))))
       )
  )

(define_memory_constraint "Ws1"
  "es:word8[SP]"
  (match_test "(rl78_es_addr (op) && satisfies_constraint_Cs1 (rl78_es_base (op)))
               || satisfies_constraint_Cs1 (op)")
  )

(define_constraint "Wfr"
  "ES/CS far pointer"
  (and (match_code "mem")
       (match_test "rl78_far_p (op)"))
  )

(define_memory_constraint "Wsa"
  "any SADDR memory access"
  (and (match_code "mem")
       (match_test "rl78_saddr_p (op)"))
)

(define_memory_constraint "Wsf"
  "any SFR memory access"
  (and (match_code "mem")
       (match_test "rl78_sfr_p (op)"))
)

(define_memory_constraint "Y"
  "any near legitimate memory access"
  (and (match_code "mem")
       (match_test "!rl78_far_p (op) && rl78_as_legitimate_address (VOIDmode, XEXP (op, 0), true, ADDR_SPACE_GENERIC)"))
)

(define_memory_constraint "U"
  "memory references valid with mov to/from a/ax"
  (and (match_code "mem")
       (match_test "rl78_virt_insns_ok ()
|| satisfies_constraint_Wab (op)
|| satisfies_constraint_Wbc (op)
|| satisfies_constraint_Wde (op)
|| satisfies_constraint_Wd2 (op)
|| satisfies_constraint_Whl (op)
|| satisfies_constraint_Wh1 (op)
|| satisfies_constraint_Whb (op)
|| satisfies_constraint_Ws1 (op)
|| satisfies_constraint_Wfr (op) ")))

(define_memory_constraint "Qbi"
  "built-in compare types"
  (match_code "eq,ne,gtu,ltu,geu,leu"))

(define_memory_constraint "Qsc"
  "synthetic compares"
  (match_code "gt,lt,ge,le"))

(define_constraint "Qs8"
  "Integer constant computed from (SUBREG (SYMREF))."
  (and (match_code "subreg")
       (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF"))
)
