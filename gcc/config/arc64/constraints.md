;; Constraint definitions for Synopsys DesignWare ARC.
;; Copyright (C) 2019 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register constraints

;; Register suited for short instructions.
(define_register_constraint "q" "AC16_REGS"
  "Registers usable in short 16-bit like instructions: @code{r0}-@code{r3},
@code{r12}-@code{r15}")

;; Register suited for sibling calls.
(define_register_constraint "Sbreg" "SIBCALL_REGS"
  "@internal
   Sibling call register")

(define_register_constraint "w" "ARC64_HAS_FP_BASE ? FP_REGS : NO_REGS"
  "Floating point and SIMD vector registers.")

;; Register suited for floating point instructions [r0-r31]
(define_register_constraint "c" "CORE_REGS"
  "@internal
   Core register set")

;; Register suited for mov_s g,h instructions like.
;; FIXME! check if we get better code when it is register_constraint.
(define_constraint "h"
  "@internal
   Register usable in short move instructions: @code{r0}-@code{r31}"
  (and (match_code "reg")
       (match_test "REGNO (op) < ILINK_REGNUM")))

(define_constraint "accum"
  "@internal"
  (and (match_code "reg")
       (match_test "REGNO (op) == R58_REGNUM")))

(define_constraint "accrn"
  "@internal"
  (and (match_code "reg")
       (match_test "REGNO (op) == R58_REGNUM")))

; Usc constant is only used for storing long constants, hence we can
; have only [b,s9], and [b] types of addresses.
(define_memory_constraint "Ucnst" "@internal
   A valid memory operand for storing constants"
  (and (match_code "mem")
       (match_test "!CONSTANT_P (XEXP (op, 0))")
       (match_test "arc64_legitimate_store_address_p (mode, XEXP (op, 0))")))

(define_memory_constraint "Uldms" "@internal
  A valid memory operand for loading using short instructions"
  (and (match_code "mem")
       (match_test "arc64_short_access_p (op, mode, true)")))

(define_memory_constraint "Ustms" "@internal
  A valid memory operand for loading using short instructions"
  (and (match_code "mem")
       (match_test "arc64_short_access_p (op, mode, false)")))

(define_memory_constraint "Ufpms" "@internal
   A valid memory operand for floating point operations"
  (and (match_code "mem")
       (match_test "arc64_fp_access_p (op, mode)")))

(define_memory_constraint "Ustor" "@internal
   A valid memory operand for store instructions"
  (and (match_code "mem")
       (match_test "arc64_legitimate_store_address_p (mode, XEXP (op, 0))")))

(define_memory_constraint "Ustw6" "@internal
   A valid memory operand for restricted storing of w6 immediate"
  (and (match_code "mem")
       (match_test "!MEM_VOLATILE_P (op) || !TARGET_VOLATILE_DI")
       (match_test "arc64_legitimate_store_address_p (mode, XEXP (op, 0))")))

(define_constraint "Ustk<"
  "@internal
   Stack pre-decrement"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC")
       (match_test "REG_P (XEXP (XEXP (op, 0), 0))")
       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REGNUM")))

(define_constraint "Ustk>"
  "@internal
   Stack post-increment"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == POST_INC")
       (match_test "REG_P (XEXP (XEXP (op, 0), 0))")
       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REGNUM")))

;;; Internal immediate constraint used to split move instructions.
;(define_constraint "Cax"
;  "@internal
;  special const_int pattern used to split move insns"
;  (and (match_code "const_int")
;       (match_test "optimize_size")
;       (match_test "arc_check_mov_const (ival)")))

(define_constraint "BLsym"
  "@internal
  is a symbol reference allowed by the BL instruction"
  (and (match_code "symbol_ref")
       (match_test "!arc64_is_long_call_p (op)")))

(define_constraint "U06M1"
  "@internal
   An unsigned 6-bit integer constant, up to 62."
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (ival + 1)")))

(define_constraint "SymMV"
  "@internal
   Special constant/symbol which fits in limm field."
  (and (match_code "label_ref, symbol_ref")
       (match_test "arc64_allow_direct_access_p (op)")))

(define_constraint "SymIm"
  "@internal
   Special constant/symbol which fits in limm field."
  (ior (match_code "label_ref, symbol_ref")
       (and (match_code "const_int")
	    (match_test "SIGNED_INT32 (ival)"))))

(define_constraint "S32S0"
  "@internal
   Special constant/symbol which fits in limm field."
  (and (match_code "const_int")
       (ior (match_test "UNSIGNED_INT32 (ival)")
	    (match_test "SIGNED_INT32 (ival)"))))

(define_constraint "U32S0"
  "@internal
   Special constant/symbol which fits in limm field."
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT32 (ival)")))

(define_constraint "S06S0" "@internal
  A 6-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT6 (ival)")))

(define_constraint "SyPic"
  "@internal
   Special symbol used for PIC addressing."
  (match_code "unspec"))

(define_constraint "U06Sx" "@internal
  A 6-bit unsigned integer constant shifted by x-bit(s)"
  (and (match_code "const_int")
       (ior (match_test "UNSIGNED_INT9_SHIFTED (ival,3)")
	    (match_test "UNSIGNED_INT8_SHIFTED (ival,2)")
	    (match_test "UNSIGNED_INT7_SHIFTED (ival,1)")
	    (match_test "UNSIGNED_INT6 (ival)"))))

(define_constraint "N06Sx" "@internal
  A negate 6-bit unsigned integer constant shifted by x-bit(s) used by add."
  (and (match_code "const_int")
       (match_test "ival < 0")
       (match_test "SIGNED_INT10(ival)")
       (ior (match_test "UNSIGNED_INT9_SHIFTED (-ival,3)")
	    (match_test "UNSIGNED_INT8_SHIFTED (-ival,2)")
	    (match_test "UNSIGNED_INT7_SHIFTED (-ival,1)")
	    (match_test "UNSIGNED_INT6 (-ival)"))))

(define_constraint "S12Sx" "@internal
  A 12-bit signed integer constant shifted by x-bit(s)"
  (and (match_code "const_int")
       (ior (match_test "SIGNED_INT15_SHIFTED (ival,3)")
	    (match_test "SIGNED_INT14_SHIFTED (ival,2)")
	    (match_test "SIGNED_INT13_SHIFTED (ival,1)")
	    (match_test "SIGNED_INT12 (ival)"))))

(define_constraint "S03MV" "@internal
  A 3-bit Integer signed constant in the interval [-1,6]"
  (and (match_code "const_int")
       (match_test "(ival >= -1) && (ival <= 6)")))

(define_constraint "Z"
  "Match single precision and a floating-point zero"
  (and (match_code "const_double")
       (ior (match_test "op == CONST0_RTX (DFmode)
                         || op == CONST0_RTX (SFmode)"))))

(define_constraint "G" "@internal
  Match single precision and a floating-point zero"
  (and (match_code "const_double")
       (ior (match_test "op == CONST0_RTX (DFmode)")
	    (match_test "GET_MODE_SIZE (GET_MODE (op)) <= 4"))))

(define_constraint "U0000" "@internal
  Match const int 0"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "U0001" "@internal
  Match const int 1"
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "U0008" "@internal
  Match const int 8"
  (and (match_code "const_int")
       (match_test "ival == 8")))

(define_constraint "U0016" "@internal
  Match const int 16"
  (and (match_code "const_int")
       (match_test "ival == 16")))

;---------------------------------------------------------

(define_constraint "U06S0" "@internal
  A 6-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT6 (ival)")))

(define_constraint "U06S1" "@internal
  A 6-bit unsigned integer constant shifted by 1-bit(s)"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT7_SHIFTED (ival,1)")))

(define_constraint "U06S2" "@internal
  A 6-bit unsigned integer constant shifted by 2-bit(s)"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT8_SHIFTED (ival,2)")))

(define_constraint "U06S3" "@internal
  A 6-bit unsigned integer constant shifted by 3-bit(s)"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT9_SHIFTED (ival,3)")))

(define_constraint "S12S0" "@internal
  A 12-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT12 (ival)")))

(define_constraint "S12S1" "@internal
  A 12-bit signed integer constant shifted by 1-bit(s)"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT13_SHIFTED (ival,1)")))

(define_constraint "S12S2" "@internal
  A 12-bit signed integer constant shifted by 2-bit(s)"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT14_SHIFTED (ival,2)")))

(define_constraint "S12S3" "@internal
  A 12-bit signed integer constant shifted by 3-bit(s)"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT15_SHIFTED (ival,3)")))

(define_constraint "S03S0" "@internal
  A 3-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT3 (ival)")))

(define_constraint "U07S0" "@internal
  A 7-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT7 (ival)")))

(define_constraint "U03S0" "@internal
  A 3-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT3 (ival)")))

(define_constraint "S11S0" "@internal
  A 11-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT11 (ival)")))

(define_constraint "U05S0" "@internal
  A 5-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT5 (ival)")))

(define_constraint "S09S0" "@internal
  A 9-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT9 (ival)")))

(define_constraint "S21S0" "@internal
  A 21-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT21 (ival)")))

(define_constraint "S25S0" "@internal
  A 25-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT25 (ival)")))

(define_constraint "S10S0" "@internal
  A 10-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT10 (ival)")))

(define_constraint "S07S0" "@internal
  A 7-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT7 (ival)")))

(define_constraint "S13S0" "@internal
  A 13-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT13 (ival)")))

(define_constraint "S08S0" "@internal
  A 8-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT8 (ival)")))

(define_constraint "U10S0" "@internal
  A 10-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT10 (ival)")))

(define_constraint "U08S0" "@internal
  A 8-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT8 (ival)")))

(define_constraint "U09S0" "@internal
  A 9-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT9 (ival)")))

(define_constraint "U12S0" "@internal
  A 16-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT12 (ival)")))

(define_constraint "U16S0" "@internal
  A 16-bit unsigned integer constant"
  (and
    (match_code "const_int")
    (match_test "UNSIGNED_INT16 (ival)")))

(define_constraint "S16S0" "@internal
  A 16-bit signed integer constant"
  (and
    (match_code "const_int")
    (match_test "SIGNED_INT16 (ival)")))


; TODO: Below this line definition should be corrected
(define_constraint "SR_R0"
  "@internal
   @code{R0} register."
  (and (match_code "reg")
       (match_test "REGNO (op) == R0_REGNUM")))

(define_constraint "SR_R1"
  "@internal
   @code{R1} register."
  (and (match_code "reg")
       (match_test "REGNO (op) == R1_REGNUM")))

(define_constraint "SR_SP"
  "@internal
   @code{SP} register."
  (and (match_code "reg")
       (match_test "REGNO (op) == SP_REGNUM")))

; TODO: FIX THIS
(define_constraint "SR_GP"
  "@internal
   @code{GP} register."
  (and (match_code "reg")
       (match_test "REGNO (op) == R0_REGNUM")))

; TODO: FIX THIS
(define_constraint "SRPCL"
  "@internal
   @code{PCL} register."
  (and (match_code "reg")
       (match_test "REGNO (op) == R0_REGNUM")))

;; Memory constraint used for atomic ops.
(define_memory_constraint "ATOMC"
  "A memory with only a base register"
  (match_operand 0 "mem_noofs_operand"))

