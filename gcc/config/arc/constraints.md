;; Constraint definitions for Synopsys DesignWare ARC.
;; Copyright (C) 2007-2022 Free Software Foundation, Inc.
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

; Most instructions accept arbitrary core registers for their inputs, even
; if the core register in question cannot be written to, like the multiply
; result registers of ARC600.
; First, define a class for core registers that can be read cheaply.  This
; is most or all core registers for ARC600, but only r0-r31 for ARC700
(define_register_constraint "c" "GENERAL_REGS"
  "Legacy, core register @code{r0}-@code{r31}, @code{ap},@code{pcl}")

; All core regs - e.g. for when we must have a way to reload a register.
(define_register_constraint "Rac" "GENERAL_REGS"
  "Legacy, core register @code{r0}-@code{r60}, @code{ap},@code{pcl}")

; Some core registers (.e.g lp_count) aren't general registers because they
; can't be used as the destination of a multi-cycle operation like
; load and/or multiply, yet they are still writable in the sense that
; register-register moves and single-cycle arithmetic (e.g "add", "and",
; but not "mpy") can write to them.
(define_register_constraint "w" "GENERAL_REGS"
  "Legacy, writable core register: @code{r0}-@code{r31}, @code{r60},
   nonfixed core register")

(define_register_constraint "W" "GENERAL_REGS"
  "Legacy, writable core register except @code{LP_COUNT} (@code{r60}):
   @code{r0}-@code{r31}, nonfixed core register")

(define_constraint "l"
  "@internal
   Loop count register @code{r60}"
  (and (match_code "reg")
       (match_test "REGNO (op) == LP_COUNT")))

(define_register_constraint "x" "R0_REGS"
  "@code{R0} register.")

(define_register_constraint "q" "ARCOMPACT16_REGS"
  "Registers usable in ARCompact 16-bit instructions: @code{r0}-@code{r3},
   @code{r12}-@code{r15}")

; NPS400 bitfield instructions require registers from the r0-r3,r12-r15
; range, and thus we need a register class and constraint that works
; independently of size optimization.
(define_register_constraint
 "Rrq" "TARGET_RRQ_CLASS ? ARCOMPACT16_REGS : NO_REGS"
  "Registers usable in NPS400 bitfield instructions: @code{r0}-@code{r3},
   @code{r12}-@code{r15}")

(define_register_constraint "D" "DOUBLE_REGS"
  "ARC FPX (dpfp) 64-bit registers. @code{D0}, @code{D1}")

(define_register_constraint "d" "SIMD_DMA_CONFIG_REGS"
  "@internal
   ARC SIMD DMA configuration registers @code{di0}-@code{di7},
   @code{do0}-@code{do7}")

(define_register_constraint "v" "SIMD_VR_REGS"
  "ARC SIMD 128-bit registers @code{VR0}-@code{VR23}")

; We could allow call-saved registers for sibling calls if we restored them
; in the delay slot of the call.  However, that would not allow to adjust the
; stack pointer afterwards, so the call-saved register would have to be
; restored from a call-used register that was just loaded with the value
; before.  So sticking to call-used registers for sibcalls will likely
; generate better code overall.
(define_register_constraint "Rsc" "SIBCALL_REGS"
  "@internal
   Sibling call register")

;; Integer constraints

(define_constraint "I"
  "@internal
   A signed 12-bit integer constant."
  (and (match_code "const_int")
       (match_test "SIGNED_INT12 (ival)")))

(define_constraint "K"
  "@internal
   A 3-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT3 (ival)")))

(define_constraint "L"
  "@internal
   A 6-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (ival)")))

(define_constraint "CnL"
  "@internal
   One's complement of a 6-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (~ival)")))

(define_constraint "CmL"
  "@internal
   Two's complement of a 6-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (-ival)")))

(define_constraint "C16"
  "@internal
   A 16-bit signed integer constant"
  (and (match_code "const_int")
       (match_test "SIGNED_INT16 (ival)")))

(define_constraint "M"
  "@internal
   A 5-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT5 (ival)")))

(define_constraint "N"
  "@internal
   Integer constant 1"
  (and (match_code "const_int")
       (match_test "IS_ONE (ival)")))

(define_constraint "O"
  "@internal
   A 7-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT7 (ival)")))

(define_constraint "P"
  "@internal
   An 8-bit unsigned integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT8 (ival)")))

(define_constraint "C_0"
  "@internal
   Zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Cn0"
  "@internal
   Negative or zero"
  (and (match_code "const_int")
       (match_test "ival <= 0")))

(define_constraint "Cca"
  "@internal
   Conditional or three-address add / sub constant"
  (and (match_code "const_int")
       (match_test "ival == (HOST_WIDE_INT)(HOST_WIDE_INT_M1U << 31)
		    || (ival >= -0x1f8 && ival <= 0x1f8
			&& ((ival >= 0 ? ival : -ival)
			    <= 0x3f * (ival & -ival)))")))

; intersection of "O" and "Cca".
(define_constraint "CL2"
  "@internal
   A 6-bit unsigned integer constant times 2"
  (and (match_code "const_int")
       (match_test "!(ival & ~126)")))

(define_constraint "CM4"
  "@internal
   A 5-bit unsigned integer constant times 4"
  (and (match_code "const_int")
       (match_test "!(ival & ~124)")))

(define_constraint "Csp"
  "@internal
   A valid stack pointer offset for a short add"
  (and (match_code "const_int")
       (match_test "!(ival & ~124) || !(-ival & ~124)")))

(define_constraint "C2a"
  "@internal
   Unconditional two-address add / sub constant"
  (and (match_code "const_int")
       (match_test "ival == (HOST_WIDE_INT) (HOST_WIDE_INT_M1U << 31)
		    || (ival >= -0x4000 && ival <= 0x4000
			&& ((ival >= 0 ? ival : -ival)
			    <= 0x7ff * (ival & -ival)))")))

(define_constraint "C0p"
 "@internal
  power of two"
  (and (match_code "const_int")
       (match_test "IS_POWEROF2_P (ival & 0xffffffff)")))

(define_constraint "C1p"
 "@internal
  constant such that x+1 is a power of two, and x != 0"
  (and (match_code "const_int")
       (match_test "ival && IS_POWEROF2_P (ival + 1)")))

(define_constraint "C2p"
 "@internal
  constant such that (~x)+1 is a power of two, and x < -1"
  (and (match_code "const_int")
       (match_test "TARGET_V2
		    && (ival < -1)
		    && IS_POWEROF2_P ((~ival) + 1)")))

(define_constraint "C3p"
 "@internal
  constant int used to select xbfu a,b,u6 instruction.  The values accepted are 1 and 2."
  (and (match_code "const_int")
       (match_test "((ival == 1) || (ival == 2))")))

(define_constraint "Ccp"
 "@internal
  constant such that ~x (one's Complement) is a power of two"
  (and (match_code "const_int")
       (match_test "IS_POWEROF2_P (~ival)")))

(define_constraint "Cux"
 "@internal
  constant such that AND gives an unsigned extension"
  (and (match_code "const_int")
       (match_test "ival == 0xff || ival == 0xffff")))

(define_constraint "Chs"
 "@internal
  constant for a highpart that can be checked with a shift (asr.f 0,rn,m)"
  (and (match_code "const_int")
       (match_test "IS_POWEROF2_P (-ival)")
       (match_test "TARGET_BARREL_SHIFTER")))

(define_constraint "Clo"
 "@internal
  constant that fits into 16 lower bits, for movl"
  (and (match_code "const_int")
       (match_test "TARGET_NPS_BITOPS")
       (match_test "(ival & ~0xffffU) == 0")))

(define_constraint "Chi"
 "@internal
  constant that fits into 16 higher bits, for movh_i"
  (and (match_code "const_int")
       (match_test "TARGET_NPS_BITOPS")
       (match_test "trunc_int_for_mode (ival >> 16, HImode) << 16 == ival")))

(define_constraint "Cbf"
 "@internal
  a mask for a bit field, for AND using movb_i"
  (and (match_code "const_int")
       (match_test "TARGET_NPS_BITOPS")
       (match_test "IS_POWEROF2_OR_0_P (ival + (ival & -ival))")))

(define_constraint "Cbn"
 "@internal
  a constant integer, valid only if TARGET_NPS_BITOPS is true"
  (and (match_code "const_int")
       (match_test "TARGET_NPS_BITOPS")))

(define_constraint "C18"
 "@internal
  1,2,4 or 8"
  (and (match_code "const_int")
       (match_test "ival == 1 || ival == 2 || ival == 4 || ival == 8")))

(define_constraint "Cbi"
 "@internal
  constant that can be loaded with movbi.cl"
  (and (match_code "const_int")
       (match_test "TARGET_NPS_BITOPS")
       (match_test "!ival
		    || ((ival & 0xffffffffUL) >> exact_log2 (ival & -ival)
			<= 0xff)")))

(define_constraint "C0x"
  "@internal
  special const_int pattern used to split ior insns"
  (and (match_code "const_int")
       (match_test "optimize_size")
       (match_test "arc_check_ior_const (ival)")))

(define_constraint "Cax"
  "@internal
  special const_int pattern used to split mov insns"
  (and (match_code "const_int")
       (match_test "optimize_size")
       (match_test "arc_check_mov_const (ival)")))

;; Floating-point constraints

(define_constraint "G"
  "@internal
   A 32-bit constant double value"
  (and (match_code "const_double")
       (match_test "arc_double_limm_p (op)")))

(define_constraint "H"
  "@internal
   All const_double values (including 64-bit values)"
  (and (match_code "const_double")
       (match_test "1")))

(define_constraint "CfZ"
  "@internal
   Match a floating-point zero"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (SFmode)")))

;; Memory constraints
(define_memory_constraint "T"
  "@internal
   A valid memory operand for ARCompact load instructions"
  (and (match_code "mem")
       (match_test "compact_memory_operand_p (op, mode, false, false)")))

(define_memory_constraint "Uts"
  "@internal
   A valid memory operand for ARCompact load instructions scaled"
  (and (match_code "mem")
       (match_test "compact_memory_operand_p (op, mode, false, TARGET_CODE_DENSITY)")))

(define_memory_constraint "S"
  "@internal
   A valid memory operand for ARCompact store instructions"
  (and (match_code "mem")
       (match_test "compact_store_memory_operand (op, VOIDmode)")))

(define_memory_constraint "Uex"
  "@internal
   A valid memory operand for limm-free extend instructions"
  (and (match_code "mem")
       (match_test "!cmem_address (XEXP (op, 0), SImode)")
       (not (match_operand 0 "long_immediate_loadstore_operand"))))

(define_memory_constraint "Usd"
   "@internal
    A valid _small-data_ memory operand for ARCompact instructions"
   (and (match_code "mem")
	(match_test "compact_sda_memory_operand (op, VOIDmode, true)")))

; Usc constant is only used for storing long constants, hence we can
; have only [b,s9], and [b] types of addresses.
(define_memory_constraint "Usc"
  "@internal
   A valid memory operand for storing constants"
  (and (match_code "mem")
       (match_test "!CONSTANT_P (XEXP (op,0))")))

(define_constraint "Us<"
  "@internal
   Stack pre-decrement"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC")
       (match_test "REG_P (XEXP (XEXP (op, 0), 0))")
       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REG")))

(define_constraint "Us>"
  "@internal
   Stack post-increment"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == POST_INC")
       (match_test "REG_P (XEXP (XEXP (op, 0), 0))")
       (match_test "REGNO (XEXP (XEXP (op, 0), 0)) == SP_REG")))

(define_constraint "Ucm"
  "@internal
  cmem access"
  (and (match_code "mem")
       (match_test "TARGET_NPS_CMEM && cmem_address (XEXP (op, 0), VOIDmode)")))

;; General constraints

(define_constraint "Cbr"
  "Branch destination"
  (ior (and (match_code "symbol_ref")
	    (match_test "!arc_is_longcall_p (op)"))
       (match_code "label_ref")))

(define_constraint "Cbp"
  "predicable branch/call destination"
  (ior (and (match_code "symbol_ref")
	    (match_test "arc_is_shortcall_p (op)"))
       (match_code "label_ref")))

(define_constraint "Cji"
  "JLI call"
  (and (match_code "symbol_ref")
       (match_test "TARGET_CODE_DENSITY")
       (match_test "arc_is_jli_call_p (op)")))

(define_constraint "Csc"
  "Secure call"
  (and (match_code "symbol_ref")
       (match_test "TARGET_CODE_DENSITY")
       (match_test "TARGET_EM")
       (match_test "arc_is_secure_call_p (op)")))

(define_constraint "Cpc"
  "pc-relative constant"
  (match_test "arc_legitimate_pic_addr_p (op)"))

(define_constraint "Clb"
  "label"
  (and (match_code "label_ref")
       (match_test "arc_text_label (as_a <rtx_insn *> (XEXP (op, 0)))")))

(define_constraint "Cal"
  "constant for arithmetic/logical operations"
  (match_test "immediate_operand (op, VOIDmode) && !arc_legitimate_pic_addr_p (op)"))

(define_constraint "C32"
  "32 bit constant for arithmetic/logical operations"
  (match_test "immediate_operand (op, VOIDmode)
	       && !arc_legitimate_pic_addr_p (op)
	       && !satisfies_constraint_I (op)"))

(define_constraint "Csz"
  "a 32 bit constant avoided when compiling for size."
  (match_test "immediate_operand (op, VOIDmode)
	       && !arc_legitimate_pic_addr_p (op)
	       && !(satisfies_constraint_I (op) && optimize_size)"))

(define_constraint "Rcb"
  "@internal
   Stack Pointer register @code{r28} - do not reload into its class"
  (and (match_code "reg")
       (match_test "REGNO (op) == 28")))

(define_constraint "Rck"
  "@internal
   blink (usful for push_s / pop_s)"
  (and (match_code "reg")
       (match_test "REGNO (op) == 31")))

(define_constraint "Rcc"
  "@internal
  Condition Codes"
  (and (match_code "reg") (match_test "cc_register (op, VOIDmode)")))

(define_constraint "Ral"
  "@internal
   Accumulator register @code{ACCL} - do not reload into its class"
  (and (match_code "reg")
       (match_test "REGNO (op) == ACCL_REGNO")))

(define_constraint "Q"
  "@internal
   Integer constant zero"
  (and (match_code "const_int")
       (match_test "IS_ZERO (ival)")))

(define_constraint "Cm1"
  "@internal
   Integer signed constant in the interval [-1,6]"
  (and (match_code "const_int")
       (match_test "(ival >= -1) && (ival <=6)")
       (match_test "TARGET_V2")))

(define_constraint "Cm2"
  "@internal
   A signed 9-bit integer constant."
  (and (match_code "const_int")
       (match_test "(ival >= -256) && (ival <=255)")))

(define_constraint "Cm3"
  "@internal
   A signed 6-bit integer constant."
  (and (match_code "const_int")
       (match_test "(ival >= -32) && (ival <=31)")
       (match_test "TARGET_V2")))

(define_constraint "C62"
  "@internal
   An unsigned 6-bit integer constant, up to 62."
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT6 (ival - 1)")))

;; Memory constraint used for atomic ops.
(define_memory_constraint "ATO"
  "A memory with only a base register"
  (match_operand 0 "mem_noofs_operand"))

(define_constraint "J12"
  "@internal
   An unsigned 12-bit integer constant."
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT12 (ival)")))

(define_constraint "J16"
  "@internal
   An unsigned 16-bit integer constant"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT16 (ival)")))

; Memory addresses suited for code density load ops
(define_memory_constraint "Ucd"
  "@internal
   A valid memory operand for use with code density load ops"
  (and (match_code "mem")
       (match_test "compact_memory_operand_p (op, mode, true, false)")
       (match_test "TARGET_V2")))

(define_register_constraint "h"
  "TARGET_V2 ? AC16_H_REGS : NO_REGS"
  "5-bit h register set except @code{r30} and @code{r29}:
   @code{r0}-@code{r31}, nonfixed core register")

; Code density registers
(define_register_constraint "Rcd"
  "TARGET_CODE_DENSITY ? R0R3_CD_REGS : NO_REGS"
  "@internal
   core register @code{r0}-@code{r3}")

(define_register_constraint "Rsd"
  "TARGET_CODE_DENSITY ? R0R1_CD_REGS : NO_REGS"
  "@internal
   core register @code{r0}-@code{r1}")

(define_register_constraint "Rzd"
  "TARGET_CODE_DENSITY ? R0_REGS : NO_REGS"
  "@internal
   @code{r0} register for code density instructions.")
