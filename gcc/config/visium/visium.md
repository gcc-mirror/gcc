;; Machine description for Visium.
;; Copyright (C) 2002-2017 Free Software Foundation, Inc.
;; Contributed by C.Nettleton, J.P.Parkes and P.Garbett.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Extra register constraints are:
;;   'b'   EAM register mdb
;;   'c'   EAM register mdc
;;   'f'   Floating-point register
;;   'k'   Register that can be used as the target of a sibcall, i.e. call-used
;;         general register not clobbered in the epilogue: r1-r8 and r10
;;   'l'   Low general register, i.e. general register accessible in user mode
;;         on the GR6 and, consequently, that can be used as the target of a
;;         branch with prediction: r1-r28
;;   't'   Register r1
;;   'u'   Register r2
;;   'v'   Register r3
;;
;; Immediate integer operand constraints are:
;;   'J'  0 .. 65535     (16-bit immediate)
;;   'K'  1 .. 31        (5-bit immediate)
;;   'L'  -1 .. -65535   (16-bit negative immediate)
;;   'M'  -1             (minus one)
;;   'O'  0              (integer zero)
;;   'P'  32             (thirty two)
;;
;; Immediate FP operand constraints are:
;;   'G'  0.0            (floating-point zero)
;;
;; Operand substitution characters are:
;;   %#   delay slot follows, if empty, fill with NOP
;;   %b   LS 8 bits of immediate operand
;;   %w   LS 16 bits of immediate operand
;;   %u   MS 16 bits of immediate operand
;;   %r   register or zero (r0)
;;   %f   FP register or zero (f0)
;;   %d   second register in a pair
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Registers by name.
(define_constants [
  (R_R1          1)
  (R_R2          2)
  (R_R3          3)
  (R_R4          4)
  (R_R5          5)
  (R_R6          6)
  (R_LINK	21)
  (R_FP		22)
  (R_SP		23)
  (R_MDB	32)
  (R_MDC	33)
  (R_FLAGS	50)
])

;; UNSPEC usage.
(define_c_enum "unspec" [
  UNSPEC_MDBHI
  UNSPEC_FLOAD
  UNSPEC_FSTORE
  UNSPEC_ITOF
  UNSPEC_FTOI
  UNSPEC_NOP
  UNSPEC_ADDV
  UNSPEC_SUBV
  UNSPEC_NEGV
])

;; UNSPEC_VOLATILE usage.
(define_c_enum "unspecv" [
  UNSPECV_BLOCKAGE
  UNSPECV_DSI
])

(include "predicates.md")
(include "constraints.md")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Attributes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Instruction type.
;
;imm_reg       Move of immediate value to register.
;mem_reg       Move from memory to register.
;eam_reg       Move from EAM to register.
;fp_reg        Move from FPU to register.
;reg_mem       Move from register to memory.
;reg_eam       Move from register to EAM.
;reg_fp        Move from register to FPU.
;arith         Arithmetic operation, result in register, sets overflow.
;arith2        Two successive arithmetic operations.
;logic         Logical operation, result in register, does not set overflow.
;abs_branch    Absolute branch.
;branch        Branch.
;bmi           Block move.
;call          Call to subprogram.
;ret           Return from subprogram.
;rfi           Return from interrupt.
;dsi           Disable interrupts.
;cmp           Compare or test.
;div           EAM 32/32 division.
;divd          EAM 64/32 division.
;mul           EAM 32 * 32 -> 64 multiplication.
;shiftdi       EAM 64 bit shift.
;fdiv          Floating point divide.
;fsqrt         Floating point square root.
;ftoi          Fix float to integer.
;itof          Float integer.
;fmove         Floating point move w/ or w/o change of sign: fmove, fabs, fneg.
;fcmp          Floating point compare or test.
;fp            Other floating point operations.
;nop           No operation.
;multi         Multiple instructions which split.
;asm           User asm instructions.
;trap          Trap instructions.

(define_attr "type"
"imm_reg,mem_reg,eam_reg,fp_reg,reg_mem,reg_eam,reg_fp,arith,arith2,logic,abs_branch,branch,bmi,call,ret,rfi,dsi,cmp,div,divd,mul,shiftdi,fdiv,fsqrt,ftoi,itof,fmove,fcmp,fp,nop,multi,asm,trap" (const_string "logic"))

; Those insns that occupy 4 bytes.
(define_attr "single_insn" "no,yes"
  (if_then_else (eq_attr "type" "arith2,rfi,multi")
                (const_string "no")
                (const_string "yes")))

; True if branch or call will be emitting a nop into its delay slot.
(define_attr "empty_delay_slot" "false,true"
  (symbol_ref "(empty_delay_slot (insn)
		? EMPTY_DELAY_SLOT_TRUE : EMPTY_DELAY_SLOT_FALSE)"))

; Length in bytes.
; The allowed range for the offset of short branches is [-131072;131068]
; and it is counted from the address of the insn so we need to subtract
; 8 for forward branches because (pc) points to the next insn for them.
(define_attr "length" ""
  (cond [(eq_attr "type" "abs_branch,call,ret")
           (if_then_else (eq_attr "empty_delay_slot" "true")
                         (const_int 8)
                         (const_int 4))
         (eq_attr "type" "branch")
           (if_then_else (leu (plus (minus (match_dup 0) (pc))
                                    (const_int 131060))
                              (const_int 262120))
                         (if_then_else (eq_attr "empty_delay_slot" "true")
                                       (const_int 8)
                                       (const_int 4))
                         (const_int 20))
         (eq_attr "single_insn" "no")
           (const_int 8)] (const_int 4)))

(define_asm_attributes [(set_attr "type" "asm")])

; Delay slots.
(define_delay (eq_attr "type" "abs_branch,branch,call,ret")
  [(and (eq_attr "type" "!abs_branch,branch,call,ret,rfi,bmi,mul,div,divd,fdiv,fsqrt,asm")
        (eq_attr "single_insn" "yes"))
    (nil) (nil)])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Processor pipeline description.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Attribute for cpu type.
; These must match the values for enum processor_type in visium-opts.h.
(define_attr "cpu" "gr5,gr6" (const (symbol_ref "visium_cpu_attr")))

(include "gr5.md")
(include "gr6.md")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Iterators.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_mode_iterator QHI [QI HI])
(define_mode_iterator I [QI HI SI])
(define_mode_attr b [(QI "8") (HI "16") (SI "32")])
(define_mode_attr s [(QI ".b") (HI ".w") (SI ".l")])

; This code iterator allows signed and unsigned widening multiplications
; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

; <u> expands to an empty string when doing a signed operation and
; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")])

; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

; This code iterator allows returns and simple returns to use the same template.
(define_code_iterator any_return [return simple_return])
(define_code_attr return_pred [(return "visium_can_use_return_insn_p ()")
			       (simple_return "!visium_interrupt_function_p ()")])
(define_code_attr return_str [(return "") (simple_return "simple_")])

; This code iterator allows integer and FP cstores to use the same template.
(define_code_iterator any_scc [ltu lt])
(define_code_attr scc_str [(ltu "sltu") (lt "slt")])

;This code iterator allows cstore splitters to use the same template.
(define_code_iterator any_add [plus minus])
(define_code_attr add_op  [(plus "PLUS") (minus "MINUS")])
(define_code_attr add_str [(plus "plus") (minus "minus")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitutions.
;;
;; They are used to define the first instruction of the pairs required by
;; the postreload compare elimination pass, with a first variant for the
;; logical insns and a second variant for the arithmetic insns.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_subst "flags_subst_logic"
  [(set (match_operand 0 "") (match_operand 1 ""))
   (clobber (reg:CC R_FLAGS))]
  ""
  [(set (reg:CC R_FLAGS)
	(compare:CC (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst_attr "subst_logic" "flags_subst_logic" "_flags" "_set_flags")

(define_subst "flags_subst_arith"
  [(set (match_operand 0 "") (match_operand 1 ""))
   (clobber (reg:CC R_FLAGS))]
  ""
  [(set (reg:CCNZ R_FLAGS)
	(compare:CCNZ (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst_attr "subst_arith" "flags_subst_arith" "_flags" "_set_flags")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; QImode moves
;;
;; For moving among registers we use the move.b instruction.  This is
;; actually an OR instruction using an alias.  For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 31.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, QImode);
})

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r")
        (match_operand:QI 1 "general_operand"      " r,rO, r, r,?b,?c,i,m"))]
  "ok_for_simple_move_operands (operands, QImode)"
  "@
    #
    write.b %0,%r1
    writemd %1,r0		;movqi ?b r
    writemdc %1		;movqi ?c r
    readmda %0		;movqi r ?b
    readmdc %0		;movqi r ?c
    moviq   %0,%b1		;movqi  r  i
    read.b  %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,mem_reg")])

(define_insn "*movqi_insn<subst_logic>"
  [(set (match_operand:QI 0 "gpc_reg_operand" "=r")
	(match_operand:QI 1 "gpc_reg_operand" "r"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.b  %0,%1"
  [(set_attr "type" "logic")])

(define_split
  [(set (match_operand:QI 0 "gpc_reg_operand" "")
	(match_operand:QI 1 "gpc_reg_operand" ""))]
  "reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

(define_expand "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" ""))
	(match_operand:QI 1 "general_operand"                   ""))]
  "")

(define_insn "*movstrictqi_insn"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r,r"))
	(match_operand:QI 1 "general_operand"                   "rO,m"))]
  "ok_for_simple_move_strict_operands (operands, QImode)"
  "@
    #
    read.b  %0,%1"
  [(set_attr "type" "logic,mem_reg")])

(define_insn "*movstrictqi_insn<subst_logic>"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r"))
	(match_operand:QI 1 "reg_or_0_operand"                  "rO"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.b  %0,%r1"
  [(set_attr "type" "logic")])

(define_split
  [(set (strict_low_part (match_operand:QI 0 "register_operand" ""))
	(match_operand:QI 1 "reg_or_0_operand" ""))]
  "reload_completed"
  [(parallel [(set (strict_low_part (match_dup 0)) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HImode moves
;;
;; For moving among registers we use the move.w instruction.  This is
;; actually an OR instruction using an alias.  For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 62 and is shifted right one place in the assembled 
;; instruction.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, HImode);
})

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r")
        (match_operand:HI 1 "general_operand"      " r,rO, r, r,?b,?c,i,m"))]
  "ok_for_simple_move_operands (operands, HImode)"
  "@
    #
    write.w %0,%r1
    writemd %1,r0		;movhi ?b r
    writemdc %1		;movhi ?c r
    readmda %0		;movhi r ?b
    readmdc %0		;movhi r ?c
    moviq   %0,%w1		;movhi  r  i
    read.w  %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,mem_reg")])

(define_insn "*movhi_insn<subst_logic>"
  [(set (match_operand:HI 0 "gpc_reg_operand" "=r")
	(match_operand:HI 1 "gpc_reg_operand" "r"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.w  %0,%1"
  [(set_attr "type" "logic")])

(define_split
  [(set (match_operand:HI 0 "gpc_reg_operand" "")
	(match_operand:HI 1 "gpc_reg_operand" ""))]
  "reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

(define_expand "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" ""))
	(match_operand:HI 1 "general_operand"                   ""))]
  "")

(define_insn "*movstricthi_insn"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r,r,r"))
	(match_operand:HI 1 "general_operand"                   " r,i,m"))]
  "ok_for_simple_move_strict_operands (operands, HImode)"
  "@
    #
    movil   %0,%w1
    read.w  %0,%1"
  [(set_attr "type" "logic,imm_reg,mem_reg")])

(define_insn "*movstricthi_insn<subst_logic>"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r"))
	(match_operand:HI 1 "register_operand"                  "r"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.w  %0,%1"
  [(set_attr "type" "logic")])

(define_split
  [(set (strict_low_part (match_operand:HI 0 "register_operand" ""))
	(match_operand:HI 1 "register_operand" ""))]
  "reload_completed"
  [(parallel [(set (strict_low_part (match_dup 0)) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SImode moves
;;
;; For moving among registers we use the move.l instruction.  This is
;; actually an OR instruction using an alias.  For moving between register
;; and memory we need the address of the memory location in a register.
;; However, we can accept an expression (reg + offset) where offset is in
;; the range 0 .. 124 and is shifted right two places in the assembled 
;; instruction.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, SImode);
})

(define_insn "*movsi_high"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r,r") 
        (high:SI (match_operand:SI 1 "immediate_operand" "n,i")) )]
  ""
  "@
    moviu   %0,%u1
    moviu   %0,%%u %a1"
  [(set_attr "type" "imm_reg")])

; We only care about the lower 16 bits of the constant 
; being inserted into the upper 16 bits of the register.
(define_insn "*moviu"
  [(set (zero_extract:SI (match_operand:SI 0 "gpc_reg_operand" "+r")
                         (const_int 16)
                         (const_int 0))
        (match_operand:SI 1 "const_int_operand" "n"))]
  ""
  "moviu   %0,%w1"
  [(set_attr "type" "imm_reg")])

(define_insn "*movsi_losum"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r,r")
        (lo_sum:SI (match_operand:SI 1 "gpc_reg_operand" "0,0")
                   (match_operand:SI 2 "immediate_operand" "n,i")))]
  ""
  "@
    movil   %0,%w2
    movil   %0,%%l %a2"
  [(set_attr "type" "imm_reg")])

(define_insn "*movil"
  [(set (zero_extract:SI (match_operand:SI 0 "gpc_reg_operand" "+r")
                         (const_int 16)
                         (const_int 16))
        (match_operand:SI 1 "const_int_operand" "n"))]
  ""
  "movil   %0,%w1"
  [(set_attr "type" "imm_reg")])

(define_insn "*movsi_insn_no_ieee"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r,r,r, r,!f")
        (match_operand:SI 1 "general_operand"      " r,rO, r, r,?b,?c,J,M,i,m,!f, r"))]
  "!TARGET_FPU_IEEE && ok_for_simple_move_operands (operands, SImode)"
  "@
    #
    write.l %0,%r1
    writemd %1,r0		;movsi  ?b  r
    writemdc %1		;movsi  ?c  r
    readmda %0		;movsi  r  ?b
    readmdc %0		;movsi  r  ?c
    moviq   %0,%1		;movsi  r  J
    #
    #			;movsi  r  i
    read.l  %0,%1
    fstore  %0,%1
    fload   %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,logic,multi,mem_reg,fp_reg,reg_fp")])

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r, m,?b,?c, r, r,r,r,r,r, r,?f,f")
        (match_operand:SI 1 "general_operand"      " r,rO, r, r,?b,?c,J,M,i,m,?f, r,f"))]
  "TARGET_FPU_IEEE && ok_for_simple_move_operands (operands, SImode)"
  "@
    #
    write.l %0,%r1
    writemd %1,r0		;movsi  ?b  r
    writemdc %1		;movsi  ?c  r
    readmda %0		;movsi  r  ?b
    readmdc %0		;movsi  r  ?c
    moviq   %0,%1		;movsi  r  J
    #
    #			;movsi  r  i
    read.l  %0,%1
    fstore  %0,%1
    fload   %0,%1
    fmove   %0,%1"
  [(set_attr "type" "logic,reg_mem,reg_eam,reg_eam,eam_reg,eam_reg,imm_reg,logic,multi,mem_reg,fp_reg,reg_fp,fmove")])

(define_insn "*movsi_insn<subst_logic>"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r")
	(match_operand:SI 1 "gpc_reg_operand" "r"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.l  %0,%1"
  [(set_attr "type" "logic")])

(define_insn "*movsi_insn_m1<subst_logic>"
  [(set (match_operand:SI 0 "gpc_reg_operand" "=r")
	(const_int -1))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "not.l   %0,r0"
  [(set_attr "type" "logic")])

(define_split
  [(set (match_operand:SI 0 "gpc_reg_operand" "")
	(match_operand:SI 1 "gpc_reg_operand" ""))]
  "reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

(define_split
  [(set (match_operand:SI 0 "gpc_reg_operand" "")
	(const_int -1))]
  "reload_completed"
  [(parallel [(set (match_dup 0) (const_int -1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

(define_insn "*movsi_mdbhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))]
  ""
  "readmdb %0"
  [(set_attr "type" "eam_reg")])

(define_split
  [(set (match_operand:SI 0 "gpc_reg_operand" "")
        (match_operand:SI 1 "large_immediate_operand" ""))]
  "reload_completed"
  [(set (match_dup 0)
        (high:SI (match_dup 1)) )
   (set (match_dup 0)
        (lo_sum:SI (match_dup 0) (match_dup 1)))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DImode moves
;;
;; When the destination is the EAM register MDB, then we use the writemd
;; instruction.  In all other cases we split the move into two 32-bit moves.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, DImode);
})

(define_insn "*movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "= r, m, r,??b")
        (match_operand:DI 1 "general_operand"      "rim,rO,?b,  r"))]
  "ok_for_simple_move_operands (operands, DImode)"
  "@
    #
    #
    #
    writemd %d1,%1		;movdi  ?b r"
  [(set_attr "type" "multi,multi,multi,reg_eam")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "") (reg:DI R_MDB))]
  "reload_completed"
  [(set (match_dup 1) (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))
   (set (match_dup 2) (reg:SI R_MDB))]
{
  operands[1] = operand_subword (operands[0], 0, 1, DImode);
  operands[2] = operand_subword (operands[0], 1, 1, DImode);
})

(define_split
  [(set (match_operand:DI 0 "non_eam_dst_operand" "")
        (match_operand:DI 1 "non_eam_src_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  visium_split_double_move (operands, DImode);
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SFmode moves
;;
;; Constants are constructed in a GP register and moved to the FP register.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, SFmode);
})

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,r,r, m,r,r,r")
        (match_operand:SF 1 "general_operand"      " f,G,r,f,r,rG,G,F,m"))]
  "ok_for_simple_move_operands (operands, SFmode)"
  "@
    fmove   %0,%1
    fmove   %0,f0
    fload   %0,%1
    fstore  %0,%1
    #
    write.l %0,%r1
    moviq   %0,0
    #
    read.l  %0,%1"
  [(set_attr "type" "fmove,fmove,reg_fp,fp_reg,logic,reg_mem,imm_reg,multi,mem_reg")])

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "gpc_reg_operand" "=r")
	(match_operand:SF 1 "gpc_reg_operand" "r"))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.l  %0,%1"
  [(set_attr "type" "logic")])

(define_split
  [(set (match_operand:SF 0 "gpc_reg_operand" "")
	(match_operand:SF 1 "gpc_reg_operand" ""))]
  "reload_completed"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])]
  "")

(define_split
  [(set (match_operand:SF 0 "gpc_reg_operand" "")
        (match_operand:SF 1 "const_double_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))]
{
  long l;

  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[1]), l);

  operands[2] = operand_subword (operands[0], 0, 0, SFmode);
  operands[3] = GEN_INT (trunc_int_for_mode (l, SImode));
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DFmode moves
;;
;; We always split a DFmode move into two SImode moves.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  ""
{
  prepare_move_operands (operands, DFmode);
})

(define_insn "*movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "= r, m")
        (match_operand:DF 1 "general_operand"      "rFm,rG"))]
  "ok_for_simple_move_operands (operands, DFmode)"
  "#"
  [(set_attr "type" "multi")])

(define_split
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  visium_split_double_move (operands, DFmode);
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Add
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "add<mode>3"
  [(set (match_operand:QHI 0 "register_operand" "")
	(plus:QHI (match_operand:QHI 1 "register_operand" "")
		  (match_operand:QHI 2 "register_operand" "")))]
  "")

(define_expand "uaddv<mode>4"
  [(set (match_operand:I 0 "register_operand" "")
	(plus:I (match_operand:I 1 "register_operand" "")
		(match_operand:I 2 "register_operand" "")))
   (set (pc)
        (if_then_else (ltu (match_dup 0) (match_dup 1))
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "")

(define_expand "addv<mode>4"
  [(set (match_operand:I 0 "register_operand" "")
	(plus:I (match_operand:I 1 "register_operand" "")
		(match_operand:I 2 "register_operand" "")))
   (set (pc)
        (if_then_else (ne (match_dup 0)
			  (unspec:I [(match_dup 1) (match_dup 2)] UNSPEC_ADDV))
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "")

(define_insn_and_split "*add<mode>3_insn"
  [(set (match_operand:QHI 0 "register_operand" "=r")
	(plus:QHI (match_operand:QHI 1 "register_operand" "%r")
		  (match_operand:QHI 2 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:QHI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "arith")])

(define_insn "*add<mode>3_insn<subst_arith>"
  [(set (match_operand:QHI 0 "register_operand" "=r")
	(plus:QHI (match_operand:QHI 1 "register_operand" "%r")
		  (match_operand:QHI 2 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "add<s>   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "*add<mode>3_insn_set_carry"
  [(set (reg:CCC R_FLAGS)
	(compare:CCC (plus:QHI (match_operand:QHI 1 "register_operand" "%r")
			       (match_operand:QHI 2 "register_operand" "r"))
		     (match_dup 1)))
   (set (match_operand:QHI 0 "register_operand" "=r")
	(plus:QHI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "add<s>   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "*add<mode>3_insn_set_overflow"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (plus:QHI (match_operand:QHI 1 "register_operand" "%r")
			       (match_operand:QHI 2 "register_operand" "r"))
		     (unspec:QHI [(match_dup 1) (match_dup 2)] UNSPEC_ADDV)))
   (set (match_operand:QHI 0 "register_operand" "=r")
	(plus:QHI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "add<s>   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "add_operand" "")))]
  "")

(define_expand "addsi3_flags"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (plus:SI (match_operand:SI 1 "register_operand" "")
			    (match_operand:SI 2 "add_operand" "")))
	      (clobber (reg:CC R_FLAGS))])]
  "reload_completed"
  "")

(define_insn_and_split "*addsi3_insn"
  [(set (match_operand:SI 0 "register_operand"          "=r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,r,0")
		 (match_operand:SI 2 "add_operand"      " L,r,J")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (plus:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "arith")])

; Favour the addition of small negative constants, since they are
; expensive to load into a register.

(define_insn "*addsi3_insn<subst_arith>"
  [(set (match_operand:SI 0 "register_operand"          "=r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,r,0")
		 (match_operand:SI 2 "add_operand"      " L,r,J")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "@
    subi    %0,%n2
    add.l   %0,%1,%2
    addi    %0,%2"
  [(set_attr "type" "arith")])

(define_insn "addsi3_insn_set_carry"
  [(set (reg:CCC R_FLAGS)
	(compare:CCC (plus:SI (match_operand:SI 1 "register_operand" "%r,0")
			      (match_operand:SI 2 "real_add_operand" " r,J"))
		     (match_dup 1)))
   (set (match_operand:SI 0 "register_operand"          "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "@
    add.l   %0,%1,%2
    addi    %0,%2"
  [(set_attr "type" "arith")])

(define_insn "*addsi3_insn_set_overflow"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (plus:SI (match_operand:SI 1 "register_operand" "%r,0")
			      (match_operand:SI 2 "real_add_operand" " r,J"))
		     (unspec:SI [(match_dup 1) (match_dup 2)] UNSPEC_ADDV)))
   (set (match_operand:SI 0 "register_operand"          "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "@
    add.l   %0,%1,%2
    addi    %0,%2"
  [(set_attr "type" "arith")])

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "add_operand" "")))]
  "")

; Disfavour the use of add.l because of the early clobber.

(define_insn_and_split "*addi3_insn"
  [(set (match_operand:DI 0 "register_operand"          "=r,r,&r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0,0, r")
		 (match_operand:DI 2 "add_operand"      " L,J, r")))]
  "ok_for_simple_arith_logic_operands (operands, DImode)"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_double_add (PLUS, operands[0], operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Add with Carry
;;
;; Only SI mode is supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*<scc_str><subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_scc:SI (reg R_FLAGS) (const_int 0)))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "adc.l   %0,r0,r0"
  [(set_attr "type" "arith")])

(define_insn "*plus_<scc_str><subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "register_operand" "r")
		 (any_scc:SI (reg R_FLAGS) (const_int 0))))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "adc.l   %0,%1,r0"
  [(set_attr "type" "arith")])

(define_insn "*plus_plus_sltu<subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "register_operand" "r"))
		 (ltu:SI (reg R_FLAGS) (const_int 0))))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "adc.l   %0,%1,%2"
  [(set_attr "type" "arith")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Subtract
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "sub<mode>3"
  [(set (match_operand:QHI 0 "register_operand" "")
	(minus:QHI (match_operand:QHI 1 "reg_or_0_operand" "")
		   (match_operand:QHI 2 "register_operand" "")))]
  "")

(define_expand "usubv<mode>4"
  [(set (match_operand:I 0 "register_operand" "")
	(minus:I (match_operand:I 1 "reg_or_0_operand" "")
		 (match_operand:I 2 "register_operand" "")))
   (set (pc)
        (if_then_else (ltu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  if (operands[1] == const0_rtx)
    {
      emit_insn (gen_unegv<mode>3 (operands[0], operands[2], operands[3]));
      DONE;
    }
})

(define_expand "subv<mode>4"
  [(set (match_operand:I 0 "register_operand" "")
	(minus:I (match_operand:I 1 "register_operand" "")
		 (match_operand:I 2 "register_operand" "")))
   (set (pc)
        (if_then_else (ne (match_dup 0)
			  (unspec:I [(match_dup 1) (match_dup 2)] UNSPEC_SUBV))
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "")

(define_insn_and_split "*sub<mode>3_insn"
  [(set (match_operand:QHI 0 "register_operand" "=r")
	(minus:QHI (match_operand:QHI 1 "reg_or_0_operand" "rO")
		   (match_operand:QHI 2 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (minus:QHI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
 ""
  [(set_attr "type" "arith")])

(define_insn "*sub<mode>3_insn<subst_arith>"
  [(set (match_operand:QHI 0 "register_operand" "=r")
	(minus:QHI (match_operand:QHI 1 "reg_or_0_operand" "rO")
		   (match_operand:QHI 2 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "sub<s>   %0,%r1,%2"
  [(set_attr "type" "arith")])

(define_insn "*sub<mode>3_insn_set_carry"
  [(set (reg:CC R_FLAGS)
	(compare:CC (match_operand:QHI 1 "reg_or_0_operand" "r0")
		    (match_operand:QHI 2 "register_operand" "r")))
   (set (match_operand:QHI 0 "register_operand" "=r")
	(minus:QHI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "sub<s>   %0,%r1,%2"
  [(set_attr "type" "arith")])

(define_insn "*sub<mode>3_insn_set_overflow"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (minus:QHI (match_operand:QHI 1 "reg_or_0_operand" "r0")
				(match_operand:QHI 2 "register_operand" "r"))
		     (unspec:QHI [(match_dup 1) (match_dup 2)] UNSPEC_SUBV)))
   (set (match_operand:QHI 0 "register_operand" "=r")
	(minus:QHI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "sub<s>   %0,%r1,%2"
  [(set_attr "type" "arith")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "")
		  (match_operand:SI 2 "add_operand" "")))]
  "")

(define_expand "subsi3_flags"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (minus:SI (match_operand:SI 1 "reg_or_0_operand" "")
			     (match_operand:SI 2 "add_operand" "")))
	      (clobber (reg:CC R_FLAGS))])]
  "reload_completed"
  "")

(define_insn_and_split "*subsi3_insn"
  [(set (match_operand:SI 0 "register_operand"           "=r,r, r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO,0")
		  (match_operand:SI 2 "add_operand"      " L,r, J")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (minus:SI (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
 ""
  [(set_attr "type" "arith")])

; Favour the subtraction of small negative constants, since they are
; expensive to load into a register.

(define_insn "*subsi3_insn<subst_arith>"
  [(set (match_operand:SI 0 "register_operand"           "=r,r, r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO,0")
		  (match_operand:SI 2 "add_operand"      " L,r, J")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "@
    addi    %0,%n2
    sub.l   %0,%r1,%2
    subi    %0,%2"
  [(set_attr "type" "arith")])

(define_insn "subsi3_insn_set_carry"
  [(set (reg:CC R_FLAGS)
	(compare:CC (match_operand:SI 1 "register_operand" "r,0")
		    (match_operand:SI 2 "real_add_operand" "r,J")))
   (set (match_operand:SI 0 "register_operand"           "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "@
    sub.l   %0,%r1,%2
    subi    %0,%2"
  [(set_attr "type" "arith")])

(define_insn "*subsi3_insn_set_overflow"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (minus:SI (match_operand:SI 1 "register_operand" "r,0")
			       (match_operand:SI 2 "real_add_operand" "r,J"))
		     (unspec:SI [(match_dup 1) (match_dup 2)] UNSPEC_SUBV)))
   (set (match_operand:SI 0 "register_operand"           "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "@
    sub.l   %0,%1,%2
    subi    %0,%2"
  [(set_attr "type" "arith")])

(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "add_operand" "")))]
  "")

; Disfavour the use of the sub.l because of the early clobber.

(define_insn_and_split "*subdi3_insn"
  [(set (match_operand:DI 0 "register_operand"           "=r,r,&r")
	(minus:DI (match_operand:DI 1 "register_operand" " 0,0, r")
		  (match_operand:DI 2 "add_operand"      " L,J, r")))]
  "ok_for_simple_arith_logic_operands (operands, DImode)"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_double_add (MINUS, operands[0], operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Subtract with Carry
;;
;; Only SI mode is supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*neg_<scc_str><subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (any_scc:SI (reg R_FLAGS) (const_int 0))))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "subc.l  %0,r0,r0"
  [(set_attr "type" "arith")])

(define_insn "*minus_<scc_str><subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (any_scc:SI (reg R_FLAGS) (const_int 0))))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "subc.l  %0,%1,r0"
  [(set_attr "type" "arith")])

(define_insn "*minus_minus_sltu<subst_arith>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
			    (match_operand:SI 2 "register_operand" "r"))
		  (ltu:SI (reg R_FLAGS) (const_int 0))))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "subc.l  %0,%r1,%2"
  [(set_attr "type" "arith")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Negate
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "neg<mode>2"
  [(set (match_operand:I 0 "register_operand" "")
	(neg:I (match_operand:I 1 "register_operand" "")))]
  "")

(define_expand "unegv<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(neg:I (match_operand:I 1 "register_operand" "")))
   (set (pc)
        (if_then_else (ne (match_dup 0) (const_int 0))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
  "")

(define_expand "negv<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(neg:I (match_operand:I 1 "register_operand" "")))
   (set (pc)
        (if_then_else (ne (match_dup 0)
			  (unspec:I [(match_dup 1)] UNSPEC_NEGV))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
  "")

(define_insn_and_split "*neg<mode>2_insn"
  [(set (match_operand:I 0 "register_operand" "=r")
	(neg:I (match_operand:I 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (neg:I (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "arith")])

(define_insn "*neg<mode>2_insn<subst_arith>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(neg:I (match_operand:I 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "sub<s>   %0,r0,%1"
  [(set_attr "type" "arith")])

(define_insn "negsi2_insn_set_carry"
  [(set (reg:CCC R_FLAGS)
	(compare:CCC (not:SI (match_operand:SI 1 "register_operand" "r"))
		     (const_int -1)))
   (set (match_operand:SI 0 "register_operand" "=r")
        (neg:SI (match_dup 1)))]
  "reload_completed"
  "sub.l   %0,r0,%1"
  [(set_attr "type" "arith")])

(define_insn "*neg<mode>2_insn_set_overflow"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (neg:I (match_operand:I 1 "register_operand" "r"))
		     (unspec:I [(match_dup 1)] UNSPEC_NEGV)))
   (set (match_operand:I 0 "register_operand" "=r")
	(neg:I (match_dup 1)))]
  "reload_completed"
  "sub<s>   %0,r0,%1"
  [(set_attr "type" "arith")])

(define_expand "negdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(neg:DI (match_operand:DI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*negdi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, DImode)"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_double_add (MINUS, operands[0], const0_rtx, operands[1]);
  DONE;
}
  [(set_attr "type" "arith2")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Multiply (non-widening and widening, signed and unsigned)
;;
;; Only SI mode is supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; The mults and multu instructions clear MDC but we only pretend that they
; clobber it to keep things relatively simple.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=b")
         (mult:SI (match_operand:SI 1 "register_operand" "%r")
                  (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:SI R_MDC))]
  ""
  "mults   %1,%2"
  [(set_attr "type" "mul")])

; The names are mulsidi3 and umulsidi3 here.

(define_insn "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=b")
        (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "%r"))
                 (any_extend:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:SI R_MDC))]
  ""
  "mult<su>   %1,%2"
  [(set_attr "type" "mul")])

; But they are smulsi3_highpart and umulsi3_highpart here.

(define_insn_and_split "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI
          (ashiftrt:DI
            (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "%r"))
                     (any_extend:DI (match_operand:SI 2 "register_operand" "r")))
            (const_int 32))))
   (clobber (reg:DI R_MDB))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (reg:DI R_MDB)
                   (mult:DI (any_extend:DI (match_dup 1))
                            (any_extend:DI (match_dup 2))))
              (clobber (reg:SI R_MDC))])
   (set (match_dup 0) (unspec:SI [(reg:DI R_MDB)] UNSPEC_MDBHI))]
  ""
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer divide and modulus (signed and unsigned)
;;
;; Only SI mode is supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*divmodsi4_insn"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (div:SI (match_operand:SI 1 "register_operand" "0")
                (match_operand:SI 2 "register_operand" "r")))
   (set (reg:SI R_MDC) (mod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divs    %2"
  [(set_attr "type" "div")])

(define_insn_and_split "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (div:SI (match_operand:SI 1 "register_operand" "0")
                (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "register_operand" "=r")
        (mod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (div:SI (match_dup 1) (match_dup 2)))
              (set (reg:SI R_MDC) (mod:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (reg:SI R_MDC))]
  ""
  [(set_attr "type" "multi")])

(define_insn "*udivmodsi4_insn"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (udiv:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "register_operand" "r")))
   (set (reg:SI R_MDC) (umod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divu    %2"
  [(set_attr "type" "div")])

(define_insn_and_split "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=b")
        (udiv:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "register_operand" "=r")
        (umod:SI (match_dup 1) (match_dup 2)))
   (clobber (reg:SI R_MDC))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (udiv:SI (match_dup 1) (match_dup 2)))
              (set (reg:SI R_MDC) (umod:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (reg:SI R_MDC))]
  ""
  [(set_attr "type" "multi")])

; FIXME. How do we persuade the compiler to use 64/32 bit divides directly ?

(define_insn "*divds"
  [(set (reg:DI R_MDB)
        (div:DI (reg:DI R_MDB) (sign_extend:DI (match_operand:SI 0 "register_operand" "r"))))
   (set (reg:SI R_MDC) (truncate:SI (mod:DI (reg:DI R_MDB) (sign_extend:DI (match_dup 0)))))]
  ""
  "divds   %0"
  [(set_attr "type" "divd")])

(define_insn "*divdu"
  [(set (reg:DI R_MDB)
        (udiv:DI (reg:DI R_MDB) (zero_extend:DI (match_operand:SI 0 "register_operand" "r"))))
   (set (reg:SI R_MDC) (truncate:SI (umod:DI (reg:DI R_MDB) (zero_extend:DI (match_dup 0)))))]
  ""
  "divdu   %0"
  [(set_attr "type" "divd")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Logical AND
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "and<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(and:I (match_operand:I 1 "register_operand" "")
	       (match_operand:I 2 "register_operand" "")))]
  "")

(define_insn_and_split "*and<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r")
	(and:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (and:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*and<mode>3_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(and:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "and<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Inclusive Logical OR
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "ior<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(ior:I (match_operand:I 1 "register_operand" "")
	       (match_operand:I 2 "register_operand" "")))]
  "")

(define_insn_and_split "*ior<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r")
	(ior:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ior:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*ior<mode>3_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(ior:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "or<s>    %0,%1,%2"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Exclusive Logical OR
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "xor<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(xor:I (match_operand:I 1 "register_operand" "")
		(match_operand:I 2 "register_operand" "")))]
  "")

(define_insn_and_split "*xor<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r")
	(xor:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (xor:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*xor<mode>3_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(xor:I (match_operand:I 1 "register_operand" "%r")
	       (match_operand:I 2 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "xor<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Logical NOT
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "one_cmpl<mode>2"
  [(set (match_operand:I 0 "register_operand" "")
	(not:I (match_operand:I 1 "reg_or_0_operand" "")))]
  "")

(define_insn_and_split "*one_cmpl<mode>2_insn"
  [(set (match_operand:I 0 "register_operand" "=r")
	(not:I (match_operand:I 1 "reg_or_0_operand" "rO")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (not:I (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*one_cmpl<mode>2_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r")
	(not:I (match_operand:I 1 "reg_or_0_operand" "rO")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "not<s>   %0,%r1"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arithmetic Shift Left
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "ashl<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(ashift:I (match_operand:I  1 "register_operand"     "")
		  (match_operand:QI 2 "reg_or_shift_operand" "")))]
  "")

(define_insn_and_split "*ashl<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(ashift:I (match_operand:I  1 "register_operand"     "r,r")
		  (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "arith")])

(define_insn "*ashl<mode>3_insn<subst_arith>"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(ashift:I (match_operand:I  1 "register_operand"     "r,r")
		  (match_operand:QI 2 "reg_or_shift_operand" "r,K")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "asl<s>   %0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (ashift:DI (match_operand:DI 1 "register_operand" "0,r")
                   (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    asld    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (ashift:DI (match_operand:DI 1 "gpc_reg_operand" "")
                   (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 4))
   (set (subreg:SI (match_dup 0) 4) (const_int 0))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arithmetic Shift Right
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "ashr<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(ashiftrt:I (match_operand:I  1 "register_operand"     "")
		    (match_operand:QI 2 "reg_or_shift_operand" "")))]
  "")

(define_insn_and_split "*ashr<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(ashiftrt:I (match_operand:I  1 "register_operand"     "r,r")
		    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashiftrt:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*ashr<mode>3_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(ashiftrt:I (match_operand:I  1 "register_operand"     "r,r")
		    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "asr<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (ashiftrt:DI (match_operand:DI 1 "register_operand" "0,r")
                     (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    asrd    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (ashiftrt:DI (match_operand:DI 1 "gpc_reg_operand" "")
                     (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 0))
   (parallel [(set (subreg:SI (match_dup 0) 0)
		   (ashiftrt:SI (subreg:SI (match_dup 1) 0) (const_int 31)))
	      (clobber (reg:CC R_FLAGS))])]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Logical Shift Right
;;
;; Modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "lshr<mode>3"
  [(set (match_operand:I 0 "register_operand" "")
	(lshiftrt:I (match_operand:I  1 "register_operand"     "")
		    (match_operand:QI 2 "reg_or_shift_operand" "")))]
  "")

(define_insn_and_split "*lshr<mode>3_insn"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(lshiftrt:I (match_operand:I  1 "register_operand"     "r,r")
		    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))]
  "ok_for_simple_arith_logic_operands (operands, <MODE>mode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (lshiftrt:I (match_dup 1) (match_dup 2)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*lshr<mode>3_insn<subst_logic>"
  [(set (match_operand:I 0 "register_operand" "=r,r")
	(lshiftrt:I (match_operand:I  1 "register_operand"     "r,r")
		    (match_operand:QI 2 "reg_or_shift_operand" "r,K")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "lsr<s>   %0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=b,r")
        (lshiftrt:DI (match_operand:DI 1 "register_operand" "0,r")
                     (match_operand:QI 2 "reg_or_32_operand" "r,P")))
   (clobber (reg:SI R_MDC))]
  ""
  "@
    lsrd    %2
    #"
  [(set_attr "type" "shiftdi,multi")])

(define_split
  [(set (match_operand:DI 0 "gpc_reg_operand" "")
        (lshiftrt:DI (match_operand:DI 1 "gpc_reg_operand" "")
                     (const_int 32)))
   (clobber (reg:SI R_MDC))]
  "reload_completed"
  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]
  "")

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Truncate
;;
;; Truncations among modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "trunchiqi2"
  [(set (match_operand:QI 0 "register_operand" "")
	 (truncate:QI (match_operand:HI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*trunchiqi2_insn"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:HI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, QImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (truncate:QI (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*trunchiqi2_insn<subst_logic>"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:HI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.b  %0,%1"
  [(set_attr "type" "logic")])

(define_expand "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(truncate:HI (match_operand:SI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*truncsihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI (match_operand:SI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, HImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (truncate:HI (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*truncsihi2_insn<subst_logic>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.w  %0,%1"
  [(set_attr "type" "logic")])

(define_expand "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI (match_operand:DI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*truncdisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI (match_operand:DI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (truncate:SI (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*truncdisi2_insn<subst_logic>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI (match_operand:DI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.l  %0,%d1"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sign-extend
;;
;; Sign-extensions among modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, HImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:HI (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*extendqihi2_insn<subst_logic>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "extb.w  %0,%1"
  [(set_attr "type" "logic")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (sign_extend:SI (match_dup 1)))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*extendqisi2_insn<subst_logic>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "extb.l  %0,%1"
  [(set_attr "type" "logic")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (sign_extend:SI (match_operand:HI 1 "register_operand" "")))
	      (clobber (reg:CC R_FLAGS))])]
  ""
  [(set_attr "type" "logic")])

(define_insn "*extendhisi2_insn<subst_logic>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "extw.l  %0,%1"
  [(set_attr "type" "logic")])

(define_expand "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, DImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 3) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])
   (parallel [(set (match_dup 2)
		   (ashiftrt:SI (match_dup 1) (const_int 31)))
	      (clobber (reg:CC R_FLAGS))])]
{
  operands[2] = operand_subword (operands[0], 0, 0, DImode);
  operands[3] = operand_subword (operands[0], 1, 0, DImode);
}
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zero-extend
;;
;; Zero-extensions among modes QI, HI, SI and DI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; QI is zero-extended to wider modes by shifting left and then performing
; a logical shift right to insert the zeroes. This avoids the need to use
; another register.

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*zero_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, HImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:HI (match_dup 2) (const_int 8)))
	      (clobber (reg:CC R_FLAGS))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:HI (match_dup 0) (const_int 8)))
	      (clobber (reg:CC R_FLAGS))])]
{
  operands[2] = gen_rtx_SUBREG (HImode, operands[1], 0);
}
  [(set_attr "type" "multi")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*zero_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, SImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0)
		   (ashift:SI (match_dup 2) (const_int 24)))
	      (clobber (reg:CC R_FLAGS))])
   (parallel [(set (match_dup 0)
		   (lshiftrt:SI (match_dup 0) (const_int 24)))
	      (clobber (reg:CC R_FLAGS))])]
{
  operands[2] = gen_rtx_SUBREG (SImode, operands[1], 0);
}
  [(set_attr "type" "multi")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "moviu   %0,0"
  [(set_attr "type" "imm_reg")])

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "")

(define_insn_and_split "*zero_extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  "ok_for_simple_arith_logic_operands (operands, DImode)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 3) (match_dup 1))
	      (clobber (reg:CC R_FLAGS))])
   (set (match_dup 2) (const_int 0))]
{
  operands[2] = operand_subword (operands[0], 0, 0, DImode);
  operands[3] = operand_subword (operands[0], 1, 0, DImode);
}
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bit Test
;;
;; Only SI mode is supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; BITS_BIG_ENDIAN is defined to 1 so operand #1 counts from the MSB.

(define_insn "*btst<mode>"
  [(set (reg:CCC R_FLAGS)
	(compare:CCC (zero_extract:I
		       (match_operand:I 0 "register_operand" "r")
		       (const_int 1)
		       (match_operand:QI 1 "const_shift_operand" "K"))
		     (const_int 0)))]
  "reload_completed"
  "lsr<s>   r0,%0,<b>-%1"
  [(set_attr "type" "logic")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer overflow tests
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*addv_tst<mode>"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (match_operand:I 0 "register_operand" "r")
 		     (unspec:I [(match_operand:I 1 "register_operand" "%r")
				(match_operand:I 2 "register_operand" "r")]
			       UNSPEC_ADDV)))]
  "reload_completed"
  "add<s>   r0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "*subv_tst<mode>"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (match_operand:I 0 "register_operand" "r")
 		     (unspec:I [(match_operand:I 1 "reg_or_0_operand" "rO")
				(match_operand:I 2 "register_operand" "r")]
			       UNSPEC_SUBV)))]
  "reload_completed"
  "sub<s>   r0,%r1,%2"
  [(set_attr "type" "arith")])

(define_insn "*negv_tst<mode>"
  [(set (reg:CCV R_FLAGS)
	(compare:CCV (match_operand:I 0 "register_operand" "r")
 		     (unspec:I [(match_operand:I 1 "register_operand" "r")]
			       UNSPEC_NEGV)))]
  "reload_completed"
  "sub<s>   r0,r0,%1"
  [(set_attr "type" "arith")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer comparisons
;;
;; Modes QI, HI and SI are supported directly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*cmp<mode>"
  [(set (reg:CC R_FLAGS)
	(compare:CC (match_operand:I 0 "register_operand" "r")
		    (match_operand:I 1 "reg_or_0_operand" "rO")))]
  "reload_completed"
  "cmp<s>   %0,%r1"
  [(set_attr "type" "cmp")])

(define_insn "*cmp<mode>_sne"
  [(set (reg:CCC R_FLAGS)
	(compare:CCC (not:I (match_operand:I 0 "register_operand" "r"))
		     (const_int -1)))]
  "reload_completed"
  "cmp<s>   r0,%0"
  [(set_attr "type" "cmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "addsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (plus:SF (match_operand:SF 1 "fp_reg_operand" "%f")
                 (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fadd    %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (minus:SF (match_operand:SF 1 "fp_reg_operand" "f")
                  (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fsub    %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (mult:SF (match_operand:SF 1 "fp_reg_operand" "%f")
                 (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fmult   %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (div:SF (match_operand:SF 1 "fp_reg_operand" "f")
                (match_operand:SF 2 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fdiv    %0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (sqrt:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fsqrt   %0,%1"
  [(set_attr "type" "fsqrt")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (neg:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fneg    %0,%1"
  [(set_attr "type" "fmove")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (abs:SF (match_operand:SF 1 "fp_reg_operand" "f")))]
  "TARGET_FPU"
  "fabs    %0,%1"
  [(set_attr "type" "fmove")])

(define_expand "copysignsf3"
  [(match_operand:SF 0 "register_operand" "")
   (match_operand:SF 1 "nonmemory_operand" "")
   (match_operand:SF 2 "register_operand" "")]
  "TARGET_FPU && !TARGET_FPU_IEEE"
{
  visium_expand_copysign (operands, SFmode);
  DONE;
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float <-> single integer conversions for !TARGET_FPU_IEEE
;;
;; An FMOVE instruction converts a signalling NaN (zero high order bit of the
;; mantissa) to a quiet NaN (-1). This is acceptable when the data to be
;; moved is in fact a floating-point number, but to avoid nasty surprises
;; integers must in general be kept out of the floating-point registers.
;; TARGET_HARD_REGNO_MODE_OK thus only allows SFmode in these registers.
;; However, since FTOI and ITOF use floating-point registers for both their
;; inputs and outputs, to use these instructions integers must transiently
;; occupy such registers. To disguise this from the compiler, UNSPECs are
;; used for floating-point operations on integers and floating from general
;; register to floating-point register and fixing in the reverse direction
;; are only split into the individual UNSPEC operations after reload.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*fload_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SI 1 "register_operand" "r")] UNSPEC_FLOAD))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "fload   %0,%1"
  [(set_attr "type" "reg_fp")])

(define_insn "*itof_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_ITOF))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "itof    %0,%1"
  [(set_attr "type" "itof")])

(define_insn_and_split "*floatsisf2_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
        (unspec:SF [(match_dup 1)] UNSPEC_FLOAD))
   (set (match_dup 0)
        (unspec:SF [(match_dup 0)] UNSPEC_ITOF))]
  ""
  [(set_attr "type" "multi")])

(define_insn "*ftoi_no_ieee"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_FTOI))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "ftoi    %0,%1"
  [(set_attr "type" "ftoi")])

(define_insn "*fstore_no_ieee"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SF 1 "fp_reg_operand" "f")] UNSPEC_FSTORE))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "fstore  %0,%1"
  [(set_attr "type" "fp_reg")])

(define_insn_and_split "fix_truncsfsi2_no_ieee"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" "f"))))
   (clobber (match_scratch:SF 2 "=1"))]
  "TARGET_FPU && !TARGET_FPU_IEEE"
  "#"
  "&& reload_completed"
  [(set (match_dup 1)
        (unspec:SF [(match_dup 1)] UNSPEC_FTOI))
   (set (match_dup 0)
        (unspec:SI [(match_dup 1)] UNSPEC_FSTORE))]
  ""
  [(set_attr "type" "multi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float <-> single integer conversions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*itof"
  [(set (match_operand:SF 0 "fp_reg_operand" "=f")
        (float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU_IEEE"
  "itof    %0,%1"
  [(set_attr "type" "itof")])

(define_expand "floatsisf2"
  [(set (match_operand:SF 0 "fp_reg_operand" "")
        (float:SF (match_operand:SI 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*ftoi"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" "f"))))]
  "TARGET_FPU_IEEE"
  "ftoi    %0,%1"
  [(set_attr "type" "ftoi")])

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(fix:SI (fix:SF (match_operand:SF 1 "fp_reg_operand" ""))))]
  "TARGET_FPU"
{
  if (!TARGET_FPU_IEEE)
    {
      emit_insn (gen_fix_truncsfsi2_no_ieee (operands[0], operands[1]));
      DONE;
    }
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single float comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "*cmpsf_fp"
  [(set (reg:CCFP R_FLAGS)
	(compare:CCFP (match_operand:SF 0 "fp_reg_or_0_operand" "fG")
		      (match_operand:SF 1 "fp_reg_or_0_operand" "fG")))]
  "TARGET_FPU && reload_completed"
  "fcmp    r0,%f0,%f1"
  [(set_attr "type" "fcmp")])

(define_insn "*cmpsf_fpe"
  [(set (reg:CCFPE R_FLAGS)
	(compare:CCFPE (match_operand:SF 0 "fp_reg_or_0_operand" "fG")
		       (match_operand:SF 1 "fp_reg_or_0_operand" "fG")))]
  "TARGET_FPU && reload_completed"
  "fcmpe   r0,%f0,%f1"
  [(set_attr "type" "fcmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conditional branch instructions
;;
;; Note - we do not specify the two instructions necessary to perform
;; a compare-and-branch in the cbranch<mode>4 pattern because that would
;; allow the comparison to be moved away from the jump before the reload
;; pass has completed.  That would be problematical because reload can
;; generate instructions in between which would clobber the CC register.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:I 1 "register_operand")
		        (match_operand:I 2 "reg_or_0_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
)

(define_insn_and_split "*cbranch<mode>4_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:I 1 "register_operand" "r")
 		        (match_operand:I 2 "reg_or_0_operand" "rO")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), operands[1], operands[2],
			operands[3]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*cbranch<mode>4_addv_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_equality_comparison_operator"
		       [(match_operand:I 1 "register_operand" "r")
 		        (unspec:I [(match_operand:I 2 "register_operand" "%r")
				   (match_operand:I 3 "register_operand" "r")]
				  UNSPEC_ADDV)])
		      (label_ref (match_operand 4 ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), XEXP (operands[0], 0),
			XEXP (operands[0], 1), operands[4]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*cbranch<mode>4_subv_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_equality_comparison_operator"
		       [(match_operand:I 1 "register_operand" "r")
 		        (unspec:I [(match_operand:I 2 "reg_or_0_operand" "rO")
				   (match_operand:I 3 "register_operand" "r")]
				  UNSPEC_SUBV)])
		      (label_ref (match_operand 4 ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), XEXP (operands[0], 0),
			XEXP (operands[0], 1), operands[4]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*cbranch<mode>4_negv_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_equality_comparison_operator"
		       [(match_operand:I 1 "register_operand" "r")
 		        (unspec:I [(match_operand:I 2 "register_operand" "r")]
				  UNSPEC_NEGV)])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), XEXP (operands[0], 0),
			XEXP (operands[0], 1), operands[3]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*cbranch<mode>4_btst_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_equality_comparison_operator"
		       [(zero_extract:I
			   (match_operand:I 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand:QI 2 "const_shift_operand" "K"))
		        (const_int 0)])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), XEXP (operands[0], 0),
			XEXP (operands[0], 1), operands[3]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_expand "cbranchsf4"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_fp_comparison_operator"
		       [(match_operand:SF 1 "fp_reg_operand")
		        (match_operand:SF 2 "fp_reg_or_0_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "TARGET_FPU"
)

(define_insn_and_split "*cbranchsf4_insn"
  [(set (pc)
	(if_then_else (match_operator 0 "visium_fp_comparison_operator"
		       [(match_operand:SF 1 "fp_reg_operand" "f")
		        (match_operand:SF 2 "fp_reg_or_0_operand" "fG")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  visium_split_cbranch (GET_CODE (operands[0]), operands[1], operands[2],
			operands[3]);
  DONE;
}
  [(set_attr "type" "fcmp")])

; Now match both normal and inverted branches.

(define_insn "*normal_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "visium_branch_operator"
                       [(reg R_FLAGS) (const_int 0)])
                      (label_ref (match_operand 0 ""))
                      (pc)))]
  "reload_completed"
{
  return output_cbranch (operands[0], GET_CODE (operands[1]),
                         GET_MODE (XEXP (operands[1], 0)), 0, insn);
}
  [(set_attr "type" "branch")])

(define_insn "*inverted_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "visium_branch_operator"
                       [(reg R_FLAGS) (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 ""))))]
  "reload_completed"
{
  return output_cbranch (operands[0], GET_CODE (operands[1]),
                         GET_MODE (XEXP (operands[1], 0)), 1, insn);
}
  [(set_attr "type" "branch")])

; And then match both normal and inverted returns.

(define_insn "*cond_<return_str>return"
  [(set (pc)
        (if_then_else (match_operator 0 "visium_branch_operator"
                       [(reg R_FLAGS) (const_int 0)])
                      (any_return)
                      (pc)))]
  "<return_pred> && reload_completed"
{
  return output_cbranch (pc_rtx, GET_CODE (operands[0]),
                         GET_MODE (XEXP (operands[0], 0)), 0, insn);
}
  [(set_attr "type" "ret")])

(define_insn "*inverted_cond_<return_str>return"
  [(set (pc)
        (if_then_else (match_operator 0 "visium_branch_operator"
                       [(reg R_FLAGS) (const_int 0)])
                      (pc)
                      (any_return)))]
  "<return_pred> && reload_completed"
{
  return output_cbranch (pc_rtx, GET_CODE (operands[0]),
                         GET_MODE (XEXP (operands[0], 0)), 1, insn);
}
  [(set_attr "type" "ret")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Unconditional branch instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
{
  return output_ubranch (operands[0], insn);
}
  [(set_attr "type" "branch")])

(define_insn "indirect_jump"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "r"))]
  ""
  "bra     tr,%0,r0%#		;indirect jump"
  [(set_attr "type" "abs_branch")])

(define_insn "tablejump"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "bra     tr,%0,r0%#		;tablejump"
  [(set_attr "type" "abs_branch")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; trap instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "stop    0,r0"
  [(set_attr "type" "trap")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subprogram call instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Subroutine call instruction returning no value.  Operand 0 is the function
; to call; operand 1 is the number of bytes of arguments pushed (in mode
; 'SImode', except it is normally a 'const_int'); operand 2 is the number of
; registers used as operands.

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (match_dup 3))])]
  ""
{
  if (GET_CODE (XEXP (operands[0], 0)) != REG)
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));

  if (!operands[2])
    operands[2] =  const0_rtx;

  operands[3] = gen_rtx_REG (Pmode, R_LINK);
})

(define_insn "*call_internal"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l,!r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (match_operand 3 "" ""))]
  "!SIBLING_CALL_P (insn)"
  "bra     tr,%0,%3%#		;call"
  [(set_attr "type" "call")])

; Subroutine call instruction returning a value.  Operand 0 is the hard
; register in which the value is returned.  There are three more operands, the
; same as the three operands of the 'call' instruction (but with numbers
; increased by one).

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (match_dup 4))])]
  ""
{
  if (GET_CODE (XEXP (operands[1], 0)) != REG)
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  if (!operands[3])
    operands[3] = const0_rtx;

  operands[4] = gen_rtx_REG (Pmode, R_LINK);
})

(define_insn "*call_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l,!r"))
	      (match_operand 2 "" "")))
	(use (match_operand 3 "" ""))
	(clobber (match_operand 4 "" ""))]
  "!SIBLING_CALL_P (insn)"
  "bra     tr,%1,%4%#		;call value"
  [(set_attr "type" "call")])

; Tail calls are similar, except that the link register is not used.  But
; we don't use r0 as the destination register of the branch because we want
; the Branch Pre-decode Logic of the GR6 to use the Address Load Array to
; predict the branch target.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (match_dup 3))])]
  ""
{
  if (GET_CODE (XEXP (operands[0], 0)) != REG)
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));

  if (!operands[2])
    operands[2] = const0_rtx;

  operands[3] = gen_rtx_SCRATCH (SImode);
})

(define_insn "*sibcall_internal"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "k"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (match_scratch:SI 3 "=0"))]
  "SIBLING_CALL_P (insn)"
  "bra     tr,%0,%0%#		;sibcall"
  [(set_attr "type" "call")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (match_dup 4))])]
  ""
{
  if (GET_CODE (XEXP (operands[1], 0)) != REG)
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  if (!operands[3])
    operands[3] = const0_rtx;

  operands[4] = gen_rtx_SCRATCH (SImode);
})

(define_insn "*sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "k"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (match_scratch:SI 4 "=1"))]
  "SIBLING_CALL_P (insn)"
  "bra     tr,%1,%1%#		;sibcall value"
  [(set_attr "type" "call")])

; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
})

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compare-and-store instructions
;;
;; Modes QI, HI, SI and SF are supported directly.
;;
;; Note - we do not specify the two instructions necessary to perform
;; a compare-and-store in the cstore<mode>4 pattern because that would
;; allow the comparison to be moved away from the store before the reload
;; pass has completed.  That would be problematical because reload can
;; generate instructions in between which would clobber the CC register.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0)
	(match_operator:SI 1 "visium_int_cstore_operator"
	 [(match_operand:I 2 "register_operand")
	  (match_operand:I 3 "reg_or_0_operand")]))]
  ""
{
  visium_expand_int_cstore (operands, <MODE>mode);
  DONE;
})

(define_insn_and_split "*cstore<mode>4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (match_operand:I 1 "register_operand" "r")
		(match_operand:I 2 "reg_or_0_operand" "rO")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (SET, operands[0], NULL_RTX,
		       LTU, operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*neg_cstore<mode>4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ltu:SI (match_operand:I 1 "register_operand" "r")
			(match_operand:I 2 "reg_or_0_operand" "rO"))))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (NEG, operands[0], NULL_RTX,
		       LTU, operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*<add_str>_cstore<mode>4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_add:SI (match_operand:SI 1 "register_operand" "r")
		    (ltu:SI (match_operand:I 2 "register_operand" "r")
			    (match_operand:I 3 "reg_or_0_operand" "rO"))))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (<add_op>, operands[0], operands[1],
		       LTU, operands[2], operands[3]);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*cstore<mode>4_sne_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (not:I (match_operand:I 1 "register_operand" "r"))
		(const_int -1)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (SET, operands[0], NULL_RTX,
		       LTU, gen_rtx_NOT (<MODE>mode, operands[1]), constm1_rtx);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*neg_cstore<mode>4_sne_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ltu:SI (not:I (match_operand:I 1 "register_operand" "r"))
			(const_int -1))))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (NEG, operands[0], NULL_RTX,
		       LTU, gen_rtx_NOT (<MODE>mode, operands[1]), constm1_rtx);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_insn_and_split "*<add_str>_cstore<mode>4_sne_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_add:SI (match_operand:SI 1 "register_operand" "r")
		    (ltu:SI (not:I (match_operand:I 2 "register_operand" "r"))
			    (const_int -1))))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (<add_op>, operands[0], operands[1],
		       LTU, gen_rtx_NOT (<MODE>mode, operands[2]), constm1_rtx);
  DONE;
}
  [(set_attr "type" "cmp")])

(define_expand "cstoresf4"
  [(set (match_operand:SI 0)
	(match_operator:SI 1 "visium_fp_cstore_operator"
	 [(match_operand:SF 2 "fp_reg_operand")
	  (match_operand:SF 3 "fp_reg_or_0_operand")]))]
  "TARGET_FPU"
{
  visium_expand_fp_cstore (operands, SFmode);
  DONE;
})

(define_insn_and_split "*cstoresf4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (match_operand:SF 1 "fp_reg_or_0_operand" "fG")
	       (match_operand:SF 2 "fp_reg_or_0_operand" "fG")))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (SET, operands [0], NULL_RTX,
		       LT, operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "fcmp")])

(define_insn_and_split "*neg_cstoresf4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (lt:SI (match_operand:SF 1 "fp_reg_or_0_operand" "fG")
		       (match_operand:SF 2 "fp_reg_or_0_operand" "fG"))))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (NEG, operands [0], NULL_RTX,
		       LT, operands[1], operands[2]);
  DONE;
}
  [(set_attr "type" "fcmp")])

(define_insn_and_split "*<add_str>_cstoresf4_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_add:SI (match_operand:SI 1 "register_operand" "r")
		    (lt:SI (match_operand:SF 2 "fp_reg_or_0_operand" "fG")
			   (match_operand:SF 3 "fp_reg_or_0_operand" "fG"))))]
  "TARGET_FPU"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  visium_split_cstore (<add_op>, operands [0], operands[1],
		       LT, operands[2], operands[3]);
  DONE;
}
  [(set_attr "type" "fcmp")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RTL pro/epilogue support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

; Expand prologue in RTL
(define_expand "prologue"
  [(const_int 0)]
  ""
{
  visium_expand_prologue ();
  DONE;
})

; Expand epilogue in RTL
(define_expand "epilogue"
  [(return)]
  ""
{
  visium_expand_epilogue ();
})

; Expand epilogue without a final jump in RTL
(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  visium_expand_epilogue ();
  DONE;
})

; The artificial dependency on the link register is to prevent the
; frame instruction from being put in a call delay slot, which can
; confuse the CFI machinery.

(define_insn "stack_save"
  [(set (reg:SI R_FP) (reg:SI R_SP))
   (use (reg:SI R_LINK))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.l  fp,sp		;stack_save"
  [(set_attr "type" "logic")])

; The construct (mem:BLK (scratch)) is considered to alias all other
; memory accesses.  Thus it can be used as a memory barrier in stack
; deallocation patterns.

(define_insn "stack_restore"
  [(set (reg:SI R_SP) (reg:SI R_FP))
   (clobber (mem:BLK (scratch)))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "move.l  sp,fp		;stack_restore"
  [(set_attr "type" "logic")])

(define_insn "stack_pop"
  [(set (reg:SI R_SP)
        (plus:SI (reg:SI R_SP) (match_operand:SI 0 "add_operand" "J,r")))
   (clobber (mem:BLK (scratch)))
   (clobber (reg:CC R_FLAGS))]
  "reload_completed"
  "@
    addi    sp,%0		;stack pop
    add.l   sp,sp,%0		;stack pop"
  [(set_attr "type" "arith")])

(define_expand "<return_str>return"
  [(any_return)]
  "<return_pred>"
  "")

(define_insn "*<return_str>return_internal"
  [(any_return)]
  "!visium_interrupt_function_p ()"
{
  return output_ubranch (pc_rtx, insn);
}
  [(set_attr "type" "ret")])

(define_insn "*return_internal_interrupt"
  [(return)]
  "visium_interrupt_function_p ()"
  "rfi\n\t nop				;return from interrupt"
  [(set_attr "type" "rfi")])

(define_insn "dsi"
  [(unspec_volatile [(const_int 0)] UNSPECV_DSI)]
  ""
  "dsi"
  [(set_attr "type" "dsi")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOP (no-op instruction)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop			;generated nop"
  [(set_attr "type" "nop")])

(define_insn "hazard_nop"
  [(unspec_volatile [(const_int 0)] UNSPEC_NOP)]
  ""
  "nop			;hazard avoidance nop"
  [(set_attr "type" "nop")])

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "nop")])

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; String/block operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; String/block move insn.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:SI  2 "general_operand" ""))
	      (use (match_operand:SI  3 "const_int_operand" ""))])]
  ""
{
  if (visium_expand_block_move (operands))
    DONE;
  else
    FAIL;
})

(define_insn "*bmd"
  [(set (mem:BLK (reg:SI R_R1))
        (mem:BLK (reg:SI R_R2)))
   (use (reg:SI R_R3))
   (clobber (reg:SI R_R1))
   (clobber (reg:SI R_R2))
   (clobber (reg:SI R_R3))
   (clobber (reg:SI R_R4))
   (clobber (reg:SI R_R5))
   (clobber (reg:SI R_R6))]
  "TARGET_BMI"
  "bmd     r1,r2,r3"
  [(set_attr "type" "bmi")])

;; String/block set insn.
;; Argument 0 is the destination
;; Argument 1 is the length
;; Argument 2 is the value
;; Argument 3 is the alignment

(define_expand "setmemsi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand 2 "nonmemory_operand" ""))
	      (use (match_operand:SI  1 "general_operand" ""))
	      (use (match_operand:SI  3 "const_int_operand" ""))])]
  ""
{
  if (visium_expand_block_set (operands))
    DONE;
  else
    FAIL;
})
