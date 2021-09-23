;; Machine description for eBPF.
;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

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

(include "predicates.md")
(include "constraints.md")

;;;; Unspecs

(define_c_enum "unspec" [
  UNSPEC_LDINDABS
  UNSPEC_XADD
])

;;;; Constants

(define_constants
  [(R0_REGNUM		0)
   (R1_REGNUM		1)
   (R2_REGNUM		2)
   (R3_REGNUM		3)
   (R4_REGNUM		4)
   (R5_REGNUM		5)
   (R6_REGNUM		6)
   (R7_REGNUM		7)
   (R8_REGNUM		8)
   (R9_REGNUM		9)
   (R10_REGNUM		10)
   (R11_REGNUM		11)
])

;;;; Attributes

;; Instruction classes.
;; alu		64-bit arithmetic.
;; alu32	32-bit arithmetic.
;; end		endianness conversion instructions.
;; ld		load instructions.
;; lddx		load 64-bit immediate instruction.
;; ldx		generic load instructions.
;; st		generic store instructions for immediates.
;; stx		generic store instructions.
;; jmp		jump instructions.
;; xadd		atomic exchange-and-add instructions.
;; multi	multiword sequence (or user asm statements).

(define_attr "type"
  "unknown,alu,alu32,end,ld,lddw,ldx,st,stx,jmp,xadd,multi"
  (const_string "unknown"))

;; Length of instruction in bytes.
(define_attr "length" ""
  (cond [
         (eq_attr "type" "lddw") (const_int 16)
         ] (const_int 8)))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

;;;; Mode attributes and iterators

(define_mode_attr mop [(QI "b") (HI "h") (SI "w") (DI "dw")
                       (SF "w") (DF "dw")])
(define_mode_attr mtype [(SI "alu32") (DI "alu")])
(define_mode_attr msuffix [(SI "32") (DI "")])

;;;; NOPs

;; The Linux kernel verifier performs some optimizations that rely on
;; nop instructions to be encoded as `ja 0', i.e. a jump to offset 0,
;; which actually means to jump to the next instruction, since in BPF
;; offsets are expressed in 64-bit words _minus one_.

(define_insn "nop"
  [(const_int 0)]
  ""
  "ja\t0"
  [(set_attr "type" "alu")])

;;;; Arithmetic/Logical

;; The arithmetic and logic operations below are defined for SI and DI
;; modes.  The mode iterator AM is used in order to expand to two
;; insns, with the proper modes.
;;
;; 32-bit arithmetic (for SI modes) is implemented using the alu32
;; instructions, if available.

(define_mode_iterator AM [(SI "bpf_has_alu32") DI])

;;; Addition
(define_insn "add<AM:mode>3"
  [(set (match_operand:AM          0 "register_operand"   "=r,r")
        (plus:AM (match_operand:AM 1 "register_operand"   " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" " r,I")))]
  "1"
  "add<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Subtraction

;; Note that subtractions of constants become additions, so there is
;; no need to handle immediate operands in the subMODE3 insns.

(define_insn "sub<AM:mode>3"
  [(set (match_operand:AM          0 "register_operand" "=r")
        (minus:AM (match_operand:AM 1 "register_operand" " 0")
                  (match_operand:AM 2 "register_operand" " r")))]
  ""
  "sub<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Negation
(define_insn "neg<AM:mode>2"
  [(set (match_operand:AM 0 "register_operand" "=r")
        (neg:AM (match_operand:AM 1 "register_operand" " 0")))]
  ""
  "neg<msuffix>\t%0"
  [(set_attr "type" "<mtype>")])

;;; Multiplication
(define_insn "mul<AM:mode>3"
  [(set (match_operand:AM          0 "register_operand"   "=r,r")
        (mult:AM (match_operand:AM 1 "register_operand"   " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "mul<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

(define_insn "*mulsidi3_zeroextend"
  [(set (match_operand:DI	   0 "register_operand" "=r,r")
        (zero_extend:DI
         (mult:SI (match_operand:SI 1 "register_operand" "0,0")
                  (match_operand:SI 2 "reg_or_imm_operand" "r,I"))))]
  ""
  "mul32\t%0,%2"
  [(set_attr "type" "alu32")])

;;; Division

;; Note that eBPF doesn't provide instructions for signed integer
;; division.

(define_insn "udiv<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (udiv:AM (match_operand:AM 1 "register_operand" " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "div<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;; However, xBPF does provide a signed division operator, sdiv.

(define_insn "div<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (div:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  "TARGET_XBPF"
  "sdiv<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Modulus

;; Note that eBPF doesn't provide instructions for signed integer
;; remainder.

(define_insn "umod<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (umod:AM (match_operand:AM 1 "register_operand" " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "mod<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;; Again, xBPF provides a signed version, smod.

(define_insn "mod<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (mod:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  "TARGET_XBPF"
  "smod<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Logical AND
(define_insn "and<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (and:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "and<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Logical inclusive-OR
(define_insn "ior<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (ior:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "or<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;; Logical exclusive-OR
(define_insn "xor<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (xor:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "xor<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;;; Conversions

;;; Zero-extensions

;; For register operands smaller than 32-bit zero-extending is
;; achieved ANDing the value in the source register to a suitable
;; mask.
;;
;; For register operands bigger or equal than 32-bit, we generate a
;; mov32 instruction to zero the high 32-bits of the destination
;; register.
;;
;; For memory operands, of any width, zero-extending is achieved using
;; the ldx{bhwdw} instructions to load the values in registers.

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "0,r,m")))]
  ""
  "@
   and\t%0,0xffff
   mov\t%0,%1\;and\t%0,0xffff
   ldxh\t%0,%1"
  [(set_attr "type" "alu,alu,ldx")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "0,r,m")))]
  ""
  "@
   and\t%0,0xff
   mov\t%0,%1\;and\t%0,0xff
   ldxb\t%0,%1"
  [(set_attr "type" "alu,alu,ldx")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI
	  (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   * return bpf_has_alu32 ? \"mov32\t%0,%1\" : \"mov\t%0,%1\;and\t%0,0xffffffff\";
   ldxw\t%0,%1"
  [(set_attr "type" "alu,ldx")])

;;; Sign-extension

;; Sign-extending a 32-bit value into a 64-bit value is achieved using
;; shifting, with instructions generated by the expand below.

(define_expand "extendsidi2"
  [(set (match_operand:DI 0 "register_operand")
	(sign_extend:DI (match_operand:SI 1 "register_operand")))]
  ""
{
  operands[1] = gen_lowpart (DImode, operands[1]);
  emit_insn (gen_ashldi3 (operands[0], operands[1], GEN_INT (32)));
  emit_insn (gen_ashrdi3 (operands[0], operands[0], GEN_INT (32)));
  DONE;
})

;;;; Data movement

(define_mode_iterator MM [QI HI SI DI SF DF])

(define_expand "mov<MM:mode>"
  [(set (match_operand:MM 0 "general_operand")
        (match_operand:MM 1 "general_operand"))]
        ""
        "
{
  if (!register_operand(operands[0], <MM:MODE>mode)
      && !register_operand(operands[1], <MM:MODE>mode))
    operands[1] = force_reg (<MM:MODE>mode, operands[1]);
}")

(define_insn "*mov<MM:mode>"
  [(set (match_operand:MM 0 "nonimmediate_operand" "=r, r,r,m,m")
        (match_operand:MM 1 "mov_src_operand"      " m,rI,B,r,I"))]
  ""
  "@
   ldx<mop>\t%0,%1
   mov\t%0,%1
   lddw\t%0,%1
   stx<mop>\t%0,%1
   st<mop>\t%0,%1"
[(set_attr "type" "ldx,alu,alu,stx,st")])

;;;; Shifts

(define_mode_iterator SIM [(SI "bpf_has_alu32") DI])

(define_insn "ashr<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"                 "=r,r")
        (ashiftrt:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                      (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "arsh<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

(define_insn "ashl<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"               "=r,r")
        (ashift:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                    (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "lsh<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

(define_insn "lshr<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"                 "=r,r")
        (lshiftrt:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                      (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "rsh<msuffix>\t%0,%2"
  [(set_attr "type" "<mtype>")])

;;;; Conditional branches

;; The eBPF jump instructions use 64-bit arithmetic when evaluating
;; the jump conditions.  Therefore we use DI modes below.

(define_mode_iterator JM [(SI "bpf_has_jmp32") DI])

(define_expand "cbranch<JM:mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
			[(match_operand:JM 1 "register_operand")
			 (match_operand:JM 2 "reg_or_imm_operand")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  if (!ordered_comparison_operator (operands[0], VOIDmode))
    FAIL;

  bpf_expand_cbranch (<JM:MODE>mode, operands);
})

(define_insn "*branch_on_<JM:mode>"
  [(set (pc)
	(if_then_else (match_operator 3 "ordered_comparison_operator"
			 [(match_operand:JM 0 "register_operand" "r")
			  (match_operand:JM 1 "reg_or_imm_operand" "rI")])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  int code = GET_CODE (operands[3]);

  switch (code)
  {
  case EQ: return "jeq<msuffix>\t%0,%1,%2"; break;
  case NE: return "jne<msuffix>\t%0,%1,%2"; break;
  case LT: return "jslt<msuffix>\t%0,%1,%2"; break;
  case LE: return "jsle<msuffix>\t%0,%1,%2"; break;
  case GT: return "jsgt<msuffix>\t%0,%1,%2"; break;
  case GE: return "jsge<msuffix>\t%0,%1,%2"; break;
  case LTU: return "jlt<msuffix>\t%0,%1,%2"; break;
  case LEU: return "jle<msuffix>\t%0,%1,%2"; break;
  case GTU: return "jgt<msuffix>\t%0,%1,%2"; break;
  case GEU: return "jge<msuffix>\t%0,%1,%2"; break;
  default:
    gcc_unreachable ();
    return "";
  }
}
  [(set_attr "type" "jmp")])

;;;; Unconditional branches

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "ja\t%0"
[(set_attr "type" "jmp")])

;;;; Function prologue/epilogue

(define_insn "exit"
  [(simple_return)]
  ""
  "exit"
  [(set_attr "type" "jmp")])

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  bpf_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  bpf_expand_epilogue ();
  DONE;
})

;;;; Function calls

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = XEXP (operands[0], 0);
  emit_call_insn (gen_call_internal (target, operands[1]));
  DONE;
})

(define_insn "call_internal"
  [(call (mem:DI (match_operand:DI 0 "call_operand" "Sr"))
         (match_operand:SI 1 "general_operand" ""))]
  ;; operands[2] is next_arg_register
  ;; operands[3] is struct_value_size_rtx.
  ""
  { return bpf_output_call (operands[0]); }
  [(set_attr "type" "jmp")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = XEXP (operands[1], 0);
  emit_call_insn (gen_call_value_internal (operands[0], target,
                                           operands[2]));
  DONE;
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:DI (match_operand:DI 1 "call_operand" "Sr"))
	      (match_operand:SI 2 "general_operand" "")))]
  ;; operands[3] is next_arg_register
  ;; operands[4] is struct_value_size_rtx.
  ""
  { return bpf_output_call (operands[1]); }
  [(set_attr "type" "jmp")])

(define_insn "sibcall"
  [(call (label_ref (match_operand 0 "" ""))
	 (match_operand:SI 1 "general_operand" ""))]
  ;; operands[2] is next_arg_register
  ;; operands[3] is struct_value_size_rtx.
  ""
  "ja\t%0"
  [(set_attr "type" "jmp")])

;;;; Non-generic load instructions

(define_mode_iterator LDM [QI HI SI DI])
(define_mode_attr ldop [(QI "b") (HI "h") (SI "w") (DI "dw")])

(define_insn "ldind<ldop>"
  [(set (reg:LDM R0_REGNUM)
        (unspec:LDM [(match_operand:DI 0 "register_operand" "r")
                    (match_operand:SI 1 "imm32_operand" "I")]
                    UNSPEC_LDINDABS))
   (clobber (reg:DI R1_REGNUM))
   (clobber (reg:DI R2_REGNUM))
   (clobber (reg:DI R3_REGNUM))
   (clobber (reg:DI R4_REGNUM))]
  ""
  "ldind<ldop>\t%0,%1"
  [(set_attr "type" "ld")])

(define_insn "ldabs<ldop>"
  [(set (reg:LDM R0_REGNUM)
        (unspec:LDM [(match_operand:SI 0 "imm32_operand" "I")
                    (match_operand:SI 1 "imm32_operand" "I")]
                    UNSPEC_LDINDABS))
   (clobber (reg:DI R1_REGNUM))
   (clobber (reg:DI R2_REGNUM))
   (clobber (reg:DI R3_REGNUM))
   (clobber (reg:DI R4_REGNUM))]
  ""
  "ldabs<ldop>\t%0"
  [(set_attr "type" "ld")])

;;;; Atomic increments

(define_mode_iterator AMO [SI DI])

(define_insn "atomic_add<AMO:mode>"
  [(set (match_operand:AMO 0 "memory_operand" "+m")
        (unspec_volatile:AMO
         [(plus:AMO (match_dup 0)
                    (match_operand:AMO 1 "register_operand" "r"))
          (match_operand:SI 2 "const_int_operand")] ;; Memory model.
         UNSPEC_XADD))]
  ""
  "xadd<mop>\t%0,%1"
  [(set_attr "type" "xadd")])
