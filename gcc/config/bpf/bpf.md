;; Machine description for eBPF.
;; Copyright (C) 2019-2025 Free Software Foundation, Inc.

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

;;;; Instruction Scheduler FSM

;; This is just to get INSN_SCHEDULING defined, so that combine does
;; not make paradoxical subregs of memory.  These subregs seems to
;; confuse LRA that ends generating wrong instructions.

(define_automaton "frob")
(define_cpu_unit "frob_unit" "frob")
(define_insn_reservation "frobnicator" 814
  (const_int 0) "frob_unit")

;;;; Unspecs

(define_c_enum "unspec" [
  UNSPEC_LDINDABS
  UNSPEC_AADD
  UNSPEC_AAND
  UNSPEC_AOR
  UNSPEC_AXOR
  UNSPEC_AFADD
  UNSPEC_AFAND
  UNSPEC_AFOR
  UNSPEC_AFXOR
  UNSPEC_AXCHG
  UNSPEC_ACMP
  UNSPEC_CORE_RELOC
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
;; end		endianness conversion or byte swap instructions.
;; ld		load instructions.
;; lddx		load 64-bit immediate instruction.
;; ldx		generic load instructions.
;; st		generic store instructions for immediates.
;; stx		generic store instructions.
;; jmp		jump instructions.
;; multi	multiword sequence (or user asm statements).

(define_attr "type"
  "unknown,alu,alu32,end,ld,lddw,ldx,st,stx,jmp,multi,atomic"
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
(define_mode_attr smop [(QI "u8") (HI "u16") (SI "u32") (DI "u64")
                       (SF "u32") (DF "u64")])
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
  "{ja\t0|goto 0}"
  [(set_attr "type" "alu")])

;;;; Stack usage

(define_expand "allocate_stack"
  [(match_operand:DI 0 "general_operand" "")
   (match_operand:DI 1 "general_operand" "")]
  ""
  "
{
  error (\"BPF does not support dynamic stack allocation\");
  emit_insn (gen_nop ());
  DONE;
}")

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
  "{add<msuffix>\t%0,%2|%w0 += %w2}"
  [(set_attr "type" "<mtype>")])

;;; Subtraction

;; Note that subtractions of constants become additions, so there is
;; no need to handle immediate operands in the subMODE3 insns.

(define_insn "sub<AM:mode>3"
  [(set (match_operand:AM          0 "register_operand" "=r")
        (minus:AM (match_operand:AM 1 "register_operand" " 0")
                  (match_operand:AM 2 "register_operand" " r")))]
  ""
  "{sub<msuffix>\t%0,%2|%w0 -= %w2}"
  [(set_attr "type" "<mtype>")])

;;; Negation
(define_insn "neg<AM:mode>2"
  [(set (match_operand:AM         0 "register_operand" "=r")
        (neg:AM (match_operand:AM 1 "register_operand" " 0")))]
  ""
  "{neg<msuffix>\t%0|%w0 = -%w1}"
  [(set_attr "type" "<mtype>")])

;;; Multiplication
(define_insn "mul<AM:mode>3"
  [(set (match_operand:AM          0 "register_operand"   "=r,r")
        (mult:AM (match_operand:AM 1 "register_operand"   " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "{mul<msuffix>\t%0,%2|%w0 *= %w2}"
  [(set_attr "type" "<mtype>")])

(define_insn "*mulsidi3_zeroextend"
  [(set (match_operand:DI	   0 "register_operand" "=r,r")
        (zero_extend:DI
         (mult:SI (match_operand:SI 1 "register_operand" "0,0")
                  (match_operand:SI 2 "reg_or_imm_operand" "r,I"))))]
  ""
  "{mul32\t%0,%2|%W0 *= %W2}"
  [(set_attr "type" "alu32")])

;;; Division

;; Note that eBPF <= V3 doesn't provide instructions for signed
;; integer division.

(define_insn "udiv<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (udiv:AM (match_operand:AM 1 "register_operand" " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "{div<msuffix>\t%0,%2|%w0 /= %w2}"
  [(set_attr "type" "<mtype>")])

;; However, BPF V4 does provide a signed division operator, sdiv.

(define_insn "div<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (div:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  "bpf_has_sdiv"
  "{sdiv<msuffix>\t%0,%2|%w0 s/= %w2}"
  [(set_attr "type" "<mtype>")])

;;; Modulus

;; Note that eBPF <= V3 doesn't provide instructions for signed
;; integer remainder.

(define_insn "umod<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (umod:AM (match_operand:AM 1 "register_operand" " 0,0")
                 (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "{mod<msuffix>\t%0,%2|%w0 %%= %w2}"
  [(set_attr "type" "<mtype>")])

;; However, BPF V4 does provide a signed modulus operator, smod.

(define_insn "mod<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (mod:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  "bpf_has_sdiv"
  "{smod<msuffix>\t%0,%2|%w0 s%%= %w2}"
  [(set_attr "type" "<mtype>")])

;;; Logical AND
(define_insn "and<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (and:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "{and<msuffix>\t%0,%2|%w0 &= %w2}"
  [(set_attr "type" "<mtype>")])

;;; Logical inclusive-OR
(define_insn "ior<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (ior:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "{or<msuffix>\t%0,%2|%w0 %|= %w2}"
  [(set_attr "type" "<mtype>")])

;;; Logical exclusive-OR
(define_insn "xor<AM:mode>3"
  [(set (match_operand:AM 0 "register_operand" "=r,r")
        (xor:AM (match_operand:AM 1 "register_operand" " 0,0")
                (match_operand:AM 2 "reg_or_imm_operand" "r,I")))]
  ""
  "{xor<msuffix>\t%0,%2|%w0 ^= %w2}"
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
	(zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "0,r,q")))]
  ""
  "@
   {and\t%0,0xffff|%0 &= 0xffff}
   *return bpf_output_move (operands, \"{mov\t%0,%1\;and\t%0,0xffff|%0 = %1;%0 &= 0xffff}\");
   *return bpf_output_move (operands, \"{ldxh\t%0,%1|%0 = *(u16 *) %1}\");"
  [(set_attr "type" "alu,alu,ldx")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "0,r,q")))]
  ""
  "@
   {and\t%0,0xff|%0 &= 0xff}
   *return bpf_output_move (operands, \"{mov\t%0,%1\;and\t%0,0xff|%0 = %1;%0 &= 0xff}\");
   *return bpf_output_move (operands, \"{ldxb\t%0,%1|%0 = *(u8 *) %1}\");"
  [(set_attr "type" "alu,alu,ldx")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI
	  (match_operand:SI 1 "nonimmediate_operand" "r,q")))]
  ""
  "@
   *return bpf_output_move (operands, bpf_has_alu32 ? \"{mov32\t%0,%1|%0 = %1}\" : \"{mov\t%0,%1\;and\t%0,0xffffffff|%0 = %1;%0 &= 0xffffffff}\");
   *return bpf_output_move (operands, \"{ldxw\t%0,%1|%0 = *(u32 *) %1}\");"
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

;; ISA V4 introduces sign-extending move and load operations.

(define_insn "*extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,q")))]
  "bpf_has_smov"
  "@
   *return bpf_output_move (operands, \"{movs\t%0,%1,32|%0 = (s32) %1}\");
   *return bpf_output_move (operands, \"{ldxsw\t%0,%1|%0 = *(s32 *) %1}\");"
  [(set_attr "type" "alu,ldx")])

(define_insn "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:HI 1 "nonimmediate_operand" "r,q")))]
  "bpf_has_smov"
  "@
   *return bpf_output_move (operands, \"{movs\t%0,%1,16|%0 = (s16) %1}\");
   *return bpf_output_move (operands, \"{ldxsh\t%0,%1|%0 = *(s16 *) %1}\");"
  [(set_attr "type" "alu,ldx")])

(define_insn "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:QI 1 "nonimmediate_operand" "r,q")))]
  "bpf_has_smov"
  "@
   *return bpf_output_move (operands, \"{movs\t%0,%1,8|%0 = (s8) %1}\");
   *return bpf_output_move (operands, \"{ldxsb\t%0,%1|%0 = *(s8 *) %1}\");"
  [(set_attr "type" "alu,ldx")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  "bpf_has_smov"
  "*return bpf_output_move (operands, \"{movs32\t%0,%1,16|%w0 = (s16) %w1}\");"
  [(set_attr "type" "alu")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "bpf_has_smov"
  "*return bpf_output_move (operands, \"{movs32\t%0,%1,8|%w0 = (s8) %w1}\");"
  [(set_attr "type" "alu")])

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
  [(set (match_operand:MM 0 "nonimmediate_operand" "=r,  r, r,q,q")
        (match_operand:MM 1 "mov_src_operand"      " q,rIc,BC,r,I"))]
  ""
  "@
   *return bpf_output_move (operands, \"{ldx<mop>\t%0,%1|%0 = *(<smop> *) %1}\");
   *return bpf_output_move (operands, \"{mov\t%0,%1|%0 = %1}\");
   *return bpf_output_move (operands, \"{lddw\t%0,%1|%0 = %1 ll}\");
   *return bpf_output_move (operands, \"{stx<mop>\t%0,%1|*(<smop> *) %0 = %1}\");
   *return bpf_output_move (operands, \"{st<mop>\t%0,%1|*(<smop> *) %0 = %1}\");"
[(set_attr "type" "ldx,alu,alu,stx,st")])

;;;; Shifts

(define_mode_iterator SIM [(SI "bpf_has_alu32") DI])

(define_insn "ashr<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"                 "=r,r")
        (ashiftrt:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                      (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "{arsh<msuffix>\t%0,%2|%w0 s>>= %w2}"
  [(set_attr "type" "<mtype>")])

(define_insn "ashl<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"               "=r,r")
        (ashift:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                    (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "{lsh<msuffix>\t%0,%2|%w0 <<= %w2}"
  [(set_attr "type" "<mtype>")])

(define_insn "lshr<SIM:mode>3"
  [(set (match_operand:SIM 0 "register_operand"                 "=r,r")
        (lshiftrt:SIM (match_operand:SIM 1 "register_operand"   " 0,0")
                      (match_operand:SIM 2 "reg_or_imm_operand" " r,I")))]
  ""
  "{rsh<msuffix>\t%0,%2|%w0 >>= %w2}"
  [(set_attr "type" "<mtype>")])

;;;; Byte swapping

(define_mode_iterator BSM [HI SI DI])
(define_mode_attr endmode [(HI "16") (SI "32") (DI "64")])

(define_insn "bswap<BSM:mode>2"
  [(set (match_operand:BSM 0 "register_operand"            "=r")
        (bswap:BSM (match_operand:BSM 1 "register_operand" " 0")))]
  ""
{
  if (bpf_has_bswap)
    return "{bswap\t%0, <endmode>|%0 = bswap<endmode> %1}";
  else
    {
      if (TARGET_BIG_ENDIAN)
        return "{endle\t%0, <endmode>|%0 = le<endmode> %1}";
      else
        return "{endbe\t%0, <endmode>|%0 = be<endmode> %1}";
    }
}
  [(set_attr "type" "end")])

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
  case EQ: return  "{jeq<msuffix>\t%0,%1,%2|if %w0 == %w1 goto %2}"; break;
  case NE: return  "{jne<msuffix>\t%0,%1,%2|if %w0 != %w1 goto %2}"; break;
  case LT: return  "{jslt<msuffix>\t%0,%1,%2|if %w0 s< %w1 goto %2}"; break;
  case LE: return  "{jsle<msuffix>\t%0,%1,%2|if %w0 s<= %w1 goto %2}"; break;
  case GT: return  "{jsgt<msuffix>\t%0,%1,%2|if %w0 s> %w1 goto %2}"; break;
  case GE: return  "{jsge<msuffix>\t%0,%1,%2|if %w0 s>= %w1 goto %2}"; break;
  case LTU: return "{jlt<msuffix>\t%0,%1,%2|if %w0 < %w1 goto %2}"; break;
  case LEU: return "{jle<msuffix>\t%0,%1,%2|if %w0 <= %w1 goto %2}"; break;
  case GTU: return "{jgt<msuffix>\t%0,%1,%2|if %w0 > %w1 goto %2}"; break;
  case GEU: return "{jge<msuffix>\t%0,%1,%2|if %w0 >= %w1 goto %2}"; break;
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
  "{ja\t%0|goto %0}"
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
  "{ja\t%0|goto %0}"
  [(set_attr "type" "jmp")])

;;;; Non-generic load instructions

(define_mode_iterator LDM [QI HI SI DI])
(define_mode_attr ldop [(QI "b") (HI "h") (SI "w") (DI "dw")])
(define_mode_attr pldop [(QI "u8") (HI "u16") (SI "u32") (DI "u64")])

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
  "{ldind<ldop>\t%0,%1|r0 = *(<pldop> *) skb[%0 + %1]}"
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
  "{ldabs<ldop>\t%0|r0 = *(<pldop> *) skb[%0]}"
  [(set_attr "type" "ld")])

;;; memmove and memcopy

;; 0 is dst
;; 1 is src
;; 2 is size of copy in bytes
;; 3 is alignment

(define_expand "cpymemdi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:DI 2 "general_operand")
   (match_operand:DI 3 "immediate_operand")]
   ""
{
  if (bpf_expand_cpymem (operands, false))
    DONE;
  FAIL;
})

;; 0 is dst
;; 1 is src
;; 2 is size of copy in bytes
;; 3 is alignment

(define_expand "movmemdi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:DI 2 "general_operand")
   (match_operand:DI 3 "immediate_operand")]
   ""
{
  if (bpf_expand_cpymem (operands, true))
    DONE;
  FAIL;
})

;; memset
;; 0 is dst
;; 1 is length
;; 2 is value
;; 3 is alignment
(define_expand "setmemdi"
  [(set (match_operand:BLK 0 "memory_operand")
	(match_operand:QI  2 "nonmemory_operand"))
   (use (match_operand:DI  1 "general_operand"))
   (match_operand 3 "immediate_operand")]
 ""
 {
  if (bpf_expand_setmem (operands))
    DONE;
  FAIL;
})

(include "atomic.md")
