;; Machine description for RISC-V for GNU compiler.
;; Copyright (C) 2011-2019 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.

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

(define_c_enum "unspec" [
  ;; Override return address for exception handling.
  UNSPEC_EH_RETURN

  ;; Symbolic accesses.  The order of this list must match that of
  ;; enum riscv_symbol_type in riscv-protos.h.
  UNSPEC_ADDRESS_FIRST
  UNSPEC_PCREL
  UNSPEC_LOAD_GOT
  UNSPEC_TLS
  UNSPEC_TLS_LE
  UNSPEC_TLS_IE
  UNSPEC_TLS_GD

  ;; High part of PC-relative address.
  UNSPEC_AUIPC

  ;; Floating-point unspecs.
  UNSPEC_FLT_QUIET
  UNSPEC_FLE_QUIET
  UNSPEC_COPYSIGN
  UNSPEC_LRINT
  UNSPEC_LROUND

  ;; Stack tie
  UNSPEC_TIE
])

(define_c_enum "unspecv" [
  ;; Register save and restore.
  UNSPECV_GPR_SAVE
  UNSPECV_GPR_RESTORE

  ;; Floating-point unspecs.
  UNSPECV_FRFLAGS
  UNSPECV_FSFLAGS

  ;; Interrupt handler instructions.
  UNSPECV_MRET
  UNSPECV_SRET
  UNSPECV_URET

  ;; Blockage and synchronization.
  UNSPECV_BLOCKAGE
  UNSPECV_FENCE
  UNSPECV_FENCE_I
])

(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (GP_REGNUM 			3)
   (T0_REGNUM			5)
   (T1_REGNUM			6)
   (S0_REGNUM			8)
   (S1_REGNUM			9)
   (S2_REGNUM			18)

   (NORMAL_RETURN		0)
   (SIBCALL_RETURN		1)
   (EXCEPTION_RETURN		2)
])

(include "predicates.md")
(include "constraints.md")

;; ....................
;;
;;	Attributes
;;
;; ....................

(define_attr "got" "unset,xgot_high,load"
  (const_string "unset"))

;; Classification of moves, extensions and truncations.  Most values
;; are as for "type" (see below) but there are also the following
;; move-specific values:
;;
;; andi		a single ANDI instruction
;; shift_shift	a shift left followed by a shift right
;;
;; This attribute is used to determine the instruction's length and
;; scheduling type.  For doubleword moves, the attribute always describes
;; the split instructions; in some cases, it is more appropriate for the
;; scheduling type to be "multi" instead.
(define_attr "move_type"
  "unknown,load,fpload,store,fpstore,mtc,mfc,move,fmove,
   const,logical,arith,andi,shift_shift"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF"
  (const_string "unknown"))

;; True if the main data type is twice the size of a word.
(define_attr "dword_mode" "no,yes"
  (cond [(and (eq_attr "mode" "DI,DF")
	      (eq (symbol_ref "TARGET_64BIT") (const_int 0)))
	 (const_string "yes")

	 (and (eq_attr "mode" "TI,TF")
	      (ne (symbol_ref "TARGET_64BIT") (const_int 0)))
	 (const_string "yes")]
	(const_string "no")))

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; fpload	floating point load
;; store	store instruction(s)
;; fpstore	floating point store
;; mtc		transfer to coprocessor
;; mfc		transfer from coprocessor
;; const	load constant
;; arith	integer arithmetic instructions
;; logical      integer logical instructions
;; shift	integer shift instructions
;; slt		set less than instructions
;; imul		integer multiply 
;; idiv		integer divide
;; move		integer register move (addi rd, rs1, 0)
;; fmove	floating point register move
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; multi	multiword sequence (or user asm statements)
;; nop		no operation
;; ghost	an instruction that produces no real code
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,store,fpstore,
   mtc,mfc,const,arith,logical,shift,slt,imul,idiv,move,fmove,fadd,fmul,
   fmadd,fdiv,fcmp,fcvt,fsqrt,multi,auipc,sfb_alu,nop,ghost"
  (cond [(eq_attr "got" "load") (const_string "load")

	 ;; If a doubleword move uses these expensive instructions,
	 ;; it is usually better to schedule them in the same way
	 ;; as the singleword form, rather than as "multi".
	 (eq_attr "move_type" "load") (const_string "load")
	 (eq_attr "move_type" "fpload") (const_string "fpload")
	 (eq_attr "move_type" "store") (const_string "store")
	 (eq_attr "move_type" "fpstore") (const_string "fpstore")
	 (eq_attr "move_type" "mtc") (const_string "mtc")
	 (eq_attr "move_type" "mfc") (const_string "mfc")

	 ;; These types of move are always single insns.
	 (eq_attr "move_type" "fmove") (const_string "fmove")
	 (eq_attr "move_type" "arith") (const_string "arith")
	 (eq_attr "move_type" "logical") (const_string "logical")
	 (eq_attr "move_type" "andi") (const_string "logical")

	 ;; These types of move are always split.
	 (eq_attr "move_type" "shift_shift")
	   (const_string "multi")

	 ;; These types of move are split for doubleword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "dword_mode" "yes"))
	   (const_string "multi")
	 (eq_attr "move_type" "move") (const_string "move")
	 (eq_attr "move_type" "const") (const_string "const")]
	(const_string "unknown")))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [
	  ;; Branches further than +/- 4 KiB require two instructions.
	  (eq_attr "type" "branch")
	  (if_then_else (and (le (minus (match_dup 0) (pc)) (const_int 4088))
				  (le (minus (pc) (match_dup 0)) (const_int 4092)))
	  (const_int 4)
	  (const_int 8))

	  ;; Conservatively assume calls take two instructions (AUIPC + JALR).
	  ;; The linker will opportunistically relax the sequence to JAL.
	  (eq_attr "type" "call") (const_int 8)

	  ;; "Ghost" instructions occupy no space.
	  (eq_attr "type" "ghost") (const_int 0)

	  (eq_attr "got" "load") (const_int 8)

	  (eq_attr "type" "fcmp") (const_int 8)

	  ;; SHIFT_SHIFTs are decomposed into two separate instructions.
	  (eq_attr "move_type" "shift_shift")
		(const_int 8)

	  ;; Check for doubleword moves that are decomposed into two
	  ;; instructions.
	  (and (eq_attr "move_type" "mtc,mfc,move")
	       (eq_attr "dword_mode" "yes"))
	  (const_int 8)

	  ;; Doubleword CONST{,N} moves are split into two word
	  ;; CONST{,N} moves.
	  (and (eq_attr "move_type" "const")
	       (eq_attr "dword_mode" "yes"))
	  (symbol_ref "riscv_split_const_insns (operands[1]) * 4")

	  ;; Otherwise, constants, loads and stores are handled by external
	  ;; routines.
	  (eq_attr "move_type" "load,fpload")
	  (symbol_ref "riscv_load_store_insns (operands[1], insn) * 4")
	  (eq_attr "move_type" "store,fpstore")
	  (symbol_ref "riscv_load_store_insns (operands[0], insn) * 4")
	  ] (const_int 4)))

;; Is copying of this instruction disallowed?
(define_attr "cannot_copy" "no,yes" (const_string "no"))

;; Microarchitectures we know how to tune for.
;; Keep this in sync with enum riscv_microarchitecture.
(define_attr "tune"
  "generic,sifive_7"
  (const (symbol_ref "((enum attr_tune) riscv_microarchitecture)")))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

;; This mode iterator allows 32-bit and 64-bit GPR patterns to be generated
;; from the same template.
(define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; Likewise, but for XLEN-sized quantities.
(define_mode_iterator X [(SI "!TARGET_64BIT") (DI "TARGET_64BIT")])

;; Branches operate on XLEN-sized quantities, but for RV64 we accept
;; QImode values so we can force zero-extension.
(define_mode_iterator BR [(QI "TARGET_64BIT") SI (DI "TARGET_64BIT")])

;; 32-bit moves for which we provide move patterns.
(define_mode_iterator MOVE32 [SI])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64 [DI DF])

;; Iterator for sub-32-bit integer modes.
(define_mode_iterator SHORT [QI HI])

;; Iterator for HImode constant generation.
(define_mode_iterator HISI [HI SI])

;; Iterator for QImode extension patterns.
(define_mode_iterator SUPERQI [HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware integer modes narrower than XLEN.
(define_mode_iterator SUBX [QI HI (SI "TARGET_64BIT")])

;; Iterator for hardware-supported integer modes.
(define_mode_iterator ANYI [QI HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware-supported floating-point modes.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT")
			    (DF "TARGET_DOUBLE_FLOAT")])

;; Iterator for floating-point modes that can be loaded into X registers.
(define_mode_iterator SOFTF [SF (DF "TARGET_64BIT")])

;; This attribute gives the length suffix for a sign- or zero-extension
;; instruction.
(define_mode_attr size [(QI "b") (HI "h")])

;; Mode attributes for loads.
(define_mode_attr load [(QI "lb") (HI "lh") (SI "lw") (DI "ld") (SF "flw") (DF "fld")])

;; Instruction names for integer loads that aren't explicitly sign or zero
;; extended.  See riscv_output_move and LOAD_EXTEND_OP.
(define_mode_attr default_load [(QI "lbu") (HI "lhu") (SI "lw") (DI "ld")])

;; Mode attribute for FP loads into integer registers.
(define_mode_attr softload [(SF "lw") (DF "ld")])

;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd") (SF "fsw") (DF "fsd")])

;; Instruction names for FP stores from integer registers.
(define_mode_attr softstore [(SF "sw") (DF "sd")])

;; This attribute gives the best constraint to use for registers of
;; a given mode.
(define_mode_attr reg [(SI "d") (DI "d") (CC "d")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d")])

;; This attribute gives the integer suffix for floating-point conversions.
(define_mode_attr ifmt [(SI "w") (DI "l")])

;; This attribute gives the format suffix for atomic memory operations.
(define_mode_attr amo [(SI "w") (DI "d")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (TF "DI")])

;; Iterator and attributes for floating-point rounding instructions.
(define_int_iterator RINT [UNSPEC_LRINT UNSPEC_LROUND])
(define_int_attr rint_pattern [(UNSPEC_LRINT "rint") (UNSPEC_LROUND "round")])
(define_int_attr rint_rm [(UNSPEC_LRINT "dyn") (UNSPEC_LROUND "rmm")])

;; Iterator and attributes for quiet comparisons.
(define_int_iterator QUIET_COMPARISON [UNSPEC_FLT_QUIET UNSPEC_FLE_QUIET])
(define_int_attr quiet_pattern [(UNSPEC_FLT_QUIET "lt") (UNSPEC_FLE_QUIET "le")])

;; This code iterator allows signed and unsigned widening multiplications
;; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

;; This code iterator allows the two right shift instructions to be
;; generated from the same template.
(define_code_iterator any_shiftrt [ashiftrt lshiftrt])

;; This code iterator allows the three shift instructions to be generated
;; from the same template.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; This code iterator allows the three bitwise instructions to be generated
;; from the same template.
(define_code_iterator any_bitwise [and ior xor])

;; This code iterator allows unsigned and signed division to be generated
;; from the same template.
(define_code_iterator any_div [div udiv mod umod])

;; This code iterator allows unsigned and signed modulus to be generated
;; from the same template.
(define_code_iterator any_mod [mod umod])

;; These code iterators allow the signed and unsigned scc operations to use
;; the same template.
(define_code_iterator any_gt [gt gtu])
(define_code_iterator any_ge [ge geu])
(define_code_iterator any_lt [lt ltu])
(define_code_iterator any_le [le leu])

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")
		     (gt "") (gtu "u")
		     (ge "") (geu "u")
		     (lt "") (ltu "u")
		     (le "") (leu "u")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (div "div")
			 (mod "mod")
			 (udiv "udiv")
			 (umod "umod")
			 (ge "ge")
			 (le "le")
			 (gt "gt")
			 (lt "lt")
			 (ior "ior")
			 (xor "xor")
			 (and "and")
			 (plus "add")
			 (minus "sub")])

;; <insn> expands to the name of the insn that implements a particular code.
(define_code_attr insn [(ashift "sll")
			(ashiftrt "sra")
			(lshiftrt "srl")
			(div "div")
			(mod "rem")
			(udiv "divu")
			(umod "remu")
			(ior "or")
			(xor "xor")
			(and "and")
			(plus "add")
			(minus "sub")])

;; Ghost instructions produce no real code and introduce no hazards.
;; They exist purely to express an effect on dataflow.
(define_insn_reservation "ghost" 0
  (eq_attr "type" "ghost")
  "nothing")

;;
;;  ....................
;;
;;	ADDITION
;;
;;  ....................
;;

(define_insn "add<mode>3"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(plus:ANYF (match_operand:ANYF 1 "register_operand" " f")
		   (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "addsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" " r,r")
		 (match_operand:SI 2 "arith_operand"    " r,I")))]
  ""
  { return TARGET_64BIT ? "add%i2w\t%0,%1,%2" : "add%i2\t%0,%1,%2"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "adddi3"
  [(set (match_operand:DI          0 "register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "register_operand" " r,r")
		 (match_operand:DI 2 "arith_operand"    " r,I")))]
  "TARGET_64BIT"
  "add%i2\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*addsi3_extended"
  [(set (match_operand:DI               0 "register_operand" "=r,r")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" " r,r")
		      (match_operand:SI 2 "arith_operand"    " r,I"))))]
  "TARGET_64BIT"
  "add%i2w\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_extended2"
  [(set (match_operand:DI                       0 "register_operand" "=r,r")
	(sign_extend:DI
	  (subreg:SI (plus:DI (match_operand:DI 1 "register_operand" " r,r")
			      (match_operand:DI 2 "arith_operand"    " r,I"))
		     0)))]
  "TARGET_64BIT"
  "add%i2w\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	SUBTRACTION
;;
;;  ....................
;;

(define_insn "sub<mode>3"
  [(set (match_operand:ANYF             0 "register_operand" "=f")
	(minus:ANYF (match_operand:ANYF 1 "register_operand" " f")
		    (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fsub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "subdi3"
  [(set (match_operand:DI 0            "register_operand" "= r")
	(minus:DI (match_operand:DI 1  "reg_or_0_operand" " rJ")
		   (match_operand:DI 2 "register_operand" "  r")))]
  "TARGET_64BIT"
  "sub\t%0,%z1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "subsi3"
  [(set (match_operand:SI           0 "register_operand" "= r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " rJ")
		  (match_operand:SI 2 "register_operand" "  r")))]
  ""
  { return TARGET_64BIT ? "subw\t%0,%z1,%2" : "sub\t%0,%z1,%2"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_extended"
  [(set (match_operand:DI               0 "register_operand" "= r")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "reg_or_0_operand" " rJ")
		      (match_operand:SI 2 "register_operand" "  r"))))]
  "TARGET_64BIT"
  "subw\t%0,%z1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_extended2"
  [(set (match_operand:DI                        0 "register_operand" "= r")
	(sign_extend:DI
	  (subreg:SI (minus:DI (match_operand:DI 1 "reg_or_0_operand" " rJ")
			       (match_operand:DI 2 "register_operand" "  r"))
		     0)))]
  "TARGET_64BIT"
  "subw\t%0,%z1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "negdi2"
  [(set (match_operand:DI         0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" " r")))]
  "TARGET_64BIT"
  "neg\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" " r")))]
  ""
  { return TARGET_64BIT ? "negw\t%0,%1" : "neg\t%0,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_extended"
  [(set (match_operand:DI          0 "register_operand" "=r")
	(sign_extend:DI
	 (neg:SI (match_operand:SI 1 "register_operand" " r"))))]
  "TARGET_64BIT"
  "negw\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_extended2"
  [(set (match_operand:DI                     0 "register_operand" "=r")
	(sign_extend:DI
	 (subreg:SI (neg:DI (match_operand:DI 1 "register_operand" " r"))
	 	    0)))]
  "TARGET_64BIT"
  "negw\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_insn "mul<mode>3"
  [(set (match_operand:ANYF               0 "register_operand" "=f")
	(mult:ANYF (match_operand:ANYF    1 "register_operand" " f")
		      (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fmul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_MUL"
  { return TARGET_64BIT ? "mulw\t%0,%1,%2" : "mul\t%0,%1,%2"; }
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "muldi3"
  [(set (match_operand:DI          0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "register_operand" " r")
		 (match_operand:DI 2 "register_operand" " r")))]
  "TARGET_MUL && TARGET_64BIT"
  "mul\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_insn "*mulsi3_extended"
  [(set (match_operand:DI              0 "register_operand" "=r")
	(sign_extend:DI
	    (mult:SI (match_operand:SI 1 "register_operand" " r")
		     (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_MUL && TARGET_64BIT"
  "mulw\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "*mulsi3_extended2"
  [(set (match_operand:DI                       0 "register_operand" "=r")
	(sign_extend:DI
	  (subreg:SI (mult:DI (match_operand:DI 1 "register_operand" " r")
			      (match_operand:DI 2 "register_operand" " r"))
		     0)))]
  "TARGET_MUL && TARGET_64BIT"
  "mulw\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;;
;;  ........................
;;
;;	MULTIPLICATION HIGH-PART
;;
;;  ........................
;;


(define_expand "<u>mulditi3"
  [(set (match_operand:TI                         0 "register_operand")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		 (any_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_MUL && TARGET_64BIT"
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_<u>muldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "<u>muldi3_highpart"
  [(set (match_operand:DI                0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (any_extend:TI
		       (match_operand:DI 1 "register_operand" " r"))
		     (any_extend:TI
		       (match_operand:DI 2 "register_operand" " r")))
	    (const_int 64))))]
  "TARGET_MUL && TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "usmulditi3"
  [(set (match_operand:TI                          0 "register_operand")
	(mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand"))
		 (sign_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_MUL && TARGET_64BIT"
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_usmuldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "usmuldi3_highpart"
  [(set (match_operand:DI                0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (zero_extend:TI
		       (match_operand:DI 1 "register_operand"  "r"))
		     (sign_extend:TI
		       (match_operand:DI 2 "register_operand" " r")))
	    (const_int 64))))]
  "TARGET_MUL && TARGET_64BIT"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI            0 "register_operand" "=r")
	(mult:DI (any_extend:DI
		   (match_operand:SI 1 "register_operand" " r"))
		 (any_extend:DI
		   (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_MUL && !TARGET_64BIT"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
  emit_insn (gen_<u>mulsi3_highpart (riscv_subword (operands[0], true),
				     operands[1], operands[2]));
  emit_insn (gen_movsi (riscv_subword (operands[0], false), temp));
  DONE;
})

(define_insn "<u>mulsi3_highpart"
  [(set (match_operand:SI                0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (any_extend:DI
		       (match_operand:SI 1 "register_operand" " r"))
		     (any_extend:DI
		       (match_operand:SI 2 "register_operand" " r")))
	    (const_int 32))))]
  "TARGET_MUL && !TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])


(define_expand "usmulsidi3"
  [(set (match_operand:DI            0 "register_operand" "=r")
	(mult:DI (zero_extend:DI
		   (match_operand:SI 1 "register_operand" " r"))
		 (sign_extend:DI
		   (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_MUL && !TARGET_64BIT"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
  emit_insn (gen_usmulsi3_highpart (riscv_subword (operands[0], true),
				     operands[1], operands[2]));
  emit_insn (gen_movsi (riscv_subword (operands[0], false), temp));
  DONE;
})

(define_insn "usmulsi3_highpart"
  [(set (match_operand:SI                0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI
		       (match_operand:SI 1 "register_operand" " r"))
		     (sign_extend:DI
		       (match_operand:SI 2 "register_operand" " r")))
	    (const_int 32))))]
  "TARGET_MUL && !TARGET_64BIT"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

(define_insn "<optab>si3"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(any_div:SI (match_operand:SI 1 "register_operand" " r")
		    (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_DIV"
  { return TARGET_64BIT ? "<insn>%i2w\t%0,%1,%2" : "<insn>%i2\t%0,%1,%2"; }
  [(set_attr "type" "idiv")
   (set_attr "mode" "SI")])

(define_insn "<optab>di3"
  [(set (match_operand:DI             0 "register_operand" "=r")
	(any_div:DI (match_operand:DI 1 "register_operand" " r")
		    (match_operand:DI 2 "register_operand" " r")))]
  "TARGET_DIV && TARGET_64BIT"
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "idiv")
   (set_attr "mode" "DI")])

(define_insn "*<optab>si3_extended"
  [(set (match_operand:DI                 0 "register_operand" "=r")
	(sign_extend:DI
	    (any_div:SI (match_operand:SI 1 "register_operand" " r")
			(match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_DIV && TARGET_64BIT"
  "<insn>%i2w\t%0,%1,%2"
  [(set_attr "type" "idiv")
   (set_attr "mode" "DI")])

(define_insn "div<mode>3"
  [(set (match_operand:ANYF           0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" " f")
		  (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT && TARGET_FDIV"
  "fdiv.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fdiv")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_insn "sqrt<mode>2"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT && TARGET_FDIV"
{
    return "fsqrt.<fmt>\t%0,%1";
}
  [(set_attr "type" "fsqrt")
   (set_attr "mode" "<UNITMODE>")])

;; Floating point multiply accumulate instructions.

;; a * b + c
(define_insn "fma<mode>4"
  [(set (match_operand:ANYF           0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" " f")
		  (match_operand:ANYF 2 "register_operand" " f")
		  (match_operand:ANYF 3 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; a * b - c
(define_insn "fms<mode>4"
  [(set (match_operand:ANYF                     0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF           1 "register_operand" " f")
		  (match_operand:ANYF           2 "register_operand" " f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT"
  "fmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -a * b - c
(define_insn "fnms<mode>4"
  [(set (match_operand:ANYF               0 "register_operand" "=f")
	(fma:ANYF
	    (neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
	    (match_operand:ANYF           2 "register_operand" " f")
	    (neg:ANYF (match_operand:ANYF 3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -a * b + c
(define_insn "fnma<mode>4"
  [(set (match_operand:ANYF               0 "register_operand" "=f")
	(fma:ANYF
	    (neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
	    (match_operand:ANYF           2 "register_operand" " f")
	    (match_operand:ANYF           3 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(-a * b - c), modulo signed zeros
(define_insn "*fma<mode>4"
  [(set (match_operand:ANYF                   0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
		(match_operand:ANYF           2 "register_operand" " f")
		(neg:ANYF (match_operand:ANYF 3 "register_operand" " f")))))]
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(-a * b + c), modulo signed zeros
(define_insn "*fms<mode>4"
  [(set (match_operand:ANYF                   0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
		(match_operand:ANYF           2 "register_operand" " f")
		(match_operand:ANYF           3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(a * b + c), modulo signed zeros
(define_insn "*fnms<mode>4"
  [(set (match_operand:ANYF         0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF 1 "register_operand" " f")
		(match_operand:ANYF 2 "register_operand" " f")
		(match_operand:ANYF 3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(a * b - c), modulo signed zeros
(define_insn "*fnma<mode>4"
  [(set (match_operand:ANYF                   0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF           1 "register_operand" " f")
		(match_operand:ANYF           2 "register_operand" " f")
		(neg:ANYF (match_operand:ANYF 3 "register_operand" " f")))))]
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	SIGN INJECTION
;;
;;  ....................

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF           0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fabs.<fmt>\t%0,%1"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "copysign<mode>3"
  [(set (match_operand:ANYF 0 "register_operand"               "=f")
	(unspec:ANYF [(match_operand:ANYF 1 "register_operand" " f")
		      (match_operand:ANYF 2 "register_operand" " f")]
		     UNSPEC_COPYSIGN))]
  "TARGET_HARD_FLOAT"
  "fsgnj.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF           0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fneg.<fmt>\t%0,%1"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	MIN/MAX
;;
;;  ....................

(define_insn "smin<mode>3"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(smin:ANYF (match_operand:ANYF 1 "register_operand" " f")
		   (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(smax:ANYF (match_operand:ANYF 1 "register_operand" " f")
		   (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

;; For RV64, we don't expose the SImode operations to the rtl expanders,
;; but SImode versions exist for combine.

(define_insn "<optab><mode>3"
  [(set (match_operand:X                0 "register_operand" "=r,r")
	(any_bitwise:X (match_operand:X 1 "register_operand" "%r,r")
		       (match_operand:X 2 "arith_operand"    " r,I")))]
  ""
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*<optab>si3_internal"
  [(set (match_operand:SI                 0 "register_operand" "=r,r")
	(any_bitwise:SI (match_operand:SI 1 "register_operand" "%r,r")
			(match_operand:SI 2 "arith_operand"    " r,I")))]
  "TARGET_64BIT"
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:X        0 "register_operand" "=r")
	(not:X (match_operand:X 1 "register_operand" " r")))]
  ""
  "not\t%0,%1"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*one_cmplsi2_internal"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" " r")))]
  "TARGET_64BIT"
  "not\t%0,%1"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	TRUNCATION
;;
;;  ....................

(define_insn "truncdfsf2"
  [(set (match_operand:SF     0 "register_operand" "=f")
	(float_truncate:SF
	    (match_operand:DF 1 "register_operand" " f")))]
  "TARGET_DOUBLE_FLOAT"
  "fcvt.s.d\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(zero_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT"
  "@
   #
   lwu\t%0,%1"
  "&& reload_completed
   && REG_P (operands[1])
   && !paradoxical_subreg_p (operands[0])"
  [(set (match_dup 0)
	(ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
	(lshiftrt:DI (match_dup 0) (const_int 32)))]
  { operands[1] = gen_lowpart (DImode, operands[1]); }
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "DI")])

(define_insn_and_split "zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand"     "=r,r")
	(zero_extend:GPR
	    (match_operand:HI 1 "nonimmediate_operand" " r,m")))]
  ""
  "@
   #
   lhu\t%0,%1"
  "&& reload_completed
   && REG_P (operands[1])
   && !paradoxical_subreg_p (operands[0])"
  [(set (match_dup 0)
	(ashift:GPR (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(lshiftrt:GPR (match_dup 0) (match_dup 2)))]
  {
    operands[1] = gen_lowpart (<GPR:MODE>mode, operands[1]);
    operands[2] = GEN_INT(GET_MODE_BITSIZE(<GPR:MODE>mode) - 16);
  }
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "zero_extendqi<SUPERQI:mode>2"
  [(set (match_operand:SUPERQI 0 "register_operand"    "=r,r")
	(zero_extend:SUPERQI
	    (match_operand:QI 1 "nonimmediate_operand" " r,m")))]
  ""
  "@
   andi\t%0,%1,0xff
   lbu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "mode" "<SUPERQI:MODE>")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

(define_insn "extendsidi2"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(sign_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT"
  "@
   sext.w\t%0,%1
   lw\t%0,%1"
  [(set_attr "move_type" "move,load")
   (set_attr "mode" "DI")])

(define_insn_and_split "extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI   0 "register_operand"     "=r,r")
	(sign_extend:SUPERQI
	    (match_operand:SHORT 1 "nonimmediate_operand" " r,m")))]
  ""
  "@
   #
   l<SHORT:size>\t%0,%1"
  "&& reload_completed
   && REG_P (operands[1])
   && !paradoxical_subreg_p (operands[0])"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 2)))]
{
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = GEN_INT (GET_MODE_BITSIZE (SImode)
			 - GET_MODE_BITSIZE (<SHORT:MODE>mode));
}
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "SI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF     0 "register_operand" "=f")
	(float_extend:DF
	    (match_operand:SF 1 "register_operand" " f")))]
  "TARGET_DOUBLE_FLOAT"
  "fcvt.d.s\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_insn "fix_trunc<ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR      0 "register_operand" "=r")
	(fix:GPR
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.<GPR:ifmt>.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "fixuns_trunc<ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR      0 "register_operand" "=r")
	(unsigned_fix:GPR
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.<GPR:ifmt>u.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "float<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>\t%0,%z1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "floatuns<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(unsigned_float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>u\t%0,%z1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "l<rint_pattern><ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR       0 "register_operand" "=r")
	(unspec:GPR
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    RINT))]
  "TARGET_HARD_FLOAT"
  "fcvt.<GPR:ifmt>.<ANYF:fmt> %0,%1,<rint_rm>"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; Lower-level instructions for loading an address from the GOT.
;; We could use MEMs, but an unspec gives more optimization
;; opportunities.

(define_insn "got_load<mode>"
   [(set (match_operand:P      0 "register_operand" "=r")
	 (unspec:P
	     [(match_operand:P 1 "symbolic_operand" "")]
	     UNSPEC_LOAD_GOT))]
  ""
  "la\t%0,%1"
   [(set_attr "got" "load")
    (set_attr "mode" "<MODE>")])

(define_insn "tls_add_tp_le<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "register_operand" "r")
	     (match_operand:P 2 "register_operand" "r")
	     (match_operand:P 3 "symbolic_operand" "")]
	    UNSPEC_TLS_LE))]
  ""
  "add\t%0,%1,%2,%%tprel_add(%3)"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "got_load_tls_gd<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_GD))]
  ""
  "la.tls.gd\t%0,%1"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "got_load_tls_ie<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_IE))]
  ""
  "la.tls.ie\t%0,%1"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "auipc<mode>"
  [(set (match_operand:P           0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P      1 "symbolic_operand" "")
		  (match_operand:P 2 "const_int_operand")
		  (pc)]
	    UNSPEC_AUIPC))]
  ""
  ".LA%2: auipc\t%0,%h1"
  [(set_attr "type" "auipc")
   (set_attr "cannot_copy" "yes")])

;; Instructions for adding the low 12 bits of an address to a register.
;; Operand 2 is the address: riscv_print_operand works out which relocation
;; should be applied.

(define_insn "*low<mode>"
  [(set (match_operand:P           0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" " r")
		  (match_operand:P 2 "symbolic_operand" "")))]
  ""
  "addi\t%0,%1,%R2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

;; Allow combine to split complex const_int load sequences, using operand 2
;; to store the intermediate results.  See move_operand for details.
(define_split
  [(set (match_operand:GPR 0 "register_operand")
	(match_operand:GPR 1 "splittable_const_int_operand"))
   (clobber (match_operand:GPR 2 "register_operand"))]
  ""
  [(const_int 0)]
{
  riscv_move_integer (operands[2], operands[0], INTVAL (operands[1]),
		      <GPR:MODE>mode, TRUE);
  DONE;
})

;; Likewise, for symbolic operands.
(define_split
  [(set (match_operand:P 0 "register_operand")
	(match_operand:P 1))
   (clobber (match_operand:P 2 "register_operand"))]
  "riscv_split_symbol (operands[2], operands[1], MAX_MACHINE_MODE, NULL, TRUE)"
  [(set (match_dup 0) (match_dup 3))]
{
  riscv_split_symbol (operands[2], operands[1],
		      MAX_MACHINE_MODE, &operands[3], TRUE);
})

;; 64-bit integer moves

(define_expand "movdi"
  [(set (match_operand:DI 0 "")
	(match_operand:DI 1 ""))]
  ""
{
  if (riscv_legitimize_move (DImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,m,  *f,*f,*r,*f,*m")
	(match_operand:DI 1 "move_operand"         " r,i,m,r,*J*r,*m,*f,*f,*f"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fmove,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*f,*r,*f,*m")
	(match_operand:DI 1 "move_operand"         " r,T,m,rJ,*r*J,*m,*f,*f,*f"))]
  "TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fmove,fpstore")
   (set_attr "mode" "DI")])

;; 32-bit Integer moves

(define_expand "mov<mode>"
  [(set (match_operand:MOVE32 0 "")
	(match_operand:MOVE32 1 ""))]
  ""
{
  if (riscv_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*f,*r,*m")
	(match_operand:SI 1 "move_operand"         " r,T,m,rJ,*r*J,*m,*f,*f"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "SI")])

;; 16-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movhi"
  [(set (match_operand:HI 0 "")
	(match_operand:HI 1 ""))]
  ""
{
  if (riscv_legitimize_move (HImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*r")
	(match_operand:HI 1 "move_operand"	   " r,T,m,rJ,*r*J,*f"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,mfc")
   (set_attr "mode" "HI")])

;; HImode constant generation; see riscv_move_integer for details.
;; si+si->hi without truncation is legal because of
;; TARGET_TRULY_NOOP_TRUNCATION.

(define_insn "*add<mode>hi3"
  [(set (match_operand:HI            0 "register_operand" "=r,r")
	(plus:HI (match_operand:HISI 1 "register_operand" " r,r")
		 (match_operand:HISI 2 "arith_operand"    " r,I")))]
  ""
  { return TARGET_64BIT ? "add%i2w\t%0,%1,%2" : "add%i2\t%0,%1,%2"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "HI")])

(define_insn "*xor<mode>hi3"
  [(set (match_operand:HI 0 "register_operand"           "=r,r")
	(xor:HI (match_operand:HISI 1 "register_operand" " r,r")
		(match_operand:HISI 2 "arith_operand"    " r,I")))]
  ""
  "xor%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

;; 8-bit Integer moves

(define_expand "movqi"
  [(set (match_operand:QI 0 "")
	(match_operand:QI 1 ""))]
  ""
{
  if (riscv_legitimize_move (QImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*r")
	(match_operand:QI 1 "move_operand"         " r,I,m,rJ,*r*J,*f"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,mfc")
   (set_attr "mode" "QI")])

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "")
	(match_operand:SF 1 ""))]
  ""
{
  if (riscv_legitimize_move (SFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movsf_hardfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:SF 1 "move_operand"         " f,G,m,f,G,*r,*f,*G*r,*m,*r"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "= r,r,m")
	(match_operand:SF 1 "move_operand"         " Gr,m,r"))]
  "!TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "SF")])

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "")
	(match_operand:DF 1 ""))]
  ""
{
  if (riscv_legitimize_move (DFmode, operands[0], operands[1]))
    DONE;
})

;; In RV32, we lack fmv.x.d and fmv.d.x.  Go through memory instead.
;; (However, we can still use fcvt.d.w to zero a floating-point register.)
(define_insn "*movdf_hardfloat_rv32"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,  *r,*r,*m")
	(match_operand:DF 1 "move_operand"         " f,G,m,f,G,*r*G,*m,*r"))]
  "!TARGET_64BIT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_hardfloat_rv64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:DF 1 "move_operand"         " f,G,m,f,G,*r,*f,*r*G,*m,*r"))]
  "TARGET_64BIT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "= r,r, m")
	(match_operand:DF 1 "move_operand"         " rG,m,rG"))]
  "!TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "DF")])

(define_split
  [(set (match_operand:MOVE64 0 "nonimmediate_operand")
	(match_operand:MOVE64 1 "move_operand"))]
  "reload_completed
   && riscv_split_64bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  riscv_split_doubleword_move (operands[0], operands[1]);
  DONE;
})

(define_expand "cpymemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  ""
{
  if (riscv_expand_block_move (operands[0], operands[1], operands[2]))
    DONE;
  else
    FAIL;
})

;; Expand in-line code to clear the instruction cache between operand[0] and
;; operand[1].
(define_expand "clear_cache"
  [(match_operand 0 "pmode_register_operand")
   (match_operand 1 "pmode_register_operand")]
  ""
{
#ifdef ICACHE_FLUSH_FUNC
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, ICACHE_FLUSH_FUNC),
		     LCT_NORMAL, VOIDmode, operands[0], Pmode,
		     operands[1], Pmode, const0_rtx, Pmode);
#else
  emit_insn (gen_fence_i ());
#endif
  DONE;
})

(define_insn "fence"
  [(unspec_volatile [(const_int 0)] UNSPECV_FENCE)]
  ""
  "%|fence%-")

(define_insn "fence_i"
  [(unspec_volatile [(const_int 0)] UNSPECV_FENCE_I)]
  ""
  "fence.i")

;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

;; Use a QImode shift count, to avoid generating sign or zero extend
;; instructions for shift counts, and to avoid dropping subregs.
;; expand_shift_1 can do this automatically when SHIFT_COUNT_TRUNCATED is
;; defined, but use of that is discouraged.

(define_insn "<optab>si3"
  [(set (match_operand:SI     0 "register_operand" "= r")
	(any_shift:SI
	    (match_operand:SI 1 "register_operand" "  r")
	    (match_operand:QI 2 "arith_operand"    " rI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (SImode) - 1));

  return TARGET_64BIT ? "<insn>%i2w\t%0,%1,%2" : "<insn>%i2\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*<optab>si3_mask"
  [(set (match_operand:SI     0 "register_operand" "= r")
	(any_shift:SI
	    (match_operand:SI 1 "register_operand" "  r")
	    (subreg:QI
	     (and:SI
	      (match_operand:SI 2 "register_operand"  "r")
	      (match_operand 3 "const_int_operand")) 0)))]
  "(INTVAL (operands[3]) & (GET_MODE_BITSIZE (SImode)-1))
   == GET_MODE_BITSIZE (SImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(any_shift:SI (match_dup 1)
		      (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*<optab>si3_mask_1"
  [(set (match_operand:SI     0 "register_operand" "= r")
	(any_shift:SI
	    (match_operand:SI 1 "register_operand" "  r")
	    (subreg:QI
	     (and:DI
	      (match_operand:DI 2 "register_operand"  "r")
	      (match_operand 3 "const_int_operand")) 0)))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) & (GET_MODE_BITSIZE (SImode)-1))
       == GET_MODE_BITSIZE (SImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(any_shift:SI (match_dup 1)
		      (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "<optab>di3"
  [(set (match_operand:DI 0 "register_operand"     "= r")
	(any_shift:DI
	    (match_operand:DI 1 "register_operand" "  r")
	    (match_operand:QI 2 "arith_operand"    " rI")))]
  "TARGET_64BIT"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (DImode) - 1));

  return "<insn>%i2\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")])

(define_insn_and_split "*<optab>di3_mask"
  [(set (match_operand:DI     0 "register_operand" "= r")
	(any_shift:DI
	    (match_operand:DI 1 "register_operand" "  r")
	    (subreg:QI
	     (and:SI
	      (match_operand:SI 2 "register_operand"  "r")
	      (match_operand 3 "const_int_operand")) 0)))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) & (GET_MODE_BITSIZE (DImode)-1))
       == GET_MODE_BITSIZE (DImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(any_shift:DI (match_dup 1)
		      (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")])

(define_insn_and_split "*<optab>di3_mask_1"
  [(set (match_operand:DI     0 "register_operand" "= r")
	(any_shift:DI
	    (match_operand:DI 1 "register_operand" "  r")
	    (subreg:QI
	     (and:DI
	      (match_operand:DI 2 "register_operand"  "r")
	      (match_operand 3 "const_int_operand")) 0)))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) & (GET_MODE_BITSIZE (DImode)-1))
       == GET_MODE_BITSIZE (DImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(any_shift:DI (match_dup 1)
		      (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")])

(define_insn "*<optab>si3_extend"
  [(set (match_operand:DI                   0 "register_operand" "= r")
	(sign_extend:DI
	    (any_shift:SI (match_operand:SI 1 "register_operand" "  r")
			  (match_operand:QI 2 "arith_operand"    " rI"))))]
  "TARGET_64BIT"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "<insn>%i2w\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*<optab>si3_extend_mask"
  [(set (match_operand:DI                   0 "register_operand" "= r")
	(sign_extend:DI
	    (any_shift:SI
	     (match_operand:SI 1 "register_operand" "  r")
	     (subreg:QI
	      (and:SI
	       (match_operand:SI 2 "register_operand" " r")
	       (match_operand 3 "const_int_operand")) 0))))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) & (GET_MODE_BITSIZE (SImode)-1))
       == GET_MODE_BITSIZE (SImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(sign_extend:DI
	 (any_shift:SI (match_dup 1)
		       (match_dup 2))))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*<optab>si3_extend_mask_1"
  [(set (match_operand:DI                   0 "register_operand" "= r")
	(sign_extend:DI
	    (any_shift:SI
	     (match_operand:SI 1 "register_operand" "  r")
	     (subreg:QI
	      (and:DI
	       (match_operand:DI 2 "register_operand" " r")
	       (match_operand 3 "const_int_operand")) 0))))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) & (GET_MODE_BITSIZE (SImode)-1))
       == GET_MODE_BITSIZE (SImode)-1"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(sign_extend:DI
	 (any_shift:SI (match_dup 1)
		       (match_dup 2))))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;; Non-canonical, but can be formed by ree when combine is not successful at
;; producing one of the two canonical patterns below.
(define_insn "*lshrsi3_zero_extend_1"
  [(set (match_operand:DI                   0 "register_operand" "=r")
	(zero_extend:DI
	 (lshiftrt:SI (match_operand:SI     1 "register_operand" " r")
		      (match_operand        2 "const_int_operand"))))]
  "TARGET_64BIT && (INTVAL (operands[2]) & 0x1f) > 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srliw\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;; Canonical form for a zero-extend of a logical right shift.
(define_insn "*lshrsi3_zero_extend_2"
  [(set (match_operand:DI                   0 "register_operand" "=r")
	(zero_extract:DI (match_operand:DI  1 "register_operand" " r")
			 (match_operand     2 "const_int_operand")
			 (match_operand     3 "const_int_operand")))]
  "(TARGET_64BIT && (INTVAL (operands[3]) > 0)
    && (INTVAL (operands[2]) + INTVAL (operands[3]) == 32))"
{
  return "srliw\t%0,%1,%3";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;; Canonical form for a zero-extend of a logical right shift when the
;; shift count is 31.
(define_insn "*lshrsi3_zero_extend_3"
  [(set (match_operand:DI                   0 "register_operand" "=r")
	(lt:DI (match_operand:SI            1 "register_operand" " r")
	       (const_int 0)))]
  "TARGET_64BIT"
{
  return "srliw\t%0,%1,31";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;; Handle AND with 2^N-1 for N from 12 to XLEN.  This can be split into
;; two logical shifts.  Otherwise it requires 3 instructions: lui,
;; xor/addi/srli, and.

;; Generating a temporary for the shift output gives better combiner results;
;; and also fixes a problem where op0 could be a paradoxical reg and shifting
;; by amounts larger than the size of the SUBREG_REG doesn't work.
(define_split
  [(set (match_operand:GPR 0 "register_operand")
	(and:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "p2m1_shift_operand")))
   (clobber (match_operand:GPR 3 "register_operand"))]
  ""
 [(set (match_dup 3)
       (ashift:GPR (match_dup 1) (match_dup 2)))
  (set (match_dup 0)
       (lshiftrt:GPR (match_dup 3) (match_dup 2)))]
{
  /* Op2 is a VOIDmode constant, so get the mode size from op1.  */
  operands[2] = GEN_INT (GET_MODE_BITSIZE (GET_MODE (operands[1]))
			 - exact_log2 (INTVAL (operands[2]) + 1));
})

;; Handle AND with 0xF...F0...0 where there are 32 to 63 zeros.  This can be
;; split into two shifts.  Otherwise it requires 3 instructions: li, sll, and.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(and:DI (match_operand:DI 1 "register_operand")
		(match_operand:DI 2 "high_mask_shift_operand")))
   (clobber (match_operand:DI 3 "register_operand"))]
  "TARGET_64BIT"
  [(set (match_dup 3)
	(lshiftrt:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(ashift:DI (match_dup 3) (match_dup 2)))]
{
  operands[2] = GEN_INT (ctz_hwi (INTVAL (operands[2])));
})

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches

(define_insn "*branch<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:X 2 "register_operand" "r")
			  (match_operand:X 3 "reg_or_0_operand" "rJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "b%C1\t%2,%z3,%0"
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

;; Patterns for implementations that optimize short forward branches.

(define_expand "mov<mode>cc"
  [(set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_operand 1 "comparison_operator")
			  (match_operand:GPR 2 "register_operand")
			  (match_operand:GPR 3 "sfb_alu_operand")))]
  "TARGET_SFB_ALU"
{
  rtx cmp = operands[1];
  /* We only handle word mode integer compares for now.  */
  if (GET_MODE (XEXP (cmp, 0)) != word_mode)
    FAIL;
  riscv_expand_conditional_move (operands[0], operands[2], operands[3],
				 GET_CODE (cmp), XEXP (cmp, 0), XEXP (cmp, 1));
  DONE;
})

(define_insn "*mov<GPR:mode><X:mode>cc"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(if_then_else:GPR
	 (match_operator 5 "order_operator"
		[(match_operand:X 1 "register_operand" "r,r")
		 (match_operand:X 2 "reg_or_0_operand" "rJ,rJ")])
	 (match_operand:GPR 3 "register_operand" "0,0")
	 (match_operand:GPR 4 "sfb_alu_operand" "rJ,IL")))]
  "TARGET_SFB_ALU"
  "@
   b%C5 %1,%z2,1f; mv %0,%z4; 1: # movcc
   b%C5 %1,%z2,1f; li %0,%4; 1: # movcc"
  [(set_attr "length" "8")
   (set_attr "type" "sfb_alu")
   (set_attr "mode" "<GPR:MODE>")])

;; Used to implement built-in functions.
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		      [(match_operand:BR 1 "register_operand")
		       (match_operand:BR 2 "nonmemory_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  riscv_expand_conditional_branch (operands[3], GET_CODE (operands[0]),
				   operands[1], operands[2]);
  DONE;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "fp_branch_comparison"
		       [(match_operand:ANYF 1 "register_operand")
			(match_operand:ANYF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "TARGET_HARD_FLOAT"
{
  riscv_expand_conditional_branch (operands[3], GET_CODE (operands[0]),
				   operands[1], operands[2]);
  DONE;
})

(define_insn_and_split "*branch_on_bit<X:mode>"
  [(set (pc)
	(if_then_else
	    (match_operator 0 "equality_operator"
	        [(zero_extract:X (match_operand:X 2 "register_operand" "r")
				 (const_int 1)
				 (match_operand 3 "branch_on_bit_operand"))
				 (const_int 0)])
	    (label_ref (match_operand 1))
	    (pc)))
   (clobber (match_scratch:X 4 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4)
	(ashift:X (match_dup 2) (match_dup 3)))
   (set (pc)
	(if_then_else
	    (match_op_dup 0 [(match_dup 4) (const_int 0)])
	    (label_ref (match_operand 1))
	    (pc)))]
{
  int shift = GET_MODE_BITSIZE (<MODE>mode) - 1 - INTVAL (operands[3]);
  operands[3] = GEN_INT (shift);

  if (GET_CODE (operands[0]) == EQ)
    operands[0] = gen_rtx_GE (<MODE>mode, operands[4], const0_rtx);
  else
    operands[0] = gen_rtx_LT (<MODE>mode, operands[4], const0_rtx);
})

(define_insn_and_split "*branch_on_bit_range<X:mode>"
  [(set (pc)
	(if_then_else
	    (match_operator 0 "equality_operator"
		[(zero_extract:X (match_operand:X 2 "register_operand" "r")
				 (match_operand 3 "branch_on_bit_operand")
				 (const_int 0))
				 (const_int 0)])
	    (label_ref (match_operand 1))
	    (pc)))
   (clobber (match_scratch:X 4 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4)
	(ashift:X (match_dup 2) (match_dup 3)))
   (set (pc)
	(if_then_else
	    (match_op_dup 0 [(match_dup 4) (const_int 0)])
	    (label_ref (match_operand 1))
	    (pc)))]
{
  operands[3] = GEN_INT (GET_MODE_BITSIZE (<MODE>mode) - INTVAL (operands[3]));
})

;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

;; Destination is always set in SI mode.

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "order_operator"
	    [(match_operand:GPR 2 "register_operand")
	     (match_operand:GPR 3 "nonmemory_operand")]))]
  ""
{
  riscv_expand_int_scc (operands[0], GET_CODE (operands[1]), operands[2],
			operands[3]);
  DONE;
})

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "fp_scc_comparison"
	     [(match_operand:ANYF 2 "register_operand")
	      (match_operand:ANYF 3 "register_operand")]))]
  "TARGET_HARD_FLOAT"
{
  riscv_expand_float_scc (operands[0], GET_CODE (operands[1]), operands[2],
			  operands[3]);
  DONE;
})

(define_insn "*cstore<ANYF:mode><X:mode>4"
   [(set (match_operand:X         0 "register_operand" "=r")
	 (match_operator:X 1 "fp_native_comparison"
	     [(match_operand:ANYF 2 "register_operand" " f")
	      (match_operand:ANYF 3 "register_operand" " f")]))]
  "TARGET_HARD_FLOAT"
  "f%C1.<fmt>\t%0,%2,%3"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "f<quiet_pattern>_quiet<ANYF:mode><X:mode>4"
   [(parallel [(set (match_operand:X      0 "register_operand")
		    (unspec:X
		     [(match_operand:ANYF 1 "register_operand")
		      (match_operand:ANYF 2 "register_operand")]
		     QUIET_COMPARISON))
	       (clobber (match_scratch:X 3))])]
  "TARGET_HARD_FLOAT")

(define_insn "*f<quiet_pattern>_quiet<ANYF:mode><X:mode>4_default"
   [(set (match_operand:X      0 "register_operand" "=r")
	 (unspec:X
	  [(match_operand:ANYF 1 "register_operand" " f")
	   (match_operand:ANYF 2 "register_operand" " f")]
	  QUIET_COMPARISON))
    (clobber (match_scratch:X 3 "=&r"))]
  "TARGET_HARD_FLOAT && ! HONOR_SNANS (<ANYF:MODE>mode)"
  "frflags\t%3\n\tf<quiet_pattern>.<fmt>\t%0,%1,%2\n\tfsflags %3"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length") (const_int 12))])

(define_insn "*f<quiet_pattern>_quiet<ANYF:mode><X:mode>4_snan"
   [(set (match_operand:X      0 "register_operand" "=r")
	 (unspec:X
	  [(match_operand:ANYF 1 "register_operand" " f")
	   (match_operand:ANYF 2 "register_operand" " f")]
	  QUIET_COMPARISON))
    (clobber (match_scratch:X 3 "=&r"))]
  "TARGET_HARD_FLOAT && HONOR_SNANS (<ANYF:MODE>mode)"
  "frflags\t%3\n\tf<quiet_pattern>.<fmt>\t%0,%1,%2\n\tfsflags %3\n\tfeq.<fmt>\tzero,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length") (const_int 16))])

(define_insn "*seq_zero_<X:mode><GPR:mode>"
  [(set (match_operand:GPR       0 "register_operand" "=r")
	(eq:GPR (match_operand:X 1 "register_operand" " r")
		(const_int 0)))]
  ""
  "seqz\t%0,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sne_zero_<X:mode><GPR:mode>"
  [(set (match_operand:GPR       0 "register_operand" "=r")
	(ne:GPR (match_operand:X 1 "register_operand" " r")
		(const_int 0)))]
  ""
  "snez\t%0,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sgt<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR           0 "register_operand" "= r")
	(any_gt:GPR (match_operand:X 1 "register_operand" "  r")
		    (match_operand:X 2 "reg_or_0_operand" " rJ")))]
  ""
  "sgt<u>\t%0,%1,%z2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sge<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR           0 "register_operand" "=r")
	(any_ge:GPR (match_operand:X 1 "register_operand" " r")
		    (const_int 1)))]
  ""
  "slt%i2<u>\t%0,zero,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*slt<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR           0 "register_operand" "= r")
	(any_lt:GPR (match_operand:X 1 "register_operand" "  r")
		    (match_operand:X 2 "arith_operand"    " rI")))]
  ""
  "slt%i2<u>\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sle<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR           0 "register_operand" "=r")
	(any_le:GPR (match_operand:X 1 "register_operand" " r")
		    (match_operand:X 2 "sle_operand" "")))]
  ""
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt%i2<u>\t%0,%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

;;
;;  ....................
;;
;;	UNCONDITIONAL BRANCHES
;;
;;  ....................

;; Unconditional branches.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "j\t%l0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  operands[0] = force_reg (Pmode, operands[0]);
  if (Pmode == SImode)
    emit_jump_insn (gen_indirect_jumpsi (operands[0]));
  else
    emit_jump_insn (gen_indirect_jumpdi (operands[0]));
  DONE;
})

(define_insn "indirect_jump<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "l"))]
  ""
  "jr\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_expand "tablejump"
  [(set (pc) (match_operand 0 "register_operand" ""))
	      (use (label_ref (match_operand 1 "" "")))]
  ""
{
  if (CASE_VECTOR_PC_RELATIVE)
      operands[0] = expand_simple_binop (Pmode, PLUS, operands[0],
					 gen_rtx_LABEL_REF (Pmode, operands[1]),
					 NULL_RTX, 0, OPTAB_DIRECT);

  if (CASE_VECTOR_PC_RELATIVE && Pmode == DImode)
    emit_jump_insn (gen_tablejumpdi (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejumpsi (operands[0], operands[1]));
  DONE;
})

(define_insn "tablejump<mode>"
  [(set (pc) (match_operand:GPR 0 "register_operand" "l"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jr\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;;
;;  ....................
;;
;;	Function prologue/epilogue
;;
;;  ....................
;;

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  riscv_expand_prologue ();
  DONE;
})

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")
   (set_attr "mode" "none")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  riscv_expand_epilogue (NORMAL_RETURN);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  riscv_expand_epilogue (SIBCALL_RETURN);
  DONE;
})

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better.

(define_expand "return"
  [(simple_return)]
  "riscv_can_use_return_insn ()"
  "")

(define_insn "simple_return"
  [(simple_return)]
  ""
{
  return riscv_output_return ();
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "simple_return_internal"
  [(simple_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "jr\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  if (GET_MODE (operands[0]) != word_mode)
    operands[0] = convert_to_mode (word_mode, operands[0], 0);
  if (TARGET_64BIT)
    emit_insn (gen_eh_set_lr_di (operands[0]));
  else
    emit_insn (gen_eh_set_lr_si (operands[0]));

  emit_jump_insn (gen_eh_return_internal ());
  emit_barrier ();
  DONE;
})

;; Clobber the return address on the stack.  We can't expand this
;; until we know where it will be put in the stack frame.

(define_insn "eh_set_lr_si"
  [(unspec [(match_operand:SI 0 "register_operand" "r")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  "! TARGET_64BIT"
  "#")

(define_insn "eh_set_lr_di"
  [(unspec [(match_operand:DI 0 "register_operand" "r")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:DI 1 "=&r"))]
  "TARGET_64BIT"
  "#")

(define_split
  [(unspec [(match_operand 0 "register_operand")] UNSPEC_EH_RETURN)
   (clobber (match_scratch 1))]
  "reload_completed"
  [(const_int 0)]
{
  riscv_set_return_address (operands[0], operands[1]);
  DONE;
})

(define_insn_and_split "eh_return_internal"
  [(eh_return)]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
  "riscv_expand_epilogue (EXCEPTION_RETURN); DONE;")

;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[0], 0));
  emit_call_insn (gen_sibcall_internal (target, operands[1]));
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,S,U"))
	 (match_operand 1 "" ""))]
  "SIBLING_CALL_P (insn)"
  "@
   jr\t%0
   tail\t%0
   tail\t%0@plt"
  [(set_attr "type" "call")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[1], 0));
  emit_call_insn (gen_sibcall_value_internal (operands[0], target, operands[2]));
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "j,S,U"))
	      (match_operand 2 "" "")))]
  "SIBLING_CALL_P (insn)"
  "@
   jr\t%1
   tail\t%1
   tail\t%1@plt"
  [(set_attr "type" "call")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[0], 0));
  emit_call_insn (gen_call_internal (target, operands[1]));
  DONE;
})

(define_insn "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "l,S,U"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  "@
   jalr\t%0
   call\t%0
   call\t%0@plt"
  [(set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[1], 0));
  emit_call_insn (gen_call_value_internal (operands[0], target, operands[2]));
  DONE;
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "l,S,U"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  "@
   jalr\t%1
   call\t%1
   call\t%1@plt"
  [(set_attr "type" "call")])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
		    (const_int 0))
	      (match_operand 1 "")
	      (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      riscv_emit_move (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "ebreak")

(define_insn "gpr_save"
  [(unspec_volatile [(match_operand 0 "const_int_operand")] UNSPECV_GPR_SAVE)
   (clobber (reg:SI T0_REGNUM))
   (clobber (reg:SI T1_REGNUM))]
  ""
  { return riscv_output_gpr_save (INTVAL (operands[0])); })

(define_insn "gpr_restore"
  [(unspec_volatile [(match_operand 0 "const_int_operand")] UNSPECV_GPR_RESTORE)]
  ""
  "tail\t__riscv_restore_%0")

(define_insn "gpr_restore_return"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))
   (const_int 0)]
  ""
  "")

(define_insn "riscv_frflags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile [(const_int 0)] UNSPECV_FRFLAGS))]
  "TARGET_HARD_FLOAT"
  "frflags\t%0")

(define_insn "riscv_fsflags"
  [(unspec_volatile [(match_operand:SI 0 "csr_operand" "rK")] UNSPECV_FSFLAGS)]
  "TARGET_HARD_FLOAT"
  "fsflags\t%0")

(define_insn "riscv_mret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_MRET)]
  ""
  "mret")

(define_insn "riscv_sret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_SRET)]
  ""
  "sret")

(define_insn "riscv_uret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_URET)]
  ""
  "uret")

(define_insn "stack_tie<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:X 0 "register_operand" "r")
		     (match_operand:X 1 "register_operand" "r")]
		    UNSPEC_TIE))]
  ""
  ""
  [(set_attr "length" "0")]
)

;; This fixes a failure with gcc.c-torture/execute/pr64242.c at -O2 for a
;; 32-bit target when using -mtune=sifive-7-series.  The first sched pass
;; runs before register elimination, and we have a non-obvious dependency
;; between a use of the soft fp and a set of the hard fp.  We fix this by
;; emitting a clobber using the hard fp between the two insns.
(define_expand "restore_stack_nonlocal"
  [(match_operand 0 "register_operand")
   (match_operand 1 "memory_operand")]
  ""
{
  emit_move_insn (operands[0], operands[1]);
  /* Prevent the following hard fp restore from being moved before the move
     insn above which uses a copy of the soft fp reg.  */
  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));
  DONE;
})

(include "sync.md")
(include "peephole.md")
(include "pic.md")
(include "generic.md")
(include "sifive-7.md")
