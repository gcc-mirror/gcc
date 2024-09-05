;; Machine Description for LoongArch for GNU compiler.
;; Copyright (C) 2021-2025 Free Software Foundation, Inc.
;; Contributed by Loongson Ltd.
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
  ;; Floating-point moves.
  UNSPEC_LOAD_LOW
  UNSPEC_LOAD_HIGH
  UNSPEC_STORE_WORD
  UNSPEC_MOVGR2FRH
  UNSPEC_MOVFRH2GR

  ;; Floating point unspecs.
  UNSPEC_FRINT
  UNSPEC_FCLASS
  UNSPEC_FMAX
  UNSPEC_FMIN
  UNSPEC_FTINT
  UNSPEC_FTINTRM
  UNSPEC_FTINTRP
  UNSPEC_FSCALEB
  UNSPEC_FLOGB

  ;; Override return address for exception handling.
  UNSPEC_EH_RETURN

  ;; Bit operation
  UNSPEC_BITREV_4B
  UNSPEC_BITREV_8B

  ;; TLS
  UNSPEC_TLS
  UNSPEC_TLS_DESC
  UNSPEC_TLS_DESC_OFF64

  ;; Stack tie
  UNSPEC_TIE

  ;; RSQRT
  UNSPEC_RSQRT
  UNSPEC_RSQRTE

  ;; RECIP
  UNSPEC_RECIPE

  ;; CRC
  UNSPEC_CRC
  UNSPEC_CRCC

  UNSPEC_LOAD_FROM_GOT
  UNSPEC_PCALAU12I
  UNSPEC_PCALAU12I_GR
  UNSPEC_ADD_TLS_LE_RELAX
  UNSPEC_ORI_L_LO12
  UNSPEC_LUI_L_HI20
  UNSPEC_LUI_H_LO20
  UNSPEC_LUI_H_HI12
  UNSPEC_TLS_LOW

  ;; Fake div.w[u] mod.w[u]
  UNSPEC_FAKE_ANY_DIV

  UNSPEC_SIBCALL_VALUE_MULTIPLE_INTERNAL_1
  UNSPEC_CALL_VALUE_MULTIPLE_INTERNAL_1

  UNSPEC_LOAD_SYMBOL_OFFSET64
  UNSPEC_LA_PCREL_64_PART1
  UNSPEC_LA_PCREL_64_PART2
])

(define_c_enum "unspecv" [
  ;; Blockage and synchronisation.
  UNSPECV_BLOCKAGE
  UNSPECV_DBAR
  UNSPECV_IBAR

  ;; Privileged instructions
  UNSPECV_CSRRD
  UNSPECV_CSRWR
  UNSPECV_CSRXCHG
  UNSPECV_IOCSRRD
  UNSPECV_IOCSRWR
  UNSPECV_CACOP
  UNSPECV_LDDIR
  UNSPECV_LDPTE
  UNSPECV_ERTN

  ;; Stack checking.
  UNSPECV_PROBE_STACK_RANGE

  ;; Floating-point environment.
  UNSPECV_MOVFCSR2GR
  UNSPECV_MOVGR2FCSR

  ;; Others
  UNSPECV_CPUCFG
  UNSPECV_ASRTLE_D
  UNSPECV_ASRTGT_D
  UNSPECV_SYSCALL
  UNSPECV_BREAK
])

(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (TP_REGNUM			2)
   (T0_REGNUM			12)
   (T1_REGNUM			13)
   (S0_REGNUM			23)

   (FCC0_REGNUM			64)
   (FCC1_REGNUM			65)
   (FCC2_REGNUM			66)
   (FCC3_REGNUM			67)
   (FCC4_REGNUM			68)
   (FCC5_REGNUM			69)
   (FCC6_REGNUM			70)
   (FCC7_REGNUM			71)

   ;; Return path styles
   (NORMAL_RETURN		0)
   (SIBCALL_RETURN		1)
   (EXCEPTION_RETURN		2)

   ;; PIC long branch sequences are never longer than 100 bytes.
   (MAX_PIC_BRANCH_LENGTH	100)
])

(include "predicates.md")
(include "constraints.md")

;; ....................
;;
;;	Attributes
;;
;; ....................

(define_attr "enabled" "no,yes" (const_string "yes"))

(define_attr "got" "unset,load"
  (const_string "unset"))

;; For jirl instructions, this attribute is DIRECT when the target address
;; is symbolic and INDIRECT when it is a register.
(define_attr "jirl" "unset,direct,indirect"
  (const_string "unset"))


;; Classification of moves, extensions and truncations.  Most values
;; are as for "type" (see below) but there are also the following
;; move-specific values:
;;
;; sll0		"slli.w DEST,SRC,0", which on 64-bit targets is guaranteed
;;		to produce a sign-extended DEST, even if SRC is not
;;		properly sign-extended
;; pick_ins	BSTRPICK.W, BSTRPICK.D, BSTRINS.W or BSTRINS.D instruction
;; andi		a single ANDI instruction
;; shift_shift	a shift left followed by a shift right
;;
;; This attribute is used to determine the instruction's length and
;; scheduling type.  For doubleword moves, the attribute always describes
;; the split instructions; in some cases, it is more appropriate for the
;; scheduling type to be "multi" instead.
(define_attr "move_type"
  "unknown,load,fpload,store,fpstore,mgtf,mftg,imul,move,fmove,
   const,signext,pick_ins,logical,arith,sll0,andi,shift_shift"
  (const_string "unknown"))

(define_attr "alu_type" "unknown,add,sub,not,nor,and,or,xor,simd_add"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF,FCC,
  V2DI,V4SI,V8HI,V16QI,V2DF,V4SF,V4DI,V8SI,V16HI,V32QI,V4DF,V8SF"
  (const_string "unknown"))

;; True if the main data type is twice the size of a word.
(define_attr "dword_mode" "no,yes"
  (cond [(and (eq_attr "mode" "DI,DF")
	      (not (match_test "TARGET_64BIT")))
	 (const_string "yes")

	 (and (eq_attr "mode" "TI,TF")
	      (match_test "TARGET_64BIT"))
	 (const_string "yes")]
	(const_string "no")))

;; True if the main data type is four times of the size of a word.
(define_attr "qword_mode" "no,yes"
  (cond [(and (eq_attr "mode" "TI,TF")
	      (not (match_test "TARGET_64BIT")))
	 (const_string "yes")]
	(const_string "no")))

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; fpload	floating point load
;; fpidxload    floating point indexed load
;; store	store instruction(s)
;; fpstore	floating point store
;; fpidxstore	floating point indexed store
;; prefetch	memory prefetch (register + offset)
;; prefetchx	memory indexed prefetch (register + register)
;; condmove	conditional moves
;; mgtf		move general-purpose register to floating point register
;; mftg		move floating point register to general-purpose register
;; const	load constant
;; arith	integer arithmetic instructions
;; logical      integer logical instructions
;; shift	integer shift instructions
;; slt		set less than instructions
;; signext      sign extend instructions
;; clz		the clz and clo instructions
;; trap		trap if instructions
;; imul		integer multiply
;; idiv		integer divide
;; move		integer move
;; fmove	floating point register move
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; frdiv	floating point reciprocal divide
;; frecipe      floating point approximate reciprocal
;; fabs		floating point absolute value
;; flogb	floating point exponent extract
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcopysign	floating point copysign
;; fcvt		floating point convert
;; fscaleb	floating point scale
;; fsqrt	floating point square root
;; frsqrt       floating point reciprocal square root
;; frsqrte      floating point approximate reciprocal square root
;; multi	multiword sequence (or user asm statements)
;; atomic	atomic memory update instruction
;; syncloop	memory atomic operation implemented as a sync loop
;; nop		no operation
;; ghost	an instruction that produces no real code
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,fpidxload,store,fpstore,fpidxstore,
   prefetch,prefetchx,condmove,mgtf,mftg,const,arith,logical,
   shift,slt,signext,clz,trap,imul,idiv,move,
   fmove,fadd,fmul,fmadd,fdiv,frdiv,frecipe,fabs,flogb,fneg,fcmp,fcopysign,fcvt,
   fscaleb,fsqrt,frsqrt,frsqrte,accext,accmod,multi,atomic,syncloop,nop,ghost,
   simd_div,simd_fclass,simd_flog2,simd_fadd,simd_fcvt,simd_fmul,simd_fmadd,
   simd_fdiv,simd_bitins,simd_bitmov,simd_insert,simd_sld,simd_mul,simd_fcmp,
   simd_fexp2,simd_int_arith,simd_bit,simd_shift,simd_splat,simd_fill,
   simd_permute,simd_shf,simd_sat,simd_pcnt,simd_copy,simd_branch,simd_clsx,
   simd_fminmax,simd_logic,simd_move,simd_load,simd_store"
  (cond [(eq_attr "jirl" "!unset") (const_string "call")
	 (eq_attr "got" "load") (const_string "load")

	 (eq_attr "alu_type" "add,sub") (const_string "arith")

	 (eq_attr "alu_type" "not,nor,and,or,xor") (const_string "logical")

	 ;; If a doubleword move uses these expensive instructions,
	 ;; it is usually better to schedule them in the same way
	 ;; as the singleword form, rather than as "multi".
	 (eq_attr "move_type" "load") (const_string "load")
	 (eq_attr "move_type" "fpload") (const_string "fpload")
	 (eq_attr "move_type" "store") (const_string "store")
	 (eq_attr "move_type" "fpstore") (const_string "fpstore")
	 (eq_attr "move_type" "mgtf") (const_string "mgtf")
	 (eq_attr "move_type" "mftg") (const_string "mftg")

	 ;; These types of move are always single insns.
	 (eq_attr "move_type" "imul") (const_string "imul")
	 (eq_attr "move_type" "fmove") (const_string "fmove")
	 (eq_attr "move_type" "signext") (const_string "signext")
	 (eq_attr "move_type" "pick_ins") (const_string "arith")
	 (eq_attr "move_type" "arith") (const_string "arith")
	 (eq_attr "move_type" "logical") (const_string "logical")
	 (eq_attr "move_type" "sll0") (const_string "shift")
	 (eq_attr "move_type" "andi") (const_string "logical")

	 ;; These types of move are always split.
	 (eq_attr "move_type" "shift_shift")
	   (const_string "multi")

	 ;; These types of move are split for quadword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "qword_mode" "yes"))
	   (const_string "multi")

	 ;; These types of move are split for doubleword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "dword_mode" "yes"))
	   (const_string "multi")
	 (eq_attr "move_type" "move") (const_string "move")
	 (eq_attr "move_type" "const") (const_string "const")]
	(const_string "unknown")))

;; Mode for conversion types (fcvt)
;; I2S	integer to float single (SI/DI to SF)
;; I2D	integer to float double (SI/DI to DF)
;; S2I	float to integer (SF to SI/DI)
;; D2I	float to integer (DF to SI/DI)
;; D2S	double to float single
;; S2D	float single to double

(define_attr "cnv_mode" "unknown,I2S,I2D,S2I,D2I,D2S,S2D"
  (const_string "unknown"))

;; The number of individual instructions that a non-branch pattern generates
(define_attr "insn_count" ""
  (cond [;; "Ghost" instructions occupy no space.
	 (eq_attr "type" "ghost")
	 (const_int 0)

	 ;; Check for doubleword moves that are decomposed into two
	 ;; instructions.
	 (and (eq_attr "move_type" "mgtf,mftg,move")
	      (eq_attr "dword_mode" "yes"))
	 (const_int 2)

	 ;; Check for quadword moves that are decomposed into four
	 ;; instructions.
	 (and (eq_attr "move_type" "mgtf,mftg,move")
	      (eq_attr "qword_mode" "yes"))
	 (const_int 4)

	 ;; Constants, loads and stores are handled by external routines.
	 (and (eq_attr "move_type" "const")
	      (eq_attr "dword_mode" "yes"))
	 (symbol_ref "loongarch_split_const_insns (operands[1])")
	 (eq_attr "move_type" "const")
	 (symbol_ref "loongarch_const_insns (operands[1])")
	 (eq_attr "move_type" "load,fpload")
	 (symbol_ref "loongarch_load_store_insns (operands[1], insn)")
	 (eq_attr "move_type" "store,fpstore")
	 (symbol_ref "loongarch_load_store_insns (operands[0], insn)")

	 (eq_attr "type" "idiv")
	 (symbol_ref "loongarch_idiv_insns (GET_MODE (PATTERN (insn)))")]
(const_int 1)))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [
	  ;; Branch futher than +/- 128 KiB require two instructions.
	  (eq_attr "type" "branch")
	  (if_then_else (and (le (minus (match_dup 0) (pc)) (const_int 131064))
			     (le (minus (pc) (match_dup 0)) (const_int 131068)))
	  (const_int 4)
	  (const_int 8))]
    (symbol_ref "get_attr_insn_count (insn) * 4")))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

;; This mode iterator allows 32-bit and 64-bit GPR patterns to be generated
;; from the same template.
(define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

;; A copy of GPR that can be used when a pattern has two independent
;; modes.
(define_mode_iterator GPR2 [SI (DI "TARGET_64BIT")])

;; This mode iterator allows 16-bit and 32-bit GPR patterns and 32-bit 64-bit
;; FPR patterns to be generated from the same template.
(define_mode_iterator JOIN_MODE [HI
				 SI
				 (SF "TARGET_HARD_FLOAT")
				 (DF "TARGET_DOUBLE_FLOAT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; Likewise, but for GRLEN-sized quantities.
(define_mode_iterator X [(SI "!TARGET_64BIT") (DI "TARGET_64BIT")])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64 [DI DF])

;; Iterator for sub-32-bit integer modes.
(define_mode_iterator SHORT [QI HI])

;; Likewise the 64-bit truncate-and-shift patterns.
(define_mode_iterator SUBDI [QI HI SI])

;; Iterator for scalar fixed point modes.
(define_mode_iterator QHWD [QI HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware-supported floating-point modes.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT")
			    (DF "TARGET_DOUBLE_FLOAT")])

;; Iterator for fixed-point modes which can be hold by a hardware
;; floating-point register.
(define_mode_iterator ANYFI [(SI "TARGET_HARD_FLOAT")
			     (DI "TARGET_DOUBLE_FLOAT")])

;; A mode for anything with 32 bits or more, and able to be loaded with
;; the same addressing mode as ld.w.
(define_mode_iterator LD_AT_LEAST_32_BIT [GPR ANYF])

;; A mode for anything able to be stored with the same addressing mode as
;; st.w.
(define_mode_iterator ST_ANY [QHWD ANYF])

;; A mode for anything legal as a input of a div or mod instruction.
(define_mode_iterator DIV [(DI "TARGET_64BIT")
			   (SI "!TARGET_64BIT || ISA_HAS_DIV32")])

;; In GPR templates, a string like "mul.<d>" will expand to "mul.w" in the
;; 32-bit version and "mul.d" in the 64-bit version.
(define_mode_attr d [(SI "w") (DI "d")])

;; This attribute gives the length suffix for a load or store instruction.
;; The same suffixes work for zero and sign extensions.
(define_mode_attr size [(QI "b") (HI "h") (SI "w") (DI "d")])
(define_mode_attr SIZE [(QI "B") (HI "H") (SI "W") (DI "D")])

;; This attribute gives the mode mask of a SHORT.
(define_mode_attr mask [(QI "0x00ff") (HI "0xffff")])

;; This attribute gives the size (bits) of a SHORT.
(define_mode_attr 7_or_15 [(QI "7") (HI "15")])

;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d")])
(define_mode_attr ifmt [(SI "w") (DI "l")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode or vector mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF") (V2SF "SF") (V4SF "SF")
			    (V16QI "QI") (V8HI "HI") (V4SI "SI") (V2DI "DI")
			    (V2DF "DF")(V8SF "SF")(V32QI "QI")(V16HI "HI")(V8SI "SI")(V4DI "DI")(V4DF "DF")])

;; As above, but in lower case.
(define_mode_attr unitmode [(SF "sf") (DF "df") (V2SF "sf") (V4SF "sf")
			    (V16QI "qi") (V8QI "qi") (V8HI "hi") (V4HI "hi")
			    (V4SI "si") (V2SI "si") (V2DI "di") (V2DF "df")
			    (V8SI "si") (V4DI "di") (V32QI "qi") (V16HI "hi")
			    (V8SF "sf") (V4DF "df")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (V2SF "SI")
			    (V2SI "SI") (V4HI "SI") (V8QI "SI")
			    (TF "DI")])

;; This attribute gives the integer mode that has the same size of a
;; floating-point mode.
(define_mode_attr IMODE [(SF "SI") (DF "DI")])

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
(define_code_iterator neg_bitwise [and ior])

;; This code iterator allows unsigned and signed division to be generated
;; from the same template.
(define_code_iterator any_div [div udiv mod umod])

;; This code iterator allows addition and subtraction to be generated
;; from the same template.
(define_code_iterator addsub [plus minus])

;; This code iterator allows addition and multiplication to be generated
;; from the same template.
(define_code_iterator addmul [plus mult])

;; This code iterator allows addition subtraction and multiplication to be
;; generated from the same template
(define_code_iterator addsubmul [plus minus mult])

;; This code iterator allows all native floating-point comparisons to be
;; generated from the same template.
(define_code_iterator fcond [unordered uneq unlt unle eq lt le
			     ordered ltgt ne ge gt unge ungt])

;; Equality operators.
(define_code_iterator equality_op [eq ne])

;; These code iterators allow the signed and unsigned scc operations to use
;; the same template.
(define_code_iterator any_gt [gt gtu])
(define_code_iterator any_lt [lt ltu])
(define_code_iterator any_le [le leu])

(define_code_iterator any_return [return simple_return])

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")
		     (div "") (udiv "u")
		     (mod "") (umod "u")
		     (gt "") (gtu "u")
		     (ge "") (geu "u")
		     (lt "") (ltu "u")
		     (le "") (leu "u")
		     (smax "") (umax "u")])

;; <U> is like <u> except uppercase.
(define_code_attr U [(sign_extend "") (zero_extend "U")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")
                      (smax "s") (umax "u")])

(define_code_attr u_bool [(sign_extend "false") (zero_extend "true")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (rotatert "rotr")
			 (ior "ior")
			 (xor "xor")
			 (and "and")
			 (plus "add")
			 (minus "sub")
			 (mult "mul")
			 (div "div")
			 (udiv "udiv")
			 (mod "mod")
			 (umod "umod")
			 (return "return")
			 (simple_return "simple_return")])

;; <insn> expands to the name of the insn that implements a particular code.
(define_code_attr insn [(ashift "sll")
			(ashiftrt "sra")
			(lshiftrt "srl")
			(rotatert "rotr")
			(ior "or")
			(xor "xor")
			(and "and")
			(plus "addu")
			(minus "subu")
			(div "div")
			(udiv "div")
			(mod "mod")
			(umod "mod")])

;; <fcond> is the fcmp.cond.fmt condition associated with a particular code.
(define_code_attr fcond [(unordered "cun")
			 (uneq "cueq")
			 (unlt "cult")
			 (unle "cule")
			 (eq "ceq")
			 (lt "slt")
			 (le "sle")
			 (ordered "cor")
			 (ltgt "sne")
			 (ne "cune")
			 (ge "sge")
			 (gt "sgt")
			 (unge "cuge")
			 (ungt "cugt")])

;; The sel mnemonic to use depending on the condition test.
(define_code_attr sel [(eq "masknez") (ne "maskeqz")])
(define_code_attr selinv [(eq "maskeqz") (ne "masknez")])

;; Iterator and attributes for floating-point to fixed-point conversion
;; instructions.
(define_int_iterator LRINT [UNSPEC_FTINT UNSPEC_FTINTRM UNSPEC_FTINTRP])
(define_int_attr lrint_pattern [(UNSPEC_FTINT "lrint")
				(UNSPEC_FTINTRM "lfloor")
				(UNSPEC_FTINTRP "lceil")])
(define_int_attr lrint_submenmonic [(UNSPEC_FTINT "")
				    (UNSPEC_FTINTRM "rm")
				    (UNSPEC_FTINTRP "rp")])

;; Iterator and attributes for bytepick.d
(define_int_iterator bytepick_w_ashift_amount [8 16 24])
(define_int_attr bytepick_w_lshiftrt_amount [(8 "24")
					     (16 "16")
					     (24 "8")])
(define_int_iterator bytepick_d_ashift_amount [8 16 24 32 40 48 56])
(define_int_attr bytepick_d_lshiftrt_amount [(8 "56")
					     (16 "48")
					     (24 "40")
					     (32 "32")
					     (40 "24")
					     (48 "16")
					     (56 "8")])
(define_int_attr bytepick_imm [(8 "1")
				 (16 "2")
				 (24 "3")
				 (32 "4")
				 (40 "5")
				 (48 "6")
				 (56 "7")])

;; Expand some 32-bit operations to si3_extend operations if TARGET_64BIT
;; so the redundant sign extension can be removed if the output is used as
;; an input of a bitwise operation.  Note plus, rotl, and div are handled
;; separately.
(define_code_iterator shift_w [any_shift rotatert])
(define_code_iterator arith_w [minus mult])

(define_expand "<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(shift_w:GPR (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_<optab>si3_extend (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_expand "<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(arith_w:GPR (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:GPR 2 "register_operand" "r")))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_<optab>si3_extend (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

;;
;;  ....................
;;
;;	CONDITIONAL TRAPS
;;
;;  ....................
;;

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
{
  return "break\t0";
}
  [(set_attr "type" "trap")])



;;
;;  ....................
;;
;;	ADDITION
;;
;;  ....................
;;

(define_insn "add<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn_and_split "*addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r,r,r,r")
		  (match_operand:SI 2 "plus_si_operand"
				       "r,I,La,Lb,Le")))]
  ""
  "@
   add.w\t%0,%1,%2
   addi.w\t%0,%1,%2
   #
   * operands[2] = GEN_INT (INTVAL (operands[2]) / 65536); \
     return \"addu16i.d\t%0,%1,%2\";
   #"
  "CONST_INT_P (operands[2]) && !IMM12_INT (operands[2]) \
   && !ADDU16I_OPERAND (INTVAL (operands[2]))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 4)))]
  {
    loongarch_split_plus_constant (&operands[2], SImode);
  }
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")
   (set_attr "insn_count" "1,1,2,1,2")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r,r,r,r")
		 (match_operand:SI 2 "plus_si_operand"  "r,I,La,Le,Lb")))]
  "TARGET_64BIT"
{
  if (CONST_INT_P (operands[2]) && !IMM12_INT (operands[2])
      && ADDU16I_OPERAND (INTVAL (operands[2])))
    {
      rtx t1 = gen_reg_rtx (DImode);
      rtx t2 = gen_reg_rtx (DImode);
      rtx t3 = gen_reg_rtx (DImode);
      emit_insn (gen_extend_insn (t1, operands[1], DImode, SImode, 0));
      t2 = operands[2];
      emit_insn (gen_adddi3 (t3, t1, t2));
      t3 = gen_lowpart (SImode, t3);
      emit_move_insn (operands[0], t3);
      DONE;
    }
  else
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_addsi3_extended (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn_and_split "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r,r,r")
	(plus:DI (match_operand:DI 1 "register_operand" "r,r,r,r,r,r")
		  (match_operand:DI 2 "plus_di_operand"
				       "r,I,La,Lb,Lc,Ld")))]
  "TARGET_64BIT"
  "@
   add.d\t%0,%1,%2
   addi.d\t%0,%1,%2
   #
   * operands[2] = GEN_INT (INTVAL (operands[2]) / 65536); \
     return \"addu16i.d\t%0,%1,%2\";
   #
   #"
  "&& CONST_INT_P (operands[2]) && !IMM12_INT (operands[2]) \
   && !ADDU16I_OPERAND (INTVAL (operands[2]))"
  [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 4)))]
  {
    loongarch_split_plus_constant (&operands[2], DImode);
  }
  [(set_attr "alu_type" "add")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "1,1,2,1,2,2")])

(define_insn_and_split "addsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" "r,r,r,r")
		      (match_operand:SI 2 "plus_si_extend_operand"
					  "r,I,La,Le"))))]
  "TARGET_64BIT"
  "@
   add.w\t%0,%1,%2
   addi.w\t%0,%1,%2
   #
   #"
  "CONST_INT_P (operands[2]) && !IMM12_INT (operands[2])"
  [(set (subreg:SI (match_dup 0) 0) (plus:SI (match_dup 1) (match_dup 3)))
   (set (match_dup 0)
	(sign_extend:DI (plus:SI (subreg:SI (match_dup 0) 0)
				 (match_dup 4))))]
  {
    loongarch_split_plus_constant (&operands[2], SImode);
  }
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")
   (set_attr "insn_count" "1,1,2,2")])


;;
;;  ....................
;;
;;	SUBTRACTION
;;
;;  ....................
;;

(define_insn "sub<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		    (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fsub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*sub<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(minus:GPR (match_operand:GPR 1 "register_operand" "r")
		   (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "sub.<d>\t%0,%z1,%2"
  [(set_attr "alu_type" "sub")
   (set_attr "mode" "<MODE>")])


(define_insn "subsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
		      (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "sub.w\t%0,%z1,%2"
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
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<MODE>")])

(define_insn "*mul<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(mult:GPR (match_operand:GPR 1 "register_operand" "r")
		  (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "mul.<d>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")])

(define_insn "mulsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	    (mult:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "mul.w\t%0,%1,%2"
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
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		 (any_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT"
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_<su>muldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "<su>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (any_extend:TI
		       (match_operand:DI 1 "register_operand" " r"))
		     (any_extend:TI
		       (match_operand:DI 2 "register_operand" " r")))
	    (const_int 64))))]
  "TARGET_64BIT"
  "mulh.d<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI
		   (match_operand:SI 1 "register_operand"))
		 (any_extend:DI
		   (match_operand:SI 2 "register_operand"))))]
  ""
{
  if (!TARGET_64BIT)
  {
    rtx temp = gen_reg_rtx (SImode);
    emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
    emit_insn (gen_<su>mulsi3_highpart (loongarch_subword (operands[0], true),
				       operands[1], operands[2]));
    emit_insn (gen_movsi (loongarch_subword (operands[0], false), temp));
    DONE;
  }
})

(define_insn "<u>mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "mulw.d.w<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_insn "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (any_extend:DI
		       (match_operand:SI 1 "register_operand" " r"))
		     (any_extend:DI
		       (match_operand:SI 2 "register_operand" " r")))
	    (const_int 32))))]
  ""
  "mulh.w<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;; Under the LoongArch architecture, the mulh.w[u] instruction performs
;; sign extension by default, so the sign extension instruction can be
;; eliminated.
(define_peephole
  [(set (match_operand:SI 0 "register_operand")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (any_extend:DI
		       (match_operand:SI 1 "register_operand"))
		     (any_extend:DI
		       (match_operand:SI 2 "register_operand")))
	    (const_int 32))))
   (set (match_operand:DI 3 "register_operand")
	(sign_extend:DI (match_dup 0)))]
   "TARGET_64BIT && REGNO (operands[0]) == REGNO (operands[3])"
   "mulh.w<u>\t%0,%1,%2")

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

;; Float division and modulus.
(define_expand "div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand")
    (div:ANYF (match_operand:ANYF 1 "reg_or_1_operand")
	      (match_operand:ANYF 2 "register_operand")))]
  ""
{
  if (<MODE>mode == SFmode
    && TARGET_RECIP_DIV
    && optimize_insn_for_speed_p ()
    && flag_finite_math_only && !flag_trapping_math
    && flag_unsafe_math_optimizations)
  {
    loongarch_emit_swdivsf (operands[0], operands[1],
	operands[2], SFmode);
    DONE;
  }
})

(define_insn "*div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fdiv.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fdiv")
   (set_attr "mode" "<UNITMODE>")])

;; In 3A5000, the reciprocal operation is the same as the division operation.

(define_insn "*recip<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "frecip.<fmt>\t%0,%2"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "<UNITMODE>")])

;; Approximate Reciprocal Instructions.

(define_insn "loongarch_frecipe_<fmt>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
    (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
	     UNSPEC_RECIPE))]
  "ISA_HAS_FRECIPE"
  "frecipe.<fmt>\t%0,%1"
  [(set_attr "type" "frecipe")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

;; Integer division and modulus.
(define_expand "<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(any_div:GPR (match_operand:GPR 1 "register_operand")
		     (match_operand:GPR 2 "register_operand")))]
  ""
{
 if (GET_MODE (operands[0]) == SImode && TARGET_64BIT)
  {
    if (ISA_HAS_DIV32)
      {
        rtx t = gen_reg_rtx (DImode);
        emit_insn (gen_<optab>si3_extended (t, operands[1], operands[2]));
        t = gen_lowpart (SImode, t);
        SUBREG_PROMOTED_VAR_P (t) = 1;
        SUBREG_PROMOTED_SET (t, SRP_SIGNED);
        emit_move_insn (operands[0], t);
        DONE;
      }

    rtx reg1 = gen_reg_rtx (DImode);
    rtx reg2 = gen_reg_rtx (DImode);
    rtx rd = gen_reg_rtx (DImode);

    operands[1] = gen_rtx_SIGN_EXTEND (word_mode, operands[1]);
    operands[2] = gen_rtx_SIGN_EXTEND (word_mode, operands[2]);

    emit_insn (gen_rtx_SET (reg1, operands[1]));
    emit_insn (gen_rtx_SET (reg2, operands[2]));

    emit_insn (gen_<optab>di3_fake (rd, reg1, reg2));
    emit_insn (gen_rtx_SET (operands[0],
			    simplify_gen_subreg (SImode, rd, DImode, 0)));
    DONE;
  }
})

(define_insn "*<optab><mode>3"
  [(set (match_operand:DIV 0 "register_operand" "=r,&r,&r")
	(any_div:DIV (match_operand:DIV 1 "register_operand" "r,r,0")
		     (match_operand:DIV 2 "register_operand" "r,r,r")))]
  ""
{
  return loongarch_output_division ("<insn>.<d><u>\t%0,%1,%2", operands);
}
  [(set_attr "type" "idiv")
   (set_attr "mode" "<MODE>")
   (set (attr "enabled")
      (if_then_else
	(match_test "!!which_alternative == loongarch_check_zero_div_p()")
	(const_string "yes")
	(const_string "no")))])

(define_insn "<optab>si3_extended"
  [(set (match_operand:DI 0 "register_operand" "=r,&r,&r")
	(sign_extend:DI
	  (any_div:SI (match_operand:SI 1 "register_operand" "r,r,0")
		      (match_operand:SI 2 "register_operand" "r,r,r"))))]
  "TARGET_64BIT && ISA_HAS_DIV32"
{
  return loongarch_output_division ("<insn>.w<u>\t%0,%1,%2", operands);
}
  [(set_attr "type" "idiv")
   (set_attr "mode" "SI")
   (set (attr "enabled")
      (if_then_else
	(match_test "!!which_alternative == loongarch_check_zero_div_p()")
	(const_string "yes")
	(const_string "no")))])

(define_insn "<optab>di3_fake"
  [(set (match_operand:DI 0 "register_operand" "=r,&r,&r")
	(sign_extend:DI
	  (unspec:SI
	   [(subreg:SI
	     (any_div:DI (match_operand:DI 1 "register_operand" "r,r,0")
			 (match_operand:DI 2 "register_operand" "r,r,r")) 0)]
	  UNSPEC_FAKE_ANY_DIV)))]
  "TARGET_64BIT && !ISA_HAS_DIV32"
{
  return loongarch_output_division ("<insn>.w<u>\t%0,%1,%2", operands);
}
  [(set_attr "type" "idiv")
   (set_attr "mode" "SI")
   (set (attr "enabled")
      (if_then_else
	(match_test "!!which_alternative == loongarch_check_zero_div_p()")
	(const_string "yes")
	(const_string "no")))])

;; Floating point multiply accumulate instructions.

;; a * b + c
(define_insn "fma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "f")))]
  ""
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; a * b - c
(define_insn "fms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "f"))))]
  ""
  "fmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; fnma is defined in GCC as (fma (neg op1) op2 op3)
;; (-op1 * op2) + op3 ==> -(op1 * op2) + op3 ==> -((op1 * op2) - op3)
;; The loongarch nmsub instructions implement -((op1 * op2) - op3)
;; This transformation means we may return the wrong signed zero
;; so we check HONOR_SIGNED_ZEROS.

;; -a * b + c
(define_insn "fnma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "f")))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; fnms is defined as: (fma (neg op1) op2 (neg op3))
;; ((-op1) * op2) - op3 ==> -(op1 * op2) - op3 ==> -((op1 * op2) + op3)
;; The loongarch nmadd instructions implement -((op1 * op2) + op3)
;; This transformation means we may return the wrong signed zero
;; so we check HONOR_SIGNED_ZEROS.

;; -a * b - c
(define_insn "fnms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF
	    (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
	    (match_operand:ANYF 2 "register_operand" "f")
	    (neg:ANYF (match_operand:ANYF 3 "register_operand" "f"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(-a * b - c), modulo signed zeros
(define_insn "*fma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
		(match_operand:ANYF 2 "register_operand" " f")
		(neg:ANYF (match_operand:ANYF 3 "register_operand" " f")))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(-a * b + c), modulo signed zeros
(define_insn "*fms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(neg:ANYF (match_operand:ANYF 1 "register_operand" " f"))
		(match_operand:ANYF 2 "register_operand" " f")
		(match_operand:ANYF 3 "register_operand" " f"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(a * b + c)
(define_insn "*fnms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF 1 "register_operand" " f")
		(match_operand:ANYF 2 "register_operand" " f")
		(match_operand:ANYF 3 "register_operand" " f"))))]
  ""
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(a * b - c)
(define_insn "*fnma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF 1 "register_operand" " f")
		(match_operand:ANYF 2 "register_operand" " f")
		(neg:ANYF (match_operand:ANYF 3 "register_operand" " f")))))]
  ""
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_expand "sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand")
    (sqrt:ANYF (match_operand:ANYF 1 "register_operand")))]
  ""
 {
  if (<MODE>mode == SFmode
      && TARGET_RECIP_SQRT
      && flag_unsafe_math_optimizations
      && !optimize_insn_for_size_p ()
      && flag_finite_math_only && !flag_trapping_math)
    {
      loongarch_emit_swrsqrtsf (operands[0], operands[1], SFmode, 0);
      DONE;
    }
 })

(define_insn "*sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fsqrt.<fmt>\t%0,%1"
  [(set_attr "type" "fsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand")
    (unspec:ANYF [(match_operand:ANYF 1 "register_operand")]
	   UNSPEC_RSQRT))]
  "TARGET_HARD_FLOAT"
{
   if (<MODE>mode == SFmode && TARGET_RECIP_RSQRT)
     {
       loongarch_emit_swrsqrtsf (operands[0], operands[1], SFmode, 1);
       DONE;
     }
})

(define_insn "*rsqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
    (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
	     UNSPEC_RSQRT))]
  "TARGET_HARD_FLOAT"
  "frsqrt.<fmt>\t%0,%1"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")])

;; Approximate Reciprocal Square Root Instructions.

(define_insn "loongarch_frsqrte_<fmt>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
    (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
		 UNSPEC_RSQRTE))]
  "ISA_HAS_FRECIPE"
  "frsqrte.<fmt>\t%0,%1"
  [(set_attr "type" "frsqrte")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fabs.<fmt>\t%0,%1"
  [(set_attr "type" "fabs")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	FLOATING POINT COPYSIGN
;;
;;  ....................

(define_insn "copysign<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(copysign:ANYF (match_operand:ANYF 1 "register_operand" "f")
		       (match_operand:ANYF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcopysign.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fcopysign")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "@xorsign<mode>3"
  [(match_operand:ANYF 0 "register_operand")
   (match_operand:ANYF 1 "register_operand")
   (match_operand:ANYF 2 "register_operand")]
  "ISA_HAS_LSX"
{
  machine_mode lsx_mode
    = <MODE>mode == SFmode ? V4SFmode : V2DFmode;
  rtx tmp = gen_reg_rtx (lsx_mode);
  rtx op1 = lowpart_subreg (lsx_mode, operands[1], <MODE>mode);
  rtx op2 = lowpart_subreg (lsx_mode, operands[2], <MODE>mode);
  emit_insn (gen_xorsign3 (lsx_mode, tmp, op1, op2));
  emit_move_insn (operands[0],
          lowpart_subreg (<MODE>mode, tmp, lsx_mode));
  DONE;
})

;;
;;  ....................
;;
;;	FLOATING POINT SCALE
;;
;;  ....................

(define_insn "ldexp<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unspec:ANYF [(match_operand:ANYF    1 "register_operand" "f")
		      (match_operand:<IMODE> 2 "register_operand" "f")]
		     UNSPEC_FSCALEB))]
  "TARGET_HARD_FLOAT"
  "fscaleb.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fscaleb")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	FLOATING POINT EXPONENT EXTRACT
;;
;;  ....................

(define_insn "logb_non_negative<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
		     UNSPEC_FLOGB))]
  "TARGET_HARD_FLOAT"
  "flogb.<fmt>\t%0,%1"
  [(set_attr "type" "flogb")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "logb<mode>2"
  [(set (match_operand:ANYF 0 "register_operand")
	(unspec:ANYF [(abs:ANYF (match_operand:ANYF 1 "register_operand"))]
		     UNSPEC_FLOGB))]
  "TARGET_HARD_FLOAT"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_abs<mode>2 (tmp, operands[1]));
  emit_insn (gen_logb_non_negative<mode>2 (operands[0], tmp));
  DONE;
})

;;
;;  ...................
;;
;;  Count leading zeroes.
;;
;;  ...................
;;

(define_insn "clz<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(clz:GPR (match_operand:GPR 1 "register_operand" "r")))]
  ""
  "clz.<d>\t%0,%1"
  [(set_attr "type" "clz")
   (set_attr "mode" "<MODE>")])

;;
;;  ...................
;;
;;  Count trailing zeroes.
;;
;;  ...................
;;

(define_insn "ctz<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(ctz:GPR (match_operand:GPR 1 "register_operand" "r")))]
  ""
  "ctz.<d>\t%0,%1"
  [(set_attr "type" "clz")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	MIN/MAX
;;
;;  ....................

(define_insn "smax<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(smax:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(smin:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "fmax<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" "f"))
		      (use (match_operand:ANYF 2 "register_operand" "f"))]
		     UNSPEC_FMAX))]
  ""
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "fmin<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" "f"))
		      (use (match_operand:ANYF 2 "register_operand" "f"))]
		     UNSPEC_FMIN))]
  ""
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "smaxa<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(if_then_else:ANYF
	      (gt (abs:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (abs:ANYF (match_operand:ANYF 2 "register_operand" "f")))
	      (match_dup 1)
	      (match_dup 2)))]
  ""
  "fmaxa.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "smina<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(if_then_else:ANYF
		(lt (abs:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		    (abs:ANYF (match_operand:ANYF 2 "register_operand" "f")))
		(match_dup 1)
		(match_dup 2)))]
  ""
  "fmina.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	NEGATION and ONE'S COMPLEMENT
;;
;;  ....................

(define_insn "neg<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(neg:GPR (match_operand:GPR 1 "register_operand" "r")))]
  ""
  "sub.<d>\t%0,%.,%1"
  [(set_attr "alu_type"	"sub")
   (set_attr "mode" "<MODE>")])

(define_insn "*negsi2_extended"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (neg:SI (match_operand:SI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "sub.w\t%0,%.,%1"
  [(set_attr "alu_type" "sub")
   (set_attr "mode" "SI")])

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fneg.<fmt>\t%0,%1"
  [(set_attr "type" "fneg")
   (set_attr "mode" "<UNITMODE>")])


;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

(define_insn "<optab><mode>3"
  [(set (match_operand:X 0 "register_operand" "=r,r")
	(any_bitwise:X (match_operand:X 1 "register_operand" "%r,r")
		       (match_operand:X 2 "uns_arith_operand" "r,K")))]
  ""
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*<optab>si3_internal"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(any_bitwise:SI (match_operand:SI 1 "register_operand" "%r,r")
			(match_operand:SI 2 "uns_arith_operand"    " r,K")))]
  "TARGET_64BIT"
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:X 0 "register_operand" "=r")
	(not:X (match_operand:X 1 "register_operand" "r")))]
  ""
  "nor\t%0,%.,%1"
  [(set_attr "alu_type" "not")
   (set_attr "mode" "<MODE>")])

(define_insn "*one_cmplsi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" " r")))]
  "TARGET_64BIT"
  "nor\t%0,%.,%1"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

(define_insn "and<mode>3_extended"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(and:GPR (match_operand:GPR 1 "nonimmediate_operand" "r")
		 (match_operand:GPR 2 "low_bitmask_operand" "Yx")))]
  ""
{
  int len;

  len = low_bitmask_len (<MODE>mode, INTVAL (operands[2]));
  operands[2] = GEN_INT (len-1);
  return "bstrpick.<d>\t%0,%1,%2,0";
}
  [(set_attr "move_type" "pick_ins")
   (set_attr "mode" "<MODE>")])

(define_insn_and_split "*bstrins_<mode>_for_mask"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(and:GPR (match_operand:GPR 1 "register_operand" "r")
		 (match_operand:GPR 2 "ins_zero_bitmask_operand" "i")))]
  ""
  "#"
  ""
  [(set (match_dup 0) (match_dup 1))
   (set (zero_extract:GPR (match_dup 0) (match_dup 2) (match_dup 3))
	(const_int 0))]
  {
    unsigned HOST_WIDE_INT mask = ~UINTVAL (operands[2]);
    int lo = ffs_hwi (mask) - 1;
    int len = low_bitmask_len (<MODE>mode, mask >> lo);

    len = MIN (len, GET_MODE_BITSIZE (<MODE>mode) - lo);
    operands[2] = GEN_INT (len);
    operands[3] = GEN_INT (lo);
  })

(define_insn_and_split "*bstrins_<mode>_for_ior_mask"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(ior:GPR (and:GPR (match_operand:GPR 1 "register_operand" "r")
			  (match_operand:GPR 2 "const_int_operand" "i"))
		 (and:GPR (match_operand:GPR 3 "register_operand" "r")
			  (match_operand:GPR 4 "const_int_operand" "i"))))]
  "loongarch_pre_reload_split ()
   && loongarch_use_bstrins_for_ior_with_mask (<MODE>mode, operands)"
  "#"
  "&& true"
  [(set (match_dup 0) (match_dup 1))
   (set (zero_extract:GPR (match_dup 0) (match_dup 2) (match_dup 4))
	(match_dup 3))]
  {
    if (loongarch_use_bstrins_for_ior_with_mask (<MODE>mode, operands) < 0)
      {
	std::swap (operands[1], operands[3]);
	std::swap (operands[2], operands[4]);
      }

    unsigned HOST_WIDE_INT mask = ~UINTVAL (operands[2]);
    int lo = ffs_hwi (mask) - 1;
    int len = low_bitmask_len (<MODE>mode, mask >> lo);

    len = MIN (len, GET_MODE_BITSIZE (<MODE>mode) - lo);
    operands[2] = GEN_INT (len);
    operands[4] = GEN_INT (lo);

    if (lo)
      {
	rtx tmp = gen_reg_rtx (<MODE>mode);
	emit_move_insn (tmp, gen_rtx_ASHIFTRT(<MODE>mode, operands[3],
					      GEN_INT (lo)));
	operands[3] = tmp;
      }
  })

;; We always avoid the shift operation in bstrins_<mode>_for_ior_mask
;; if possible, but the result may be sub-optimal when one of the masks
;; is (1 << N) - 1 and one of the src register is the dest register.
;; For example:
;;     move		t0, a0
;;     move		a0, a1
;;     bstrins.d	a0, t0, 42, 0
;;     ret
;; using a shift operation would be better:
;;     srai.d		t0, a1, 43
;;     bstrins.d	a0, t0, 63, 43
;;     ret
;; unfortunately we cannot figure it out in split1: before reload we cannot
;; know if the dest register is one of the src register.  Fix it up in
;; peephole2.
(define_peephole2
  [(set (match_operand:GPR 0 "register_operand")
	(match_operand:GPR 1 "register_operand"))
   (set (match_dup 1) (match_operand:GPR 2 "register_operand"))
   (set (zero_extract:GPR (match_dup 1)
			  (match_operand:SI 3 "const_int_operand")
			  (const_int 0))
	(match_dup 0))]
  "peep2_reg_dead_p (3, operands[0])"
  [(const_int 0)]
  {
    int len = GET_MODE_BITSIZE (<MODE>mode) - INTVAL (operands[3]);

    emit_insn (gen_ashr<mode>3 (operands[0], operands[2], operands[3]));
    emit_insn (gen_insv<mode> (operands[1], GEN_INT (len), operands[3],
			       operands[0]));
    DONE;
  })

(define_insn "*iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ior:HI (match_operand:HI 1 "register_operand" "%r,r")
		(match_operand:HI 2 "uns_arith_operand" "r,K")))]
  ""
  "or%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

(define_insn "nor<mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X (not:X (match_operand:X 1 "register_operand" "%r"))
		 (not:X (match_operand:X 2 "register_operand" "r"))))]
  ""
  "nor\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*norsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "%r"))
		 (not:SI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "nor\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

(define_insn "<optab>n<mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
	(neg_bitwise:X
	    (not:X (match_operand:X 2 "register_operand" "r"))
	    (match_operand:X 1 "register_operand" "r")))]
  ""
  "<insn>n\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*<optab>nsi_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg_bitwise:SI
	    (not:SI (match_operand:SI 1 "register_operand" "r"))
	    (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_64BIT"
  "<insn>n\t%0,%2,%1"
  [(set_attr "type" "logical")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	TRUNCATION
;;
;;  ....................

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_DOUBLE_FLOAT"
  "fcvt.s.d\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "cnv_mode"	"D2S")
   (set_attr "mode" "SF")])

;; In vector registers, popcount can be implemented directly through
;; the vector instruction [X]VPCNT.  For GP registers, we can implement
;; it through the following method.  Compared with loop implementation
;; of popcount, the following method has better performance.

;; This attribute used for get connection of scalar mode and corresponding
;; vector mode.
(define_mode_attr cntmap [(SI "v4si") (DI "v2di")])

(define_expand "popcount<mode>2"
  [(set (match_operand:GPR 0 "register_operand")
	(popcount:GPR (match_operand:GPR 1 "register_operand")))]
  "ISA_HAS_LSX"
{
  rtx in = operands[1];
  rtx out = operands[0];
  rtx vreg = <MODE>mode == SImode ? gen_reg_rtx (V4SImode) :
				    gen_reg_rtx (V2DImode);
  emit_insn (gen_lsx_vinsgr2vr_<size> (vreg, in, vreg, GEN_INT (1)));
  emit_insn (gen_popcount<cntmap>2 (vreg, vreg));
  emit_insn (gen_lsx_vpickve2gr_<size> (out, vreg, GEN_INT (0)));
  DONE;
})

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................
(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand")
	(zero_extend:DI (match_operand:SI 1 "nonimmediate_operand")))]
  "TARGET_64BIT")

(define_insn_and_split "*zero_extendsidi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m,ZC,k")))]
  "TARGET_64BIT"
  "@
   bstrpick.d\t%0,%1,31,0
   ld.wu\t%0,%1
   #
   ldx.wu\t%0,%1"
  "&& reload_completed
   && MEM_P (operands[1])
   && (loongarch_14bit_shifted_offset_address_p (XEXP (operands[1], 0), SImode)
       && !loongarch_12bit_offset_address_p (XEXP (operands[1], 0), SImode))
   && !paradoxical_subreg_p (operands[0])"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 0)
	(ior:DI (zero_extend:DI
		  (subreg:SI (match_dup 0) 0))
		(match_dup 2)))]
  {
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[3] = gen_lowpart (SImode, operands[0]);
    operands[2] = const0_rtx;
  }
  [(set_attr "move_type" "arith,load,load,load")
   (set_attr "mode" "DI")])

(define_insn "zero_extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r,r,r")
	(zero_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "r,m,k")))]
  ""
  "@
   bstrpick.w\t%0,%1,<SHORT:7_or_15>,0
   ld.<SHORT:size>u\t%0,%1
   ldx.<SHORT:size>u\t%0,%1"
  [(set_attr "move_type" "pick_ins,load,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,k,m")))]
  ""
  "@
   andi\t%0,%1,0xff
   ldx.bu\t%0,%1
   ld.bu\t%0,%1"
  [(set_attr "move_type" "andi,load,load")
   (set_attr "mode" "HI")])

;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn "*zero_extend<GPR:mode>_trunc<SHORT:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(zero_extend:GPR
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "bstrpick.w\t%0,%1,<SHORT:7_or_15>,0"
  [(set_attr "move_type" "pick_ins")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*zero_extendhi_truncqi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "andi\t%0,%1,0xff"
  [(set_attr "alu_type" "and")
   (set_attr "mode" "HI")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r,r")
	(sign_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" "r,ZC,m,k,f")))]
  "TARGET_64BIT"
  "@
   slli.w\t%0,%1,0
   ldptr.w\t%0,%1
   ld.w\t%0,%1
   ldx.w\t%0,%1
   movfr2gr.s\t%0,%1"
  [(set_attr "move_type" "sll0,load,load,load,mftg")
   (set_attr "mode" "DI")])

(define_insn "extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r,r,r")
	(sign_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "r,m,k")))]
  ""
  "@
   ext.w.<SHORT:size>\t%0,%1
   ld.<SHORT:size>\t%0,%1
   ldx.<SHORT:size>\t%0,%1"
  [(set_attr "move_type" "signext,load,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(sign_extend:HI
	     (match_operand:QI 1 "nonimmediate_operand" "r,m,k")))]
  ""
  "@
   ext.w.b\t%0,%1
   ld.b\t%0,%1
   ldx.b\t%0,%1"
  [(set_attr "move_type" "signext,load,load")
   (set_attr "mode" "SI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_DOUBLE_FLOAT"
  "fcvt.d.s\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "cnv_mode" "S2D")
   (set_attr "mode" "DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

;; conversion of a floating-point value to a integer

(define_insn "fix_trunc<ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=f")
	(fix:GPR (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "ftintrz.<GPR:ifmt>.<ANYF:fmt> %0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

;; conversion of an integeral (or boolean) value to a floating-point value

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_DOUBLE_FLOAT"
  "ffint.d.w\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "DF")
   (set_attr "cnv_mode"	"I2D")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_DOUBLE_FLOAT"
  "ffint.d.l\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "DF")
   (set_attr "cnv_mode" "I2D")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "ffint.s.w\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")
   (set_attr "cnv_mode"	"I2S")])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_DOUBLE_FLOAT"
  "ffint.s.l\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")
   (set_attr "cnv_mode"	"I2S")])

;; Convert a floating-point value to an unsigned integer.

(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand")))]
  "TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31, DFmode);

  loongarch_emit_move (reg1,
		       const_double_from_real_value (offset, DFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  loongarch_emit_move (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
  loongarch_emit_move (reg3, GEN_INT (trunc_int_for_mode
					(BITMASK_HIGH, SImode)));

  emit_insn (gen_fix_truncdfsi2 (operands[0], reg2));
  emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_use (stack_pointer_rtx);
  DONE;
})

(define_expand "fixuns_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand")
	(unsigned_fix:DI (match_operand:DF 1 "register_operand")))]
  "TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63, DFmode);

  loongarch_emit_move (reg1, const_double_from_real_value (offset, DFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncdfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  loongarch_emit_move (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
  loongarch_emit_move (reg3, GEN_INT (BITMASK_HIGH));
  emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

  emit_insn (gen_fix_truncdfdi2 (operands[0], reg2));
  emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_use (stack_pointer_rtx);
  DONE;
})

(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand")))]
  "TARGET_HARD_FLOAT"
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31, SFmode);

  loongarch_emit_move (reg1, const_double_from_real_value (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  loongarch_emit_move (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  loongarch_emit_move (reg3, GEN_INT (trunc_int_for_mode
				 (BITMASK_HIGH, SImode)));

  emit_insn (gen_fix_truncsfsi2 (operands[0], reg2));
  emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_use (stack_pointer_rtx);
  DONE;
})

(define_expand "fixuns_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand")
	(unsigned_fix:DI (match_operand:SF 1 "register_operand")))]
  "TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63, SFmode);

  loongarch_emit_move (reg1, const_double_from_real_value (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  loongarch_emit_move (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  loongarch_emit_move (reg3, GEN_INT (BITMASK_HIGH));
  emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

  emit_insn (gen_fix_truncsfdi2 (operands[0], reg2));
  emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_use (stack_pointer_rtx);
  DONE;
})

;;
;;  ....................
;;
;;	EXTRACT AND INSERT
;;
;;  ....................

(define_expand "extzv<mode>"
  [(set (match_operand:X 0 "register_operand")
	(zero_extract:X (match_operand:X 1 "register_operand")
			(match_operand 2 "const_int_operand")
			(match_operand 3 "const_int_operand")))]
  ""
{
  if (!loongarch_use_ins_ext_p (operands[1], INTVAL (operands[2]),
				INTVAL (operands[3])))
    FAIL;
})

(define_insn "*extzv<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "loongarch_use_ins_ext_p (operands[1], INTVAL (operands[2]),
			    INTVAL (operands[3]))"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "bstrpick.<d>\t%0,%1,%2,%3";
}
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_expand "insv<mode>"
  [(set (zero_extract:GPR (match_operand:GPR 0 "register_operand")
			  (match_operand 1 "const_int_operand")
			  (match_operand 2 "const_int_operand"))
	(match_operand:GPR 3 "reg_or_0_operand"))]
  ""
{
  if (!loongarch_use_ins_ext_p (operands[0], INTVAL (operands[1]),
				INTVAL (operands[2])))
    FAIL;
})

(define_insn "*insv<mode>"
  [(set (zero_extract:GPR (match_operand:GPR 0 "register_operand" "+r")
			  (match_operand:SI 1 "const_int_operand" "")
			  (match_operand:SI 2 "const_int_operand" ""))
	(match_operand:GPR 3 "reg_or_0_operand" "rJ"))]
  "loongarch_use_ins_ext_p (operands[0], INTVAL (operands[1]),
			    INTVAL (operands[2]))"
{
  operands[1] = GEN_INT (INTVAL (operands[1]) + INTVAL (operands[2]) - 1);
  return "bstrins.<d>\t%0,%z3,%1,%2";
}
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; 64-bit integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movdi"
  [(set (match_operand:DI 0 "")
	(match_operand:DI 1 ""))]
  ""
{
  if (loongarch_legitimize_move (DImode, operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,w,*f,*f,*r,*m")
       (match_operand:DI 1 "move_operand" "r,i,w,r,*J*r,*m,*f,*f"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return loongarch_output_move (operands); }
  "CONST_INT_P (operands[1]) && REG_P (operands[0]) && GP_REG_P (REGNO
  (operands[0]))"
  [(const_int 0)]
  "
{
  loongarch_move_integer (operands[0], operands[0], INTVAL (operands[1]));
  DONE;
}
  "
  [(set_attr "move_type" "move,const,load,store,mgtf,fpload,mftg,fpstore")
   (set_attr "mode" "DI")])

(define_insn_and_split "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,w,*f,*f,*r,*m")
	(match_operand:DI 1 "move_operand" "r,Yd,w,rJ,*r*J,*m,*f,*f"))]
  "TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  {
    return loongarch_output_move (operands);
  }
  "CONST_INT_P (operands[1]) && REG_P (operands[0]) && GP_REG_P (REGNO
  (operands[0]))"
  [(const_int 0)]
  "
{
  loongarch_move_integer (operands[0], operands[0], INTVAL (operands[1]));
  DONE;
}
  "
  [(set_attr "move_type" "move,const,load,store,mgtf,fpload,mftg,fpstore")
   (set_attr "mode" "DI")])

;; Use two registers to get the global symbol address from the got table.
;; la.global rd, rt, sym

(define_insn_and_split "movdi_symbolic_off64"
 [(set (match_operand:DI 0 "register_operand" "=r,r")
       (match_operand:DI 1 "symbolic_off64_or_reg_operand" "Yd,r"))
  (unspec:DI [(const_int 0)]
    UNSPEC_LOAD_SYMBOL_OFFSET64)
  (clobber (match_operand:DI 2 "register_operand" "=&r,r"))]
 "TARGET_64BIT && TARGET_CMODEL_EXTREME"
{
  if (which_alternative == 1)
    return "#";

  enum loongarch_symbol_type symbol_type;
  gcc_assert (loongarch_symbolic_constant_p (operands[1], &symbol_type));

  switch (symbol_type)
    {
    case SYMBOL_PCREL64:
      return "la.local\t%0,%2,%1";
    case SYMBOL_GOT_DISP:
      return "la.global\t%0,%2,%1";
    case SYMBOL_TLS_IE:
      return "la.tls.ie\t%0,%2,%1";
    case SYMBOL_TLSGD:
      return "la.tls.gd\t%0,%2,%1";
    case SYMBOL_TLSLDM:
      return "la.tls.ld\t%0,%2,%1";

    default:
      gcc_unreachable ();
  }
}
 "&& REG_P (operands[1]) && find_reg_note (insn, REG_UNUSED, operands[2]) != 0"
 [(set (match_dup 0) (match_dup 1))]
 ""
 [(set_attr "mode" "DI")
  (set_attr "insn_count" "5")])

;; The 64-bit PC-relative part of address loading.
;; Note that the psABI does not allow splitting it.
(define_insn "la_pcrel64_two_parts"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 2 "") (pc)] UNSPEC_LA_PCREL_64_PART1))
   (set (match_operand:DI 1 "register_operand" "=r")
	(unspec:DI [(match_dup 2) (pc)] UNSPEC_LA_PCREL_64_PART2))]
  "TARGET_ABI_LP64 && la_opt_explicit_relocs != EXPLICIT_RELOCS_NONE"
  {
    return "pcalau12i\t%0,%r2\n\t"
	   "addi.d\t%1,$r0,%L2\n\t"
	   "lu32i.d\t%1,%R2\n\t"
	   "lu52i.d\t%1,%1,%H2";
  }
  [(set_attr "move_type" "move")
   (set_attr "mode" "DI")
   (set_attr "length" "16")])

;; 32-bit Integer moves

(define_expand "movsi"
  [(set (match_operand:SI 0 "")
	(match_operand:SI 1 ""))]
  ""
{
  if (loongarch_legitimize_move (SImode, operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,w,*f,f,*r,*m")
	(match_operand:SI 1 "move_operand" "r,Yd,w,rJ,*r*J,m,*f,*f"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  { return loongarch_output_move (operands); }
  "CONST_INT_P (operands[1]) && REG_P (operands[0]) && GP_REG_P (REGNO
  (operands[0]))"
  [(const_int 0)]
  "
{
  loongarch_move_integer (operands[0], operands[0], INTVAL (operands[1]));
  DONE;
}
  "
  [(set_attr "move_type" "move,const,load,store,mgtf,fpload,mftg,fpstore")
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
  if (loongarch_legitimize_move (HImode, operands[0], operands[1]))
    DONE;
})

(define_insn_and_split "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,m,r,k")
	(match_operand:HI 1 "move_operand" "r,Yd,I,m,rJ,k,rJ"))]
  "(register_operand (operands[0], HImode)
       || reg_or_0_operand (operands[1], HImode))"
  { return loongarch_output_move (operands); }
  "CONST_INT_P (operands[1]) && REG_P (operands[0]) && GP_REG_P (REGNO
  (operands[0]))"
  [(const_int 0)]
  "
{
  loongarch_move_integer (operands[0], operands[0], INTVAL (operands[1]));
  DONE;
}
  "
  [(set_attr "move_type" "move,const,const,load,store,load,store")
   (set_attr "mode" "HI")])

;; 8-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movqi"
  [(set (match_operand:QI 0 "")
	(match_operand:QI 1 ""))]
  ""
{
  if (loongarch_legitimize_move (QImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m,r,k")
	(match_operand:QI 1 "move_operand" "r,I,m,rJ,k,rJ"))]
  "(register_operand (operands[0], QImode)
       || reg_or_0_operand (operands[1], QImode))"
  { return loongarch_output_move (operands); }
  [(set_attr "move_type" "move,const,load,store,load,store")
   (set_attr "mode" "QI")])

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "")
	(match_operand:SF 1 ""))]
  ""
{
  if (loongarch_legitimize_move (SFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movsf_hardfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,f,k,m,k,*f,*r,*r,*r,*m")
	(match_operand:SF 1 "move_operand" "f,G,m,f,k,f,G,G,*r,*f,*G*r,*m,*r"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return loongarch_output_move (operands); }
  [(set_attr "move_type" "fmove,mgtf,fpload,fpstore,fpload,fpstore,store,store,mgtf,mftg,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "move_operand" "Gr,m,r"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return loongarch_output_move (operands); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "SF")])

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "")
	(match_operand:DF 1 ""))]
  ""
{
  if (loongarch_legitimize_move (DFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movdf_hardfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,f,k,m,k,*f,*r,*r,*r,*m")
	(match_operand:DF 1 "move_operand" "f,G,m,f,k,f,G,G,*r,*f,*r*G,*m,*r"))]
  "TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return loongarch_output_move (operands); }
  [(set_attr "move_type" "fmove,mgtf,fpload,fpstore,fpload,fpstore,store,store,mgtf,mftg,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:DF 1 "move_operand" "rG,m,rG"))]
  "(TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT)
   && TARGET_64BIT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return loongarch_output_move (operands); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "DF")])

;; Clear one FCC register

(define_expand "movfcc"
  [(set (match_operand:FCC 0 "")
	(match_operand:FCC 1 ""))]
  "TARGET_HARD_FLOAT"
{
  if (memory_operand (operands[0], FCCmode)
      && memory_operand (operands[1], FCCmode))
    operands[1] = force_reg (FCCmode, operands[1]);
})

(define_insn "movfcc_internal"
  [(set (match_operand:FCC 0 "nonimmediate_operand"
			     "=z,z,*f,*f,*r,*r,*m,*f,*r,z,*r")
	(match_operand:FCC 1 "reg_or_0_operand"
			     "J,*f,z,*f,J*r,*m,J*r,J*r,*f,*r,z"))]
  "TARGET_HARD_FLOAT"
  "@
   fcmp.caf.s\t%0,$f0,$f0
   movfr2cf\t%0,%1
   movcf2fr\t%0,%1
   fmov.s\t%0,%1
   or\t%0,%z1,$r0
   ld.b\t%0,%1
   st.b\t%z1,%0
   movgr2fr.w\t%0,%1
   movfr2gr.s\t%0,%1
   movgr2cf\t%0,%1
   movcf2gr\t%0,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "FCC")])

(define_insn "fcc_to_<X:mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(if_then_else:X (ne (match_operand:FCC 1 "register_operand" "0")
			    (const_int 0))
			(const_int 1)
			(const_int 0)))]
  "TARGET_HARD_FLOAT"
  ""
  [(set_attr "length" "0")
   (set_attr "type" "ghost")])

(define_expand "cstore<ANYF:mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "loongarch_fcmp_operator"
	  [(match_operand:ANYF 2 "register_operand")
	   (match_operand:ANYF 3 "register_operand")]))]
  ""
  {
    rtx fcc = gen_reg_rtx (FCCmode);
    rtx cmp = gen_rtx_fmt_ee (GET_CODE (operands[1]), FCCmode,
			      operands[2], operands[3]);

    emit_insn (gen_rtx_SET (fcc, cmp));
    if (TARGET_64BIT)
      {
	rtx gpr = gen_reg_rtx (DImode);
	emit_insn (gen_fcc_to_di (gpr, fcc));
	emit_insn (gen_rtx_SET (operands[0],
				lowpart_subreg (SImode, gpr, DImode)));
      }
    else
      emit_insn (gen_fcc_to_si (operands[0], fcc));

    DONE;
  })

;; Conditional move instructions.

(define_insn "*sel<code><GPR:mode>_using_<GPR2:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(if_then_else:GPR
	 (equality_op:GPR2 (match_operand:GPR2 1 "register_operand" "r,r")
			   (const_int 0))
	 (match_operand:GPR 2 "reg_or_0_operand" "r,J")
	 (match_operand:GPR 3 "reg_or_0_operand" "J,r")))]
  "register_operand (operands[2], <GPR:MODE>mode)
   != register_operand (operands[3], <GPR:MODE>mode)"
  "@
   <sel>\t%0,%2,%1
   <selinv>\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

;; fsel copies the 3rd argument when the 1st is non-zero and the 2nd
;; argument if the 1st is zero.  This means operand 2 and 3 are
;; inverted in the instruction.

(define_insn "*sel<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(if_then_else:ANYF
	 (ne:FCC (match_operand:FCC 1 "register_operand" "z")
		 (const_int 0))
	 (match_operand:ANYF 2 "reg_or_0_operand" "f")
	 (match_operand:ANYF 3 "reg_or_0_operand" "f")))]
  ""
  "fsel\t%0,%3,%2,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<ANYF:MODE>")])

;; These are the main define_expand's used to make conditional moves.

(define_expand "mov<mode>cc"
  [(set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_operator 1 "comparison_operator"
			 [(match_operand:GPR 2 "reg_or_0_operand")
			  (match_operand:GPR 3 "reg_or_0_operand")])))]
  "TARGET_COND_MOVE_INT"
{
  if (!INTEGRAL_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  loongarch_expand_conditional_move (operands);
  DONE;
})

(define_expand "mov<mode>cc"
  [(set (match_operand:ANYF 0 "register_operand")
	(if_then_else:ANYF (match_operator 1 "comparison_operator"
			  [(match_operand:ANYF 2 "reg_or_0_operand")
			   (match_operand:ANYF 3 "reg_or_0_operand")])))]
  "TARGET_COND_MOVE_FLOAT"
{
  if (!FLOAT_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  loongarch_expand_conditional_move (operands);
  DONE;
})

(define_insn "lu32i_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI
	  (zero_extend:DI
	    (subreg:SI (match_operand:DI 1 "register_operand" "0") 0))
	  (match_operand:DI 2 "const_lu32i_operand" "u")))]
  "TARGET_64BIT"
  {
    operands[2] = GEN_INT (INTVAL (operands[2]) >> 32);
    return "lu32i.d\t%0,%X2";
  }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "lu52i_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI
	  (and:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand 2 "lu52i_mask_operand"))
	  (match_operand 3 "const_lu52i_operand" "v")))]
  "TARGET_64BIT"
  {
    operands[3] = GEN_INT (INTVAL (operands[3]) >> 52);
    return "lu52i.d\t%0,%1,%X3";
  }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

;; Instructions for adding the low 12 bits of an address to a register.
;; Operand 2 is the address: loongarch_print_operand works out which relocation
;; should be applied.

(define_insn "*low<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
 (lo_sum:P (match_operand:P 1 "register_operand" " r")
     (match_operand:P 2 "symbolic_operand" "")))]
  ""
  "addi.<d>\t%0,%1,%L2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "@tls_low<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(mem:P (lo_sum:P (match_operand:P 1 "register_operand" "r")
				    (match_operand:P 2 "symbolic_operand" "")))]
	UNSPEC_TLS_LOW))]
  ""
  "addi.<d>\t%0,%1,%L2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

;; Instructions for loading address from GOT entry.
;; operands[1] is pc plus the high half of the address difference with the got
;; entry;
;; operands[2] is low 12 bits for low 12 bit of the address difference with the
;; got entry.
;; loongarch_print_operand works out which relocation should be applied.

(define_insn "@ld_from_got<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(mem:P (lo_sum:P
				(match_operand:P 1 "register_operand" "r")
				(match_operand:P 2 "symbolic_operand")))]
	UNSPEC_LOAD_FROM_GOT))]
  ""
  "%Q2ld.<d>\t%0,%1,%L2"
  [(set_attr "type" "move")]
)

(define_insn "@lui_l_hi20<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "symbolic_operand")]
	UNSPEC_LUI_L_HI20))]
  ""
  "lu12i.w\t%0,%r1"
  [(set_attr "type" "move")]
)

(define_insn "@pcalau12i<mode>"
  [(set (match_operand:P 0 "register_operand" "=j")
	(unspec:P [(match_operand:P 1 "symbolic_operand" "")]
	UNSPEC_PCALAU12I))]
  ""
  "pcalau12i\t%0,%%pc_hi20(%1)"
  [(set_attr "type" "move")])

;; @pcalau12i may be used for sibcall so it has a strict constraint.  This
;; allows any general register as the operand.
(define_insn "@pcalau12i_gr<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
       (unspec:P [(match_operand:P 1 "symbolic_operand" "")]
       UNSPEC_PCALAU12I_GR))]
  ""
  "pcalau12i\t%0,%%pc_hi20(%1)"
  [(set_attr "type" "move")])

(define_insn "@add_tls_le_relax<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "register_operand" "r")
		   (match_operand:P 2 "register_operand" "r")
		   (match_operand:P 3 "symbolic_operand")]
	  UNSPEC_ADD_TLS_LE_RELAX))]
  "HAVE_AS_TLS_LE_RELAXATION"
  "add.<d>\t%0,%1,%2,%%le_add_r(%3)"
  [(set_attr "type" "move")]
)

(define_insn "@ori_l_lo12<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "register_operand" "r")
		   (match_operand:P 2 "symbolic_operand")]
	UNSPEC_ORI_L_LO12))]
  ""
  "ori\t%0,%1,%L2"
  [(set_attr "type" "move")]
)

(define_insn "lui_h_lo20"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:DI 2 "symbolic_operand")]
	UNSPEC_LUI_H_LO20))]
  "TARGET_64BIT"
  "lu32i.d\t%0,%R2"
  [(set_attr "type" "move")]
)

(define_insn "lui_h_hi12"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")
		    (match_operand:DI 2 "symbolic_operand")]
	UNSPEC_LUI_H_HI12))]
  "TARGET_64BIT"
  "lu52i.d\t%0,%1,%H2"
  [(set_attr "type" "move")]
)

;; Round floating-point numbers to integers
(define_insn "rint<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
		      UNSPEC_FRINT))]
  ""
  "frint.<fmt>\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<MODE>")])

;; Convert floating-point numbers to integers
(define_insn "<lrint_pattern><ANYF:mode><ANYFI:mode>2"
  [(set (match_operand:ANYFI 0 "register_operand" "=f")
	(unspec:ANYFI [(match_operand:ANYF 1 "register_operand" "f")]
		      LRINT))]
  "TARGET_HARD_FLOAT &&
   (<LRINT> == UNSPEC_FTINT
    || flag_fp_int_builtin_inexact
    || !flag_trapping_math)"
  "ftint<lrint_submenmonic>.<ANYFI:ifmt>.<ANYF:fmt> %0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

;; Thread-Local Storage

(define_insn "@got_load_tls_desc<mode>"
  [(set (reg:P 4)
	(unspec:P
	    [(match_operand:P 0 "symbolic_operand" "")]
	    UNSPEC_TLS_DESC))
    (clobber (reg:SI FCC0_REGNUM))
    (clobber (reg:SI FCC1_REGNUM))
    (clobber (reg:SI FCC2_REGNUM))
    (clobber (reg:SI FCC3_REGNUM))
    (clobber (reg:SI FCC4_REGNUM))
    (clobber (reg:SI FCC5_REGNUM))
    (clobber (reg:SI FCC6_REGNUM))
    (clobber (reg:SI FCC7_REGNUM))
    (clobber (reg:SI RETURN_ADDR_REGNUM))]
  "TARGET_TLS_DESC"
{
  return TARGET_EXPLICIT_RELOCS
    ? "pcalau12i\t$r4,%%desc_pc_hi20(%0)\n\t"
      "addi.d\t$r4,$r4,%%desc_pc_lo12(%0)\n\t"
      "ld.d\t$r1,$r4,%%desc_ld(%0)\n\t"
      "jirl\t$r1,$r1,%%desc_call(%0)"
    : "la.tls.desc\t$r4,%0";
}
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "16")])

(define_insn "got_load_tls_desc_off64"
  [(set (reg:DI 4)
	(unspec:DI
	    [(match_operand:DI 0 "symbolic_operand" "")]
	    UNSPEC_TLS_DESC_OFF64))
    (clobber (reg:SI FCC0_REGNUM))
    (clobber (reg:SI FCC1_REGNUM))
    (clobber (reg:SI FCC2_REGNUM))
    (clobber (reg:SI FCC3_REGNUM))
    (clobber (reg:SI FCC4_REGNUM))
    (clobber (reg:SI FCC5_REGNUM))
    (clobber (reg:SI FCC6_REGNUM))
    (clobber (reg:SI FCC7_REGNUM))
    (clobber (reg:SI RETURN_ADDR_REGNUM))
    (clobber (match_operand:DI 1 "register_operand" "=&r"))]
  "TARGET_TLS_DESC && TARGET_CMODEL_EXTREME"
{
  return TARGET_EXPLICIT_RELOCS
    ? "pcalau12i\t$r4,%%desc_pc_hi20(%0)\n\t"
      "addi.d\t%1,$r0,%%desc_pc_lo12(%0)\n\t"
      "lu32i.d\t%1,%%desc64_pc_lo20(%0)\n\t"
      "lu52i.d\t%1,%1,%%desc64_pc_hi12(%0)\n\t"
      "add.d\t$r4,$r4,%1\n\t"
      "ld.d\t$r1,$r4,%%desc_ld(%0)\n\t"
      "jirl\t$r1,$r1,%%desc_call(%0)"
    : "la.tls.desc\t$r4,%1,%0";
}
  [(set_attr "got" "load")
   (set_attr "length" "28")])

(define_insn "@load_tls<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS))]
  ""
{
  enum loongarch_symbol_type symbol_type;
  gcc_assert (loongarch_symbolic_constant_p (operands[1], &symbol_type));

  switch (symbol_type)
    {
    case SYMBOL_TLS_LE:
      return "la.tls.le\t%0,%1";
    case SYMBOL_TLS_IE:
      return "la.tls.ie\t%0,%1";
    case SYMBOL_TLSLDM:
      return "la.tls.ld\t%0,%1";
    case SYMBOL_TLSGD:
      return "la.tls.gd\t%0,%1";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "mode" "<MODE>")
   (set (attr "insn_count")
      (if_then_else
	(match_test "TARGET_CMODEL_EXTREME")
	(const_int 4)
	(const_int 2)))])


;; Expand in-line code to clear the instruction cache between operand[0] and
;; operand[1].
(define_expand "clear_cache"
  [(match_operand 0 "pmode_register_operand")
   (match_operand 1 "pmode_register_operand")]
  ""
{
  emit_insn (gen_loongarch_ibar (const0_rtx));
  DONE;
})

(define_insn "loongarch_ibar"
  [(unspec_volatile:SI
      [(match_operand 0 "const_uimm15_operand")]
       UNSPECV_IBAR)
   (clobber (mem:BLK (scratch)))]
  ""
  "ibar\t%0")

(define_insn "loongarch_dbar"
  [(unspec_volatile:SI
      [(match_operand 0 "const_uimm15_operand")]
       UNSPECV_DBAR)
   (clobber (mem:BLK (scratch)))]
  ""
  "dbar\t%0")



;; Privileged state instruction

(define_insn "loongarch_cpucfg"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")]
			     UNSPECV_CPUCFG))]
  ""
  "cpucfg\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "SI")])

(define_insn "loongarch_syscall"
  [(unspec_volatile:SI
      [(match_operand 0 "const_uimm15_operand")]
       UNSPECV_SYSCALL)
   (clobber (mem:BLK (scratch)))]
  ""
  "syscall\t%0")

(define_insn "loongarch_break"
  [(unspec_volatile:SI
      [(match_operand 0 "const_uimm15_operand")]
       UNSPECV_BREAK)
   (clobber (mem:BLK (scratch)))]
  ""
  "break\t%0")

(define_insn "loongarch_asrtle_d"
  [(unspec_volatile:DI [(match_operand:DI 0 "register_operand" "r")
			(match_operand:DI 1 "register_operand" "r")]
			UNSPECV_ASRTLE_D)]
  "TARGET_64BIT"
  "asrtle.d\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")])

(define_insn "loongarch_asrtgt_d"
  [(unspec_volatile:DI [(match_operand:DI 0 "register_operand" "r")
			(match_operand:DI 1 "register_operand" "r")]
			UNSPECV_ASRTGT_D)]
  "TARGET_64BIT"
  "asrtgt.d\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")])

(define_insn "loongarch_csrrd_<d>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec_volatile:GPR [(match_operand  1 "const_uimm14_operand")]
			      UNSPECV_CSRRD))
   (clobber (mem:BLK (scratch)))]
  ""
  "csrrd\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_csrwr_<d>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	  (unspec_volatile:GPR
	    [(match_operand:GPR 1 "register_operand" "0")
	     (match_operand 2 "const_uimm14_operand")]
	     UNSPECV_CSRWR))
   (clobber (mem:BLK (scratch)))]
  ""
  "csrwr\t%0,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_csrxchg_<d>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	  (unspec_volatile:GPR
	    [(match_operand:GPR 1 "register_operand" "0")
	     (match_operand:GPR 2 "register_operand" "q")
	     (match_operand 3 "const_uimm14_operand")]
	     UNSPECV_CSRXCHG))
   (clobber (mem:BLK (scratch)))]
  ""
  "csrxchg\t%0,%2,%3"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_iocsrrd_<size>"
  [(set (match_operand:QHWD 0 "register_operand" "=r")
	(unspec_volatile:QHWD [(match_operand:SI 1 "register_operand" "r")]
			      UNSPECV_IOCSRRD))
   (clobber (mem:BLK (scratch)))]
  ""
  "iocsrrd.<size>\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_iocsrwr_<size>"
  [(unspec_volatile:QHWD [(match_operand:QHWD 0 "register_operand" "r")
			  (match_operand:SI 1 "register_operand" "r")]
			  UNSPECV_IOCSRWR)
   (clobber (mem:BLK (scratch)))]
  ""
  "iocsrwr.<size>\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_cacop_<d>"
  [(unspec_volatile:X [(match_operand 0 "const_uimm5_operand")
		       (match_operand:X 1 "register_operand" "r")
		       (match_operand 2 "const_imm12_operand")]
		       UNSPECV_CACOP)
   (clobber (mem:BLK (scratch)))]
  ""
  "cacop\t%0,%1,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_lddir_<d>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")
		       (match_operand:X 1 "register_operand" "r")
		       (match_operand 2 "const_uimm5_operand")]
		       UNSPECV_LDDIR)
   (clobber (mem:BLK (scratch)))]
  ""
  "lddir\t%0,%1,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_ldpte_<d>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")
		       (match_operand 1 "const_uimm5_operand")]
		       UNSPECV_LDPTE)
   (clobber (mem:BLK (scratch)))]
  ""
  "ldpte\t%0,%1"
  [(set_attr "type" "load")
   (set_attr "mode" "<MODE>")])


;; Block moves, see loongarch.c for more details.
;; Argument 0 is the destination.
;; Argument 1 is the source.
;; Argument 2 is the length.
;; Argument 3 is the alignment.

(define_expand "cpymemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  ""
{
  if (TARGET_DO_OPTIMIZE_BLOCK_MOVE_P
      && loongarch_expand_block_move (operands[0], operands[1],
				      operands[2], operands[3]))
    DONE;
  else
    FAIL;
})

;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

(define_insn "*<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(any_shift:GPR (match_operand:GPR 1 "register_operand" "r")
		       (match_operand:SI 2 "arith_operand" "rI")))]
  ""
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (<MODE>mode) - 1));

  return "<insn>%i2.<d>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

(define_insn "<optab>si3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	   (any_shift:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "arith_operand" "rI"))))]
  "TARGET_64BIT"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "<insn>%i2.w\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "*rotr<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "r,r")
		      (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "rotr%i2.<d>\t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "mode" "<MODE>")])

(define_insn "rotrsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	  (rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		       (match_operand:SI 2 "arith_operand" "r,I"))))]
  "TARGET_64BIT"
  "rotr%i2.w\t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "mode" "SI")])

;; Expand left rotate to right rotate.
(define_expand "rotl<mode>3"
  [(set (match_dup 3)
	(neg:SI (match_operand:SI 2 "register_operand")))
   (set (match_operand:GPR 0 "register_operand")
	(rotatert:GPR (match_operand:GPR 1 "register_operand")
		      (match_dup 3)))]
  ""
  {
    operands[3] = gen_reg_rtx (SImode);

    if (TARGET_64BIT && <MODE>mode == SImode)
      {
	rtx t = gen_reg_rtx (DImode);

	emit_insn (gen_negsi2 (operands[3], operands[2]));
	emit_insn (gen_rotrsi3_extend (t, operands[1], operands[3]));
	t = gen_lowpart (SImode, t);
	SUBREG_PROMOTED_VAR_P (t) = 1;
	SUBREG_PROMOTED_SET (t, SRP_SIGNED);
	emit_move_insn (operands[0], t);
	DONE;
      }
  });

;; The following templates were added to generate "bstrpick.d + alsl.d"
;; instruction pairs.
;; It is required that the values of const_immalsl_operand and
;; immediate_operand must have the following correspondence:
;;
;; (immediate_operand >> const_immalsl_operand) == 0xffffffff

(define_insn "zero_extend_ashift"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			   (match_operand 2 "const_immalsl_operand" ""))
		(match_operand 3 "immediate_operand" "")))]
  "TARGET_64BIT
   && ((INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff)"
  "bstrpick.d\t%0,%1,31,0\n\talsl.d\t%0,%0,$r0,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "2")])

(define_insn "bstrpick_alsl_paired"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI
	  (and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			     (match_operand 2 "const_immalsl_operand" ""))
		  (match_operand 3 "immediate_operand" ""))
	  (match_operand:DI 4 "register_operand" "r")))]
  "TARGET_64BIT
   && ((INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff)"
  "bstrpick.d\t%0,%1,31,0\n\talsl.d\t%0,%0,%4,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "2")])

(define_insn "alsl<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(plus:GPR (ashift:GPR (match_operand:GPR 1 "register_operand" "r")
			      (match_operand 2 "const_immalsl_operand" ""))
		  (match_operand:GPR 3 "register_operand" "r")))]
  ""
  "alsl.<d>\t%0,%1,%3,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "*alslsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	  (plus:SI
	    (ashift:SI (match_operand:SI 1 "register_operand" "r")
		       (match_operand 2 "const_immalsl_operand" ""))
	    (match_operand:SI 3 "register_operand" "r"))))]
  "TARGET_64BIT"
  "alsl.w<u>\t%0,%1,%3,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])



;; Reverse the order of bytes of operand 1 and store the result in operand 0.

(define_insn "revb_2h"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotatert:SI (bswap:SI (match_operand:SI 1 "register_operand" "r"))
		     (const_int 16)))]
  ""
  "revb.2h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "revb_2h_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (rotatert:SI
	    (bswap:SI (match_operand:SI 1 "register_operand" "r"))
	    (const_int 16))))]
  "TARGET_64BIT"
  "revb.2h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "revb.2h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "revb_2w"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(rotatert:DI (bswap:DI (match_operand:DI 1 "register_operand" "r"))
		     (const_int 32)))]
  "TARGET_64BIT"
  "revb.2w\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "*bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_64BIT"
  "revb.2w\t%0,%1"
  [(set_attr "type" "shift")])

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  if (!TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (SImode);
      emit_insn (gen_revb_2h (t, operands[1]));
      emit_insn (gen_rotrsi3 (operands[0], t, GEN_INT (16)));
      DONE;
    }
})

(define_insn "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(bswap:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_64BIT"
  "revb.d\t%0,%1"
  [(set_attr "type" "shift")])


;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches

(define_insn "*branch_fp_FCCmode"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "equality_operator"
	      [(match_operand:FCC 2 "register_operand" "z")
		(const_int 0)])
	  (label_ref (match_operand 0 "" ""))
	(pc)))]
  "TARGET_HARD_FLOAT"
{
  return loongarch_output_conditional_branch (insn, operands,
					      LARCH_BRANCH ("b%F1", "%Z2%0"),
					      LARCH_BRANCH ("b%W1", "%Z2%0"));
}
  [(set_attr "type" "branch")])

(define_insn "*branch_fp_inverted_FCCmode"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "equality_operator"
	    [(match_operand:FCC 2 "register_operand" "z")
	    (const_int 0)])
	  (pc)
	  (label_ref (match_operand 0 "" ""))))]
  "TARGET_HARD_FLOAT"
{
  return loongarch_output_conditional_branch (insn, operands,
					      LARCH_BRANCH ("b%W1", "%Z2%0"),
					      LARCH_BRANCH ("b%F1", "%Z2%0"));
}
  [(set_attr "type" "branch")])

;; Conditional branches on ordered comparisons with zero.

(define_insn "*branch_order<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:X 2 "register_operand" "r,r")
			  (match_operand:X 3 "reg_or_0_operand" "J,r")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return loongarch_output_order_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")])

(define_insn "*branch_order<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:X 2 "register_operand" "r,r")
			  (match_operand:X 3 "reg_or_0_operand" "J,r")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return loongarch_output_order_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")])

;; Conditional branch on equality comparison.

(define_insn "branch_equality<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:X 2 "register_operand" "r")
			  (match_operand:X 3 "reg_or_0_operand" "rJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return loongarch_output_equal_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")])


(define_insn "*branch_equality<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:X 2 "register_operand" "r")
			  (match_operand:X 3 "reg_or_0_operand" "rJ")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return loongarch_output_equal_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")])


;; Branches operate on GRLEN-sized quantities, but for LoongArch64 we accept
;; QImode values so we can force zero-extension.
(define_mode_iterator BR [(QI "TARGET_64BIT") SI (DI "TARGET_64BIT")])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
			[(match_operand:BR 1 "register_operand")
			 (match_operand:BR 2 "nonmemory_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  loongarch_expand_conditional_branch (operands);
  DONE;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
			[(match_operand:ANYF 1 "register_operand")
			 (match_operand:ANYF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  loongarch_expand_conditional_branch (operands);
  DONE;
})

;; Used to implement built-in functions.
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])


;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

;; Destination is always set in SI mode.

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "loongarch_cstore_operator"
	 [(match_operand:GPR 2 "register_operand")
	  (match_operand:GPR 3 "nonmemory_operand")]))]
  ""
{
  loongarch_expand_scc (operands);
  DONE;
})

(define_insn "*seq_zero_<X:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(eq:GPR (match_operand:X 1 "register_operand" "r")
		 (const_int 0)))]
  ""
  "sltui\t%0,%1,1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])


(define_insn "*sne_zero_<X:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(ne:GPR (match_operand:X 1 "register_operand" "r")
		 (const_int 0)))]
  ""
  "sltu\t%0,%.,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sgt<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(any_gt:GPR (match_operand:X 1 "register_operand" "r")
		     (match_operand:X 2 "reg_or_0_operand" "rJ")))]
  ""
  "slt<u>\t%0,%z2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*slt<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(any_lt:GPR (match_operand:X 1 "register_operand" "r")
		     (match_operand:X 2 "arith_operand" "rI")))]
  ""
  "slt<u>%i2\t%0,%1,%2";
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*sle<u>_<X:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(any_le:GPR (match_operand:X 1 "register_operand" "r")
		    (match_operand:X 2 "sle_operand" "")))]
  ""
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>i\t%0,%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])


;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................

(define_insn "s<code>_<ANYF:mode>_using_FCCmode"
  [(set (match_operand:FCC 0 "register_operand" "=z")
	(fcond:FCC (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fcmp.<fcond>.<fmt>\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FCC")])


;;
;;  ....................
;;
;;	UNCONDITIONAL BRANCHES
;;
;;  ....................

;; Unconditional branches.

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0)))])

(define_insn "*jump_absolute"
  [(set (pc)
	(label_ref (match_operand 0)))]
  "!flag_pic"
{
  return "b\t%l0";
}
  [(set_attr "type" "branch")])

(define_insn "*jump_pic"
  [(set (pc)
	(label_ref (match_operand 0)))]
  "flag_pic"
{
  return "b\t%0";
}
  [(set_attr "type" "branch")])

;; Micro-architecture unconditionally treats a "jr $ra" as "return from subroutine",
;; non-returning indirect jumps through $ra would interfere with both subroutine
;; return prediction and the more general indirect branch prediction.

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  operands[0] = force_reg (Pmode, operands[0]);
  emit_jump_insn (gen_indirect_jump (Pmode, operands[0]));
  DONE;
})

(define_insn "@indirect_jump<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "e"))]
  ""
  "jr\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand"))
   (use (label_ref (match_operand 1 "")))]
  ""
{
  if (flag_pic)
    operands[0] = expand_simple_binop (Pmode, PLUS, operands[0],
				       gen_rtx_LABEL_REF (Pmode,
							  operands[1]),
				       NULL_RTX, 0, OPTAB_DIRECT);
  emit_jump_insn (gen_tablejump (Pmode, operands[0], operands[1]));
  DONE;
})

(define_mode_attr mode_size [(DI "8") (SI "4")])

(define_insn "@tablejump<mode>"
  [(set (pc)
	(match_operand:P 0 "register_operand" "e"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  {
    return TARGET_ANNOTATE_TABLEJUMP
      ? "1:jr\t%0\n\t"
	".pushsection\t.discard.tablejump_annotate\n\t"
	"\t.<mode_size>byte\t1b\n\t"
	"\t.<mode_size>byte\t%1\n\t"
	".popsection"
      : "jr\t%0";
  }
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
  loongarch_expand_prologue ();
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

(define_insn "@probe_stack_range<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "0")
			    (match_operand:P 2 "register_operand" "r")
			    (match_operand:P 3 "register_operand" "r")]
			    UNSPECV_PROBE_STACK_RANGE))]
  ""
{
  return loongarch_output_probe_stack_range (operands[0],
					     operands[2],
					     operands[3]);
}
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  loongarch_expand_epilogue (NORMAL_RETURN);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  loongarch_expand_epilogue (SIBCALL_RETURN);
  DONE;
})

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better.

(define_expand "return"
  [(simple_return)]
  "loongarch_can_use_return_insn ()"
  { })

(define_expand "simple_return"
  [(simple_return)]
  ""
  { })

(define_insn "*<optab>"
  [(any_return)]
  ""
{
  operands[0] = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
  return "jr\t%0";
}
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; Normal return.

(define_insn "<optab>_internal"
  [(any_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "jr\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; Exception return.
(define_insn "loongarch_ertn"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_ERTN)]
  ""
  "ertn"
  [(set_attr "type" "trap")
   (set_attr "mode" "none")])

;; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  if (GET_MODE (operands[0]) != word_mode)
    operands[0] = convert_to_mode (word_mode, operands[0], 0);
  if (TARGET_64BIT)
    emit_insn (gen_eh_set_ra_di (operands[0]));
  else
    emit_insn (gen_eh_set_ra_si (operands[0]));

  emit_jump_insn (gen_eh_return_internal ());
  emit_barrier ();
  DONE;
})

(define_insn_and_split "eh_return_internal"
  [(eh_return)]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  loongarch_expand_epilogue (EXCEPTION_RETURN);
  DONE;
})

;; Clobber the return address on the stack.  We can't expand this
;; until we know where it will be put in the stack frame.

(define_insn "eh_set_ra_si"
  [(unspec [(match_operand:SI 0 "register_operand" "r")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  "! TARGET_64BIT"
  "#")

(define_insn "eh_set_ra_di"
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
  loongarch_set_return_address (operands[0], operands[1]);
  DONE;
})



;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

;; Sibling calls.  All these patterns use jump instructions.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[0], 0));

  if (GET_CODE (target) == LO_SUM)
    emit_call_insn (gen_sibcall_internal_1 (Pmode, XEXP (target, 0),
					    XEXP (target, 1),
					    operands[1]));
  else
    {
      rtx call = emit_call_insn (gen_sibcall_internal (target, operands[1]));

      if (TARGET_CMODEL_MEDIUM && !REG_P (target))
	clobber_reg (&CALL_INSN_FUNCTION_USAGE (call),
		     gen_rtx_REG (Pmode, T0_REGNUM));
    }
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,c,b"))
	 (match_operand 1 "" ""))]
  "SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
    {
    case 0:
      return "jr\t%0";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%0)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%0";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%0)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%%plt(%0)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@sibcall_internal_1<mode>"
  [(call (mem:P (lo_sum:P (match_operand:P 0 "register_operand" "j")
			  (match_operand:P 1 "symbolic_operand" "")))
	 (match_operand 2 "" ""))]
  "SIBLING_CALL_P (insn) && TARGET_CMODEL_MEDIUM"
  "jirl\t$r0,%0,%%pc_lo12(%1)"
  [(set_attr "jirl" "indirect")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[1], 0));

 /*  Handle return values created by loongarch_pass_fpr_pair.  */
  if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 2)
    {
      rtx arg1 = XEXP (XVECEXP (operands[0],0, 0), 0);
      rtx arg2 = XEXP (XVECEXP (operands[0],0, 1), 0);

      if (GET_CODE (target) == LO_SUM)
	emit_call_insn (gen_sibcall_value_multiple_internal_1 (Pmode, arg1,
							   XEXP (target, 0),
							   XEXP (target, 1),
							   operands[2],
							   arg2));
      else
	{
	  rtx call
	    = emit_call_insn (gen_sibcall_value_multiple_internal (arg1,
								   target,
								   operands[2],
								   arg2));

	  if (TARGET_CMODEL_MEDIUM && !REG_P (target))
	    clobber_reg (&CALL_INSN_FUNCTION_USAGE (call),
			gen_rtx_REG (Pmode, T0_REGNUM));
	}
    }
   else
    {
      /*  Handle return values created by loongarch_return_fpr_single.  */
      if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 1)
	operands[0] = XEXP (XVECEXP (operands[0], 0, 0), 0);

      if (GET_CODE (target) == LO_SUM)
	emit_call_insn (gen_sibcall_value_internal_1 (Pmode, operands[0],
						  XEXP (target, 0),
						  XEXP (target, 1),
						  operands[2]));
      else
	{
	  rtx call = emit_call_insn (gen_sibcall_value_internal (operands[0],
								 target,
								 operands[2]));

	  if (TARGET_CMODEL_MEDIUM && !REG_P (target))
	    clobber_reg (&CALL_INSN_FUNCTION_USAGE (call),
			gen_rtx_REG (Pmode, T0_REGNUM));
	}
    }
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "j,c,b"))
	      (match_operand 2 "" "")))]
  "SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
    {
    case 0:
      return "jr\t%1";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%1)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%1";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%1)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%%plt(%1)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@sibcall_value_internal_1<mode>"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:P (lo_sum:P (match_operand:P 1 "register_operand" "j")
			       (match_operand:P 2 "symbolic_operand" "")))
	      (match_operand 3 "" "")))]
  "SIBLING_CALL_P (insn) && TARGET_CMODEL_MEDIUM"
  "jirl\t$r0,%1,%%pc_lo12(%2)"
  [(set_attr "jirl" "indirect")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "j,c,b"))
	      (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
    {
    case 0:
      return "jr\t%1";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%1)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%1";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r12,%%call36(%1)\n\tjirl\t$r0,$r12,0";
      else
	return "b\t%%plt(%1)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@sibcall_value_multiple_internal_1<mode>"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:P (unspec:P [(match_operand:P 1 "register_operand" "j")
			        (match_operand:P 2 "symbolic_operand" "")]
		      UNSPEC_SIBCALL_VALUE_MULTIPLE_INTERNAL_1))
	      (match_operand 3 "" "")))
   (set (match_operand 4 "register_operand" "")
	(call (mem:P (unspec:P [(match_dup 1)
			        (match_dup 2)]
		      UNSPEC_SIBCALL_VALUE_MULTIPLE_INTERNAL_1))
	      (match_dup 3)))]
  "SIBLING_CALL_P (insn) && TARGET_CMODEL_MEDIUM"
  "jirl\t$r0,%1,%%pc_lo12(%2)"
  [(set_attr "jirl" "indirect")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[0], 0));

  if (GET_CODE (target) == LO_SUM)
    emit_call_insn (gen_call_internal_1 (Pmode, XEXP (target, 0),
					 XEXP (target, 1), operands[1]));
  else
    emit_call_insn (gen_call_internal (target, operands[1]));
  DONE;
})

(define_insn "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "e,c,b"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "jirl\t$r1,%0,0";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%0)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%0";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%0)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%%plt(%0)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@call_internal_1<mode>"
  [(call (mem:P (lo_sum:P (match_operand:P 0 "register_operand" "j")
			  (match_operand:P 1 "symbolic_operand" "")))
	 (match_operand 2 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  "TARGET_CMODEL_MEDIUM"
  "jirl\t$r1,%0,%%pc_lo12(%1)"
  [(set_attr "jirl" "indirect")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[1], 0));
  /* Handle return values created by loongarch_pass_fpr_pair.  */
  if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 2)
    {
      rtx arg1 = XEXP (XVECEXP (operands[0], 0, 0), 0);
      rtx arg2 = XEXP (XVECEXP (operands[0], 0, 1), 0);

      if (GET_CODE (target) == LO_SUM)
	emit_call_insn (gen_call_value_multiple_internal_1 (Pmode, arg1,
							    XEXP (target, 0),
							    XEXP (target, 1),
							    operands[2], arg2));
      else
	emit_call_insn (gen_call_value_multiple_internal (arg1, target,
							operands[2], arg2));
    }
   else
    {
      /* Handle return values created by loongarch_return_fpr_single.  */
      if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 1)
	    operands[0] = XEXP (XVECEXP (operands[0], 0, 0), 0);

      if (GET_CODE (target) == LO_SUM)
	emit_call_insn (gen_call_value_internal_1 (Pmode, operands[0],
						   XEXP (target, 0),
						   XEXP (target, 1),
						   operands[2]));
      else
	emit_call_insn (gen_call_value_internal (operands[0], target,
					       operands[2]));
    }
  DONE;
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "e,c,b"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "jirl\t$r1,%1,0";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%1)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%1";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%1)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%%plt(%1)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@call_value_internal_1<mode>"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:P (lo_sum:P (match_operand:P 1 "register_operand" "j")
			       (match_operand:P 2 "symbolic_operand" "")))
	      (match_operand 3 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  "TARGET_CMODEL_MEDIUM"
  "jirl\t$r1,%1,%%pc_lo12(%2)"
  [(set_attr "jirl" "indirect")])

(define_insn "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "e,c,b"))
	      (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "jirl\t$r1,%1,0";
    case 1:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%1)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%1";
    case 2:
      if (TARGET_CMODEL_MEDIUM)
	return "pcaddu18i\t$r1,%%call36(%1)\n\tjirl\t$r1,$r1,0";
      else
	return "bl\t%%plt(%1)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct")])

(define_insn "@call_value_multiple_internal_1<mode>"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:P (unspec:P [(match_operand:P 1 "register_operand" "j")
			        (match_operand:P 2 "symbolic_operand" "")]
		      UNSPEC_CALL_VALUE_MULTIPLE_INTERNAL_1))
	      (match_operand 3 "" "")))
   (set (match_operand 4 "register_operand" "")
	(call (mem:P (unspec:P [(match_dup 1)
			        (match_dup 2)]
		      UNSPEC_CALL_VALUE_MULTIPLE_INTERNAL_1))
	      (match_dup 3)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  "TARGET_CMODEL_MEDIUM"
  "jirl\t$r1,%1,%%pc_lo12(%2)"
  [(set_attr "jirl" "indirect")])


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
      loongarch_emit_move (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

;;
;;  ....................
;;
;;	MISC.
;;
;;  ....................
;;

(define_insn "prefetch"
  [(prefetch (match_operand 0 "address_operand" "ZD")
	     (match_operand 1 "const_int_operand" "n")
	     (match_operand 2 "const_int_operand" "n"))]
  ""
{
  switch (INTVAL (operands[1]))
  {
    case 0:
    case 2: return "preld\t0,%a0";
    case 1: return "preld\t8,%a0";
    default: gcc_unreachable ();
  }
})

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "nop")
   (set_attr "mode" "none")])

;; __builtin_loongarch_movfcsr2gr: move the FCSR into operand 0.
(define_insn "loongarch_movfcsr2gr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand 1 "const_uimm5_operand")]
			     UNSPECV_MOVFCSR2GR))]
  "TARGET_HARD_FLOAT"
  "movfcsr2gr\t%0,$r%1")

;; __builtin_loongarch_movgr2fcsr: move operand 0 into the FCSR.
(define_insn "loongarch_movgr2fcsr"
  [(unspec_volatile [(match_operand 0 "const_uimm5_operand")
		     (match_operand:SI 1 "register_operand" "r")]
		     UNSPECV_MOVGR2FCSR)]
  "TARGET_HARD_FLOAT"
  "movgr2fcsr\t$r%0,%1")

(define_insn "fclass_<fmt>"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(unspec:SI [(match_operand:ANYF 1 "register_operand" "f")]
		   UNSPEC_FCLASS))]
  "TARGET_HARD_FLOAT"
  "fclass.<fmt>\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_int_iterator FCLASS_MASK [68 136 952])
(define_int_attr fclass_optab
  [(68	"isinf")
   (136	"isnormal")
   (952	"isfinite")])

(define_expand "<FCLASS_MASK:fclass_optab><ANYF:mode>2"
  [(match_operand:SI   0 "register_operand" "=r")
   (match_operand:ANYF 1 "register_operand" " f")
   (const_int FCLASS_MASK)]
  "TARGET_HARD_FLOAT"
  {
    rtx ft0 = gen_reg_rtx (SImode);
    rtx t0 = gen_reg_rtx (word_mode);
    rtx mask = GEN_INT (<FCLASS_MASK>);

    emit_insn (gen_fclass_<ANYF:fmt> (ft0, operands[1]));

    if (TARGET_64BIT)
      emit_insn (gen_extend_insn (t0, ft0, DImode, SImode, 0));
    else
      emit_move_insn (t0, ft0);

    emit_move_insn (t0, gen_rtx_AND (word_mode, t0, mask));
    emit_move_insn (t0, gen_rtx_NE (word_mode, t0, const0_rtx));

    if (TARGET_64BIT)
      {
	t0 = lowpart_subreg (SImode, t0, DImode);
	SUBREG_PROMOTED_VAR_P (t0) = 1;
	SUBREG_PROMOTED_SET (t0, SRP_SIGNED);
      }

    emit_move_insn (operands[0], t0);

    DONE;
  })

(define_insn "bytepick_w_<bytepick_imm>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
			     (const_int <bytepick_w_lshiftrt_amount>))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int bytepick_w_ashift_amount))))]
  ""
  "bytepick.w\t%0,%1,%2,<bytepick_imm>"
  [(set_attr "mode" "SI")])

(define_mode_attr bitsize [(QI "8") (HI "16")])
(define_mode_attr bytepick_imm [(QI "3") (HI "2")])
(define_mode_attr bytepick_w_ashift_amount [(QI "24") (HI "16")])

(define_insn "bytepick_w_<bytepick_imm>_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI
	  (ashift:DI
	    (sign_extend:DI
	      (subreg:SHORT (match_operand:DI 1 "register_operand" "r") 0))
	    (const_int <bytepick_w_ashift_amount>))
	  (zero_extract:DI (match_operand:DI 2 "register_operand" "r")
			   (const_int <bytepick_w_ashift_amount>)
			   (const_int <bitsize>))))]
  "TARGET_64BIT"
  "bytepick.w\t%0,%2,%1,<bytepick_imm>"
  [(set_attr "mode" "SI")])

(define_insn "bytepick_w_1_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI
	  (ashift:DI
	    (sign_extract:DI (match_operand:DI 1 "register_operand" "r")
                         (const_int 24)
                         (const_int 0))
        (const_int 8))
	  (zero_extract:DI (match_operand:DI 2 "register_operand" "r")
			   (const_int 8)
			   (const_int 24))))]
  "TARGET_64BIT"
  "bytepick.w\t%0,%2,%1,1"
  [(set_attr "mode" "SI")])

(define_insn "bytepick_d_<bytepick_imm>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (lshiftrt (match_operand:DI 1 "register_operand" "r")
			  (const_int <bytepick_d_lshiftrt_amount>))
		(ashift (match_operand:DI 2 "register_operand" "r")
			(const_int bytepick_d_ashift_amount))))]
  "TARGET_64BIT"
  "bytepick.d\t%0,%1,%2,<bytepick_imm>"
  [(set_attr "mode" "DI")])

(define_insn "bytepick_d_<bytepick_imm>_rev"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (ashift (match_operand:DI 1 "register_operand" "r")
			(const_int bytepick_d_ashift_amount))
		(lshiftrt (match_operand:DI 2 "register_operand" "r")
			  (const_int <bytepick_d_lshiftrt_amount>))))]
  "TARGET_64BIT"
  "bytepick.d\t%0,%2,%1,<bytepick_imm>"
  [(set_attr "mode" "DI")])

(define_insn "bitrev_4b"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")]
		    UNSPEC_BITREV_4B))]
  ""
  "bitrev.4b\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "SI")])

(define_insn "bitrev_8b"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")]
		    UNSPEC_BITREV_8B))]
  ""
  "bitrev.8b\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "DI")])

(define_insn "@rbit<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(bitreverse:GPR (match_operand:GPR 1 "register_operand" "r")))]
  ""
  "bitrev.<size>\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_insn "rbitsi_extended"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (bitreverse:SI (match_operand:SI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "bitrev.w\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "SI")])

;; If we don't care high bits, bitrev.4b can reverse bits of values in
;; QImode.
(define_insn "rbitqi"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(bitreverse:QI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "bitrev.4b\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "SI")])

;; For HImode it's a little complicated...
(define_expand "rbithi"
  [(match_operand:HI 0 "register_operand")
   (match_operand:HI 1 "register_operand")]
  ""
  {
    rtx t = gen_reg_rtx (word_mode);

    /* Oh, using paradoxical subreg.  I learnt the trick from RISC-V,
       hoping we won't be blown up altogether one day.  */
    emit_insn (gen_rbit(word_mode, t,
			gen_lowpart (word_mode, operands[1])));
    t = expand_simple_binop (word_mode, LSHIFTRT, t,
			     GEN_INT (GET_MODE_BITSIZE (word_mode) - 16),
			     NULL_RTX, false, OPTAB_DIRECT);

    t = gen_lowpart (HImode, t);
    SUBREG_PROMOTED_VAR_P (t) = 1;
    SUBREG_PROMOTED_SET (t, SRP_UNSIGNED);
    emit_move_insn (operands[0], t);

    DONE;
  })

(define_insn "@stack_tie<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:X 0 "register_operand" "r")
		     (match_operand:X 1 "register_operand" "r")]
		     UNSPEC_TIE))]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "type" "ghost")])

;; Named pattern for expanding thread pointer reference.
(define_expand "get_thread_pointer<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(reg:P TP_REGNUM))]
  "HAVE_AS_TLS"
  {})

(define_split
  [(match_operand 0 "small_data_pattern")]
  "reload_completed"
  [(match_dup 0)]
  { operands[0] = loongarch_rewrite_small_data (operands[0]); })


;; Match paired HI/SI/SF/DFmode load/stores.
(define_insn "*join2_load_store<JOIN_MODE:mode>"
  [(set (match_operand:JOIN_MODE 0 "nonimmediate_operand"
  "=&r,f,m,m,&r,ZC")
	(match_operand:JOIN_MODE 1 "nonimmediate_operand" "m,m,r,f,ZC,r"))
   (set (match_operand:JOIN_MODE 2 "nonimmediate_operand"
   "=r,f,m,m,r,ZC")
	(match_operand:JOIN_MODE 3 "nonimmediate_operand" "m,m,r,f,ZC,r"))]
  "reload_completed"
  {
    /* The load destination does not overlap the source.  */
    gcc_assert (!reg_overlap_mentioned_p (operands[0], operands[1]));
    output_asm_insn (loongarch_output_move (operands),
		     operands);
    output_asm_insn (loongarch_output_move (&operands[2]),
		     &operands[2]);
    return "";
  }
  [(set_attr "move_type"
  "load,fpload,store,fpstore,load,store")
   (set_attr "insn_count" "2,2,2,2,2,2")])

;; 2 HI/SI/SF/DF loads are bonded.
(define_peephole2
  [(set (match_operand:JOIN_MODE 0 "register_operand")
	(match_operand:JOIN_MODE 1 "non_volatile_mem_operand"))
   (set (match_operand:JOIN_MODE 2 "register_operand")
	(match_operand:JOIN_MODE 3 "non_volatile_mem_operand"))]
  "loongarch_load_store_bonding_p (operands, <JOIN_MODE:MODE>mode, true)"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (set (match_dup 2)
		   (match_dup 3))])]
  "")

;; 2 HI/SI/SF/DF stores are bonded.
(define_peephole2
  [(set (match_operand:JOIN_MODE 0 "memory_operand")
	(match_operand:JOIN_MODE 1 "register_operand"))
   (set (match_operand:JOIN_MODE 2 "memory_operand")
	(match_operand:JOIN_MODE 3 "register_operand"))]
  "loongarch_load_store_bonding_p (operands, <JOIN_MODE:MODE>mode, false)"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (set (match_dup 2)
		   (match_dup 3))])]
  "")

;; Match paired HImode loads.
(define_insn "*join2_loadhi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(any_extend:SI (match_operand:HI 1 "non_volatile_mem_operand" "m")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(any_extend:SI (match_operand:HI 3 "non_volatile_mem_operand" "m")))]
  "reload_completed"
  {
    /* The load destination does not overlap the source.  */
    gcc_assert (!reg_overlap_mentioned_p (operands[0], operands[1]));
    output_asm_insn ("ld.h<u>\t%0,%1", operands);
    output_asm_insn ("ld.h<u>\t%2,%3", operands);

    return "";
  }
  [(set_attr "move_type" "load")
   (set_attr "insn_count" "2")])


;; 2 HI loads are bonded.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
	(any_extend:SI (match_operand:HI 1 "non_volatile_mem_operand")))
   (set (match_operand:SI 2 "register_operand")
	(any_extend:SI (match_operand:HI 3 "non_volatile_mem_operand")))]
  "loongarch_load_store_bonding_p (operands, HImode, true)"
  [(parallel [(set (match_dup 0)
		   (any_extend:SI (match_dup 1)))
	      (set (match_dup 2)
		   (any_extend:SI (match_dup 3)))])]
  "")



(define_int_iterator CRC [UNSPEC_CRC UNSPEC_CRCC])
(define_int_attr crc [(UNSPEC_CRC "crc") (UNSPEC_CRCC "crcc")])

(define_insn "loongarch_<crc>_w_<size>_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:QHWD 1 "register_operand" "r")
		   (match_operand:SI 2 "register_operand" "r")]
		     CRC))]
  ""
  "<crc>.w.<size>.w\t%0,%1,%2"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_insn "loongarch_<crc>_w_<size>_w_extended"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (unspec:SI [(match_operand:QHWD 1 "register_operand" "r")
		      (match_operand:SI 2 "register_operand" "r")]
		     CRC)))]
  "TARGET_64BIT"
  "<crc>.w.<size>.w\t%0,%1,%2"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_expand "crc_rev<mode>si4"
  [(match_operand:SI	0 "register_operand")	; new_chksum
   (match_operand:SI	1 "register_operand")	; old_chksum
   (match_operand:SUBDI	2 "reg_or_0_operand")	; msg
   (match_operand	3 "const_int_operand")]	; poly
  ""
  {
    unsigned HOST_WIDE_INT poly = UINTVAL (operands[3]);
    rtx msg = operands[2];
    rtx (*crc_insn)(rtx, rtx, rtx) = nullptr;

    /* TODO: Review this when adding LA32 support.  If we're going to
       support CRC instructions on LA32 we'll need a "-mcrc" switch as
       they are optional on LA32.  */

    if (TARGET_64BIT)
      {
	if (poly == reflect_hwi (0xedb88320u, 32))
	  crc_insn = gen_loongarch_crc_w_<size>_w;
	else if (poly == reflect_hwi (0x82f63b78u, 32))
	  crc_insn = gen_loongarch_crcc_w_<size>_w;
      }

    if (crc_insn)
      {
	/* We cannot make crc_insn to accept const0_rtx easily:
	   it's not possible to figure out the mode of const0_rtx so we'd
	   have to separate both UNSPEC_CRC and UNSPEC_CRCC to 4 different
	   UNSPECs.  Instead just hack it around here.  */
	if (msg == const0_rtx)
	  msg = gen_rtx_REG (<MODE>mode, 0);

	emit_insn (crc_insn (operands[0], msg, operands[1]));
      }
    else
      {
	/* No CRC instruction is suitable, use the generic table-based
	   implementation but optimize bit reversion.  */
	auto rbit = [](rtx *r)
	  {
	    /* Well, this is ugly.  The problem is
	       expand_reversed_crc_table_based only accepts one helper
	       for reversing data elements and CRC states.  */
	    auto mode = GET_MODE (*r);
	    auto rbit = (mode == <MODE>mode ? gen_rbit<mode> : gen_rbitsi);
	    rtx out = gen_reg_rtx (mode);

	    emit_insn (rbit (out, *r));
	    *r = out;
	  };
	expand_reversed_crc_table_based (operands[0], operands[1],
					 msg, operands[3], <MODE>mode,
					 rbit);
      }
    DONE;
  })

(define_insn_and_split "*crc_combine"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI
	  [(reg:SUBDI 0)
	   (subreg:SI
	     (xor:DI
	       (match_operand:DI 1 "register_operand" "r,r")
	       ; Our LOAD_EXTEND_OP makes this same as sign_extend
	       ; if SUBDI is SI, or zero_extend if SUBDI is QI or HI.
	       ; For the former the high bits in rk are ignored by
	       ; crc.w.w.w anyway, for the latter the zero extension is
	       ; necessary for the correctness of this transformation.
	       (subreg:DI
		 (match_operand:SUBDI 2 "memory_operand" "m,k") 0)) 0)]
	  CRC))]
  "TARGET_64BIT && loongarch_pre_reload_split ()"
  "#"
  "&& true"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0)
	(unspec:SI [(match_dup 3) (subreg:SI (match_dup 1) 0)] CRC))]
  {
    operands[3] = gen_reg_rtx (<MODE>mode);
  })

;; With normal or medium code models, if the only use of a pc-relative
;; address is for loading or storing a value, then relying on linker
;; relaxation is not better than emitting the machine instruction directly.
;; Even if the la.local pseudo op can be relaxed, we get:
;;
;;     pcaddi     $t0, %pcrel_20(x)
;;     ld.d       $t0, $t0, 0
;;
;; There are still two instructions, same as using the machine instructions
;; and explicit relocs:
;;
;;     pcalau12i  $t0, %pc_hi20(x)
;;     ld.d       $t0, $t0, %pc_lo12(x)
;;
;; And if the pseudo op cannot be relaxed, we'll get a worse result (with
;; 3 instructions).
(define_insn_and_rewrite "simple_load<mode>"
  [(set (match_operand:LD_AT_LEAST_32_BIT 0 "register_operand" "=r,f")
	(match_operand:LD_AT_LEAST_32_BIT 1 "mem_simple_ldst_operand" ""))]
  "loongarch_pre_reload_split ()
   && la_opt_explicit_relocs == EXPLICIT_RELOCS_AUTO
   && (TARGET_CMODEL_NORMAL || TARGET_CMODEL_MEDIUM)"
  "#"
  "&& true"
  {
    operands[1] = loongarch_rewrite_mem_for_simple_ldst (operands[1]);
  })

(define_insn_and_rewrite "simple_load_<su>ext<SUBDI:mode><GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(any_extend:GPR
	  (match_operand:SUBDI 1 "mem_simple_ldst_operand" "")))]
  "loongarch_pre_reload_split ()
   && la_opt_explicit_relocs == EXPLICIT_RELOCS_AUTO
   && (TARGET_CMODEL_NORMAL || TARGET_CMODEL_MEDIUM)"
  "#"
  "&& true"
  {
    operands[1] = loongarch_rewrite_mem_for_simple_ldst (operands[1]);
  })

(define_insn_and_rewrite "simple_store<mode>"
  [(set (match_operand:ST_ANY 0 "mem_simple_ldst_operand" "")
	(match_operand:ST_ANY 1 "reg_or_0_operand" "r,f"))]
  "loongarch_pre_reload_split ()
   && la_opt_explicit_relocs == EXPLICIT_RELOCS_AUTO
   && (TARGET_CMODEL_NORMAL || TARGET_CMODEL_MEDIUM)"
  "#"
  "&& true"
  {
    operands[0] = loongarch_rewrite_mem_for_simple_ldst (operands[0]);
  })

;; Synchronization instructions.

(include "sync.md")

(include "generic.md")
(include "la464.md")

; The LoongArch SIMD Instructions.
(include "simd.md")

(define_c_enum "unspec" [
  UNSPEC_ADDRESS_FIRST
])
