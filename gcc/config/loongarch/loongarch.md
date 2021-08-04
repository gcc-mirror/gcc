;;  Loongarch.md	     Machine Description for LoongArch based processors
;;  Copyright (C) 2021 Free Software Foundation, Inc.

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

(define_enum "processor" [
  loongarch
  loongarch64
  gs464v
])

(define_c_enum "unspec" [
  ;; Integer operations that are too cumbersome to describe directly.
  UNSPEC_REVB_2H
  UNSPEC_REVB_4H
  UNSPEC_REVH_D

  ;; Floating-point moves.
  UNSPEC_LOAD_LOW
  UNSPEC_LOAD_HIGH
  UNSPEC_STORE_WORD
  UNSPEC_MOVGR2FRH
  UNSPEC_MOVFRH2GR

  ;; Floating-point environment.
  UNSPEC_MOVFCSR2GR
  UNSPEC_MOVGR2FCSR

  ;; GP manipulation.
  UNSPEC_EH_RETURN

  ;;
  UNSPEC_FRINT
  UNSPEC_FCLASS
  UNSPEC_BYTEPICK_W
  UNSPEC_BYTEPICK_D
  UNSPEC_BITREV_4B
  UNSPEC_BITREV_8B

  ;; Symbolic accesses.
  UNSPEC_LOAD_CALL

  ;; Blockage and synchronisation.
  UNSPEC_BLOCKAGE
  UNSPEC_DBAR
  UNSPEC_IBAR

  ;; CPUCFG
  UNSPEC_CPUCFG
  UNSPEC_ASRTLE_D
  UNSPEC_ASRTGT_D

  UNSPEC_CSRRD
  UNSPEC_CSRWR
  UNSPEC_CSRXCHG
  UNSPEC_IOCSRRD
  UNSPEC_IOCSRWR

  ;; CACOP
  UNSPEC_CACOP

  ;; PTE
  UNSPEC_LDDIR
  UNSPEC_LDPTE

  ;; Interrupt handling.
  UNSPEC_ERTN

  ;; Used in a call expression in place of args_size.  It's present for PIC
  ;; indirect calls where it contains args_size and the function symbol.
  UNSPEC_CALL_ATTR

  ;; Stack checking.
  UNSPEC_PROBE_STACK_RANGE

  ;; TLS
  UNSPEC_TLS_GD
  UNSPEC_TLS_LD
  UNSPEC_TLS_LE
  UNSPEC_TLS_IE

  ;; load immediate
  UNSPEC_LU52I_D

  ;; FIXME: Stack tie
  UNSPEC_TIE

  ;; Register save and restore.
  UNSPECV_GPR_SAVE
  UNSPECV_GPR_RESTORE

  ;; CRC
  UNSPEC_CRC
  UNSPEC_CRCC
])

;; FIXME
(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (T0_REGNUM			12)
   (T1_REGNUM			13)
   (S0_REGNUM			23)

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

(define_attr "alu_type" "unknown,add,sub,not,nor,and,or,xor"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,OI,SF,DF,TF,FCC"
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

;; True if the main data type is eight times of the size of a word.
(define_attr "oword_mode" "no,yes"
  (cond [(and (eq_attr "mode" "OI")
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
;; mgtf		move generate register to float register
;; mftg		move float register to generate register
;; const	load constant
;; arith	integer arithmetic instructions
;; logical      integer logical instructions
;; shift	integer shift instructions
;; slt		set less than instructions
;; signext      sign extend instructions
;; clz		the clz and clo instructions
;; trap		trap if instructions
;; imul		integer multiply
;; idiv	integer divide
;; move		integer register move ({,D}ADD{,U} with rt = 0)
;; fmove	floating point register move
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; frdiv	floating point reciprocal divide
;; fabs		floating point absolute value
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; frsqrt       floating point reciprocal square root
;; multi	multiword sequence (or user asm statements)
;; atomic	atomic memory update instruction
;; syncloop	memory atomic operation implemented as a sync loop
;; nop		no operation
;; ghost	an instruction that produces no real code
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,fpidxload,store,fpstore,fpidxstore,
   prefetch,prefetchx,condmove,mgtf,mftg,const,arith,logical,
   shift,slt,signext,clz,trap,imul,idiv,move,
   fmove,fadd,fmul,fmadd,fdiv,frdiv,fabs,fneg,fcmp,fcvt,fsqrt,
   frsqrt,accext,accmod,multi,atomic,syncloop,nop,ghost"
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

	 ;; These types of move are split for octaword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "oword_mode" "yes"))
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
;; I2S          integer to float single (SI/DI to SF)
;; I2D          integer to float double (SI/DI to DF)
;; S2I          float to integer (SF to SI/DI)
;; D2I          float to integer (DF to SI/DI)
;; D2S          double to float single
;; S2D          float single to double

(define_attr "cnv_mode" "unknown,I2S,I2D,S2I,D2I,D2S,S2D"
  (const_string "unknown"))

(define_attr "compression" "none,all"
  (const_string "none"))

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

	 ;; Check for Octaword moves that are decomposed into eight
	 ;; instructions.
	 (and (eq_attr "move_type" "mgtf,mftg,move")
	      (eq_attr "oword_mode" "yes"))
	 (const_int 8)

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
          ;; Branch instructions have a range of [-0x20000,0x1fffc].
          ;; If a branch is outside this range, we have a choice of two
          ;; sequences.
          ;;
          ;; For PIC, an out-of-range branch like:
          ;;
          ;;    bne     r1,r2,target
          ;;
          ;; becomes the equivalent of:
          ;;
          ;;    beq     r1,r2,1f
          ;;    la      rd,target
          ;;    jirl    $r0,rd,0
          ;; 1:
          ;;
          ;; The non-PIC case is similar except that we use a direct
          ;; jump instead of an la/jirl pair.  Since the target of this
          ;; jump is an absolute 28-bit bit address (the other bits
          ;; coming from the address of the delay slot) this form cannot
          ;; cross a 256MB boundary.  We could provide the option of
          ;; using la/jirl in this case too, but we do not do so at
          ;; present.
          ;;
          ;; from the shorten_branches reference address.
          (eq_attr "type" "branch")
          (cond [;; Any variant can handle the 17-bit range.
                 (and (le (minus (match_dup 0) (pc)) (const_int 65532))
                      (le (minus (pc) (match_dup 0)) (const_int 65534)))
                   (const_int 4)

                 ;; The non-PIC case: branch, and J.
                 (match_test "TARGET_ABSOLUTE_JUMPS")
                   (const_int 8)]

                 ;; Use MAX_PIC_BRANCH_LENGTH as a (gross) overestimate.
                 ;; loongarch_adjust_insn_length substitutes the correct length.
                 ;;
                 ;; Note that we can't simply use (symbol_ref ...) here
                 ;; because genattrtab needs to know the maximum length
                 ;; of an insn.
                 (const_int MAX_PIC_BRANCH_LENGTH))
		]
         (symbol_ref "get_attr_insn_count (insn) * 4")))

;; Attribute describing the processor.
(define_enum_attr "cpu" "processor"
  (const (symbol_ref "loongarch_tune")))

;; The type of hardware hazard associated with this instruction.
;; DELAY means that the next instruction cannot read the result
;; of this one.
(define_attr "hazard" "none,delay,forbidden_slot"
  (const_string "none"))

;; Can the instruction be put into a delay slot?
(define_attr "can_delay" "no,yes"
  (if_then_else (and (eq_attr "type" "!branch,call,jump")
		     (eq_attr "hazard" "none")
		     (match_test "get_attr_insn_count (insn) == 1"))
		(const_string "yes")
		(const_string "no")))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")
   (set_attr "can_delay" "no")])

;; This mode iterator allows 32-bit and 64-bit GPR patterns to be generated
;; from the same template.
(define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

;; A copy of GPR that can be used when a pattern has two independent
;; modes.
(define_mode_iterator GPR2 [SI (DI "TARGET_64BIT")])

;; Likewise, but for XLEN-sized quantities.
(define_mode_iterator X [(SI "!TARGET_64BIT") (DI "TARGET_64BIT")])

;; This mode iterator allows 16-bit and 32-bit GPR patterns and 32-bit 64-bit
;; FPR patterns to be generated from the same template.
(define_mode_iterator JOIN_MODE [HI
				 SI
				 (SF "TARGET_HARD_FLOAT")
				 (DF "TARGET_HARD_FLOAT
				      && TARGET_DOUBLE_FLOAT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64 [DI DF])

;; 128-bit modes for which we provide move patterns on 64-bit targets.
(define_mode_iterator MOVE128 [TI TF])

;; Iterator for sub-32-bit integer modes.
(define_mode_iterator SHORT [QI HI])

;; Likewise the 64-bit truncate-and-shift patterns.
(define_mode_iterator SUBDI [QI HI SI])

;; This mode iterator allows the QI HI SI and DI extension patterns to be
(define_mode_iterator QHWD [QI HI SI (DI "TARGET_64BIT")])

;; Iterator for hardware-supported floating-point modes.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT")
			    (DF "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT")])

;; A floating-point mode for which moves involving FPRs may need to be split.
(define_mode_iterator SPLITF
  [(DF "!TARGET_64BIT && TARGET_DOUBLE_FLOAT")
   (DI "!TARGET_64BIT && TARGET_DOUBLE_FLOAT")
   (TF "TARGET_64BIT && TARGET_FLOAT64")])

;; In GPR templates, a string like "mul.<d>" will expand to "mul" in the
;; 32-bit "mul.w" and "mul.d" in the 64-bit version.
(define_mode_attr d [(SI "w") (DI "d")])

;; This attribute gives the length suffix for a load or store instruction.
;; The same suffixes work for zero and sign extensions.
(define_mode_attr size [(QI "b") (HI "h") (SI "w") (DI "d")])
(define_mode_attr SIZE [(QI "B") (HI "H") (SI "W") (DI "D")])

;; This attributes gives the mode mask of a SHORT.
(define_mode_attr mask [(QI "0x00ff") (HI "0xffff")])

;; This attributes gives the size (bits) of a SHORT.
(define_mode_attr qi_hi [(QI "7") (HI "15")])

;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd")])

;; Similarly for LoongArch indexed FPR loads and stores.
(define_mode_attr floadx [(SF "fldx.s") (DF "fldx.d") (V2SF "fldx.d")])
(define_mode_attr fstorex [(SF "fstx.s") (DF "fstx.d") (V2SF "fstx.d")])

;; Similarly for LoongArch indexed GPR loads and stores.
(define_mode_attr loadx [(QI "ldx.b")
                           (HI "ldx.h")
                           (SI "ldx.w")
                           (DI "ldx.d")])
(define_mode_attr storex [(QI "stx.b")
                            (HI "stx.h")
                            (SI "stx.w")
                            (DI "stx.d")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode or vector mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF") (V2SF "SF")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (V2SF "SI")
			    (TF "DI")])

;; This attribute gives the integer prefix for some instructions templates.
(define_mode_attr p [(SI "") (DI "d")])

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

;; This code iterator allows all native floating-point comparisons to be
;; generated from the same template.
(define_code_iterator fcond [unordered uneq unlt unle eq lt le ordered ltgt ne ge gt unge ungt])

;; Equality operators.
(define_code_iterator equality_op [eq ne])

;; These code iterators allow the signed and unsigned scc operations to use
;; the same template.
(define_code_iterator any_gt [gt gtu])
(define_code_iterator any_ge [ge geu])
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
		     (le "") (leu "u")])

;; <U> is like <u> except uppercase.
(define_code_attr U [(sign_extend "") (zero_extend "U")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
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
			(ior "or")
			(xor "xor")
			(and "and")
			(plus "addu")
			(minus "subu")
			(div "div")
			(udiv "div")
			(mod "mod")
			(umod "mod")])

;; <fcond> is the c.cond.fmt condition associated with a particular code.
(define_code_attr fcond [(unordered "cun")
			 (uneq "cueq")
			 (unlt "cult")
			 (unle "cule")
			 (eq "ceq")
			 (lt "slt")
			 (le "sle")
			 (ordered "cor")
			 (ltgt "cne")
			 (ne "cune")
			 (ge "sge")
			 (gt "sgt")
			 (unge "cuge")
			 (ungt "cugt")])

;; The sel mnemonic to use depending on the condition test.
(define_code_attr sel [(eq "masknez") (ne "maskeqz")])
(define_code_attr selinv [(eq "maskeqz") (ne "masknez")])

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

(define_insn "add<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(plus:GPR (match_operand:GPR 1 "register_operand" "r,r")
		  (match_operand:GPR 2 "arith_operand" "r,Q")))]
  ""
  "add%i2.<d>\t%0,%1,%2";
  [(set_attr "alu_type" "add")
   (set_attr "compression" "*,*")
   (set_attr "mode" "<MODE>")])

(define_insn "*addsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" "r,r")
		      (match_operand:SI 2 "arith_operand" "r,Q"))))]
  "TARGET_64BIT"
  "add%i2.w\t%0,%1,%2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_extended2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	  (subreg:SI (plus:DI (match_operand:DI 1 "register_operand" "r,r")
			      (match_operand:DI 2 "arith_operand"    "r,Q"))
		     0)))]
  "TARGET_64BIT"
  "add%i2.w\t%0,%1,%2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")])


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

(define_insn "sub<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(minus:GPR (match_operand:GPR 1 "register_operand" "rJ")
		   (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "sub.<d>\t%0,%z1,%2"
  [(set_attr "alu_type" "sub")
   (set_attr "compression" "*")
   (set_attr "mode" "<MODE>")])

(define_insn "*subsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "register_operand" "rJ")
		      (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "sub.w\t%0,%z1,%2"
  [(set_attr "alu_type" "sub")
   (set_attr "mode" "DI")])

(define_insn "*subsi3_extended2"
  [(set (match_operand:DI                        0 "register_operand" "=r")
	(sign_extend:DI
	  (subreg:SI (minus:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			       (match_operand:DI 2 "register_operand" "r"))
		     0)))]
  "TARGET_64BIT"
  "sub.w\t%0,%z1,%2"
  [(set_attr "alu_type" "sub")
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
  "TARGET_HARD_FLOAT"
  "fmul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(mult:GPR (match_operand:GPR 1 "register_operand" "r")
		  (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "mul.<d>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")])

(define_insn "mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "mul.d\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_insn "*mulsi3_extended"
  [(set (match_operand:DI              0 "register_operand" "=r")
	(sign_extend:DI
	    (mult:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_64BIT"
  "mul.w\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "*mulsi3_extended2"
  [(set (match_operand:DI                       0 "register_operand" "=r")
	(sign_extend:DI
	  (subreg:SI (mult:DI (match_operand:DI 1 "register_operand" "r")
			      (match_operand:DI 2 "register_operand" "r"))
		     0)))]
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
  [(set (match_operand:TI                         0 "register_operand")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		 (any_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT"
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
  "TARGET_64BIT"
  "mulh.d<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI            0 "register_operand" "=r")
	(mult:DI (any_extend:DI
		   (match_operand:SI 1 "register_operand" " r"))
		 (any_extend:DI
		   (match_operand:SI 2 "register_operand" " r"))))]
  "!TARGET_64BIT"
{
  rtx temp = gen_reg_rtx (SImode);
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
  emit_insn (gen_<u>mulsi3_highpart (loongarch_subword (operands[0], true),
				     operands[1], operands[2]));
  emit_insn (gen_movsi (loongarch_subword (operands[0], false), temp));
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
  "!TARGET_64BIT"
  "mulh.w<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

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
  "TARGET_HARD_FLOAT"
{})

(define_insn "*div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fdiv.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fdiv")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

;; In 3A5000, the reciprocal operation is the same as the division operation.

(define_insn "*recip<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "frecip.<fmt>\t%0,%2"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

;; Integer division and modulus.

(define_insn "<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(any_div:GPR (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:GPR 2 "register_operand" "r")))]
  ""
  {
    return loongarch_output_division ("<insn>.<d><u>\t%0,%1,%2", operands);
  }
  [(set_attr "type" "idiv")
   (set_attr "mode" "<MODE>")])


;; Floating point multiply accumulate instructions.

;; a * b + c
(define_expand "fma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand")
	(fma:ANYF (match_operand:ANYF 1 "register_operand")
		  (match_operand:ANYF 2 "register_operand")
		  (match_operand:ANYF 3 "register_operand")))]
  "TARGET_HARD_FLOAT")

(define_insn "*fma<mode>4_madd4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; a * b - c
(define_insn "fms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
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
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
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
  "TARGET_HARD_FLOAT && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
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

;; -(a * b + c)
(define_insn "*fnms<mode>4"
  [(set (match_operand:ANYF         0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF 1 "register_operand" " f")
		(match_operand:ANYF 2 "register_operand" " f")
		(match_operand:ANYF 3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; -(a * b - c)
(define_insn "*fnma<mode>4"
  [(set (match_operand:ANYF                   0 "register_operand" "=f")
	(neg:ANYF
	    (fma:ANYF
		(match_operand:ANYF           1 "register_operand" " f")
		(match_operand:ANYF           2 "register_operand" " f")
		(neg:ANYF (match_operand:ANYF 3 "register_operand" " f")))))]
  "TARGET_HARD_FLOAT"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_insn "sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fsqrt.<fmt>\t%0,%1"
  [(set_attr "type" "fsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

(define_insn "*rsqrt<mode>a"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (sqrt:ANYF (match_operand:ANYF 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
  "frsqrt.<fmt>\t%0,%2"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

(define_insn "*rsqrt<mode>b"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
			     (match_operand:ANYF 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
  "frsqrt.<fmt>\t%0,%2"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set_attr "insn_count" "1")])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fabs.<fmt>\t%0,%1"
  [(set_attr "type" "fabs")
   (set_attr "mode" "<UNITMODE>")])

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
  "TARGET_HARD_FLOAT"
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
       (smin:ANYF (match_operand:ANYF 1 "register_operand" "f")
                  (match_operand:ANYF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
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
  "TARGET_HARD_FLOAT"
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
  "TARGET_HARD_FLOAT"
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
   (set_attr "mode"	"<MODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(not:GPR (match_operand:GPR 1 "register_operand" "r")))]
  ""
  "nor\t%0,%.,%1"
  [(set_attr "alu_type" "not")
   (set_attr "compression" "*")
   (set_attr "mode" "<MODE>")])

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
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
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(any_bitwise:GPR (match_operand:GPR 1 "register_operand" "r,r")
		 (match_operand:GPR 2 "uns_arith_operand" "r,K")))]
  ""
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "compression" "*,*")
   (set_attr "mode" "<MODE>")])

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

(define_insn "*iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ior:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "uns_arith_operand" "r,K")))]
  ""
  "or%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

(define_insn "*nor<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(and:GPR (not:GPR (match_operand:GPR 1 "register_operand" "r"))
		 (not:GPR (match_operand:GPR 2 "register_operand" "r"))))]
  ""
  "nor\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "andn<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (and:GPR
	 (not:GPR (match_operand:GPR 1 "register_operand" "r"))
	 (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "andn\t%0,%2,%1"
  [(set_attr "type" "logical")])

(define_insn "orn<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (ior:GPR
	 (not:GPR (match_operand:GPR 1 "register_operand" "r"))
	 (match_operand:GPR 2 "register_operand" "r")))]
  ""
  "orn\t%0,%2,%1"
  [(set_attr "type" "logical")])


;;
;;  ....................
;;
;;	TRUNCATION
;;
;;  ....................

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "fcvt.s.d\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "cnv_mode"	"D2S")
   (set_attr "mode"	"SF")])

;; Integer truncation patterns.  Truncating SImode values to smaller
;; modes is a no-op, as it is for most other GCC ports.  Truncating
;; DImode values to SImode is not a no-op for TARGET_64BIT since we
;; need to make sure that the lower 32 bits are properly sign-extended
;; (see TARGET_TRULY_NOOP_TRUNCATION).  Truncating DImode values into modes
;; smaller than SImode is equivalent to two separate truncations:
;;
;;                        A       B
;;    DI ---> HI  ==  DI ---> SI ---> HI
;;    DI ---> QI  ==  DI ---> SI ---> QI
;;
;; Step A needs a real instruction but step B does not.

(define_insn "truncdi<mode>2"
  [(set (match_operand:SUBDI 0 "nonimmediate_operand" "=r,m")
        (truncate:SUBDI (match_operand:DI 1 "register_operand" "r,r")))]
  "TARGET_64BIT"
  "@
    slli.w\t%0,%1,0
    st.<size>\t%1,%0"
  [(set_attr "move_type" "sll0,store")
   (set_attr "mode" "SI")])

(define_insn "truncdisi2_extended"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ZC")
        (truncate:SI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_64BIT"
  "stptr.w\t%1,%0"
  [(set_attr "move_type" "store")
   (set_attr "mode" "SI")])

;; Combiner patterns to optimize shift/truncate combinations.

(define_insn "*ashr_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=r")
        (truncate:SUBDI
	  (ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		       (match_operand:DI 2 "const_arith_operand" ""))))]
  "TARGET_64BIT && IN_RANGE (INTVAL (operands[2]), 32, 63)"
  "srai.d\t%0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

(define_insn "*lshr32_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=r")
        (truncate:SUBDI
	  (lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		       (const_int 32))))]
  "TARGET_64BIT"
  "srai.d\t%0,%1,32"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,ZC,W")))]
  "TARGET_64BIT"
  "@
   bstrpick.d\t%0,%1,31,0
   ldptr.w\t%0,%1\n\tlu32i.d\t%0,0
   ld.wu\t%0,%1"
  [(set_attr "move_type" "arith,load,load")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "1,2,1")])

(define_insn "*zero_extendsidi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (subreg:DI (match_operand:SI 1 "nonimmediate_operand" "r,ZC,W") 0))]
  "TARGET_64BIT"
  "@
   bstrpick.d\t%0,%1,31,0
   ldptr.w\t%0,%1\n\tlu32i.d\t%0,0
   ld.wu\t%0,%1"
  [(set_attr "move_type" "arith,load,load")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "1,2,1")])

;; See the comment before the *and<mode>3 pattern why this is generated by
;; combine.

(define_insn "zero_extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
        (zero_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   bstrpick.<d>\t%0,%1,<SHORT:qi_hi>,0
   ld.<SHORT:size>u\t%0,%1"
  [(set_attr "move_type" "pick_ins,load")
   (set_attr "compression" "*,*")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   andi\t%0,%1,0xff
   ld.bu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "mode" "HI")])

;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn "*zero_extend<GPR:mode>_trunc<SHORT:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (zero_extend:GPR
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "bstrpick.<d>\t%0,%1,<SHORT:qi_hi>,0"
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

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

;; When TARGET_64BIT, all SImode integer and accumulator registers
;; should already be in sign-extended form (see TARGET_TRULY_NOOP_TRUNCATION
;; and truncdisi2).  We can therefore get rid of register->register
;; instructions if we constrain the source to be in the same register as
;; the destination.
;;
;; Only the pre-reload scheduler sees the type of the register alternatives;
;; we split them into nothing before the post-reload scheduler runs.
;; These alternatives therefore have type "move" in order to reflect
;; what happens if the two pre-reload operands cannot be tied, and are
;; instead allocated two separate GPRs.  We don't distinguish between
;; the GPR and LO cases because we don't usually know during pre-reload
;; scheduling whether an operand will be LO or not.
(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "0,ZC,m")))]
  "TARGET_64BIT"
  "@
   #
   ldptr.w\t%0,%1
   ld.w\t%0,%1"
  "&& reload_completed && register_operand (operands[1], VOIDmode)"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
}
  [(set_attr "move_type" "move,load,load")
   (set_attr "mode" "DI")])

(define_insn "extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
        (sign_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   ext.w.<SHORT:size>\t%0,%1
   ld.<SHORT:size>\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI
	     (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   ext.w.b\t%0,%1
   ld.b\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "SI")])

;; Combiner patterns for truncate/sign_extend combinations.  The SI versions
;; use the shift/truncate patterns.

(define_insn_and_split "*extenddi_truncate<mode>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (match_dup 3)))
   (set (match_dup 0)
	(ashiftrt:DI (match_dup 2)
		     (match_dup 3)))]
{
  operands[2] = gen_lowpart (DImode, operands[0]);
  operands[3] = GEN_INT (BITS_PER_WORD - GET_MODE_BITSIZE (<MODE>mode));
}
  [(set_attr "move_type" "shift_shift")
   (set_attr "mode" "DI")])

(define_insn_and_split "*extendsi_truncate<mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (match_dup 3)))
   (set (match_dup 0)
	(truncate:SI (ashiftrt:DI (match_dup 2)
				  (match_dup 3))))]
{
  operands[2] = gen_lowpart (DImode, operands[0]);
  operands[3] = GEN_INT (BITS_PER_WORD - GET_MODE_BITSIZE (<MODE>mode));
}
  [(set_attr "move_type" "shift_shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*extendhi_truncateqi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (const_int 56)))
   (set (match_dup 0)
	(truncate:HI (ashiftrt:DI (match_dup 2)
				  (const_int 56))))]
{
  operands[2] = gen_lowpart (DImode, operands[0]);
}
  [(set_attr "move_type" "shift_shift")
   (set_attr "mode" "SI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "fcvt.d.s\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "cnv_mode"	"S2D")
   (set_attr "mode"	"DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

;; conversion of a floating-point value to a integer

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ftintrz.w.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "ftintrz.w.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])


(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "ftintrz.l.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "ftintrz.l.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])

;; conversion of an integeral (or boolean) value to a floating-point value

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ffint.d.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "ffint.d.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "ffint.s.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "ffint.s.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])

;; floating point value by converting to value to an unsigned integer

(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31, DFmode);

  if (reg1)			/* Turn off complaints about unreached code.  */
    {
      loongarch_emit_move (reg1, const_double_from_real_value (offset, DFmode));
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
    }
})


(define_expand "fixuns_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand")
	(unsigned_fix:DI (match_operand:DF 1 "register_operand")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
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
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
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
  [(set (match_operand:GPR 0 "register_operand")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand")
			  (match_operand 2 "const_int_operand")
			  (match_operand 3 "const_int_operand")))]
  ""
{
  if (!loongarch_use_ins_ext_p (operands[1], INTVAL (operands[2]),
			   INTVAL (operands[3])))
    FAIL;
})

(define_insn "*extzv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "loongarch_use_ins_ext_p (operands[1], INTVAL (operands[2]),
		       INTVAL (operands[3]))"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) -1 );
  return "bstrpick.<d>\t%0,%1,%2,%3";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"<MODE>")])

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
  operands[1] = GEN_INT (INTVAL (operands[1]) + INTVAL (operands[2]) -1 );
  return "bstrins.<d>\t%0,%z3,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"<MODE>")])


;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; Allow combine to split complex const_int load sequences, using operand 2
;; to store the intermediate results.  See move_operand for details.
(define_split
  [(set (match_operand:GPR 0 "register_operand")
	(match_operand:GPR 1 "splittable_const_int_operand"))
   (clobber (match_operand:GPR 2 "register_operand"))]
  ""
  [(const_int 0)]
{
  loongarch_move_integer (operands[2], operands[0], INTVAL (operands[1]));
  DONE;
})

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

(define_insn "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,ZC,r,m,*f,*f,*r,*m")
       (match_operand:DI 1 "move_operand" "r,i,ZC,r,m,r,*J*r,*m,*f,*f"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,load,store,mgtf,fpload,mftg,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,ZC,r,m,*f,*f,*r,*m")
	(match_operand:DI 1 "move_operand" "r,Yd,ZC,rJ,m,rJ,*r*J,*m,*f,*f"))]
  "TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,load,store,mgtf,fpload,mftg,fpstore")
   (set_attr "mode" "DI")])

;; 32-bit Integer moves

(define_expand "movsi"
  [(set (match_operand:SI 0 "")
	(match_operand:SI 1 ""))]
  ""
{
  if (loongarch_legitimize_move (SImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,ZC,r,m,*f,*f,*r,*m,*r,*z")
	(match_operand:SI 1 "move_operand" "r,Yd,ZC,rJ,m,rJ,*r*J,*m,*f,*f,*z,*r"))]
  "(register_operand (operands[0], SImode)
       || reg_or_0_operand (operands[1], SImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,load,store,mgtf,fpload,mftg,fpstore,mftg,mgtf")
   (set_attr "compression" "all,*,*,*,*,*,*,*,*,*,*,*")
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

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,m")
	(match_operand:HI 1 "move_operand"         "r,Yd,I,m,rJ"))]
  "(register_operand (operands[0], HImode)
       || reg_or_0_operand (operands[1], HImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store")
   (set_attr "compression" "all,all,*,*,*")
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
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:QI 1 "move_operand"         "r,I,m,rJ"))]
  "(register_operand (operands[0], QImode)
       || reg_or_0_operand (operands[1], QImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store")
   (set_attr "compression" "all,*,*,*")
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
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*r,*r,*r,*m")
	(match_operand:SF 1 "move_operand" "f,G,m,f,G,*r,*f,*G*r,*m,*r"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mgtf,fpload,fpstore,store,mgtf,mftg,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "move_operand" "Gr,m,r"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return loongarch_output_move (operands[0], operands[1]); }
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
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*r,*r,*r,*m")
	(match_operand:DF 1 "move_operand" "f,G,m,f,G,*r,*f,*r*G,*m,*r"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mgtf,fpload,fpstore,store,mgtf,mftg,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:DF 1 "move_operand" "rG,m,rG"))]
  "(TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT)
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "DF")])


;; 128-bit integer moves

(define_expand "movti"
  [(set (match_operand:TI 0)
	(match_operand:TI 1))]
  "TARGET_64BIT"
{
  if (loongarch_legitimize_move (TImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:TI 1 "move_operand" "r,i,m,rJ"))]
  "TARGET_64BIT
   && (register_operand (operands[0], TImode)
       || reg_or_0_operand (operands[1], TImode))"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store")
   (set (attr "mode")
    (if_then_else (eq_attr "move_type" "imul")
		      (const_string "SI")
		      (const_string "TI")))])


;; 128-bit floating point moves

(define_expand "movtf"
  [(set (match_operand:TF 0)
	(match_operand:TF 1))]
  "TARGET_64BIT"
{
  if (loongarch_legitimize_move (TFmode, operands[0], operands[1]))
    DONE;
})

;; This pattern handles both hard- and soft-float cases.
(define_insn "*movtf"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=r,r,m,f,r,f,m")
	(match_operand:TF 1 "move_operand" "rG,m,rG,rG,f,m,f"))]
  "TARGET_64BIT
   && (register_operand (operands[0], TFmode)
       || reg_or_0_operand (operands[1], TFmode))"
  "#"
  [(set_attr "move_type" "move,load,store,mgtf,mftg,fpload,fpstore")
   (set_attr "mode" "TF")])


(define_split
  [(set (match_operand:MOVE64 0 "nonimmediate_operand")
	(match_operand:MOVE64 1 "move_operand"))]
  "reload_completed && loongarch_split_move_insn_p (operands[0], operands[1], insn)"
  [(const_int 0)]
{
  loongarch_split_move_insn (operands[0], operands[1], curr_insn);
  DONE;
})

(define_split
  [(set (match_operand:MOVE128 0 "nonimmediate_operand")
	(match_operand:MOVE128 1 "move_operand"))]
  "reload_completed && loongarch_split_move_insn_p (operands[0], operands[1], insn)"
  [(const_int 0)]
{
  loongarch_split_move_insn (operands[0], operands[1], curr_insn);
  DONE;
})

;; Emit a doubleword move in which exactly one of the operands is
;; a floating-point register.  We can't just emit two normal moves
;; because of the constraints imposed by the FPU register model;
;; see loongarch_cannot_change_mode_class for details.  Instead, we keep
;; the FPR whole and use special patterns to refer to each word of
;; the other operand.

(define_expand "move_doubleword_fpr<mode>"
  [(set (match_operand:SPLITF 0)
	(match_operand:SPLITF 1))]
  ""
{
  if (FP_REG_RTX_P (operands[0]))
    {
      rtx low = loongarch_subword (operands[1], 0);
      rtx high = loongarch_subword (operands[1], 1);
      emit_insn (gen_load_low<mode> (operands[0], low));
      if (!TARGET_64BIT)
       emit_insn (gen_movgr2frh<mode> (operands[0], high, operands[0]));
      else
       emit_insn (gen_load_high<mode> (operands[0], high, operands[0]));
    }
  else
    {
      rtx low = loongarch_subword (operands[0], 0);
      rtx high = loongarch_subword (operands[0], 1);
      emit_insn (gen_store_word<mode> (low, operands[1], const0_rtx));
      if (!TARGET_64BIT)
       emit_insn (gen_movfrh2gr<mode> (high, operands[1]));
      else
       emit_insn (gen_store_word<mode> (high, operands[1], const1_rtx));
    }
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

;; sel.fmt copies the 3rd argument when the 1st is non-zero and the 2nd
;; argument if the 1st is zero.  This means operand 2 and 3 are
;; inverted in the instruction.

;; FIXME: fsel
(define_insn "*sel<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(if_then_else:ANYF
	 (ne:FCC (match_operand:FCC 1 "register_operand" "z")
		 (const_int 0))
	 (match_operand:ANYF 2 "reg_or_0_operand" "f")
	 (match_operand:ANYF 3 "reg_or_0_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fsel\t%0,%3,%2,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<ANYF:MODE>")])

;; These are the main define_expand's used to make conditional moves.

(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_dup 5)
			  (match_operand:GPR 2 "reg_or_0_operand")
			  (match_operand:GPR 3 "reg_or_0_operand")))]
  "TARGET_COND_MOVE_INT"
{
  if (!INTEGRAL_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  loongarch_expand_conditional_move (operands);
  DONE;
})

;; FIXME: fsel
(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:ANYF 0 "register_operand")
	(if_then_else:ANYF (match_dup 5)
			      (match_operand:ANYF 2 "reg_or_0_operand")
			      (match_operand:ANYF 3 "reg_or_0_operand")))]
  "TARGET_HARD_FLOAT && TARGET_COND_MOVE_FLOAT"
{
  if (!FLOAT_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  loongarch_expand_conditional_move (operands);
  DONE;
})

;; lu32i.d
(define_insn "lu32i_d"
  [(set (match_operand:DI   0 "register_operand" "=r")
        (ior:DI
           (zero_extend:DI
             (subreg:SI (match_operand:DI 1 "register_operand" "0") 0))
           (match_operand:DI 2 "const_lu32i_operand" "u")))]
  "TARGET_64BIT"
  "lu32i.d\t%0,%X2>>32"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

;; lu52i.d

(define_insn "lu52i_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ior:DI
          (and:DI (match_operand:DI 1 "register_operand" "r")
                  (match_operand 2 "lu52i_mask_operand"))
          (match_operand 3 "const_lu52i_operand" "v")))]
    "TARGET_64BIT"
    "lu52i.d\t%0,%1,%X3>>52"
    [(set_attr "type" "arith")
     (set_attr "mode" "DI")])

;; Convert floating-point numbers to integers

(define_insn "frint_<fmt>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
       (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
                    UNSPEC_FRINT))]
  "TARGET_HARD_FLOAT"
  "frint.<fmt>\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<MODE>")])

;; LoongArch supports loading and storing a floating point register from
;; the sum of two general registers.  We use two versions for each of
;; these four instructions: one where the two general registers are
;; SImode, and one where they are DImode.  This is because general
;; registers will be in SImode when they hold 32-bit values, but,
;; since the 32-bit values are always sign extended, the f{ld/st}x.{s/d}
;; instructions will still work correctly.

;; ??? Perhaps it would be better to support these instructions by
;; modifying TARGET_LEGITIMATE_ADDRESS_P and friends.  However, since
;; these instructions can only be used to load and store floating
;; point registers, that would probably cause trouble in reload.

(define_insn "*<ANYF:floadx>_<P:mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (plus:P (match_operand:P 1 "register_operand" "r")
			  (match_operand:P 2 "register_operand" "r"))))]
  "TARGET_HARD_FLOAT"
  "<ANYF:floadx>\t%0,%1,%2"
  [(set_attr "type" "fpidxload")
   (set_attr "mode" "<ANYF:UNITMODE>")])

(define_insn "*<ANYF:fstorex>_<P:mode>"
  [(set (mem:ANYF (plus:P (match_operand:P 1 "register_operand" "r")
			  (match_operand:P 2 "register_operand" "r")))
	(match_operand:ANYF 0 "register_operand" "f"))]
  "TARGET_HARD_FLOAT"
  "<ANYF:fstorex>\t%0,%1,%2"
  [(set_attr "type" "fpidxstore")
   (set_attr "mode" "<ANYF:UNITMODE>")])

;; loading and storing a integer register from the sum of two general registers.

(define_insn "*<GPR:loadx>_<P:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (mem:GPR
                (plus:P (match_operand:P 1 "register_operand" "r")
                        (match_operand:P 2 "register_operand" "r"))))]
  ""
  "<GPR:loadx>\t%0,%1,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*<GPR:storex>_<P:mode>"
  [(set (mem:GPR (plus:P (match_operand:P 1 "register_operand" "r")
                         (match_operand:P 2 "register_operand" "r")))
        (match_operand:GPR 0 "register_operand" "r"))]
  ""
  "<GPR:storex>\t%0,%1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "<GPR:MODE>")])

;; SHORT mode sign_extend.
(define_insn "*extend_<SHORT:loadx>_<GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (sign_extend:GPR
          (mem:SHORT
            (plus:P (match_operand:P 1 "register_operand" "r")
                    (match_operand:P 2 "register_operand" "r")))))]
  ""
  "<SHORT:loadx>\t%0,%1,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*extend_<SHORT:storex>"
  [(set (mem:SHORT (plus:P (match_operand:P 1 "register_operand" "r")
                           (match_operand:P 2 "register_operand" "r")))
        (match_operand:SHORT 0 "register_operand" "r"))]
  ""
  "<SHORT:storex>\t%0,%1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

;; Load the low word of operand 0 with operand 1.
(define_insn "load_low<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "rJ,m")]
		       UNSPEC_LOAD_LOW))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = loongarch_subword (operands[0], 0);
  return loongarch_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mgtf,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Load the high word of operand 0 from operand 1, preserving the value
;; in the low word.
(define_insn "load_high<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "rJ,m")
			(match_operand:SPLITF 2 "register_operand" "0,0")]
		       UNSPEC_LOAD_HIGH))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = loongarch_subword (operands[0], 1);
  return loongarch_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mgtf,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Store one word of operand 1 in operand 0.  Operand 2 is 1 to store the
;; high word and 0 to store the low word.
(define_insn "store_word<mode>"
  [(set (match_operand:<HALFMODE> 0 "nonimmediate_operand" "=r,m")
	(unspec:<HALFMODE> [(match_operand:SPLITF 1 "register_operand" "f,f")
			    (match_operand 2 "const_int_operand")]
			   UNSPEC_STORE_WORD))]
  "TARGET_HARD_FLOAT"
{
  operands[1] = loongarch_subword (operands[1], INTVAL (operands[2]));
  return loongarch_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mftg,fpstore")
   (set_attr "mode" "<HALFMODE>")])

;; Thread-Local Storage

(define_insn "got_load_tls_gd<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_GD))]
  ""
  "la.tls.gd\t%0,%1"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "got_load_tls_ld<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_LD))]
  ""
  "la.tls.ld\t%0,%1"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "got_load_tls_le<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_LE))]
  ""
  "la.tls.le\t%0,%1"
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

;; Move operand 1 to the high word of operand 0 using movgr2frh, preserving the
;; value in the low word.
(define_insn "movgr2frh<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "reg_or_0_operand" "rJ")
		        (match_operand:SPLITF 2 "register_operand" "0")]
		       UNSPEC_MOVGR2FRH))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64"
  "movgr2frh.w\t%z1,%0"
  [(set_attr "move_type" "mgtf")
   (set_attr "mode" "<HALFMODE>")])

;; Move high word of operand 1 to operand 0 using movfrh2gr.
(define_insn "movfrh2gr<mode>"
  [(set (match_operand:<HALFMODE> 0 "register_operand" "=r")
	(unspec:<HALFMODE> [(match_operand:SPLITF 1 "register_operand" "f")]
			    UNSPEC_MOVFRH2GR))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64"
  "movfrh2gr.s\t%0,%1"
  [(set_attr "move_type" "mftg")
   (set_attr "mode" "<HALFMODE>")])


;; Expand in-line code to clear the instruction cache between operand[0] and
;; operand[1].
(define_expand "clear_cache"
  [(match_operand 0 "pmode_register_operand")
   (match_operand 1 "pmode_register_operand")]
  ""
  "
{
  emit_insn (gen_ibar (const0_rtx));
  DONE;
}")

(define_insn "ibar"
  [(unspec_volatile:SI [(match_operand 0 "const_uimm15_operand")] UNSPEC_IBAR)]
  ""
  "ibar\t%0")

(define_insn "dbar"
  [(unspec_volatile:SI [(match_operand 0 "const_uimm15_operand")] UNSPEC_DBAR)]
  ""
  "dbar\t%0")



;; Privileged state instruction

(define_insn "cpucfg"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")]
			     UNSPEC_CPUCFG))]
  ""
  "cpucfg\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")])

(define_insn "asrtle_d"
	[(unspec_volatile:DI [(match_operand:DI 0 "register_operand" "r")
                       (match_operand:DI 1 "register_operand" "r")]
			     UNSPEC_ASRTLE_D)]
  "TARGET_64BIT"
  "asrtle.d\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")])

(define_insn "asrtgt_d"
	[(unspec_volatile:DI [(match_operand:DI 0 "register_operand" "r")
                       (match_operand:DI 1 "register_operand" "r")]
			     UNSPEC_ASRTGT_D)]
  "TARGET_64BIT"
  "asrtgt.d\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")])

(define_insn "<p>csrrd"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec_volatile:GPR [(match_operand  1 "const_uimm14_operand")]
			     UNSPEC_CSRRD))]
  ""
  "csrrd\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])

(define_insn "<p>csrwr"
  [(set (match_operand:GPR 0 "register_operand" "=r")
         (unspec_volatile:GPR
          [(match_operand:GPR 1 "register_operand" "0")
           (match_operand 2 "const_uimm14_operand")]
	  UNSPEC_CSRWR))]
  ""
  "csrwr\t%0,%2"
  [(set_attr "type"	"store")
   (set_attr "mode"	"<MODE>")])

(define_insn "<p>csrxchg"
  [(set (match_operand:GPR 0 "register_operand" "=r")
         (unspec_volatile:GPR
          [(match_operand:GPR 1 "register_operand" "0")
           (match_operand:GPR 2 "register_operand" "q")
           (match_operand     3 "const_uimm14_operand")]
          UNSPEC_CSRXCHG))]
  ""
  "csrxchg\t%0,%2,%3"
  [(set_attr "type"    "load")
   (set_attr "mode"    "<MODE>")])

(define_insn "iocsrrd_<size>"
  [(set (match_operand:QHWD 0 "register_operand" "=r")
	(unspec_volatile:QHWD [(match_operand:SI  1 "register_operand" "r")]
			      UNSPEC_IOCSRRD))]
  ""
  "iocsrrd.<size>\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])

(define_insn "iocsrwr_<size>"
  [(unspec_volatile:QHWD [(match_operand:QHWD 0 "register_operand" "r")
                        (match_operand:SI 1 "register_operand" "r")]
                        UNSPEC_IOCSRWR)]
  ""
  "iocsrwr.<size>\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])

(define_insn "<p>cacop"
  [(unspec_volatile:X [(match_operand 0 "const_uimm5_operand")
			 (match_operand:X 1 "register_operand" "r")
			 (match_operand 2 "const_imm12_operand")]
			 UNSPEC_CACOP)]
  ""
  "cacop\t%0,%1,%2"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])

(define_insn "<p>lddir"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")
			 (match_operand:X 1 "register_operand" "r")
			 (match_operand 2 "const_uimm5_operand")]
			 UNSPEC_LDDIR)]
  ""
  "lddir\t%0,%1,%2"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])

(define_insn "<p>ldpte"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")
			 (match_operand 1 "const_uimm5_operand")]
			 UNSPEC_LDPTE)]
  ""
  "ldpte\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"<MODE>")])


;; Block moves, see loongarch.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  " !TARGET_MEMCPY"
{
  if (loongarch_expand_block_move (operands[0], operands[1], operands[2]))
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

(define_insn "<optab><mode>3"
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
   (set_attr "compression" "none")
   (set_attr "mode" "<MODE>")])

(define_insn "*<optab>si3_extend"
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

(define_insn "rotr<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "r,r")
		      (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "rotr%i2.<d>\t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "mode" "<MODE>")])

(define_insn "zero_extend_ashift1"
 [ (set (match_operand:DI 0 "register_operand" "=r")
        (and:DI (ashift:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
                           (match_operand 2 "const_immlsa_operand" ""))
                    (match_operand 3 "shift_mask_operand" "")))]
"TARGET_64BIT"
"bstrpick.d\t%0,%1,31,0\n\talsl.d\t%0,%0,$r0,%2"
[(set_attr "type" "arith")
 (set_attr "mode" "DI")
 (set_attr "insn_count" "2")])

(define_insn "zero_extend_ashift2"
 [ (set (match_operand:DI 0 "register_operand" "=r")
        (and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
                           (match_operand 2 "const_immlsa_operand" ""))
                    (match_operand 3 "shift_mask_operand" "")))]
"TARGET_64BIT"
"bstrpick.d\t%0,%1,31,0\n\talsl.d\t%0,%0,$r0,%2"
[(set_attr "type" "arith")
 (set_attr "mode" "DI")
 (set_attr "insn_count" "2")])

(define_insn "alsl_paired1"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (plus:DI (and:DI (ashift:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
			   (match_operand 2 "const_immlsa_operand" ""))
			(match_operand 3 "shift_mask_operand" ""))
		(match_operand:DI 4 "register_operand" "r")))]
 "TARGET_64BIT"
 "bstrpick.d\t%0,%1,31,0\n\talsl.d\t%0,%0,%4,%2"
 [(set_attr "type" "arith")
  (set_attr "mode" "DI")
  (set_attr "insn_count" "2")])

(define_insn "alsl_paired2"
 [(set (match_operand:DI 0 "register_operand" "=&r")
       (plus:DI (match_operand:DI 1 "register_operand" "r")
		(and:DI (ashift:DI (match_operand:DI 2 "register_operand" "r")
			   (match_operand 3 "const_immlsa_operand" ""))
			(match_operand 4 "shift_mask_operand" ""))))]
 "TARGET_64BIT"
 "bstrpick.d\t%0,%2,31,0\n\talsl.d\t%0,%0,%1,%3"
 [(set_attr "type" "arith")
  (set_attr "mode" "DI")
  (set_attr "insn_count" "2")])

(define_insn "alsl<mode>3"
 [(set (match_operand:GPR 0 "register_operand" "=r")
       (plus:GPR (ashift:GPR (match_operand:GPR 1 "register_operand" "r")
			     (match_operand 2 "const_immlsa_operand" ""))
		(match_operand:GPR 3 "register_operand" "r")))]
 ""
 "alsl.<d>\t%0,%1,%3,%2"
 [(set_attr "type" "arith")
  (set_attr "mode" "<MODE>")])



;; Reverse the order of bytes of operand 1 and store the result in operand 0.

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "revb.2h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn_and_split "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  ""
  [(set (match_dup 0) (unspec:SI [(match_dup 1)] UNSPEC_REVB_2H))
   (set (match_dup 0) (rotatert:SI (match_dup 0) (const_int 16)))]
  ""
  [(set_attr "insn_count" "2")])

(define_insn_and_split "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(bswap:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_64BIT"
  "#"
  ""
  [(set (match_dup 0) (unspec:DI [(match_dup 1)] UNSPEC_REVB_4H))
   (set (match_dup 0) (unspec:DI [(match_dup 0)] UNSPEC_REVH_D))]
  ""
  [(set_attr "insn_count" "2")])

(define_insn "revb_2h"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")] UNSPEC_REVB_2H))]
  ""
  "revb.2h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "revb_4h"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")] UNSPEC_REVB_4H))]
  "TARGET_64BIT"
  "revb.4h\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "revh_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")] UNSPEC_REVH_D))]
  "TARGET_64BIT"
  "revh.d\t%0,%1"
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
			 [(match_operand:GPR 2 "register_operand" "r,r")
			  (match_operand:GPR 3 "reg_or_0_operand" "J,r")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return loongarch_output_order_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")
   (set_attr "hazard" "forbidden_slot")])

(define_insn "*branch_order<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "r,r")
			  (match_operand:GPR 3 "reg_or_0_operand" "J,r")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return loongarch_output_order_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")
   (set_attr "hazard" "forbidden_slot")])

;; Conditional branch on equality comparison.

(define_insn "*branch_equality<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "r")
			  (match_operand:GPR 3 "reg_or_0_operand" "rJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  { return loongarch_output_equal_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")
   (set_attr "hazard" "forbidden_slot")])


(define_insn "*branch_equality<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "r")
			  (match_operand:GPR 3 "reg_or_0_operand" "rJ")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  { return loongarch_output_equal_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")
   (set_attr "hazard" "forbidden_slot")])


(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:GPR 1 "register_operand")
		        (match_operand:GPR 2 "nonmemory_operand")])
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

(define_insn "*seq_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(eq:GPR2 (match_operand:GPR 1 "register_operand" "r")
		 (const_int 0)))]
  ""
  "sltui\t%0,%1,1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])


(define_insn "*sne_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(ne:GPR2 (match_operand:GPR 1 "register_operand" "r")
		 (const_int 0)))]
  ""
  "sltu\t%0,%.,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sgt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(any_gt:GPR2 (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ")))]
  ""
  "slt<u>\t%0,%z2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sge<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(any_ge:GPR2 (match_operand:GPR 1 "register_operand" "r")
		     (const_int 1)))]
  ""
  "slt<u>i\t%0,%.,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*slt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(any_lt:GPR2 (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:GPR 2 "arith_operand" "rI")))]
  ""
  "slt<u>%i2\t%0,%1,%2";
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sle<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=r")
	(any_le:GPR2 (match_operand:GPR 1 "register_operand" "r")
		     (match_operand:GPR 2 "sle_operand" "")))]
  ""
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>i\t%0,%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])


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
  "TARGET_HARD_FLOAT"
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
  "TARGET_ABSOLUTE_JUMPS"
{
  return "b\t%l0";
}
  [(set_attr "type" "branch")])

(define_insn "*jump_pic"
  [(set (pc)
	(label_ref (match_operand 0)))]
  "!TARGET_ABSOLUTE_JUMPS"
{
  return "b\t%0";
}
  [(set_attr "type" "branch")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  operands[0] = force_reg (Pmode, operands[0]);
  emit_jump_insn (PMODE_INSN (gen_indirect_jump, (operands[0])));
  DONE;
})

(define_insn "indirect_jump_<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "r"))]
  ""
  {
    return "jr\t%0";
  }
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
					 gen_rtx_LABEL_REF (Pmode, operands[1]),
					 NULL_RTX, 0, OPTAB_DIRECT);
  emit_jump_insn (PMODE_INSN (gen_tablejump, (operands[0], operands[1])));
  DONE;
})

(define_insn "tablejump_<mode>"
  [(set (pc)
	(match_operand:P 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  {
    return "jr\t%0";
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
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")
   (set_attr "mode" "none")])

(define_insn "probe_stack_range_<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "0")
			    (match_operand:P 2 "register_operand" "r")]
			    UNSPEC_PROBE_STACK_RANGE))]
  ""
 { return loongarch_output_probe_stack_range (operands[0], operands[2]); }
  [(set_attr "type" "unknown")
   (set_attr "can_delay" "no")
   (set_attr "mode" "<MODE>")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  loongarch_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  loongarch_expand_epilogue (true);
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
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "<optab>_internal"
  [(any_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  {
    return "jr\t%0";
  }
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Exception return.
(define_insn "loongarch_ertn"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPEC_ERTN)]
  ""
  "ertn"
  [(set_attr "type"	"trap")
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
  "TARGET_SIBCALLS"
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[0], 0));

  emit_call_insn (gen_sibcall_internal (target, operands[1]));
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,c,a,t,h"))
	 (match_operand 1 "" ""))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
    {
    case 0:
      return "jr\t%0";
    case 1:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,(%%pcrel(%0+0x20000))>>18\n\tjirl\t$r0,$r12,%%pcrel(%0+4)-(%%pcrel(%0+4+0x20000)>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r12,$r13,%0\n\tjr\t$r12";
      else
        return "b\t%0";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "b\t%0";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%0\n\tjr\t$r12";
      else
        return "la.global\t$r12,%0\n\tjr\t$r12";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%0\n\tjr\t$r12";
      else
        return "la.global\t$r12,%0\n\tjr\t$r12";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return "b\t%%plt(%0)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,(%%plt(%0)+0x20000)>>18\n\tjirl\t$r0,$r12,%%plt(%0)+4-((%%plt(%0)+(4+0x20000))>>18<<18)";
      else
	{
          sorry ("cmodel extreme and tiny static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  "TARGET_SIBCALLS"
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[1], 0));

 /*  Handle return values created by loongarch_return_fpr_pair. */
  if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 2)
    {
      emit_call_insn (gen_sibcall_value_multiple_internal (XEXP (XVECEXP (operands[0], 0, 0), 0),
      target, operands[2], XEXP (XVECEXP (operands[0], 0, 1), 0)));
    }
   else
    {
      /*  Handle return values created by loongarch_return_fpr_single. */
      if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 1)
      operands[0] = XEXP (XVECEXP (operands[0], 0, 0), 0);

      emit_call_insn (gen_sibcall_value_internal (operands[0], target, operands[2]));
    }
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,c,a,t,h"))
              (match_operand 2 "" "")))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
  {
    case 0:
      return "jr\t%1";
    case 1:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,%%pcrel(%1+0x20000)>>18\n\tjirl\t$r0,$r12,%%pcrel(%1+4)-((%%pcrel(%1+4+0x20000))>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "b\t%1";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "b\t%1";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "la.global\t$r12,%1\n\tjr\t$r12";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "la.global\t$r12,%1\n\tjr\t$r12";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return " b\t%%plt(%1)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,(%%plt(%1)+0x20000)>>18\n\tjirl\t$r0,$r12,%%plt(%1)+4-((%%plt(%1)+(4+0x20000))>>18<<18)";
      else
	{
          sorry ("loongarch cmodel extreme and tiny-static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
  }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,c,a,t,h"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
{
  switch (which_alternative)
  {
    case 0:
      return "jr\t%1";
    case 1:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,%%pcrel(%1+0x20000)>>18\n\tjirl\t$r0,$r12,%%pcrel(%1+4)-(%%pcrel(%1+4+0x20000)>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "b\t%1";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "b\t%1";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "la.global\t$r12,%1\n\tjr\t$r12";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r12,$r13,%1\n\tjr\t$r12";
      else
        return "la.global\t$r12,%1\n\tjr\t$r12";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return "b\t%%plt(%1)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r12,(%%plt(%1)+0x20000)>>18\n\tjirl\t$r0,$r12,%%plt(%1)+4-((%%plt(%1)+(4+0x20000))>>18<<18)";
      else
	{
          sorry ("loongarch cmodel extreme and tiny-static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
  }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[0], 0));

  emit_call_insn (gen_call_internal (target, operands[1]));
  DONE;
})

(define_insn "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "e,c,a,t,h"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "jirl\t$r1,%0,0";
    case 1:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,%%pcrel(%0+0x20000)>>18\n\tjirl\t$r1,$r1,%%pcrel(%0+4)-(%%pcrel(%0+4+0x20000)>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r1,$r12,%0\n\tjirl\t$r1,$r1,0";
      else
        return "bl\t%0";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "bl\t%0";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%0\n\tjirl\t$r1,$r1,0";
      else
        return "la.global\t$r1,%0\n\tjirl\t$r1,$r1,0";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%0\n\tjirl\t$r1,$r1,0";
      else
        return "la.global\t$r1,%0\n\tjirl\t$r1,$r1,0";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,(%%plt(%0)+0x20000)>>18\n\tjirl\t$r1,$r1,%%plt(%0)+4-((%%plt(%0)+(4+0x20000))>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return "bl\t%%plt(%0)";
      else
	{
          sorry ("cmodel extreme and tiny-static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")
   (set_attr "insn_count" "1,2,3,3,2")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  rtx target = loongarch_legitimize_call_address (XEXP (operands[1], 0));
 /*  Handle return values created by loongarch_return_fpr_pair. */
  if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 2)
    emit_call_insn (gen_call_value_multiple_internal (XEXP (XVECEXP (operands[0], 0, 0), 0),
    target, operands[2], XEXP (XVECEXP (operands[0], 0, 1), 0)));
   else
    {
      /*  Handle return values created by loongarch_return_fpr_single. */
      if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) == 1)
      operands[0] = XEXP (XVECEXP (operands[0], 0, 0), 0);

      emit_call_insn (gen_call_value_internal (operands[0], target, operands[2]));
    }
  DONE;
})

;; See comment for call_internal.
(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "e,c,a,t,h"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "jirl\t$r1,%1,0";
    case 1:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,%%pcrel(%1+0x20000)>>18\n\tjirl\t$r1,$r1,%%pcrel(%1+4)-(%%pcrel(%1+4+0x20000)>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0";
      else
        return "bl\t%1";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "bl\t%1";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0";
      else
        return "la.global\t$r1,%1\n\tjirl\t$r1,$r1,0";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0";
      else
        return "la.global\t$r1,%1\n\tjirl\t$r1,$r1,0";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,(%%plt(%1)+0x20000)>>18\n\tjirl\t$r1,$r1,%%plt(%1)+4-((%%plt(%1)+(4+0x20000))>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return "bl\t%%plt(%1)";
      else
	{
          sorry ("loongarch cmodel extreme and tiny-static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")
   (set_attr "insn_count" "1,2,3,3,2")])

;; See comment for call_internal.
(define_insn "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "e,c,a,t,h"))
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
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,%%pcrel(%1+0x20000)>>18\n\tjirl\t$r1,$r1,%%pcrel(%1+4)-(%%pcrel(%1+4+0x20000)>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.local\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0";
      else
        return "bl\t%1";
    case 2:
      if (loongarch_cmodel_var == LARCH_CMODEL_TINY_STATIC)
        return "bl\t%1";
      else if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0 ";
      else
        return "la.global\t$r1,%1\n\tjirl\t$r1,$r1,0";
    case 3:
      if (loongarch_cmodel_var == LARCH_CMODEL_EXTREME)
        return "la.global\t$r1,$r12,%1\n\tjirl\t$r1,$r1,0";
      else
        return "la.global\t$r1,%1\n\tjirl\t$r1,$r1,0";
    case 4:
      if (loongarch_cmodel_var == LARCH_CMODEL_LARGE)
        return "pcaddu18i\t$r1,(%%plt(%1)+0x20000)>>18\n\tjirl\t$r1,$r1,%%plt(%1)+4-((%%plt(%1)+(4+0x20000))>>18<<18)";
      else if (loongarch_cmodel_var == LARCH_CMODEL_NORMAL || loongarch_cmodel_var == LARCH_CMODEL_TINY)
        return "bl\t%%plt(%1)";
      else
	{
          sorry ("loongarch cmodel extreme and tiny-static not support plt");
	  return "";  /* GCC complains about may fall through.  */
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "jirl" "indirect,direct,direct,direct,direct")
   (set_attr "insn_count" "1,2,3,3,2")])


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

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])

;; __builtin_loongarch_movfcsr2gr: move the FCSR into operand 0.
(define_insn "loongarch_movfcsr2gr"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (unspec_volatile:SI [(match_operand 1 "const_uimm5_operand")] UNSPEC_MOVFCSR2GR))]
  "TARGET_HARD_FLOAT"
  "movfcsr2gr\t%0,$r%1")

;; __builtin_loongarch_movgr2fcsr: move operand 0 into the FCSR.
(define_insn "loongarch_movgr2fcsr"
  [(unspec_volatile [(match_operand 0 "const_uimm5_operand")
                    (match_operand:SI 1 "register_operand" "r")]
          UNSPEC_MOVGR2FCSR)]
  "TARGET_HARD_FLOAT"
  "movgr2fcsr\t$r%0,%1")

(define_insn "fclass_<fmt>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
       (unspec:ANYF [(match_operand:ANYF 1 "register_operand" "f")]
                        UNSPEC_FCLASS))]
  "TARGET_HARD_FLOAT"
  "fclass.<fmt>\t%0,%1"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_insn "bytepick_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "register_operand" "r")
                   (match_operand:SI 3 "const_0_to_3_operand" "n")]
                  UNSPEC_BYTEPICK_W))]
  ""
  "bytepick.w\t%0,%1,%2,%z3"
  [(set_attr "mode"    "SI")])

(define_insn "bytepick_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
       (unspec:DI [(match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "register_operand" "r")
                   (match_operand:DI 3 "const_0_to_7_operand" "n")]
                  UNSPEC_BYTEPICK_D))]
  ""
  "bytepick.d\t%0,%1,%2,%z3"
  [(set_attr "mode"    "DI")])

(define_insn "bitrev_4b"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (unspec:SI [(match_operand:SI 1 "register_operand" "r")]
                  UNSPEC_BITREV_4B))]
  ""
  "bitrev.4b\t%0,%1"
  [(set_attr "type"    "unknown")
   (set_attr "mode"    "SI")])

(define_insn "bitrev_8b"
  [(set (match_operand:DI 0 "register_operand" "=r")
       (unspec:DI [(match_operand:DI 1 "register_operand" "r")]
                  UNSPEC_BITREV_8B))]
  ""
  "bitrev.8b\t%0,%1"
  [(set_attr "type"    "unknown")
   (set_attr "mode"    "DI")])

(define_insn "stack_tie<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:GPR 0 "register_operand" "r")
		     (match_operand:GPR 1 "register_operand" "r")]
		    UNSPEC_TIE))]
  ""
  ""
  [(set_attr "length" "0")]
)

(define_insn "gpr_restore_return"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))
   (const_int 0)]
  ""
  "")

(define_split
  [(match_operand 0 "small_data_pattern")]
  "reload_completed"
  [(match_dup 0)]
  { operands[0] = loongarch_rewrite_small_data (operands[0]); })


;; Match paired HI/SI/SF/DFmode load/stores.
(define_insn "*join2_load_store<JOIN_MODE:mode>"
  [(set (match_operand:JOIN_MODE 0 "nonimmediate_operand" "=r,f,m,m,r,ZC")
	(match_operand:JOIN_MODE 1 "nonimmediate_operand" "m,m,r,f,ZC,r"))
   (set (match_operand:JOIN_MODE 2 "nonimmediate_operand" "=r,f,m,m,r,ZC")
	(match_operand:JOIN_MODE 3 "nonimmediate_operand" "m,m,r,f,ZC,r"))]
  "reload_completed"
  {
    bool load_p = (which_alternative == 0 || which_alternative == 1);
    /* Reg-renaming pass reuses base register if it is dead after bonded loads.
       Hardware does not bond those loads, even when they are consecutive.
       However, order of the loads need to be checked for correctness.  */
    if (!load_p || !reg_overlap_mentioned_p (operands[0], operands[1]))
      {
	output_asm_insn (loongarch_output_move (operands[0], operands[1]),
			 operands);
	output_asm_insn (loongarch_output_move (operands[2], operands[3]),
			 &operands[2]);
      }
    else
      {
	output_asm_insn (loongarch_output_move (operands[2], operands[3]),
			 &operands[2]);
	output_asm_insn (loongarch_output_move (operands[0], operands[1]),
			 operands);
      }
    return "";
  }
  [(set_attr "move_type" "load,fpload,store,fpstore,load,store")
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_extend:SI (match_operand:HI 1 "non_volatile_mem_operand" "m")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(any_extend:SI (match_operand:HI 3 "non_volatile_mem_operand" "m")))]
  "reload_completed"
  {
    /* Reg-renaming pass reuses base register if it is dead after bonded loads.
       Hardware does not bond those loads, even when they are consecutive.
       However, order of the loads need to be checked for correctness.  */
    if (!reg_overlap_mentioned_p (operands[0], operands[1]))
      {
	output_asm_insn ("ld.h<u>\t%0,%1", operands);
	output_asm_insn ("ld.h<u>\t%2,%3", operands);
      }
    else
      {
	output_asm_insn ("ld.h<u>\t%2,%3", operands);
	output_asm_insn ("ld.h<u>\t%0,%1", operands);
      }

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



(define_mode_iterator QHSD [QI HI SI DI])

(define_insn "crc_w_<size>_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:QHSD 1 "register_operand" "r")
		   (match_operand:SI 2 "register_operand" "r")]
		     UNSPEC_CRC))]
  ""
  "crc.w.<size>.w\t%0,%1,%2"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

(define_insn "crcc_w_<size>_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:QHSD 1 "register_operand" "r")
		   (match_operand:SI 2 "register_operand" "r")]
		     UNSPEC_CRCC))]
  ""
  "crcc.w.<size>.w\t%0,%1,%2"
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")])

;; Synchronization instructions.

(include "sync.md")

(include "generic.md")
(include "gs464v.md")

(define_c_enum "unspec" [
  UNSPEC_ADDRESS_FIRST
])
