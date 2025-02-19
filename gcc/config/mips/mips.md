;;  Mips.md	     Machine Description for MIPS based processors
;;  Copyright (C) 1989-2025 Free Software Foundation, Inc.
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  64-bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
;;  Brendan Eich, brendan@microunity.com.

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
  r3000
  4kc
  4kp
  5kc
  5kf
  20kc
  24kc
  24kf2_1
  24kf1_1
  74kc
  74kf2_1
  74kf1_1
  74kf3_2
  loongson_2e
  loongson_2f
  gs464
  gs464e
  gs264e
  m4k
  octeon
  octeon2
  octeon3
  r3900
  r6000
  r4000
  r4100
  r4111
  r4120
  r4130
  r4300
  r4600
  r4650
  r4700
  r5000
  r5400
  r5500
  r5900
  r7000
  r8000
  r9000
  r10000
  sb1
  sb1a
  sr71000
  xlr
  xlp
  p5600
  m5100
  i6400
  p6600
])

(define_c_enum "unspec" [
  ;; Unaligned accesses.
  UNSPEC_LOAD_LEFT
  UNSPEC_LOAD_RIGHT
  UNSPEC_STORE_LEFT
  UNSPEC_STORE_RIGHT

  ;; Integer operations that are too cumbersome to describe directly.
  UNSPEC_WSBH
  UNSPEC_DSBH
  UNSPEC_DSHD

  ;; Floating-point moves.
  UNSPEC_LOAD_LOW
  UNSPEC_LOAD_HIGH
  UNSPEC_STORE_WORD
  UNSPEC_MFHC1
  UNSPEC_MTHC1

  ;; Floating-point environment.
  UNSPEC_GET_FCSR
  UNSPEC_SET_FCSR

  ;; Floating-point unspecs.
  UNSPEC_FMIN
  UNSPEC_FMAX
  UNSPEC_FRINT
  UNSPEC_FCLASS

  ;; HI/LO moves.
  UNSPEC_MFHI
  UNSPEC_MTHI
  UNSPEC_SET_HILO

  ;; GP manipulation.
  UNSPEC_LOADGP
  UNSPEC_COPYGP
  UNSPEC_MOVE_GP
  UNSPEC_POTENTIAL_CPRESTORE
  UNSPEC_CPRESTORE
  UNSPEC_RESTORE_GP
  UNSPEC_EH_RETURN
  UNSPEC_GP
  UNSPEC_SET_GOT_VERSION
  UNSPEC_UPDATE_GOT_VERSION

  ;; Symbolic accesses.
  UNSPEC_LOAD_CALL
  UNSPEC_LOAD_GOT
  UNSPEC_TLS_LDM
  UNSPEC_TLS_GET_TP
  UNSPEC_UNSHIFTED_HIGH

  ;; MIPS16 constant pools.
  UNSPEC_ALIGN
  UNSPEC_CONSTTABLE
  UNSPEC_CONSTTABLE_END
  UNSPEC_CONSTTABLE_INT
  UNSPEC_CONSTTABLE_FLOAT

  ;; Blockage and synchronisation.
  UNSPEC_BLOCKAGE
  UNSPEC_CLEAR_HAZARD
  UNSPEC_RDHWR
  UNSPEC_SYNCI
  UNSPEC_SYNC

  ;; Cache manipulation.
  UNSPEC_MIPS_CACHE
  UNSPEC_R10K_CACHE_BARRIER

  ;; Interrupt handling.
  UNSPEC_ERET
  UNSPEC_DERET
  UNSPEC_DI
  UNSPEC_EHB
  UNSPEC_RDPGPR
  UNSPEC_COP0

  ;; Used in a call expression in place of args_size.  It's present for PIC
  ;; indirect calls where it contains args_size and the function symbol.
  UNSPEC_CALL_ATTR

  ;; MIPS16 casesi jump table dispatch.
  UNSPEC_CASESI_DISPATCH

  ;; Stack checking.
  UNSPEC_PROBE_STACK_RANGE

  ;; The `.insn' pseudo-op.
  UNSPEC_INSN_PSEUDO
  UNSPEC_JRHB

  VUNSPEC_SPECULATION_BARRIER
])

(define_constants
  [(TLS_GET_TP_REGNUM		3)
   (GET_FCSR_REGNUM		2)
   (SET_FCSR_REGNUM		4)
   (PIC_FUNCTION_ADDR_REGNUM	25)
   (GLOBAL_POINTER_REGNUM	28)
   (RETURN_ADDR_REGNUM		31)
   (CPRESTORE_SLOT_REGNUM	76)
   (GOT_VERSION_REGNUM		79)

   ;; PIC long branch sequences are never longer than 100 bytes.
   (MAX_PIC_BRANCH_LENGTH	100)
  ]
)

(include "predicates.md")
(include "constraints.md")

;; ....................
;;
;;	Attributes
;;
;; ....................

(define_attr "got" "unset,xgot_high,load"
  (const_string "unset"))

;; For jal instructions, this attribute is DIRECT when the target address
;; is symbolic and INDIRECT when it is a register.
(define_attr "jal" "unset,direct,indirect"
  (const_string "unset"))

;; This attribute is YES if the instruction is a jal macro (not a
;; real jal instruction).
;;
;; jal is always a macro for TARGET_CALL_CLOBBERED_GP because it includes
;; an instruction to restore $gp.  Direct jals are also macros for
;; !TARGET_ABSOLUTE_JUMPS because they first load the target address
;; into a register.
(define_attr "jal_macro" "no,yes"
  (cond [(eq_attr "jal" "direct")
	 (symbol_ref "(TARGET_CALL_CLOBBERED_GP || !TARGET_ABSOLUTE_JUMPS
		       ? JAL_MACRO_YES : JAL_MACRO_NO)")
	 (eq_attr "jal" "indirect")
	 (symbol_ref "(TARGET_CALL_CLOBBERED_GP
		       ? JAL_MACRO_YES : JAL_MACRO_NO)")]
	(const_string "no")))

;; Classification of moves, extensions and truncations.  Most values
;; are as for "type" (see below) but there are also the following
;; move-specific values:
;;
;; constN	move an N-constraint integer into a MIPS16 register
;; sll0		"sll DEST,SRC,0", which on 64-bit targets is guaranteed
;;		to produce a sign-extended DEST, even if SRC is not
;;		properly sign-extended
;; ext_ins	EXT, DEXT, INS or DINS instruction
;; andi		a single ANDI instruction
;; loadpool	move a constant into a MIPS16 register by loading it
;;		from the pool
;; shift_shift	a shift left followed by a shift right
;;
;; This attribute is used to determine the instruction's length and
;; scheduling type.  For doubleword moves, the attribute always describes
;; the split instructions; in some cases, it is more appropriate for the
;; scheduling type to be "multi" instead.
(define_attr "move_type"
  "unknown,load,fpload,store,fpstore,mtc,mfc,mtlo,mflo,imul,move,fmove,
   const,constN,signext,ext_ins,logical,arith,sll0,andi,loadpool,
   shift_shift"
  (const_string "unknown"))

(define_attr "alu_type" "unknown,add,sub,not,nor,and,or,xor,simd_add"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF,FPSW,
  V2DI,V4SI,V8HI,V16QI,V2DF,V4SF"
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

;; Attributes describing a sync loop.  These loops have the form:
;;
;;       if (RELEASE_BARRIER == YES) sync
;;    1: OLDVAL = *MEM
;;       if ((OLDVAL & INCLUSIVE_MASK) != REQUIRED_OLDVAL) goto 2
;;         CMP  = 0 [delay slot]
;;       $TMP1 = OLDVAL & EXCLUSIVE_MASK
;;       $TMP2 = INSN1 (OLDVAL, INSN1_OP2)
;;       $TMP3 = INSN2 ($TMP2, INCLUSIVE_MASK)
;;       $AT |= $TMP1 | $TMP3
;;       if (!commit (*MEM = $AT)) goto 1.
;;         if (INSN1 != MOVE && INSN1 != LI) NEWVAL = $TMP3 [delay slot]
;;       CMP  = 1
;;       if (ACQUIRE_BARRIER == YES) sync
;;    2:
;;
;; where "$" values are temporaries and where the other values are
;; specified by the attributes below.  Values are specified as operand
;; numbers and insns are specified as enums.  If no operand number is
;; specified, the following values are used instead:
;;
;;    - OLDVAL: $AT
;;    - CMP: NONE
;;    - NEWVAL: $AT
;;    - INCLUSIVE_MASK: -1
;;    - REQUIRED_OLDVAL: OLDVAL & INCLUSIVE_MASK
;;    - EXCLUSIVE_MASK: 0
;;
;; MEM and INSN1_OP2 are required.
;;
;; Ideally, the operand attributes would be integers, with -1 meaning "none",
;; but the gen* programs don't yet support that.
(define_attr "sync_mem" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_oldval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_cmp" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_newval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_inclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_exclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_required_oldval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1_op2" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1" "move,li,addu,addiu,subu,and,andi,or,ori,xor,xori"
  (const_string "move"))
(define_attr "sync_insn2" "nop,and,xor,not"
  (const_string "nop"))
;; Memory model specifier.
;; "0"-"9" values specify the operand that stores the memory model value.
;; "10" specifies MEMMODEL_ACQ_REL,
;; "11" specifies MEMMODEL_ACQUIRE.
(define_attr "sync_memmodel" "" (const_int 10))

;; Performance ratio.  Add this attr to the slow INSNs.
;; Used by mips_insn_cost.
(define_attr "perf_ratio" "" (const_int 0))

;; Accumulator operand for madd patterns.
(define_attr "accum_in" "none,0,1,2,3,4,5" (const_string "none"))

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
;; mtc		transfer to coprocessor
;; mfc		transfer from coprocessor
;; mthi		transfer to a hi register
;; mtlo		transfer to a lo register
;; mfhi		transfer from a hi register
;; mflo		transfer from a lo register
;; const	load constant
;; arith	integer arithmetic instructions
;; logical      integer logical instructions
;; shift	integer shift instructions
;; slt		set less than instructions
;; signext      sign extend instructions
;; clz		the clz and clo instructions
;; pop		the pop instruction
;; trap		trap if instructions
;; imul		integer multiply 2 operands
;; imul3	integer multiply 3 operands
;; imul3nc	integer multiply 3 operands without clobbering HI/LO
;; imadd	integer multiply-add
;; idiv		integer divide 2 operands
;; idiv3	integer divide 3 operands
;; move		integer register move ({,D}ADD{,U} with rt = 0)
;; fmove	floating point register move
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; frdiv	floating point reciprocal divide
;; frdiv1	floating point reciprocal divide step 1
;; frdiv2	floating point reciprocal divide step 2
;; fabs		floating point absolute value
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; frsqrt       floating point reciprocal square root
;; frsqrt1      floating point reciprocal square root step1
;; frsqrt2      floating point reciprocal square root step2
;; fminmax      floating point min/max
;; frint        floating point round to integral
;; fclass       floating point class mask
;; dspmac       DSP MAC instructions not saturating the accumulator
;; dspmacsat    DSP MAC instructions that saturate the accumulator
;; accext       DSP accumulator extract instructions
;; accmod       DSP accumulator modify instructions
;; dspalu       DSP ALU instructions not saturating the result
;; dspalusat    DSP ALU instructions that saturate the result
;; multi	multiword sequence (or user asm statements)
;; atomic	atomic memory update instruction
;; syncloop	memory atomic operation implemented as a sync loop
;; nop		no operation
;; ghost	an instruction that produces no real code
;; multimem	microMIPS multiword load and store
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,fpidxload,store,fpstore,fpidxstore,
   prefetch,prefetchx,condmove,mtc,mfc,mthi,mtlo,mfhi,mflo,const,arith,logical,
   shift,slt,signext,clz,pop,trap,imul,imul3,imul3nc,imadd,idiv,idiv3,move,
   fmove,fadd,fmul,fmadd,fdiv,frdiv,frdiv1,frdiv2,fabs,fneg,fcmp,fcvt,fsqrt,
   frsqrt,frsqrt1,frsqrt2,fminmax,frint,fclass,dspmac,dspmacsat,accext,
   accmod,dspalu,dspalusat,multi,atomic,syncloop,nop,ghost,multimem,
   simd_div,simd_fclass,simd_flog2,simd_fadd,simd_fcvt,simd_fmul,simd_fmadd,
   simd_fdiv,simd_bitins,simd_bitmov,simd_insert,simd_sld,simd_mul,simd_fcmp,
   simd_fexp2,simd_int_arith,simd_bit,simd_shift,simd_splat,simd_fill,
   simd_permute,simd_shf,simd_sat,simd_pcnt,simd_copy,simd_branch,simd_cmsa,
   simd_fminmax,simd_logic,simd_move,simd_load,simd_store"
  (cond [(eq_attr "jal" "!unset") (const_string "call")
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
	 (eq_attr "move_type" "mtc") (const_string "mtc")
	 (eq_attr "move_type" "mfc") (const_string "mfc")
	 (eq_attr "move_type" "mtlo") (const_string "mtlo")
	 (eq_attr "move_type" "mflo") (const_string "mflo")

	 ;; These types of move are always single insns.
	 (eq_attr "move_type" "imul") (const_string "imul")
	 (eq_attr "move_type" "fmove") (const_string "fmove")
	 (eq_attr "move_type" "loadpool") (const_string "load")
	 (eq_attr "move_type" "signext") (const_string "signext")
	 (eq_attr "move_type" "ext_ins") (const_string "arith")
	 (eq_attr "move_type" "arith") (const_string "arith")
	 (eq_attr "move_type" "logical") (const_string "logical")
	 (eq_attr "move_type" "sll0") (const_string "shift")
	 (eq_attr "move_type" "andi") (const_string "logical")

	 ;; These types of move are always split.
	 (eq_attr "move_type" "constN,shift_shift")
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
	 (eq_attr "move_type" "const") (const_string "const")
	 (eq_attr "sync_mem" "!none") (const_string "syncloop")]
	(const_string "unknown")))

(define_attr "compact_form" "always,maybe,never"
  (cond [(eq_attr "jal" "direct")
	 (const_string "always")
	 (eq_attr "jal" "indirect")
	 (const_string "maybe")
	 (eq_attr "type" "jump")
	 (const_string "maybe")]
	(const_string "never")))

;; Mode for conversion types (fcvt)
;; I2S          integer to float single (SI/DI to SF)
;; I2D          integer to float double (SI/DI to DF)
;; S2I          float to integer (SF to SI/DI)
;; D2I          float to integer (DF to SI/DI)
;; D2S          double to float single
;; S2D          float single to double

(define_attr "cnv_mode" "unknown,I2S,I2D,S2I,D2I,D2S,S2D" 
  (const_string "unknown"))

;; Is this an extended instruction in mips16 mode?
(define_attr "extended_mips16" "no,yes"
  (if_then_else (ior ;; In general, constant-pool loads are extended
  		     ;; instructions.  We don't yet optimize for 16-bit
		     ;; PC-relative references.
		     (eq_attr "move_type" "sll0,loadpool,ext_ins")
		     (eq_attr "jal" "direct")
		     (eq_attr "got" "load"))
		(const_string "yes")
		(const_string "no")))

(define_attr "compression" "none,all,micromips32,micromips"
  (const_string "none"))

(define_attr "enabled" "no,yes"
  (cond [;; The o32 FPXX and FP64A ABI extensions prohibit direct moves between
	 ;; GR_REG and FR_REG for 64-bit values.
	 (and (eq_attr "move_type" "mtc,mfc")
	      (match_test "(TARGET_FLOATXX && !ISA_HAS_MXHC1)
			   || TARGET_O32_FP64A_ABI")
	      (eq_attr "dword_mode" "yes"))
	 (const_string "no")
	 (and (eq_attr "compression" "micromips32,micromips")
	      (match_test "!TARGET_MICROMIPS"))
	 (const_string "no")]
	(const_string "yes")))

;; The number of individual instructions that a non-branch pattern generates,
;; using units of BASE_INSN_LENGTH.
(define_attr "insn_count" ""
  (cond [;; "Ghost" instructions occupy no space.
	 (eq_attr "type" "ghost")
	 (const_int 0)

	 ;; Extended instructions count as 2.
   	 (and (eq_attr "extended_mips16" "yes")
	      (match_test "TARGET_MIPS16"))
	 (const_int 2)

	 ;; A GOT load followed by an add of $gp.  This is not used for MIPS16.
	 (eq_attr "got" "xgot_high")
	 (const_int 2)

	 ;; SHIFT_SHIFTs are decomposed into two separate instructions.
	 ;; They are extended instructions on MIPS16 targets.
	 (eq_attr "move_type" "shift_shift")
	 (if_then_else (match_test "TARGET_MIPS16")
	 	       (const_int 4)
	 	       (const_int 2))

	 ;; Check for doubleword moves that are decomposed into two
	 ;; instructions.  The individual instructions are unextended
	 ;; MIPS16 ones.
	 (and (eq_attr "move_type" "mtc,mfc,mtlo,mflo,move")
	      (eq_attr "dword_mode" "yes"))
	 (const_int 2)

	 ;; Check for quadword moves that are decomposed into four
	 ;; instructions.
	 (and (eq_attr "move_type" "mtc,mfc,move")
	      (eq_attr "qword_mode" "yes"))
	 (const_int 4)

	 ;; Constants, loads and stores are handled by external routines.
	 (and (eq_attr "move_type" "const,constN")
	      (eq_attr "dword_mode" "yes"))
	 (symbol_ref "mips_split_const_insns (operands[1])")
	 (eq_attr "move_type" "const,constN")
	 (symbol_ref "mips_const_insns (operands[1])")
	 (eq_attr "move_type" "load,fpload")
	 (symbol_ref "mips_load_store_insns (operands[1], insn)")
	 (eq_attr "move_type" "store,fpstore")
	 (symbol_ref "mips_load_store_insns (operands[0], insn)
		      + (TARGET_FIX_24K ? 1 : 0)")

	 ;; In the worst case, a call macro will take 8 instructions:
	 ;;
	 ;;	lui $25,%call_hi(FOO)
	 ;;	addu $25,$25,$28
	 ;;	lw $25,%call_lo(FOO)($25)
	 ;;	nop
	 ;;	jalr $25
	 ;;	nop
	 ;;	lw $gp,X($sp)
	 ;;	nop
	 (eq_attr "jal_macro" "yes")
	 (const_int 8)

	 ;; Various VR4120 errata require a nop to be inserted after a macc
	 ;; instruction.  The assembler does this for us, so account for
	 ;; the worst-case length here.
	 (and (eq_attr "type" "imadd")
	      (match_test "TARGET_FIX_VR4120"))
	 (const_int 2)

	 ;; VR4120 errata MD(4): if there are consecutive dmult instructions,
	 ;; the result of the second one is missed.  The assembler should work
	 ;; around this by inserting a nop after the first dmult.
	 (and (eq_attr "type" "imul,imul3")
	      (eq_attr "mode" "DI")
	      (match_test "TARGET_FIX_VR4120"))
	 (const_int 2)

	 (eq_attr "type" "idiv,idiv3")
	 (symbol_ref "mips_idiv_insns (GET_MODE (PATTERN (insn)))")

	 ;; simd div have 3 instruction if TARGET_CHECK_ZERO_DIV is true.
	 (eq_attr "type" "simd_div")
	 (if_then_else (match_test "TARGET_CHECK_ZERO_DIV")
		       (const_int 3)
		       (const_int 1))

	 (not (eq_attr "sync_mem" "none"))
	 (symbol_ref "mips_sync_loop_insns (insn, operands)")]
	(const_int 1)))

;; Length of instruction in bytes.  The default is derived from "insn_count",
;; but there are special cases for branches (which must be handled here)
;; and for compressed single instructions.
(define_attr "length" ""
   (cond [(and (ior (eq_attr "compression" "micromips,all")
		    (and (eq_attr "compression" "micromips32")
			 (eq_attr "mode" "SI,SF")))
	       (eq_attr "dword_mode" "no")
	       (match_test "TARGET_MICROMIPS"))
	  (const_int 2)

	  ;; Direct microMIPS branch instructions have a range of
	  ;; [-0x10000,0xfffe], otherwise the range is [-0x20000,0x1fffc].
	  ;; If a branch is outside this range, we have a choice of two
	  ;; sequences.
	  ;;
	  ;; For PIC, an out-of-range branch like:
	  ;;
	  ;;	bne	r1,r2,target
	  ;;	dslot
	  ;;
	  ;; becomes the equivalent of:
	  ;;
	  ;;	beq	r1,r2,1f
	  ;;	dslot
	  ;;	la	$at,target
	  ;;	jr	$at
	  ;;	nop
	  ;; 1:
	  ;;
	  ;; The non-PIC case is similar except that we use a direct
	  ;; jump instead of an la/jr pair.  Since the target of this
	  ;; jump is an absolute 28-bit bit address (the other bits
	  ;; coming from the address of the delay slot) this form cannot
	  ;; cross a 256MB boundary.  We could provide the option of
	  ;; using la/jr in this case too, but we do not do so at
	  ;; present.
	  ;;
	  ;; The value we specify here does not account for the delay slot
	  ;; instruction, whose length is added separately.  If the RTL
	  ;; pattern has no explicit delay slot, mips_adjust_insn_length
	  ;; will add the length of the implicit nop.  The range of
	  ;; [-0x20000, 0x1fffc] from the address of the delay slot
	  ;; therefore translates to a range of:
	  ;;
	  ;;    [-(0x20000 - sizeof (branch)), 0x1fffc - sizeof (slot)]
	  ;; == [-0x1fffc, 0x1fff8]
	  ;;
	  ;; from the shorten_branches reference address.
	  (and (eq_attr "type" "branch")
	       (not (match_test "TARGET_MIPS16")))
	  (cond [;; Any variant can handle the 17-bit range.
		 (and (le (minus (match_dup 0) (pc)) (const_int 65532))
		      (le (minus (pc) (match_dup 0)) (const_int 65534)))
		   (const_int 4)

		 ;; The 18-bit range is OK other than for microMIPS.
		 (and (not (match_test "TARGET_MICROMIPS"))
		      (and (le (minus (match_dup 0) (pc)) (const_int 131064))
		      	   (le (minus (pc) (match_dup 0)) (const_int 131068))))
		   (const_int 4)

		 ;; The non-PIC case: branch, first delay slot, and J.
		 (match_test "TARGET_ABSOLUTE_JUMPS")
		   (const_int 12)]

		 ;; Use MAX_PIC_BRANCH_LENGTH as a (gross) overestimate.
		 ;; mips_adjust_insn_length substitutes the correct length.
		 ;;
		 ;; Note that we can't simply use (symbol_ref ...) here
		 ;; because genattrtab needs to know the maximum length
		 ;; of an insn.
		 (const_int MAX_PIC_BRANCH_LENGTH))

	  ;; An unextended MIPS16 branch has a range of [-0x100, 0xfe]
	  ;; from the address of the following instruction, which leads
	  ;; to a range of:
	  ;;
	  ;;    [-(0x100 - sizeof (branch)), 0xfe]
	  ;; == [-0xfe, 0xfe]
	  ;;
	  ;; from the shorten_branches reference address.  Extended branches
	  ;; likewise have a range of [-0x10000, 0xfffe] from the address
	  ;; of the following instruction, which leads to a range of:
	  ;;
	  ;;    [-(0x10000 - sizeof (branch)), 0xfffe]
	  ;; == [-0xfffc, 0xfffe]
	  ;;
	  ;; from the reference address.
	  ;;
	  ;; When a branch is out of range, mips_reorg splits it into a form
	  ;; that uses in-range branches.  There are four basic sequences:
	  ;;
	  ;; (1) Absolute addressing with a readable text segment
	  ;;     (32-bit addresses):
	  ;;
	  ;;	 b... foo		2 bytes
	  ;;	 move $1,$2		2 bytes
	  ;;     lw $2,label		2 bytes
	  ;;	 jr $2			2 bytes
	  ;;	 move $2,$1		2 bytes
	  ;;	 .align 2		0 or 2 bytes
	  ;; label:
	  ;;	 .word target		4 bytes
	  ;; foo:
	  ;;				(16 bytes in the worst case)
	  ;;
	  ;; (2) Absolute addressing with a readable text segment
	  ;;     (64-bit addresses):
	  ;;
	  ;;	 b... foo		2 bytes
	  ;;	 move $1,$2		2 bytes
	  ;;     ld $2,label		2 bytes
	  ;;	 jr $2			2 bytes
	  ;;	 move $2,$1		2 bytes
	  ;;	 .align 3		0 to 6 bytes
	  ;; label:
	  ;;	 .dword target		8 bytes
	  ;; foo:
	  ;;				(24 bytes in the worst case)
	  ;;
	  ;; (3) Absolute addressing without a readable text segment
	  ;;     (which requires 32-bit addresses at present):
	  ;;
	  ;;	 b... foo		2 bytes
	  ;;	 move $1,$2		2 bytes
	  ;;     lui $2,%hi(target)	4 bytes
	  ;;	 sll $2,8		2 bytes
	  ;;	 sll $2,8		2 bytes
	  ;;     addiu $2,%lo(target)	4 bytes
	  ;;	 jr $2			2 bytes
	  ;;	 move $2,$1		2 bytes
	  ;; foo:
	  ;;				(20 bytes)
	  ;;
	  ;; (4) PIC addressing (which requires 32-bit addresses at present):
	  ;;
	  ;;	 b... foo		2 bytes
	  ;;	 move $1,$2		2 bytes
	  ;;     lw $2,cprestore	0, 2 or 4 bytes
	  ;;	 lw $2,%got(target)($2)	4 bytes
	  ;;     addiu $2,%lo(target)	4 bytes
	  ;;	 jr $2			2 bytes
	  ;;	 move $2,$1		2 bytes
	  ;; foo:
	  ;;				(20 bytes in the worst case)
	  (and (eq_attr "type" "branch")
	       (match_test "TARGET_MIPS16"))
	  (cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		      (le (minus (pc) (match_dup 0)) (const_int 254)))
		 (const_int 2)
		 (and (le (minus (match_dup 0) (pc)) (const_int 65534))
		      (le (minus (pc) (match_dup 0)) (const_int 65532)))
		 (const_int 4)
		 (and (match_test "TARGET_ABICALLS")
		      (not (match_test "TARGET_ABSOLUTE_ABICALLS")))
		 (const_int 20)
		 (match_test "Pmode == SImode")
		 (const_int 16)
		 ] (const_int 24))]
	 (symbol_ref "get_attr_insn_count (insn) * BASE_INSN_LENGTH")))

;; Attribute describing the processor.
(define_enum_attr "cpu" "processor"
  (const (symbol_ref "mips_tune")))

;; The type of hardware hazard associated with this instruction.
;; DELAY means that the next instruction cannot read the result
;; of this one.  HILO means that the next two instructions cannot
;; write to HI or LO.
(define_attr "hazard" "none,delay,hilo,forbidden_slot"
  (cond [(and (eq_attr "type" "load,fpload,fpidxload")
	      (match_test "ISA_HAS_LOAD_DELAY"))
	 (const_string "delay")

	 (and (eq_attr "type" "mfc,mtc")
	      (match_test "ISA_HAS_XFER_DELAY"))
	 (const_string "delay")

	 (and (eq_attr "type" "fcmp")
	      (match_test "ISA_HAS_FCMP_DELAY"))
	 (const_string "delay")

	 ;; The r4000 multiplication patterns include an mflo instruction.
	 (and (eq_attr "type" "imul")
	      (match_test "TARGET_FIX_R4000"))
	 (const_string "hilo")

	 (and (eq_attr "type" "mfhi,mflo")
	      (not (match_test "ISA_HAS_HILO_INTERLOCKS")))
	 (const_string "hilo")]
	(const_string "none")))

;; Can the instruction be put into a delay slot?
(define_attr "can_delay" "no,yes"
  (if_then_else (and (eq_attr "type" "!branch,call,jump,simd_branch")
		     (eq_attr "hazard" "none")
		     (match_test "get_attr_insn_count (insn) == 1"))
		(const_string "yes")
		(const_string "no")))

;; Attribute defining whether or not we can use the branch-likely
;; instructions.
(define_attr "branch_likely" "no,yes"
  (if_then_else (match_test "GENERATE_BRANCHLIKELY")
		(const_string "yes")
		(const_string "no")))

;; True if an instruction might assign to hi or lo when reloaded.
;; This is used by the TUNE_MACC_CHAINS code.
(define_attr "may_clobber_hilo" "no,yes"
  (if_then_else (eq_attr "type" "imul,imul3,imadd,idiv,mthi,mtlo")
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

(define_mode_iterator MOVEP1 [SI SF])
(define_mode_iterator MOVEP2 [SI SF])
(define_mode_iterator JOIN_MODE [HI
				 SI
				 (SF "TARGET_HARD_FLOAT")
				 (DF "TARGET_HARD_FLOAT
				      && TARGET_DOUBLE_FLOAT")])

;; This mode iterator allows :HILO to be used as the mode of the
;; concatenated HI and LO registers.
(define_mode_iterator HILO [(DI "!TARGET_64BIT") (TI "TARGET_64BIT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; This mode iterator allows :MOVECC to be used anywhere that a
;; conditional-move-type condition is needed.
(define_mode_iterator MOVECC [SI (DI "TARGET_64BIT")
                              (CC "TARGET_HARD_FLOAT
				   && !TARGET_LOONGSON_2EF
				   && !TARGET_MIPS5900")
                              (CCE "TARGET_HARD_FLOAT
				   && !TARGET_LOONGSON_2EF
				   && !TARGET_MIPS5900")])

;; This mode iterator allows :FPCC to be used anywhere that an FP condition
;; is needed.
(define_mode_iterator FPCC [(CC "!ISA_HAS_CCF") (CCE "!ISA_HAS_CCF")
			    (CCF "ISA_HAS_CCF")])

;; 32-bit integer moves for which we provide move patterns.
(define_mode_iterator IMOVE32
  [SI
   (V2HI "TARGET_DSP")
   (V4QI "TARGET_DSP")
   (V2HQ "TARGET_DSP")
   (V2UHQ "TARGET_DSP")
   (V2HA "TARGET_DSP")
   (V2UHA "TARGET_DSP")
   (V4QQ "TARGET_DSP")
   (V4UQQ "TARGET_DSP")])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64
  [DI DF
   (V2SF "TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT")
   (V2SI "TARGET_HARD_FLOAT && TARGET_LOONGSON_MMI")
   (V4HI "TARGET_HARD_FLOAT && TARGET_LOONGSON_MMI")
   (V8QI "TARGET_HARD_FLOAT && TARGET_LOONGSON_MMI")])

;; 128-bit modes for which we provide move patterns on 64-bit targets.
(define_mode_iterator MOVE128 [TI TF])

;; This mode iterator allows the QI and HI extension patterns to be
;; defined from the same template.
(define_mode_iterator SHORT [QI HI])

;; Likewise the 64-bit truncate-and-shift patterns.
(define_mode_iterator SUBDI [QI HI SI])

;; This mode iterator allows :ANYF to be used wherever a scalar or vector
;; floating-point mode is allowed.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT")
			    (DF "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT")
			    (V2SF "TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT")])

;; Like ANYF, but only applies to scalar modes.
(define_mode_iterator SCALARF [(SF "TARGET_HARD_FLOAT")
			       (DF "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT")])

;; A floating-point mode for which moves involving FPRs may need to be split.
(define_mode_iterator SPLITF
  [(DF "!TARGET_64BIT && TARGET_DOUBLE_FLOAT")
   (DI "!TARGET_64BIT && TARGET_DOUBLE_FLOAT")
   (V2SF "!TARGET_64BIT && TARGET_PAIRED_SINGLE_FLOAT")
   (V2SI "!TARGET_64BIT && TARGET_LOONGSON_MMI")
   (V4HI "!TARGET_64BIT && TARGET_LOONGSON_MMI")
   (V8QI "!TARGET_64BIT && TARGET_LOONGSON_MMI")
   (TF "TARGET_64BIT && TARGET_FLOAT64")])

;; In GPR templates, a string like "<d>subu" will expand to "subu" in the
;; 32-bit version and "dsubu" in the 64-bit version.
(define_mode_attr d [(SI "") (DI "d")
		     (QQ "") (HQ "") (SQ "") (DQ "d")
		     (UQQ "") (UHQ "") (USQ "") (UDQ "d")
		     (HA "") (SA "") (DA "d")
		     (UHA "") (USA "") (UDA "d")])

;; Same as d but upper-case.
(define_mode_attr D [(SI "") (DI "D")
		     (QQ "") (HQ "") (SQ "") (DQ "D")
		     (UQQ "") (UHQ "") (USQ "") (UDQ "D")
		     (HA "") (SA "") (DA "D")
		     (UHA "") (USA "") (UDA "D")])

;; This attribute gives the length suffix for a load or store instruction.
;; The same suffixes work for zero and sign extensions.
(define_mode_attr size [(QI "b") (HI "h") (SI "w") (DI "d")])
(define_mode_attr SIZE [(QI "B") (HI "H") (SI "W") (DI "D")])

;; This attributes gives the mode mask of a SHORT.
(define_mode_attr mask [(QI "0x00ff") (HI "0xffff")])

;; Mode attributes for GPR loads.
(define_mode_attr load [(SI "lw") (DI "ld")])
;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd")])

;; Similarly for MIPS IV indexed FPR loads and stores.
(define_mode_attr loadx [(SF "lwxc1") (DF "ldxc1") (V2SF "ldxc1")])
(define_mode_attr storex [(SF "swxc1") (DF "sdxc1") (V2SF "sdxc1")])

;; The unextended ranges of the MIPS16 addiu and daddiu instructions
;; are different.  Some forms of unextended addiu have an 8-bit immediate
;; field but the equivalent daddiu has only a 5-bit field.
(define_mode_attr si8_di5 [(SI "8") (DI "5")])

;; This attribute gives the best constraint to use for registers of
;; a given mode.
(define_mode_attr reg [(SI "d") (DI "d") (CC "z") (CCE "z") (CCF "f")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d") (V2SF "ps")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode or vector mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF") (V2SF "SF") (V4SF "SF")
			    (V16QI "QI") (V8HI "HI") (V4SI "SI") (V2DI "DI")
			    (V2DF "DF")])

;; As above, but in lower case.
(define_mode_attr unitmode [(SF "sf") (DF "df") (V2SF "sf") (V4SF "sf")
			    (V16QI "qi") (V8QI "qi") (V8HI "hi") (V4HI "hi")
			    (V4SI "si") (V2SI "si") (V2DI "di") (V2DF "df")])

;; This attribute gives the integer mode that has the same size as a
;; fixed-point mode.
(define_mode_attr IMODE [(QQ "QI") (HQ "HI") (SQ "SI") (DQ "DI")
			 (UQQ "QI") (UHQ "HI") (USQ "SI") (UDQ "DI")
			 (HA "HI") (SA "SI") (DA "DI")
			 (UHA "HI") (USA "SI") (UDA "DI")
			 (V4UQQ "SI") (V2UHQ "SI") (V2UHA "SI")
			 (V2HQ "SI") (V2HA "SI")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (V2SF "SI")
			    (V2SI "SI") (V4HI "SI") (V8QI "SI")
			    (TF "DI")])

;; This attribute works around the early SB-1 rev2 core "F2" erratum:
;;
;; In certain cases, div.s and div.ps may have a rounding error
;; and/or wrong inexact flag.
;;
;; Therefore, we only allow div.s if not working around SB-1 rev2
;; errata or if a slight loss of precision is OK.
(define_mode_attr divide_condition
  [DF (SF "!TARGET_FIX_SB1 || flag_unsafe_math_optimizations")
   (V2SF "TARGET_SB1 && (!TARGET_FIX_SB1 || flag_unsafe_math_optimizations)")])

;; This attribute gives the conditions under which SQRT.fmt instructions
;; can be used.
(define_mode_attr sqrt_condition
  [(SF "!ISA_MIPS1") (DF "!ISA_MIPS1") (V2SF "TARGET_SB1")])

;; This attribute provides the correct mnemonic for each FP condition mode.
(define_mode_attr fpcmp [(CC "c") (CCE "c") (CCF "cmp")])

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
(define_code_iterator any_div [div udiv])

;; This code iterator allows unsigned and signed modulus to be generated
;; from the same template.
(define_code_iterator any_mod [mod umod])

;; This code iterator allows addition and subtraction to be generated
;; from the same template.
(define_code_iterator addsub [plus minus])

;; This code iterator allows all native floating-point comparisons to be
;; generated from the same template.
(define_code_iterator fcond [unordered uneq unlt unle eq lt le
			     (ordered "ISA_HAS_CCF")
			     (ltgt "ISA_HAS_CCF")
			     (ne "ISA_HAS_CCF")])

;; This code iterator is used for comparisons that can be implemented
;; by swapping the operands.
(define_code_iterator swapped_fcond [ge gt unge ungt])

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
			(minus "subu")])

;; <immediate_insn> expands to the name of the insn that implements
;; a particular code to operate on immediate values.
(define_code_attr immediate_insn [(ior "ori")
				  (xor "xori")
				  (and "andi")])

(define_code_attr shift_compression [(ashift "micromips32")
				     (lshiftrt "micromips32")
				     (ashiftrt "none")])

;; <fcond> is the c.cond.fmt condition associated with a particular code.
(define_code_attr fcond [(unordered "un")
			 (uneq "ueq")
			 (unlt "ult")
			 (unle "ule")
			 (eq "eq")
			 (lt "lt")
			 (le "le")
			 (ordered "or")
			 (ltgt "sne")
			 (ne "une")])

;; Similar, but for swapped conditions.
(define_code_attr swapped_fcond [(ge "le")
				 (gt "lt")
				 (unge "ule")
				 (ungt "ult")])

;; The value of the bit when the branch is taken for branch_bit patterns.
;; Comparison is always against zero so this depends on the operator.
(define_code_attr bbv [(eq "0") (ne "1")])

;; This is the inverse value of bbv.
(define_code_attr bbinv [(eq "1") (ne "0")])

;; The sel mnemonic to use depending on the condition test.
(define_code_attr sel [(eq "seleqz") (ne "selnez")])
(define_code_attr selinv [(eq "selnez") (ne "seleqz")])

;; .........................
;;
;;	Branch, call and jump delay slots
;;
;; .........................

(define_delay (and (eq_attr "type" "branch")
		   (not (match_test "TARGET_MIPS16"))
		   (eq_attr "branch_likely" "yes"))
  [(eq_attr "can_delay" "yes")
   (nil)
   (eq_attr "can_delay" "yes")])

;; Branches that have delay slots and don't have likely variants do
;; not annul on false.
(define_delay (and (eq_attr "type" "branch,simd_branch")
		   (not (match_test "TARGET_MIPS16"))
		   (ior (match_test "TARGET_CB_NEVER")
			(and (eq_attr "compact_form" "maybe")
			     (not (match_test "TARGET_CB_ALWAYS")))
			(eq_attr "compact_form" "never"))
		   (eq_attr "branch_likely" "no"))
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

(define_delay (and (eq_attr "type" "jump")
		   (ior (match_test "TARGET_CB_NEVER")
			(and (eq_attr "compact_form" "maybe")
			     (not (match_test "TARGET_CB_ALWAYS")))
			(eq_attr "compact_form" "never")))
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

;; Call type instructions should never have a compact form as the
;; type is only used for MIPS16 patterns.  For safety put the compact
;; branch detection condition in anyway.
(define_delay (and (eq_attr "type" "call")
		   (eq_attr "jal_macro" "no")
		   (ior (match_test "TARGET_CB_NEVER")
			(and (eq_attr "compact_form" "maybe")
			     (not (match_test "TARGET_CB_ALWAYS")))
			(eq_attr "compact_form" "never")))
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

;; Pipeline descriptions.
;;
;; generic.md provides a fallback for processors without a specific
;; pipeline description.  It is derived from the old define_function_unit
;; version and uses the "alu" and "imuldiv" units declared below.
;;
;; Some of the processor-specific files are also derived from old
;; define_function_unit descriptions and simply override the parts of
;; generic.md that don't apply.  The other processor-specific files
;; are self-contained.
(define_automaton "alu,imuldiv")

(define_cpu_unit "alu" "alu")
(define_cpu_unit "imuldiv" "imuldiv")

;; Ghost instructions produce no real code and introduce no hazards.
;; They exist purely to express an effect on dataflow.
(define_insn_reservation "ghost" 0
  (eq_attr "type" "ghost")
  "nothing")

(include "i6400.md")
(include "p5600.md")
(include "m5100.md")
(include "p6600.md")
(include "4k.md")
(include "5k.md")
(include "20kc.md")
(include "24k.md")
(include "74k.md")
(include "3000.md")
(include "4000.md")
(include "4100.md")
(include "4130.md")
(include "4300.md")
(include "4600.md")
(include "5000.md")
(include "5400.md")
(include "5500.md")
(include "6000.md")
(include "7000.md")
(include "9000.md")
(include "10000.md")
(include "loongson2ef.md")
(include "gs464.md")
(include "gs464e.md")
(include "gs264e.md")
(include "octeon.md")
(include "sb1.md")
(include "sr71k.md")
(include "xlr.md")
(include "xlp.md")
(include "generic.md")

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
  if (ISA_HAS_COND_TRAP)
    return "teq\t$0,$0";
  else if (TARGET_MIPS16)
    return "break 0";
  else
    return "break";
}
  [(set_attr "type" "trap")])

(define_expand "ctrap<mode>4"
  [(trap_if (match_operator 0 "comparison_operator"
			    [(match_operand:GPR 1 "reg_or_0_operand")
			     (match_operand:GPR 2 "arith_operand")])
	    (match_operand 3 "const_0_operand"))]
  "ISA_HAS_COND_TRAPI || ISA_HAS_COND_TRAP"
{
  mips_expand_conditional_trap (operands[0]);
  DONE;
})

(define_insn "*conditional_trap_reg<mode>"
  [(trap_if (match_operator:GPR 0 "trap_comparison_operator"
				[(match_operand:GPR 1 "reg_or_0_operand" "dJ")
				 (match_operand:GPR 2 "reg_or_0_operand" "dJ")])
	    (const_int 0))]
  "ISA_HAS_COND_TRAP && !ISA_HAS_COND_TRAPI"
  "t%C0\t%z1,%z2"
  [(set_attr "type" "trap")])

(define_insn "*conditional_trap<mode>"
  [(trap_if (match_operator:GPR 0 "trap_comparison_operator"
				[(match_operand:GPR 1 "reg_or_0_operand" "dJ")
				 (match_operand:GPR 2 "arith_operand" "dI")])
	    (const_int 0))]
  "ISA_HAS_COND_TRAPI"
  "t%C0\t%z1,%2"
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
  "add.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "add<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(plus:GPR (match_operand:GPR 1 "register_operand")
		  (match_operand:GPR 2 "arith_operand")))]
  "")

(define_insn "*add<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d,!u,!u,!ks,!d,d")
	(plus:GPR (match_operand:GPR 1 "register_operand" "!u,d,!u,!ks,!ks,0,d")
		  (match_operand:GPR 2 "arith_operand" "!u,d,Uead,Uuw6,Uesp,Usb4,Q")))]
  "!TARGET_MIPS16"
{
  if (which_alternative == 0 
      || which_alternative == 1)
    return "<d>addu\t%0,%1,%2";
  else
    return "<d>addiu\t%0,%1,%2";
}
  [(set_attr "alu_type" "add")
   (set_attr "compression" "micromips32,*,micromips32,micromips32,micromips32,micromips32,*")
   (set_attr "mode" "<MODE>")])

(define_insn "*add<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=ks,ks,d,d,d,d,d,d,d")
	(plus:GPR (match_operand:GPR 1 "register_operand" "ks,ks,ks,ks,0,0,d,d,d")
		  (match_operand:GPR 2 "arith_operand" "Usd8,Q,Uuw<si8_di5>,Q,Usb<si8_di5>,Q,Usb4,O,d")))]
  "TARGET_MIPS16"
  "@
    <d>addiu\t%0,%2
    <d>addiu\t%0,%2
    <d>addiu\t%0,%1,%2
    <d>addiu\t%0,%1,%2
    <d>addiu\t%0,%2
    <d>addiu\t%0,%2
    <d>addiu\t%0,%1,%2
    <d>addiu\t%0,%1,%2
    <d>addu\t%0,%1,%2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "no,yes,no,yes,no,yes,no,yes,no")])

;; On the mips16, we can sometimes split an add of a constant which is
;; a 4 byte instruction into two adds which are both 2 byte
;; instructions.  There are two cases: one where we are adding a
;; constant plus a register to another register, and one where we are
;; simply adding a constant to a register.

(define_split
  [(set (match_operand:SI 0 "d_operand")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "const_int_operand")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) > 0x7f
	&& INTVAL (operands[1]) <= 0x7f + 0x7f)
       || (INTVAL (operands[1]) < - 0x80
	   && INTVAL (operands[1]) >= - 0x80 - 0x80))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val >= 0)
    {
      operands[1] = GEN_INT (0x7f);
      operands[2] = GEN_INT (val - 0x7f);
    }
  else
    {
      operands[1] = GEN_INT (- 0x80);
      operands[2] = GEN_INT (val + 0x80);
    }
})

(define_split
  [(set (match_operand:SI 0 "d_operand")
	(plus:SI (match_operand:SI 1 "d_operand")
		 (match_operand:SI 2 "const_int_operand")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && REGNO (operands[0]) != REGNO (operands[1])
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0x7f)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x80))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x7);
      operands[3] = GEN_INT (val - 0x7);
    }
  else
    {
      operands[2] = GEN_INT (- 0x8);
      operands[3] = GEN_INT (val + 0x8);
    }
})

(define_split
  [(set (match_operand:DI 0 "d_operand")
	(plus:DI (match_dup 0)
		 (match_operand:DI 1 "const_int_operand")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) > 0xf
	&& INTVAL (operands[1]) <= 0xf + 0xf)
       || (INTVAL (operands[1]) < - 0x10
	   && INTVAL (operands[1]) >= - 0x10 - 0x10))"
  [(set (match_dup 0) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 2)))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val >= 0)
    {
      operands[1] = GEN_INT (0xf);
      operands[2] = GEN_INT (val - 0xf);
    }
  else
    {
      operands[1] = GEN_INT (- 0x10);
      operands[2] = GEN_INT (val + 0x10);
    }
})

(define_split
  [(set (match_operand:DI 0 "d_operand")
	(plus:DI (match_operand:DI 1 "d_operand")
		 (match_operand:DI 2 "const_int_operand")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && REGNO (operands[0]) != REGNO (operands[1])
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0xf)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x10))"
  [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 3)))]
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x7);
      operands[3] = GEN_INT (val - 0x7);
    }
  else
    {
      operands[2] = GEN_INT (- 0x8);
      operands[3] = GEN_INT (val + 0x8);
    }
})

(define_insn "*addsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" "d,d")
		      (match_operand:SI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
    addu\t%0,%1,%2
    addiu\t%0,%1,%2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")])

;; Split this insn so that the addiu splitters can have a crack at it.
;; Use a conservative length estimate until the split.
(define_insn_and_split "*addsi3_extended_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" "0,d,d")
		      (match_operand:SI 2 "arith_operand" "Q,O,d"))))]
  "TARGET_64BIT && TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (plus:SI (match_dup 1) (match_dup 2)))]
  { operands[3] = gen_lowpart (SImode, operands[0]); }
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "yes")])

;; Combiner patterns for unsigned byte-add.

(define_insn "*baddu_si_eb"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI
	 (subreg:QI
	  (plus:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")) 3)))]
  "ISA_HAS_BADDU && BYTES_BIG_ENDIAN"
  "baddu\\t%0,%1,%2"
  [(set_attr "alu_type" "add")])

(define_insn "*baddu_si_el"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI
	 (subreg:QI
	  (plus:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")) 0)))]
  "ISA_HAS_BADDU && !BYTES_BIG_ENDIAN"
  "baddu\\t%0,%1,%2"
  [(set_attr "alu_type" "add")])

(define_insn "*baddu_di<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR
	 (truncate:QI
	  (plus:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:DI 2 "register_operand" "d")))))]
  "ISA_HAS_BADDU && TARGET_64BIT"
  "baddu\\t%0,%1,%2"
  [(set_attr "alu_type" "add")])

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
  "sub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d")
	(minus:GPR (match_operand:GPR 1 "register_operand" "!u,d")
		   (match_operand:GPR 2 "register_operand" "!u,d")))]
  ""
  "<d>subu\t%0,%1,%2"
  [(set_attr "alu_type" "sub")
   (set_attr "compression" "micromips32,*")
   (set_attr "mode" "<MODE>")])

(define_insn "*subsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "register_operand" "d")
		      (match_operand:SI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "subu\t%0,%1,%2"
  [(set_attr "alu_type" "sub")
   (set_attr "mode" "DI")])

;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_expand "mul<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand")
	(mult:SCALARF (match_operand:SCALARF 1 "register_operand")
		      (match_operand:SCALARF 2 "register_operand")))]
  ""
  "")

(define_insn "*mul<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(mult:SCALARF (match_operand:SCALARF 1 "register_operand" "f")
		      (match_operand:SCALARF 2 "register_operand" "f")))]
  "!TARGET_4300_MUL_FIX"
  "mul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<MODE>")])

;; Early VR4300 silicon has a CPU bug where multiplies with certain
;; operands may corrupt immediately following multiplies. This is a
;; simple fix to insert NOPs.

(define_insn "*mul<mode>3_r4300"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(mult:SCALARF (match_operand:SCALARF 1 "register_operand" "f")
		      (match_operand:SCALARF 2 "register_operand" "f")))]
  "TARGET_4300_MUL_FIX"
  "mul.<fmt>\t%0,%1,%2\;nop"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<MODE>")
   (set_attr "insn_count" "2")])

(define_insn "mulv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(mult:V2SF (match_operand:V2SF 1 "register_operand" "f")
		   (match_operand:V2SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT"
  "mul.ps\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "SF")])

;; The original R4000 has a cpu bug.  If a double-word or a variable
;; shift executes while an integer multiplication is in progress, the
;; shift may give an incorrect result.  Avoid this by keeping the mflo
;; with the mult on the R4000.
;;
;; From "MIPS R4000PC/SC Errata, Processor Revision 2.2 and 3.0"
;; (also valid for MIPS R4000MC processors):
;;
;; "16. R4000PC, R4000SC: Please refer to errata 28 for an update to
;;	this errata description.
;;	The following code sequence causes the R4000 to incorrectly
;;	execute the Double Shift Right Arithmetic 32 (dsra32)
;;	instruction.  If the dsra32 instruction is executed during an
;;	integer multiply, the dsra32 will only shift by the amount in
;;	specified in the instruction rather than the amount plus 32
;;	bits.
;;	instruction 1:		mult	rs,rt		integer multiply
;;	instruction 2-12:	dsra32	rd,rt,rs	doubleword shift
;;							right arithmetic + 32
;;	Workaround: A dsra32 instruction placed after an integer
;;	multiply should not be one of the 11 instructions after the
;;	multiply instruction."
;;
;; and:
;;
;; "28. R4000PC, R4000SC: The text from errata 16 should be replaced by
;;	the following description.
;;	All extended shifts (shift by n+32) and variable shifts (32 and
;;	64-bit versions) may produce incorrect results under the
;;	following conditions:
;;	1) An integer multiply is currently executing
;;	2) These types of shift instructions are executed immediately
;;	   following an integer divide instruction.
;;	Workaround:
;;	1) Make sure no integer multiply is running wihen these
;;	   instruction are executed.  If this cannot be predicted at
;;	   compile time, then insert a "mfhi" to R0 instruction
;;	   immediately after the integer multiply instruction.  This
;;	   will cause the integer multiply to complete before the shift
;;	   is executed.
;;	2) Separate integer divide and these two classes of shift
;;	   instructions by another instruction or a noop."
;;
;; These processors have PRId values of 0x00004220 and 0x00004300,
;; respectively.

(define_expand "mul<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(mult:GPR (match_operand:GPR 1 "register_operand")
		  (match_operand:GPR 2 "register_operand")))]
  "ISA_HAS_<D>MULT || ISA_HAS_R6<D>MUL"
{
  rtx lo;

  if (TARGET_LOONGSON_2EF || TARGET_LOONGSON_EXT || ISA_HAS_R6<D>MUL)
    emit_insn (gen_mul<mode>3_mul3_nohilo (operands[0], operands[1],
					   operands[2]));
  else if (ISA_HAS_<D>MUL3)
    emit_insn (gen_mul<mode>3_mul3 (operands[0], operands[1], operands[2]));
  else if (TARGET_MIPS16)
    {
      lo = gen_rtx_REG (<MODE>mode, LO_REGNUM);
      emit_insn (gen_mul<mode>3_internal (lo, operands[1], operands[2]));
      emit_move_insn (operands[0], lo);
    }
  else if (TARGET_FIX_R4000)
    emit_insn (gen_mul<mode>3_r4000 (operands[0], operands[1], operands[2]));
  else
    emit_insn
      (gen_mul<mode>3_internal (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "mul<mode>3_mul3_nohilo"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (mult:GPR (match_operand:GPR 1 "register_operand" "d")
                  (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF || TARGET_LOONGSON_EXT || ISA_HAS_R6<D>MUL"
{
  if (TARGET_LOONGSON_2EF)
    return "<d>multu.g\t%0,%1,%2";
  else if (TARGET_LOONGSON_EXT)
    return "gs<d>multu\t%0,%1,%2";
  else
    return "<d>mul\t%0,%1,%2";
}
  [(set_attr "type" "imul3nc")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3_mul3"
  [(set (match_operand:GPR 0 "register_operand" "=d,l")
	(mult:GPR (match_operand:GPR 1 "register_operand" "d,d")
		  (match_operand:GPR 2 "register_operand" "d,d")))
   (clobber (match_scratch:GPR 3 "=l,X"))]
  "ISA_HAS_<D>MUL3"
{
  if (which_alternative == 1)
    return "<d>mult\t%1,%2";
  if (<MODE>mode == SImode && (TARGET_MIPS3900 || TARGET_MIPS5900))
    return "mult\t%0,%1,%2";
  return "<d>mul\t%0,%1,%2";
}
  [(set_attr "type" "imul3,imul")
   (set_attr "mode" "<MODE>")])

;; If a register gets allocated to LO, and we spill to memory, the reload
;; will include a move from LO to a GPR.  Merge it into the multiplication
;; if it can set the GPR directly.
;;
;; Operand 0: LO
;; Operand 1: GPR (1st multiplication operand)
;; Operand 2: GPR (2nd multiplication operand)
;; Operand 3: GPR (destination)
(define_peephole2
  [(parallel
       [(set (match_operand:SI 0 "lo_operand")
	     (mult:SI (match_operand:SI 1 "d_operand")
		      (match_operand:SI 2 "d_operand")))
        (clobber (scratch:SI))])
   (set (match_operand:SI 3 "d_operand")
	(match_dup 0))]
  "ISA_HAS_MUL3 && peep2_reg_dead_p (2, operands[0])"
  [(parallel
       [(set (match_dup 3)
	     (mult:SI (match_dup 1)
		      (match_dup 2)))
        (clobber (match_dup 0))])])

(define_insn "mul<mode>3_internal"
  [(set (match_operand:GPR 0 "muldiv_target_operand" "=l")
	(mult:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))]
  "ISA_HAS_<D>MULT && !TARGET_FIX_R4000"
  "<d>mult\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3_r4000"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(mult:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))
   (clobber (match_scratch:GPR 3 "=l"))]
  "ISA_HAS_<D>MULT && TARGET_FIX_R4000"
  "<d>mult\t%1,%2\;mflo\t%0"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")
   (set_attr "insn_count" "2")])

;; On the VR4120 and VR4130, it is better to use "mtlo $0; macc" instead
;; of "mult; mflo".  They have the same latency, but the first form gives
;; us an extra cycle to compute the operands.

;; Operand 0: LO
;; Operand 1: GPR (1st multiplication operand)
;; Operand 2: GPR (2nd multiplication operand)
;; Operand 3: GPR (destination)
(define_peephole2
  [(set (match_operand:SI 0 "lo_operand")
	(mult:SI (match_operand:SI 1 "d_operand")
		 (match_operand:SI 2 "d_operand")))
   (set (match_operand:SI 3 "d_operand")
	(match_dup 0))]
  "ISA_HAS_MACC && !ISA_HAS_MUL3"
  [(set (match_dup 0)
	(const_int 0))
   (parallel
       [(set (match_dup 0)
	     (plus:SI (mult:SI (match_dup 1)
			       (match_dup 2))
		      (match_dup 0)))
	(set (match_dup 3)
	     (plus:SI (mult:SI (match_dup 1)
			       (match_dup 2))
		      (match_dup 0)))])])

;; Multiply-accumulate patterns

;; This pattern is first matched by combine, which tries to use the
;; pattern wherever it can.  We don't know until later whether it
;; is actually profitable to use MADD over a "MUL; ADDIU" sequence,
;; so we need to keep both options open.
;;
;; The second alternative has a "?" marker because it is generally
;; one instruction more costly than the first alternative.  This "?"
;; marker is enough to convey the relative costs to the register
;; allocator.
;;
;; However, reload counts reloads of operands 4 and 5 in the same way as
;; reloads of the other operands, even though operands 4 and 5 need no
;; copy instructions.  Reload therefore thinks that the second alternative
;; is two reloads more costly than the first.  We add "*?*?" to the first
;; alternative as a counterweight.
;;
;; LRA simulates reload but the cost of reloading scratches is lower
;; than of the classic reload. For the time being, removing the counterweight
;; for LRA is more profitable.
(define_insn "*mul_acc_si"
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,l,d?")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d,d")
			  (match_operand:SI 2 "register_operand" "d,d,d"))
		 (match_operand:SI 3 "register_operand" "l,l,d")))
   (clobber (match_scratch:SI 4 "=X,X,l"))
   (clobber (match_scratch:SI 5 "=X,X,&d"))]
  "GENERATE_MADD_MSUB && !TARGET_MIPS16"
  "@
    madd\t%1,%2
    madd\t%1,%2
    #"
  [(set_attr "type"	"imadd")
   (set_attr "accum_in"	"3")
   (set_attr "mode"	"SI")
   (set_attr "insn_count" "1,1,2")
   (set (attr "enabled")
        (cond [(eq_attr "alternative" "1,2")
                  (const_string "yes")]
              (const_string "no")))])

;; The same idea applies here.  The middle alternative needs one less
;; clobber than the final alternative, so we add "*?" as a counterweight.
(define_insn "*mul_acc_si_r3900"
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,l,d*?,d?")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d,d,d")
			  (match_operand:SI 2 "register_operand" "d,d,d,d"))
		 (match_operand:SI 3 "register_operand" "l,l,l,d")))
   (clobber (match_scratch:SI 4 "=X,X,3,l"))
   (clobber (match_scratch:SI 5 "=X,X,X,&d"))]
  "TARGET_MIPS3900 && !TARGET_MIPS16"
  "@
    madd\t%1,%2
    madd\t%1,%2
    madd\t%0,%1,%2
    #"
  [(set_attr "type"	"imadd")
   (set_attr "accum_in"	"3")
   (set_attr "mode"	"SI")
   (set_attr "insn_count" "1,1,1,2")
   (set (attr "enabled")
        (cond [(eq_attr "alternative" "1,2,3")
                  (const_string "yes")]
              (const_string "no")))])

;; Split *mul_acc_si if both the source and destination accumulator
;; values are GPRs.
(define_split
  [(set (match_operand:SI 0 "d_operand")
	(plus:SI (mult:SI (match_operand:SI 1 "d_operand")
			  (match_operand:SI 2 "d_operand"))
		 (match_operand:SI 3 "d_operand")))
   (clobber (match_operand:SI 4 "lo_operand"))
   (clobber (match_operand:SI 5 "d_operand"))]
  "reload_completed"
  [(parallel [(set (match_dup 5)
		   (mult:SI (match_dup 1) (match_dup 2)))
	      (clobber (match_dup 4))])
   (set (match_dup 0) (plus:SI (match_dup 5) (match_dup 3)))]
  "")

(define_insn "*macc"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d")
			  (match_operand:SI 2 "register_operand" "d,d"))
		 (match_operand:SI 3 "register_operand" "l,l")))
   (clobber (match_scratch:SI 4 "=X,3"))]
  "ISA_HAS_MACC"
{
  if (which_alternative == 1)
    return "macc\t%0,%1,%2";
  else if (TARGET_MIPS5500)
    return "madd\t%1,%2";
  else
    /* The VR4130 assumes that there is a two-cycle latency between a macc
       that "writes" to $0 and an instruction that reads from it.  We avoid
       this by assigning to $1 instead.  */
    return "%[macc\t%@,%1,%2%]";
}
  [(set_attr "type" "imadd")
   (set_attr "accum_in"	"3")
   (set_attr "mode" "SI")])

(define_insn "*msac"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "l,l")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d")
                           (match_operand:SI 3 "register_operand" "d,d"))))
   (clobber (match_scratch:SI 4 "=X,1"))]
  "ISA_HAS_MSAC"
{
  if (which_alternative == 1)
    return "msac\t%0,%2,%3";
  else if (TARGET_MIPS5500)
    return "msub\t%2,%3";
  else
    return "msac\t$0,%2,%3";
}
  [(set_attr "type"     "imadd")
   (set_attr "accum_in"	"1")
   (set_attr "mode"     "SI")])

;; An msac-like instruction implemented using negation and a macc.
(define_insn_and_split "*msac_using_macc"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "l,l")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d")
                           (match_operand:SI 3 "register_operand" "d,d"))))
   (clobber (match_scratch:SI 4 "=X,1"))
   (clobber (match_scratch:SI 5 "=d,d"))]
  "ISA_HAS_MACC && !ISA_HAS_MSAC"
  "#"
  "&& reload_completed"
  [(set (match_dup 5)
	(neg:SI (match_dup 3)))
   (parallel
       [(set (match_dup 0)
	     (plus:SI (mult:SI (match_dup 2)
			       (match_dup 5))
		      (match_dup 1)))
	(clobber (match_dup 4))])]
  ""
  [(set_attr "type"     "imadd")
   (set_attr "accum_in"	"1")
   (set_attr "insn_count" "2")])

;; Patterns generated by the define_peephole2 below.

(define_insn "*macc2"
  [(set (match_operand:SI 0 "muldiv_target_operand" "=l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d")
			  (match_operand:SI 2 "register_operand" "d"))
		 (match_dup 0)))
   (set (match_operand:SI 3 "register_operand" "=d")
	(plus:SI (mult:SI (match_dup 1)
			  (match_dup 2))
		 (match_dup 0)))]
  "ISA_HAS_MACC && reload_completed"
  "macc\t%3,%1,%2"
  [(set_attr "type"	"imadd")
   (set_attr "accum_in"	"0")
   (set_attr "mode"	"SI")])

(define_insn "*msac2"
  [(set (match_operand:SI 0 "muldiv_target_operand" "=l")
	(minus:SI (match_dup 0)
		  (mult:SI (match_operand:SI 1 "register_operand" "d")
			   (match_operand:SI 2 "register_operand" "d"))))
   (set (match_operand:SI 3 "register_operand" "=d")
	(minus:SI (match_dup 0)
		  (mult:SI (match_dup 1)
			   (match_dup 2))))]
  "ISA_HAS_MSAC && reload_completed"
  "msac\t%3,%1,%2"
  [(set_attr "type"	"imadd")
   (set_attr "accum_in"	"0")
   (set_attr "mode"	"SI")])

;; Convert macc $0,<r1>,<r2> & mflo <r3> into macc <r3>,<r1>,<r2>
;; Similarly msac.
;;
;; Operand 0: LO
;; Operand 1: macc/msac
;; Operand 2: GPR (destination)
(define_peephole2
  [(parallel
       [(set (match_operand:SI 0 "lo_operand")
	     (match_operand:SI 1 "macc_msac_operand"))
	(clobber (scratch:SI))])
   (set (match_operand:SI 2 "d_operand")
	(match_dup 0))]
  ""
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (set (match_dup 2)
		   (match_dup 1))])])

;; When we have a three-address multiplication instruction, it should
;; be faster to do a separate multiply and add, rather than moving
;; something into LO in order to use a macc instruction.
;;
;; This peephole needs a scratch register to cater for the case when one
;; of the multiplication operands is the same as the destination.
;;
;; Operand 0: GPR (scratch)
;; Operand 1: LO
;; Operand 2: GPR (addend)
;; Operand 3: GPR (destination)
;; Operand 4: macc/msac
;; Operand 5: new multiplication
;; Operand 6: new addition/subtraction
(define_peephole2
  [(match_scratch:SI 0 "d")
   (set (match_operand:SI 1 "lo_operand")
	(match_operand:SI 2 "d_operand"))
   (match_dup 0)
   (parallel
       [(set (match_operand:SI 3 "d_operand")
	     (match_operand:SI 4 "macc_msac_operand"))
	(clobber (match_dup 1))])]
  "ISA_HAS_MUL3 && peep2_reg_dead_p (2, operands[1])"
  [(parallel [(set (match_dup 0)
		   (match_dup 5))
	      (clobber (match_dup 1))])
   (set (match_dup 3)
	(match_dup 6))]
{
  operands[5] = XEXP (operands[4], GET_CODE (operands[4]) == PLUS ? 0 : 1);
  operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[4]), SImode,
				operands[2], operands[0]);
})

;; Same as above, except LO is the initial target of the macc.
;;
;; Operand 0: GPR (scratch)
;; Operand 1: LO
;; Operand 2: GPR (addend)
;; Operand 3: macc/msac
;; Operand 4: GPR (destination)
;; Operand 5: new multiplication
;; Operand 6: new addition/subtraction
(define_peephole2
  [(match_scratch:SI 0 "d")
   (set (match_operand:SI 1 "lo_operand")
	(match_operand:SI 2 "d_operand"))
   (match_dup 0)
   (parallel
       [(set (match_dup 1)
	     (match_operand:SI 3 "macc_msac_operand"))
	(clobber (scratch:SI))])
   (match_dup 0)
   (set (match_operand:SI 4 "d_operand")
	(match_dup 1))]
  "ISA_HAS_MUL3 && peep2_reg_dead_p (3, operands[1])"
  [(parallel [(set (match_dup 0)
		   (match_dup 5))
	      (clobber (match_dup 1))])
   (set (match_dup 4)
	(match_dup 6))]
{
  operands[5] = XEXP (operands[3], GET_CODE (operands[3]) == PLUS ? 0 : 1);
  operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[3]), SImode,
				operands[2], operands[0]);
})

;; See the comment above *mul_add_si for details.
(define_insn "*mul_sub_si"
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,l,d?")
        (minus:SI (match_operand:SI 1 "register_operand" "l,l,d")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d,d")
                           (match_operand:SI 3 "register_operand" "d,d,d"))))
   (clobber (match_scratch:SI 4 "=X,X,l"))
   (clobber (match_scratch:SI 5 "=X,X,&d"))]
  "GENERATE_MADD_MSUB"
  "@
   msub\t%2,%3
   msub\t%2,%3
   #"
  [(set_attr "type"     "imadd")
   (set_attr "accum_in"	"1")
   (set_attr "mode"     "SI")
   (set_attr "insn_count" "1,1,2")
   (set (attr "enabled")
        (cond [(eq_attr "alternative" "1,2")
                  (const_string "yes")]
              (const_string "no")))])

;; Split *mul_sub_si if both the source and destination accumulator
;; values are GPRs.
(define_split
  [(set (match_operand:SI 0 "d_operand")
        (minus:SI (match_operand:SI 1 "d_operand")
                  (mult:SI (match_operand:SI 2 "d_operand")
                           (match_operand:SI 3 "d_operand"))))
   (clobber (match_operand:SI 4 "lo_operand"))
   (clobber (match_operand:SI 5 "d_operand"))]
  "reload_completed"
  [(parallel [(set (match_dup 5)
                   (mult:SI (match_dup 2) (match_dup 3)))
              (clobber (match_dup 4))])
   (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 5)))]
  "")

(define_insn "*muls"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (neg:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d")
                         (match_operand:SI 2 "register_operand" "d,d"))))
   (clobber (match_scratch:SI 3 "=X,l"))]
  "ISA_HAS_MULS"
  "@
   muls\t$0,%1,%2
   muls\t%0,%1,%2"
  [(set_attr "type"     "imul,imul3")
   (set_attr "mode"     "SI")])

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))]
  "mips_mulsidi3_gen_fn (<CODE>) != NULL"
{
  mulsidi3_gen_fn fn = mips_mulsidi3_gen_fn (<CODE>);
  emit_insn (fn (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "<u>mulsidi3_32bit_r6"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))]
  "!TARGET_64BIT && ISA_HAS_R6MUL"
{
  rtx dest = gen_reg_rtx (DImode);
  rtx low = mips_subword (dest, 0);
  rtx high = mips_subword (dest, 1);

  emit_insn (gen_mulsi3_mul3_nohilo (low, operands[1], operands[2]));
  emit_insn (gen_<su>mulsi3_highpart_r6 (high, operands[1], operands[2]));

  emit_move_insn (mips_subword (operands[0], 0), low);
  emit_move_insn (mips_subword (operands[0], 1), high);
  DONE;
})

(define_expand "<u>mulsidi3_32bit_mips16"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))]
  "!TARGET_64BIT && TARGET_MIPS16"
{
  rtx hilo;

  hilo = gen_rtx_REG (DImode, MD_REG_FIRST);
  emit_insn (gen_<u>mulsidi3_32bit (hilo, operands[1], operands[2]));
  emit_move_insn (operands[0], hilo);
  DONE;
})

;; As well as being named patterns, these instructions are used by the
;; __builtin_mips_mult<u>() functions.  We must always make those functions
;; available if !TARGET_64BIT && ISA_HAS_DSP.
(define_insn "<u>mulsidi3_32bit"
  [(set (match_operand:DI 0 "muldiv_target_operand" "=ka")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_64BIT && (!TARGET_FIX_R4000 || ISA_HAS_DSP) && ISA_HAS_MULT"
{
  if (ISA_HAS_DSP_MULT)
    return "mult<u>\t%q0,%1,%2";
  else
    return "mult<u>\t%1,%2";
}
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "<u>mulsidi3_32bit_r4000"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (match_scratch:DI 3 "=x"))]
  "!TARGET_64BIT && TARGET_FIX_R4000 && !ISA_HAS_DSP && ISA_HAS_MULT"
  "mult<u>\t%1,%2\;mflo\t%L0\;mfhi\t%M0"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set_attr "insn_count" "3")])

(define_insn_and_split "<u>mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (match_scratch:TI 3 "=x"))
   (clobber (match_scratch:DI 4 "=d"))]
  "TARGET_64BIT && !TARGET_FIX_R4000 && !ISA_HAS_DMUL3
   && !TARGET_MIPS16 && ISA_HAS_MULT"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_<u>mulsidi3_64bit_split (operands[0], operands[1],
					  operands[2], operands[4]));
  DONE;
}
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set (attr "insn_count")
	(if_then_else (match_test "ISA_HAS_EXT_INS")
		      (const_int 4)
		      (const_int 7)))])

(define_expand "<u>mulsidi3_64bit_mips16"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  emit_insn (gen_<u>mulsidi3_64bit_split (operands[0], operands[1],
					  operands[2], gen_reg_rtx (DImode)));
  DONE;
})

(define_expand "<u>mulsidi3_64bit_split"
  [(set (match_operand:DI 0 "register_operand")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		 (any_extend:DI (match_operand:SI 2 "register_operand"))))
   (clobber (match_operand:DI 3 "register_operand"))]
  ""
{
  rtx hilo;

  hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
  emit_insn (gen_<u>mulsidi3_64bit_hilo (hilo, operands[1], operands[2]));

  emit_move_insn (operands[0], gen_rtx_REG (DImode, LO_REGNUM));
  emit_insn (gen_mfhidi_ti (operands[3], hilo));

  if (ISA_HAS_EXT_INS)
    emit_insn (gen_insvdi (operands[0], GEN_INT (32), GEN_INT (32),
			   operands[3]));
  else
    {
      /* Zero-extend the low part.  */
      mips_emit_binary (ASHIFT, operands[0], operands[0], GEN_INT (32));
      mips_emit_binary (LSHIFTRT, operands[0], operands[0], GEN_INT (32));

      /* Shift the high part into place.  */
      mips_emit_binary (ASHIFT, operands[3], operands[3], GEN_INT (32));

      /* OR the two halves together.  */
      mips_emit_binary (IOR, operands[0], operands[0], operands[3]);
    }
  DONE;
})

(define_insn "<u>mulsidi3_64bit_hilo"
  [(set (match_operand:TI 0 "muldiv_target_operand" "=x")
	(unspec:TI
	  [(mult:DI
	     (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	     (any_extend:DI (match_operand:SI 2 "register_operand" "d")))]
	  UNSPEC_SET_HILO))]
  "TARGET_64BIT && !TARGET_FIX_R4000"
  "mult<u>\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;; See comment before the ISA_HAS_DMUL3 case in mips_mulsidi3_gen_fn.
(define_insn "mulsidi3_64bit_dmul"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (match_scratch:DI 3 "=l"))]
  "ISA_HAS_DMUL3"
  "dmul\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "DI")])

(define_insn "mulsidi3_64bit_r6dmul"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "ISA_HAS_R6DMUL"
  "dmul\t%0,%1,%2"
  [(set_attr "type" "imul3nc")
   (set_attr "mode" "DI")])

;; Widening multiply with negation.
(define_insn "*muls<u>_di"
  [(set (match_operand:DI 0 "muldiv_target_operand" "=x")
        (neg:DI
	 (mult:DI
	  (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	  (any_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MULS"
  "muls<u>\t$0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;; As well as being named patterns, these instructions are used by the
;; __builtin_mips_msub<u>() functions.  We must always make those functions
;; available if !TARGET_64BIT && ISA_HAS_DSP.
;;
;; This leads to a slight inconsistency.  We honor any tuning overrides
;; in GENERATE_MADD_MSUB for -mno-dsp, but always ignore them for -mdsp,
;; even if !ISA_HAS_DSP_MULT.
(define_insn "<u>msubsidi4"
  [(set (match_operand:DI 0 "muldiv_target_operand" "=ka")
        (minus:DI
	   (match_operand:DI 3 "muldiv_target_operand" "0")
	   (mult:DI
	      (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	      (any_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && (ISA_HAS_MSAC || GENERATE_MADD_MSUB || ISA_HAS_DSP)"
{
  if (ISA_HAS_DSP_MULT)
    return "msub<u>\t%q0,%1,%2";
  else if (TARGET_MIPS5500 || GENERATE_MADD_MSUB)
    return "msub<u>\t%1,%2";
  else
    return "msac<u>\t$0,%1,%2";
}
  [(set_attr "type" "imadd")
   (set_attr "accum_in"	"3")
   (set_attr "mode" "SI")])

;; _highpart patterns

(define_expand "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		   (any_extend:DI (match_operand:SI 2 "register_operand")))
	  (const_int 32))))]
  ""
{
  if (ISA_HAS_MULHI)
    emit_insn (gen_<su>mulsi3_highpart_mulhi_internal (operands[0],
						       operands[1],
						       operands[2]));
  else if (TARGET_MIPS16)
    emit_insn (gen_<su>mulsi3_highpart_split (operands[0], operands[1],
					      operands[2]));
  else if (ISA_HAS_R6MUL)
    emit_insn (gen_<su>mulsi3_highpart_r6 (operands[0], operands[1],
					   operands[2]));
  else
    emit_insn (gen_<su>mulsi3_highpart_internal (operands[0], operands[1],
					         operands[2]));
  DONE;
})

(define_insn "<su>mulsi3_highpart_r6"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		   (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))]
  "ISA_HAS_R6MUL"
  "muh<u>\t%0,%1,%2"
  [(set_attr "type" "imul3nc")
   (set_attr "mode" "SI")])

(define_insn_and_split "<su>mulsi3_highpart_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		   (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "ISA_HAS_MULT && !ISA_HAS_MULHI && !TARGET_MIPS16"
  { return TARGET_FIX_R4000 ? "mult<u>\t%1,%2\n\tmfhi\t%0" : "#"; }
  "&& reload_completed && !TARGET_FIX_R4000"
  [(const_int 0)]
{
  emit_insn (gen_<su>mulsi3_highpart_split (operands[0], operands[1],
					    operands[2]));
  DONE;
}
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set_attr "insn_count" "2")])

(define_expand "<su>mulsi3_highpart_split"
  [(set (match_operand:SI 0 "register_operand")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand"))
		   (any_extend:DI (match_operand:SI 2 "register_operand")))
	  (const_int 32))))]
  ""
{
  rtx hilo;

  if (TARGET_64BIT)
    {
      hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
      emit_insn (gen_<u>mulsidi3_64bit_hilo (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhisi_ti (operands[0], hilo));
    }
  else
    {
      hilo = gen_rtx_REG (DImode, MD_REG_FIRST);
      emit_insn (gen_<u>mulsidi3_32bit (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhisi_di (operands[0], hilo));
    }
  DONE;
})

(define_insn "<su>mulsi3_highpart_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	   (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "ISA_HAS_MULHI"
  "mulhi<u>\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

(define_insn "*<su>mulsi3_highpart_neg_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI
	 (lshiftrt:DI
	  (neg:DI
	   (mult:DI
	    (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	    (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "ISA_HAS_MULHI"
  "mulshi<u>\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

;; Disable unsigned multiplication for -mfix-vr4120.  This is for VR4120
;; errata MD(0), which says that dmultu does not always produce the
;; correct result.
(define_expand "<su>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		   (any_extend:TI (match_operand:DI 2 "register_operand")))
	  (const_int 64))))]
  "ISA_HAS_R6DMUL
   || (ISA_HAS_DMULT
       && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120))"
{
  if (TARGET_MIPS16)
    emit_insn (gen_<su>muldi3_highpart_split (operands[0], operands[1],
					      operands[2]));
  else if (ISA_HAS_R6DMUL)
    emit_insn (gen_<su>muldi3_highpart_r6 (operands[0], operands[1],
					   operands[2]));
  else
    emit_insn (gen_<su>muldi3_highpart_internal (operands[0], operands[1],
						 operands[2]));
  DONE;
})

(define_insn "<su>muldi3_highpart_r6"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		   (any_extend:TI (match_operand:DI 2 "register_operand" "d")))
	  (const_int 64))))]
  "ISA_HAS_R6DMUL"
  "dmuh<u>\t%0,%1,%2"
  [(set_attr "type" "imul3nc")
   (set_attr "mode" "DI")])

(define_insn_and_split "<su>muldi3_highpart_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		   (any_extend:TI (match_operand:DI 2 "register_operand" "d")))
	  (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))]
  "ISA_HAS_DMULT
   && !TARGET_MIPS16
   && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
  { return TARGET_FIX_R4000 ? "dmult<u>\t%1,%2\n\tmfhi\t%0" : "#"; }
  "&& reload_completed && !TARGET_FIX_R4000"
  [(const_int 0)]
{
  emit_insn (gen_<su>muldi3_highpart_split (operands[0], operands[1],
					    operands[2]));
  DONE;
}
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "2")])

(define_expand "<su>muldi3_highpart_split"
  [(set (match_operand:DI 0 "register_operand")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		   (any_extend:TI (match_operand:DI 2 "register_operand")))
	  (const_int 64))))]
  ""
{
  rtx hilo;

  hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
  emit_insn (gen_<u>mulditi3_internal (hilo, operands[1], operands[2]));
  emit_insn (gen_mfhidi_ti (operands[0], hilo));
  DONE;
})

(define_expand "<u>mulditi3"
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		 (any_extend:TI (match_operand:DI 2 "register_operand"))))]
  "ISA_HAS_R6DMUL
   || (ISA_HAS_DMULT
       && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120))"
{
  rtx hilo, hi, lo;

  if (TARGET_MIPS16)
    {
      hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
      emit_insn (gen_<u>mulditi3_internal (hilo, operands[1], operands[2]));
      emit_move_insn (operands[0], hilo);
    }
  else if (TARGET_FIX_R4000)
    emit_insn (gen_<u>mulditi3_r4000 (operands[0], operands[1], operands[2]));
  else if (ISA_HAS_DMULT)
    emit_insn (gen_<u>mulditi3_internal (operands[0], operands[1],
					 operands[2]));
  else
    {
      hi = mips_subword (operands[0], 1);
      lo = mips_subword (operands[0], 0);
      emit_insn (gen_muldi3_mul3_nohilo (lo, operands[1], operands[2]));
      emit_insn (gen_<su>muldi3_highpart_r6 (hi, operands[1], operands[2]));
    }
  DONE;
})

(define_insn "<u>mulditi3_internal"
  [(set (match_operand:TI 0 "muldiv_target_operand" "=x")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		 (any_extend:TI (match_operand:DI 2 "register_operand" "d"))))]
  "ISA_HAS_DMULT
   && !TARGET_FIX_R4000
   && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
  "dmult<u>\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_insn "<u>mulditi3_r4000"
  [(set (match_operand:TI 0 "register_operand" "=d")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		 (any_extend:TI (match_operand:DI 2 "register_operand" "d"))))
   (clobber (match_scratch:TI 3 "=x"))]
  "ISA_HAS_DMULT
   && TARGET_FIX_R4000
   && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
  "dmult<u>\t%1,%2\;mflo\t%L0\;mfhi\t%M0"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")
   (set_attr "insn_count" "3")])

;; The R4650 supports a 32-bit multiply/ 64-bit accumulate
;; instruction.  The HI/LO registers are used as a 64-bit accumulator.

(define_insn "madsi"
  [(set (match_operand:SI 0 "register_operand" "+l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d")
			  (match_operand:SI 2 "register_operand" "d"))
		 (match_dup 0)))]
  "TARGET_MAD"
  "mad\t%1,%2"
  [(set_attr "type"	"imadd")
   (set_attr "accum_in"	"0")
   (set_attr "mode"	"SI")])

;; See the comment above <u>msubsidi4 for the relationship between
;; ISA_HAS_DSP and ISA_HAS_DSP_MULT.
(define_insn "<u>maddsidi4"
  [(set (match_operand:DI 0 "muldiv_target_operand" "=ka")
	(plus:DI
	 (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		  (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	 (match_operand:DI 3 "muldiv_target_operand" "0")))]
  "(TARGET_MAD || ISA_HAS_MACC || GENERATE_MADD_MSUB || ISA_HAS_DSP)
   && !TARGET_64BIT"
{
  if (TARGET_MAD)
    return "mad<u>\t%1,%2";
  else if (ISA_HAS_DSP_MULT)
    return "madd<u>\t%q0,%1,%2";
  else if (GENERATE_MADD_MSUB || TARGET_MIPS5500)
    return "madd<u>\t%1,%2";
  else
    /* See comment in *macc.  */
    return "%[macc<u>\t%@,%1,%2%]";
}
  [(set_attr "type" "imadd")
   (set_attr "accum_in"	"3")
   (set_attr "mode" "SI")])

;; Floating point multiply accumulate instructions.

(define_expand "fma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand")
	(fma:ANYF (match_operand:ANYF 1 "register_operand")
		  (match_operand:ANYF 2 "register_operand")
		  (match_operand:ANYF 3 "register_operand")))]
  "ISA_HAS_FUSED_MADDF || ISA_HAS_FUSED_MADD3 || ISA_HAS_FUSED_MADD4")

(define_insn "*fma<mode>4_madd3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_FUSED_MADD3"
  "madd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*fma<mode>4_madd4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_FUSED_MADD4"
  "madd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*fma<mode>4_maddf"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_FUSED_MADDF"
  "maddf.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; The fms, fnma, and fnms instructions can be used even when HONOR_NANS
;; is true because while IEEE 754-2008 requires the negate operation to
;; negate the sign of a NAN and the MIPS neg instruction does not do this,
;; the fma part of the instruction has no requirement on how the sign of
;; a NAN is handled and so the final sign bit of the entire operation is
;; undefined.

(define_expand "fms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand")
	(fma:ANYF (match_operand:ANYF 1 "register_operand")
		  (match_operand:ANYF 2 "register_operand")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand"))))]
  "(ISA_HAS_FUSED_MADD3 || ISA_HAS_FUSED_MADD4)")

(define_insn "*fms<mode>4_msub3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "0"))))]
  "ISA_HAS_FUSED_MADD3"
  "msub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*fms<mode>4_msub4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_FUSED_MADD4"
  "msub.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; fnma is defined in GCC as (fma (neg op1) op2 op3)
;; (-op1 * op2) + op3 ==> -(op1 * op2) + op3 ==> -((op1 * op2) - op3)
;; The mips nmsub instructions implement -((op1 * op2) - op3)
;; This transformation means we may return the wrong signed zero
;; so we check HONOR_SIGNED_ZEROS.

(define_expand "fnma<mode>4"
  [(set (match_operand:ANYF 0 "register_operand")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand"))
		  (match_operand:ANYF 2 "register_operand")
		  (match_operand:ANYF 3 "register_operand")))]
  "(ISA_HAS_FUSED_MADD3 || ISA_HAS_FUSED_MADD4)
   && !HONOR_SIGNED_ZEROS (<MODE>mode)")

(define_insn "*fnma<mode>4_nmsub3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_FUSED_MADD3 && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*fnma<mode>4_nmsub4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		  (match_operand:ANYF 2 "register_operand" "f")
		  (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_FUSED_MADD4 && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; fnms is defined as: (fma (neg op1) op2 (neg op3))
;; ((-op1) * op2) - op3 ==> -(op1 * op2) - op3 ==> -((op1 * op2) + op3)
;; The mips nmadd instructions implement -((op1 * op2) + op3)
;; This transformation means we may return the wrong signed zero
;; so we check HONOR_SIGNED_ZEROS.

(define_expand "fnms<mode>4"
  [(set (match_operand:ANYF 0 "register_operand")
	(fma:ANYF
	  (neg:ANYF (match_operand:ANYF 1 "register_operand"))
	  (match_operand:ANYF 2 "register_operand")
	  (neg:ANYF (match_operand:ANYF 3 "register_operand"))))]
  "(ISA_HAS_FUSED_MADD3 || ISA_HAS_FUSED_MADD4)
   && !HONOR_SIGNED_ZEROS (<MODE>mode)")

(define_insn "*fnms<mode>4_nmadd3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF
	  (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
	  (match_operand:ANYF 2 "register_operand" "f")
	  (neg:ANYF (match_operand:ANYF 3 "register_operand" "0"))))]
  "ISA_HAS_FUSED_MADD3 && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*fnms<mode>4_nmadd4"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(fma:ANYF
	  (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
	  (match_operand:ANYF 2 "register_operand" "f")
	  (neg:ANYF (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_FUSED_MADD4 && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; Non-fused Floating point multiply accumulate instructions.

;; These instructions are not fused and round in between the multiply
;; and the add (or subtract) so they are equivalent to the separate
;; multiply and add/sub instructions.

(define_insn "*madd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_UNFUSED_MADD4"
  "madd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*msub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			       (match_operand:ANYF 2 "register_operand" "f"))
		    (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_UNFUSED_MADD4"
  "msub.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; Like with the fused fms, fnma, and fnms instructions, these unfused
;; instructions can be used even if HONOR_NANS is set because while
;; IEEE 754-2008 requires the negate operation to negate the sign of a
;; NAN and the MIPS neg instruction does not do this, the multiply and
;; add (or subtract) part of the instruction has no requirement on how
;; the sign of a NAN is handled and so the final sign bit of the entire
;; operation is undefined.

(define_insn "*nmadd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (plus:ANYF
		   (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_UNFUSED_MADD4"
  "nmadd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (minus:ANYF
		   (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_UNFUSED_MADD4"
  "nmsub.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; Fast-math Non-fused Floating point multiply accumulate instructions.

;; These instructions are not fused but the expressions they match are
;; not exactly what the instruction implements in the sense that they
;; may not generate the properly signed zeros.

;; This instruction recognizes  ((-op1) * op2) - op3 and generates an
;; nmadd which is really -((op1 * op2) + op3).  They are equivalent
;; except for the sign bit when the result is zero or NaN.

(define_insn "*nmadd4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	  (mult:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		     (match_operand:ANYF 2 "register_operand" "f"))
	  (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_UNFUSED_MADD4
   && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; This instruction recognizes (op1 - (op2 * op3) and generates an
;; nmsub which is really -((op2 * op3) - op1).  They are equivalent
;; except for the sign bit when the result is zero or NaN.

(define_insn "*nmsub4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	  (match_operand:ANYF 1 "register_operand" "f")
	  (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
		     (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_UNFUSED_MADD4
   && !HONOR_SIGNED_ZEROS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

(define_expand "div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand")
	(div:ANYF (match_operand:ANYF 1 "reg_or_1_operand")
		  (match_operand:ANYF 2 "register_operand")))]
  "<divide_condition>"
{
  if (const_1_operand (operands[1], <MODE>mode))
    if (!(ISA_HAS_FP_RECIP_RSQRT (<MODE>mode)
	  && flag_unsafe_math_optimizations))
      operands[1] = force_reg (<MODE>mode, operands[1]);
})

;; These patterns work around the early SB-1 rev2 core "F1" erratum:
;;
;; If an mfc1 or dmfc1 happens to access the floating point register
;; file at the same time a long latency operation (div, sqrt, recip,
;; sqrt) iterates an intermediate result back through the floating
;; point register file bypass, then instead returning the correct
;; register value the mfc1 or dmfc1 operation returns the intermediate
;; result of the long latency operation.
;;
;; The workaround is to insert an unconditional 'mov' from/to the
;; long latency op destination register.

(define_insn "*div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  "<divide_condition>"
{
  if (TARGET_FIX_SB1)
    return "div.<fmt>\t%0,%1,%2\;mov.<fmt>\t%0,%0";
  else
    return "div.<fmt>\t%0,%1,%2";
}
  [(set_attr "type" "fdiv")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "insn_count")
        (if_then_else (match_test "TARGET_FIX_SB1")
                      (const_int 2)
                      (const_int 1)))])

(define_insn "*recip<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  "ISA_HAS_FP_RECIP_RSQRT (<MODE>mode) && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "recip.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "recip.<fmt>\t%0,%2";
}
  [(set_attr "type" "frdiv")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "insn_count")
        (if_then_else (match_test "TARGET_FIX_SB1")
                      (const_int 2)
                      (const_int 1)))])

;; VR4120 errata MD(A1): signed division instructions do not work correctly
;; with negative operands.  We use special libgcc functions instead.
(define_expand "divmod<mode>4"
  [(parallel
     [(set (match_operand:GPR 0 "register_operand")
	   (div:GPR (match_operand:GPR 1 "register_operand")
		    (match_operand:GPR 2 "register_operand")))
      (set (match_operand:GPR 3 "register_operand")
	   (mod:GPR (match_dup 1)
		    (match_dup 2)))])]
  "ISA_HAS_<D>DIV && !TARGET_FIX_VR4120"
{
  if (TARGET_MIPS16)
    {
      rtx lo = gen_rtx_REG (<MODE>mode, LO_REGNUM);
      emit_insn (gen_divmod<mode>4_mips16 (operands[0], operands[1],
					   operands[2], operands[3], lo));
      DONE;
    }
})

(define_insn_and_split "*divmod<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=l")
	(div:GPR (match_operand:GPR 1 "register_operand" "d")
		 (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(mod:GPR (match_dup 1)
		 (match_dup 2)))]
  "ISA_HAS_<D>DIV && !TARGET_FIX_VR4120 && !TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_divmod<mode>4_split (operands[3], operands[1], operands[2]));
  DONE;
}
 [(set_attr "type" "idiv")
  (set_attr "mode" "<MODE>")
  (set_attr "insn_count" "2")])

;; Expand generates divmod instructions for individual division and modulus
;; operations.  We then rely on CSE to reuse earlier divmods where possible.
;; This means that, when generating MIPS16 code, it is better not to expose
;; the fixed LO register until after CSE has finished.  However, it's still
;; better to split before register allocation, so that we don't allocate
;; one of the scarce MIPS16 registers to an unused result.
(define_insn_and_split "divmod<mode>4_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(div:GPR (match_operand:GPR 1 "register_operand" "d")
		 (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(mod:GPR (match_dup 1)
		 (match_dup 2)))
   (clobber (match_operand:GPR 4 "lo_operand" "=l"))]
  "ISA_HAS_<D>DIV && !TARGET_FIX_VR4120 && TARGET_MIPS16"
  "#"
  "&& cse_not_expected"
  [(const_int 0)]
{
  emit_insn (gen_divmod<mode>4_split (operands[3], operands[1], operands[2]));
  emit_move_insn (operands[0], operands[4]);
  DONE;
}
 [(set_attr "type" "idiv")
  (set_attr "mode" "<MODE>")
  (set_attr "insn_count" "3")])

(define_expand "udivmod<mode>4"
  [(parallel
     [(set (match_operand:GPR 0 "register_operand")
	   (udiv:GPR (match_operand:GPR 1 "register_operand")
		     (match_operand:GPR 2 "register_operand")))
      (set (match_operand:GPR 3 "register_operand")
	   (umod:GPR (match_dup 1)
		     (match_dup 2)))])]
  "ISA_HAS_<D>DIV && !TARGET_FIX_VR4120"
{
  if (TARGET_MIPS16)
    {
      rtx lo = gen_rtx_REG (<MODE>mode, LO_REGNUM);
      emit_insn (gen_udivmod<mode>4_mips16 (operands[0], operands[1],
					    operands[2], operands[3], lo));
      DONE;
    }
})

(define_insn_and_split "*udivmod<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=l")
	(udiv:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(umod:GPR (match_dup 1)
		  (match_dup 2)))]
  "ISA_HAS_<D>DIV && !TARGET_MIPS16"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_udivmod<mode>4_split (operands[3], operands[1], operands[2]));
  DONE;
}
  [(set_attr "type" "idiv")
   (set_attr "mode" "<MODE>")
   (set_attr "insn_count" "2")])

;; See the comment above "divmod<mode>4_mips16" for the split timing.
(define_insn_and_split "udivmod<mode>4_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(udiv:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(umod:GPR (match_dup 1)
		  (match_dup 2)))
   (clobber (match_operand:GPR 4 "lo_operand" "=l"))]
  "ISA_HAS_<D>DIV && TARGET_MIPS16"
  "#"
  "cse_not_expected"
  [(const_int 0)]
{
  emit_insn (gen_udivmod<mode>4_split (operands[3], operands[1], operands[2]));
  emit_move_insn (operands[0], operands[4]);
  DONE;
}
  [(set_attr "type" "idiv")
   (set_attr "mode" "<MODE>")
   (set_attr "insn_count" "3")])

(define_expand "<u>divmod<mode>4_split"
  [(set (match_operand:GPR 0 "register_operand")
	(any_mod:GPR (match_operand:GPR 1 "register_operand")
		     (match_operand:GPR 2 "register_operand")))]
  ""
{
  rtx hilo;

  if (TARGET_64BIT)
    {
      hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
      emit_insn (gen_<u>divmod<mode>4_hilo_ti (hilo, operands[1],
					       operands[2]));
      emit_insn (gen_mfhi<mode>_ti (operands[0], hilo));
    }
  else
    {
      hilo = gen_rtx_REG (DImode, MD_REG_FIRST);
      emit_insn (gen_<u>divmod<mode>4_hilo_di (hilo, operands[1],
					       operands[2]));
      emit_insn (gen_mfhi<mode>_di (operands[0], hilo));
    }
  DONE;
})

(define_insn "<u>divmod<GPR:mode>4_hilo_<HILO:mode>"
  [(set (match_operand:HILO 0 "muldiv_target_operand" "=x")
	(unspec:HILO
	  [(any_div:GPR (match_operand:GPR 1 "register_operand" "d")
			(match_operand:GPR 2 "register_operand" "d"))]
	  UNSPEC_SET_HILO))]
  "ISA_HAS_<GPR:D>DIV"
  { return mips_output_division ("<GPR:d>div<u>\t%.,%1,%2", operands); }
  [(set_attr "type" "idiv")
   (set_attr "mode" "<GPR:MODE>")])

;; Integer division and modulus.

(define_insn "<u>div<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_div:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF || TARGET_LOONGSON_EXT || ISA_HAS_R6<D>DIV"
  {
    if (TARGET_LOONGSON_2EF)
      return mips_output_division ("<d>div<u>.g\t%0,%1,%2", operands);
    else if (TARGET_LOONGSON_EXT)
      return mips_output_division ("gs<d>div<u>\t%0,%1,%2", operands);
    else
      return mips_output_division ("<d>div<u>\t%0,%1,%2", operands);
  }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])

(define_insn "<u>mod<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_mod:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF || TARGET_LOONGSON_EXT || ISA_HAS_R6<D>DIV"
  {
    if (TARGET_LOONGSON_2EF)
      return mips_output_division ("<d>mod<u>.g\t%0,%1,%2", operands);
    else if (TARGET_LOONGSON_EXT)
      return mips_output_division ("gs<d>mod<u>\t%0,%1,%2", operands);
    else
      return mips_output_division ("<d>mod<u>\t%0,%1,%2", operands);
  }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

;; These patterns work around the early SB-1 rev2 core "F1" erratum (see
;; "*div[sd]f3" comment for details).

(define_insn "sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "<sqrt_condition>"
{
  if (TARGET_FIX_SB1)
    return "sqrt.<fmt>\t%0,%1\;mov.<fmt>\t%0,%0";
  else
    return "sqrt.<fmt>\t%0,%1";
}
  [(set_attr "type" "fsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "insn_count")
        (if_then_else (match_test "TARGET_FIX_SB1")
                      (const_int 2)
                      (const_int 1)))])

(define_insn "*rsqrt<mode>a"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (sqrt:ANYF (match_operand:ANYF 2 "register_operand" "f"))))]
  "ISA_HAS_FP_RECIP_RSQRT (<MODE>mode) && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "rsqrt.<fmt>\t%0,%2";
}
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "insn_count")
        (if_then_else (match_test "TARGET_FIX_SB1")
                      (const_int 2)
                      (const_int 1)))])

(define_insn "*rsqrt<mode>b"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
			     (match_operand:ANYF 2 "register_operand" "f"))))]
  "ISA_HAS_FP_RECIP_RSQRT (<MODE>mode) && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "rsqrt.<fmt>\t%0,%2";
}
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "insn_count")
        (if_then_else (match_test "TARGET_FIX_SB1")
                      (const_int 2)
                      (const_int 1)))])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

;; Do not use the integer abs macro instruction, since that signals an
;; exception on -2147483648 (sigh).

;; The "legacy" (as opposed to "2008") form of ABS.fmt is an arithmetic
;; instruction that treats all NaN inputs as invalid; it does not clear
;; their sign bit.  We therefore can't use that form if the signs of
;; NaNs matter.

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "mips_abs == MIPS_IEEE_754_2008 || !HONOR_NANS (<MODE>mode)"
  "abs.<fmt>\t%0,%1"
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
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(clz:GPR (match_operand:GPR 1 "register_operand" "d")))]
  "ISA_HAS_CLZ_CLO"
  "<d>clz\t%0,%1"
  [(set_attr "type" "clz")
   (set_attr "mode" "<MODE>")])


(define_insn "*clo<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(clz:GPR (not:GPR (match_operand:GPR 1 "register_operand" "d"))))]
  "ISA_HAS_CLZ_CLO"
  "<d>clo\t%0,%1"
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
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(ctz:GPR (match_operand:GPR 1 "register_operand" "d")))]
  "ISA_HAS_CTZ_CTO"
  "<d>ctz\t%0,%1"
  [(set_attr "type" "clz")
   (set_attr "mode" "<MODE>")])


;;
;;  ...................
;;
;;  Count number of set bits.
;;
;;  ...................
;;

(define_insn "popcount<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(popcount:GPR (match_operand:GPR 1 "register_operand" "d")))]
  "ISA_HAS_POP"
  "<d>pop\t%0,%1"
  [(set_attr "type" "pop")
   (set_attr "mode" "<MODE>")])

;; The POP instruction is special as it does not take into account the upper
;; 32bits and is documented that way.
(define_insn "*popcountdi2_trunc"
  [(set (match_operand:SI 0 "register_operand" "=d")
       (popcount:SI (truncate:SI (match_operand:DI 1 "register_operand" "d"))))]
  "ISA_HAS_POP && TARGET_64BIT"
  "pop\t%0,%1"
  [(set_attr "type" "pop")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	NEGATION and ONE'S COMPLEMENT
;;
;;  ....................

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
{
  if (TARGET_MIPS16)
    return "neg\t%0,%1";
  else
    return "subu\t%0,%.,%1";
}
  [(set_attr "alu_type"	"sub")
   (set_attr "mode"	"SI")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "dsubu\t%0,%.,%1"
  [(set_attr "alu_type"	"sub")
   (set_attr "mode"	"DI")])

;; The "legacy" (as opposed to "2008") form of NEG.fmt is an arithmetic
;; instruction that treats all NaN inputs as invalid; it does not flip
;; their sign bit.  We therefore can't use that form if the signs of
;; NaNs matter.

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "mips_abs == MIPS_IEEE_754_2008 || !HONOR_NANS (<MODE>mode)"
  "neg.<fmt>\t%0,%1"
  [(set_attr "type" "fneg")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d")
	(not:GPR (match_operand:GPR 1 "register_operand" "!u,d")))]
  ""
{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  else
    return "nor\t%0,%.,%1";
}
  [(set_attr "alu_type" "not")
   (set_attr "compression" "micromips,*")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

;; Many of these instructions use trivial define_expands, because we
;; want to use a different set of constraints when TARGET_MIPS16.

(define_expand "and<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(and:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "and_reg_operand")))])

;; The middle-end is not allowed to convert ANDing with 0xffff_ffff into a
;; zero_extendsidi2 because of TARGET_TRULY_NOOP_TRUNCATION, so handle these
;; here.  Note that this variant does not trigger for SI mode because we
;; require a 64-bit HOST_WIDE_INT and 0xffff_ffff wouldn't be a canonical
;; sign-extended SImode value.
;;
;; These are possible combinations for operand 1 and 2.  The table
;; includes both MIPS and MIPS16 cases.  (r=register, mem=memory,
;; 16=MIPS16, x=match, S=split):
;;
;;     \ op1    r/EXT   r/!EXT  mem   r/16   mem/16
;;  op2
;;
;;  andi           x     x
;;  0xff           x     x       x             x
;;  0xffff         x     x       x             x
;;  0xffff_ffff    x     S       x     S       x
;;  low-bitmask    x
;;  register       x     x
;;  register =op1                      x

(define_insn "*and<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,!u,d,d,d,!u,d,d")
	(and:GPR (match_operand:GPR 1 "nonimmediate_operand" "o,o,W,!u,d,d,d,0,d,0")
		 (match_operand:GPR 2 "and_operand" "Yb,Yh,Yw,Uean,K,Yx,Yw,!u,d,Yz")))]
  "!TARGET_MIPS16 && and_operands_ok (<MODE>mode, operands[1], operands[2])"
{
  int len;
  int pos;

  switch (which_alternative)
    {
    case 0:
      operands[1] = gen_lowpart (QImode, operands[1]);
      return "lbu\t%0,%1";
    case 1:
      operands[1] = gen_lowpart (HImode, operands[1]);
      return "lhu\t%0,%1";
    case 2:
      operands[1] = gen_lowpart (SImode, operands[1]);
      return "lwu\t%0,%1";
    case 3:
    case 4:
      return "andi\t%0,%1,%x2";
    case 5:
      len = low_bitmask_len (<MODE>mode, INTVAL (operands[2]));
      operands[2] = GEN_INT (len);
      return "<d>ext\t%0,%1,0,%2";
    case 6:
      return "#";
    case 7:
    case 8:
      return "and\t%0,%1,%2";
    case 9:
      mips_bit_clear_info (<MODE>mode, INTVAL (operands[2]), &pos, &len);
      operands[1] = GEN_INT (pos);
      operands[2] = GEN_INT (len);
      return "<d>ins\t%0,$0,%1,%2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "move_type" "load,load,load,andi,andi,ext_ins,shift_shift,logical,logical,ext_ins")
   (set_attr "compression" "*,*,*,micromips,*,*,*,micromips,*,*")
   (set_attr "mode" "<MODE>")])

(define_insn "*and<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,d,d,d,d,d,d,d")
	(and:GPR (match_operand:GPR 1 "nonimmediate_operand" "%0,0,W,W,W,d,0,d,0,0?")
		 (match_operand:GPR 2 "and_operand" "Yb,Yh,Yb,Yh,Yw,Yw,d,Yx,Yz,K")))]
  "TARGET_MIPS16 && and_operands_ok (<MODE>mode, operands[1], operands[2])"
{
  int len;
  int pos;

  switch (which_alternative)
    {
    case 0:
      return "zeb\t%0";
    case 1:
      return "zeh\t%0";
    case 2:
      operands[1] = gen_lowpart (QImode, operands[1]);
      return "lbu\t%0,%1";
    case 3:
      operands[1] = gen_lowpart (HImode, operands[1]);
      return "lhu\t%0,%1";
    case 4:
      operands[1] = gen_lowpart (SImode, operands[1]);
      return "lwu\t%0,%1";
    case 5:
      return "#";
    case 6:
      return "and\t%0,%2";
    case 7:
      len = low_bitmask_len (<MODE>mode, INTVAL (operands[2]));
      operands[2] = GEN_INT (len);
      return "ext\t%0,%1,0,%2";
    case 8:
      mips_bit_clear_info (<MODE>mode, INTVAL (operands[2]), &pos, &len);
      operands[1] = GEN_INT (pos);
      operands[2] = GEN_INT (len);
      return "ins\t%0,$0,%1,%2";
    case 9:
      return "andi\t%0,%x2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "move_type" "andi,andi,load,load,load,shift_shift,logical,ext_ins,ext_ins,andi")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "no,no,no,no,no,no,no,yes,yes,yes")
   (set (attr "enabled")
   (cond [(and (eq_attr "alternative" "9")
			   (not (match_test "ISA_HAS_MIPS16E2")))
		  (const_string "no")
		  (and (eq_attr "alternative" "0,1")
			   (match_test "!GENERATE_MIPS16E"))
		  (const_string "no")]
		 (const_string "yes")))])

(define_expand "ior<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(ior:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "uns_arith_operand")))]
  ""
{
  if (TARGET_MIPS16 && !ISA_HAS_MIPS16E2)
    operands[2] = force_reg (<MODE>mode, operands[2]);
})

(define_insn "*ior<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d,d")
	(ior:GPR (match_operand:GPR 1 "register_operand" "%0,d,d")
		 (match_operand:GPR 2 "uns_arith_operand" "!u,d,K")))]
  "!TARGET_MIPS16"
  "@
   or\t%0,%1,%2
   or\t%0,%1,%2
   ori\t%0,%1,%x2"
  [(set_attr "alu_type" "or")
   (set_attr "compression" "micromips,*,*")
   (set_attr "mode" "<MODE>")])

(define_insn "*iorsi3_mips16_asmacro"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "ISA_HAS_MIPS16E2"
  "@
   or\t%0,%2
   ori\t%0,%x2"
   [(set_attr "alu_type" "or")
    (set_attr "mode" "SI")
    (set_attr "extended_mips16" "*,yes")])

(define_insn "*ior<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(ior:GPR (match_operand:GPR 1 "register_operand" "%0")
		 (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_MIPS16 && !ISA_HAS_MIPS16E2"
  "or\t%0,%2"
  [(set_attr "alu_type" "or")
   (set_attr "mode" "<MODE>")])

(define_expand "xor<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(xor:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "uns_arith_operand")))]
  ""
  "")

(define_insn "*xor<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d,d")
	(xor:GPR (match_operand:GPR 1 "register_operand" "%0,d,d")
		 (match_operand:GPR 2 "uns_arith_operand" "!u,d,K")))]
  "!TARGET_MIPS16"
  "@
   xor\t%0,%1,%2
   xor\t%0,%1,%2
   xori\t%0,%1,%x2"
  [(set_attr "alu_type" "xor")
   (set_attr "compression" "micromips,*,*")
   (set_attr "mode" "<MODE>")])

;; We increase statically the cost of the output register for XORI
;; to counterweight LRA cost calculation as XORI tends to be chosen
;; frequently hurting the code size.  The reason of not choosing CMPI is
;; that LRA tends to add up the cost of the T register as it is in a small
;; class and a possible reload.  In reality, the use of T register comes for
;; free in a number of cases as we don't need any MIPS16 registers.
(define_insn "*xor<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d,t,t,t,d?")
	(xor:GPR (match_operand:GPR 1 "register_operand" "%0,d,d,d,0")
		 (match_operand:GPR 2 "uns_arith_operand" "d,Uub8,K,d,K")))]
  "TARGET_MIPS16"
  "@
   xor\t%0,%2
   cmpi\t%1,%2
   cmpi\t%1,%2
   cmp\t%1,%2
   xori\t%0,%x2"
  [(set_attr "alu_type" "xor")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "no,no,yes,no,yes")
   (set (attr "enabled")
		(cond [(and (eq_attr "alternative" "4")
					(not (match_test "ISA_HAS_MIPS16E2")))
			   (const_string "no")]
			  (const_string "yes")))])

(define_insn "*nor<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(and:GPR (not:GPR (match_operand:GPR 1 "register_operand" "d"))
		 (not:GPR (match_operand:GPR 2 "register_operand" "d"))))]
  "!TARGET_MIPS16"
  "nor\t%0,%1,%2"
  [(set_attr "alu_type" "nor")
   (set_attr "mode" "<MODE>")])

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
  "cvt.s.d\t%0,%1"
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
  [(set (match_operand:SUBDI 0 "nonimmediate_operand" "=d,m")
        (truncate:SUBDI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sll\t%0,%1,0
    <store>\t%1,%0"
  [(set_attr "move_type" "sll0,store")
   (set_attr "mode" "SI")])

;; Combiner patterns to optimize shift/truncate combinations.

(define_insn "*ashr_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	  (ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		       (match_operand:DI 2 "const_arith_operand" ""))))]
  "TARGET_64BIT && !TARGET_MIPS16 && IN_RANGE (INTVAL (operands[2]), 32, 63)"
  "dsra\t%0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

(define_insn "*lshr32_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	  (lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		       (const_int 32))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "dsra\t%0,%1,32"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

;; Logical shift by more than 32 results in proper SI values so truncation is
;; removed by the middle end.  Note that a logical shift by 32 is handled by
;; the previous pattern.
(define_insn "*<optab>_trunc<mode>_exts"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	 (any_shiftrt:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "const_arith_operand" ""))))]
  "ISA_HAS_EXTS && TARGET_64BIT && UINTVAL (operands[2]) < 32"
  "exts\t%0,%1,%2,31"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

;; This could likely be generalized for any SUBDI mode, and any right
;; shift, but AFAICT this is used so rarely it is not worth the additional
;; complexity.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (ashiftrt:SI
	  (truncate:SI
	    (ashift:DI (match_operand:DI 1 "register_operand" "d")
		       (match_operand:DI 2 "const_arith_operand" "")))
	  (match_operand:DI 3 "const_arith_operand" "")))]
  "(ISA_HAS_EXTS && TARGET_64BIT
    && UINTVAL (operands[2]) < 32 && UINTVAL (operands[3]) < 32
    && UINTVAL (operands[3]) >= UINTVAL (operands[2]))"
  {
    rtx xoperands[4];
    xoperands[0] = operands[0];
    xoperands[1] = operands[1];

    /* The length of the field is the size of the outer mode less the outer
       shift constant.  We fix the outer mode as SImode for simplicity.  */
    unsigned int right_shift = INTVAL (operands[3]);
    xoperands[3] = GEN_INT (32 - right_shift);

    /* The field starts at the outer shift constant less the inner shift
       constant.  */
    unsigned int left_shift = INTVAL (operands[2]);
    xoperands[2] = GEN_INT (right_shift - left_shift);

    /* Sanity checks.  These constraints are taken from the MIPS ISA
       manual.  */
    gcc_assert (INTVAL (xoperands[2]) >= 0 && INTVAL (xoperands[2]) < 32);
    gcc_assert (INTVAL (xoperands[3]) > 0 && INTVAL (xoperands[3]) <= 32);
    gcc_assert (INTVAL (xoperands[2]) + INTVAL (xoperands[3]) > 0
		&& INTVAL (xoperands[2]) + INTVAL (xoperands[3]) <= 32);

    output_asm_insn ("exts\t%0,%1,%2,%m3", xoperands);
    return "";
  }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand")))]
  "TARGET_64BIT")

(define_insn_and_split "*zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,W")))]
  "TARGET_64BIT && !ISA_HAS_EXT_INS"
  "@
   #
   lwu\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
        (lshiftrt:DI (match_dup 0) (const_int 32)))]
  { operands[1] = gen_lowpart (DImode, operands[1]); }
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "DI")])

(define_insn "*zero_extendsidi2_dext"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,W")))]
  "TARGET_64BIT && ISA_HAS_EXT_INS"
  "@
   dext\t%0,%1,0,32
   lwu\t%0,%1"
  [(set_attr "move_type" "arith,load")
   (set_attr "mode" "DI")])

;; See the comment before the *and<mode>3 pattern why this is generated by
;; combine.

(define_split
  [(set (match_operand:DI 0 "register_operand")
        (and:DI (match_operand:DI 1 "register_operand")
		(const_int 4294967295)))]
  "TARGET_64BIT && !ISA_HAS_EXT_INS && reload_completed"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
        (lshiftrt:DI (match_dup 0) (const_int 32)))])

(define_expand "zero_extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand")
        (zero_extend:GPR (match_operand:SHORT 1 "nonimmediate_operand")))]
  ""
{
  if (TARGET_MIPS16 && !GENERATE_MIPS16E
      && !memory_operand (operands[1], <SHORT:MODE>mode))
    {
      emit_insn (gen_and<GPR:mode>3 (operands[0],
				     gen_lowpart (<GPR:MODE>mode, operands[1]),
				     force_reg (<GPR:MODE>mode,
						GEN_INT (<SHORT:mask>))));
      DONE;
    }
})

(define_insn "*zero_extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d,d")
        (zero_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "!u,d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,<SHORT:mask>
   andi\t%0,%1,<SHORT:mask>
   l<SHORT:size>u\t%0,%1"
  [(set_attr "move_type" "andi,andi,load")
   (set_attr "compression" "micromips,*,*")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*zero_extend<SHORT:mode><GPR:mode>2_mips16e"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR (match_operand:SHORT 1 "register_operand" "0")))]
  "GENERATE_MIPS16E"
  "ze<SHORT:size>\t%0"
  ;; This instruction is effectively a special encoding of ANDI.
  [(set_attr "move_type" "andi")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*zero_extend<SHORT:mode><GPR:mode>2_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR (match_operand:SHORT 1 "memory_operand" "m")))]
  "TARGET_MIPS16"
  "l<SHORT:size>u\t%0,%1"
  [(set_attr "move_type" "load")
   (set_attr "mode" "<GPR:MODE>")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand")))]
  ""
{
  if (TARGET_MIPS16 && !memory_operand (operands[1], QImode))
    {
      emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				       operands[1]));
      DONE;
    }
})

(define_insn "*zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,0x00ff
   lbu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "mode" "HI")])

(define_insn "*zero_extendqihi2_mips16"
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_MIPS16"
  "lbu\t%0,%1"
  [(set_attr "move_type" "load")
   (set_attr "mode" "HI")])

;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn "*zero_extend<GPR:mode>_trunc<SHORT:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  operands[2] = GEN_INT (GET_MODE_MASK (<SHORT:MODE>mode));
  return "andi\t%0,%1,%x2";
}
  [(set_attr "alu_type" "and")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*zero_extendhi_truncqi"
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
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
  [(set (match_operand:DI 0 "register_operand" "=d,l,d")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "0,0,m")))]
  "TARGET_64BIT"
  "@
   #
   #
   lw\t%0,%1"
  "&& reload_completed && register_operand (operands[1], VOIDmode)"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
}
  [(set_attr "move_type" "move,move,load")
   (set_attr "mode" "DI")])

(define_expand "extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand")
        (sign_extend:GPR (match_operand:SHORT 1 "nonimmediate_operand")))]
  "")

(define_insn "*extend<SHORT:mode><GPR:mode>2_mips16e"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (sign_extend:GPR (match_operand:SHORT 1 "nonimmediate_operand" "0,m")))]
  "GENERATE_MIPS16E"
  "@
   se<SHORT:size>\t%0
   l<SHORT:size>\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn_and_split "*extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (sign_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "d,m")))]
  "!ISA_HAS_SEB_SEH && !GENERATE_MIPS16E"
  "@
   #
   l<SHORT:size>\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0) (ashift:GPR (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:GPR (match_dup 0) (match_dup 2)))]
{
  operands[1] = gen_lowpart (<GPR:MODE>mode, operands[1]);
  operands[2] = GEN_INT (GET_MODE_BITSIZE (<GPR:MODE>mode)
			 - GET_MODE_BITSIZE (<SHORT:MODE>mode));
}
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*extend<SHORT:mode><GPR:mode>2_se<SHORT:size>"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (sign_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "d,m")))]
  "ISA_HAS_SEB_SEH"
  "@
   se<SHORT:size>\t%0,%1
   l<SHORT:size>\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand")
        (sign_extend:HI (match_operand:QI 1 "nonimmediate_operand")))]
  "")

(define_insn "*extendqihi2_mips16e"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "0,m")))]
  "GENERATE_MIPS16E"
  "@
   seb\t%0
   lb\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "SI")])

(define_insn_and_split "*extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (sign_extend:HI
	     (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "!ISA_HAS_SEB_SEH && !GENERATE_MIPS16E"
  "@
   #
   lb\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 2)))]
{
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = GEN_INT (GET_MODE_BITSIZE (SImode)
			 - GET_MODE_BITSIZE (QImode));
}
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "SI")])

(define_insn "*extendqihi2_seb"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (sign_extend:HI
	     (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "ISA_HAS_SEB_SEH"
  "@
   seb\t%0,%1
   lb\t%0,%1"
  [(set_attr "move_type" "signext,load")
   (set_attr "mode" "SI")])

;; Combiner patterns for truncate/sign_extend combinations.  The SI versions
;; use the shift/truncate patterns.

(define_insn_and_split "*extenddi_truncate<mode>"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && !ISA_HAS_EXTS"
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
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && !ISA_HAS_EXTS"
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
  [(set (match_operand:HI 0 "register_operand" "=d")
	(sign_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && !ISA_HAS_EXTS"
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

(define_insn "*extend<GPR:mode>_truncate<SHORT:mode>_exts"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(sign_extend:GPR
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && ISA_HAS_EXTS"
{
  operands[2] = GEN_INT (GET_MODE_BITSIZE (<SHORT:MODE>mode));
  return "exts\t%0,%1,0,%m2";
}
  [(set_attr "type" "arith")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*extendhi_truncateqi_exts"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(sign_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && ISA_HAS_EXTS"
  "exts\t%0,%1,0,7"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.d.s\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "cnv_mode"	"S2D")   
   (set_attr "mode"	"DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_expand "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand")
	(fix:SI (match_operand:DF 1 "register_operand")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  if (!ISA_HAS_TRUNC_W)
    {
      emit_insn (gen_fix_truncdfsi2_macro (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "fix_truncdfsi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W"
  "trunc.w.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])

(define_insn "fix_truncdfsi2_macro"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))
   (clobber (match_scratch:DF 2 "=d"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W"
{
  if (mips_nomacro.nesting_level > 0)
    return ".set\tmacro\;trunc.w.d %0,%1,%2\;.set\tnomacro";
  else
    return "trunc.w.d %0,%1,%2";
}
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")
   (set_attr "insn_count" "9")])

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand")
	(fix:SI (match_operand:SF 1 "register_operand")))]
  "TARGET_HARD_FLOAT"
{
  if (!ISA_HAS_TRUNC_W)
    {
      emit_insn (gen_fix_truncsfsi2_macro (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "fix_truncsfsi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && ISA_HAS_TRUNC_W"
  "trunc.w.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])

(define_insn "fix_truncsfsi2_macro"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))
   (clobber (match_scratch:SF 2 "=d"))]
  "TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W"
{
  if (mips_nomacro.nesting_level > 0)
    return ".set\tmacro\;trunc.w.s %0,%1,%2\;.set\tnomacro";
  else
    return "trunc.w.s %0,%1,%2";
}
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")
   (set_attr "insn_count" "9")])


(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "trunc.l.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "trunc.l.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])


(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.d.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "cvt.d.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "cvt.s.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "cvt.s.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


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
      mips_emit_move (reg1, const_double_from_real_value (offset, DFmode));
      do_pending_stack_adjust ();

      test = gen_rtx_GE (VOIDmode, operands[1], reg1);
      emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

      emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (pc_rtx,
                                   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      mips_emit_move (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
      mips_emit_move (reg3, GEN_INT (trunc_int_for_mode
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

  mips_emit_move (reg1, const_double_from_real_value (offset, DFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncdfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  mips_emit_move (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
  mips_emit_move (reg3, GEN_INT (BITMASK_HIGH));
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

  mips_emit_move (reg1, const_double_from_real_value (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  mips_emit_move (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  mips_emit_move (reg3, GEN_INT (trunc_int_for_mode
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

  mips_emit_move (reg1, const_double_from_real_value (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  mips_emit_move (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  mips_emit_move (reg3, GEN_INT (BITMASK_HIGH));
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
;;	DATA MOVEMENT
;;
;;  ....................

;; Bit field extract patterns which use lwl/lwr or ldl/ldr.

(define_expand "extvmisalign<mode>"
  [(set (match_operand:GPR 0 "register_operand")
	(sign_extract:GPR (match_operand:BLK 1 "memory_operand")
			  (match_operand 2 "const_int_operand")
			  (match_operand 3 "const_int_operand")))]
  "ISA_HAS_LWL_LWR"
{
  if (mips_expand_ext_as_unaligned_load (operands[0], operands[1],
					 INTVAL (operands[2]),
					 INTVAL (operands[3]),
					 /*unsigned=*/ false))
    DONE;
  else
    FAIL;
})

(define_expand "extv<mode>"
  [(set (match_operand:GPR 0 "register_operand")
	(sign_extract:GPR (match_operand:GPR 1 "register_operand")
			  (match_operand 2 "const_int_operand")
			  (match_operand 3 "const_int_operand")))]
  "ISA_HAS_EXTS"
{
  if (UINTVAL (operands[2]) > 32)
    FAIL;
})

(define_insn "*extv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (sign_extract:GPR (match_operand:GPR 1 "register_operand" "d")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "ISA_HAS_EXTS && UINTVAL (operands[2]) <= 32"
  "exts\t%0,%1,%3,%m2"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "<MODE>")])

(define_expand "extzvmisalign<mode>"
  [(set (match_operand:GPR 0 "register_operand")
	(zero_extract:GPR (match_operand:BLK 1 "memory_operand")
			  (match_operand 2 "const_int_operand")
			  (match_operand 3 "const_int_operand")))]
  "ISA_HAS_LWL_LWR"
{
  if (mips_expand_ext_as_unaligned_load (operands[0], operands[1],
					 INTVAL (operands[2]),
					 INTVAL (operands[3]),
					 /*unsigned=*/ true))
    DONE;
  else
    FAIL;
})

(define_expand "extzv<mode>"
  [(set (match_operand:GPR 0 "register_operand")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand")
			  (match_operand 2 "const_int_operand")
			  (match_operand 3 "const_int_operand")))]
  ""
{
  if (!mips_use_ins_ext_p (operands[1], INTVAL (operands[2]),
			   INTVAL (operands[3])))
    FAIL;
})

(define_insn "*extzv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "d")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "mips_use_ins_ext_p (operands[1], INTVAL (operands[2]),
		       INTVAL (operands[3]))"
  "<d>ext\t%0,%1,%3,%2"
  [(set_attr "type"	"arith")
   (set_attr "extended_mips16"  "yes")
   (set_attr "mode"	"<MODE>")])

(define_insn "*extzv_truncsi_exts"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI
	 (zero_extract:DI (match_operand:DI 1 "register_operand" "d")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" ""))))]
  "ISA_HAS_EXTS && TARGET_64BIT && IN_RANGE (INTVAL (operands[2]), 32, 63)"
  "exts\t%0,%1,%3,31"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])

(define_insn "*insqisi_extended"
  [(set (match_operand:DI 0 "register_operand" "=d")
    (sign_extend:DI
      (ior:SI (and:SI (subreg:SI (match_dup 0) 0)
		(const_int 16777215))
	      (ashift:SI
		(subreg:SI (match_operand:QI 1 "register_operand" "d") 0)
		(const_int 24)))))]
  "TARGET_64BIT && !TARGET_MIPS16 && ISA_HAS_EXT_INS"
  "ins\t%0,%1,24,8"
  [(set_attr "mode" "SI")
   (set_attr "perf_ratio" "1")])

(define_insn "*inshisi_extended"
  [(set (match_operand:DI 0 "register_operand" "=d")
    (sign_extend:DI
      (ior:SI
	(ashift:SI (subreg:SI (match_operand:HI 1 "register_operand" "d") 0)
	  (const_int 16))
	(zero_extend:SI (subreg:HI (match_dup 0) 0)))))]
  "TARGET_64BIT && !TARGET_MIPS16 && ISA_HAS_EXT_INS"
  "ins\t%0,%1,16,16"
  [(set_attr "mode" "SI")
   (set_attr "perf_ratio" "1")])

(define_expand "insvmisalign<mode>"
  [(set (zero_extract:GPR (match_operand:BLK 0 "memory_operand")
			  (match_operand 1 "const_int_operand")
			  (match_operand 2 "const_int_operand"))
	(match_operand:GPR 3 "reg_or_0_operand"))]
  "ISA_HAS_LWL_LWR"
{
  if (mips_expand_ins_as_unaligned_store (operands[0], operands[3],
					  INTVAL (operands[1]),
					  INTVAL (operands[2])))
    DONE;
  else
    FAIL;
})

(define_expand "insv<mode>"
  [(set (zero_extract:GPR (match_operand:GPR 0 "register_operand")
			  (match_operand 1 "const_int_operand")
			  (match_operand 2 "const_int_operand"))
	(match_operand:GPR 3 "reg_or_0_operand"))]
  ""
{
  if (!mips_use_ins_ext_p (operands[0], INTVAL (operands[1]),
			   INTVAL (operands[2])))
    FAIL;
})

(define_insn "*insv<mode>"
  [(set (zero_extract:GPR (match_operand:GPR 0 "register_operand" "+d")
			  (match_operand:SI 1 "const_int_operand" "")
			  (match_operand:SI 2 "const_int_operand" ""))
	(match_operand:GPR 3 "reg_or_0_operand" "dJ"))]
  "mips_use_ins_ext_p (operands[0], INTVAL (operands[1]),
		       INTVAL (operands[2]))"
  "<d>ins\t%0,%z3,%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "extended_mips16"  "yes")
   (set_attr "mode"	"<MODE>")])

;; Combiner pattern for cins (clear and insert bit field).  We can
;; implement mask-and-shift-left operation with this.  Note that if
;; the upper bit of the mask is set in an SImode operation, the mask
;; itself will be sign-extended.  mask_low_and_shift_len will
;; therefore be greater than our threshold of 32.

(define_insn "*cins<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(and:GPR
	 (ashift:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "const_int_operand" ""))
	 (match_operand:GPR 3 "const_int_operand" "")))]
  "ISA_HAS_CINS
   && mask_low_and_shift_p (<MODE>mode, operands[3], operands[2], 32)"
{
  operands[3] =
    GEN_INT (mask_low_and_shift_len (<MODE>mode, operands[3], operands[2]));
  return "cins\t%0,%1,%2,%m3";
}
  [(set_attr "type"     "shift")
   (set_attr "mode"     "<MODE>")])

;; Unaligned word moves generated by the bit field patterns.
;;
;; As far as the rtl is concerned, both the left-part and right-part
;; instructions can access the whole field.  However, the real operand
;; refers to just the first or the last byte (depending on endianness).
;; We therefore use two memory operands to each instruction, one to
;; describe the rtl effect and one to use in the assembly output.
;;
;; Operands 0 and 1 are the rtl-level target and source respectively.
;; This allows us to use the standard length calculations for the "load"
;; and "store" type attributes.

(define_insn "mov_<load>l"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(unspec:GPR [(match_operand:BLK 1 "memory_operand" "m")
		     (match_operand:QI 2 "memory_operand" "ZC")]
		    UNSPEC_LOAD_LEFT))]
  "(!TARGET_MIPS16 || ISA_HAS_MIPS16E2)
    && mips_mem_fits_mode_p (<MODE>mode, operands[1])"
  "<load>l\t%0,%2"
  [(set_attr "move_type" "load")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "yes")])

(define_insn "mov_<load>r"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(unspec:GPR [(match_operand:BLK 1 "memory_operand" "m")
		     (match_operand:QI 2 "memory_operand" "ZC")
		     (match_operand:GPR 3 "register_operand" "0")]
		    UNSPEC_LOAD_RIGHT))]
  "(!TARGET_MIPS16 || ISA_HAS_MIPS16E2)
    && mips_mem_fits_mode_p (<MODE>mode, operands[1])"
  "<load>r\t%0,%2"
  [(set_attr "move_type" "load")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "yes")])

(define_insn "mov_<store>l"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
	(unspec:BLK [(match_operand:GPR 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "ZC")]
		    UNSPEC_STORE_LEFT))]
  "!TARGET_MIPS16
   && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>l\t%z1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "mov_<store>r"
  [(set (match_operand:BLK 0 "memory_operand" "+m")
	(unspec:BLK [(match_operand:GPR 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "ZC")
		     (match_dup 0)]
		    UNSPEC_STORE_RIGHT))]
  "!TARGET_MIPS16
   && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>r\t%z1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "mov_<store>l_mips16e2"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
    (unspec:BLK [(match_operand:GPR 1 "register_operand" "d")
		     (match_operand:QI 2 "memory_operand" "ZC")]
		    UNSPEC_STORE_LEFT))]
  "TARGET_MIPS16 && ISA_HAS_MIPS16E2
   && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>l\t%1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "yes")])

(define_insn "mov_<store>r_mips16e2"
  [(set (match_operand:BLK 0 "memory_operand" "+m")
    (unspec:BLK [(match_operand:GPR 1 "register_operand" "d")
		     (match_operand:QI 2 "memory_operand" "ZC")
		     (match_dup 0)]
		    UNSPEC_STORE_RIGHT))]
  "TARGET_MIPS16 && ISA_HAS_MIPS16E2
   && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>r\t%1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "yes")])

;; Unaligned direct access
(define_expand "movmisalign<mode>"
  [(set (match_operand:JOIN_MODE 0)
	(match_operand:JOIN_MODE 1))]
  "ISA_HAS_UNALIGNED_ACCESS"
{
  if (mips_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

;; An instruction to calculate the high part of a 64-bit SYMBOL_ABSOLUTE.
;; The required value is:
;;
;;	(%highest(op1) << 48) + (%higher(op1) << 32) + (%hi(op1) << 16)
;;
;; which translates to:
;;
;;	lui	op0,%highest(op1)
;;	daddiu	op0,op0,%higher(op1)
;;	dsll	op0,op0,16
;;	daddiu	op0,op0,%hi(op1)
;;	dsll	op0,op0,16
;;
;; The split is deferred until after flow2 to allow the peephole2 below
;; to take effect.
(define_insn_and_split "*lea_high64"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(high:DI (match_operand:DI 1 "absolute_symbolic_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && ABI_HAS_64BIT_SYMBOLS"
  "#"
  "&& epilogue_completed"
  [(set (match_dup 0) (high:DI (match_dup 2)))
   (set (match_dup 0) (lo_sum:DI (match_dup 0) (match_dup 2)))
   (set (match_dup 0) (ashift:DI (match_dup 0) (const_int 16)))
   (set (match_dup 0) (lo_sum:DI (match_dup 0) (match_dup 3)))
   (set (match_dup 0) (ashift:DI (match_dup 0) (const_int 16)))]
{
  operands[2] = mips_unspec_address (operands[1], SYMBOL_64_HIGH);
  operands[3] = mips_unspec_address (operands[1], SYMBOL_64_MID);
}
  [(set_attr "insn_count" "5")])

;; Use a scratch register to reduce the latency of the above pattern
;; on superscalar machines.  The optimized sequence is:
;;
;;	lui	op1,%highest(op2)
;;	lui	op0,%hi(op2)
;;	daddiu	op1,op1,%higher(op2)
;;	dsll32	op1,op1,0
;;	daddu	op1,op1,op0
(define_peephole2
  [(set (match_operand:DI 1 "d_operand")
	(high:DI (match_operand:DI 2 "absolute_symbolic_operand")))
   (match_scratch:DI 0 "d")]
  "TARGET_EXPLICIT_RELOCS && ABI_HAS_64BIT_SYMBOLS"
  [(set (match_dup 1) (high:DI (match_dup 3)))
   (set (match_dup 0) (high:DI (match_dup 4)))
   (set (match_dup 1) (lo_sum:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 1) (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 1) (plus:DI (match_dup 1) (match_dup 0)))]
{
  operands[3] = mips_unspec_address (operands[2], SYMBOL_64_HIGH);
  operands[4] = mips_unspec_address (operands[2], SYMBOL_64_LOW);
})

;; On most targets, the expansion of (lo_sum (high X) X) for a 64-bit
;; SYMBOL_ABSOLUTE X will take 6 cycles.  This next pattern allows combine
;; to merge the HIGH and LO_SUM parts of a move if the HIGH part is only
;; used once.  We can then use the sequence:
;;
;;	lui	op0,%highest(op1)
;;	lui	op2,%hi(op1)
;;	daddiu	op0,op0,%higher(op1)
;;	daddiu	op2,op2,%lo(op1)
;;	dsll32	op0,op0,0
;;	daddu	op0,op0,op2
;;
;; which takes 4 cycles on most superscalar targets.
(define_insn_and_split "*lea64"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(match_operand:DI 1 "absolute_symbolic_operand" ""))
   (clobber (match_scratch:DI 2 "=&d"))]
  "!TARGET_MIPS16
   && TARGET_EXPLICIT_RELOCS
   && ABI_HAS_64BIT_SYMBOLS
   && cse_not_expected"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (high:DI (match_dup 3)))
   (set (match_dup 2) (high:DI (match_dup 4)))
   (set (match_dup 0) (lo_sum:DI (match_dup 0) (match_dup 3)))
   (set (match_dup 2) (lo_sum:DI (match_dup 2) (match_dup 4)))
   (set (match_dup 0) (ashift:DI (match_dup 0) (const_int 32)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 2)))]
{
  operands[3] = mips_unspec_address (operands[1], SYMBOL_64_HIGH);
  operands[4] = mips_unspec_address (operands[1], SYMBOL_64_LOW);
}
  [(set_attr "insn_count" "6")])

;; Split HIGHs into:
;;
;;	li op0,%hi(sym)
;;	sll op0,16
;;
;; on MIPS16 targets.
(define_split
  [(set (match_operand:P 0 "d_operand")
	(high:P (match_operand:P 1 "symbolic_operand_with_high")))]
  "TARGET_MIPS16 && reload_completed && !ISA_HAS_MIPS16E2"
  [(set (match_dup 0) (unspec:P [(match_dup 1)] UNSPEC_UNSHIFTED_HIGH))
   (set (match_dup 0) (ashift:P (match_dup 0) (const_int 16)))])

(define_insn "*unshifted_high"
  [(set (match_operand:P 0 "d_operand" "=d")
	(unspec:P [(match_operand:P 1 "symbolic_operand_with_high")]
		  UNSPEC_UNSHIFTED_HIGH))]
  ""
  "li\t%0,%h1"
  [(set_attr "extended_mips16" "yes")])

;; Insns to fetch a symbol from a big GOT.

(define_insn_and_split "*xgot_hi<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(high:P (match_operand:P 1 "got_disp_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (high:P (match_dup 2)))
   (set (match_dup 0) (plus:P (match_dup 0) (match_dup 3)))]
{
  operands[2] = mips_unspec_address (operands[1], SYMBOL_GOTOFF_DISP);
  operands[3] = pic_offset_table_rtx;
}
  [(set_attr "got" "xgot_high")
   (set_attr "mode" "<MODE>")])

(define_insn_and_split "*xgot_lo<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "d")
		  (match_operand:P 2 "got_disp_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(unspec:P [(match_dup 1) (match_dup 3)] UNSPEC_LOAD_GOT))]
  { operands[3] = mips_unspec_address (operands[2], SYMBOL_GOTOFF_DISP); }
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Insns to fetch a symbol from a normal GOT.

(define_insn_and_split "*got_disp<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(match_operand:P 1 "got_disp_operand" ""))]
  "TARGET_EXPLICIT_RELOCS && !mips_split_p[SYMBOL_GOT_DISP]"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_got_load (NULL, operands[1], SYMBOL_GOTOFF_DISP); }
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Insns for loading the "page" part of a page/ofst address from the GOT.

(define_insn_and_split "*got_page<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(high:P (match_operand:P 1 "got_page_ofst_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && !mips_split_hi_p[SYMBOL_GOT_PAGE_OFST]"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_got_load (NULL, operands[1], SYMBOL_GOTOFF_PAGE); }
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Convenience expander that generates the rhs of a load_got<mode> insn.
(define_expand "unspec_got_<mode>"
  [(unspec:P [(match_operand:P 0)
	      (match_operand:P 1)] UNSPEC_LOAD_GOT)])

;; Lower-level instructions for loading an address from the GOT.
;; We could use MEMs, but an unspec gives more optimization
;; opportunities.

(define_insn "load_got<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "register_operand" "d")
		   (match_operand:P 2 "immediate_operand" "")]
		  UNSPEC_LOAD_GOT))]
  ""
  "<load>\t%0,%R2(%1)"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Instructions for adding the low 16 bits of an address to a register.
;; Operand 2 is the address: mips_print_operand works out which relocation
;; should be applied.

(define_insn "*low<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "d")
		  (match_operand:P 2 "immediate_operand" "")))]
  "!TARGET_MIPS16"
  "<d>addiu\t%0,%1,%R2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "<MODE>")])

(define_insn "*lowsi_mips16_gp"
  [(set (match_operand:SI 0 "register_operand" "=d")
    (lo_sum:SI (reg:SI GLOBAL_POINTER_REGNUM)
		  (match_operand 1 "immediate_operand" "")))]
  "MIPS16_GP_LOADS"
  "addiu\t%0,$28,%R1"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "yes")])

(define_insn "*low<mode>_mips16"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "0")
		  (match_operand:P 2 "immediate_operand" "")))]
  "TARGET_MIPS16"
  "<d>addiu\t%0,%R2"
  [(set_attr "alu_type" "add")
   (set_attr "mode" "<MODE>")
   (set_attr "extended_mips16" "yes")])

;; Expose MIPS16 uses of the global pointer after reload if the function
;; is responsible for setting up the register itself.
(define_split
  [(set (match_operand:GPR 0 "d_operand")
	(const:GPR (unspec:GPR [(const_int 0)] UNSPEC_GP)))]
  "TARGET_MIPS16 && TARGET_USE_GOT && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  { operands[1] = pic_offset_table_rtx; })

;; Allow combine to split complex const_int load sequences, using operand 2
;; to store the intermediate results.  See move_operand for details.
(define_split
  [(set (match_operand:GPR 0 "register_operand")
	(match_operand:GPR 1 "splittable_const_int_operand"))
   (clobber (match_operand:GPR 2 "register_operand"))]
  ""
  [(const_int 0)]
{
  mips_move_integer (operands[2], operands[0], INTVAL (operands[1]));
  DONE;
})

;; Likewise, for symbolic operands.
(define_split
  [(set (match_operand:P 0 "register_operand")
	(match_operand:P 1))
   (clobber (match_operand:P 2 "register_operand"))]
  "mips_split_symbol (operands[2], operands[1], MAX_MACHINE_MODE, NULL)"
  [(set (match_dup 0) (match_dup 3))]
{
  mips_split_symbol (operands[2], operands[1],
		     MAX_MACHINE_MODE, &operands[3]);
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
  if (mips_legitimize_move (DImode, operands[0], operands[1]))
    DONE;
})

;; For mips16, we need a special case to handle storing $31 into
;; memory, since we don't have a constraint to match $31.  This
;; instruction can be generated by save_restore_insns.

(define_insn "*mov<mode>_ra"
  [(set (match_operand:GPR 0 "stack_operand" "=m")
	(reg:GPR RETURN_ADDR_REGNUM))]
  "TARGET_MIPS16"
  "<store>\t$31,%0"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,m,*a,*a,*d,*f,*f,*d,*m,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "move_operand" "d,i,m,d,*J,*d,*a,*J*d,*m,*f,*f,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,imul,mtlo,mflo,mtc,fpload,mfc,fpstore,mtc,fpload,mfc,fpstore")
   (set (attr "mode")
   	(if_then_else (eq_attr "move_type" "imul")
		      (const_string "SI")
		      (const_string "DI")))])

(define_insn "*movdi_32bit_mips16"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:DI 1 "move_operand" "d,d,y,K,N,m,d,*x"))]
  "!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mflo")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*d,*m,*a,*d,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "move_operand" "d,Yd,Yf,m,dJ,*d*J,*m,*f,*f,*J*d,*a,*d,*m,*B*C*D,*B*C*D"))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store,mtc,fpload,mfc,fpstore,mtlo,mflo,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit_mips16"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,m,*d")
	(match_operand:DI 1 "move_operand" "d,d,y,K,N,Yd,kf,m,d,*a"))]
  "TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,const,loadpool,load,store,mflo")
   (set_attr "mode" "DI")])

;; On the mips16, we can split ld $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:DI 0 "d_operand")
	(mem:DI (plus:DI (match_dup 0)
			 (match_operand:DI 1 "const_int_operand"))))]
  "TARGET_64BIT && TARGET_MIPS16 && reload_completed
   && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x10)
       || (INTVAL (operands[1]) >= 32 * 8
	   && INTVAL (operands[1]) <= 31 * 8 + 0x8)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 8
	   && (INTVAL (operands[1]) & 7) != 0))"
  [(set (match_dup 0) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:DI (plus:DI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = const0_rtx;
  else if (val >= 32 * 8)
    {
      int off = val & 7;

      operands[1] = GEN_INT (0x8 + off);
      operands[2] = GEN_INT (val - off - 0x8);
    }
  else
    {
      int off = val & 7;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

;; 32-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "mov<mode>"
  [(set (match_operand:IMOVE32 0 "")
	(match_operand:IMOVE32 1 ""))]
  ""
{
  if (mips_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "*mov<mode>_internal"
  [(set (match_operand:IMOVE32 0 "nonimmediate_operand" "=d,!u,!u,d,e,!u,!ks,d,ZS,ZT,m,*f,*f,*d,*m,*d,*z,*a,*d,*B*C*D,*B*C*D,*d,*m")
	(match_operand:IMOVE32 1 "move_operand" "d,J,Udb7,Yd,Yf,ZT,ZS,m,!ks,!kbJ,dJ,*d*J,*m,*f,*f,*z,*d,*J*d,*a,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], <MODE>mode)
       || reg_or_0_operand (operands[1], <MODE>mode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,const,const,const,load,load,load,store,store,store,mtc,fpload,mfc,fpstore,mfc,mtc,mtlo,mflo,mtc,fpload,mfc,fpstore")
   (set_attr "compression" "all,micromips,micromips,*,*,micromips,micromips,*,micromips,micromips,*,*,*,*,*,*,*,*,*,*,*,*,*")
   (set_attr "mode" "SI")])

(define_insn "*mov<mode>_mips16"
  [(set (match_operand:IMOVE32 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,m,*d")
	(match_operand:IMOVE32 1 "move_operand" "d,d,y,K,N,Yd,kf,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,const,loadpool,load,store,mflo")
   (set_attr "mode" "SI")])

;; On the mips16, we can split lw $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:SI 0 "d_operand")
	(mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand"))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 4
	   && INTVAL (operands[1]) <= 31 * 4 + 0x7c)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 4
	   && (INTVAL (operands[1]) & 3) != 0))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:SI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = const0_rtx;
  else if (val >= 32 * 4)
    {
      int off = val & 3;

      operands[1] = GEN_INT (0x7c + off);
      operands[2] = GEN_INT (val - off - 0x7c);
    }
  else
    {
      int off = val & 3;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

;; On the mips16, we can split a load of certain constants into a load
;; and an add.  This turns a 4 byte instruction into 2 2 byte
;; instructions.

(define_split
  [(set (match_operand:SI 0 "d_operand")
	(match_operand:SI 1 "const_int_operand"))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && INTVAL (operands[1]) >= 0x100
   && INTVAL (operands[1]) <= 0xff + 0x7f"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
{
  int val = INTVAL (operands[1]);

  operands[1] = GEN_INT (0xff);
  operands[2] = GEN_INT (val - 0xff);
})

;; MIPS4 supports loading and storing a floating point register from
;; the sum of two general registers.  We use two versions for each of
;; these four instructions: one where the two general registers are
;; SImode, and one where they are DImode.  This is because general
;; registers will be in SImode when they hold 32-bit values, but,
;; since the 32-bit values are always sign extended, the [ls][wd]xc1
;; instructions will still work correctly.

;; ??? Perhaps it would be better to support these instructions by
;; modifying TARGET_LEGITIMATE_ADDRESS_P and friends.  However, since
;; these instructions can only be used to load and store floating
;; point registers, that would probably cause trouble in reload.

(define_insn "*<ANYF:loadx>_<P:mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (plus:P (match_operand:P 1 "register_operand" "d")
			  (match_operand:P 2 "register_operand" "d"))))]
  "ISA_HAS_LXC1_SXC1"
  "<ANYF:loadx>\t%0,%1(%2)"
  [(set_attr "type" "fpidxload")
   (set_attr "mode" "<ANYF:UNITMODE>")])

(define_insn "*<ANYF:storex>_<P:mode>"
  [(set (mem:ANYF (plus:P (match_operand:P 1 "register_operand" "d")
			  (match_operand:P 2 "register_operand" "d")))
	(match_operand:ANYF 0 "register_operand" "f"))]
  "ISA_HAS_LXC1_SXC1"
  "<ANYF:storex>\t%0,%1(%2)"
  [(set_attr "type" "fpidxstore")
   (set_attr "mode" "<ANYF:UNITMODE>")])

;; Scaled indexed address load.
;; Per md.texi, we only need to look for a pattern with multiply in the
;; address expression, not shift.

(define_insn "*lwxs"
  [(set (match_operand:IMOVE32 0 "register_operand" "=d")
	(mem:IMOVE32
	  (plus:P (mult:P (match_operand:P 1 "register_operand" "d")
			  (const_int 4))
		  (match_operand:P 2 "register_operand" "d"))))]
  "ISA_HAS_LWXS"
  "lwxs\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")])

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
  if (mips_legitimize_move (HImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,!u,d,!u,d,ZU,m,*a,*d")
	(match_operand:HI 1 "move_operand"         "d,J,I,ZU,m,!kbJ,dJ,*d*J,*a"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || reg_or_0_operand (operands[1], HImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,load,store,store,mtlo,mflo")
   (set_attr "compression" "all,micromips,*,micromips,*,micromips,*,*,*")
   (set_attr "mode" "HI")])

(define_insn "*movhi_mips16"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:HI 1 "move_operand"         "d,d,y,K,N,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mflo")
   (set_attr "mode" "HI")])

;; On the mips16, we can split lh $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:HI 0 "d_operand")
	(mem:HI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand"))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 2
	   && INTVAL (operands[1]) <= 31 * 2 + 0x7e)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 2
	   && (INTVAL (operands[1]) & 1) != 0))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:HI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = const0_rtx;
  else if (val >= 32 * 2)
    {
      int off = val & 1;

      operands[1] = GEN_INT (0x7e + off);
      operands[2] = GEN_INT (val - off - 0x7e);
    }
  else
    {
      int off = val & 1;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

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
  if (mips_legitimize_move (QImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,!u,d,!u,d,ZV,m,*a,*d")
	(match_operand:QI 1 "move_operand"         "d,J,I,ZW,m,!kbJ,dJ,*d*J,*a"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || reg_or_0_operand (operands[1], QImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,load,store,store,mtlo,mflo")
   (set_attr "compression" "all,micromips,*,micromips,*,micromips,*,*,*")
   (set_attr "mode" "QI")])

(define_insn "*movqi_mips16"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:QI 1 "move_operand"         "d,d,y,K,N,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mflo")
   (set_attr "mode" "QI")])

;; On the mips16, we can split lb $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:QI 0 "d_operand")
	(mem:QI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand"))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32
	   && INTVAL (operands[1]) <= 31 + 0x7f))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:QI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = const0_rtx;
  else
    {
      operands[1] = GEN_INT (0x7f);
      operands[2] = GEN_INT (val - 0x7f);
    }
})

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "")
	(match_operand:SF 1 ""))]
  ""
{
  if (mips_legitimize_move (SFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "movccf"
  [(set (match_operand:CCF 0 "nonimmediate_operand" "=f,f,m")
	(match_operand:CCF 1 "nonimmediate_operand" "f,m,f"))]
  "ISA_HAS_CCF"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,fpload,fpstore")])

(define_insn "*movsf_hardfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*d,*d,*d,*m")
	(match_operand:SF 1 "move_operand" "f,G,m,f,G,*d,*f,*G*d,*m,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,m")
	(match_operand:SF 1 "move_operand" "Gd,m,d"))]
  "TARGET_SOFT_FLOAT && !TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_mips16"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,y,d,d,m")
	(match_operand:SF 1 "move_operand" "d,d,y,m,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,load,store")
   (set_attr "mode" "SF")])

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "")
	(match_operand:DF 1 ""))]
  ""
{
  if (mips_legitimize_move (DFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movdf_hardfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*d,*d,*d,*m")
	(match_operand:DF 1 "move_operand" "f,G,m,f,G,*d,*f,*d*G,*m,*d"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,m")
	(match_operand:DF 1 "move_operand" "dG,m,dG"))]
  "(TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_mips16"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,y,d,d,m")
	(match_operand:DF 1 "move_operand" "d,d,y,m,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,load,store")
   (set_attr "mode" "DF")])

;; 128-bit integer moves

(define_expand "movti"
  [(set (match_operand:TI 0)
	(match_operand:TI 1))]
  "TARGET_64BIT"
{
  if (mips_legitimize_move (TImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=d,d,d,m,*a,*a,*d")
	(match_operand:TI 1 "move_operand" "d,i,m,dJ,*J,*d,*a"))]
  "TARGET_64BIT
   && !TARGET_MIPS16
   && (register_operand (operands[0], TImode)
       || reg_or_0_operand (operands[1], TImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,imul,mtlo,mflo")
   (set (attr "mode")
   	(if_then_else (eq_attr "move_type" "imul")
		      (const_string "SI")
		      (const_string "TI")))])

(define_insn "*movti_mips16"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:TI 1 "move_operand" "d,d,y,K,N,m,d,*a"))]
  "TARGET_64BIT
   && TARGET_MIPS16
   && (register_operand (operands[0], TImode)
       || register_operand (operands[1], TImode))"
  "#"
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mflo")
   (set_attr "mode" "TI")])

;; 128-bit floating point moves

(define_expand "movtf"
  [(set (match_operand:TF 0)
	(match_operand:TF 1))]
  "TARGET_64BIT"
{
  if (mips_legitimize_move (TFmode, operands[0], operands[1]))
    DONE;
})

;; This pattern handles both hard- and soft-float cases.
(define_insn "*movtf"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=d,d,m,f,d,f,m")
	(match_operand:TF 1 "move_operand" "dG,m,dG,dG,f,m,f"))]
  "TARGET_64BIT
   && !TARGET_MIPS16
   && (register_operand (operands[0], TFmode)
       || reg_or_0_operand (operands[1], TFmode))"
  "#"
  [(set_attr "move_type" "move,load,store,mtc,mfc,fpload,fpstore")
   (set_attr "mode" "TF")])

(define_insn "*movtf_mips16"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=d,y,d,d,m")
	(match_operand:TF 1 "move_operand" "d,d,y,m,d"))]
  "TARGET_64BIT
   && TARGET_MIPS16
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "#"
  [(set_attr "move_type" "move,move,move,load,store")
   (set_attr "mode" "TF")])

(define_split
  [(set (match_operand:MOVE64 0 "nonimmediate_operand")
	(match_operand:MOVE64 1 "move_operand"))]
  "reload_completed && mips_split_move_insn_p (operands[0], operands[1], insn)"
  [(const_int 0)]
{
  mips_split_move_insn (operands[0], operands[1], curr_insn);
  DONE;
})

(define_split
  [(set (match_operand:MOVE128 0 "nonimmediate_operand")
	(match_operand:MOVE128 1 "move_operand"))]
  "reload_completed && mips_split_move_insn_p (operands[0], operands[1], insn)"
  [(const_int 0)]
{
  mips_split_move_insn (operands[0], operands[1], curr_insn);
  DONE;
})

;; When generating mips16 code, split moves of negative constants into
;; a positive "li" followed by a negation.
(define_split
  [(set (match_operand 0 "d_operand")
	(match_operand 1 "const_int_operand"))]
  "TARGET_MIPS16 && reload_completed && INTVAL (operands[1]) < 0"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 2)
	(neg:SI (match_dup 2)))]
{
  operands[2] = gen_lowpart (SImode, operands[0]);
  operands[3] = GEN_INT (-INTVAL (operands[1]));
})

;; 64-bit paired-single floating point moves

(define_expand "movv2sf"
  [(set (match_operand:V2SF 0)
	(match_operand:V2SF 1))]
  "TARGET_HARD_FLOAT && TARGET_PAIRED_SINGLE_FLOAT"
{
  if (mips_legitimize_move (V2SFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movv2sf"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*d,*d,*d,*m")
	(match_operand:V2SF 1 "move_operand" "f,YG,m,f,YG,*d,*f,*d*YG,*m,*d"))]
  "TARGET_HARD_FLOAT
   && TARGET_PAIRED_SINGLE_FLOAT
   && (register_operand (operands[0], V2SFmode)
       || reg_or_0_operand (operands[1], V2SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

;; Extract the high part of a HI/LO value.  See mips_hard_regno_mode_ok_p
;; for the reason why we can't just use (reg:GPR HI_REGNUM).
;;
;; When generating VR4120 or VR4130 code, we use MACCHI and DMACCHI
;; instead of MFHI.  This avoids both the normal MIPS III hi/lo hazards
;; and the errata related to -mfix-vr4130.
(define_insn "mfhi<GPR:mode>_<HILO:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(unspec:GPR [(match_operand:HILO 1 "hilo_operand" "x")]
		    UNSPEC_MFHI))]
  ""
  { return ISA_HAS_MACCHI ? "<GPR:d>macchi\t%0,%.,%." : "mfhi\t%0"; }
  [(set_attr "type" "mfhi")
   (set_attr "mode" "<GPR:MODE>")])

;; Set the high part of a HI/LO value, given that the low part has
;; already been set.  See mips_hard_regno_mode_ok_p for the reason
;; why we can't just use (reg:GPR HI_REGNUM).
(define_insn "mthi<GPR:mode>_<HILO:mode>"
  [(set (match_operand:HILO 0 "register_operand" "=x")
	(unspec:HILO [(match_operand:GPR 1 "reg_or_0_operand" "dJ")
		      (match_operand:GPR 2 "register_operand" "l")]
		     UNSPEC_MTHI))]
  ""
  "mthi\t%z1"
  [(set_attr "type" "mthi")
   (set_attr "mode" "SI")])

;; Emit a doubleword move in which exactly one of the operands is
;; a floating-point register.  We can't just emit two normal moves
;; because of the constraints imposed by the FPU register model;
;; see mips_cannot_change_mode_class for details.  Instead, we keep
;; the FPR whole and use special patterns to refer to each word of
;; the other operand.

(define_expand "move_doubleword_fpr<mode>"
  [(set (match_operand:SPLITF 0)
	(match_operand:SPLITF 1))]
  ""
{
  if (FP_REG_RTX_P (operands[0]))
    {
      rtx low = mips_subword (operands[1], 0);
      rtx high = mips_subword (operands[1], 1);
      emit_insn (gen_load_low<mode> (operands[0], low));
      if (ISA_HAS_MXHC1 && !TARGET_64BIT)
      	emit_insn (gen_mthc1<mode> (operands[0], high, operands[0]));
      else
	emit_insn (gen_load_high<mode> (operands[0], high, operands[0]));
    }
  else
    {
      rtx low = mips_subword (operands[0], 0);
      rtx high = mips_subword (operands[0], 1);
      emit_insn (gen_store_word<mode> (low, operands[1], const0_rtx));
      if (ISA_HAS_MXHC1 && !TARGET_64BIT)
	emit_insn (gen_mfhc1<mode> (high, operands[1]));
      else
	emit_insn (gen_store_word<mode> (high, operands[1], const1_rtx));
    }
  DONE;
})

;; Load the low word of operand 0 with operand 1.
(define_insn "load_low<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "dJ,m")]
		       UNSPEC_LOAD_LOW))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = mips_subword (operands[0], 0);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mtc,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Load the high word of operand 0 from operand 1, preserving the value
;; in the low word.
(define_insn "load_high<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "dJ,m")
			(match_operand:SPLITF 2 "register_operand" "0,0")]
		       UNSPEC_LOAD_HIGH))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = mips_subword (operands[0], 1);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mtc,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Store one word of operand 1 in operand 0.  Operand 2 is 1 to store the
;; high word and 0 to store the low word.
(define_insn "store_word<mode>"
  [(set (match_operand:<HALFMODE> 0 "nonimmediate_operand" "=d,m")
	(unspec:<HALFMODE> [(match_operand:SPLITF 1 "register_operand" "f,f")
			    (match_operand 2 "const_int_operand")]
			   UNSPEC_STORE_WORD))]
  "TARGET_HARD_FLOAT"
{
  operands[1] = mips_subword (operands[1], INTVAL (operands[2]));
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mfc,fpstore")
   (set_attr "mode" "<HALFMODE>")])

;; Move operand 1 to the high word of operand 0 using mthc1, preserving the
;; value in the low word.
(define_insn "mthc1<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "reg_or_0_operand" "dJ")
		        (match_operand:SPLITF 2 "register_operand" "0")]
		       UNSPEC_MTHC1))]
  "TARGET_HARD_FLOAT && ISA_HAS_MXHC1"
  "mthc1\t%z1,%0"
  [(set_attr "move_type" "mtc")
   (set_attr "mode" "<HALFMODE>")])

;; Move high word of operand 1 to operand 0 using mfhc1.
(define_insn "mfhc1<mode>"
  [(set (match_operand:<HALFMODE> 0 "register_operand" "=d")
	(unspec:<HALFMODE> [(match_operand:SPLITF 1 "register_operand" "f")]
			    UNSPEC_MFHC1))]
  "TARGET_HARD_FLOAT && ISA_HAS_MXHC1"
  "mfhc1\t%0,%1"
  [(set_attr "move_type" "mfc")
   (set_attr "mode" "<HALFMODE>")])

;; Move a constant that satisfies CONST_GP_P into operand 0.
(define_expand "load_const_gp_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(const:P (unspec:P [(const_int 0)] UNSPEC_GP)))])

;; Insn to initialize $gp for n32/n64 abicalls.  Operand 0 is the offset
;; of _gp from the start of this function.  Operand 1 is the incoming
;; function address.
(define_insn_and_split "loadgp_newabi_<mode>"
  [(set (match_operand:P 0 "register_operand" "=&d")
	(unspec:P [(match_operand:P 1)
		   (match_operand:P 2 "register_operand" "d")]
		  UNSPEC_LOADGP))]
  "mips_current_loadgp_style () == LOADGP_NEWABI"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0) (match_dup 4))
   (set (match_dup 0) (match_dup 5))]
{
  operands[3] = gen_rtx_HIGH (Pmode, operands[1]);
  operands[4] = gen_rtx_PLUS (Pmode, operands[0], operands[2]);
  operands[5] = gen_rtx_LO_SUM (Pmode, operands[0], operands[1]);
}
  [(set_attr "type" "ghost")])

;; Likewise, for -mno-shared code.  Operand 0 is the __gnu_local_gp symbol.
(define_insn_and_split "loadgp_absolute_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1)] UNSPEC_LOADGP))]
  "mips_current_loadgp_style () == LOADGP_ABSOLUTE"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(const_int 0)]
{
  mips_emit_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "ghost")])

;; This blockage instruction prevents the gp load from being
;; scheduled after an implicit use of gp.  It also prevents
;; the load from being deleted as dead.
(define_insn "loadgp_blockage"
  [(unspec_volatile [(reg:SI 28)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")])

;; Initialize $gp for RTP PIC.  Operand 0 is the __GOTT_BASE__ symbol
;; and operand 1 is the __GOTT_INDEX__ symbol.
(define_insn_and_split "loadgp_rtp_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "symbol_ref_operand")
		   (match_operand:P 2 "symbol_ref_operand")]
		  UNSPEC_LOADGP))]
  "mips_current_loadgp_style () == LOADGP_RTP"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(set (match_dup 0) (high:P (match_dup 3)))
   (set (match_dup 0) (unspec:P [(match_dup 0)
				 (match_dup 3)] UNSPEC_LOAD_GOT))
   (set (match_dup 0) (unspec:P [(match_dup 0)
				 (match_dup 4)] UNSPEC_LOAD_GOT))]
{
  operands[3] = mips_unspec_address (operands[1], SYMBOL_ABSOLUTE);
  operands[4] = mips_unspec_address (operands[2], SYMBOL_HALF);
}
  [(set_attr "type" "ghost")])

;; Initialize the global pointer for MIPS16 code.  Operand 0 is the
;; global pointer and operand 1 is the MIPS16 register that holds
;; the required value.
(define_insn_and_split "copygp_mips16_<mode>"
  [(set (match_operand:P 0 "register_operand" "=y")
	(unspec:P [(match_operand:P 1 "register_operand" "d")]
		  UNSPEC_COPYGP))]
  "TARGET_MIPS16"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(set (match_dup 0) (match_dup 1))]
  ""
  [(set_attr "type" "ghost")])

;; A placeholder for where the cprestore instruction should go,
;; if we decide we need one.  Operand 0 and operand 1 are as for
;; "cprestore".  Operand 2 is a register that holds the gp value.
;;
;; The "cprestore" pattern requires operand 2 to be pic_offset_table_rtx,
;; otherwise any register that holds the correct value will do.
(define_insn_and_split "potential_cprestore_<mode>"
  [(set (match_operand:P 0 "cprestore_save_slot_operand" "=X,X")
	(unspec:P [(match_operand:P 1 "const_int_operand" "I,i")
		   (match_operand:P 2 "register_operand" "d,d")]
		  UNSPEC_POTENTIAL_CPRESTORE))
   (clobber (match_operand:P 3 "scratch_operand" "=X,&d"))]
  "!TARGET_CPRESTORE_DIRECTIVE || operands[2] == pic_offset_table_rtx"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "mips_must_initialize_gp_p ()"
  [(const_int 0)]
{
  mips_save_gp_to_cprestore_slot (operands[0], operands[1],
				  operands[2], operands[3]);
  DONE;
}
  [(set_attr "type" "ghost")])

;; Emit a .cprestore directive, which normally expands to a single store
;; instruction.  Operand 0 is a (possibly illegitimate) sp-based MEM
;; for the cprestore slot.  Operand 1 is the offset of the slot from
;; the stack pointer.  (This is redundant with operand 0, but it makes
;; things a little simpler.)
(define_insn "cprestore_<mode>"
  [(set (match_operand:P 0 "cprestore_save_slot_operand" "=X,X")
	(unspec:P [(match_operand:P 1 "const_int_operand" "I,i")
		   (reg:P 28)]
		  UNSPEC_CPRESTORE))]
  "TARGET_CPRESTORE_DIRECTIVE"
{
  if (mips_nomacro.nesting_level > 0 && which_alternative == 1)
    return ".set\tmacro\;.cprestore\t%1\;.set\tnomacro";
  else
    return ".cprestore\t%1";
}
  [(set_attr "type" "store")
   (set_attr "insn_count" "1,3")])

(define_insn "use_cprestore_<mode>"
  [(set (reg:P CPRESTORE_SLOT_REGNUM)
	(match_operand:P 0 "cprestore_load_slot_operand"))]
  ""
  ""
  [(set_attr "type" "ghost")])

;; Expand in-line code to clear the instruction cache between operand[0] and
;; operand[1].
(define_expand "clear_cache"
  [(match_operand 0 "pmode_register_operand")
   (match_operand 1 "pmode_register_operand")]
  ""
  "
{
  if (TARGET_SYNCI)
    {
      mips_expand_synci_loop (operands[0], operands[1]);
      emit_insn (gen_sync ());
      emit_insn (PMODE_INSN (gen_clear_hazard, ()));
    }
  else if (mips_cache_flush_func && mips_cache_flush_func[0])
    {
      rtx len = gen_reg_rtx (Pmode);
      emit_insn (gen_sub3_insn (len, operands[1], operands[0]));
      MIPS_ICACHE_SYNC (operands[0], len);
    }
  DONE;
}")

(define_insn "sync"
  [(unspec_volatile [(const_int 0)] UNSPEC_SYNC)]
  "GENERATE_SYNC"
  { return mips_output_sync (); })

(define_insn "synci"
  [(unspec_volatile [(match_operand 0 "pmode_register_operand" "d")]
		    UNSPEC_SYNCI)]
  "TARGET_SYNCI"
  "synci\t0(%0)")

(define_insn "rdhwr_synci_step_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
        (unspec_volatile:P [(const_int 1)]
        UNSPEC_RDHWR))]
  "ISA_HAS_SYNCI"
  "rdhwr\t%0,$1")

(define_insn "clear_hazard_<mode>"
  [(unspec_volatile [(const_int 0)] UNSPEC_CLEAR_HAZARD)
   (clobber (reg:P RETURN_ADDR_REGNUM))]
  "ISA_HAS_SYNCI"
{
  return "%(%<bal\t1f\n"
         "\tnop\n"
         "1:\t<d>addiu\t$31,$31,12\n"
         "\tjr.hb\t$31\n"
         "\tnop%>%)";
}
  [(set_attr "insn_count" "5")])

;; Cache operations for R4000-style caches.
(define_insn "mips_cache"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "const_int_operand")
		     (match_operand:QI 1 "address_operand" "ZD")]
		    UNSPEC_MIPS_CACHE))]
  "ISA_HAS_CACHE"
  "cache\t%X0,%a1"
  [(set_attr "extended_mips16" "yes")])

;; Similar, but with the operands hard-coded to an R10K cache barrier
;; operation.  We keep the pattern distinct so that we can identify
;; cache operations inserted by -mr10k-cache-barrier=, and so that
;; the operation is never inserted into a delay slot.
(define_insn "r10k_cache_barrier"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(const_int 0)] UNSPEC_R10K_CACHE_BARRIER))]
  "ISA_HAS_CACHE"
  "cache\t0x14,0(%$)"
  [(set_attr "can_delay" "no")])

;; Block moves, see mips.cc for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "cpymemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  "!TARGET_MIPS16 && !TARGET_MEMCPY"
{
  if (mips_expand_block_move (operands[0], operands[1], operands[2]))
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

(define_expand "<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(any_shift:GPR (match_operand:GPR 1 "register_operand")
		       (match_operand:SI 2 "arith_operand")))]
  ""
{
  /* On the mips16, a shift of more than 8 is a four byte instruction,
     so, for a shift between 8 and 16, it is just as fast to do two
     shifts of 8 or less.  If there is a lot of shifting going on, we
     may win in CSE.  Otherwise combine will put the shifts back
     together again.  This can be called by mips_function_arg, so we must
     be careful not to allocate a new register if we've reached the
     reload pass.  */
  if (TARGET_MIPS16
      && optimize
      && CONST_INT_P (operands[2])
      && INTVAL (operands[2]) > 8
      && INTVAL (operands[2]) <= 16
      && !reload_in_progress
      && !reload_completed)
    {
      rtx temp = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_<optab><mode>3 (temp, operands[1], GEN_INT (8)));
      emit_insn (gen_<optab><mode>3 (operands[0], temp,
				     GEN_INT (INTVAL (operands[2]) - 8)));
      DONE;
    }
})

(define_insn "*<optab><mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=!u,d")
	(any_shift:GPR (match_operand:GPR 1 "register_operand" "!u,d")
		       (match_operand:SI 2 "arith_operand" "Uib3,dI")))]
  "!TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (<MODE>mode) - 1));

  return "<d><insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "compression" "<shift_compression>,none")
   (set_attr "mode" "<MODE>")])

(define_insn "*<optab>si3_extend"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	   (any_shift:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "<insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "*<optab>si3_mips16"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(any_shift:SI (match_operand:SI 1 "register_operand" "0,d,d")
		      (match_operand:SI 2 "arith_operand" "d,Uib3,I")))]
  "TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "<insn>\t%0,%2";

  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "<insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "no,no,yes")])

(define_insn "<GPR:d>lsa"
 [(set (match_operand:GPR 0 "register_operand" "=d")
       (plus:GPR (ashift:GPR (match_operand:GPR 1 "register_operand" "d")
			     (match_operand 2 "const_immlsa_operand" ""))
		(match_operand:GPR 3 "register_operand" "d")))]
 "ISA_HAS_<GPR:D>LSA"
 "<GPR:d>lsa\t%0,%1,%3,%2"
 [(set_attr "type" "arith")
  (set_attr "mode" "<GPR:MODE>")])

;; We need separate DImode MIPS16 patterns because of the irregularity
;; of right shifts.
(define_insn "*ashldi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0,d,d")
		   (match_operand:SI 2 "arith_operand" "d,Uib3,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "dsll\t%0,%2";

  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);
  return "dsll\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr "extended_mips16" "no,no,yes")])

(define_insn "*ashrdi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "0,0,0")
		     (match_operand:SI 2 "arith_operand" "d,Uib3,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr "extended_mips16" "no,no,yes")])

(define_insn "*lshrdi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0,0,0")
		     (match_operand:SI 2 "arith_operand" "d,Uib3,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr "extended_mips16" "no,no,yes")])

;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:GPR 0 "d_operand")
	(any_shift:GPR (match_operand:GPR 1 "d_operand")
		       (match_operand:GPR 2 "const_int_operand")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (any_shift:GPR (match_dup 1) (const_int 8)))
   (set (match_dup 0) (any_shift:GPR (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

;; If we load a byte on the mips16 as a bitfield, the resulting
;; sequence of instructions is too complicated for combine, because it
;; involves four instructions: a load, a shift, a constant load into a
;; register, and an and (the key problem here is that the mips16 does
;; not have and immediate).  We recognize a shift of a load in order
;; to make it simple enough for combine to understand.
;;
;; The instruction count here is the worst case.
(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (match_operand:SI 2 "immediate_operand" "I")))]
  "TARGET_MIPS16"
  "#"
  "&& 1"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set (attr "insn_count")
	(symbol_ref "mips_load_store_insns (operands[1], insn) + 2"))])

(define_insn "rotr<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "d")
		      (match_operand:SI 2 "arith_operand" "dI")))]
  "ISA_HAS_ROR"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2])
                           & (GET_MODE_BITSIZE (<MODE>mode) - 1));

  return "<d>ror\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(bswap:HI (match_operand:HI 1 "register_operand" "d")))]
  "ISA_HAS_WSBH"
  "wsbh\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn_and_split "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(bswap:SI (match_operand:SI 1 "register_operand" "d")))]
  "ISA_HAS_WSBH && ISA_HAS_ROR"
  "#"
  "&& 1"
  [(set (match_dup 0) (unspec:SI [(match_dup 1)] UNSPEC_WSBH))
   (set (match_dup 0) (rotatert:SI (match_dup 0) (const_int 16)))]
  ""
  [(set_attr "insn_count" "2")])

(define_insn_and_split "bswapdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(bswap:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT && ISA_HAS_WSBH"
  "#"
  "&& 1"
  [(set (match_dup 0) (unspec:DI [(match_dup 1)] UNSPEC_DSBH))
   (set (match_dup 0) (unspec:DI [(match_dup 0)] UNSPEC_DSHD))]
  ""
  [(set_attr "insn_count" "2")])

(define_insn "wsbh"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "register_operand" "d")] UNSPEC_WSBH))]
  "ISA_HAS_WSBH"
  "wsbh\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "dsbh"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unspec:DI [(match_operand:DI 1 "register_operand" "d")] UNSPEC_DSBH))]
  "TARGET_64BIT && ISA_HAS_WSBH"
  "dsbh\t%0,%1"
  [(set_attr "type" "shift")])

(define_insn "dshd"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unspec:DI [(match_operand:DI 1 "register_operand" "d")] UNSPEC_DSHD))]
  "TARGET_64BIT && ISA_HAS_WSBH"
  "dshd\t%0,%1"
  [(set_attr "type" "shift")])

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches on floating-point equality tests.

(define_insn "*branch_fp_<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 1 "equality_operator"
                         [(match_operand:FPCC 2 "register_operand" "<reg>")
			  (const_int 0)])
         (label_ref (match_operand 0 "" ""))
         (pc)))]
  "TARGET_HARD_FLOAT"
{
  return mips_output_conditional_branch (insn, operands,
					 MIPS_BRANCH ("b%F1", "%Z2%0"),
					 MIPS_BRANCH ("b%W1", "%Z2%0"));
}
  [(set_attr "type" "branch")])

(define_insn "*branch_fp_inverted_<mode>"
  [(set (pc)
        (if_then_else
         (match_operator 1 "equality_operator"
                         [(match_operand:FPCC 2 "register_operand" "<reg>")
			  (const_int 0)])
         (pc)
         (label_ref (match_operand 0 "" ""))))]
  "TARGET_HARD_FLOAT"
{
  return mips_output_conditional_branch (insn, operands,
					 MIPS_BRANCH ("b%W1", "%Z2%0"),
					 MIPS_BRANCH ("b%F1", "%Z2%0"));
}
  [(set_attr "type" "branch")])

;; Conditional branches on ordered comparisons with zero.

(define_insn "*branch_order<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "d,d")
			  (match_operand:GPR 3 "reg_or_0_operand" "J,d")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "!TARGET_MIPS16"
  { return mips_output_order_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe,always")
   (set_attr "hazard" "forbidden_slot")])

(define_insn "*branch_order<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "d,d")
			  (match_operand:GPR 3 "reg_or_0_operand" "J,d")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  "!TARGET_MIPS16"
  { return mips_output_order_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe,always")
   (set_attr "hazard" "forbidden_slot")])

;; Conditional branch on equality comparison.

(define_insn "*branch_equality<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (match_operand:GPR 3 "reg_or_0_operand" "dJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "!TARGET_MIPS16"
  { return mips_output_equal_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe")
   (set_attr "hazard" "forbidden_slot")])

(define_insn "*branch_equality<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (match_operand:GPR 3 "reg_or_0_operand" "dJ")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  "!TARGET_MIPS16"
  { return mips_output_equal_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe")
   (set_attr "hazard" "forbidden_slot")])

;; MIPS16 branches

(define_insn "*branch_equality<mode>_mips16"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "d,t")
			  (const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "TARGET_MIPS16"
  "@
   b%C1z\t%2,%0
   bt%C1z\t%0"
  [(set_attr "type" "branch")])

(define_insn "*branch_equality<mode>_mips16_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "d,t")
			  (const_int 0)])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  "TARGET_MIPS16"
  "@
   b%N1z\t%2,%0
   bt%N1z\t%0"
  [(set_attr "type" "branch")])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:GPR 1 "register_operand")
		        (match_operand:GPR 2 "nonmemory_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  mips_expand_conditional_branch (operands);
  DONE;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:SCALARF 1 "register_operand")
		        (match_operand:SCALARF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  mips_expand_conditional_branch (operands);
  DONE;
})

;; Used to implement built-in functions.
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])

;; Branch if bit is set/clear.

(define_insn "*branch_bit<bbv><mode>"
  [(set (pc)
	(if_then_else
	 (equality_op (zero_extract:GPR
		       (match_operand:GPR 1 "register_operand" "d")
		       (const_int 1)
		       (match_operand 2 "const_int_operand" ""))
		      (const_int 0))
	 (label_ref (match_operand 0 ""))
	 (pc)))]
  "ISA_HAS_BBIT && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
{
  return
    mips_output_conditional_branch (insn, operands,
				    MIPS_BRANCH ("bbit<bbv>", "%1,%2,%0"),
				    MIPS_BRANCH ("bbit<bbinv>", "%1,%2,%0"));
}
  [(set_attr "type"	     "branch")
   (set_attr "branch_likely" "no")])

(define_insn "*branch_bit<bbv><mode>_inverted"
  [(set (pc)
	(if_then_else
	 (equality_op (zero_extract:GPR
		       (match_operand:GPR 1 "register_operand" "d")
		       (const_int 1)
		       (match_operand 2 "const_int_operand" ""))
		      (const_int 0))
	 (pc)
	 (label_ref (match_operand 0 ""))))]
  "ISA_HAS_BBIT && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
{
  return
    mips_output_conditional_branch (insn, operands,
				    MIPS_BRANCH ("bbit<bbinv>", "%1,%2,%0"),
				    MIPS_BRANCH ("bbit<bbv>", "%1,%2,%0"));
}
  [(set_attr "type"	     "branch")
   (set_attr "branch_likely" "no")])

;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

;; Destination is always set in SI mode.

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "mips_cstore_operator"
	 [(match_operand:GPR 2 "register_operand")
	  (match_operand:GPR 3 "nonmemory_operand")]))]
  ""
{
  mips_expand_scc (operands);
  DONE;
})

(define_insn "*seq_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(eq:GPR2 (match_operand:GPR 1 "register_operand" "d")
		 (const_int 0)))]
  "!TARGET_MIPS16 && !ISA_HAS_SEQ_SNE"
  "sltu\t%0,%1,1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*seq_zero_<GPR:mode><GPR2:mode>_mips16"
  [(set (match_operand:GPR2 0 "register_operand" "=t")
	(eq:GPR2 (match_operand:GPR 1 "register_operand" "d")
		 (const_int 0)))]
  "TARGET_MIPS16 && !ISA_HAS_SEQ_SNE"
  "sltu\t%1,1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

;; Generate sltiu unless using seq results in better code.
(define_insn "*seq_<GPR:mode><GPR2:mode>_seq"
  [(set (match_operand:GPR2 0 "register_operand" "=d,d,d")
	(eq:GPR2 (match_operand:GPR 1 "register_operand" "%d,d,d")
		 (match_operand:GPR 2 "reg_imm10_operand" "d,J,YB")))]
  "ISA_HAS_SEQ_SNE"
  "@
   seq\t%0,%1,%2
   sltiu\t%0,%1,1
   seqi\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sne_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(ne:GPR2 (match_operand:GPR 1 "register_operand" "d")
		 (const_int 0)))]
  "!TARGET_MIPS16 && !ISA_HAS_SEQ_SNE"
  "sltu\t%0,%.,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

;; Generate sltu unless using sne results in better code.
(define_insn "*sne_<GPR:mode><GPR2:mode>_sne"
  [(set (match_operand:GPR2 0 "register_operand" "=d,d,d")
	(ne:GPR2 (match_operand:GPR 1 "register_operand" "%d,d,d")
		 (match_operand:GPR 2 "reg_imm10_operand" "d,J,YB")))]
  "ISA_HAS_SEQ_SNE"
  "@
   sne\t%0,%1,%2
   sltu\t%0,%.,%1
   snei\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sgt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_gt:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "reg_or_0_operand" "dJ")))]
  "!TARGET_MIPS16"
  "slt<u>\t%0,%z2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sgt<u>_<GPR:mode><GPR2:mode>_mips16"
  [(set (match_operand:GPR2 0 "register_operand" "=t")
	(any_gt:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "slt<u>\t%2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sge<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_ge:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (const_int 1)))]
  "!TARGET_MIPS16"
  "slt<u>\t%0,%.,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*slt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_lt:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "slt<u>\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*slt<u>_<GPR:mode><GPR2:mode>_mips16"
  [(set (match_operand:GPR2 0 "register_operand" "=t,t,t")
	(any_lt:GPR2 (match_operand:GPR 1 "register_operand" "d,d,d")
		     (match_operand:GPR 2 "arith_operand" "d,Uub8,I")))]
  "TARGET_MIPS16"
  "slt<u>\t%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")
   (set_attr "extended_mips16" "no,no,yes")])

(define_insn "*sle<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_le:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "sle_operand" "")))]
  "!TARGET_MIPS16"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>\t%0,%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sle<u>_<GPR:mode><GPR2:mode>_mips16"
  [(set (match_operand:GPR2 0 "register_operand" "=t,t")
	(any_le:GPR2 (match_operand:GPR 1 "register_operand" "d,d")
		     (match_operand:GPR 2 "sle_operand" "Udb8,i")))]
  "TARGET_MIPS16"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>\t%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")
   (set_attr "extended_mips16" "no,yes")])

;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................

(define_insn "s<code>_<SCALARF:mode>_using_<FPCC:mode>"
  [(set (match_operand:FPCC 0 "register_operand" "=<reg>")
	(fcond:FPCC (match_operand:SCALARF 1 "register_operand" "f")
		    (match_operand:SCALARF 2 "register_operand" "f")))]
  ""
  {
    return mips_output_compare ("<fpcmp>", "<fcond>", "<fmt>", "<FPCC:mode>", false);
  }
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "s<code>_<SCALARF:mode>_using_<FPCC:mode>"
  [(set (match_operand:FPCC 0 "register_operand" "=<reg>")
	(swapped_fcond:FPCC (match_operand:SCALARF 1 "register_operand" "f")
			    (match_operand:SCALARF 2 "register_operand" "f")))]
  ""
  {
    return mips_output_compare ("<fpcmp>", "<swapped_fcond>", "<fmt>", "<FPCC:mode>", true);
  }
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

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
  "!TARGET_MIPS16 && TARGET_ABSOLUTE_JUMPS"
{
  if (get_attr_length (insn) <= 8)
    {
      if (TARGET_CB_MAYBE)
	return MIPS_ABSOLUTE_JUMP ("%*b%:\t%l0");
      else
	return MIPS_ABSOLUTE_JUMP ("%*b\t%l0%/");
    }
  else
    {
      if (TARGET_CB_MAYBE && !final_sequence)
	return MIPS_ABSOLUTE_JUMP ("%*bc\t%l0");
      else
	return MIPS_ABSOLUTE_JUMP ("%*j\t%l0%/");
    }
}
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe")])

(define_insn "*jump_pic"
  [(set (pc)
	(label_ref (match_operand 0)))]
  "!TARGET_MIPS16 && !TARGET_ABSOLUTE_JUMPS"
{
  if (get_attr_length (insn) <= 8)
    {
      if (TARGET_CB_MAYBE)
	return "%*b%:\t%l0";
      else
	return "%*b\t%l0%/";
    }
  else
    {
      mips_output_load_label (operands[0]);
      if (TARGET_CB_MAYBE)
	return "%*jr%:\t%@%]";
      else
	return "%*jr\t%@%/%]";
    }
}
  [(set_attr "type" "branch")
   (set_attr "compact_form" "maybe")])

;; We need a different insn for the mips16, because a mips16 branch
;; does not have a delay slot.

(define_insn "*jump_mips16"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_MIPS16"
  "b\t%l0"
  [(set_attr "type" "branch")
   (set (attr "length")
	;; This calculation is like the normal branch one, but the
	;; range of the unextended instruction is [-0x800, 0x7fe] rather
	;; than [-0x100, 0xfe].  This translates to a range of:
	;;
	;;    [-(0x800 - sizeof (branch)), 0x7fe]
	;; == [-0x7fe, 0x7fe]
	;;
	;; from the shorten_branches reference address.  Long-branch
	;; sequences will replace this one, so the minimum length
	;; is one instruction shorter than for conditional branches.
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 2046))
		    (le (minus (pc) (match_dup 0)) (const_int 2046)))
	       (const_int 2)
	       (and (le (minus (match_dup 0) (pc)) (const_int 65534))
		    (le (minus (pc) (match_dup 0)) (const_int 65532)))
	       (const_int 4)
	       (and (match_test "TARGET_ABICALLS")
		    (not (match_test "TARGET_ABSOLUTE_ABICALLS")))
	       (const_int 18)
	       (match_test "Pmode == SImode")
	       (const_int 14)
	       ] (const_int 22)))])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  operands[0] = force_reg (Pmode, operands[0]);
  emit_jump_insn (PMODE_INSN (gen_indirect_jump, (operands[0])));
  DONE;
})

(define_insn "indirect_jump_<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "d"))]
  ""
  {
    return mips_output_jump (operands, 0, -1, false);
  }
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; A combined jump-and-move instruction, used for MIPS16 long-branch
;; sequences.  Having a dedicated pattern is more convenient than
;; creating a SEQUENCE for this special case.
(define_insn "indirect_jump_and_restore_<mode>"
  [(set (pc) (match_operand:P 1 "register_operand" "d"))
   (set (match_operand:P 0 "register_operand" "=d")
   	(match_operand:P 2 "register_operand" "y"))]
  ""
  "%(%<jr\t%1\;move\t%0,%2%>%)"
  [(set_attr "type" "multi")
   (set_attr "extended_mips16" "yes")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand"))
   (use (label_ref (match_operand 1 "")))]
  "!TARGET_MIPS16_SHORT_JUMP_TABLES"
{
  if (TARGET_GPWORD)
    operands[0] = expand_binop (Pmode, add_optab, operands[0],
				pic_offset_table_rtx, 0, 0, OPTAB_WIDEN);
  else if (TARGET_RTP_PIC)
    {
      /* When generating RTP PIC, we use case table entries that are relative
	 to the start of the function.  Add the function's address to the
	 value we loaded.  */
      rtx start = get_hard_reg_initial_val (Pmode, PIC_FUNCTION_ADDR_REGNUM);
      operands[0] = expand_binop (ptr_mode, add_optab, operands[0],
				  start, 0, 0, OPTAB_WIDEN);
    }

  emit_jump_insn (PMODE_INSN (gen_tablejump, (operands[0], operands[1])));
  DONE;
})

(define_insn "tablejump_<mode>"
  [(set (pc)
	(match_operand:P 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  {
    return mips_output_jump (operands, 0, -1, false);
  }
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; For MIPS16, we don't know whether a given jump table will use short or
;; word-sized offsets until late in compilation, when we are able to determine
;; the sizes of the insns which comprise the containing function.  This
;; necessitates the use of the casesi rather than the tablejump pattern, since
;; the latter tries to calculate the index of the offset to jump through early
;; in compilation, i.e. at expand time, when nothing is known about the
;; eventual function layout.

(define_expand "casesi"
  [(match_operand:SI 0 "register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand 3 "" "")			; table label
   (match_operand 4 "" "")]			; out of range label
  "TARGET_MIPS16_SHORT_JUMP_TABLES"
{
  if (operands[1] != const0_rtx)
    {
      rtx reg = gen_reg_rtx (SImode);
      rtx offset = gen_int_mode (-INTVAL (operands[1]), SImode);
      
      if (!arith_operand (offset, SImode))
        offset = force_reg (SImode, offset);
      
      emit_insn (gen_addsi3 (reg, operands[0], offset));
      operands[0] = reg;
    }

  if (!arith_operand (operands[0], SImode))
    operands[0] = force_reg (SImode, operands[0]);

  emit_cmp_and_jump_insns (operands[0], operands[2], GTU,
			   NULL_RTX, SImode, 1, operands[4]);
  emit_jump_insn (PMODE_INSN (gen_casesi_internal_mips16,
			      (operands[0], operands[3])));
  DONE;
})

(define_insn "casesi_internal_mips16_<mode>"
  [(set (pc)
	(unspec:P [(match_operand:SI 0 "register_operand" "d")
		   (label_ref (match_operand 1 "" ""))]
	 UNSPEC_CASESI_DISPATCH))
   (clobber (match_scratch:P 2 "=d"))
   (clobber (match_scratch:P 3 "=d"))]
  "TARGET_MIPS16_SHORT_JUMP_TABLES"
{
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[1])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  switch (GET_MODE (diff_vec))
    {
    case E_HImode:
      output_asm_insn ("sll\t%3,%0,1", operands);
      output_asm_insn ("<d>la\t%2,%1", operands);
      output_asm_insn ("<d>addu\t%3,%2,%3", operands);
      output_asm_insn ("lh\t%3,0(%3)", operands);
      break;

    case E_SImode:
      output_asm_insn ("sll\t%3,%0,2", operands);
      output_asm_insn ("<d>la\t%2,%1", operands);
      output_asm_insn ("<d>addu\t%3,%2,%3", operands);
      output_asm_insn ("lw\t%3,0(%3)", operands);
      break;

    default:
      gcc_unreachable ();
    }

  output_asm_insn ("<d>addu\t%2,%2,%3", operands);

  if (GENERATE_MIPS16E)
    return "jrc\t%2";
  else
    return "jr\t%2";
}
  [(set (attr "insn_count")
	(if_then_else (match_test "GENERATE_MIPS16E")
		      (const_string "6")
		      (const_string "7")))])

;; For TARGET_USE_GOT, we save the gp in the jmp_buf as well.
;; While it is possible to either pull it off the stack (in the
;; o32 case) or recalculate it given t9 and our target label,
;; it takes 3 or 4 insns to do so.

(define_expand "builtin_setjmp_setup"
  [(use (match_operand 0 "register_operand"))]
  "TARGET_USE_GOT"
{
  rtx addr;

  addr = plus_constant (Pmode, operands[0], GET_MODE_SIZE (Pmode) * 3);
  mips_emit_move (gen_rtx_MEM (Pmode, addr), pic_offset_table_rtx);
  DONE;
})

;; Restore the gp that we saved above.  Despite the earlier comment, it seems
;; that older code did recalculate the gp from $25.  Continue to jump through
;; $25 for compatibility (we lose nothing by doing so).

(define_expand "builtin_longjmp"
  [(use (match_operand 0 "register_operand"))]
  "TARGET_USE_GOT"
{
  /* The elements of the buffer are, in order:  */
  int W = GET_MODE_SIZE (Pmode);
  rtx fp = gen_rtx_MEM (Pmode, operands[0]);
  rtx lab = gen_rtx_MEM (Pmode, plus_constant (Pmode, operands[0], 1*W));
  rtx stack = gen_rtx_MEM (Pmode, plus_constant (Pmode, operands[0], 2*W));
  rtx gpv = gen_rtx_MEM (Pmode, plus_constant (Pmode, operands[0], 3*W));
  rtx pv = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
  /* Use gen_raw_REG to avoid being given pic_offset_table_rtx.
     The target is bound to be using $28 as the global pointer
     but the current function might not be.  */
  rtx gp = gen_raw_REG (Pmode, GLOBAL_POINTER_REGNUM);

  /* This bit is similar to expand_builtin_longjmp except that it
     restores $gp as well.  */
  mips_emit_move (pv, lab);
  /* Restore the frame pointer and stack pointer and gp.  We must use a
     temporary since the setjmp buffer may be a local.  */
  fp = copy_to_reg (fp);
  gpv = copy_to_reg (gpv);
  emit_stack_restore (SAVE_NONLOCAL, stack);

  /* Ensure the frame pointer move is not optimized.  */
  emit_insn (gen_blockage ());
  emit_clobber (hard_frame_pointer_rtx);
  emit_clobber (frame_pointer_rtx);
  emit_clobber (gp);
  mips_emit_move (hard_frame_pointer_rtx, fp);
  mips_emit_move (gp, gpv);
  emit_use (hard_frame_pointer_rtx);
  emit_use (stack_pointer_rtx);
  emit_use (gp);
  emit_indirect_jump (pv);
  DONE;
})

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
  mips_expand_prologue ();
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
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "0")
			    (match_operand:P 2 "register_operand" "d")]
			    UNSPEC_PROBE_STACK_RANGE))]
  ""
 { return mips_output_probe_stack_range (operands[0], operands[2]); }
  [(set_attr "type" "unknown")
   (set_attr "can_delay" "no")
   (set_attr "mode" "<MODE>")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (true);
  DONE;
})

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better.

(define_expand "return"
  [(simple_return)]
  "mips_can_use_return_insn ()"
  { mips_expand_before_return (); })

(define_expand "simple_return"
  [(simple_return)]
  ""
  { mips_expand_before_return (); })

(define_insn "*<optab>"
  [(any_return)]
  ""
  {
    operands[0] = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
    return mips_output_jump (operands, 0, -1, false);
  }
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Insn to clear execution and instruction hazards while returning.
;; However, it doesn't clear hazards created by the insn in its delay slot.
;; Thus, explicitly place a nop in its delay slot.

(define_insn "mips_hb_return_internal"
  [(return)
   (unspec_volatile [(match_operand 0 "pmode_register_operand" "")]
		    UNSPEC_JRHB)]
  ""
  {
    return "%(jr.hb\t$31%/%)";
  }
  [(set_attr "insn_count" "2")])

;; Normal return.

(define_insn "<optab>_internal"
  [(any_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  {
    return mips_output_jump (operands, 0, -1, false);
  }
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Exception return.
(define_insn "mips_eret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPEC_ERET)]
  ""
  "eret"
  [(set_attr "type"	"trap")
   (set_attr "mode"	"none")])

;; Debug exception return.
(define_insn "mips_deret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPEC_DERET)]
  ""
  "deret"
  [(set_attr "type"	"trap")
   (set_attr "mode"	"none")])

;; Disable interrupts.
(define_insn "mips_di"
  [(unspec_volatile [(const_int 0)] UNSPEC_DI)]
  ""
  "di"
  [(set_attr "type"	"trap")
   (set_attr "mode"	"none")])

;; Execution hazard barrier.
(define_insn "mips_ehb"
  [(unspec_volatile [(const_int 0)] UNSPEC_EHB)]
  ""
  "ehb"
  [(set_attr "type"	"trap")
   (set_attr "mode"	"none")])

;; Read GPR from previous shadow register set.
(define_insn "mips_rdpgpr_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "d")]
			   UNSPEC_RDPGPR))]
  ""
  "rdpgpr\t%0,%1"
  [(set_attr "type"	"move")
   (set_attr "mode"	"<MODE>")])

;; Move involving COP0 registers.
(define_insn "cop0_move"
  [(set (match_operand:SI 0 "register_operand" "=B,d")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand" "d,B")]
			    UNSPEC_COP0))]
  ""
{ return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"mtc,mfc")
   (set_attr "mode"	"SI")])

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
  [(unspec [(match_operand:SI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&d"))]
  "! TARGET_64BIT"
  "#")

(define_insn "eh_set_lr_di"
  [(unspec [(match_operand:DI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:DI 1 "=&d"))]
  "TARGET_64BIT"
  "#")

(define_split
  [(unspec [(match_operand 0 "register_operand")] UNSPEC_EH_RETURN)
   (clobber (match_scratch 1))]
  "reload_completed"
  [(const_int 0)]
{
  mips_set_return_address (operands[0], operands[1]);
  DONE;
})

(define_expand "exception_receiver"
  [(const_int 0)]
  "TARGET_USE_GOT"
{
  /* See the comment above load_call<mode> for details.  */
  emit_insn (gen_set_got_version ());

  /* If we have a call-clobbered $gp, restore it from its save slot.  */
  if (HAVE_restore_gp_si)
    emit_insn (gen_restore_gp_si ());
  else if (HAVE_restore_gp_di)
    emit_insn (gen_restore_gp_di ());
  DONE;
})

(define_expand "nonlocal_goto_receiver"
  [(const_int 0)]
  "TARGET_USE_GOT"
{
  /* See the comment above load_call<mode> for details.  */
  emit_insn (gen_set_got_version ());
  DONE;
})

;; Restore $gp from its .cprestore stack slot.  The instruction remains
;; volatile until all uses of $28 are exposed.
(define_insn_and_split "restore_gp_<mode>"
  [(set (reg:P 28)
	(unspec_volatile:P [(const_int 0)] UNSPEC_RESTORE_GP))
   (clobber (match_scratch:P 0 "=&d"))]
  "TARGET_CALL_CLOBBERED_GP"
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
{
  mips_restore_gp_from_cprestore_slot (operands[0]);
  DONE;
}
  [(set_attr "type" "ghost")])

;; Move between $gp and its register save slot.
(define_insn_and_split "move_gp<mode>"
  [(set (match_operand:GPR 0 "nonimmediate_operand" "=d,m")
  	(unspec:GPR [(match_operand:GPR 1 "move_operand" "m,d")]
		    UNSPEC_MOVE_GP))]
  ""
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "mips_must_initialize_gp_p ()"
  [(const_int 0)]
{
  mips_emit_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "ghost")])

;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

;; Instructions to load a call address from the GOT.  The address might
;; point to a function or to a lazy binding stub.  In the latter case,
;; the stub will use the dynamic linker to resolve the function, which
;; in turn will change the GOT entry to point to the function's real
;; address.
;;
;; This means that every call, even pure and constant ones, can
;; potentially modify the GOT entry.  And once a stub has been called,
;; we must not call it again.
;;
;; We represent this restriction using an imaginary, fixed, call-saved
;; register called GOT_VERSION_REGNUM.  The idea is to make the register
;; live throughout the function and to change its value after every
;; potential call site.  This stops any rtx value that uses the register
;; from being computed before an earlier call.  To do this, we:
;;
;;    - Ensure that the register is live on entry to the function,
;;	so that it is never thought to be used uninitalized.
;;
;;    - Ensure that the register is live on exit from the function,
;;	so that it is live throughout.
;;
;;    - Make each call (lazily-bound or not) use the current value
;;	of GOT_VERSION_REGNUM, so that updates of the register are
;;	not moved across call boundaries.
;;
;;    - Add "ghost" definitions of the register to the beginning of
;;	blocks reached by EH and ABNORMAL_CALL edges, because those
;;	edges may involve calls that normal paths don't.  (E.g. the
;;	unwinding code that handles a non-call exception may change
;;	lazily-bound GOT entries.)  We do this by making the
;;	exception_receiver and nonlocal_goto_receiver expanders emit
;;	a set_got_version instruction.
;;
;;    - After each call (lazily-bound or not), use a "ghost"
;;	update_got_version instruction to change the register's value.
;;	This instruction mimics the _possible_ effect of the dynamic
;;	resolver during the call and it remains live even if the call
;;	itself becomes dead.
;;
;;    - Leave GOT_VERSION_REGNUM out of all register classes.
;;	The register is therefore not a valid register_operand
;;	and cannot be moved to or from other registers.

(define_insn "load_call<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "register_operand" "d")
		   (match_operand:P 2 "immediate_operand" "")
		   (reg:SI GOT_VERSION_REGNUM)] UNSPEC_LOAD_CALL))]
  "TARGET_USE_GOT"
  "<load>\t%0,%R2(%1)"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "set_got_version"
  [(set (reg:SI GOT_VERSION_REGNUM)
	(unspec_volatile:SI [(const_int 0)] UNSPEC_SET_GOT_VERSION))]
  "TARGET_USE_GOT"
  ""
  [(set_attr "type" "ghost")])

(define_insn "update_got_version"
  [(set (reg:SI GOT_VERSION_REGNUM)
	(unspec:SI [(reg:SI GOT_VERSION_REGNUM)] UNSPEC_UPDATE_GOT_VERSION))]
  "TARGET_USE_GOT"
  ""
  [(set_attr "type" "ghost")])

;; Sibling calls.  All these patterns use jump instructions.

;; If TARGET_SIBCALLS, call_insn_operand will only accept constant
;; addresses if a direct jump is acceptable.  Since the 'S' constraint
;; is defined in terms of call_insn_operand, the same is true of the
;; constraints.

;; When we use an indirect jump, we need a register that will be
;; preserved by the epilogue.  Since TARGET_USE_PIC_FN_ADDR_REG forces
;; us to use $25 for this purpose -- and $25 is never clobbered by the
;; epilogue -- we might as well use it for !TARGET_USE_PIC_FN_ADDR_REG
;; as well.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  "TARGET_SIBCALLS"
{
  mips_expand_call (MIPS_CALL_SIBCALL, NULL_RTX, XEXP (operands[0], 0),
		    operands[1], operands[2], false);
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,S"))
	 (match_operand 1 "" ""))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  { return mips_output_jump (operands, 0, 1, false); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  "TARGET_SIBCALLS"
{
  mips_expand_call (MIPS_CALL_SIBCALL, operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], false);
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  { return mips_output_jump (operands, 1, 2, false); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  { return mips_output_jump (operands, 1, 2, false); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  mips_expand_call (MIPS_CALL_NORMAL, NULL_RTX, XEXP (operands[0], 0),
		    operands[1], operands[2], false);
  DONE;
})

;; This instruction directly corresponds to an assembly-language "jal".
;; There are four cases:
;;
;;    - -mno-abicalls:
;;	  Both symbolic and register destinations are OK.  The pattern
;;	  always expands to a single mips instruction.
;;
;;    - -mabicalls/-mno-explicit-relocs:
;;	  Again, both symbolic and register destinations are OK.
;;	  The call is treated as a multi-instruction black box.
;;
;;    - -mabicalls/-mexplicit-relocs with n32 or n64:
;;	  Only "jal $25" is allowed.  This expands to a single "jalr $25"
;;	  instruction.
;;
;;    - -mabicalls/-mexplicit-relocs with o32 or o64:
;;	  Only "jal $25" is allowed.  The call is actually two instructions:
;;	  "jalr $25" followed by an insn to reload $gp.
;;
;; In the last case, we can generate the individual instructions with
;; a define_split.  There are several things to be wary of:
;;
;;   - We can't expose the load of $gp before reload.  If we did,
;;     it might get removed as dead, but reload can introduce new
;;     uses of $gp by rematerializing constants.
;;
;;   - We shouldn't restore $gp after calls that never return.
;;     It isn't valid to insert instructions between a noreturn
;;     call and the following barrier.
;;
;;   - The splitter deliberately changes the liveness of $gp.  The unsplit
;;     instruction preserves $gp and so have no effect on its liveness.
;;     But once we generate the separate insns, it becomes obvious that
;;     $gp is not live on entry to the call.
;;
(define_insn_and_split "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c,S"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  {
    return (TARGET_SPLIT_CALLS ? "#"
	    : mips_output_jump (operands, 0, 1, true));
  }
  "reload_completed && TARGET_SPLIT_CALLS"
  [(const_int 0)]
{
  mips_split_call (curr_insn, gen_call_split (operands[0], operands[1]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_split"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c,S"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return mips_output_jump (operands, 0, 1, true); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

;; A pattern for calls that must be made directly.  It is used for
;; MIPS16 calls that the linker may need to redirect to a hard-float
;; stub; the linker relies on the call relocation type to detect when
;; such redirection is needed.
(define_insn_and_split "call_internal_direct"
  [(call (mem:SI (match_operand 0 "const_call_insn_operand"))
	 (match_operand 1))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  {
    return (TARGET_SPLIT_CALLS ? "#"
	    : mips_output_jump (operands, 0, -1, true));
  }
  "reload_completed && TARGET_SPLIT_CALLS"
  [(const_int 0)]
{
  mips_split_call (curr_insn,
		   gen_call_direct_split (operands[0], operands[1]));
  DONE;
}
  [(set_attr "jal" "direct")])

(define_insn "call_direct_split"
  [(call (mem:SI (match_operand 0 "const_call_insn_operand"))
	 (match_operand 1))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return mips_output_jump (operands, 0, -1, true); }
  [(set_attr "jal" "direct")
   (set_attr "jal_macro" "no")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  mips_expand_call (MIPS_CALL_NORMAL, operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], false);
  DONE;
})

;; See comment for call_internal.
(define_insn_and_split "call_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  {
    return (TARGET_SPLIT_CALLS ? "#"
	    : mips_output_jump (operands, 1, 2, true));
  }
  "reload_completed && TARGET_SPLIT_CALLS"
  [(const_int 0)]
{
  mips_split_call (curr_insn,
		   gen_call_value_split (operands[0], operands[1],
					 operands[2]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_value_split"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return mips_output_jump (operands, 1, 2, true); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

;; See call_internal_direct.
(define_insn_and_split "call_value_internal_direct"
  [(set (match_operand 0 "register_operand")
        (call (mem:SI (match_operand 1 "const_call_insn_operand"))
              (match_operand 2)))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  {
    return (TARGET_SPLIT_CALLS ? "#"
	    : mips_output_jump (operands, 1, -1, true));
  }
  "reload_completed && TARGET_SPLIT_CALLS"
  [(const_int 0)]
{
  mips_split_call (curr_insn,
		   gen_call_value_direct_split (operands[0], operands[1],
						operands[2]));
  DONE;
}
  [(set_attr "jal" "direct")])

(define_insn "call_value_direct_split"
  [(set (match_operand 0 "register_operand")
        (call (mem:SI (match_operand 1 "const_call_insn_operand"))
              (match_operand 2)))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return mips_output_jump (operands, 1, -1, true); }
  [(set_attr "jal" "direct")
   (set_attr "jal_macro" "no")])

;; See comment for call_internal.
(define_insn_and_split "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  {
    return (TARGET_SPLIT_CALLS ? "#"
	    : mips_output_jump (operands, 1, 2, true));
  }
  "reload_completed && TARGET_SPLIT_CALLS"
  [(const_int 0)]
{
  mips_split_call (curr_insn,
		   gen_call_value_multiple_split (operands[0], operands[1],
						  operands[2], operands[3]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_value_multiple_split"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return mips_output_jump (operands, 1, 2, true); }
  [(set_attr "jal" "indirect,direct")
   (set_attr "jal_macro" "no")])

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
      mips_emit_move (SET_DEST (set), SET_SRC (set));
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
  [(prefetch (match_operand:QI 0 "address_operand" "ZD")
	     (match_operand 1 "const_int_operand" "n")
	     (match_operand 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && TARGET_EXPLICIT_RELOCS"
{
  if (TARGET_LOONGSON_2EF || TARGET_LOONGSON_EXT)
    {
      /* Loongson 2[ef] and Loongson ext use load to $0 for prefetching.  */
      if (TARGET_64BIT)
	return "ld\t$0,%a0";
      else
	return "lw\t$0,%a0";
    }
  if (operands[1] == const2_rtx)
    operands[1] = const0_rtx;
  /* Loongson ext2 implementation pref instructions.  */
  if (TARGET_LOONGSON_EXT2)
    {
      operands[1] = mips_loongson_ext2_prefetch_cookie (operands[1],
							operands[2]);
      return "pref\t%1, %a0";
    }
  operands[1] = mips_prefetch_cookie (operands[1], operands[2]);
  return "pref\t%1,%a0";
}
  [(set_attr "type" "prefetch")])

(define_insn "*prefetch_indexed_<mode>"
  [(prefetch (plus:P (match_operand:P 0 "register_operand" "d")
		     (match_operand:P 1 "register_operand" "d"))
	     (match_operand 2 "const_int_operand" "n")
	     (match_operand 3 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCHX && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  if (TARGET_LOONGSON_EXT)
    {
      /* Loongson Loongson ext use index load to $0 for prefetching.  */
      if (TARGET_64BIT)
	return "gsldx\t$0,0(%0,%1)";
      else
	return "gslwx\t$0,0(%0,%1)";
    }
  /* Loongson ext2 implementation pref instructions.  */
  if (TARGET_LOONGSON_EXT2)
    {
      operands[2] = mips_loongson_ext2_prefetch_cookie (operands[2],
							operands[3]);
      return "prefx\t%2,%1(%0)";
    }
  operands[2] = mips_prefetch_cookie (operands[2], operands[3]);
  return "prefx\t%2,%1(%0)";
}
  [(set_attr "type" "prefetchx")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "%(nop%)"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])

;; Like nop, but commented out when outside a .set noreorder block.
(define_insn "hazard_nop"
  [(const_int 1)]
  ""
  {
    if (mips_noreorder.nesting_level > 0)
      return "nop";
    else
      return "#nop";
  }
  [(set_attr "type"	"nop")])

;; The `.insn' pseudo-op.
(define_insn "insn_pseudo"
  [(unspec_volatile [(const_int 0)] UNSPEC_INSN_PSEUDO)]
  ""
  ".insn"
  [(set_attr "mode" "none")
   (set_attr "insn_count" "0")])

;; MIPS4 Conditional move instructions.

(define_insn "mov<GPR:mode>_on_<MOVECC:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(if_then_else:GPR
	 (match_operator 4 "equality_operator"
		[(match_operand:MOVECC 1 "register_operand" "<MOVECC:reg>,<MOVECC:reg>")
		 (const_int 0)])
	 (match_operand:GPR 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:GPR 3 "reg_or_0_operand" "0,dJ")))]
  "!TARGET_MIPS16 && ISA_HAS_CONDMOVE"
  "@
    mov%T4\t%0,%z2,%1
    mov%t4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "mov<GPR:mode>_on_<MOVECC:mode>_mips16e2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,d")
	(if_then_else:GPR
	 (match_operator 4 "equality_operator"
		[(match_operand:MOVECC 1 "register_operand" "<MOVECC:reg>,<MOVECC:reg>,t,t")
		 (const_int 0)])
	 (match_operand:GPR 2 "reg_or_0_operand_mips16e2" "dJ,0,dJ,0")
	 (match_operand:GPR 3 "reg_or_0_operand_mips16e2" "0,dJ,0,dJ")))]
  "ISA_HAS_MIPS16E2 && ISA_HAS_CONDMOVE"
  "@
    mov%T4\t%0,%z2,%1
    mov%t4\t%0,%z3,%1
    movt%T4\t%0,%z2
    movt%t4\t%0,%z3"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")
   (set_attr "extended_mips16" "yes")])

(define_insn "mov<GPR:mode>_on_<GPR2:mode>_ne"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
       (if_then_else:GPR
        (match_operand:GPR2 1 "register_operand" "<GPR2:reg>,<GPR2:reg>")
        (match_operand:GPR 2 "reg_or_0_operand" "dJ,0")
        (match_operand:GPR 3 "reg_or_0_operand" "0,dJ")))]
  "!TARGET_MIPS16 && ISA_HAS_CONDMOVE"
  "@
    movn\t%0,%z2,%1
    movz\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "mov<GPR:mode>_on_<GPR2:mode>_ne_mips16e2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,d")
	   (if_then_else:GPR
		(match_operand:GPR2 1 "register_operand" "<GPR2:reg>,<GPR2:reg>,t,t")
		(match_operand:GPR 2 "reg_or_0_operand_mips16e2" "dJ,0,dJ,0")
		(match_operand:GPR 3 "reg_or_0_operand_mips16e2" "0,dJ,0,dJ")))]
 "ISA_HAS_MIPS16E2 && ISA_HAS_CONDMOVE"
  "@
    movn\t%0,%z2,%1
    movz\t%0,%z3,%1
    movtn\t%0,%z2
    movtz\t%0,%z3"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")
   (set_attr "extended_mips16" "yes")])

(define_insn "*mov<SCALARF:mode>_on_<MOVECC:mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f,f")
	(if_then_else:SCALARF
	 (match_operator 4 "equality_operator"
		[(match_operand:MOVECC 1 "register_operand" "<MOVECC:reg>,<MOVECC:reg>")
		 (const_int 0)])
	 (match_operand:SCALARF 2 "register_operand" "f,0")
	 (match_operand:SCALARF 3 "register_operand" "0,f")))]
  "ISA_HAS_FP_CONDMOVE"
  "@
    mov%T4.<fmt>\t%0,%2,%1
    mov%t4.<fmt>\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<SCALARF:MODE>")])

(define_insn "*sel<code><GPR:mode>_using_<GPR2:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(if_then_else:GPR
	 (equality_op:GPR2 (match_operand:GPR2 1 "register_operand" "d,d")
			   (const_int 0))
	 (match_operand:GPR 2 "reg_or_0_operand" "d,J")
	 (match_operand:GPR 3 "reg_or_0_operand" "J,d")))]
  "ISA_HAS_SEL
   && (register_operand (operands[2], <GPR:MODE>mode)
       != register_operand (operands[3], <GPR:MODE>mode))"
  "@
   <sel>\t%0,%2,%1
   <selinv>\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

;; sel.fmt copies the 3rd argument when the 1st is non-zero and the 2nd
;; argument if the 1st is zero.  This means operand 2 and 3 are
;; inverted in the instruction.

(define_insn "*sel<mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f,f,f")
	(if_then_else:SCALARF
	 (ne:CCF (match_operand:CCF 1 "register_operand" "0,f,f")
		 (const_int 0))
	 (match_operand:SCALARF 2 "reg_or_0_operand" "f,G,f")
	 (match_operand:SCALARF 3 "reg_or_0_operand" "f,f,G")))]
  "ISA_HAS_SEL && ISA_HAS_CCF"
  "@
   sel.<fmt>\t%0,%3,%2
   seleqz.<fmt>\t%0,%3,%1
   selnez.<fmt>\t%0,%2,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<SCALARF:MODE>")])

;; These are the main define_expand's used to make conditional moves.

(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_dup 5)
			  (match_operand:GPR 2 "reg_or_0_operand")
			  (match_operand:GPR 3 "reg_or_0_operand")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_SEL"
{
  if (!ISA_HAS_FP_CONDMOVE
      && !INTEGRAL_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  mips_expand_conditional_move (operands);
  DONE;
})

(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:SCALARF 0 "register_operand")
	(if_then_else:SCALARF (match_dup 5)
			      (match_operand:SCALARF 2 "reg_or_0_operand")
			      (match_operand:SCALARF 3 "reg_or_0_operand")))]
  "ISA_HAS_FP_CONDMOVE
   || (ISA_HAS_SEL && ISA_HAS_CCF)"
{
  if (ISA_HAS_SEL && !FLOAT_MODE_P (GET_MODE (XEXP (operands[1], 0))))
    FAIL;

  /* Workaround an LRA bug which means that tied operands in the sel.fmt
     pattern lead to the double precision destination of sel.d getting
     reloaded with the full register file usable and the restrictions on
     whether the CCFmode input can be used in odd-numbered single-precision
     registers are ignored.  For consistency reasons the CCF mode values
     must be guaranteed to only exist in the even-registers because of
     the unusual duality between single and double precision values.  */
  if (ISA_HAS_SEL && <MODE>mode == DFmode
      && (!TARGET_ODD_SPREG || TARGET_FLOATXX))
    FAIL;

  mips_expand_conditional_move (operands);
  DONE;
})

(define_expand "speculation_barrier"
  [(unspec_volatile [(const_int 0)] VUNSPEC_SPECULATION_BARRIER)]
  ""
  "
  mips_emit_speculation_barrier_function ();
  DONE;
  "
)


;;
;;  ....................
;;
;;	mips16 inline constant tables
;;
;;  ....................
;;

(define_insn "consttable"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "")]
		    UNSPEC_CONSTTABLE)]
  ""
  ""
  [(set_attr "mode" "none")
   (set_attr "insn_count" "0")])

(define_insn "consttable_end"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "")]
		    UNSPEC_CONSTTABLE_END)]
  ""
  ""
  [(set_attr "mode" "none")
   (set_attr "insn_count" "0")])

(define_insn "consttable_tls_reloc"
  [(unspec_volatile [(match_operand 0 "tls_reloc_operand" "")
		     (match_operand 1 "const_int_operand" "")]
		    UNSPEC_CONSTTABLE_INT)]
  "TARGET_MIPS16_PCREL_LOADS"
  { return mips_output_tls_reloc_directive (&operands[0]); }
  [(set (attr "length") (symbol_ref "INTVAL (operands[1])"))])

(define_insn "consttable_int"
  [(unspec_volatile [(match_operand 0 "consttable_operand" "")
		     (match_operand 1 "const_int_operand" "")]
		    UNSPEC_CONSTTABLE_INT)]
  "TARGET_MIPS16"
{
  assemble_integer (mips_strip_unspec_address (operands[0]),
		    INTVAL (operands[1]),
		    BITS_PER_UNIT * INTVAL (operands[1]), 1);
  return "";
}
  [(set (attr "length") (symbol_ref "INTVAL (operands[1])"))])

(define_insn "consttable_float"
  [(unspec_volatile [(match_operand 0 "consttable_operand" "")]
		    UNSPEC_CONSTTABLE_FLOAT)]
  "TARGET_MIPS16"
{
  gcc_assert (GET_CODE (operands[0]) == CONST_DOUBLE);
  assemble_real (*CONST_DOUBLE_REAL_VALUE (operands[0]),
		 as_a <scalar_float_mode> (GET_MODE (operands[0])),
		 GET_MODE_BITSIZE (GET_MODE (operands[0])));
  return "";
}
  [(set (attr "length")
	(symbol_ref "GET_MODE_SIZE (GET_MODE (operands[0]))"))])

(define_insn "align"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "")] UNSPEC_ALIGN)]
  ""
  ".align\t%0"
  [(set (attr "length") (symbol_ref "(1 << INTVAL (operands[0])) - 1"))])

(define_split
  [(match_operand 0 "small_data_pattern")]
  "reload_completed"
  [(match_dup 0)]
  { operands[0] = mips_rewrite_small_data (operands[0]); })

;;
;;  ....................
;;
;;	MIPS16e Save/Restore
;;
;;  ....................
;;

(define_insn "*mips16e_save_restore"
  [(match_parallel 0 ""
       [(set (match_operand:SI 1 "register_operand")
	     (plus:SI (match_dup 1)
		      (match_operand:SI 2 "const_int_operand")))])]
  "operands[1] == stack_pointer_rtx
   && mips16e_save_restore_pattern_p (operands[0], INTVAL (operands[2]), NULL)"
  { return mips16e_output_save_restore (operands[0], INTVAL (operands[2])); }
  [(set_attr "type" "arith")
   (set_attr "extended_mips16" "yes")])

;; Thread-Local Storage

;; The TLS base pointer is accessed via "rdhwr $3, $29".  No current
;; MIPS architecture defines this register, and no current
;; implementation provides it; instead, any OS which supports TLS is
;; expected to trap and emulate this instruction.  rdhwr is part of the
;; MIPS 32r2 specification, but we use it on any architecture because
;; we expect it to be emulated.  Use .set to force the assembler to
;; accept it.
;;
;; We do not use a constraint to force the destination to be $3
;; because $3 can appear explicitly as a function return value.
;; If we leave the use of $3 implicit in the constraints until
;; reload, we may end up making a $3 return value live across
;; the instruction, leading to a spill failure when reloading it.
(define_insn_and_split "tls_get_tp_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(const_int 0)] UNSPEC_TLS_GET_TP))
   (clobber (reg:P TLS_GET_TP_REGNUM))]
  "HAVE_AS_TLS && !TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(set (reg:P TLS_GET_TP_REGNUM)
	(unspec:P [(const_int 0)] UNSPEC_TLS_GET_TP))
   (set (match_dup 0) (reg:P TLS_GET_TP_REGNUM))]
  ""
  [(set_attr "type" "unknown")
   (set_attr "mode" "<MODE>")
   (set_attr "insn_count" "2")])

(define_insn "*tls_get_tp_<mode>_split"
  [(set (reg:P TLS_GET_TP_REGNUM)
	(unspec:P [(const_int 0)] UNSPEC_TLS_GET_TP))]
  "HAVE_AS_TLS && !TARGET_MIPS16"
  {
    if (mips_isa_rev >= 2)
      return "rdhwr\t$3,$29";

    return ".set\tpush\;.set\tmips32r2\t\;rdhwr\t$3,$29\;.set\tpop";
  }
  [(set_attr "type" "unknown")
   ; Since rdhwr always generates a trap for now, putting it in a delay
   ; slot would make the kernel's emulation of it much slower.
   (set_attr "can_delay" "no")
   (set_attr "mode" "<MODE>")])

;; In MIPS16 mode, the TLS base pointer is accessed by a
;; libgcc helper function __mips16_rdhwr(), as 'rdhwr' is not
;; accessible in MIPS16.
;;
;; This is not represented as a call insn, to avoid the
;; unnecesarry clobbering of caller-save registers by a
;; function consisting only of: "rdhwr $3,$29; j $31; nop;"
;;
;; A $25 clobber is added to cater for a $25 load stub added by the
;; linker to __mips16_rdhwr when the call is made from non-PIC code.

(define_insn_and_split "tls_get_tp_mips16_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "call_insn_operand" "dS")]
		  UNSPEC_TLS_GET_TP))
   (clobber (reg:P TLS_GET_TP_REGNUM))
   (clobber (reg:P PIC_FUNCTION_ADDR_REGNUM))
   (clobber (reg:P RETURN_ADDR_REGNUM))]
  "HAVE_AS_TLS && TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:P TLS_GET_TP_REGNUM)
	  	   (unspec:P [(match_dup 1)] UNSPEC_TLS_GET_TP))
	      (clobber (reg:P PIC_FUNCTION_ADDR_REGNUM))
	      (clobber (reg:P RETURN_ADDR_REGNUM))])
   (set (match_dup 0) (reg:P TLS_GET_TP_REGNUM))]
  ""
  [(set_attr "type" "multi")
   (set_attr "insn_count" "4")
   (set_attr "mode" "<MODE>")])

(define_insn "*tls_get_tp_mips16_call_<mode>"
  [(set (reg:P TLS_GET_TP_REGNUM)
	(unspec:P [(match_operand:P 0 "call_insn_operand" "dS")]
		  UNSPEC_TLS_GET_TP))
   (clobber (reg:P PIC_FUNCTION_ADDR_REGNUM))
   (clobber (reg:P RETURN_ADDR_REGNUM))]
  "HAVE_AS_TLS && TARGET_MIPS16"
  { return mips_output_jump (operands, 0, -1, true); }
  [(set_attr "type" "call")
   (set_attr "insn_count" "3")
   (set_attr "mode" "<MODE>")])

;; Named pattern for expanding thread pointer reference.
(define_expand "get_thread_pointer<mode>"
  [(match_operand:P 0 "register_operand" "=d")]
  "HAVE_AS_TLS"
{
  mips_expand_thread_pointer (operands[0]);
  DONE;
})

;; __builtin_mips_get_fcsr: move the FCSR into operand 0.
(define_expand "mips_get_fcsr"
  [(set (match_operand:SI 0 "register_operand")
       (unspec_volatile:SI [(const_int 0)] UNSPEC_GET_FCSR))]
  "TARGET_HARD_FLOAT_ABI"
{
  if (TARGET_MIPS16)
    {
      mips16_expand_get_fcsr (operands[0]);
      DONE;
    }
})

(define_insn "*mips_get_fcsr"
  [(set (match_operand:SI 0 "register_operand" "=d")
       (unspec_volatile:SI [(const_int 0)] UNSPEC_GET_FCSR))]
  "TARGET_HARD_FLOAT"
  "cfc1\t%0,$31")

;; See tls_get_tp_mips16_<mode> for why this form is used.
(define_insn "mips_get_fcsr_mips16_<mode>"
  [(set (reg:SI GET_FCSR_REGNUM)
	(unspec:SI [(match_operand:P 0 "call_insn_operand" "dS")]
		   UNSPEC_GET_FCSR))
   (clobber (reg:P PIC_FUNCTION_ADDR_REGNUM))
   (clobber (reg:P RETURN_ADDR_REGNUM))]
  "TARGET_HARD_FLOAT_ABI && TARGET_MIPS16"
  { return mips_output_jump (operands, 0, -1, true); }
  [(set_attr "type" "call")
   (set_attr "insn_count" "3")])

;; __builtin_mips_set_fcsr: move operand 0 into the FCSR.
(define_expand "mips_set_fcsr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand")]
  		    UNSPEC_SET_FCSR)]
  "TARGET_HARD_FLOAT_ABI"
{
  if (TARGET_MIPS16)
    {
      mips16_expand_set_fcsr (operands[0]);
      DONE;
    }
})

(define_insn "*mips_set_fcsr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "d")]
  		    UNSPEC_SET_FCSR)]
  "TARGET_HARD_FLOAT"
  "ctc1\t%0,$31")

;; See tls_get_tp_mips16_<mode> for why this form is used.
(define_insn "mips_set_fcsr_mips16_<mode>"
  [(unspec_volatile:SI [(match_operand:P 0 "call_insn_operand" "dS")
  	                (reg:SI SET_FCSR_REGNUM)] UNSPEC_SET_FCSR)
   (clobber (reg:P PIC_FUNCTION_ADDR_REGNUM))
   (clobber (reg:P RETURN_ADDR_REGNUM))]
  "TARGET_HARD_FLOAT_ABI && TARGET_MIPS16"
  { return mips_output_jump (operands, 0, -1, true); }
  [(set_attr "type" "call")
   (set_attr "insn_count" "3")])

;; Match paired HI/SI/SF/DFmode load/stores.
(define_insn "*join2_load_store<JOIN_MODE:mode>"
  [(set (match_operand:JOIN_MODE 0 "nonimmediate_operand" "=d,f,m,m")
	(match_operand:JOIN_MODE 1 "nonimmediate_operand" "m,m,d,f"))
   (set (match_operand:JOIN_MODE 2 "nonimmediate_operand" "=d,f,m,m")
	(match_operand:JOIN_MODE 3 "nonimmediate_operand" "m,m,d,f"))]
  "ENABLE_LD_ST_PAIRS && reload_completed"
  {
    bool load_p = (which_alternative == 0 || which_alternative == 1);
    /* Reg-renaming pass reuses base register if it is dead after bonded loads.
       Hardware does not bond those loads, even when they are consecutive.
       However, order of the loads need to be checked for correctness.  */
    if (!load_p || !reg_overlap_mentioned_p (operands[0], operands[1]))
      {
	output_asm_insn (mips_output_move (operands[0], operands[1]),
			 operands);
	output_asm_insn (mips_output_move (operands[2], operands[3]),
			 &operands[2]);
      }
    else
      {
	output_asm_insn (mips_output_move (operands[2], operands[3]),
			 &operands[2]);
	output_asm_insn (mips_output_move (operands[0], operands[1]),
			 operands);
      }
    return "";
  }
  [(set_attr "move_type" "load,fpload,store,fpstore")
   (set_attr "insn_count" "2,2,2,2")])

;; 2 HI/SI/SF/DF loads are joined.
;; P5600 does not support bonding of two LBs, hence QI mode is not included.
;; The loads must be non-volatile as they might be reordered at the time of asm
;; generation.
(define_peephole2
  [(set (match_operand:JOIN_MODE 0 "register_operand")
	(match_operand:JOIN_MODE 1 "non_volatile_mem_operand"))
   (set (match_operand:JOIN_MODE 2 "register_operand")
	(match_operand:JOIN_MODE 3 "non_volatile_mem_operand"))]
  "ENABLE_LD_ST_PAIRS
   && mips_load_store_bonding_p (operands, <JOIN_MODE:MODE>mode, true)"
  [(parallel [(set (match_dup 0)
		   (match_dup 1))
	      (set (match_dup 2)
		   (match_dup 3))])]
  "")

;; 2 HI/SI/SF/DF stores are joined.
;; P5600 does not support bonding of two SBs, hence QI mode is not included.
(define_peephole2
  [(set (match_operand:JOIN_MODE 0 "memory_operand")
	(match_operand:JOIN_MODE 1 "register_operand"))
   (set (match_operand:JOIN_MODE 2 "memory_operand")
	(match_operand:JOIN_MODE 3 "register_operand"))]
  "ENABLE_LD_ST_PAIRS
   && mips_load_store_bonding_p (operands, <JOIN_MODE:MODE>mode, false)"
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
  "ENABLE_LD_ST_PAIRS && reload_completed"
  {
    /* Reg-renaming pass reuses base register if it is dead after bonded loads.
       Hardware does not bond those loads, even when they are consecutive.
       However, order of the loads need to be checked for correctness.  */
    if (!reg_overlap_mentioned_p (operands[0], operands[1]))
      {
	output_asm_insn ("lh<u>\t%0,%1", operands);
	output_asm_insn ("lh<u>\t%2,%3", operands);
      }
    else
      {
	output_asm_insn ("lh<u>\t%2,%3", operands);
	output_asm_insn ("lh<u>\t%0,%1", operands);
      }

    return "";
  }
  [(set_attr "move_type" "load")
   (set_attr "insn_count" "2")])

;;
;;  Float point MIN/MAX
;;

(define_insn "smin<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(smin:SCALARF (match_operand:SCALARF 1 "register_operand" "f")
		      (match_operand:SCALARF 2 "register_operand" "f")))]
  "ISA_HAS_FMIN_FMAX"
  "min.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(smax:SCALARF (match_operand:SCALARF 1 "register_operand" "f")
		      (match_operand:SCALARF 2 "register_operand" "f")))]
  "ISA_HAS_FMIN_FMAX"
  "max.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
  (set_attr "mode" "<UNITMODE>")])

(define_insn "fmin<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(unspec:SCALARF [(use (match_operand:SCALARF 1 "register_operand" "f"))
			 (use (match_operand:SCALARF 2 "register_operand" "f"))]
			UNSPEC_FMIN))]
  "ISA_HAS_FMIN_FMAX"
  "min.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "fmax<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(unspec:SCALARF [(use (match_operand:SCALARF 1 "register_operand" "f"))
			 (use (match_operand:SCALARF 2 "register_operand" "f"))]
			UNSPEC_FMAX))]
  "ISA_HAS_FMIN_FMAX"
  "max.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
  (set_attr "mode" "<UNITMODE>")])

(define_insn "fmin_a_<mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
    (if_then_else
       (lt (abs:SCALARF (match_operand:SCALARF 1 "register_operand" "f"))
           (abs:SCALARF (match_operand:SCALARF 2 "register_operand" "f")))
       (match_dup 1)
       (match_dup 2)))]
  "ISA_HAS_FMIN_FMAX"
  "mina.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "fmax_a_<mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
    (if_then_else
       (gt (abs:SCALARF (match_operand:SCALARF 1 "register_operand" "f"))
           (abs:SCALARF (match_operand:SCALARF 2 "register_operand" "f")))
       (match_dup 1)
       (match_dup 2)))]
  "ISA_HAS_FMIN_FMAX"
  "maxa.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fminmax")
   (set_attr "mode" "<UNITMODE>")])

;;Float point round to integral
(define_insn "rint<mode>2"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(unspec:SCALARF [(match_operand:SCALARF 1 "register_operand" "f")]
			UNSPEC_FRINT))]
  "ISA_HAS_FRINT"
  "rint.<fmt>\t%0,%1"
  [(set_attr "type" "frint")
   (set_attr "mode" "<UNITMODE>")])

;;Float point class mask
(define_insn "fclass_<mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(unspec:SCALARF [(match_operand:SCALARF 1 "register_operand" "f")]
			UNSPEC_FCLASS))]
  "ISA_HAS_FCLASS"
  "class.<fmt>\t%0,%1"
  [(set_attr "type" "fclass")
   (set_attr "mode" "<UNITMODE>")])

;; 2 HI loads are joined.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
	(any_extend:SI (match_operand:HI 1 "non_volatile_mem_operand")))
   (set (match_operand:SI 2 "register_operand")
	(any_extend:SI (match_operand:HI 3 "non_volatile_mem_operand")))]
  "ENABLE_LD_ST_PAIRS
   && mips_load_store_bonding_p (operands, HImode, true)"
  [(parallel [(set (match_dup 0)
		   (any_extend:SI (match_dup 1)))
	      (set (match_dup 2)
		   (any_extend:SI (match_dup 3)))])]
  "")


;; Synchronization instructions.

(include "sync.md")

; The MIPS Paired-Single Floating Point and MIPS-3D Instructions.

(include "mips-ps-3d.md")

; The MIPS DSP Instructions.

(include "mips-dsp.md")

; The MIPS DSP REV 2 Instructions.

(include "mips-dspr2.md")

; MIPS fixed-point instructions.
(include "mips-fixed.md")

; microMIPS patterns.
(include "micromips.md")

; Loongson MultiMedia extensions Instructions (MMI) patterns.
(include "loongson-mmi.md")

; The MIPS MSA Instructions.
(include "mips-msa.md")

(define_c_enum "unspec" [
  UNSPEC_ADDRESS_FIRST
])
