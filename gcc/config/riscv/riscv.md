;; Machine description for RISC-V for GNU compiler.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
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


;; Keep this list and the one above riscv_print_operand in sync.
;; The special asm out single letter directives following a '%' are:
;; h -- Print the high-part relocation associated with OP, after stripping
;;	  any outermost HIGH.
;; R -- Print the low-part relocation associated with OP.
;; C -- Print the integer branch condition for comparison OP.
;; A -- Print the atomic operation suffix for memory model OP.
;; F -- Print a FENCE if the memory model requires a release.
;; z -- Print x0 if OP is zero, otherwise print OP normally.
;; i -- Print i if the operand is not a register.
;; S -- Print shift-index of single-bit mask OP.
;; T -- Print shift-index of inverted single-bit mask OP.
;; ~ -- Print w if TARGET_64BIT is true; otherwise not print anything.

(define_c_enum "unspec" [
  ;; Override return address for exception handling.
  UNSPEC_EH_RETURN

  ;; Symbolic accesses.  The order of this list must match that of
  ;; enum riscv_symbol_type in riscv-protos.h.
  UNSPEC_ADDRESS_FIRST
  UNSPEC_FORCE_FOR_MEM
  UNSPEC_PCREL
  UNSPEC_LOAD_GOT
  UNSPEC_TLS
  UNSPEC_TLS_LE
  UNSPEC_TLS_IE
  UNSPEC_TLS_GD
  UNSPEC_TLSDESC
  ;; High part of PC-relative address.
  UNSPEC_AUIPC

  ;; Floating-point unspecs.
  UNSPEC_FLT_QUIET
  UNSPEC_FLE_QUIET
  UNSPEC_COPYSIGN
  UNSPEC_FMV_X_W
  UNSPEC_FMVH_X_D
  UNSPEC_RINT
  UNSPEC_ROUND
  UNSPEC_FLOOR
  UNSPEC_CEIL
  UNSPEC_BTRUNC
  UNSPEC_ROUNDEVEN
  UNSPEC_NEARBYINT
  UNSPEC_LRINT
  UNSPEC_FMIN
  UNSPEC_FMAX
  UNSPEC_FMINM
  UNSPEC_FMAXM
  UNSPEC_FCLASS

  ;; Stack tie
  UNSPEC_TIE

  ;; OR-COMBINE
  UNSPEC_ORC_B

  ;; Zbc unspecs
  UNSPEC_CLMUL
  UNSPEC_CLMULH
  UNSPEC_CLMULR

  ;; the calling convention of callee
  UNSPEC_CALLEE_CC

  ;; String unspecs
  UNSPEC_STRLEN

  ;; Workaround for HFmode and BFmode without hardware extension
  UNSPEC_FMV_FP16_X

  ;; XTheadFmv moves
  UNSPEC_XTHEADFMV
  UNSPEC_XTHEADFMV_HW

  ;; CRC unspecs
  UNSPEC_CRC
  UNSPEC_CRC_REV
])

(define_c_enum "unspecv" [
  ;; Register save and restore.
  UNSPECV_GPR_SAVE
  UNSPECV_GPR_RESTORE

  ;; Floating-point unspecs.
  UNSPECV_FRCSR
  UNSPECV_FSCSR
  UNSPECV_FRFLAGS
  UNSPECV_FSFLAGS
  UNSPECV_FSNVSNAN

  ;; Interrupt handler instructions.
  UNSPECV_MRET
  UNSPECV_SRET
  UNSPECV_URET

  ;; Blockage and synchronization.
  UNSPECV_BLOCKAGE
  UNSPECV_FENCE
  UNSPECV_FENCE_I

  ;; Stack Smash Protector
  UNSPEC_SSP_SET
  UNSPEC_SSP_TEST

  ;; CMO instructions.
  UNSPECV_CLEAN
  UNSPECV_FLUSH
  UNSPECV_INVAL
  UNSPECV_ZERO
  UNSPECV_PREI

  ;; Zihintpause unspec
  UNSPECV_PAUSE

  ;; ZICFISS
  UNSPECV_SSPUSH
  UNSPECV_SSPOPCHK
  UNSPECV_SSRDP
  UNSPECV_SSP

  ;; ZICFILP
  UNSPECV_LPAD
  UNSPECV_SETLPL
  UNSPECV_LPAD_ALIGN
  UNSPECV_SET_GUARDED

  ;; XTheadInt unspec
  UNSPECV_XTHEADINT_PUSH
  UNSPECV_XTHEADINT_POP
])

(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (SP_REGNUM 			2)
   (GP_REGNUM 			3)
   (TP_REGNUM			4)
   (T0_REGNUM			5)
   (T1_REGNUM			6)
   (T2_REGNUM			7)
   (S0_REGNUM			8)
   (S1_REGNUM			9)
   (A0_REGNUM			10)
   (A1_REGNUM			11)
   (S2_REGNUM			18)
   (S3_REGNUM			19)
   (S4_REGNUM			20)
   (S5_REGNUM			21)
   (S6_REGNUM			22)
   (S7_REGNUM			23)
   (S8_REGNUM			24)
   (S9_REGNUM			25)
   (S10_REGNUM			26)
   (S11_REGNUM			27)

   (NORMAL_RETURN		0)
   (SIBCALL_RETURN		1)
   (EXCEPTION_RETURN		2)
   (VL_REGNUM			66)
   (VTYPE_REGNUM		67)
   (VXRM_REGNUM			68)
   (FRM_REGNUM			69)
])

(include "predicates.md")
(include "constraints.md")
(include "iterators.md")

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
   const,logical,arith,andi,shift_shift,rdvlenb"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,HF,BF,SF,DF,TF,
  RVVMF64BI,RVVMF32BI,RVVMF16BI,RVVMF8BI,RVVMF4BI,RVVMF2BI,RVVM1BI,
  RVVM8QI,RVVM4QI,RVVM2QI,RVVM1QI,RVVMF2QI,RVVMF4QI,RVVMF8QI,
  RVVM8HI,RVVM4HI,RVVM2HI,RVVM1HI,RVVMF2HI,RVVMF4HI,
  RVVM8BF,RVVM4BF,RVVM2BF,RVVM1BF,RVVMF2BF,RVVMF4BF,
  RVVM8HF,RVVM4HF,RVVM2HF,RVVM1HF,RVVMF2HF,RVVMF4HF,
  RVVM8SI,RVVM4SI,RVVM2SI,RVVM1SI,RVVMF2SI,
  RVVM8SF,RVVM4SF,RVVM2SF,RVVM1SF,RVVMF2SF,
  RVVM8DI,RVVM4DI,RVVM2DI,RVVM1DI,
  RVVM8DF,RVVM4DF,RVVM2DF,RVVM1DF,
  RVVM1x8QI,RVVMF2x8QI,RVVMF4x8QI,RVVMF8x8QI,
  RVVM1x7QI,RVVMF2x7QI,RVVMF4x7QI,RVVMF8x7QI,
  RVVM1x6QI,RVVMF2x6QI,RVVMF4x6QI,RVVMF8x6QI,
  RVVM1x5QI,RVVMF2x5QI,RVVMF4x5QI,RVVMF8x5QI,
  RVVM2x4QI,RVVM1x4QI,RVVMF2x4QI,RVVMF4x4QI,RVVMF8x4QI,
  RVVM2x3QI,RVVM1x3QI,RVVMF2x3QI,RVVMF4x3QI,RVVMF8x3QI,
  RVVM4x2QI,RVVM2x2QI,RVVM1x2QI,RVVMF2x2QI,RVVMF4x2QI,RVVMF8x2QI,
  RVVM1x8HI,RVVMF2x8HI,RVVMF4x8HI,
  RVVM1x7HI,RVVMF2x7HI,RVVMF4x7HI,
  RVVM1x6HI,RVVMF2x6HI,RVVMF4x6HI,
  RVVM1x5HI,RVVMF2x5HI,RVVMF4x5HI,
  RVVM2x4HI,RVVM1x4HI,RVVMF2x4HI,RVVMF4x4HI,
  RVVM2x3HI,RVVM1x3HI,RVVMF2x3HI,RVVMF4x3HI,
  RVVM4x2HI,RVVM2x2HI,RVVM1x2HI,RVVMF2x2HI,RVVMF4x2HI,
  RVVM1x8BF,RVVMF2x8BF,RVVMF4x8BF,RVVM1x7BF,RVVMF2x7BF,
  RVVMF4x7BF,RVVM1x6BF,RVVMF2x6BF,RVVMF4x6BF,RVVM1x5BF,
  RVVMF2x5BF,RVVMF4x5BF,RVVM2x4BF,RVVM1x4BF,RVVMF2x4BF,
  RVVMF4x4BF,RVVM2x3BF,RVVM1x3BF,RVVMF2x3BF,RVVMF4x3BF,
  RVVM4x2BF,RVVM2x2BF,RVVM1x2BF,RVVMF2x2BF,RVVMF4x2BF,
  RVVM1x8HF,RVVMF2x8HF,RVVMF4x8HF,RVVM1x7HF,RVVMF2x7HF,
  RVVMF4x7HF,RVVM1x6HF,RVVMF2x6HF,RVVMF4x6HF,RVVM1x5HF,
  RVVMF2x5HF,RVVMF4x5HF,RVVM2x4HF,RVVM1x4HF,RVVMF2x4HF,
  RVVMF4x4HF,RVVM2x3HF,RVVM1x3HF,RVVMF2x3HF,RVVMF4x3HF,
  RVVM4x2HF,RVVM2x2HF,RVVM1x2HF,RVVMF2x2HF,RVVMF4x2HF,
  RVVM1x8SI,RVVMF2x8SI,
  RVVM1x7SI,RVVMF2x7SI,
  RVVM1x6SI,RVVMF2x6SI,
  RVVM1x5SI,RVVMF2x5SI,
  RVVM2x4SI,RVVM1x4SI,RVVMF2x4SI,
  RVVM2x3SI,RVVM1x3SI,RVVMF2x3SI,
  RVVM4x2SI,RVVM2x2SI,RVVM1x2SI,RVVMF2x2SI,
  RVVM1x8SF,RVVMF2x8SF,RVVM1x7SF,RVVMF2x7SF,
  RVVM1x6SF,RVVMF2x6SF,RVVM1x5SF,RVVMF2x5SF,
  RVVM2x4SF,RVVM1x4SF,RVVMF2x4SF,RVVM2x3SF,
  RVVM1x3SF,RVVMF2x3SF,RVVM4x2SF,RVVM2x2SF,
  RVVM1x2SF,RVVMF2x2SF,
  RVVM1x8DI,RVVM1x7DI,RVVM1x6DI,RVVM1x5DI,
  RVVM2x4DI,RVVM1x4DI,RVVM2x3DI,RVVM1x3DI,
  RVVM4x2DI,RVVM2x2DI,RVVM1x2DI,RVVM1x8DF,
  RVVM1x7DF,RVVM1x6DF,RVVM1x5DF,RVVM2x4DF,
  RVVM1x4DF,RVVM2x3DF,RVVM1x3DF,RVVM4x2DF,
  RVVM2x2DF,RVVM1x2DF,
  V1QI,V2QI,V4QI,V8QI,V16QI,V32QI,V64QI,V128QI,V256QI,V512QI,V1024QI,V2048QI,V4096QI,
  V1HI,V2HI,V4HI,V8HI,V16HI,V32HI,V64HI,V128HI,V256HI,V512HI,V1024HI,V2048HI,
  V1SI,V2SI,V4SI,V8SI,V16SI,V32SI,V64SI,V128SI,V256SI,V512SI,V1024SI,
  V1DI,V2DI,V4DI,V8DI,V16DI,V32DI,V64DI,V128DI,V256DI,V512DI,
  V1HF,V2HF,V4HF,V8HF,V16HF,V32HF,V64HF,V128HF,V256HF,V512HF,V1024HF,V2048HF,
  V1SF,V2SF,V4SF,V8SF,V16SF,V32SF,V64SF,V128SF,V256SF,V512SF,V1024SF,
  V1DF,V2DF,V4DF,V8DF,V16DF,V32DF,V64DF,V128DF,V256DF,V512DF,
  V1BI,V2BI,V4BI,V8BI,V16BI,V32BI,V64BI,V128BI,V256BI,V512BI,V1024BI,V2048BI,V4096BI"
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

;; ISA attributes.
(define_attr "ext" "base,f,d,vector"
  (const_string "base"))

;; True if the extension is enabled.
(define_attr "ext_enabled" "no,yes"
  (cond [(eq_attr "ext" "base")
	 (const_string "yes")

	 (and (eq_attr "ext" "f")
	      (match_test "TARGET_HARD_FLOAT"))
	 (const_string "yes")

	 (and (eq_attr "ext" "d")
	      (match_test "TARGET_DOUBLE_FLOAT"))
	 (const_string "yes")

	 (and (eq_attr "ext" "vector")
	      (match_test "TARGET_VECTOR"))
	 (const_string "yes")
	]
	(const_string "no")))

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional direct jump
;; jalr		unconditional indirect jump
;; ret		various returns, no arguments
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
;; fcvt_i2f	integer to floating point convert
;; fcvt_f2i	floating point to integer convert
;; fsqrt	floating point square root
;; multi	multiword sequence (or user asm statements)
;; auipc	integer addition to PC
;; sfb_alu  SFB ALU instruction
;; nop		no operation
;; trap		trap instruction
;; ghost	an instruction that produces no real code
;; bitmanip	bit manipulation instructions
;; clmul    clmul, clmulh, clmulr
;; rotate   rotation instructions
;; atomic   atomic instructions
;; condmove	conditional moves
;; crypto cryptography instructions
;; mvpair    zc move pair instructions
;; zicond    zicond instructions
;; Classification of RVV instructions which will be added to each RVV .md pattern and used by scheduler.
;; rdvlenb     vector byte length vlenb csrr read
;; rdvl        vector length vl csrr read
;; wrvxrm      vector fixed-point rounding mode write
;; wrfrm       vector floating-point rounding mode write
;; vsetvl      vector configuration-setting instrucions
;; 7. Vector Loads and Stores
;; vlde        vector unit-stride load instructions
;; vste        vector unit-stride store instructions
;; vldm        vector unit-stride mask load instructions
;; vstm        vector unit-stride mask store instructions
;; vlds        vector strided load instructions
;; vsts        vector strided store instructions
;; vldux       vector unordered indexed load instructions
;; vldox       vector ordered indexed load instructions
;; vstux       vector unordered indexed store instructions
;; vstox       vector ordered indexed store instructions
;; vldff       vector unit-stride fault-only-first load instructions
;; vldr        vector whole register load instructions
;; vstr        vector whole register store instructions
;; vlsegde     vector segment unit-stride load instructions
;; vssegte     vector segment unit-stride store instructions
;; vlsegds     vector segment strided load instructions
;; vssegts     vector segment strided store instructions
;; vlsegdux    vector segment unordered indexed load instructions
;; vlsegdox    vector segment ordered indexed load instructions
;; vssegtux    vector segment unordered indexed store instructions
;; vssegtox    vector segment ordered indexed store instructions
;; vlsegdff    vector segment unit-stride fault-only-first load instructions
;; 11. Vector integer arithmetic instructions
;; vialu       vector single-width integer add and subtract and logical nstructions
;; viwalu      vector widening integer add/subtract
;; vext        vector integer extension
;; vicalu      vector arithmetic with carry or borrow instructions
;; vshift      vector single-width bit shift instructions
;; vnshift     vector narrowing integer shift instructions
;; viminmax    vector integer min/max instructions
;; vicmp       vector integer comparison instructions
;; vimul       vector single-width integer multiply instructions
;; vidiv       vector single-width integer divide instructions
;; viwmul      vector widening integer multiply instructions
;; vimuladd    vector single-width integer multiply-add instructions
;; viwmuladd   vector widening integer multiply-add instructions
;; vimerge     vector integer merge instructions
;; vimov       vector integer move vector instructions
;; 12. Vector fixed-point arithmetic instructions
;; vsalu       vector single-width saturating add and subtract and logical instructions
;; vaalu       vector single-width averaging add and subtract and logical instructions
;; vsmul       vector single-width fractional multiply with rounding and saturation instructions
;; vsshift     vector single-width scaling shift instructions
;; vnclip      vector narrowing fixed-point clip instructions
;; 13. Vector floating-point instructions
;; vfalu       vector single-width floating-point add/subtract instructions
;; vfwalu      vector widening floating-point add/subtract instructions
;; vfmul       vector single-width floating-point multiply instructions
;; vfdiv       vector single-width floating-point divide instructions
;; vfwmul      vector widening floating-point multiply instructions
;; vfmuladd    vector single-width floating-point multiply-add instructions
;; vfwmuladd   vector widening floating-point multiply-add instructions
;; vfsqrt      vector floating-point square-root instructions
;; vfrecp      vector floating-point reciprocal square-root instructions
;; vfminmax    vector floating-point min/max instructions
;; vfcmp       vector floating-point comparison instructions
;; vfsgnj      vector floating-point sign-injection instructions
;; vfclass     vector floating-point classify instruction
;; vfmerge     vector floating-point merge instruction
;; vfmov       vector floating-point move instruction
;; vfcvtitof   vector single-width integer to floating-point instruction
;; vfcvtftoi   vector single-width floating-point to integer instruction
;; vfwcvtitof  vector widening integer to floating-point instruction
;; vfwcvtftoi  vector widening floating-point to integer instruction
;; vfwcvtftof  vector widening floating-point to floating-point instruction
;; vfncvtitof  vector narrowing integer to floating-point instruction
;; vfncvtftoi  vector narrowing floating-point to integer instruction
;; vfncvtftof  vector narrowing floating-point to floating-point instruction
;; 14. Vector reduction operations
;; vired       vector single-width integer reduction instructions
;; viwred      vector widening integer reduction instructions
;; vfredu      vector single-width floating-point un-ordered reduction instruction
;; vfredo      vector single-width floating-point ordered reduction instruction
;; vfwredu     vector widening floating-point un-ordered reduction instruction
;; vfwredo     vector widening floating-point ordered reduction instruction
;; 15. Vector mask instructions
;; vmalu       vector mask-register logical instructions
;; vmpop       vector mask population count
;; vmffs       vector find-first-set mask bit
;; vmsfs       vector set mask bit
;; vmiota      vector iota
;; vmidx       vector element index instruction
;; 16. Vector permutation instructions
;; vimovvx      integer scalar move instructions
;; vimovxv      integer scalar move instructions
;; vfmovvf      floating-point scalar move instructions
;; vfmovfv      floating-point scalar move instructions
;; vslideup     vector slide instructions
;; vslidedown   vector slide instructions
;; vislide1up   vector slide instructions
;; vislide1down vector slide instructions
;; vfslide1up   vector slide instructions
;; vfslide1down vector slide instructions
;; vgather      vector register gather instructions
;; vcompress    vector compress instruction
;; vmov         whole vector register move
;; vector       unknown vector instruction
;; 17. Crypto Vector instructions
;; vandn        crypto vector bitwise and-not instructions
;; vbrev        crypto vector reverse bits in elements instructions
;; vbrev8       crypto vector reverse bits in bytes instructions
;; vrev8        crypto vector reverse bytes instructions
;; vclz         crypto vector count leading Zeros instructions
;; vctz         crypto vector count lrailing Zeros instructions
;; vrol         crypto vector rotate left instructions
;; vror         crypto vector rotate right instructions
;; vwsll        crypto vector widening shift left logical instructions
;; vclmul       crypto vector carry-less multiply - return low half instructions
;; vclmulh      crypto vector carry-less multiply - return high half instructions
;; vghsh        crypto vector add-multiply over GHASH Galois-Field instructions
;; vgmul        crypto vector multiply over GHASH Galois-Field instrumctions
;; vaesef       crypto vector AES final-round encryption instructions
;; vaesem       crypto vector AES middle-round encryption instructions
;; vaesdf       crypto vector AES final-round decryption instructions
;; vaesdm       crypto vector AES middle-round decryption instructions
;; vaeskf1      crypto vector AES-128 Forward KeySchedule generation instructions
;; vaeskf2      crypto vector AES-256 Forward KeySchedule generation instructions
;; vaesz        crypto vector AES round zero encryption/decryption instructions
;; vsha2ms      crypto vector SHA-2 message schedule instructions
;; vsha2ch      crypto vector SHA-2 two rounds of compression instructions
;; vsha2cl      crypto vector SHA-2 two rounds of compression instructions
;; vsm4k        crypto vector SM4 KeyExpansion instructions
;; vsm4r        crypto vector SM4 Rounds instructions
;; vsm3me       crypto vector SM3 Message Expansion instructions
;; vsm3c        crypto vector SM3 Compression instructions
;; 18.Vector BF16 instrctions
;; vfncvtbf16  vector narrowing single floating-point to brain floating-point instruction
;; vfwcvtbf16  vector widening brain floating-point to single floating-point instruction
;; vfwmaccbf16  vector BF16 widening multiply-accumulate
;; SiFive custom extension instrctions
;; sf_vqmacc      vector matrix integer multiply-add instructions
;; sf_vfnrclip     vector fp32 to int8 ranged clip instructions
(define_attr "type"
  "unknown,branch,jump,jalr,ret,call,load,fpload,store,fpstore,
   mtc,mfc,const,arith,logical,shift,slt,imul,idiv,move,fmove,fadd,fmul,
   fmadd,fdiv,fcmp,fcvt,fcvt_i2f,fcvt_f2i,fsqrt,multi,auipc,sfb_alu,nop,trap,
   ghost,bitmanip,rotate,clmul,min,max,minu,maxu,clz,ctz,cpop,
   atomic,condmove,crypto,mvpair,zicond,rdvlenb,rdvl,wrvxrm,wrfrm,
   rdfrm,vsetvl,vsetvl_pre,vlde,vste,vldm,vstm,vlds,vsts,
   vldux,vldox,vstux,vstox,vldff,vldr,vstr,
   vlsegde,vssegte,vlsegds,vssegts,vlsegdux,vlsegdox,vssegtux,vssegtox,vlsegdff,
   vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,
   vimul,vidiv,viwmul,vimuladd,sf_vqmacc,viwmuladd,vimerge,vimov,
   vsalu,vaalu,vsmul,vsshift,vnclip,sf_vfnrclip,
   vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,
   vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,
   vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,
   vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,
   vired,viwred,vfredu,vfredo,vfwredu,vfwredo,
   vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,
   vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,
   vgather,vcompress,vmov,vector,vandn,vbrev,vbrev8,vrev8,vclz,vctz,vcpop,vrol,vror,vwsll,
   vclmul,vclmulh,vghsh,vgmul,vaesef,vaesem,vaesdf,vaesdm,vaeskf1,vaeskf2,vaesz,
   vsha2ms,vsha2ch,vsha2cl,vsm4k,vsm4r,vsm3me,vsm3c,vfncvtbf16,vfwcvtbf16,vfwmaccbf16"
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
	 (eq_attr "move_type" "const") (const_string "const")
	 (eq_attr "move_type" "rdvlenb") (const_string "rdvlenb")]
	(const_string "unknown")))

;; True if the float point vector is disabled.
(define_attr "fp_vector_disabled" "no,yes"
  (cond [
    (and (eq_attr "type" "vfmov,vfalu,vfmul,vfdiv,
			  vfwalu,vfwmul,vfmuladd,vfwmuladd,
			  vfsqrt,vfrecp,vfminmax,vfsgnj,vfcmp,
			  vfclass,vfmerge,
			  vfncvtitof,vfwcvtftoi,vfcvtftoi,vfcvtitof,
			  vfredo,vfredu,vfwredo,vfwredu,
			  vfslide1up,vfslide1down")
	 (and (eq_attr "mode" "RVVM8HF,RVVM4HF,RVVM2HF,RVVM1HF,RVVMF2HF,RVVMF4HF")
	      (match_test "!TARGET_ZVFH")))
    (const_string "yes")

    ;; The mode records as QI for the FP16 <=> INT8 instruction.
    (and (eq_attr "type" "vfncvtftoi,vfwcvtitof")
	 (and (eq_attr "mode" "RVVM4QI,RVVM2QI,RVVM1QI,RVVMF2QI,RVVMF4QI,RVVMF8QI")
	      (match_test "!TARGET_ZVFH")))
    (const_string "yes")
  ]
  (const_string "no")))

;; This attribute marks the alternatives not matching the constraints
;; described in spec as disabled.
(define_attr "spec_restriction" "none,thv,rvv"
  (const_string "none"))

(define_attr "spec_restriction_disabled" "no,yes"
  (cond [(eq_attr "spec_restriction" "none")
	 (const_string "no")

	 (and (eq_attr "spec_restriction" "thv")
	      (match_test "TARGET_XTHEADVECTOR"))
	 (const_string "yes")

	 (and (eq_attr "spec_restriction" "rvv")
	      (match_test "TARGET_VECTOR && !TARGET_XTHEADVECTOR"))
	 (const_string "yes")
	]
       (const_string "no")))

;; Attribute to control enable or disable instructions.
(define_attr "enabled" "no,yes"
  (cond [
    (eq_attr "ext_enabled" "no")
    (const_string "no")

    (eq_attr "fp_vector_disabled" "yes")
    (const_string "no")

    (eq_attr "spec_restriction_disabled" "yes")
    (const_string "no")
  ]
  (const_string "yes")))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [
	  ;; Branches further than +/- 1 MiB require three instructions.
	  ;; Branches further than +/- 4 KiB require two instructions.
	  (eq_attr "type" "branch")
	  (if_then_else (and (le (minus (match_dup 0) (pc))
				 (const_int 4088))
			     (le (minus (pc) (match_dup 0))
				 (const_int 4092)))
			(const_int 4)
			(if_then_else (and (le (minus (match_dup 0) (pc))
					       (const_int 1048568))
					   (le (minus (pc) (match_dup 0))
					       (const_int 1048572)))
				      (const_int 8)
				      (const_int 12)))

	  ;; Jumps further than +/- 1 MiB require two instructions.
	  (eq_attr "type" "jump")
	  (if_then_else (and (le (minus (match_dup 0) (pc))
				 (const_int 1048568))
			     (le (minus (pc) (match_dup 0))
				 (const_int 1048572)))
			(const_int 4)
			(const_int 8))

	  ;; Conservatively assume calls take two instructions (AUIPC + JALR).
	  ;; The linker will opportunistically relax the sequence to JAL.
	  (eq_attr "type" "call") (const_int 8)

	  ;; "Ghost" instructions occupy no space.
	  (eq_attr "type" "ghost") (const_int 0)

	  (eq_attr "got" "load") (const_int 8)

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
  "generic,sifive_7,sifive_p400,sifive_p600,xiangshan,generic_ooo"
  (const (symbol_ref "((enum attr_tune) riscv_microarchitecture)")))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*addsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" " r,r")
		 (match_operand:SI 2 "arith_operand"    " r,I")))]
  ""
  "add%i2%~\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "addsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" " r,r")
		 (match_operand:SI 2 "arith_operand"    " r,I")))]
  ""
{
  if (TARGET_64BIT)
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

(define_insn "adddi3"
  [(set (match_operand:DI          0 "register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "register_operand" " r,r")
		 (match_operand:DI 2 "arith_operand"    " r,I")))]
  "TARGET_64BIT"
  "add%i2\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

;; Special case of adding a reg and constant if latter is sum of two S12
;; values (in range -2048 to 2047). Avoid materialized the const and fuse
;; into the add (with an additional add for 2nd value). Makes a 3 insn
;; sequence into 2 insn.

(define_insn_and_split "*add<mode>3_const_sum_of_two_s12"
  [(set (match_operand:P	 0 "register_operand" "=r,r")
	(plus:P (match_operand:P 1 "register_operand" " r,r")
		(match_operand:P 2 "const_two_s12"    " MiG,r")))]
  "!riscv_reg_frame_related (operands[0])"
{
  /* operand matching MiG constraint is always meant to be split.  */
  if (which_alternative == 0)
    return "#";
  else
    return "add %0,%1,%2";
}
  ""
  [(set (match_dup 0)
	(plus:P (match_dup 1) (match_dup 3)))
   (set (match_dup 0)
	(plus:P (match_dup 0) (match_dup 4)))]
{
  int val = INTVAL (operands[2]);
  if (SUM_OF_TWO_S12_P (val))
    {
       operands[3] = GEN_INT (2047);
       operands[4] = GEN_INT (val - 2047);
    }
  else if (SUM_OF_TWO_S12_N (val))
    {
       operands[3] = GEN_INT (-2048);
       operands[4] = GEN_INT (val + 2048);
    }
  else
      gcc_unreachable ();
}
  [(set_attr "type" "arith")
   (set_attr "mode" "<P:MODE>")])

(define_expand "addv<mode>4"
  [(set (match_operand:GPR           0 "register_operand" "=r,r")
	(plus:GPR (match_operand:GPR 1 "register_operand" " r,r")
		  (match_operand:GPR 2 "arith_operand"    " r,I")))
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);
      rtx t5 = gen_reg_rtx (DImode);
      rtx t6 = gen_reg_rtx (DImode);

      riscv_emit_binary (PLUS, operands[0], operands[1], operands[2]);
      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t4, operands[1], DImode, SImode, 0));
      else
	t4 = operands[1];
      if (GET_CODE (operands[2]) != CONST_INT)
	emit_insn (gen_extend_insn (t5, operands[2], DImode, SImode, 0));
      else
	t5 = operands[2];
      emit_insn (gen_adddi3 (t3, t4, t5));
      emit_insn (gen_extend_insn (t6, operands[0], DImode, SImode, 0));

      riscv_expand_conditional_branch (operands[3], NE, t6, t3);
    }
  else
    {
      rtx t3 = gen_reg_rtx (<MODE>mode);
      rtx t4 = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_add3_insn (operands[0], operands[1], operands[2]));
      rtx cmp1 = gen_rtx_LT (<MODE>mode, operands[2], const0_rtx);
      emit_insn (gen_cstore<mode>4 (t3, cmp1, operands[2], const0_rtx));
      rtx cmp2 = gen_rtx_LT (<MODE>mode, operands[0], operands[1]);

      emit_insn (gen_cstore<mode>4 (t4, cmp2, operands[0], operands[1]));
      riscv_expand_conditional_branch (operands[3], NE, t3, t4);
    }
  DONE;
})

(define_expand "uaddv<mode>4"
  [(set (match_operand:GPR           0 "register_operand" "=r,r")
	(plus:GPR (match_operand:GPR 1 "register_operand" " r,r")
		  (match_operand:GPR 2 "arith_operand"    " r,I")))
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);

      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t3, operands[1], DImode, SImode, 0));
      else
	t3 = operands[1];
      riscv_emit_binary (PLUS, operands[0], operands[1], operands[2]);
      emit_insn (gen_extend_insn (t4, operands[0], DImode, SImode, 0));

      riscv_expand_conditional_branch (operands[3], LTU, t4, t3);
    }
  else
    {
      emit_insn (gen_add3_insn (operands[0], operands[1], operands[2]));
      riscv_expand_conditional_branch (operands[3], LTU, operands[0],
				       operands[1]);
    }

  DONE;
})

(define_insn "addsi3_extended"
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
	  (match_operator:SI 3 "subreg_lowpart_operator"
	     [(plus:DI (match_operand:DI 1 "register_operand" " r,r")
		       (match_operand:DI 2 "arith_operand"    " r,I"))])))]
  "TARGET_64BIT"
  "add%i2w\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;; Transform (X & C1) + C2 into (X | ~C1) - (-C2 | ~C1)
;; Where C1 is not a LUI operand, but ~C1 is a LUI operand

(define_insn_and_split "*lui_constraint<X:mode>_and_to_or"
	[(set (match_operand:X 0 "register_operand" "=r")
	(plus:X (and:X (match_operand:X 1 "register_operand" "r")
		       (match_operand 2 "const_int_operand"))
		(match_operand 3 "const_int_operand")))
   (clobber (match_scratch:X 4 "=&r"))]
  "(LUI_OPERAND (~INTVAL (operands[2]))
    && ((INTVAL (operands[2]) & (-INTVAL (operands[3])))
	== (-INTVAL (operands[3])))
    && riscv_const_insns (operands[3], false)
    && (riscv_const_insns (GEN_INT (~INTVAL (operands[2])
				    | -INTVAL (operands[3])), false)
	<= riscv_const_insns (operands[3], false)))"
  "#"
  "&& reload_completed"
  [(set (match_dup 4) (match_dup 5))
   (set (match_dup 0) (ior:X (match_dup 1) (match_dup 4)))
   (set (match_dup 4) (match_dup 6))
   (set (match_dup 0) (minus:X (match_dup 0) (match_dup 4)))]
  {
    operands[5] = GEN_INT (~INTVAL (operands[2]));
    operands[6] = GEN_INT ((~INTVAL (operands[2])) | (-INTVAL (operands[3])));
  }
  [(set_attr "type" "arith")])

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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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

(define_insn "*subsi3"
  [(set (match_operand:SI           0 "register_operand" "= r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " rJ")
		  (match_operand:SI 2 "register_operand" "  r")))]
  ""
  "sub%~\t%0,%z1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "subsi3"
  [(set (match_operand:SI           0 "register_operand" "= r")
       (minus:SI (match_operand:SI 1 "reg_or_0_operand" " rJ")
                 (match_operand:SI 2 "register_operand" "  r")))]
  ""
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_subsi3_extended (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_expand "subv<mode>4"
  [(set (match_operand:GPR            0 "register_operand" "= r")
	(minus:GPR (match_operand:GPR 1 "reg_or_0_operand" " rJ")
		   (match_operand:GPR 2 "register_operand" "  r")))
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);
      rtx t5 = gen_reg_rtx (DImode);
      rtx t6 = gen_reg_rtx (DImode);

      riscv_emit_binary (MINUS, operands[0], operands[1], operands[2]);
      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t4, operands[1], DImode, SImode, 0));
      else
	t4 = operands[1];
      if (GET_CODE (operands[2]) != CONST_INT)
	emit_insn (gen_extend_insn (t5, operands[2], DImode, SImode, 0));
      else
	t5 = operands[2];
      emit_insn (gen_subdi3 (t3, t4, t5));
      emit_insn (gen_extend_insn (t6, operands[0], DImode, SImode, 0));

      riscv_expand_conditional_branch (operands[3], NE, t6, t3);
    }
  else
    {
      rtx t3 = gen_reg_rtx (<MODE>mode);
      rtx t4 = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_sub3_insn (operands[0], operands[1], operands[2]));

      rtx cmp1 = gen_rtx_LT (<MODE>mode, operands[2], const0_rtx);
      emit_insn (gen_cstore<mode>4 (t3, cmp1, operands[2], const0_rtx));

      rtx cmp2 = gen_rtx_LT (<MODE>mode, operands[1], operands[0]);
      emit_insn (gen_cstore<mode>4 (t4, cmp2, operands[1], operands[0]));

      riscv_expand_conditional_branch (operands[3], NE, t3, t4);
    }

  DONE;
})

(define_expand "usubv<mode>4"
  [(set (match_operand:GPR            0 "register_operand" "= r")
	(minus:GPR (match_operand:GPR 1 "reg_or_0_operand" " rJ")
		   (match_operand:GPR 2 "register_operand" "  r")))
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);

      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t3, operands[1], DImode, SImode, 0));
      else
	t3 = operands[1];
      riscv_emit_binary (MINUS, operands[0], operands[1], operands[2]);
      emit_insn (gen_extend_insn (t4, operands[0], DImode, SImode, 0));

      riscv_expand_conditional_branch (operands[3], LTU, t3, t4);
    }
  else
    {
      emit_insn (gen_sub3_insn (operands[0], operands[1], operands[2]));
      riscv_expand_conditional_branch (operands[3], LTU, operands[1],
				       operands[0]);
    }

  DONE;
})


(define_insn "subsi3_extended"
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
	  (match_operator:SI 3 "subreg_lowpart_operator"
	    [(minus:DI (match_operand:DI 1 "reg_or_0_operand" " rJ")
		       (match_operand:DI 2 "register_operand" "  r"))])))]
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

(define_insn "*negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" " r")))]
  ""
  "neg%~\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" " r")))]
  ""
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_negsi2_extended (t, operands[1]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "negsi2_extended"
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
	 (match_operator:SI 2 "subreg_lowpart_operator"
	   [(neg:DI (match_operand:DI 1 "register_operand" " r"))])))]
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
  "TARGET_HARD_FLOAT  || TARGET_ZFINX"
  "fmul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_ZMMUL || TARGET_MUL"
  "mul%~\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_expand "mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r")
       (mult:SI (match_operand:SI 1 "register_operand" " r")
                (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_ZMMUL || TARGET_MUL"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_mulsi3_extended (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "muldi3"
  [(set (match_operand:DI          0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "register_operand" " r")
		 (match_operand:DI 2 "register_operand" " r")))]
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
  "mul\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "mulv<mode>4"
  [(set (match_operand:GPR           0 "register_operand" "=r")
	(mult:GPR (match_operand:GPR 1 "register_operand" " r")
		  (match_operand:GPR 2 "register_operand" " r")))
   (label_ref (match_operand 3 "" ""))]
  "TARGET_ZMMUL || TARGET_MUL"
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);
      rtx t5 = gen_reg_rtx (DImode);
      rtx t6 = gen_reg_rtx (DImode);

      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t4, operands[1], DImode, SImode, 0));
      else
	t4 = operands[1];
      if (GET_CODE (operands[2]) != CONST_INT)
	emit_insn (gen_extend_insn (t5, operands[2], DImode, SImode, 0));
      else
	t5 = operands[2];
      emit_insn (gen_muldi3 (t3, t4, t5));

      emit_move_insn (operands[0], gen_lowpart (SImode, t3));
      emit_insn (gen_extend_insn (t6, operands[0], DImode, SImode, 0));

      riscv_expand_conditional_branch (operands[3], NE, t6, t3);
    }
  else
    {
      rtx hp = gen_reg_rtx (<MODE>mode);
      rtx lp = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_smul<mode>3_highpart (hp, operands[1], operands[2]));
      emit_insn (gen_mul<mode>3 (operands[0], operands[1], operands[2]));
      riscv_emit_binary (ASHIFTRT, lp, operands[0],
			 GEN_INT (BITS_PER_WORD - 1));

      riscv_expand_conditional_branch (operands[3], NE, hp, lp);
    }

  DONE;
})

(define_expand "umulv<mode>4"
  [(set (match_operand:GPR           0 "register_operand" "=r")
	(mult:GPR (match_operand:GPR 1 "register_operand" " r")
		  (match_operand:GPR 2 "register_operand" " r")))
   (label_ref (match_operand 3 "" ""))]
  "TARGET_ZMMUL || TARGET_MUL"
{
  if (TARGET_64BIT && <MODE>mode == SImode)
    {
      rtx t3 = gen_reg_rtx (DImode);
      rtx t4 = gen_reg_rtx (DImode);
      rtx t5 = gen_reg_rtx (DImode);
      rtx t6 = gen_reg_rtx (DImode);
      rtx t7 = gen_reg_rtx (DImode);
      rtx t8 = gen_reg_rtx (DImode);

      if (GET_CODE (operands[1]) != CONST_INT)
	emit_insn (gen_extend_insn (t3, operands[1], DImode, SImode, 0));
      else
	t3 = operands[1];
      if (GET_CODE (operands[2]) != CONST_INT)
	emit_insn (gen_extend_insn (t4, operands[2], DImode, SImode, 0));
      else
	t4 = operands[2];

      emit_insn (gen_ashldi3 (t5, t3, GEN_INT (32)));
      emit_insn (gen_ashldi3 (t6, t4, GEN_INT (32)));
      emit_insn (gen_umuldi3_highpart (t7, t5, t6));
      emit_move_insn (operands[0], gen_lowpart (SImode, t7));
      emit_insn (gen_lshrdi3 (t8, t7, GEN_INT (32)));

      riscv_expand_conditional_branch (operands[3], NE, t8, const0_rtx);
    }
  else
    {
      rtx hp = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_umul<mode>3_highpart (hp, operands[1], operands[2]));
      emit_insn (gen_mul<mode>3 (operands[0], operands[1], operands[2]));

      riscv_expand_conditional_branch (operands[3], NE, hp, const0_rtx);
    }

  DONE;
})

(define_insn "mulsi3_extended"
  [(set (match_operand:DI              0 "register_operand" "=r")
	(sign_extend:DI
	    (mult:SI (match_operand:SI 1 "register_operand" " r")
		     (match_operand:SI 2 "register_operand" " r"))))]
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
  "mulw\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "*mulsi3_extended2"
  [(set (match_operand:DI                       0 "register_operand" "=r")
	(sign_extend:DI
	  (match_operator:SI 3 "subreg_lowpart_operator"
	    [(mult:DI (match_operand:DI 1 "register_operand" " r")
		      (match_operand:DI 2 "register_operand" " r"))])))]
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
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
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
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
  [(set (match_operand:DI                0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (any_extend:TI
		       (match_operand:DI 1 "register_operand" " r"))
		     (any_extend:TI
		       (match_operand:DI 2 "register_operand" " r")))
	    (const_int 64))))]
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "usmulditi3"
  [(set (match_operand:TI                          0 "register_operand")
	(mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand"))
		 (sign_extend:TI (match_operand:DI 2 "register_operand"))))]
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
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
  "(TARGET_ZMMUL || TARGET_MUL) && TARGET_64BIT"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")])

(define_expand "<u>mulsidi3"
  [(set (match_operand:DI            0 "register_operand" "=r")
	(mult:DI (any_extend:DI
		   (match_operand:SI 1 "register_operand" " r"))
		 (any_extend:DI
		   (match_operand:SI 2 "register_operand" " r"))))]
  "(TARGET_ZMMUL || TARGET_MUL) && !TARGET_64BIT"
{
  rtx temp = gen_reg_rtx (SImode);
  riscv_emit_binary (MULT, temp, operands[1], operands[2]);
  emit_insn (gen_<su>mulsi3_highpart (riscv_subword (operands[0], true),
				     operands[1], operands[2]));
  emit_insn (gen_movsi (riscv_subword (operands[0], false), temp));
  DONE;
})

(define_insn "<su>mulsi3_highpart"
  [(set (match_operand:SI                0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (any_extend:DI
		       (match_operand:SI 1 "register_operand" " r"))
		     (any_extend:DI
		       (match_operand:SI 2 "register_operand" " r")))
	    (const_int 32))))]
  "(TARGET_ZMMUL || TARGET_MUL) && !TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])


(define_expand "usmulsidi3"
  [(set (match_operand:DI            0 "register_operand" "=r")
	(mult:DI (zero_extend:DI
		   (match_operand:SI 1 "register_operand" " r"))
		 (sign_extend:DI
		   (match_operand:SI 2 "register_operand" " r"))))]
  "(TARGET_ZMMUL || TARGET_MUL) && !TARGET_64BIT"
{
  rtx temp = gen_reg_rtx (SImode);
  riscv_emit_binary (MULT, temp, operands[1], operands[2]);
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
  "(TARGET_ZMMUL || TARGET_MUL) && !TARGET_64BIT"
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

(define_insn "*<optab>si3"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(any_div:SI (match_operand:SI 1 "register_operand" " r")
		    (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_DIV"
  "<insn>%i2%~\t%0,%1,%2"
  [(set_attr "type" "idiv")
   (set_attr "mode" "SI")])

(define_expand "<optab>si3"
  [(set (match_operand:SI             0 "register_operand" "=r")
       (any_div:SI (match_operand:SI 1 "register_operand" " r")
                   (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_DIV"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_<optab>si3_extended (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "<optab>di3"
  [(set (match_operand:DI             0 "register_operand" "=r")
	(any_div:DI (match_operand:DI 1 "register_operand" " r")
		    (match_operand:DI 2 "register_operand" " r")))]
  "TARGET_DIV && TARGET_64BIT"
  "<insn>%i2\t%0,%1,%2"
  [(set_attr "type" "idiv")
   (set_attr "mode" "DI")])

(define_expand "<u>divmod<mode>4"
  [(parallel
     [(set (match_operand:GPR 0 "register_operand")
           (only_div:GPR (match_operand:GPR 1 "register_operand")
                         (match_operand:GPR 2 "register_operand")))
      (set (match_operand:GPR 3 "register_operand")
           (<paired_mod>:GPR (match_dup 1) (match_dup 2)))])]
  "TARGET_DIV && riscv_use_divmod_expander ()"
  {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_<u>div<GPR:mode>3 (operands[0], operands[1], operands[2]));
      emit_insn (gen_mul<GPR:mode>3 (tmp, operands[0], operands[2]));
      emit_insn (gen_sub<GPR:mode>3 (operands[3], operands[1], tmp));
      DONE;
  })

(define_insn "<optab>si3_extended"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && TARGET_FDIV"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && TARGET_FDIV"
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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;; a * b - c
(define_insn "fms<mode>4"
  [(set (match_operand:ANYF                     0 "register_operand" "=f")
	(fma:ANYF (match_operand:ANYF           1 "register_operand" " f")
		  (match_operand:ANYF           2 "register_operand" " f")
		  (neg:ANYF (match_operand:ANYF 3 "register_operand" " f"))))]
  "TARGET_HARD_FLOAT  || TARGET_ZFINX"
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
  "TARGET_HARD_FLOAT  || TARGET_ZFINX"
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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SIGNED_ZEROS (<MODE>mode)"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SIGNED_ZEROS (<MODE>mode)"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SIGNED_ZEROS (<MODE>mode)"
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
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SIGNED_ZEROS (<MODE>mode)"
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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fabs.<fmt>\t%0,%1"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "copysign<mode>3"
  [(set (match_operand:ANYF 0 "register_operand"               "=f")
	(unspec:ANYF [(match_operand:ANYF 1 "register_operand" " f")
		      (match_operand:ANYF 2 "register_operand" " f")]
		     UNSPEC_COPYSIGN))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fsgnj.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF           0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fneg.<fmt>\t%0,%1"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	MIN/MAX
;;
;;  ....................

(define_insn "fminm<mode>3"
  [(set (match_operand:ANYF                    0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" " f"))
		      (use (match_operand:ANYF 2 "register_operand" " f"))]
		     UNSPEC_FMINM))]
  "TARGET_HARD_FLOAT && TARGET_ZFA"
  "fminm.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "fmaxm<mode>3"
  [(set (match_operand:ANYF                    0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" " f"))
		      (use (match_operand:ANYF 2 "register_operand" " f"))]
		     UNSPEC_FMAXM))]
  "TARGET_HARD_FLOAT && TARGET_ZFA"
  "fmaxm.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "fmin<mode>3"
  [(set (match_operand:ANYF                    0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" " f"))
		      (use (match_operand:ANYF 2 "register_operand" " f"))]
		     UNSPEC_FMIN))]
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SNANS (<MODE>mode)"
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "fmax<mode>3"
  [(set (match_operand:ANYF                    0 "register_operand" "=f")
	(unspec:ANYF [(use (match_operand:ANYF 1 "register_operand" " f"))
		      (use (match_operand:ANYF 2 "register_operand" " f"))]
		     UNSPEC_FMAX))]
  "(TARGET_HARD_FLOAT || TARGET_ZFINX) && !HONOR_SNANS (<MODE>mode)"
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(smin:ANYF (match_operand:ANYF 1 "register_operand" " f")
		   (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:ANYF            0 "register_operand" "=f")
	(smax:ANYF (match_operand:ANYF 1 "register_operand" " f")
		   (match_operand:ANYF 2 "register_operand" " f")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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

(define_expand "and<mode>3"
  [(set (match_operand:X                0 "register_operand")
        (and:X (match_operand:X 1 "register_operand")
	       (match_operand:X 2 "arith_or_mode_mask_or_zbs_operand")))]
  ""
{
  /* If the second operand is a mode mask, emit an extension
     insn instead.  */
  if (CONST_INT_P (operands[2]))
    {
      enum machine_mode tmode = VOIDmode;
      if (UINTVAL (operands[2]) == GET_MODE_MASK (HImode))
	tmode = HImode;
      else if (UINTVAL (operands[2]) == GET_MODE_MASK (SImode))
	tmode = SImode;

      if (tmode != VOIDmode)
	{
	  rtx tmp = gen_lowpart (tmode, operands[1]);
	  emit_insn (gen_extend_insn (operands[0], tmp, <MODE>mode, tmode, 1));
	  DONE;
	}
    }
})

(define_insn "*and<mode>3"
  [(set (match_operand:X                0 "register_operand" "=r,r")
	(and:X (match_operand:X 1 "register_operand" "%r,r")
		       (match_operand:X 2 "arith_operand"    " r,I")))]
  ""
  "and%i2\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

;; When we construct constants we may want to twiddle a single bit
;; by generating an IOR.  But the constant likely doesn't fit
;; arith_operand.  So the generic code will reload the constant into
;; a register.  Post-reload we won't have the chance to squash things
;; back into a Zbs insn.
;;
;; So indirect through a define_expand.  That allows us to have a
;; predicate that conditionally accepts single bit constants without
;; putting the details of Zbs instructions in here.
(define_expand "<optab><mode>3"
  [(set (match_operand:X 0 "register_operand")
	(any_or:X (match_operand:X 1 "register_operand" "")
		   (match_operand:X 2 "arith_or_zbs_operand" "")))]
  "")

(define_insn "*<optab><mode>3"
  [(set (match_operand:X                0 "register_operand" "=r,r")
	(any_or:X (match_operand:X 1 "register_operand" "%r,r")
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
  "TARGET_DOUBLE_FLOAT || TARGET_ZDINX"
  "fcvt.s.d\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

(define_insn "truncsfhf2"
  [(set (match_operand:HF     0 "register_operand" "=f")
       (float_truncate:HF
           (match_operand:SF 1 "register_operand" " f")))]
  "TARGET_ZFHMIN || TARGET_ZHINXMIN"
  "fcvt.h.s\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "HF")])

(define_insn "truncdfhf2"
  [(set (match_operand:HF     0 "register_operand" "=f")
       (float_truncate:HF
           (match_operand:DF 1 "register_operand" " f")))]
  "(TARGET_ZFHMIN && TARGET_DOUBLE_FLOAT) ||
   (TARGET_ZHINXMIN && TARGET_ZDINX)"
  "fcvt.h.d\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "HF")])

(define_insn "truncsfbf2"
  [(set (match_operand:BF    0 "register_operand" "=f")
	(float_truncate:BF
	   (match_operand:SF 1 "register_operand" " f")))]
  "TARGET_ZFBFMIN"
  "fcvt.bf16.s\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "BF")])

;; The conversion of HF/DF/TF to BF needs to be done with SF if there is a
;; chance to generate at least one instruction, otherwise just using
;; libfunc __trunc[h|d|t]fbf2.
(define_expand "trunc<mode>bf2"
  [(set (match_operand:BF	0 "register_operand" "=f")
	(float_truncate:BF
	   (match_operand:FBF	1 "register_operand" " f")))]
  "TARGET_ZFBFMIN"
  {
    convert_move (operands[0],
		  convert_modes (SFmode, <MODE>mode, operands[1], 0), 0);
    DONE;
  }
  [(set_attr "type" "fcvt")
   (set_attr "mode" "BF")])

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
  "TARGET_64BIT"
{
  if (SUBREG_P (operands[1]) && SUBREG_PROMOTED_VAR_P (operands[1])
      && SUBREG_PROMOTED_UNSIGNED_P (operands[1]))
    {
      emit_insn (gen_movdi (operands[0], SUBREG_REG (operands[1])));
      DONE;
    }
})

(define_insn_and_split "*zero_extendsidi2_internal"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(zero_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT && !TARGET_ZBA && !TARGET_XTHEADBB && !TARGET_XTHEADMEMIDX
   && !(REG_P (operands[1]) && VL_REG_P (REGNO (operands[1])))"
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
   (set_attr "type" "load")
   (set_attr "mode" "DI")])

(define_expand "zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand")
	(zero_extend:GPR
	    (match_operand:HI 1 "nonimmediate_operand")))]
  "")

(define_insn_and_split "*zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand"     "=r,r")
	(zero_extend:GPR
	    (match_operand:HI 1 "nonimmediate_operand" " r,m")))]
  "!TARGET_ZBB && !TARGET_XTHEADBB && !TARGET_XTHEADMEMIDX"
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
   (set_attr "type" "load")
   (set_attr "mode" "<GPR:MODE>")])

(define_expand "zero_extendqi<SUPERQI:mode>2"
  [(set (match_operand:SUPERQI    0 "register_operand")
	(zero_extend:SUPERQI
	    (match_operand:QI 1 "nonimmediate_operand")))]
  "")

(define_insn "*zero_extendqi<SUPERQI:mode>2_internal"
  [(set (match_operand:SUPERQI 0 "register_operand"    "=r,r")
	(zero_extend:SUPERQI
	    (match_operand:QI 1 "nonimmediate_operand" " r,m")))]
  "!TARGET_XTHEADMEMIDX"
  "@
   andi\t%0,%1,0xff
   lbu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "type" "arith,load")
   (set_attr "mode" "<SUPERQI:MODE>")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

(define_expand "extendsidi2"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(sign_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT"
{
  if (SUBREG_P (operands[1]) && SUBREG_PROMOTED_VAR_P (operands[1])
      && SUBREG_PROMOTED_SIGNED_P (operands[1]))
    {
      emit_insn (gen_movdi (operands[0], SUBREG_REG (operands[1])));
      DONE;
    }
})

(define_insn "*extendsidi2_internal"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(sign_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT && !TARGET_XTHEADMEMIDX"
  "@
   sext.w\t%0,%1
   lw\t%0,%1"
  [(set_attr "move_type" "move,load")
   (set_attr "type" "move,load")
   (set_attr "mode" "DI")])

(define_expand "extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI 0 "register_operand")
	(sign_extend:SUPERQI (match_operand:SHORT 1 "nonimmediate_operand")))]
  "")

(define_insn_and_split "*extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI   0 "register_operand"     "=r,r")
	(sign_extend:SUPERQI
	    (match_operand:SHORT 1 "nonimmediate_operand" " r,m")))]
  "!TARGET_ZBB && !TARGET_XTHEADBB && !TARGET_XTHEADMEMIDX"
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
   (set_attr "type" "load")
   (set_attr "mode" "SI")])

(define_insn "extendhfsf2"
  [(set (match_operand:SF     0 "register_operand" "=f")
       (float_extend:SF
           (match_operand:HF 1 "register_operand" " f")))]
  "TARGET_ZFHMIN || TARGET_ZHINXMIN"
  "fcvt.s.h\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

(define_insn "extendbfsf2"
  [(set (match_operand:SF    0 "register_operand" "=f")
	(float_extend:SF
	   (match_operand:BF 1 "register_operand" " f")))]
  "TARGET_ZFBFMIN"
  "fcvt.s.bf16\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF     0 "register_operand" "=f")
	(float_extend:DF
	    (match_operand:SF 1 "register_operand" " f")))]
  "TARGET_DOUBLE_FLOAT || TARGET_ZDINX"
  "fcvt.d.s\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "DF")])

(define_insn "extendhfdf2"
  [(set (match_operand:DF     0 "register_operand" "=f")
       (float_extend:DF
           (match_operand:HF 1 "register_operand" " f")))]
  "(TARGET_ZFHMIN && TARGET_DOUBLE_FLOAT) ||
   (TARGET_ZHINXMIN && TARGET_ZDINX)"
  "fcvt.d.h\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "DF")])

;; 16-bit floating point moves
(define_expand "mov<mode>"
  [(set (match_operand:HFBF 0 "")
	(match_operand:HFBF 1 ""))]
  ""
{
  if (riscv_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_insn "*mov<mode>_hardfloat"
  [(set (match_operand:HFBF 0 "nonimmediate_operand" "=f,   f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:HFBF 1 "move_operand"	     " f,zfli,G,m,f,G,*r,*f,*G*r,*m,*r"))]
  "((TARGET_ZFHMIN && <MODE>mode == HFmode)
    || (TARGET_ZFBFMIN && <MODE>mode == BFmode))
   && (register_operand (operands[0], <MODE>mode)
       || reg_or_0_operand (operands[1], <MODE>mode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "<MODE>")])

(define_insn "*mov<mode>_softfloat"
  [(set (match_operand:HFBF 0 "nonimmediate_operand" "=f, r,r,m,*f,*r")
	(match_operand:HFBF 1 "move_operand"	     " f,Gr,m,r,*r,*f"))]
  "((!TARGET_ZFHMIN && <MODE>mode == HFmode) || (<MODE>mode == BFmode))
   && (register_operand (operands[0], <MODE>mode)
       || reg_or_0_operand (operands[1], <MODE>mode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,move,load,store,mtc,mfc")
   (set_attr "type" "fmove,move,load,store,mtc,mfc")
   (set_attr "mode" "<MODE>")])

(define_insn "*mov<HFBF:mode>_softfloat_boxing"
  [(set (match_operand:HFBF 0 "register_operand"	    "=f")
	(unspec:HFBF [(match_operand:X 1 "register_operand" " r")]
	 UNSPEC_FMV_FP16_X))]
  "!TARGET_ZFHMIN"
  "fmv.w.x\t%0,%1"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_expand "<fix_uns>_trunc<ANYF:mode>si2"
  [(set (match_operand:SI      0 "register_operand" "=r")
	(fix_ops:SI
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_<fix_uns>_trunc<ANYF:mode>si2_sext (t, operands[1]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "*<fix_uns>_trunc<ANYF:mode>si2"
  [(set (match_operand:SI      0 "register_operand" "=r")
	(fix_ops:SI
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.w<u>.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "<fix_uns>_trunc<ANYF:mode>si2_sext"
  [(set (match_operand:DI      0 "register_operand" "=r")
  (sign_extend:DI (fix_ops:SI
	    (match_operand:ANYF 1 "register_operand" " f"))))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.w<u>.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "<fix_uns>_trunc<ANYF:mode>di2"
  [(set (match_operand:DI      0 "register_operand" "=r")
	(fix_ops:DI
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.l<u>.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "float<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>\t%0,%z1"
  [(set_attr "type" "fcvt_i2f")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "floatuns<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(unsigned_float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>u\t%0,%z1"
  [(set_attr "type" "fcvt_i2f")
   (set_attr "mode" "<ANYF:MODE>")])

(define_expand "lrint<ANYF:mode>si2"
  [(set (match_operand:SI       0 "register_operand" "=r")
	(unspec:SI
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    UNSPEC_LRINT))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_lrint<ANYF:mode>si2_sext (t, operands[1]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "*lrint<ANYF:mode>si2"
  [(set (match_operand:SI       0 "register_operand" "=r")
	(unspec:SI
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    UNSPEC_LRINT))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.w.<ANYF:fmt> %0,%1,dyn"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "lrint<ANYF:mode>si2_sext"
  [(set (match_operand:DI       0 "register_operand" "=r")
  (sign_extend:DI (unspec:SI
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    UNSPEC_LRINT)))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.w.<ANYF:fmt> %0,%1,dyn"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "lrint<ANYF:mode>di2"
  [(set (match_operand:DI       0 "register_operand" "=r")
	(unspec:DI
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    UNSPEC_LRINT))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.l.<ANYF:fmt> %0,%1,dyn"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_expand "l<round_pattern><ANYF:mode>si2"
  [(set (match_operand:SI       0 "register_operand" "=r")
	(unspec:SI
	    [(match_operand:ANYF 1 "register_operand" " f")]
    ROUND))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_l<round_pattern><ANYF:mode>si2_sext (t, operands[1]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "*l<round_pattern><ANYF:mode>si2"
  [(set (match_operand:SI       0 "register_operand" "=r")
	(unspec:SI
	    [(match_operand:ANYF 1 "register_operand" " f")]
    ROUND))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.w.<ANYF:fmt> %0,%1,<round_rm>"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "l<round_pattern><ANYF:mode>si2_sext"
  [(set (match_operand:DI       0 "register_operand" "=r")
	 (sign_extend:DI (unspec:SI
			     [(match_operand:ANYF 1 "register_operand" " f")]
		      ROUND)))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.w.<ANYF:fmt> %0,%1,<round_rm>"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "l<round_pattern><ANYF:mode>di2"
  [(set (match_operand:DI       0 "register_operand" "=r")
	(unspec:DI
	    [(match_operand:ANYF 1 "register_operand" " f")]
    ROUND))]
  "TARGET_64BIT && (TARGET_HARD_FLOAT || TARGET_ZFINX)"
  "fcvt.l.<ANYF:fmt> %0,%1,<round_rm>"
  [(set_attr "type" "fcvt_f2i")
   (set_attr "mode" "<ANYF:MODE>")])

;; There are a couple non-obvious restrictions to be aware of.
;;
;; We'll do a FP-INT conversion in the sequence.  But we don't
;; have a .l (64bit) variant of those instructions for rv32.
;; To preserve proper semantics we must reject DFmode inputs
;; for rv32 unless Zfa is enabled.
;;
;; The ANYF iterator allows HFmode.  We don't have all the
;; necessary patterns defined for HFmode.  So restrict HFmode
;; to TARGET_ZFA.
(define_expand "<round_pattern><ANYF:mode>2"
  [(set (match_operand:ANYF     0 "register_operand" "=f")
	(unspec:ANYF
	    [(match_operand:ANYF 1 "register_operand" " f")]
	ROUND))]
  "(TARGET_HARD_FLOAT
    && (TARGET_ZFA || flag_fp_int_builtin_inexact || !flag_trapping_math)
    && (TARGET_ZFA || TARGET_64BIT || <ANYF:MODE>mode != DFmode)
    && (TARGET_ZFA || <ANYF:MODE>mode != HFmode))"
{
  if (TARGET_ZFA)
    emit_insn (gen_<round_pattern><ANYF:mode>_zfa2 (operands[0],
                                                    operands[1]));
  else
    {
      rtx reg;
      rtx label = gen_label_rtx ();
      rtx end_label = gen_label_rtx ();
      rtx abs_reg = gen_reg_rtx (<ANYF:MODE>mode);
      rtx coeff_reg = gen_reg_rtx (<ANYF:MODE>mode);
      rtx tmp_reg = gen_reg_rtx (<ANYF:MODE>mode);

      riscv_emit_move (tmp_reg, operands[1]);
      riscv_emit_move (coeff_reg,
		       riscv_vector::get_fp_rounding_coefficient (<ANYF:MODE>mode));
      emit_insn (gen_abs<ANYF:mode>2 (abs_reg, operands[1]));

      riscv_expand_conditional_branch (label, LT, abs_reg, coeff_reg);

      emit_jump_insn (gen_jump (end_label));
      emit_barrier ();

      emit_label (label);
      switch (<ANYF:MODE>mode)
	{
	case SFmode:
	  reg = gen_reg_rtx (SImode);
	  emit_insn (gen_l<round_pattern>sfsi2 (reg, operands[1]));
	  emit_insn (gen_floatsisf2 (abs_reg, reg));
	  break;
	case DFmode:
	  reg = gen_reg_rtx (DImode);
	  emit_insn (gen_l<round_pattern>dfdi2 (reg, operands[1]));
	  emit_insn (gen_floatdidf2 (abs_reg, reg));
	  break;
	default:
	  gcc_unreachable ();
	}

      emit_insn (gen_copysign<ANYF:mode>3 (tmp_reg, abs_reg, operands[1]));

      emit_label (end_label);
      riscv_emit_move (operands[0], tmp_reg);
    }

  DONE;
})

(define_insn "<round_pattern><ANYF:mode>_zfa2"
  [(set (match_operand:ANYF     0 "register_operand" "=f")
	(unspec:ANYF
	    [(match_operand:ANYF 1 "register_operand" " f")]
	ROUND))]
  "TARGET_HARD_FLOAT && TARGET_ZFA"
  "fround.<ANYF:fmt>\t%0,%1,<round_rm>"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "rint<ANYF:mode>2"
  [(set (match_operand:ANYF     0 "register_operand" "=f")
	(unspec:ANYF
	    [(match_operand:ANYF 1 "register_operand" " f")]
	UNSPEC_RINT))]
  "TARGET_HARD_FLOAT && TARGET_ZFA"
  "froundnx.<ANYF:fmt>\t%0,%1"
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
    (set_attr "type" "load")
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
   (set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "got_load_tls_ie<mode>"
  [(set (match_operand:P      0 "register_operand" "=r")
	(unspec:P
	    [(match_operand:P 1 "symbolic_operand" "")]
	    UNSPEC_TLS_IE))]
  ""
  "la.tls.ie\t%0,%1"
  [(set_attr "got" "load")
   (set_attr "type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "@tlsdesc<mode>"
  [(set (reg:P A0_REGNUM)
	(unspec:P
	    [(match_operand:P 0 "symbolic_operand" "")]
	    UNSPEC_TLSDESC))
   (clobber (reg:P T0_REGNUM))]
  "TARGET_TLSDESC"
  {
    return ".LT%=: auipc\ta0,%%tlsdesc_hi(%0)\;"
           "<load>\tt0,%%tlsdesc_load_lo(.LT%=)(a0)\;"
           "addi\ta0,a0,%%tlsdesc_add_lo(.LT%=)\;"
           "jalr\tt0,t0,%%tlsdesc_call(.LT%=)";
  }
  [(set_attr "type" "multi")
   (set_attr "length" "16")
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
		      <GPR:MODE>mode);
  DONE;
})

;; Likewise, for symbolic operands.
(define_split
  [(set (match_operand:P 0 "register_operand")
	(match_operand:P 1))
   (clobber (match_operand:P 2 "register_operand"))]
  "riscv_split_symbol (operands[2], operands[1], MAX_MACHINE_MODE, NULL)"
  [(set (match_dup 0) (match_dup 3))]
{
  riscv_split_symbol (operands[2], operands[1],
		      MAX_MACHINE_MODE, &operands[3]);
})

;; Pretend to have the ability to load complex const_int in order to get
;; better code generation around them.
;; But avoid constants that are special cased elsewhere.
;;
;; Hide it from IRA register equiv recog* () to elide potential undoing of split
;;
(define_insn_and_split "*mvconst_internal"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (match_operand:GPR 1 "splittable_const_int_operand" "i"))]
  "!ira_in_progress
   && !(p2m1_shift_operand (operands[1], <MODE>mode)
        || high_mask_shift_operand (operands[1], <MODE>mode))"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_move_integer (operands[0], operands[0], INTVAL (operands[1]),
                      <MODE>mode);
  DONE;
}
[(set_attr "type" "move")])

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
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,m,  *f,*f,*r,*f,*m,r")
	(match_operand:DI 1 "move_operand"         " r,i,m,r,*J*r,*m,*f,*f,*f,vp"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fmove,fpstore,rdvlenb")
   (set_attr "mode" "DI")
   (set_attr "type" "move,move,load,store,move,fpload,move,fmove,fpstore,move")
   (set_attr "ext" "base,base,base,base,d,d,d,d,d,vector")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*f,*r,*f,*m,r")
	(match_operand:DI 1 "move_operand"         " r,T,m,rJ,*r*J,*m,*f,*f,*f,vp"))]
  "TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fmove,fpstore,rdvlenb")
   (set_attr "mode" "DI")
   (set_attr "type" "move,move,load,store,mtc,fpload,mfc,fmove,fpstore,move")
   (set_attr "ext" "base,base,base,base,d,d,d,d,d,vector")])

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
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*f,*r,*m,r")
	(match_operand:SI 1 "move_operand"         " r,T,m,rJ,*r*J,*m,*f,*f,vp"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))
    && !(REG_P (operands[1]) && VL_REG_P (REGNO (operands[1])))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fpstore,rdvlenb")
   (set_attr "mode" "SI")
   (set_attr "type" "move,move,load,store,mtc,fpload,mfc,fpstore,move")
   (set_attr "ext" "base,base,base,base,f,f,f,f,vector")])

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
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*r,r")
	(match_operand:HI 1 "move_operand"	   " r,T,m,rJ,*r*J,*f,vp"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,mfc,rdvlenb")
   (set_attr "mode" "HI")
   (set_attr "type" "move,move,load,store,mtc,mfc,move")
   (set_attr "ext" "base,base,base,base,f,f,vector")])

;; HImode constant generation; see riscv_move_integer for details.
;; si+si->hi without truncation is legal because of
;; TARGET_TRULY_NOOP_TRUNCATION.

(define_insn "*add<mode>hi3"
  [(set (match_operand:HI            0 "register_operand" "=r,r")
	(plus:HI (match_operand:HISI 1 "register_operand" " r,r")
		 (match_operand:HISI 2 "arith_operand"    " r,I")))]
  ""
  "add%i2%~\t%0,%1,%2"
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
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r, m,  *f,*r,r")
	(match_operand:QI 1 "move_operand"         " r,I,m,rJ,*r*J,*f,vp"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,mfc,rdvlenb")
   (set_attr "mode" "QI")
   (set_attr "type" "move,move,load,store,mtc,mfc,move")
   (set_attr "ext" "base,base,base,base,f,f,vector")])

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
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,   f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:SF 1 "move_operand"         " f,zfli,G,m,f,G,*r,*f,*G*r,*m,*r"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "= r,r,m")
	(match_operand:SF 1 "move_operand"         " Gr,m,r"))]
  "!TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "type" "move,load,store")
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
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,   f,f,f,m,m,*zmvf,*zmvr,  *r,*r,*th_m_noi")
	(match_operand:DF 1 "move_operand"         " f,zfli,G,m,f,G,*zmvr,*zmvf,*r*G,*th_m_noi,*r"))]
  "!TARGET_64BIT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_hardfloat_rv64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,   f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:DF 1 "move_operand"         " f,zfli,G,m,f,G,*r,*f,*r*G,*m,*r"))]
  "TARGET_64BIT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "type" "fmove,fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "= r,r, m")
	(match_operand:DF 1 "move_operand"         " rG,m,rG"))]
  "!TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "type" "fmove,fpload,fpstore")
   (set_attr "mode" "DF")])

(define_insn "movsidf2_low_rv32"
  [(set (match_operand:SI      0 "register_operand" "=  r")
	(unspec:SI
	    [(match_operand:DF 1 "register_operand" "zmvf")]
	UNSPEC_FMV_X_W))]
  "TARGET_HARD_FLOAT && !TARGET_64BIT && TARGET_ZFA"
  "fmv.x.w\t%0,%1"
  [(set_attr "move_type" "fmove")
   (set_attr "type" "fmove")
   (set_attr "mode" "DF")])


(define_insn "movsidf2_high_rv32"
  [(set (match_operand:SI      0 "register_operand" "=  r")
	(unspec:SI
	    [(match_operand:DF 1 "register_operand" "zmvf")]
	UNSPEC_FMVH_X_D))]
  "TARGET_HARD_FLOAT && !TARGET_64BIT && TARGET_ZFA"
  "fmvh.x.d\t%0,%1"
  [(set_attr "move_type" "fmove")
   (set_attr "type" "fmove")
   (set_attr "mode" "DF")])

(define_insn "movdfsisi3_rv32"
  [(set (match_operand:DF      0 "register_operand"    "=  f")
	(plus:DF
            (match_operand:SI 2 "register_operand"     "zmvr")
            (ashift:SI
                (match_operand:SI 1 "register_operand" "zmvr")
                (const_int 32))))]
  "TARGET_HARD_FLOAT && !TARGET_64BIT && TARGET_ZFA"
  "fmvp.d.x\t%0,%2,%1"
  [(set_attr "move_type" "fmove")
   (set_attr "type" "fmove")
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

(define_expand "cmpmemsi"
  [(parallel [(set (match_operand:SI 0)
               (compare:SI (match_operand:BLK 1)
                           (match_operand:BLK 2)))
	      (use (match_operand:SI 3))
	      (use (match_operand:SI 4))])]
  "!optimize_size"
{
  /* If TARGET_VECTOR is false, this routine will return false and we will
     try scalar expansion.  */
  if (riscv_vector::expand_vec_cmpmem (operands[0], operands[1],
				       operands[2], operands[3]))
    DONE;

  rtx temp = gen_reg_rtx (word_mode);
  if (riscv_expand_block_compare (temp, operands[1], operands[2],
                                  operands[3]))
    {
      if (TARGET_64BIT)
	{
	  temp = gen_lowpart (SImode, temp);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_SET (temp, SRP_SIGNED);
	}
      emit_move_insn (operands[0], temp);
      DONE;
    }
  else
    FAIL;
})

(define_expand "cpymem<mode>"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:P 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  ""
{
  if (riscv_expand_block_move (operands[0], operands[1], operands[2]))
    DONE;
  else
    FAIL;
})

;; Fill memory with constant byte.
;; Argument 0 is the destination
;; Argument 1 is the constant byte
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "setmem<mode>"
  [(parallel [(set (match_operand:BLK 0 "memory_operand")
		   (match_operand:QI 2 "nonmemory_operand"))
	      (use (match_operand:P 1 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
 ""
{
  /* If TARGET_VECTOR is false, this routine will return false and we will
     try scalar expansion.  */
  if (riscv_vector::expand_vec_setmem (operands[0], operands[1], operands[2]))
    DONE;

  /* If value to set is not zero, use the library routine.  */
  if (operands[2] != const0_rtx)
    FAIL;

  if (riscv_expand_block_clear (operands[0], operands[1]))
    DONE;
  else
    FAIL;
})

(define_expand "movmem<mode>"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
   (match_operand:BLK 1 "general_operand"))
    (use (match_operand:P 2 "const_int_operand"))
    (use (match_operand:SI 3 "const_int_operand"))])]
  "TARGET_VECTOR"
{
  if (riscv_vector::expand_block_move (operands[0], operands[1], operands[2],
				       true))
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
  if (TARGET_ZIFENCEI)
    emit_insn (gen_fence_i ());
#endif
  DONE;
})

(define_insn "fence"
  [(unspec_volatile [(const_int 0)] UNSPECV_FENCE)]
  ""
  "%|fence%-"
  [(set_attr "type" "atomic")])

(define_insn "fence_i"
  [(unspec_volatile [(const_int 0)] UNSPECV_FENCE_I)]
  "TARGET_ZIFENCEI"
  "fence.i"
  [(set_attr "type" "atomic")])

(define_insn "riscv_pause"
  [(unspec_volatile [(const_int 0)] UNSPECV_PAUSE)]
  ""
  "* return TARGET_ZIHINTPAUSE ? \"pause\" : \".insn\t0x0100000f\";"
  [(set_attr "type" "atomic")])

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

(define_insn "*<optab>si3"
  [(set (match_operand:SI     0 "register_operand" "= r")
	(any_shift:SI
	    (match_operand:SI 1 "register_operand" "  r")
	    (match_operand:QI 2 "arith_operand"    " rI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (SImode) - 1));

  return "<insn>%i2%~\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_expand "<optab>si3"
  [(set (match_operand:SI     0 "register_operand" "= r")
       (any_shift:SI (match_operand:SI 1 "register_operand" "  r")
                (match_operand:QI 2 "arith_operand"    " rI")))]
  ""
{
  if (TARGET_64BIT)
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

(define_insn_and_split "*<optab><GPR:mode>3_mask_1"
  [(set (match_operand:GPR     0 "register_operand" "= r")
	(any_shift:GPR
	    (match_operand:GPR 1 "register_operand" "  r")
	    (match_operator 4 "subreg_lowpart_operator"
	     [(and:GPR2
	       (match_operand:GPR2 2 "register_operand"  "r")
	       (match_operand 3 "<GPR:shiftm1>"))])))]
  ""
  "#"
  "&& 1"
  [(set (match_dup 0)
	(any_shift:GPR (match_dup 1)
		      (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "<optab>si3_extend"
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
	     (match_operator 4 "subreg_lowpart_operator"
	      [(and:GPR
	        (match_operand:GPR 2 "register_operand" " r")
	        (match_operand 3 "const_si_mask_operand"))]))))]
  "TARGET_64BIT"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(sign_extend:DI
	 (any_shift:SI (match_dup 1)
		       (match_dup 2))))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;; We can reassociate the shift and bitwise operator which may allow us to
;; reduce the immediate operand of the bitwise operator into a range that
;; fits in a simm12.
;;
;; We need to make sure that shifting does not lose any bits, particularly
;; for IOR/XOR.  It probably doesn't matter for AND.
;;
;; We also don't want to do this if the immediate already fits in a simm12
;; field, or it is a single bit operand and zbs is available.
(define_insn_and_split "<optab>_shift_reverse<X:mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
    (any_bitwise:X (ashift:X (match_operand:X 1 "register_operand" "r")
			     (match_operand 2 "immediate_operand" "n"))
		   (match_operand 3 "immediate_operand" "n")))]
  "(!SMALL_OPERAND (INTVAL (operands[3]))
    && SMALL_OPERAND (INTVAL (operands[3]) >> INTVAL (operands[2]))
    && (!TARGET_ZBS || popcount_hwi (INTVAL (operands[3])) > 1)
    && (INTVAL (operands[3]) & ((1ULL << INTVAL (operands[2])) - 1)) == 0)"
  "#"
  "&& 1"
  [(set (match_dup 0) (any_bitwise:X (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (ashift:X (match_dup 0) (match_dup 2)))]
  {
    operands[3] = GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2]));
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<X:MODE>")])

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

;; Canonical form for a sign/zero-extend of a logical right shift.
;; Special case: extract MSB bits of lower 32-bit word
(define_insn "*lshrsi3_extend_2"
  [(set (match_operand:DI                   0 "register_operand" "=r")
	(any_extract:DI (match_operand:DI  1 "register_operand" " r")
			 (match_operand     2 "const_int_operand")
			 (match_operand     3 "const_int_operand")))]
  "(TARGET_64BIT && (INTVAL (operands[3]) > 0)
    && (INTVAL (operands[2]) + INTVAL (operands[3]) == 32))"
{
  return "<extract_sidi_shift>\t%0,%1,%3";
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

;; Canonical form for a extend of a logical shift right (sign/zero extraction).
;; Special cases, that are ignored (handled elsewhere):
;; * Single-bit extraction (Zbs/XTheadBs)
;; * Single-bit extraction (Zicondops/XVentanaCondops)
;; * Single-bit extraction (SFB)
;; * Extraction instruction th.ext(u) (XTheadBb)
;; * lshrsi3_extend_2 (see above)
(define_insn_and_split "*<any_extract:optab><GPR:mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	 (any_extract:GPR
       (match_operand:GPR 1 "register_operand" " r")
       (match_operand     2 "const_int_operand")
       (match_operand     3 "const_int_operand")))
   (clobber (match_scratch:GPR  4 "=&r"))]
  "!((TARGET_ZBS || TARGET_XTHEADBS || TARGET_ZICOND
      || TARGET_XVENTANACONDOPS || TARGET_SFB_ALU)
     && (INTVAL (operands[2]) == 1))
   && !TARGET_XTHEADBB
   && !(TARGET_64BIT
        && (INTVAL (operands[3]) > 0)
        && (INTVAL (operands[2]) + INTVAL (operands[3]) == 32))"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
     (ashift:GPR (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
     (<extract_shift>:GPR (match_dup 4) (match_dup 3)))]
{
  int regbits = GET_MODE_BITSIZE (GET_MODE (operands[0])).to_constant ();
  int sizebits = INTVAL (operands[2]);
  int startbits = INTVAL (operands[3]);
  int lshamt = regbits - sizebits - startbits;
  int rshamt = lshamt + startbits;
  operands[2] = GEN_INT (lshamt);
  operands[3] = GEN_INT (rshamt);
}
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

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
  operands[2] = GEN_INT (GET_MODE_BITSIZE (GET_MODE (operands[1])).to_constant ()
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

;; Handle SImode to DImode zero-extend combined with a left shift.  This can
;; occur when unsigned int is used for array indexing.  Split this into two
;; shifts.  Otherwise we can get 3 shifts.

(define_insn_and_split "zero_extendsidi2_shifted"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			   (match_operand:QI 2 "immediate_operand" "I"))
		(match_operand 3 "immediate_operand" "")))
   (clobber (match_scratch:DI 4 "=&r"))]
  "TARGET_64BIT && !TARGET_ZBA
   && ((INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff)"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
	(lshiftrt:DI (match_dup 4) (match_dup 5)))]
  "operands[5] = GEN_INT (32 - (INTVAL (operands [2])));"
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")])

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches

(define_insn_and_split "*branch<ANYI:mode>_shiftedarith_equals_zero"
  [(set (pc)
	(if_then_else (match_operator 1 "equality_operator"
		       [(and:ANYI (match_operand:ANYI 2 "register_operand" "r")
				  (match_operand 3 "shifted_const_arith_operand" "i"))
			(const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_scratch:X 4 "=&r"))]
  "!SMALL_OPERAND (INTVAL (operands[3]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 4) (lshiftrt:X (subreg:X (match_dup 2) 0) (match_dup 6)))
   (set (match_dup 4) (and:X (match_dup 4) (match_dup 7)))
   (set (pc) (if_then_else (match_op_dup 1 [(match_dup 4) (const_int 0)])
			   (label_ref (match_dup 0)) (pc)))]
{
	HOST_WIDE_INT mask = INTVAL (operands[3]);
	int trailing = ctz_hwi (mask);

	operands[6] = GEN_INT (trailing);
	operands[7] = GEN_INT (mask >> trailing);
}
[(set_attr "type" "branch")])

(define_insn_and_split "*branch<ANYI:mode>_shiftedarith_<optab>_shifted"
  [(set (pc)
	(if_then_else (any_eq
		    (and:ANYI (match_operand:ANYI 1 "register_operand" "r")
			  (match_operand 2 "shifted_const_arith_operand" "i"))
		    (match_operand 3 "shifted_const_arith_operand" "i"))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_scratch:X 4 "=&r"))
   (clobber (match_scratch:X 5 "=&r"))]
  "!SMALL_OPERAND (INTVAL (operands[2]))
    && !SMALL_OPERAND (INTVAL (operands[3]))
    && SMALL_AFTER_COMMON_TRAILING_SHIFT (INTVAL (operands[2]),
					     INTVAL (operands[3]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 4) (ashiftrt:X (match_dup 1) (match_dup 7)))
   (set (match_dup 4) (and:X (match_dup 4) (match_dup 8)))
   (set (match_dup 5) (match_dup 9))
   (set (pc) (if_then_else (any_eq (match_dup 4) (match_dup 5))
			   (label_ref (match_dup 0)) (pc)))]
{
  HOST_WIDE_INT mask1 = INTVAL (operands[2]);
  HOST_WIDE_INT mask2 = INTVAL (operands[3]);
  int trailing_shift = COMMON_TRAILING_ZEROS (mask1, mask2);

  operands[7] = GEN_INT (trailing_shift);
  operands[8] = GEN_INT (mask1 >> trailing_shift);
  operands[9] = GEN_INT (mask2 >> trailing_shift);
}
[(set_attr "type" "branch")])

(define_insn_and_split "*branch<ANYI:mode>_shiftedmask_equals_zero"
  [(set (pc)
	(if_then_else (match_operator 1 "equality_operator"
		       [(and:ANYI (match_operand:ANYI 2 "register_operand" "r")
				  (match_operand 3 "consecutive_bits_operand" "i"))
			(const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_scratch:X 4 "=&r"))]
  "(INTVAL (operands[3]) >= 0 || !partial_subreg_p (operands[2]))
    && popcount_hwi (INTVAL (operands[3])) > 1
    && !SMALL_OPERAND (INTVAL (operands[3]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 4) (ashift:X (subreg:X (match_dup 2) 0) (match_dup 6)))
   (set (match_dup 4) (lshiftrt:X (match_dup 4) (match_dup 7)))
   (set (pc) (if_then_else (match_op_dup 1 [(match_dup 4) (const_int 0)])
			   (label_ref (match_dup 0)) (pc)))]
{
	unsigned HOST_WIDE_INT mask = INTVAL (operands[3]);
	int leading  = clz_hwi (mask);
	int trailing = ctz_hwi (mask);

	operands[6] = GEN_INT (leading);
	operands[7] = GEN_INT (leading + trailing);
}
[(set_attr "type" "branch")])

(define_insn "*branch<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "ordered_comparison_operator"
			 [(match_operand:X 2 "register_operand" "r")
			  (match_operand:X 3 "reg_or_0_operand" "rJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "!TARGET_XCVBI"
{
  if (get_attr_length (insn) == 12)
    return "b%n1\t%2,%z3,1f; jump\t%l0,ra; 1:";

  return "b%C1\t%2,%z3,%l0";
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

;; Conditional move and add patterns.

(define_expand "mov<mode>cc"
  [(set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_operand 1 "comparison_operator")
			  (match_operand:GPR 2 "movcc_operand")
			  (match_operand:GPR 3 "movcc_operand")))]
  "TARGET_SFB_ALU || TARGET_XTHEADCONDMOV || TARGET_ZICOND_LIKE
   || TARGET_MOVCC"
{
  if (riscv_expand_conditional_move (operands[0], operands[1],
				     operands[2], operands[3]))
    DONE;
  else
    FAIL;
})

(define_expand "add<mode>cc"
  [(match_operand:GPR 0 "register_operand")
   (match_operand     1 "comparison_operator")
   (match_operand:GPR 2 "arith_operand")
   (match_operand:GPR 3 "arith_operand")]
  "TARGET_MOVCC"
{
  rtx cmp = operands[1];
  rtx cmp0 = XEXP (cmp, 0);
  rtx cmp1 = XEXP (cmp, 1);
  machine_mode mode0 = GET_MODE (cmp0);

  /* We only handle word mode integer compares for now.  */
  if (INTEGRAL_MODE_P (mode0) && mode0 != word_mode)
    FAIL;

  enum rtx_code code = GET_CODE (cmp);
  rtx reg0 = gen_reg_rtx (<MODE>mode);
  rtx reg1 = gen_reg_rtx (<MODE>mode);
  rtx reg2 = gen_reg_rtx (<MODE>mode);
  bool invert = false;

  if (INTEGRAL_MODE_P (mode0))
    riscv_expand_int_scc (reg0, code, cmp0, cmp1, &invert);
  else if (FLOAT_MODE_P (mode0) && fp_scc_comparison (cmp, GET_MODE (cmp)))
    riscv_expand_float_scc (reg0, code, cmp0, cmp1, &invert);
  else
    FAIL;

  if (invert)
    riscv_emit_binary (PLUS, reg1, reg0, constm1_rtx);
  else
    riscv_emit_unary (NEG, reg1, reg0);
  riscv_emit_binary (AND, reg2, reg1, operands[3]);
  riscv_emit_binary (PLUS, operands[0], reg2, operands[2]);

  DONE;
})

;; Used to implement built-in functions.
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])

(define_expand "@cbranch<mode>4"
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

(define_expand "@cbranch<ANYF:mode>4"
  [(parallel [(set (pc)
		   (if_then_else (match_operator 0 "fp_branch_comparison"
				  [(match_operand:ANYF 1 "register_operand")
				   (match_operand:ANYF 2 "register_operand")])
				 (label_ref (match_operand 3 ""))
				 (pc)))
	      (clobber (match_operand 4 ""))])]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
{
  if (!signed_order_operator (operands[0], GET_MODE (operands[0])))
    {
      riscv_expand_conditional_branch (operands[3], GET_CODE (operands[0]),
				       operands[1], operands[2]);
      DONE;
    }
  operands[4] = gen_reg_rtx (TARGET_64BIT ? DImode : SImode);
})

(define_insn_and_split "*cbranch<ANYF:mode>4"
  [(set (pc)
	(if_then_else (match_operator 1 "fp_native_comparison"
		       [(match_operand:ANYF 2 "register_operand" "f")
			(match_operand:ANYF 3 "register_operand" "f")])
		      (label_ref (match_operand 0 ""))
		      (pc)))
   (clobber (match_operand:X 4 "register_operand" "=r"))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(match_op_dup:X 1 [(match_dup 2) (match_dup 3)]))
   (set (pc)
	(if_then_else (ne:X (match_dup 4) (const_int 0))
		      (label_ref (match_operand 0))
		      (pc)))]
  ""
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (le (minus (match_dup 0) (pc))
			       (const_int 4084))
			   (le (minus (pc) (match_dup 0))
			       (const_int 4096)))
		      (const_int 8)
		      (if_then_else (and (le (minus (match_dup 0) (pc))
					     (const_int 1048564))
					 (le (minus (pc) (match_dup 0))
					     (const_int 1048576)))
				    (const_int 12)
				    (const_int 16))))])

(define_insn_and_split "*cbranch<ANYF:mode>4"
  [(set (pc)
	(if_then_else (match_operator 1 "ne_operator"
		       [(match_operand:ANYF 2 "register_operand" "f")
			(match_operand:ANYF 3 "register_operand" "f")])
		      (label_ref (match_operand 0 ""))
		      (pc)))
   (clobber (match_operand:X 4 "register_operand" "=r"))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(eq:X (match_dup 2) (match_dup 3)))
   (set (pc)
	(if_then_else (eq:X (match_dup 4) (const_int 0))
		      (label_ref (match_operand 0))
		      (pc)))]
  ""
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (le (minus (match_dup 0) (pc))
			       (const_int 4084))
			   (le (minus (pc) (match_dup 0))
			       (const_int 4096)))
		      (const_int 8)
		      (if_then_else (and (le (minus (match_dup 0) (pc))
					     (const_int 1048564))
					 (le (minus (pc) (match_dup 0))
					     (const_int 1048576)))
				    (const_int 12)
				    (const_int 16))))])

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
}
[(set_attr "type" "branch")])

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
}
[(set_attr "type" "branch")])

;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

;; Destination is always set in SI mode.

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "ordered_comparison_operator"
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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "f%C1.<fmt>\t%0,%2,%3"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "f<quiet_pattern>_quiet<ANYF:mode><X:mode>4"
   [(set (match_operand:X               0 "register_operand")
	 (unspec:X [(match_operand:ANYF 1 "register_operand")
		    (match_operand:ANYF 2 "register_operand")]
		   QUIET_COMPARISON))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];

  if (TARGET_ZFA)
    emit_insn (gen_f<quiet_pattern>_quiet<ANYF:mode><X:mode>4_zfa(op0, op1, op2));
  else
    {
      rtx tmp = gen_reg_rtx (SImode);
      rtx cmp = gen_rtx_<QUIET_PATTERN> (<X:MODE>mode, op1, op2);
      rtx frflags = gen_rtx_UNSPEC_VOLATILE (SImode, gen_rtvec (1, const0_rtx),
					     UNSPECV_FRFLAGS);
      rtx fsflags = gen_rtx_UNSPEC_VOLATILE (SImode, gen_rtvec (1, tmp),
					     UNSPECV_FSFLAGS);

      emit_insn (gen_rtx_SET (tmp, frflags));
      emit_insn (gen_rtx_SET (op0, cmp));
      emit_insn (fsflags);
    }

  if (HONOR_SNANS (<ANYF:MODE>mode))
    emit_insn (gen_rtx_UNSPEC_VOLATILE (<ANYF:MODE>mode,
					gen_rtvec (2, op1, op2),
					UNSPECV_FSNVSNAN));
  DONE;
})

(define_insn "f<quiet_pattern>_quiet<ANYF:mode><X:mode>4_zfa"
   [(set (match_operand:X      0 "register_operand" "=r")
	 (unspec:X
	  [(match_operand:ANYF 1 "register_operand" " f")
	   (match_operand:ANYF 2 "register_operand" " f")]
	  QUIET_COMPARISON))]
  "TARGET_HARD_FLOAT && TARGET_ZFA"
  "f<quiet_pattern>q.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length") (const_int 16))])

;; fclass instruction output bitmap
;;   0 negative infinity
;;   1 negative normal number.
;;   2 negative subnormal number.
;;   3 -0
;;   4 +0
;;   5 positive subnormal number.
;;   6 positive normal number.
;;   7 positive infinity
;;   8 signaling NaN.
;;   9 quiet NaN

(define_insn "fclass<ANYF:mode><X:mode>"
  [(set (match_operand:X	     0 "register_operand" "=r")
	(unspec [(match_operand:ANYF 1 "register_operand" " f")]
		   UNSPEC_FCLASS))]
  "TARGET_HARD_FLOAT"
  "fclass.<fmt>\t%0,%1";
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")])

;; Implements optab for isfinite, isnormal, isinf

(define_int_iterator FCLASS_MASK [126 66 129])
(define_int_attr fclass_optab
  [(126	"isfinite")
   (66	"isnormal")
   (129	"isinf")])

(define_expand "<FCLASS_MASK:fclass_optab><ANYF:mode>2"
  [(match_operand      0 "register_operand" "=r")
   (match_operand:ANYF 1 "register_operand" " f")
   (const_int FCLASS_MASK)]
  "TARGET_HARD_FLOAT"
{
  if (GET_MODE (operands[0]) != SImode
      && GET_MODE (operands[0]) != word_mode)
    FAIL;

  rtx t = gen_reg_rtx (word_mode);
  rtx t_op0 = gen_reg_rtx (word_mode);

  if (TARGET_64BIT)
    emit_insn (gen_fclass<ANYF:mode>di (t, operands[1]));
  else
    emit_insn (gen_fclass<ANYF:mode>si (t, operands[1]));

  riscv_emit_binary (AND, t, t, GEN_INT (<FCLASS_MASK>));
  rtx cmp = gen_rtx_NE (word_mode, t, const0_rtx);
  emit_insn (gen_cstore<mode>4 (t_op0, cmp, t, const0_rtx));

  if (TARGET_64BIT)
    {
      t_op0 = gen_lowpart (SImode, t_op0);
      SUBREG_PROMOTED_VAR_P (t_op0) = 1;
      SUBREG_PROMOTED_SET (t_op0, SRP_SIGNED);
    }

  emit_move_insn (operands[0], t_op0);
  DONE;
})

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
  "slti<u>\t%0,zero,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<X:MODE>")])

(define_insn "@slt<u>_<X:mode><GPR:mode>3"
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
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
{
  /* Hopefully this does not happen often as this is going
     to clobber $ra and muck up the return stack predictors.  */
  if (get_attr_length (insn) == 8)
    return "jump\t%l0,ra";

  return "j\t%l0";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  if (is_zicfilp_p ())
    emit_insn (gen_set_lpl (Pmode, const0_rtx));

  operands[0] = force_reg (Pmode, operands[0]);
  if (is_zicfilp_p ())
    emit_use (gen_rtx_REG (Pmode, T2_REGNUM));

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
  [(set_attr "type" "jalr")
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

  if (is_zicfilp_p ())
    {
      rtx t2 = RISCV_CALL_ADDRESS_LPAD (GET_MODE (operands[0]));
      emit_move_insn (t2, operands[0]);

      if (CASE_VECTOR_PC_RELATIVE && Pmode == DImode)
	emit_jump_insn (gen_tablejump_cfidi (operands[1]));
      else
	emit_jump_insn (gen_tablejump_cfisi (operands[1]));
    }
  else
    {
      if (CASE_VECTOR_PC_RELATIVE && Pmode == DImode)
	emit_jump_insn (gen_tablejumpdi (operands[0], operands[1]));
      else
	emit_jump_insn (gen_tablejumpsi (operands[0], operands[1]));
    }
  DONE;
})

(define_insn "tablejump<mode>"
  [(set (pc) (match_operand:GPR 0 "register_operand" "l"))
   (use (label_ref (match_operand 1 "" "")))]
  "!is_zicfilp_p ()"
  "jr\t%0"
  [(set_attr "type" "jalr")
   (set_attr "mode" "none")])

(define_insn "tablejump_cfi<mode>"
  [(set (pc) (reg:GPR T2_REGNUM))
   (use (label_ref (match_operand 0 "")))]
  "is_zicfilp_p ()"
  "jr\tt2"
  [(set_attr "type" "jalr")
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
  [(set_attr "type"	"jalr")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "simple_return_internal"
  [(simple_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "jr\t%0"
  [(set_attr "type"	"jalr")
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
  "#"
  [(set_attr "type" "jump")])

(define_insn "eh_set_lr_di"
  [(unspec [(match_operand:DI 0 "register_operand" "r")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:DI 1 "=&r"))]
  "TARGET_64BIT"
  "#"
  [(set_attr "type" "jump")])

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
  "riscv_expand_epilogue (EXCEPTION_RETURN); DONE;"
  [(set_attr "type" "ret")])

;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (unspec:SI [
		     (match_operand 2 "const_int_operand")
	           ] UNSPEC_CALLEE_CC))])]
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[0], 0));
  emit_call_insn (gen_sibcall_internal (target, operands[1], operands[2]));
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,S,U"))
	 (match_operand 1 "" ""))
   (use (unspec:SI [
          (match_operand 2 "const_int_operand")
        ] UNSPEC_CALLEE_CC))]
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
	      (use (unspec:SI [
		     (match_operand 3 "const_int_operand")
	           ] UNSPEC_CALLEE_CC))])]
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[1], 0));
  emit_call_insn (gen_sibcall_value_internal (operands[0], target, operands[2],
					      operands[3]));
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "j,S,U"))
	      (match_operand 2 "" "")))
   (use (unspec:SI [
          (match_operand 3 "const_int_operand")
        ] UNSPEC_CALLEE_CC))]
  "SIBLING_CALL_P (insn)"
  "@
   jr\t%1
   tail\t%1
   tail\t%1@plt"
  [(set_attr "type" "call")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (unspec:SI [
		     (match_operand 2 "const_int_operand")
	           ] UNSPEC_CALLEE_CC))])]
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[0], 0));
  emit_call_insn (gen_call_internal (target, operands[1], operands[2]));
  DONE;
})

(define_insn "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "l,S,U"))
	 (match_operand 1 "" ""))
   (use (unspec:SI [
          (match_operand 2 "const_int_operand")
        ] UNSPEC_CALLEE_CC))
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
	      (use (unspec:SI [
		     (match_operand 3 "const_int_operand")
	           ] UNSPEC_CALLEE_CC))])]
  ""
{
  rtx target = riscv_legitimize_call_address (XEXP (operands[1], 0));
  emit_call_insn (gen_call_value_internal (operands[0], target, operands[2],
					   operands[3]));
  DONE;
})

(define_insn "call_value_internal"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "call_insn_operand" "l,S,U"))
	      (match_operand 2 "" "")))
   (use (unspec:SI [
          (match_operand 3 "const_int_operand")
        ] UNSPEC_CALLEE_CC))
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

  /* Untyped calls always use the RISCV_CC_BASE calling convention.  */
  emit_call_insn (gen_call (operands[0], const0_rtx,
			    gen_int_mode (RISCV_CC_BASE, SImode)));

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
  "ebreak"
  [(set_attr "type" "trap")])

;; Must use the registers that we save to prevent the rename reg optimization
;; pass from using them before the gpr_save pattern when shrink wrapping
;; occurs.  See bug 95252 for instance.

(define_insn "gpr_save"
  [(match_parallel 1 "gpr_save_operation"
     [(unspec_volatile [(match_operand 0 "const_int_operand")]
	               UNSPECV_GPR_SAVE)])]
  ""
  "call\tt0,__riscv_save_%0"
  [(set_attr "type" "call")])

(define_insn "gpr_restore"
  [(unspec_volatile [(match_operand 0 "const_int_operand")] UNSPECV_GPR_RESTORE)]
  ""
  "tail\t__riscv_restore_%0"
  [(set_attr "type" "call")])

(define_insn "gpr_restore_return"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))
   (const_int 0)]
  ""
  ""
  [(set_attr "type" "ret")])

(define_insn "riscv_frcsr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_FRCSR))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "frcsr\t%0"
  [(set_attr "type" "fmove")])

(define_insn "riscv_fscsr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] UNSPECV_FSCSR)]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fscsr\t%0"
  [(set_attr "type" "fmove")])

(define_insn "riscv_frflags"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_FRFLAGS))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "frflags\t%0"
  [(set_attr "type" "fmove")])

(define_insn "riscv_fsflags"
  [(unspec_volatile [(match_operand:SI 0 "csr_operand" "rK")] UNSPECV_FSFLAGS)]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fsflags%i0\t%0"
  [(set_attr "type" "fmove")])

(define_insn "*riscv_fsnvsnan<mode>2"
  [(unspec_volatile [(match_operand:ANYF 0 "register_operand" "f")
		     (match_operand:ANYF 1 "register_operand" "f")]
		    UNSPECV_FSNVSNAN)]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "feq.<fmt>\tzero,%0,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "riscv_mret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_MRET)]
  ""
  "mret"
  [(set_attr "type" "ret")])

(define_insn "riscv_sret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_SRET)]
  ""
  "sret"
  [(set_attr "type" "ret")])

(define_insn "riscv_uret"
  [(return)
   (unspec_volatile [(const_int 0)] UNSPECV_URET)]
  ""
  "uret"
  [(set_attr "type" "ret")])

(define_insn "stack_tie<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:X 0 "register_operand" "r")
		     (match_operand:X 1 "register_operand" "r")]
		    UNSPEC_TIE))]
  "!rtx_equal_p (operands[0], operands[1])"
  ""
  [(set_attr "type" "ghost")
   (set_attr "length" "0")]
)

(define_expand "save_stack_nonlocal"
  [(set (match_operand 0 "memory_operand")
	(match_operand 1 "register_operand"))]
  ""
{
  rtx stack_slot;

  if (need_shadow_stack_push_pop_p ())
    {
      /* Copy shadow stack pointer to the first slot
	 and stack pointer to the second slot.  */
      rtx ssp_slot = adjust_address (operands[0], word_mode, 0);
      stack_slot = adjust_address (operands[0], Pmode, UNITS_PER_WORD);

      rtx reg_ssp = force_reg (word_mode, const0_rtx);
      emit_insn (gen_ssrdp (word_mode, reg_ssp));
      emit_move_insn (ssp_slot, reg_ssp);
    }
  else
    stack_slot = adjust_address (operands[0], Pmode, 0);
  emit_move_insn (stack_slot, operands[1]);
  DONE;
})

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
  rtx stack_slot;

  if (need_shadow_stack_push_pop_p ())
    {
      rtx t0 = gen_rtx_REG (Pmode, RISCV_PROLOGUE_TEMP_REGNUM);
      /* Restore shadow stack pointer from the first slot
	 and stack pointer from the second slot.  */
      rtx ssp_slot = adjust_address (operands[1], word_mode, 0);
      stack_slot = adjust_address (operands[1], Pmode, UNITS_PER_WORD);

      /* Get the current shadow stack pointer.  */
      rtx cur_ssp = force_reg (word_mode, const0_rtx);
      emit_insn (gen_ssrdp (word_mode, cur_ssp));

      /* Compare and jump over adjustment code.  */
      rtx noadj_label = gen_label_rtx ();
      emit_cmp_and_jump_insns (cur_ssp, const0_rtx, EQ, NULL_RTX,
			       word_mode, 1, noadj_label);

      rtx loop_label = gen_label_rtx ();
      emit_label (loop_label);
      LABEL_NUSES (loop_label) = 1;

      /* Check if current ssp less than jump buffer ssp,
	 so no loop is needed.  */
      emit_cmp_and_jump_insns (ssp_slot, cur_ssp, LE, NULL_RTX,
			       ptr_mode, 1, noadj_label);

      /* Advance by a maximum of 4K at a time to avoid unwinding
	 past bounds of the shadow stack.  */
      rtx reg_4096 = force_reg (word_mode, GEN_INT (4096));
      rtx cmp_ssp  = gen_reg_rtx (word_mode);
      cmp_ssp = expand_simple_binop (ptr_mode, MINUS,
				     ssp_slot, cur_ssp,
				     cmp_ssp, 1, OPTAB_DIRECT);

      /* Update curr_ssp from jump buffer ssp.  */
      emit_move_insn (cur_ssp, ssp_slot);
      emit_insn (gen_write_ssp (word_mode, cur_ssp));
      emit_jump_insn (gen_jump (loop_label));
      emit_barrier ();

      /* Adjust the ssp in a loop.  */
      rtx cmp_4k_label = gen_label_rtx ();
      emit_label (cmp_4k_label);
      LABEL_NUSES (cmp_4k_label) = 1;

      /* Add 4k for curr_ssp.  */
      cur_ssp = expand_simple_binop (ptr_mode, PLUS,
				     cur_ssp, reg_4096,
				     cur_ssp, 1, OPTAB_DIRECT);
      emit_insn (gen_write_ssp (word_mode, cur_ssp));
      emit_insn (gen_sspush (Pmode, t0));
      emit_insn (gen_sspopchk (Pmode, t0));
      emit_jump_insn (gen_jump (loop_label));
      emit_barrier ();

      emit_label (noadj_label);
      LABEL_NUSES (noadj_label) = 1;
    }
  else
    stack_slot = adjust_address (operands[1], Pmode, 0);

  emit_move_insn (operands[0], stack_slot);
  /* Prevent the following hard fp restore from being moved before the move
     insn above which uses a copy of the soft fp reg.  */
  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));
  DONE;
})

;; Named pattern for expanding thread pointer reference.
(define_expand "get_thread_pointer<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(reg:P TP_REGNUM))]
  ""
{})

;; Named patterns for stack smashing protection.

(define_expand "stack_protect_set"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "memory_operand")]
  ""
{
  machine_mode mode = GET_MODE (operands[0]);
  if (riscv_stack_protector_guard == SSP_TLS)
  {
    rtx reg = gen_rtx_REG (Pmode, riscv_stack_protector_guard_reg);
    rtx offset = GEN_INT (riscv_stack_protector_guard_offset);
    rtx addr = gen_rtx_PLUS (Pmode, reg, offset);
    operands[1] = gen_rtx_MEM (Pmode, addr);
  }

  emit_insn ((mode == DImode
	      ? gen_stack_protect_set_di
	      : gen_stack_protect_set_si) (operands[0], operands[1]));
  DONE;
})

;; DO NOT SPLIT THIS PATTERN.  It is important for security reasons that the
;; canary value does not live beyond the life of this sequence.
(define_insn "stack_protect_set_<mode>"
  [(set (match_operand:GPR 0 "memory_operand" "=m")
	(unspec:GPR [(match_operand:GPR 1 "memory_operand" "m")]
	 UNSPEC_SSP_SET))
   (set (match_scratch:GPR 2 "=&r") (const_int 0))]
  ""
  "<load>\t%2, %1\;<store>\t%2, %0\;li\t%2, 0"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])

(define_expand "stack_protect_test"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "memory_operand")
   (match_operand 2)]
  ""
{
  rtx result;
  machine_mode mode = GET_MODE (operands[0]);

  result = gen_reg_rtx(mode);
  if (riscv_stack_protector_guard == SSP_TLS)
  {
      rtx reg = gen_rtx_REG (Pmode, riscv_stack_protector_guard_reg);
      rtx offset = GEN_INT (riscv_stack_protector_guard_offset);
      rtx addr = gen_rtx_PLUS (Pmode, reg, offset);
      operands[1] = gen_rtx_MEM (Pmode, addr);
  }
  emit_insn ((mode == DImode
		  ? gen_stack_protect_test_di
		  : gen_stack_protect_test_si) (result,
					        operands[0],
					        operands[1]));

  rtx cond = gen_rtx_EQ (VOIDmode, result, const0_rtx);
  emit_jump_insn (gen_cbranch4 (mode, cond, result, const0_rtx, operands[2]));

  DONE;
})

(define_insn "stack_protect_test_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "memory_operand" "m")
		     (match_operand:GPR 2 "memory_operand" "m")]
	 UNSPEC_SSP_TEST))
   (clobber (match_scratch:GPR 3 "=&r"))]
  ""
  "<load>\t%3, %1\;<load>\t%0, %2\;xor\t%0, %3, %0\;li\t%3, 0"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])

(define_insn "riscv_clean_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_CLEAN)]
  "TARGET_ZICBOM"
  "cbo.clean\t%a0"
  [(set_attr "type" "store")]
)

(define_insn "riscv_flush_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_FLUSH)]
  "TARGET_ZICBOM"
  "cbo.flush\t%a0"
  [(set_attr "type" "store")]
)

(define_insn "riscv_inval_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_INVAL)]
  "TARGET_ZICBOM"
  "cbo.inval\t%a0"
  [(set_attr "type" "store")]
)

(define_insn "riscv_zero_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_ZERO)]
  "TARGET_ZICBOZ"
  "cbo.zero\t%a0"
  [(set_attr "type" "store")]
)

(define_insn "prefetch"
  [(prefetch (match_operand 0 "address_operand" "r")
             (match_operand 1 "imm5_operand" "i")
             (match_operand 2 "const_int_operand" "n"))]
  "TARGET_ZICBOP"
{
  switch (INTVAL (operands[1]))
  {
    case 0:
    case 2: return TARGET_ZIHINTNTL ? "%L2prefetch.r\t%a0" : "prefetch.r\t%a0";
    case 1: return TARGET_ZIHINTNTL ? "%L2prefetch.w\t%a0" : "prefetch.w\t%a0";
    default: gcc_unreachable ();
  }
}
  [(set_attr "type" "store")
   (set (attr "length") (if_then_else (and (match_test "TARGET_ZIHINTNTL")
					   (match_test "IN_RANGE (INTVAL (operands[2]), 0, 2)"))
				      (const_string "8")
				      (const_string "4")))])

(define_insn "riscv_prefetchi_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "address_operand" "r")
              (match_operand:X 1 "imm5_operand" "i")]
              UNSPECV_PREI)]
  "TARGET_ZICBOP"
  "prefetch.i\t%a0"
  [(set_attr "type" "store")])

(define_expand "extv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(sign_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			 (match_operand 2 "const_int_operand")
			 (match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
)

(define_expand "extzv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "r")
			 (match_operand 2 "const_int_operand")
			 (match_operand 3 "const_int_operand")))]
  "TARGET_XTHEADBB"
{
  if (TARGET_XTHEADBB
      && (INTVAL (operands[2]) < 8) && (INTVAL (operands[3]) == 0))
    FAIL;
})

(define_expand "maddhisi4"
  [(set (match_operand:SI 0 "register_operand")
	(plus:SI
	  (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand"))
		   (sign_extend:SI (match_operand:HI 2 "register_operand")))
	  (match_operand:SI 3 "register_operand")))]
  "TARGET_XTHEADMAC"
)

(define_expand "msubhisi4"
  [(set (match_operand:SI 0 "register_operand")
	(minus:SI
	  (match_operand:SI 3 "register_operand")
	  (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand"))
		   (sign_extend:SI (match_operand:HI 2 "register_operand")))))]
  "TARGET_XTHEADMAC"
)

;; String compare with length insn.
;; Argument 0 is the target (result)
;; Argument 1 is the source1
;; Argument 2 is the source2
;; Argument 3 is the length
;; Argument 4 is the alignment

(define_expand "cmpstrnsi"
  [(parallel [(set (match_operand:SI 0)
	      (compare:SI (match_operand:BLK 1)
			  (match_operand:BLK 2)))
	      (use (match_operand:SI 3))
	      (use (match_operand:SI 4))])]
  "riscv_inline_strncmp && !optimize_size
    && (TARGET_ZBB || TARGET_XTHEADBB || TARGET_VECTOR)"
{
  rtx temp = gen_reg_rtx (word_mode);
  if (riscv_expand_strcmp (temp, operands[1], operands[2],
                           operands[3], operands[4]))
    {
      if (TARGET_64BIT)
	{
	  temp = gen_lowpart (SImode, temp);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_SET (temp, SRP_SIGNED);
	}
      emit_move_insn (operands[0], temp);
      DONE;
    }
  else
    FAIL;
})

;; String compare insn.
;; Argument 0 is the target (result)
;; Argument 1 is the source1
;; Argument 2 is the source2
;; Argument 3 is the alignment

(define_expand "cmpstrsi"
  [(parallel [(set (match_operand:SI 0)
	      (compare:SI (match_operand:BLK 1)
			  (match_operand:BLK 2)))
	      (use (match_operand:SI 3))])]
  "riscv_inline_strcmp && !optimize_size
    && (TARGET_ZBB || TARGET_XTHEADBB || TARGET_VECTOR)"
{
  rtx temp = gen_reg_rtx (word_mode);
  if (riscv_expand_strcmp (temp, operands[1], operands[2],
                           NULL_RTX, operands[3]))
    {
      if (TARGET_64BIT)
	{
	  temp = gen_lowpart (SImode, temp);
	  SUBREG_PROMOTED_VAR_P (temp) = 1;
	  SUBREG_PROMOTED_SET (temp, SRP_SIGNED);
	}
      emit_move_insn (operands[0], temp);
      DONE;
    }
  else
    FAIL;
})

;; Search character in string (generalization of strlen).
;; Argument 0 is the resulting offset
;; Argument 1 is the string
;; Argument 2 is the search character
;; Argument 3 is the alignment

(define_expand "strlen<mode>"
  [(set (match_operand:X 0 "register_operand")
	(unspec:X [(match_operand:BLK 1 "general_operand")
		     (match_operand:SI 2 "const_int_operand")
		     (match_operand:SI 3 "const_int_operand")]
		  UNSPEC_STRLEN))]
  "riscv_inline_strlen && !optimize_size
    && (TARGET_ZBB || TARGET_XTHEADBB || TARGET_VECTOR)"
{
  rtx search_char = operands[2];

  if (search_char != const0_rtx)
    FAIL;

  if (riscv_expand_strlen (operands[0], operands[1], operands[2], operands[3]))
    DONE;
  else
    FAIL;
})

(define_insn "*large_load_address"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (mem:DI (match_operand 1 "pcrel_symbol_operand" "")))]
  "TARGET_64BIT && riscv_cmodel == CM_LARGE"
  "ld\t%0,%1"
  [(set_attr "type" "load")
   (set (attr "length") (const_int 8))])

;; The AND is redunant here.  It always turns off the high 32 bits  and the
;; low number of bits equal to the shift count.  Those upper 32 bits will be
;; reset by the SIGN_EXTEND at the end.
;;
;; One could argue combine should have realized this and simplified what it
;; presented to the backend.  But we can obviously cope with what it gave us.
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (plus:SI (subreg:SI
		     (and:DI
		       (ashift:DI (match_operand:DI 1 "register_operand" "r")
				  (match_operand 2 "const_int_operand" "n"))
		       (match_operand 3 "const_int_operand" "n")) 0)
		   (match_operand:SI 4 "register_operand" "r"))))
   (clobber (match_scratch:DI 5 "=&r"))]
  "TARGET_64BIT
   && (INTVAL (operands[3]) | ((1 << INTVAL (operands[2])) - 1)) == 0xffffffff"
  "#"
  "&& reload_completed"
  [(set (match_dup 5) (ashift:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (sign_extend:DI (plus:SI (match_dup 6) (match_dup 4))))]
  "{ operands[6] = gen_lowpart (SImode, operands[5]); }"
  [(set_attr "type" "arith")])

(define_expand "usadd<mode>3"
  [(match_operand:ANYI 0 "register_operand")
   (match_operand:ANYI 1 "reg_or_int_operand")
   (match_operand:ANYI 2 "reg_or_int_operand")]
  ""
  {
    riscv_expand_usadd (operands[0], operands[1], operands[2]);
    DONE;
  }
)

(define_expand "ssadd<mode>3"
  [(match_operand:ANYI 0 "register_operand")
   (match_operand:ANYI 1 "register_operand")
   (match_operand:ANYI 2 "register_operand")]
  ""
  {
    riscv_expand_ssadd (operands[0], operands[1], operands[2]);
    DONE;
  }
)

(define_expand "ussub<mode>3"
  [(match_operand:ANYI 0 "register_operand")
   (match_operand:ANYI 1 "reg_or_int_operand")
   (match_operand:ANYI 2 "reg_or_int_operand")]
  ""
  {
    riscv_expand_ussub (operands[0], operands[1], operands[2]);
    DONE;
  }
)

(define_expand "sssub<mode>3"
  [(match_operand:ANYI 0 "register_operand")
   (match_operand:ANYI 1 "register_operand")
   (match_operand:ANYI 2 "register_operand")]
  ""
  {
    riscv_expand_sssub (operands[0], operands[1], operands[2]);
    DONE;
  }
)

(define_expand "ustrunc<mode><anyi_double_truncated>2"
  [(match_operand:<ANYI_DOUBLE_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_DOUBLE_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_ustrunc (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "sstrunc<mode><anyi_double_truncated>2"
  [(match_operand:<ANYI_DOUBLE_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_DOUBLE_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_sstrunc (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "ustrunc<mode><anyi_quad_truncated>2"
  [(match_operand:<ANYI_QUAD_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_QUAD_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_ustrunc (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "sstrunc<mode><anyi_quad_truncated>2"
  [(match_operand:<ANYI_QUAD_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_QUAD_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_sstrunc (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "ustrunc<mode><anyi_oct_truncated>2"
  [(match_operand:<ANYI_OCT_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_OCT_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_ustrunc (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "sstrunc<mode><anyi_oct_truncated>2"
  [(match_operand:<ANYI_OCT_TRUNCATED> 0 "register_operand")
   (match_operand:ANYI_OCT_TRUNC       1 "register_operand")]
  ""
  {
    riscv_expand_sstrunc (operands[0], operands[1]);
    DONE;
  }
)

;; These are forms of (x << C1) + C2, potentially canonicalized from
;; ((x + C2') << C1.  Depending on the cost to load C2 vs C2' we may
;; want to go ahead and recognize this form as C2 may be cheaper to
;; synthesize than C2'.
;;
;; It might be better to refactor riscv_const_insns a bit so that we
;; can have an API that passes integer values around rather than
;; constructing a lot of garbage RTL.
;;
;; The mvconst_internal pattern in effect requires this pattern to
;; also be a define_insn_and_split due to insn count costing when
;; splitting in combine.
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			    (match_operand 2 "const_int_operand" "n"))
		 (match_operand 3 "const_int_operand" "n")))
   (clobber (match_scratch:DI 4 "=&r"))]
  "(TARGET_64BIT
    && riscv_const_insns (operands[3], false)
    && ((riscv_const_insns (operands[3], false)
	 < riscv_const_insns (GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2])), false))
	|| riscv_const_insns (GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2])), false) == 0))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (ashift:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (match_dup 3))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 4)))]
  ""
  [(set_attr "type" "arith")])

(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (plus:SI (ashift:SI
				   (match_operand:SI 1 "register_operand" "r")
				   (match_operand 2 "const_int_operand" "n"))
				 (match_operand 3 "const_int_operand" "n"))))
   (clobber (match_scratch:DI 4 "=&r"))]
  "(TARGET_64BIT
    && riscv_const_insns (operands[3], false)
    && ((riscv_const_insns (operands[3], false)
	 < riscv_const_insns (GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2])), false))
	|| riscv_const_insns (GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2])), false) == 0))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (ashift:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (match_dup 3))
   (set (match_dup 0) (sign_extend:DI (plus:SI (match_dup 5) (match_dup 6))))]
  "{
     operands[1] = gen_lowpart (DImode, operands[1]);
     operands[5] = gen_lowpart (SImode, operands[0]);
     operands[6] = gen_lowpart (SImode, operands[4]);
   }"
  [(set_attr "type" "arith")])

;; Shadow stack

(define_insn "@sspush<mode>"
  [(unspec_volatile [(match_operand:P 0 "x1x5_operand" "r")] UNSPECV_SSPUSH)]
  "TARGET_ZICFISS"
  "sspush\t%0"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "@sspopchk<mode>"
  [(unspec_volatile [(match_operand:P 0 "x1x5_operand" "r")] UNSPECV_SSPOPCHK)]
  "TARGET_ZICFISS"
  "sspopchk\t%0"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "@ssrdp<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec_volatile [(const_int 0)] UNSPECV_SSRDP))]
  "TARGET_ZICFISS"
  "ssrdp\t%0"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "@write_ssp<mode>"
  [(unspec_volatile [(match_operand:P 0 "register_operand" "r")] UNSPECV_SSP)]
  "TARGET_ZICFISS"
  "csrw\tssp, %0"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

;; Lading pad.

(define_insn "lpad"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")] UNSPECV_LPAD)]
  "TARGET_ZICFILP"
  "lpad\t%0"
  [(set_attr "type" "auipc")])

(define_insn "@set_lpl<mode>"
  [(set (reg:GPR T2_REGNUM)
	(unspec_volatile [(match_operand:GPR 0 "immediate_operand" "i")] UNSPECV_SETLPL))]
   "TARGET_ZICFILP"
   "lui\tt2,%0"
  [(set_attr "type" "const")
   (set_attr "mode" "<MODE>")])

(define_insn "lpad_align"
  [(unspec_volatile [(const_int 0)] UNSPECV_LPAD_ALIGN)]
  "TARGET_ZICFILP"
  ".align 2"
  [(set_attr "type" "nop")])

(define_insn "@set_guarded<mode>"
  [(set (reg:GPR T2_REGNUM)
	(unspec_volatile [(match_operand:GPR 0 "register_operand" "r")] UNSPECV_SET_GUARDED))]
  "TARGET_ZICFILP"
  "mv\tt2,%0"
  [(set_attr "type" "move")
   (set_attr "mode" "<MODE>")])

(include "bitmanip.md")
(include "crypto.md")
(include "sync.md")
(include "sync-rvwmo.md")
(include "sync-ztso.md")
(include "peephole.md")
(include "pic.md")
(include "generic.md")
(include "sifive-7.md")
(include "sifive-p400.md")
(include "sifive-p600.md")
(include "thead.md")
(include "generic-vector-ooo.md")
(include "generic-ooo.md")
(include "vector.md")
(include "vector-crypto.md")
(include "vector-bfloat16.md")
(include "zicond.md")
(include "sfb.md")
(include "zc.md")
(include "corev.md")
(include "xiangshan.md")
