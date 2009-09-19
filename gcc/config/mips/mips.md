;;  Mips.md	     Machine Description for MIPS based processors
;;  Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;  1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;  Free Software Foundation, Inc.
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

(define_constants
  [(UNSPEC_LOAD_LOW		 0)
   (UNSPEC_LOAD_HIGH		 1)
   (UNSPEC_STORE_WORD		 2)
   (UNSPEC_GET_FNADDR		 3)
   (UNSPEC_BLOCKAGE		 4)
   (UNSPEC_POTENTIAL_CPRESTORE	 5)
   (UNSPEC_CPRESTORE		 6)
   (UNSPEC_RESTORE_GP		 7)
   (UNSPEC_MOVE_GP		 8)
   (UNSPEC_EH_RETURN		 9)
   (UNSPEC_CONSTTABLE_INT	10)
   (UNSPEC_CONSTTABLE_FLOAT	11)
   (UNSPEC_ALIGN		14)
   (UNSPEC_HIGH			17)
   (UNSPEC_LOAD_LEFT		18)
   (UNSPEC_LOAD_RIGHT		19)
   (UNSPEC_STORE_LEFT		20)
   (UNSPEC_STORE_RIGHT		21)
   (UNSPEC_LOADGP		22)
   (UNSPEC_LOAD_CALL		23)
   (UNSPEC_LOAD_GOT		24)
   (UNSPEC_GP			25)
   (UNSPEC_MFHI			26)
   (UNSPEC_MTHI			27)
   (UNSPEC_SET_HILO		28)
   (UNSPEC_TLS_LDM		29)
   (UNSPEC_TLS_GET_TP		30)
   (UNSPEC_MFHC1		31)
   (UNSPEC_MTHC1		32)
   (UNSPEC_CLEAR_HAZARD		33)
   (UNSPEC_RDHWR		34)
   (UNSPEC_SYNCI		35)
   (UNSPEC_SYNC			36)
   (UNSPEC_COMPARE_AND_SWAP	37)
   (UNSPEC_COMPARE_AND_SWAP_12	38)
   (UNSPEC_SYNC_OLD_OP		39)
   (UNSPEC_SYNC_NEW_OP		40)
   (UNSPEC_SYNC_NEW_OP_12	41)
   (UNSPEC_SYNC_OLD_OP_12	42)
   (UNSPEC_SYNC_EXCHANGE	43)
   (UNSPEC_SYNC_EXCHANGE_12	44)
   (UNSPEC_MEMORY_BARRIER	45)
   (UNSPEC_SET_GOT_VERSION	46)
   (UNSPEC_UPDATE_GOT_VERSION	47)
   (UNSPEC_COPYGP		48)
   (UNSPEC_ERET			49)
   (UNSPEC_DERET		50)
   (UNSPEC_DI			51)
   (UNSPEC_EHB			52)
   (UNSPEC_RDPGPR		53)
   (UNSPEC_COP0			54)
   ;; Used in a call expression in place of args_size.  It's present for PIC
   ;; indirect calls where it contains args_size and the function symbol.
   (UNSPEC_CALL_ATTR		55)
   
   (UNSPEC_ADDRESS_FIRST	100)

   (TLS_GET_TP_REGNUM		3)
   (CPRESTORE_SLOT_REGNUM	76)
   (GOT_VERSION_REGNUM		79)

   ;; For MIPS Paired-Singled Floating Point Instructions.

   (UNSPEC_MOVE_TF_PS		200)
   (UNSPEC_C			201)

   ;; MIPS64/MIPS32R2 alnv.ps
   (UNSPEC_ALNV_PS		202)

   ;; MIPS-3D instructions
   (UNSPEC_CABS			203)

   (UNSPEC_ADDR_PS		204)
   (UNSPEC_CVT_PW_PS		205)
   (UNSPEC_CVT_PS_PW		206)
   (UNSPEC_MULR_PS		207)
   (UNSPEC_ABS_PS		208)

   (UNSPEC_RSQRT1		209)
   (UNSPEC_RSQRT2		210)
   (UNSPEC_RECIP1		211)
   (UNSPEC_RECIP2		212)
   (UNSPEC_SINGLE_CC		213)
   (UNSPEC_SCC			214)

   ;; MIPS DSP ASE Revision 0.98 3/24/2005
   (UNSPEC_ADDQ			300)
   (UNSPEC_ADDQ_S		301)
   (UNSPEC_SUBQ			302)
   (UNSPEC_SUBQ_S		303)
   (UNSPEC_ADDSC		304)
   (UNSPEC_ADDWC		305)
   (UNSPEC_MODSUB		306)
   (UNSPEC_RADDU_W_QB		307)
   (UNSPEC_ABSQ_S		308)
   (UNSPEC_PRECRQ_QB_PH		309)
   (UNSPEC_PRECRQ_PH_W		310)
   (UNSPEC_PRECRQ_RS_PH_W	311)
   (UNSPEC_PRECRQU_S_QB_PH	312)
   (UNSPEC_PRECEQ_W_PHL		313)
   (UNSPEC_PRECEQ_W_PHR		314)
   (UNSPEC_PRECEQU_PH_QBL	315)
   (UNSPEC_PRECEQU_PH_QBR	316)
   (UNSPEC_PRECEQU_PH_QBLA	317)
   (UNSPEC_PRECEQU_PH_QBRA	318)
   (UNSPEC_PRECEU_PH_QBL	319)
   (UNSPEC_PRECEU_PH_QBR	320)
   (UNSPEC_PRECEU_PH_QBLA	321)
   (UNSPEC_PRECEU_PH_QBRA	322)
   (UNSPEC_SHLL			323)
   (UNSPEC_SHLL_S		324)
   (UNSPEC_SHRL_QB		325)
   (UNSPEC_SHRA_PH		326)
   (UNSPEC_SHRA_R		327)
   (UNSPEC_MULEU_S_PH_QBL	328)
   (UNSPEC_MULEU_S_PH_QBR	329)
   (UNSPEC_MULQ_RS_PH		330)
   (UNSPEC_MULEQ_S_W_PHL	331)
   (UNSPEC_MULEQ_S_W_PHR	332)
   (UNSPEC_DPAU_H_QBL		333)
   (UNSPEC_DPAU_H_QBR		334)
   (UNSPEC_DPSU_H_QBL		335)
   (UNSPEC_DPSU_H_QBR		336)
   (UNSPEC_DPAQ_S_W_PH		337)
   (UNSPEC_DPSQ_S_W_PH		338)
   (UNSPEC_MULSAQ_S_W_PH	339)
   (UNSPEC_DPAQ_SA_L_W		340)
   (UNSPEC_DPSQ_SA_L_W		341)
   (UNSPEC_MAQ_S_W_PHL		342)
   (UNSPEC_MAQ_S_W_PHR		343)
   (UNSPEC_MAQ_SA_W_PHL		344)
   (UNSPEC_MAQ_SA_W_PHR		345)
   (UNSPEC_BITREV		346)
   (UNSPEC_INSV			347)
   (UNSPEC_REPL_QB		348)
   (UNSPEC_REPL_PH		349)
   (UNSPEC_CMP_EQ		350)
   (UNSPEC_CMP_LT		351)
   (UNSPEC_CMP_LE		352)
   (UNSPEC_CMPGU_EQ_QB		353)
   (UNSPEC_CMPGU_LT_QB		354)
   (UNSPEC_CMPGU_LE_QB		355)
   (UNSPEC_PICK			356)
   (UNSPEC_PACKRL_PH		357)
   (UNSPEC_EXTR_W		358)
   (UNSPEC_EXTR_R_W		359)
   (UNSPEC_EXTR_RS_W		360)
   (UNSPEC_EXTR_S_H		361)
   (UNSPEC_EXTP			362)
   (UNSPEC_EXTPDP		363)
   (UNSPEC_SHILO		364)
   (UNSPEC_MTHLIP		365)
   (UNSPEC_WRDSP		366)
   (UNSPEC_RDDSP		367)

   ;; MIPS DSP ASE REV 2 Revision 0.02 11/24/2006
   (UNSPEC_ABSQ_S_QB		400)
   (UNSPEC_ADDU_PH		401)
   (UNSPEC_ADDU_S_PH		402)
   (UNSPEC_ADDUH_QB		403)
   (UNSPEC_ADDUH_R_QB		404)
   (UNSPEC_APPEND		405)
   (UNSPEC_BALIGN		406)
   (UNSPEC_CMPGDU_EQ_QB		407)
   (UNSPEC_CMPGDU_LT_QB		408)
   (UNSPEC_CMPGDU_LE_QB		409)
   (UNSPEC_DPA_W_PH		410)
   (UNSPEC_DPS_W_PH		411)
   (UNSPEC_MADD			412)
   (UNSPEC_MADDU		413)
   (UNSPEC_MSUB			414)
   (UNSPEC_MSUBU		415)
   (UNSPEC_MUL_PH		416)
   (UNSPEC_MUL_S_PH		417)
   (UNSPEC_MULQ_RS_W		418)
   (UNSPEC_MULQ_S_PH		419)
   (UNSPEC_MULQ_S_W		420)
   (UNSPEC_MULSA_W_PH		421)
   (UNSPEC_MULT			422)
   (UNSPEC_MULTU		423)
   (UNSPEC_PRECR_QB_PH		424)
   (UNSPEC_PRECR_SRA_PH_W	425)
   (UNSPEC_PRECR_SRA_R_PH_W	426)
   (UNSPEC_PREPEND		427)
   (UNSPEC_SHRA_QB		428)
   (UNSPEC_SHRA_R_QB		429)
   (UNSPEC_SHRL_PH		430)
   (UNSPEC_SUBU_PH		431)
   (UNSPEC_SUBU_S_PH		432)
   (UNSPEC_SUBUH_QB		433)
   (UNSPEC_SUBUH_R_QB		434)
   (UNSPEC_ADDQH_PH		435)
   (UNSPEC_ADDQH_R_PH		436)
   (UNSPEC_ADDQH_W		437)
   (UNSPEC_ADDQH_R_W		438)
   (UNSPEC_SUBQH_PH		439)
   (UNSPEC_SUBQH_R_PH		440)
   (UNSPEC_SUBQH_W		441)
   (UNSPEC_SUBQH_R_W		442)
   (UNSPEC_DPAX_W_PH		443)
   (UNSPEC_DPSX_W_PH		444)
   (UNSPEC_DPAQX_S_W_PH		445)
   (UNSPEC_DPAQX_SA_W_PH	446)
   (UNSPEC_DPSQX_S_W_PH		447)
   (UNSPEC_DPSQX_SA_W_PH	448)

   ;; ST Microelectronics Loongson-2E/2F.
   (UNSPEC_LOONGSON_PAVG	500)
   (UNSPEC_LOONGSON_PCMPEQ	501)
   (UNSPEC_LOONGSON_PCMPGT	502)
   (UNSPEC_LOONGSON_PEXTR	503)
   (UNSPEC_LOONGSON_PINSR_0	504)
   (UNSPEC_LOONGSON_PINSR_1	505)
   (UNSPEC_LOONGSON_PINSR_2	506)
   (UNSPEC_LOONGSON_PINSR_3	507)
   (UNSPEC_LOONGSON_PMADD	508)
   (UNSPEC_LOONGSON_PMOVMSK	509)
   (UNSPEC_LOONGSON_PMULHU	510)
   (UNSPEC_LOONGSON_PMULH	511)
   (UNSPEC_LOONGSON_PMULL	512)
   (UNSPEC_LOONGSON_PMULU	513)
   (UNSPEC_LOONGSON_PASUBUB	514)
   (UNSPEC_LOONGSON_BIADD	515)
   (UNSPEC_LOONGSON_PSADBH	516)
   (UNSPEC_LOONGSON_PSHUFH	517)
   (UNSPEC_LOONGSON_PUNPCKH	518)
   (UNSPEC_LOONGSON_PUNPCKL	519)
   (UNSPEC_LOONGSON_PADDD	520)
   (UNSPEC_LOONGSON_PSUBD	521)

   ;; Used in loongson2ef.md
   (UNSPEC_LOONGSON_ALU1_TURN_ENABLED_INSN   530)
   (UNSPEC_LOONGSON_ALU2_TURN_ENABLED_INSN   531)
   (UNSPEC_LOONGSON_FALU1_TURN_ENABLED_INSN  532)
   (UNSPEC_LOONGSON_FALU2_TURN_ENABLED_INSN  533)

   (UNSPEC_MIPS_CACHE		600)
   (UNSPEC_R10K_CACHE_BARRIER	601)

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
;; lui_movf	an LUI followed by a MOVF (for d<-z CC moves)
;;
;; This attribute is used to determine the instruction's length and
;; scheduling type.  For doubleword moves, the attribute always describes
;; the split instructions; in some cases, it is more appropriate for the
;; scheduling type to be "multi" instead.
(define_attr "move_type"
  "unknown,load,fpload,store,fpstore,mtc,mfc,mthilo,mfhilo,move,fmove,
   const,constN,signext,ext_ins,logical,arith,sll0,andi,loadpool,
   shift_shift,lui_movf"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF,FPSW"
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
;; fpidxload    floating point indexed load
;; store	store instruction(s)
;; fpstore	floating point store
;; fpidxstore	floating point indexed store
;; prefetch	memory prefetch (register + offset)
;; prefetchx	memory indexed prefetch (register + register)
;; condmove	conditional moves
;; mtc		transfer to coprocessor
;; mfc		transfer from coprocessor
;; mthilo	transfer to hi/lo registers
;; mfhilo	transfer from hi/lo registers
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
;; multi	multiword sequence (or user asm statements)
;; nop		no operation
;; ghost	an instruction that produces no real code
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,fpidxload,store,fpstore,fpidxstore,
   prefetch,prefetchx,condmove,mtc,mfc,mthilo,mfhilo,const,arith,logical,
   shift,slt,signext,clz,pop,trap,imul,imul3,imul3nc,imadd,idiv,idiv3,move,
   fmove,fadd,fmul,fmadd,fdiv,frdiv,frdiv1,frdiv2,fabs,fneg,fcmp,fcvt,fsqrt,
   frsqrt,frsqrt1,frsqrt2,multi,nop,ghost"
  (cond [(eq_attr "jal" "!unset") (const_string "call")
	 (eq_attr "got" "load") (const_string "load")

	 ;; If a doubleword move uses these expensive instructions,
	 ;; it is usually better to schedule them in the same way
	 ;; as the singleword form, rather than as "multi".
	 (eq_attr "move_type" "load") (const_string "load")
	 (eq_attr "move_type" "fpload") (const_string "fpload")
	 (eq_attr "move_type" "store") (const_string "store")
	 (eq_attr "move_type" "fpstore") (const_string "fpstore")
	 (eq_attr "move_type" "mtc") (const_string "mtc")
	 (eq_attr "move_type" "mfc") (const_string "mfc")
	 (eq_attr "move_type" "mthilo") (const_string "mthilo")
	 (eq_attr "move_type" "mfhilo") (const_string "mfhilo")

	 ;; These types of move are always single insns.
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

	 ;; These types of move are split for doubleword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "dword_mode" "yes"))
	   (const_string "multi")
	 (eq_attr "move_type" "move") (const_string "move")
	 (eq_attr "move_type" "const") (const_string "const")]
	;; We classify "lui_movf" as "unknown" rather than "multi"
	;; because we don't split it.  FIXME: we should split instead.
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

;; Is this an extended instruction in mips16 mode?
(define_attr "extended_mips16" "no,yes"
  (if_then_else (ior (eq_attr "move_type" "sll0")
		     (eq_attr "type" "branch")
		     (eq_attr "jal" "direct"))
		(const_string "yes")
		(const_string "no")))

;; Attributes describing a sync loop.  These loops have the form:
;;
;;       if (RELEASE_BARRIER == YES) sync
;;    1: OLDVAL = *MEM
;;       if ((OLDVAL & INCLUSIVE_MASK) != REQUIRED_OLDVAL) goto 2
;;       $TMP1 = OLDVAL & EXCLUSIVE_MASK
;;       $TMP2 = INSN1 (OLDVAL, INSN1_OP2)
;;       $TMP3 = INSN2 ($TMP2, INCLUSIVE_MASK)
;;       $AT |= $TMP1 | $TMP3
;;       if (!commit (*MEM = $AT)) goto 1.
;;         if (INSN1 != MOVE && INSN1 != LI) NEWVAL = $TMP3 [delay slot]
;;       sync
;;    2:
;;
;; where "$" values are temporaries and where the other values are
;; specified by the attributes below.  Values are specified as operand
;; numbers and insns are specified as enums.  If no operand number is
;; specified, the following values are used instead:
;;
;;    - OLDVAL: $AT
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
(define_attr "sync_newval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_inclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_exclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_required_oldval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1_op2" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1" "move,li,addu,addiu,subu,and,andi,or,ori,xor,xori"
  (const_string "move"))
(define_attr "sync_insn2" "nop,and,xor,not"
  (const_string "nop"))
(define_attr "sync_release_barrier" "yes,no"
  (const_string "yes"))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [(and (eq_attr "extended_mips16" "yes")
	       (ne (symbol_ref "TARGET_MIPS16") (const_int 0)))
	  (const_int 8)

	  ;; Direct branch instructions have a range of [-0x20000,0x1fffc],
	  ;; relative to the address of the delay slot.  If a branch is
	  ;; outside this range, we have a choice of two sequences.
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
	  ;; Note that this value does not account for the delay slot
	  ;; instruction, whose length is added separately.  If the RTL
	  ;; pattern has no explicit delay slot, mips_adjust_insn_length
	  ;; will add the length of the implicit nop.  The values for
	  ;; forward and backward branches will be different as well.
	  (eq_attr "type" "branch")
	  (cond [(and (le (minus (match_dup 0) (pc)) (const_int 131064))
			  (le (minus (pc) (match_dup 0)) (const_int 131068)))
		   (const_int 4)

		 ;; The non-PIC case: branch, first delay slot, and J.
		 (ne (symbol_ref "TARGET_ABSOLUTE_JUMPS") (const_int 0))
		   (const_int 12)]

		 ;; Use MAX_PIC_BRANCH_LENGTH as a (gross) overestimate.
		 ;; mips_adjust_insn_length substitutes the correct length.
		 ;;
		 ;; Note that we can't simply use (symbol_ref ...) here
		 ;; because genattrtab needs to know the maximum length
		 ;; of an insn.
		 (const_int MAX_PIC_BRANCH_LENGTH))

	  ;; "Ghost" instructions occupy no space.
	  (eq_attr "type" "ghost")
	  (const_int 0)

	  (eq_attr "got" "load")
	  (if_then_else (ne (symbol_ref "TARGET_MIPS16") (const_int 0))
			(const_int 8)
			(const_int 4))
	  (eq_attr "got" "xgot_high")
	  (const_int 8)

	  ;; In general, constant-pool loads are extended instructions.
	  (eq_attr "move_type" "loadpool")
	  (const_int 8)

	  ;; LUI_MOVFs are decomposed into two separate instructions.
	  (eq_attr "move_type" "lui_movf")
	  (const_int 8)

	  ;; SHIFT_SHIFTs are decomposed into two separate instructions.
	  ;; They are extended instructions on MIPS16 targets.
	  (eq_attr "move_type" "shift_shift")
	  (if_then_else (ne (symbol_ref "TARGET_MIPS16") (const_int 0))
			(const_int 16)
			(const_int 8))

	  ;; Check for doubleword moves that are decomposed into two
	  ;; instructions.
	  (and (eq_attr "move_type" "mtc,mfc,mthilo,mfhilo,move")
	       (eq_attr "dword_mode" "yes"))
	  (const_int 8)

	  ;; Doubleword CONST{,N} moves are split into two word
	  ;; CONST{,N} moves.
	  (and (eq_attr "move_type" "const,constN")
	       (eq_attr "dword_mode" "yes"))
	  (symbol_ref "mips_split_const_insns (operands[1]) * 4")

	  ;; Otherwise, constants, loads and stores are handled by external
	  ;; routines.
	  (eq_attr "move_type" "const,constN")
	  (symbol_ref "mips_const_insns (operands[1]) * 4")
	  (eq_attr "move_type" "load,fpload")
	  (symbol_ref "mips_load_store_insns (operands[1], insn) * 4")
	  (eq_attr "move_type" "store,fpstore")
	  (symbol_ref "mips_load_store_insns (operands[0], insn) * 4")

	  ;; In the worst case, a call macro will take 8 instructions:
	  ;;
	  ;;	 lui $25,%call_hi(FOO)
	  ;;	 addu $25,$25,$28
	  ;;     lw $25,%call_lo(FOO)($25)
	  ;;	 nop
	  ;;	 jalr $25
	  ;;	 nop
	  ;;	 lw $gp,X($sp)
	  ;;	 nop
	  (eq_attr "jal_macro" "yes")
	  (const_int 32)

	  ;; Various VR4120 errata require a nop to be inserted after a macc
	  ;; instruction.  The assembler does this for us, so account for
	  ;; the worst-case length here.
	  (and (eq_attr "type" "imadd")
	       (ne (symbol_ref "TARGET_FIX_VR4120") (const_int 0)))
	  (const_int 8)

	  ;; VR4120 errata MD(4): if there are consecutive dmult instructions,
	  ;; the result of the second one is missed.  The assembler should work
	  ;; around this by inserting a nop after the first dmult.
	  (and (eq_attr "type" "imul,imul3")
	       (and (eq_attr "mode" "DI")
		    (ne (symbol_ref "TARGET_FIX_VR4120") (const_int 0))))
	  (const_int 8)

	  (eq_attr "type" "idiv,idiv3")
	  (symbol_ref "mips_idiv_insns () * 4")

	  (not (eq_attr "sync_mem" "none"))
	  (symbol_ref "mips_sync_loop_insns (insn, operands) * 4")
	  ] (const_int 4)))

;; Attribute describing the processor.  This attribute must match exactly
;; with the processor_type enumeration in mips.h.
(define_attr "cpu"
  "r3000,4kc,4kp,5kc,5kf,20kc,24kc,24kf2_1,24kf1_1,74kc,74kf2_1,74kf1_1,74kf3_2,loongson_2e,loongson_2f,m4k,octeon,r3900,r6000,r4000,r4100,r4111,r4120,r4130,r4300,r4600,r4650,r5000,r5400,r5500,r7000,r8000,r9000,r10000,sb1,sb1a,sr71000,xlr"
  (const (symbol_ref "mips_tune_attr")))

;; The type of hardware hazard associated with this instruction.
;; DELAY means that the next instruction cannot read the result
;; of this one.  HILO means that the next two instructions cannot
;; write to HI or LO.
(define_attr "hazard" "none,delay,hilo"
  (cond [(and (eq_attr "type" "load,fpload,fpidxload")
	      (ne (symbol_ref "ISA_HAS_LOAD_DELAY") (const_int 0)))
	 (const_string "delay")

	 (and (eq_attr "type" "mfc,mtc")
	      (ne (symbol_ref "ISA_HAS_XFER_DELAY") (const_int 0)))
	 (const_string "delay")

	 (and (eq_attr "type" "fcmp")
	      (ne (symbol_ref "ISA_HAS_FCMP_DELAY") (const_int 0)))
	 (const_string "delay")

	 ;; The r4000 multiplication patterns include an mflo instruction.
	 (and (eq_attr "type" "imul")
	      (ne (symbol_ref "TARGET_FIX_R4000") (const_int 0)))
	 (const_string "hilo")

	 (and (eq_attr "type" "mfhilo")
	      (eq (symbol_ref "ISA_HAS_HILO_INTERLOCKS") (const_int 0)))
	 (const_string "hilo")]
	(const_string "none")))

;; Is it a single instruction?
(define_attr "single_insn" "no,yes"
  (symbol_ref "(get_attr_length (insn) == (TARGET_MIPS16 ? 2 : 4)
		? SINGLE_INSN_YES : SINGLE_INSN_NO)"))

;; Can the instruction be put into a delay slot?
(define_attr "can_delay" "no,yes"
  (if_then_else (and (eq_attr "type" "!branch,call,jump")
		     (and (eq_attr "hazard" "none")
			  (eq_attr "single_insn" "yes")))
		(const_string "yes")
		(const_string "no")))

;; Attribute defining whether or not we can use the branch-likely
;; instructions.
(define_attr "branch_likely" "no,yes"
  (if_then_else (ne (symbol_ref "GENERATE_BRANCHLIKELY") (const_int 0))
		(const_string "yes")
		(const_string "no")))

;; True if an instruction might assign to hi or lo when reloaded.
;; This is used by the TUNE_MACC_CHAINS code.
(define_attr "may_clobber_hilo" "no,yes"
  (if_then_else (eq_attr "type" "imul,imul3,imadd,idiv,mthilo")
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

;; This mode iterator allows :HILO to be used as the mode of the
;; concatenated HI and LO registers.
(define_mode_iterator HILO [(DI "!TARGET_64BIT") (TI "TARGET_64BIT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; This mode iterator allows :MOVECC to be used anywhere that a
;; conditional-move-type condition is needed.
(define_mode_iterator MOVECC [SI (DI "TARGET_64BIT")
                              (CC "TARGET_HARD_FLOAT && !TARGET_LOONGSON_2EF")])

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
   (V2SI "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS")
   (V4HI "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS")
   (V8QI "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS")])

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
   (V2SI "!TARGET_64BIT && TARGET_LOONGSON_VECTORS")
   (V4HI "!TARGET_64BIT && TARGET_LOONGSON_VECTORS")
   (V8QI "!TARGET_64BIT && TARGET_LOONGSON_VECTORS")
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

;; This attribute gives the length suffix for a sign- or zero-extension
;; instruction.
(define_mode_attr size [(QI "b") (HI "h")])

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
(define_mode_attr reg [(SI "d") (DI "d") (CC "z")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d") (V2SF "ps")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF") (V2SF "SF")])

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

;; This attribute gives the conditions under which RECIP.fmt and RSQRT.fmt
;; instructions can be used.  The MIPS32 and MIPS64 ISAs say that RECIP.D
;; and RSQRT.D are unpredictable when doubles are stored in pairs of FPRs,
;; so for safety's sake, we apply this restriction to all targets.
(define_mode_attr recip_condition
  [(SF "ISA_HAS_FP4")
   (DF "ISA_HAS_FP4 && TARGET_FLOAT64")
   (V2SF "TARGET_SB1")])

;; This code iterator allows signed and unsigned widening multiplications
;; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

;; This code iterator allows the two right shift instructions to be
;; generated from the same template.
(define_code_iterator any_shiftrt [ashiftrt lshiftrt])

;; This code iterator allows the three shift instructions to be generated
;; from the same template.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; This code iterator allows unsigned and signed division to be generated
;; from the same template.
(define_code_iterator any_div [div udiv])

;; This code iterator allows unsigned and signed modulus to be generated
;; from the same template.
(define_code_iterator any_mod [mod umod])

;; This code iterator allows all native floating-point comparisons to be
;; generated from the same template.
(define_code_iterator fcond [unordered uneq unlt unle eq lt le])

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

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")
		     (div "") (udiv "u")
		     (mod "") (umod "u")
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
			 (ior "ior")
			 (xor "xor")
			 (and "and")
			 (plus "add")
			 (minus "sub")])

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

;; <fcond> is the c.cond.fmt condition associated with a particular code.
(define_code_attr fcond [(unordered "un")
			 (uneq "ueq")
			 (unlt "ult")
			 (unle "ule")
			 (eq "eq")
			 (lt "lt")
			 (le "le")])

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

;; .........................
;;
;;	Branch, call and jump delay slots
;;
;; .........................

(define_delay (and (eq_attr "type" "branch")
		   (eq (symbol_ref "TARGET_MIPS16") (const_int 0))
		   (eq_attr "branch_likely" "yes"))
  [(eq_attr "can_delay" "yes")
   (nil)
   (eq_attr "can_delay" "yes")])

;; Branches that don't have likely variants do not annul on false.
(define_delay (and (eq_attr "type" "branch")
		   (eq (symbol_ref "TARGET_MIPS16") (const_int 0))
		   (eq_attr "branch_likely" "no"))
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

(define_delay (eq_attr "type" "jump")
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

(define_delay (and (eq_attr "type" "call")
		   (eq_attr "jal_macro" "no"))
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
(include "octeon.md")
(include "sb1.md")
(include "sr71k.md")
(include "xlr.md")
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
  "ISA_HAS_COND_TRAP"
{
  mips_expand_conditional_trap (operands[0]);
  DONE;
})

(define_insn "*conditional_trap<mode>"
  [(trap_if (match_operator:GPR 0 "trap_comparison_operator"
				[(match_operand:GPR 1 "reg_or_0_operand" "dJ")
				 (match_operand:GPR 2 "arith_operand" "dI")])
	    (const_int 0))]
  "ISA_HAS_COND_TRAP"
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
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(plus:GPR (match_operand:GPR 1 "register_operand" "d,d")
		  (match_operand:GPR 2 "arith_operand" "d,Q")))]
  "!TARGET_MIPS16"
  "@
    <d>addu\t%0,%1,%2
    <d>addiu\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "*add<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=ks,d,d,d,d")
	(plus:GPR (match_operand:GPR 1 "register_operand" "ks,ks,0,d,d")
		  (match_operand:GPR 2 "arith_operand" "Q,Q,Q,O,d")))]
  "TARGET_MIPS16"
  "@
    <d>addiu\t%0,%2
    <d>addiu\t%0,%1,%2
    <d>addiu\t%0,%2
    <d>addiu\t%0,%1,%2
    <d>addu\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")
   (set_attr_alternative "length"
		[(if_then_else (match_operand 2 "m16_simm8_8")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand 2 "m16_uimm<si8_di5>_4")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand 2 "m16_simm<si8_di5>_1")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand 2 "m16_simm4_1")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

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
  [(set_attr "type" "arith")
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
  [(set_attr "type" "arith")
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
  [(set_attr "type" "arith")])

(define_insn "*baddu_si_el"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI
	 (subreg:QI
	  (plus:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")) 0)))]
  "ISA_HAS_BADDU && !BYTES_BIG_ENDIAN"
  "baddu\\t%0,%1,%2"
  [(set_attr "type" "arith")])

(define_insn "*baddu_di<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR
	 (truncate:QI
	  (plus:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:DI 2 "register_operand" "d")))))]
  "ISA_HAS_BADDU && TARGET_64BIT"
  "baddu\\t%0,%1,%2"
  [(set_attr "type" "arith")])

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
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(minus:GPR (match_operand:GPR 1 "register_operand" "d")
		   (match_operand:GPR 2 "register_operand" "d")))]
  ""
  "<d>subu\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "*subsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "register_operand" "d")
		      (match_operand:SI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "subu\t%0,%1,%2"
  [(set_attr "type" "arith")
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
   (set_attr "length" "8")])

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
  ""
{
  if (TARGET_LOONGSON_2EF)
    emit_insn (gen_mul<mode>3_mul3_ls2ef (operands[0], operands[1],
                                          operands[2]));
  else if (ISA_HAS_<D>MUL3)
    emit_insn (gen_mul<mode>3_mul3 (operands[0], operands[1], operands[2]));
  else if (TARGET_FIX_R4000)
    emit_insn (gen_mul<mode>3_r4000 (operands[0], operands[1], operands[2]));
  else
    emit_insn
      (gen_mul<mode>3_internal (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "mul<mode>3_mul3_ls2ef"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (mult:GPR (match_operand:GPR 1 "register_operand" "d")
                  (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF"
  "<d>multu.g\t%0,%1,%2"
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
  if (<MODE>mode == SImode && TARGET_MIPS3900)
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
  [(set (match_operand:GPR 0 "register_operand" "=l")
	(mult:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))]
  "!TARGET_FIX_R4000"
  "<d>mult\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3_r4000"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(mult:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))
   (clobber (match_scratch:GPR 3 "=l"))]
  "TARGET_FIX_R4000"
  "<d>mult\t%1,%2\;mflo\t%0"
  [(set_attr "type" "imul")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "8")])

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
(define_insn "*mul_acc_si"
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,d?")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d")
			  (match_operand:SI 2 "register_operand" "d,d"))
		 (match_operand:SI 3 "register_operand" "0,d")))
   (clobber (match_scratch:SI 4 "=X,l"))
   (clobber (match_scratch:SI 5 "=X,&d"))]
  "GENERATE_MADD_MSUB && !TARGET_MIPS16"
  "@
    madd\t%1,%2
    #"
  [(set_attr "type"	"imadd")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

;; The same idea applies here.  The middle alternative needs one less
;; clobber than the final alternative, so we add "*?" as a counterweight.
(define_insn "*mul_acc_si_r3900"
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,d*?,d?")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d,d")
			  (match_operand:SI 2 "register_operand" "d,d,d"))
		 (match_operand:SI 3 "register_operand" "0,l,d")))
   (clobber (match_scratch:SI 4 "=X,3,l"))
   (clobber (match_scratch:SI 5 "=X,X,&d"))]
  "TARGET_MIPS3900 && !TARGET_MIPS16"
  "@
    madd\t%1,%2
    madd\t%0,%1,%2
    #"
  [(set_attr "type"	"imadd")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,4,8")])

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
		 (match_operand:SI 3 "register_operand" "0,l")))
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
   (set_attr "mode" "SI")])

(define_insn "*msac"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,l")
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
   (set_attr "mode"     "SI")])

;; An msac-like instruction implemented using negation and a macc.
(define_insn_and_split "*msac_using_macc"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,l")
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
   (set_attr "length"	"8")])

;; Patterns generated by the define_peephole2 below.

(define_insn "*macc2"
  [(set (match_operand:SI 0 "register_operand" "=l")
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
   (set_attr "mode"	"SI")])

(define_insn "*msac2"
  [(set (match_operand:SI 0 "register_operand" "=l")
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
  [(set (match_operand:SI 0 "register_operand" "=l*?*?,d?")
        (minus:SI (match_operand:SI 1 "register_operand" "0,d")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d")
                           (match_operand:SI 3 "register_operand" "d,d"))))
   (clobber (match_scratch:SI 4 "=X,l"))
   (clobber (match_scratch:SI 5 "=X,&d"))]
  "GENERATE_MADD_MSUB"
  "@
   msub\t%2,%3
   #"
  [(set_attr "type"     "imadd")
   (set_attr "mode"     "SI")
   (set_attr "length"   "4,8")])

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
  "!TARGET_64BIT || !TARGET_FIX_R4000"
{
  if (TARGET_64BIT)
    emit_insn (gen_<u>mulsidi3_64bit (operands[0], operands[1], operands[2]));
  else if (TARGET_FIX_R4000)
    emit_insn (gen_<u>mulsidi3_32bit_r4000 (operands[0], operands[1],
					    operands[2]));
  else
    emit_insn (gen_<u>mulsidi3_32bit (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "<u>mulsidi3_32bit"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_64BIT && !TARGET_FIX_R4000 && !ISA_HAS_DSPR2"
  "mult<u>\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "<u>mulsidi3_32bit_r4000"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (match_scratch:DI 3 "=x"))]
  "!TARGET_64BIT && TARGET_FIX_R4000"
  "mult<u>\t%1,%2\;mflo\t%L0\;mfhi\t%M0"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set_attr "length" "12")])

(define_insn_and_split "<u>mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (match_scratch:TI 3 "=x"))
   (clobber (match_scratch:DI 4 "=d"))]
  "TARGET_64BIT && !TARGET_FIX_R4000"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(unspec:TI [(mult:DI (any_extend:DI (match_dup 1))
			     (any_extend:DI (match_dup 2)))]
		   UNSPEC_SET_HILO))

   ;; OP4 <- LO, OP0 <- HI
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 0) (unspec:DI [(match_dup 3)] UNSPEC_MFHI))

   ;; Zero-extend OP4.
   (set (match_dup 4)
	(ashift:DI (match_dup 4)
		   (const_int 32)))
   (set (match_dup 4)
	(lshiftrt:DI (match_dup 4)
		     (const_int 32)))

   ;; Shift OP0 into place.
   (set (match_dup 0)
	(ashift:DI (match_dup 0)
		   (const_int 32)))

   ;; OR the two halves together
   (set (match_dup 0)
	(ior:DI (match_dup 0)
		(match_dup 4)))]
  { operands[5] = gen_rtx_REG (DImode, LO_REGNUM); }
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set_attr "length" "24")])

(define_insn "<u>mulsidi3_64bit_hilo"
  [(set (match_operand:TI 0 "register_operand" "=x")
	(unspec:TI
	  [(mult:DI
	     (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	     (any_extend:DI (match_operand:SI 2 "register_operand" "d")))]
	  UNSPEC_SET_HILO))]
  "TARGET_64BIT && !TARGET_FIX_R4000"
  "mult<u>\t%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

;; Widening multiply with negation.
(define_insn "*muls<u>_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (neg:DI
	 (mult:DI
	  (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	  (any_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MULS"
  "muls<u>\t$0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_insn "<u>msubsidi4"
  [(set (match_operand:DI 0 "register_operand" "=ka")
        (minus:DI
	   (match_operand:DI 3 "register_operand" "0")
	   (mult:DI
	      (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
	      (any_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && (ISA_HAS_MSAC || GENERATE_MADD_MSUB || ISA_HAS_DSPR2)"
{
  if (ISA_HAS_DSPR2)
    return "msub<u>\t%q0,%1,%2";
  else if (TARGET_MIPS5500 || GENERATE_MADD_MSUB)
    return "msub<u>\t%1,%2";
  else
    return "msac<u>\t$0,%1,%2";
}
  [(set_attr "type" "imadd")
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
  else
    emit_insn (gen_<su>mulsi3_highpart_internal (operands[0], operands[1],
					         operands[2]));
  DONE;
})

(define_insn_and_split "<su>mulsi3_highpart_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		   (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "!ISA_HAS_MULHI"
  { return TARGET_FIX_R4000 ? "mult<u>\t%1,%2\n\tmfhi\t%0" : "#"; }
  "&& reload_completed && !TARGET_FIX_R4000"
  [(const_int 0)]
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
}
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")
   (set_attr "length" "8")])

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
(define_insn_and_split "<su>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		   (any_extend:TI (match_operand:DI 2 "register_operand" "d")))
	  (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))]
  "TARGET_64BIT && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
  { return TARGET_FIX_R4000 ? "dmult<u>\t%1,%2\n\tmfhi\t%0" : "#"; }
  "&& reload_completed && !TARGET_FIX_R4000"
  [(const_int 0)]
{
  rtx hilo;

  hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
  emit_insn (gen_<u>mulditi3_internal (hilo, operands[1], operands[2]));
  emit_insn (gen_mfhidi_ti (operands[0], hilo));
  DONE;
}
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")
   (set_attr "length" "8")])

(define_expand "<u>mulditi3"
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand"))
		 (any_extend:TI (match_operand:DI 2 "register_operand"))))]
  "TARGET_64BIT && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
{
  if (TARGET_FIX_R4000)
    emit_insn (gen_<u>mulditi3_r4000 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_<u>mulditi3_internal (operands[0], operands[1],
					 operands[2]));
  DONE;
})

(define_insn "<u>mulditi3_internal"
  [(set (match_operand:TI 0 "register_operand" "=x")
	(mult:TI (any_extend:TI (match_operand:DI 1 "register_operand" "d"))
		 (any_extend:TI (match_operand:DI 2 "register_operand" "d"))))]
  "TARGET_64BIT
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
  "TARGET_64BIT
   && TARGET_FIX_R4000
   && !(<CODE> == ZERO_EXTEND && TARGET_FIX_VR4120)"
  "dmult<u>\t%1,%2\;mflo\t%L0\;mfhi\t%M0"
  [(set_attr "type" "imul")
   (set_attr "mode" "DI")
   (set_attr "length" "12")])

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
   (set_attr "mode"	"SI")])

(define_insn "<u>maddsidi4"
  [(set (match_operand:DI 0 "register_operand" "=ka")
	(plus:DI
	 (mult:DI (any_extend:DI (match_operand:SI 1 "register_operand" "d"))
		  (any_extend:DI (match_operand:SI 2 "register_operand" "d")))
	 (match_operand:DI 3 "register_operand" "0")))]
  "(TARGET_MAD || ISA_HAS_MACC || GENERATE_MADD_MSUB || ISA_HAS_DSPR2)
   && !TARGET_64BIT"
{
  if (TARGET_MAD)
    return "mad<u>\t%1,%2";
  else if (ISA_HAS_DSPR2)
    return "madd<u>\t%q0,%1,%2";
  else if (GENERATE_MADD_MSUB || TARGET_MIPS5500)
    return "madd<u>\t%1,%2";
  else
    /* See comment in *macc.  */
    return "%[macc<u>\t%@,%1,%2%]";
}
  [(set_attr "type" "imadd")
   (set_attr "mode" "SI")])

;; Floating point multiply accumulate instructions.

(define_insn "*madd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_FP_MADD4_MSUB4 && TARGET_FUSED_MADD"
  "madd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*madd3<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_FP_MADD3_MSUB3 && TARGET_FUSED_MADD"
  "madd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*msub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			       (match_operand:ANYF 2 "register_operand" "f"))
		    (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_FP_MADD4_MSUB4 && TARGET_FUSED_MADD"
  "msub.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*msub3<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			       (match_operand:ANYF 2 "register_operand" "f"))
		    (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_FP_MADD3_MSUB3 && TARGET_FUSED_MADD"
  "msub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (plus:ANYF
		   (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD4_NMSUB4 (<MODE>mode)
   && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd3<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (plus:ANYF
		   (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "0"))))]
  "ISA_HAS_NMADD3_NMSUB3 (<MODE>mode)
   && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (mult:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		    (match_operand:ANYF 2 "register_operand" "f"))
	 (match_operand:ANYF 3 "register_operand" "f")))]
  "ISA_HAS_NMADD4_NMSUB4 (<MODE>mode)
   && TARGET_FUSED_MADD
   && !HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%3,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd3<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (mult:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		    (match_operand:ANYF 2 "register_operand" "f"))
	 (match_operand:ANYF 3 "register_operand" "0")))]
  "ISA_HAS_NMADD3_NMSUB3 (<MODE>mode)
   && TARGET_FUSED_MADD
   && !HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (minus:ANYF
		   (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
			      (match_operand:ANYF 3 "register_operand" "f"))
		   (match_operand:ANYF 1 "register_operand" "f"))))]
  "ISA_HAS_NMADD4_NMSUB4 (<MODE>mode)
   && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub3<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (minus:ANYF
		   (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
			      (match_operand:ANYF 3 "register_operand" "f"))
		   (match_operand:ANYF 1 "register_operand" "0"))))]
  "ISA_HAS_NMADD3_NMSUB3 (<MODE>mode)
   && TARGET_FUSED_MADD
   && HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (match_operand:ANYF 1 "register_operand" "f")
	 (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
		    (match_operand:ANYF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD4_NMSUB4 (<MODE>mode)
   && TARGET_FUSED_MADD
   && !HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub3<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (match_operand:ANYF 1 "register_operand" "f")
	 (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
		    (match_operand:ANYF 3 "register_operand" "0"))))]
  "ISA_HAS_NMADD3_NMSUB3 (<MODE>mode)
   && TARGET_FUSED_MADD
   && !HONOR_SIGNED_ZEROS (<MODE>mode)
   && !HONOR_NANS (<MODE>mode)"
  "nmsub.<fmt>\t%0,%1,%2"
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
    if (!(<recip_condition> && flag_unsafe_math_optimizations))
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
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

(define_insn "*recip<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  "<recip_condition> && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "recip.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "recip.<fmt>\t%0,%2";
}
  [(set_attr "type" "frdiv")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; VR4120 errata MD(A1): signed division instructions do not work correctly
;; with negative operands.  We use special libgcc functions instead.
(define_insn_and_split "divmod<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=l")
	(div:GPR (match_operand:GPR 1 "register_operand" "d")
		 (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(mod:GPR (match_dup 1)
		 (match_dup 2)))]
  "!TARGET_FIX_VR4120"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx hilo;

  if (TARGET_64BIT)
    {
      hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
      emit_insn (gen_divmod<mode>4_hilo_ti (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhi<mode>_ti (operands[3], hilo));
    }
  else
    {
      hilo = gen_rtx_REG (DImode, MD_REG_FIRST);
      emit_insn (gen_divmod<mode>4_hilo_di (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhi<mode>_di (operands[3], hilo));
    }
  DONE;
}
 [(set_attr "type" "idiv")
  (set_attr "mode" "<MODE>")
  (set_attr "length" "8")])

(define_insn_and_split "udivmod<mode>4"
  [(set (match_operand:GPR 0 "register_operand" "=l")
	(udiv:GPR (match_operand:GPR 1 "register_operand" "d")
		  (match_operand:GPR 2 "register_operand" "d")))
   (set (match_operand:GPR 3 "register_operand" "=d")
	(umod:GPR (match_dup 1)
		  (match_dup 2)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx hilo;

  if (TARGET_64BIT)
    {
      hilo = gen_rtx_REG (TImode, MD_REG_FIRST);
      emit_insn (gen_udivmod<mode>4_hilo_ti (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhi<mode>_ti (operands[3], hilo));
    }
  else
    {
      hilo = gen_rtx_REG (DImode, MD_REG_FIRST);
      emit_insn (gen_udivmod<mode>4_hilo_di (hilo, operands[1], operands[2]));
      emit_insn (gen_mfhi<mode>_di (operands[3], hilo));
    }
  DONE;
}
 [(set_attr "type" "idiv")
  (set_attr "mode" "<MODE>")
  (set_attr "length" "8")])

(define_insn "<u>divmod<GPR:mode>4_hilo_<HILO:mode>"
  [(set (match_operand:HILO 0 "register_operand" "=x")
	(unspec:HILO
	  [(any_div:GPR (match_operand:GPR 1 "register_operand" "d")
			(match_operand:GPR 2 "register_operand" "d"))]
	  UNSPEC_SET_HILO))]
  ""
  { return mips_output_division ("<GPR:d>div<u>\t%.,%1,%2", operands); }
  [(set_attr "type" "idiv")
   (set_attr "mode" "<GPR:MODE>")])

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
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

(define_insn "*rsqrt<mode>a"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
		  (sqrt:ANYF (match_operand:ANYF 2 "register_operand" "f"))))]
  "<recip_condition> && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "rsqrt.<fmt>\t%0,%2";
}
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

(define_insn "*rsqrt<mode>b"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (div:ANYF (match_operand:ANYF 1 "const_1_operand" "")
			     (match_operand:ANYF 2 "register_operand" "f"))))]
  "<recip_condition> && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.<fmt>\t%0,%2\;mov.<fmt>\t%0,%0";
  else
    return "rsqrt.<fmt>\t%0,%2";
}
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

;; Do not use the integer abs macro instruction, since that signals an
;; exception on -2147483648 (sigh).

;; abs.fmt is an arithmetic instruction and treats all NaN inputs as
;; invalid; it does not clear their sign bits.  We therefore can't use
;; abs.fmt if the signs of NaNs matter.

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "!HONOR_NANS (<MODE>mode)"
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
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "dsubu\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

;; neg.fmt is an arithmetic instruction and treats all NaN inputs as
;; invalid; it does not flip their sign bit.  We therefore can't use
;; neg.fmt if the signs of NaNs matter.

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  "!HONOR_NANS (<MODE>mode)"
  "neg.<fmt>\t%0,%1"
  [(set_attr "type" "fneg")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(not:GPR (match_operand:GPR 1 "register_operand" "d")))]
  ""
{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  else
    return "nor\t%0,%.,%1";
}
  [(set_attr "type" "logical")
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
;; zero_extendsidi2 because of TRULY_NOOP_TRUNCATION, so handle these here.
;; Note that this variant does not trigger for SI mode because we require
;; a 64-bit HOST_WIDE_INT and 0xffff_ffff wouldn't be a canonical
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
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,d,d,d,d")
	(and:GPR (match_operand:GPR 1 "nonimmediate_operand" "o,o,W,d,d,d,d")
		 (match_operand:GPR 2 "and_operand" "Yb,Yh,Yw,K,Yx,Yw,d")))]
  "!TARGET_MIPS16 && and_operands_ok (<MODE>mode, operands[1], operands[2])"
{
  int len;

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
      return "andi\t%0,%1,%x2";
    case 4:
      len = low_bitmask_len (<MODE>mode, INTVAL (operands[2]));
      operands[2] = GEN_INT (len);
      return "<d>ext\t%0,%1,0,%2";
    case 5:
      return "#";
    case 6:
      return "and\t%0,%1,%2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "move_type" "load,load,load,andi,ext_ins,shift_shift,logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*and<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d,d,d,d,d")
	(and:GPR (match_operand:GPR 1 "nonimmediate_operand" "%o,o,W,d,0")
		 (match_operand:GPR 2 "and_operand" "Yb,Yh,Yw,Yw,d")))]
  "TARGET_MIPS16 && and_operands_ok (<MODE>mode, operands[1], operands[2])"
{
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
      return "#";
    case 4:
      return "and\t%0,%2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "move_type" "load,load,load,shift_shift,logical")
   (set_attr "mode" "<MODE>")])

(define_expand "ior<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(ior:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "uns_arith_operand")))]
  ""
{
  if (TARGET_MIPS16)
    operands[2] = force_reg (<MODE>mode, operands[2]);
})

(define_insn "*ior<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(ior:GPR (match_operand:GPR 1 "register_operand" "%d,d")
		 (match_operand:GPR 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   or\t%0,%1,%2
   ori\t%0,%1,%x2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "*ior<mode>3_mips16"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(ior:GPR (match_operand:GPR 1 "register_operand" "%0")
		 (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "or\t%0,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_expand "xor<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(xor:GPR (match_operand:GPR 1 "register_operand")
		 (match_operand:GPR 2 "uns_arith_operand")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(xor:GPR (match_operand:GPR 1 "register_operand" "%d,d")
		 (match_operand:GPR 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   xor\t%0,%1,%2
   xori\t%0,%1,%x2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn ""
  [(set (match_operand:GPR 0 "register_operand" "=d,t,t")
	(xor:GPR (match_operand:GPR 1 "register_operand" "%0,d,d")
		 (match_operand:GPR 2 "uns_arith_operand" "d,K,d")))]
  "TARGET_MIPS16"
  "@
   xor\t%0,%2
   cmpi\t%1,%2
   cmp\t%1,%2"
  [(set_attr "type" "logical,arith,arith")
   (set_attr "mode" "<MODE>")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

(define_insn "*nor<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(and:GPR (not:GPR (match_operand:GPR 1 "register_operand" "d"))
		 (not:GPR (match_operand:GPR 2 "register_operand" "d"))))]
  "!TARGET_MIPS16"
  "nor\t%0,%1,%2"
  [(set_attr "type" "logical")
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
;; (see TRULY_NOOP_TRUNCATION).  Truncating DImode values into modes
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

;; Logical shift by 32 or more results in proper SI values so
;; truncation is removed by the middle end.
(define_insn "*<optab>_trunc<mode>_exts"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	 (any_shiftrt:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "const_arith_operand" ""))))]
  "ISA_HAS_EXTS && TARGET_64BIT && UINTVAL (operands[2]) < 32"
  "exts\t%0,%1,%2,31"
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

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
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (zero_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,<SHORT:mask>
   l<SHORT:size>u\t%0,%1"
  [(set_attr "move_type" "andi,load")
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
  [(set_attr "type" "logical")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*zero_extendhi_truncqi"
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\t%0,%1,0xff"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

;; When TARGET_64BIT, all SImode integer registers should already be in
;; sign-extended form (see TRULY_NOOP_TRUNCATION and truncdisi2).  We can
;; therefore get rid of register->register instructions if we constrain
;; the source to be in the same register as the destination.
;;
;; The register alternative has type "arith" so that the pre-reload
;; scheduler will treat it as a move.  This reflects what happens if
;; the register alternative needs a reload.
(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "0,m")))]
  "TARGET_64BIT"
  "@
   #
   lw\t%0,%1"
  "&& reload_completed && register_operand (operands[1], VOIDmode)"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
}
  [(set_attr "move_type" "move,load")
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
   (set_attr "length"	"36")])

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
   (set_attr "length"	"36")])


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
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31, DFmode);

  if (reg1)			/* Turn off complaints about unreached code.  */
    {
      mips_emit_move (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, DFmode));
      do_pending_stack_adjust ();

      test = gen_rtx_GE (VOIDmode, operands[1], reg1);
      emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

      emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
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
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63, DFmode);

  mips_emit_move (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, DFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchdf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncdfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
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
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31, SFmode);

  mips_emit_move (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
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
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  rtx test;
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63, SFmode);

  mips_emit_move (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, SFmode));
  do_pending_stack_adjust ();

  test = gen_rtx_GE (VOIDmode, operands[1], reg1);
  emit_jump_insn (gen_cbranchsf4 (test, operands[1], reg1, label1));

  emit_insn (gen_fix_truncsfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
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

(define_expand "extv"
  [(set (match_operand 0 "register_operand")
	(sign_extract (match_operand 1 "nonimmediate_operand")
		      (match_operand 2 "const_int_operand")
		      (match_operand 3 "const_int_operand")))]
  "!TARGET_MIPS16"
{
  if (mips_expand_ext_as_unaligned_load (operands[0], operands[1],
					 INTVAL (operands[2]),
					 INTVAL (operands[3])))
    DONE;
  else if (register_operand (operands[1], GET_MODE (operands[0]))
	   && ISA_HAS_EXTS && UINTVAL (operands[2]) <= 32)
    {
      if (GET_MODE (operands[0]) == DImode)
	emit_insn (gen_extvdi (operands[0], operands[1], operands[2],
			       operands[3]));
      else
	emit_insn (gen_extvsi (operands[0], operands[1], operands[2],
			       operands[3]));
      DONE;
    }
  else
    FAIL;
})

(define_insn "extv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (sign_extract:GPR (match_operand:GPR 1 "register_operand" "d")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "ISA_HAS_EXTS && UINTVAL (operands[2]) <= 32"
  "exts\t%0,%1,%3,%m2"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "<MODE>")])


(define_expand "extzv"
  [(set (match_operand 0 "register_operand")
	(zero_extract (match_operand 1 "nonimmediate_operand")
		      (match_operand 2 "const_int_operand")
		      (match_operand 3 "const_int_operand")))]
  "!TARGET_MIPS16"
{
  if (mips_expand_ext_as_unaligned_load (operands[0], operands[1],
					 INTVAL (operands[2]),
					 INTVAL (operands[3])))
    DONE;
  else if (mips_use_ins_ext_p (operands[1], INTVAL (operands[2]),
			       INTVAL (operands[3])))
    {
      if (GET_MODE (operands[0]) == DImode)
        emit_insn (gen_extzvdi (operands[0], operands[1], operands[2],
				operands[3]));
      else
        emit_insn (gen_extzvsi (operands[0], operands[1], operands[2],
				operands[3]));
      DONE;
    }
  else
    FAIL;
})

(define_insn "extzv<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(zero_extract:GPR (match_operand:GPR 1 "register_operand" "d")
			  (match_operand 2 "const_int_operand" "")
			  (match_operand 3 "const_int_operand" "")))]
  "mips_use_ins_ext_p (operands[1], INTVAL (operands[2]),
		       INTVAL (operands[3]))"
  "<d>ext\t%0,%1,%3,%2"
  [(set_attr "type"	"arith")
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


(define_expand "insv"
  [(set (zero_extract (match_operand 0 "nonimmediate_operand")
		      (match_operand 1 "immediate_operand")
		      (match_operand 2 "immediate_operand"))
	(match_operand 3 "reg_or_0_operand"))]
  "!TARGET_MIPS16"
{
  if (mips_expand_ins_as_unaligned_store (operands[0], operands[3],
					  INTVAL (operands[1]),
					  INTVAL (operands[2])))
    DONE;
  else if (mips_use_ins_ext_p (operands[0], INTVAL (operands[1]),
			       INTVAL (operands[2])))
    {
      if (GET_MODE (operands[0]) == DImode)
        emit_insn (gen_insvdi (operands[0], operands[1], operands[2],
			       operands[3]));
      else
        emit_insn (gen_insvsi (operands[0], operands[1], operands[2],
			       operands[3]));
      DONE;
   }
   else
     FAIL;
})

(define_insn "insv<mode>"
  [(set (zero_extract:GPR (match_operand:GPR 0 "register_operand" "+d")
			  (match_operand:SI 1 "immediate_operand" "I")
			  (match_operand:SI 2 "immediate_operand" "I"))
	(match_operand:GPR 3 "reg_or_0_operand" "dJ"))]
  "mips_use_ins_ext_p (operands[0], INTVAL (operands[1]),
		       INTVAL (operands[2]))"
  "<d>ins\t%0,%z3,%2,%1"
  [(set_attr "type"	"arith")
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
		     (match_operand:QI 2 "memory_operand" "m")]
		    UNSPEC_LOAD_LEFT))]
  "!TARGET_MIPS16 && mips_mem_fits_mode_p (<MODE>mode, operands[1])"
  "<load>l\t%0,%2"
  [(set_attr "move_type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "mov_<load>r"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(unspec:GPR [(match_operand:BLK 1 "memory_operand" "m")
		     (match_operand:QI 2 "memory_operand" "m")
		     (match_operand:GPR 3 "register_operand" "0")]
		    UNSPEC_LOAD_RIGHT))]
  "!TARGET_MIPS16 && mips_mem_fits_mode_p (<MODE>mode, operands[1])"
  "<load>r\t%0,%2"
  [(set_attr "move_type" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "mov_<store>l"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
	(unspec:BLK [(match_operand:GPR 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")]
		    UNSPEC_STORE_LEFT))]
  "!TARGET_MIPS16 && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>l\t%z1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "mov_<store>r"
  [(set (match_operand:BLK 0 "memory_operand" "+m")
	(unspec:BLK [(match_operand:GPR 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")
		     (match_dup 0)]
		    UNSPEC_STORE_RIGHT))]
  "!TARGET_MIPS16 && mips_mem_fits_mode_p (<MODE>mode, operands[0])"
  "<store>r\t%z1,%2"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

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
  [(set_attr "length" "20")])

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
  "TARGET_EXPLICIT_RELOCS && ABI_HAS_64BIT_SYMBOLS && cse_not_expected"
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
  [(set_attr "length" "24")])

;; Split HIGHs into:
;;
;;	li op0,%hi(sym)
;;	sll op0,16
;;
;; on MIPS16 targets.
(define_split
  [(set (match_operand:SI 0 "d_operand")
	(high:SI (match_operand:SI 1 "absolute_symbolic_operand")))]
  "TARGET_MIPS16 && reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (ashift:SI (match_dup 0) (const_int 16)))]
{
  operands[2] = mips_unspec_address (operands[1], SYMBOL_32_HIGH);
})

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
(define_expand "unspec_got<mode>"
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
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "*low<mode>_mips16"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "0")
		  (match_operand:P 2 "immediate_operand" "")))]
  "TARGET_MIPS16"
  "<d>addiu\t%0,%R2"
  [(set_attr "type" "arith")
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
	(reg:GPR 31))]
  "TARGET_MIPS16"
  "<store>\t$31,%0"
  [(set_attr "move_type" "store")
   (set_attr "mode" "<MODE>")])

(define_insn "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,m,*a,*d,*f,*f,*d,*m,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "move_operand" "d,i,m,d,*J*d,*a,*J*d,*m,*f,*f,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mthilo,mfhilo,mtc,fpload,mfc,fpstore,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_32bit_mips16"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:DI 1 "move_operand" "d,d,y,K,N,m,d,*x"))]
  "!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mfhilo")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*d,*m,*a,*d,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "move_operand" "d,U,T,m,dJ,*d*J,*m,*f,*f,*J*d,*a,*d,*m,*B*C*D,*B*C*D"))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store,mtc,fpload,mfc,fpstore,mthilo,mfhilo,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit_mips16"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,m,*d")
	(match_operand:DI 1 "move_operand" "d,d,y,K,N,U,kf,m,d,*a"))]
  "TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,const,loadpool,load,store,mfhilo")
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
  [(set (match_operand:IMOVE32 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*d,*m,*d,*z,*a,*d,*B*C*D,*B*C*D,*d,*m")
	(match_operand:IMOVE32 1 "move_operand" "d,U,T,m,dJ,*d*J,*m,*f,*f,*z,*d,*J*d,*a,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], <MODE>mode)
       || reg_or_0_operand (operands[1], <MODE>mode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store,mtc,fpload,mfc,fpstore,mfc,mtc,mthilo,mfhilo,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "SI")])

(define_insn "*mov<mode>_mips16"
  [(set (match_operand:IMOVE32 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,m,*d")
	(match_operand:IMOVE32 1 "move_operand" "d,d,y,K,N,U,kf,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,const,loadpool,load,store,mfhilo")
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

;; This insn handles moving CCmode values.  It's really just a
;; slightly simplified copy of movsi_internal2, with additional cases
;; to move a condition register to a general register and to move
;; between the general registers and the floating point registers.

(define_insn "movcc"
  [(set (match_operand:CC 0 "nonimmediate_operand" "=d,*d,*d,*m,*d,*f,*f,*f,*m")
	(match_operand:CC 1 "general_operand" "z,*d,*m,*d,*f,*d,*f,*m,*f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "lui_movf,move,load,store,mfc,mtc,fmove,fpload,fpstore")
   (set_attr "mode" "SI")])

;; Reload condition code registers.  reload_incc and reload_outcc
;; both handle moves from arbitrary operands into condition code
;; registers.  reload_incc handles the more common case in which
;; a source operand is constrained to be in a condition-code
;; register, but has not been allocated to one.
;;
;; Sometimes, such as in movcc, we have a CCmode destination whose
;; constraints do not include 'z'.  reload_outcc handles the case
;; when such an operand is allocated to a condition-code register.
;;
;; Note that reloads from a condition code register to some
;; other location can be done using ordinary moves.  Moving
;; into a GPR takes a single movcc, moving elsewhere takes
;; two.  We can leave these cases to the generic reload code.
(define_expand "reload_incc"
  [(set (match_operand:CC 0 "fcc_reload_operand" "=z")
	(match_operand:CC 1 "general_operand" ""))
   (clobber (match_operand:TF 2 "register_operand" "=&f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
{
  mips_expand_fcc_reload (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "reload_outcc"
  [(set (match_operand:CC 0 "fcc_reload_operand" "=z")
	(match_operand:CC 1 "register_operand" ""))
   (clobber (match_operand:TF 2 "register_operand" "=&f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
{
  mips_expand_fcc_reload (operands[0], operands[1], operands[2]);
  DONE;
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
  "ISA_HAS_FP4"
  "<ANYF:loadx>\t%0,%1(%2)"
  [(set_attr "type" "fpidxload")
   (set_attr "mode" "<ANYF:UNITMODE>")])

(define_insn "*<ANYF:storex>_<P:mode>"
  [(set (mem:ANYF (plus:P (match_operand:P 1 "register_operand" "d")
			  (match_operand:P 2 "register_operand" "d")))
	(match_operand:ANYF 0 "register_operand" "f"))]
  "ISA_HAS_FP4"
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
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,m,*a,*d")
	(match_operand:HI 1 "move_operand"         "d,I,m,dJ,*d*J,*a"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || reg_or_0_operand (operands[1], HImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mthilo,mfhilo")
   (set_attr "mode" "HI")])

(define_insn "*movhi_mips16"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:HI 1 "move_operand"         "d,d,y,K,N,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mfhilo")
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
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,m,*a,*d")
	(match_operand:QI 1 "move_operand"         "d,I,m,dJ,*d*J,*a"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || reg_or_0_operand (operands[1], QImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mthilo,mfhilo")
   (set_attr "mode" "QI")])

(define_insn "*movqi_mips16"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:QI 1 "move_operand"         "d,d,y,K,N,m,d,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mfhilo")
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
  [(set (match_operand:TI 0 "nonimmediate_operand" "=d,d,d,m,*a,*d")
	(match_operand:TI 1 "move_operand" "d,i,m,dJ,*d*J,*a"))]
  "TARGET_64BIT
   && !TARGET_MIPS16
   && (register_operand (operands[0], TImode)
       || reg_or_0_operand (operands[1], TImode))"
  "#"
  [(set_attr "move_type" "move,const,load,store,mthilo,mfhilo")
   (set_attr "mode" "TI")])

(define_insn "*movti_mips16"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:TI 1 "move_operand" "d,d,y,K,N,m,d,*a"))]
  "TARGET_64BIT
   && TARGET_MIPS16
   && (register_operand (operands[0], TImode)
       || register_operand (operands[1], TImode))"
  "#"
  [(set_attr "move_type" "move,move,move,const,constN,load,store,mfhilo")
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
  "reload_completed && !TARGET_64BIT
   && mips_split_64bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  mips_split_doubleword_move (operands[0], operands[1]);
  DONE;
})

(define_split
  [(set (match_operand:MOVE128 0 "nonimmediate_operand")
	(match_operand:MOVE128 1 "move_operand"))]
  "TARGET_64BIT && reload_completed"
  [(const_int 0)]
{
  mips_split_doubleword_move (operands[0], operands[1]);
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
	(unspec:GPR [(match_operand:HILO 1 "register_operand" "x")]
		    UNSPEC_MFHI))]
  ""
  { return ISA_HAS_MACCHI ? "<GPR:d>macchi\t%0,%.,%." : "mfhi\t%0"; }
  [(set_attr "move_type" "mfhilo")
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
  [(set_attr "move_type" "mthilo")
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
      if (TARGET_FLOAT64 && !TARGET_64BIT)
      	emit_insn (gen_mthc1<mode> (operands[0], high, operands[0]));
      else
	emit_insn (gen_load_high<mode> (operands[0], high, operands[0]));
    }
  else
    {
      rtx low = mips_subword (operands[0], 0);
      rtx high = mips_subword (operands[0], 1);
      emit_insn (gen_store_word<mode> (low, operands[1], const0_rtx));
      if (TARGET_FLOAT64 && !TARGET_64BIT)
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
  [(set (match_operand:P 0 "register_operand" "=d")
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
(define_insn_and_split "copygp_mips16"
  [(set (match_operand:SI 0 "register_operand" "=y")
	(unspec:SI [(match_operand:SI 1 "register_operand" "d")]
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
(define_insn_and_split "potential_cprestore"
  [(set (match_operand:SI 0 "cprestore_save_slot_operand" "=X,X")
	(unspec:SI [(match_operand:SI 1 "const_int_operand" "I,i")
		    (match_operand:SI 2 "register_operand" "d,d")]
		   UNSPEC_POTENTIAL_CPRESTORE))
   (clobber (match_operand:SI 3 "scratch_operand" "=X,&d"))]
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
(define_insn "cprestore"
  [(set (match_operand:SI 0 "cprestore_save_slot_operand" "=X,X")
	(unspec:SI [(match_operand:SI 1 "const_int_operand" "I,i")
		    (reg:SI 28)]
		   UNSPEC_CPRESTORE))]
  "TARGET_CPRESTORE_DIRECTIVE"
{
  if (mips_nomacro.nesting_level > 0 && which_alternative == 1)
    return ".set\tmacro\;.cprestore\t%1\;.set\tnomacro";
  else
    return ".cprestore\t%1";
}
  [(set_attr "type" "store")
   (set_attr "length" "4,12")])

(define_insn "use_cprestore"
  [(set (reg:SI CPRESTORE_SLOT_REGNUM)
	(match_operand:SI 0 "cprestore_load_slot_operand"))]
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
      emit_insn (Pmode == SImode
		 ? gen_clear_hazard_si ()
		 : gen_clear_hazard_di ());
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
        (unspec_volatile [(const_int 1)]
        UNSPEC_RDHWR))]
  "ISA_HAS_SYNCI"
  "rdhwr\t%0,$1")

(define_insn "clear_hazard_<mode>"
  [(unspec_volatile [(const_int 0)] UNSPEC_CLEAR_HAZARD)
   (clobber (reg:P 31))]
  "ISA_HAS_SYNCI"
{
  return "%(%<bal\t1f\n"
         "\tnop\n"
         "1:\t<d>addiu\t$31,$31,12\n"
         "\tjr.hb\t$31\n"
         "\tnop%>%)";
}
  [(set_attr "length" "20")])

;; Cache operations for R4000-style caches.
(define_insn "mips_cache"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "const_int_operand")
		     (match_operand:QI 1 "address_operand" "p")]
		    UNSPEC_MIPS_CACHE))]
  "ISA_HAS_CACHE"
  "cache\t%X0,%a1")

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

;; Block moves, see mips.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movmemsi"
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
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(any_shift:GPR (match_operand:GPR 1 "register_operand" "d")
		       (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (<MODE>mode) - 1));

  return "<d><insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
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
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(any_shift:SI (match_operand:SI 1 "register_operand" "0,d")
		      (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "<insn>\t%0,%2";

  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "<insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand 2 "m16_uimm3_b")
			       (const_int 4)
			       (const_int 8))])])

;; We need separate DImode MIPS16 patterns because of the irregularity
;; of right shifts.
(define_insn "*ashldi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0,d")
		   (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "dsll\t%0,%2";

  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);
  return "dsll\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand 2 "m16_uimm3_b")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "*ashrdi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand 2 "m16_uimm3_b")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "*lshrdi3_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand 2 "m16_uimm3_b")
			       (const_int 4)
			       (const_int 8))])])

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
;; The length here is the worst case: the length of the split version
;; will be more accurate.
(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (match_operand:SI 2 "immediate_operand" "I")))]
  "TARGET_MIPS16"
  "#"
  ""
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"16")])

(define_insn "rotr<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(rotatert:GPR (match_operand:GPR 1 "register_operand" "d")
		      (match_operand:SI 2 "arith_operand" "dI")))]
  "ISA_HAS_ROR"
{
  if (CONST_INT_P (operands[2]))
    gcc_assert (INTVAL (operands[2]) >= 0
		&& INTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode));

  return "<d>ror\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches on floating-point equality tests.

(define_insn "*branch_fp"
  [(set (pc)
        (if_then_else
         (match_operator 1 "equality_operator"
                         [(match_operand:CC 2 "register_operand" "z")
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

(define_insn "*branch_fp_inverted"
  [(set (pc)
        (if_then_else
         (match_operator 1 "equality_operator"
                         [(match_operand:CC 2 "register_operand" "z")
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
			 [(match_operand:GPR 2 "register_operand" "d")
			  (const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "!TARGET_MIPS16"
  { return mips_output_order_conditional_branch (insn, operands, false); }
  [(set_attr "type" "branch")])

(define_insn "*branch_order<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (const_int 0)])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  "!TARGET_MIPS16"
  { return mips_output_order_conditional_branch (insn, operands, true); }
  [(set_attr "type" "branch")])

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
{
  return mips_output_conditional_branch (insn, operands,
					 MIPS_BRANCH ("b%C1", "%2,%z3,%0"),
					 MIPS_BRANCH ("b%N1", "%2,%z3,%0"));
}
  [(set_attr "type" "branch")])

(define_insn "*branch_equality<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "equality_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (match_operand:GPR 3 "reg_or_0_operand" "dJ")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn, operands,
					 MIPS_BRANCH ("b%N1", "%2,%z3,%0"),
					 MIPS_BRANCH ("b%C1", "%2,%z3,%0"));
}
  [(set_attr "type" "branch")])

;; MIPS16 branches

(define_insn "*branch_equality<mode>_mips16"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
			 [(match_operand:GPR 1 "register_operand" "d,t")
			  (const_int 0)])
	 (match_operand 2 "pc_or_label_operand" "")
	 (match_operand 3 "pc_or_label_operand" "")))]
  "TARGET_MIPS16"
{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "b%C0z\t%1,%2";
      else
	return "bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "b%N0z\t%1,%3";
      else
	return "bt%N0z\t%3";
    }
}
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
  [(set (match_operand:GPR2 0 "register_operand" "=t,t")
	(any_lt:GPR2 (match_operand:GPR 1 "register_operand" "d,d")
		     (match_operand:GPR 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "slt<u>\t%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand 2 "m16_uimm8_1")
			       (const_int 4)
			       (const_int 8))])])

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
  [(set (match_operand:GPR2 0 "register_operand" "=t")
	(any_le:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "sle_operand" "")))]
  "TARGET_MIPS16"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>\t%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")
   (set (attr "length") (if_then_else (match_operand 2 "m16_uimm8_m1_1")
				      (const_int 4)
				      (const_int 8)))])

;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................

(define_insn "s<code>_<mode>"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(fcond:CC (match_operand:SCALARF 1 "register_operand" "f")
		  (match_operand:SCALARF 2 "register_operand" "f")))]
  ""
  "c.<fcond>.<fmt>\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "s<code>_<mode>"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(swapped_fcond:CC (match_operand:SCALARF 1 "register_operand" "f")
		          (match_operand:SCALARF 2 "register_operand" "f")))]
  ""
  "c.<swapped_fcond>.<fmt>\t%Z0%2,%1"
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
  { return MIPS_ABSOLUTE_JUMP ("%*j\t%l0%/"); }
  [(set_attr "type" "jump")])

(define_insn "*jump_pic"
  [(set (pc)
	(label_ref (match_operand 0)))]
  "!TARGET_MIPS16 && !TARGET_ABSOLUTE_JUMPS"
{
  if (get_attr_length (insn) <= 8)
    return "%*b\t%l0%/";
  else
    {
      mips_output_load_label (operands[0]);
      return "%*jr\t%@%/%]";
    }
}
  [(set_attr "type" "branch")])

;; We need a different insn for the mips16, because a mips16 branch
;; does not have a delay slot.

(define_insn "*jump_mips16"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_MIPS16"
  "b\t%l0"
  [(set_attr "type" "branch")])

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
  [(set (pc) (match_operand:P 0 "register_operand" "d"))]
  ""
  "%*j\t%0%/"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand"))
   (use (label_ref (match_operand 1 "")))]
  ""
{
  if (TARGET_MIPS16_SHORT_JUMP_TABLES)
    operands[0] = expand_binop (Pmode, add_optab,
				convert_to_mode (Pmode, operands[0], false),
				gen_rtx_LABEL_REF (Pmode, operands[1]),
				0, 0, OPTAB_WIDEN);
  else if (TARGET_GPWORD)
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

  if (Pmode == SImode)
    emit_jump_insn (gen_tablejumpsi (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejumpdi (operands[0], operands[1]));
  DONE;
})

(define_insn "tablejump<mode>"
  [(set (pc)
	(match_operand:P 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "%*j\t%0%/"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; For TARGET_USE_GOT, we save the gp in the jmp_buf as well.
;; While it is possible to either pull it off the stack (in the
;; o32 case) or recalculate it given t9 and our target label,
;; it takes 3 or 4 insns to do so.

(define_expand "builtin_setjmp_setup"
  [(use (match_operand 0 "register_operand"))]
  "TARGET_USE_GOT"
{
  rtx addr;

  addr = plus_constant (operands[0], GET_MODE_SIZE (Pmode) * 3);
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
  rtx lab = gen_rtx_MEM (Pmode, plus_constant (operands[0], 1*W));
  rtx stack = gen_rtx_MEM (Pmode, plus_constant (operands[0], 2*W));
  rtx gpv = gen_rtx_MEM (Pmode, plus_constant (operands[0], 3*W));
  rtx pv = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
  /* Use gen_raw_REG to avoid being given pic_offset_table_rtx.
     The target is bound to be using $28 as the global pointer
     but the current function might not be.  */
  rtx gp = gen_raw_REG (Pmode, GLOBAL_POINTER_REGNUM);

  /* This bit is similar to expand_builtin_longjmp except that it
     restores $gp as well.  */
  mips_emit_move (hard_frame_pointer_rtx, fp);
  mips_emit_move (pv, lab);
  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);
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
  [(return)]
  "mips_can_use_return_insn ()"
  { mips_expand_before_return (); })

(define_insn "*return"
  [(return)]
  "mips_can_use_return_insn ()"
  "%*j\t$31%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "return_internal"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "%*j\t%0%/"
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
(define_insn "mips_rdpgpr"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand" "d")]
			    UNSPEC_RDPGPR))]
  ""
  "rdpgpr\t%0,%1"
  [(set_attr "type"	"move")
   (set_attr "mode"	"SI")])

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
  if (HAVE_restore_gp)
    emit_insn (gen_restore_gp ());
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
(define_insn_and_split "restore_gp"
  [(set (reg:SI 28)
	(unspec_volatile:SI [(const_int 0)] UNSPEC_RESTORE_GP))
   (clobber (match_scratch:SI 0 "=&d"))]
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
  { return MIPS_CALL ("j", operands, 0, 1); }
  [(set_attr "type" "call")])

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
  { return MIPS_CALL ("j", operands, 1, 2); }
  [(set_attr "type" "call")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  { return MIPS_CALL ("j", operands, 1, 2); }
  [(set_attr "type" "call")])

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
;; ??? The operands[2] = insn check is a hack to make the original insn
;; available to the splitter.
(define_insn_and_split "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c,S"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : MIPS_CALL ("jal", operands, 0, 1); }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[2] = insn)"
  [(const_int 0)]
{
  mips_split_call (operands[2], gen_call_split (operands[0], operands[1]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_split"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "cS"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return MIPS_CALL ("jal", operands, 0, 1); }
  [(set_attr "type" "call")])

;; A pattern for calls that must be made directly.  It is used for
;; MIPS16 calls that the linker may need to redirect to a hard-float
;; stub; the linker relies on the call relocation type to detect when
;; such redirection is needed.
(define_insn_and_split "call_internal_direct"
  [(call (mem:SI (match_operand 0 "const_call_insn_operand"))
	 (match_operand 1))
   (const_int 1)
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : MIPS_CALL ("jal", operands, 0, -1); }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[2] = insn)"
  [(const_int 0)]
{
  mips_split_call (operands[2],
		   gen_call_direct_split (operands[0], operands[1]));
  DONE;
}
  [(set_attr "type" "call")])

(define_insn "call_direct_split"
  [(call (mem:SI (match_operand 0 "const_call_insn_operand"))
	 (match_operand 1))
   (const_int 1)
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return MIPS_CALL ("jal", operands, 0, -1); }
  [(set_attr "type" "call")])

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
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : MIPS_CALL ("jal", operands, 1, 2); }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[3] = insn)"
  [(const_int 0)]
{
  mips_split_call (operands[3],
		   gen_call_value_split (operands[0], operands[1],
					 operands[2]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_value_split"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "cS"))
              (match_operand 2 "" "")))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return MIPS_CALL ("jal", operands, 1, 2); }
  [(set_attr "type" "call")])

;; See call_internal_direct.
(define_insn_and_split "call_value_internal_direct"
  [(set (match_operand 0 "register_operand")
        (call (mem:SI (match_operand 1 "const_call_insn_operand"))
              (match_operand 2)))
   (const_int 1)
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : MIPS_CALL ("jal", operands, 1, -1); }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[3] = insn)"
  [(const_int 0)]
{
  mips_split_call (operands[3],
		   gen_call_value_direct_split (operands[0], operands[1],
						operands[2]));
  DONE;
}
  [(set_attr "type" "call")])

(define_insn "call_value_direct_split"
  [(set (match_operand 0 "register_operand")
        (call (mem:SI (match_operand 1 "const_call_insn_operand"))
              (match_operand 2)))
   (const_int 1)
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return MIPS_CALL ("jal", operands, 1, -1); }
  [(set_attr "type" "call")])

;; See comment for call_internal.
(define_insn_and_split "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : MIPS_CALL ("jal", operands, 1, 2); }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[4] = insn)"
  [(const_int 0)]
{
  mips_split_call (operands[4],
		   gen_call_value_multiple_split (operands[0], operands[1],
						  operands[2], operands[3]));
  DONE;
}
  [(set_attr "jal" "indirect,direct")])

(define_insn "call_value_multiple_split"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "cS"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  { return MIPS_CALL ("jal", operands, 1, 2); }
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

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

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
  [(prefetch (match_operand:QI 0 "address_operand" "p")
	     (match_operand 1 "const_int_operand" "n")
	     (match_operand 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && TARGET_EXPLICIT_RELOCS"
{
  if (TARGET_LOONGSON_2EF)
    /* Loongson 2[ef] use load to $0 to perform prefetching.  */
    return "ld\t$0,%a0";
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

;; MIPS4 Conditional move instructions.

(define_insn "*mov<GPR:mode>_on_<MOVECC:mode>"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(if_then_else:GPR
	 (match_operator:MOVECC 4 "equality_operator"
		[(match_operand:MOVECC 1 "register_operand" "<MOVECC:reg>,<MOVECC:reg>")
		 (const_int 0)])
	 (match_operand:GPR 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:GPR 3 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE"
  "@
    mov%T4\t%0,%z2,%1
    mov%t4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*mov<SCALARF:mode>_on_<MOVECC:mode>"
  [(set (match_operand:SCALARF 0 "register_operand" "=f,f")
	(if_then_else:SCALARF
	 (match_operator:MOVECC 4 "equality_operator"
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

;; These are the main define_expand's used to make conditional moves.

(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:GPR 0 "register_operand")
	(if_then_else:GPR (match_dup 5)
			  (match_operand:GPR 2 "reg_or_0_operand")
			  (match_operand:GPR 3 "reg_or_0_operand")))]
  "ISA_HAS_CONDMOVE"
{
  mips_expand_conditional_move (operands);
  DONE;
})

(define_expand "mov<mode>cc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:SCALARF 0 "register_operand")
	(if_then_else:SCALARF (match_dup 5)
			      (match_operand:SCALARF 2 "register_operand")
			      (match_operand:SCALARF 3 "register_operand")))]
  "ISA_HAS_FP_CONDMOVE"
{
  mips_expand_conditional_move (operands);
  DONE;
})

;;
;;  ....................
;;
;;	mips16 inline constant tables
;;
;;  ....................
;;

(define_insn "consttable_int"
  [(unspec_volatile [(match_operand 0 "consttable_operand" "")
		     (match_operand 1 "const_int_operand" "")]
		    UNSPEC_CONSTTABLE_INT)]
  "TARGET_MIPS16"
{
  assemble_integer (operands[0], INTVAL (operands[1]),
		    BITS_PER_UNIT * INTVAL (operands[1]), 1);
  return "";
}
  [(set (attr "length") (symbol_ref "INTVAL (operands[1])"))])

(define_insn "consttable_float"
  [(unspec_volatile [(match_operand 0 "consttable_operand" "")]
		    UNSPEC_CONSTTABLE_FLOAT)]
  "TARGET_MIPS16"
{
  REAL_VALUE_TYPE d;

  gcc_assert (GET_CODE (operands[0]) == CONST_DOUBLE);
  REAL_VALUE_FROM_CONST_DOUBLE (d, operands[0]);
  assemble_real (d, GET_MODE (operands[0]),
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
   ; Since rdhwr always generates a trap for now, putting it in a delay
   ; slot would make the kernel's emulation of it much slower.
   (set_attr "can_delay" "no")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "8")])

(define_insn "*tls_get_tp_<mode>_split"
  [(set (reg:P TLS_GET_TP_REGNUM)
	(unspec:P [(const_int 0)] UNSPEC_TLS_GET_TP))]
  "HAVE_AS_TLS && !TARGET_MIPS16"
  ".set\tpush\;.set\tmips32r2\t\;rdhwr\t$3,$29\;.set\tpop"
  [(set_attr "type" "unknown")
   ; See tls_get_tp_<mode>
   (set_attr "can_delay" "no")
   (set_attr "mode" "<MODE>")])

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

; ST-Microelectronics Loongson-2E/2F-specific patterns.
(include "loongson.md")
