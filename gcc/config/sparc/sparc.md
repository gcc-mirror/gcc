;; Machine description for SPARC.
;; Copyright (C) 1987-2025 Free Software Foundation, Inc.
;; Contributed by Michael Tiemann (tiemann@cygnus.com)
;; 64-bit SPARC-V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
;; at Cygnus Support.

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
  UNSPEC_MOVE_PIC
  UNSPEC_UPDATE_RETURN
  UNSPEC_LOAD_PCREL_SYM
  UNSPEC_FRAME_BLOCKAGE
  UNSPEC_MOVE_PIC_LABEL
  UNSPEC_SETH44
  UNSPEC_SETM44
  UNSPEC_SETHH
  UNSPEC_SETLM
  UNSPEC_EMB_HISUM
  UNSPEC_EMB_TEXTUHI
  UNSPEC_EMB_TEXTHI
  UNSPEC_EMB_TEXTULO
  UNSPEC_EMB_SETHM
  UNSPEC_MOVE_GOTDATA

  UNSPEC_MEMBAR
  UNSPEC_ATOMIC

  UNSPEC_TLSGD
  UNSPEC_TLSLDM
  UNSPEC_TLSLDO
  UNSPEC_TLSIE
  UNSPEC_TLSLE
  UNSPEC_TLSLD_BASE

  UNSPEC_FPACK16
  UNSPEC_FPACK32
  UNSPEC_FPACKFIX
  UNSPEC_FEXPAND
  UNSPEC_MUL16AU
  UNSPEC_MUL16AL
  UNSPEC_MUL8UL
  UNSPEC_MULDUL
  UNSPEC_ALIGNDATA
  UNSPEC_PDIST
  UNSPEC_EDGE8
  UNSPEC_EDGE8L
  UNSPEC_EDGE16
  UNSPEC_EDGE16L
  UNSPEC_EDGE32
  UNSPEC_EDGE32L
  UNSPEC_ARRAY8
  UNSPEC_ARRAY16
  UNSPEC_ARRAY32

  UNSPEC_SP_SET
  UNSPEC_SP_TEST

  UNSPEC_EDGE8N
  UNSPEC_EDGE8LN
  UNSPEC_EDGE16N
  UNSPEC_EDGE16LN
  UNSPEC_EDGE32N
  UNSPEC_EDGE32LN
  UNSPEC_BSHUFFLE
  UNSPEC_CMASK8
  UNSPEC_CMASK16
  UNSPEC_CMASK32
  UNSPEC_FCHKSM16
  UNSPEC_PDISTN
  UNSPEC_FHADD
  UNSPEC_FHSUB
  UNSPEC_XMUL
  UNSPEC_MUL8
  UNSPEC_MUL8SU
  UNSPEC_MULDSU

  UNSPEC_ADDV
  UNSPEC_SUBV
  UNSPEC_NEGV

  UNSPEC_DICTUNPACK
  UNSPEC_FPCMPSHL
  UNSPEC_FPCMPUSHL
  UNSPEC_FPCMPDESHL
  UNSPEC_FPCMPURSHL
])

(define_c_enum "unspecv" [
  UNSPECV_BLOCKAGE

  UNSPECV_SPECULATION_BARRIER

  UNSPECV_PROBE_STACK_RANGE

  UNSPECV_FLUSHW
  UNSPECV_SAVEW

  UNSPECV_FLUSH

  UNSPECV_LDSTUB
  UNSPECV_SWAP
  UNSPECV_CAS

  UNSPECV_LDFSR
  UNSPECV_STFSR
])

(define_constants
 [(G0_REG			0)
  (G1_REG			1)
  (G2_REG			2)
  (G3_REG			3)
  (G4_REG			4)
  (G5_REG			5)
  (G6_REG			6)
  (G7_REG			7)
  (O0_REG			8)
  (O1_REG			9)
  (O2_REG			10)
  (O3_REG			11)
  (O4_REG			12)
  (O5_REG			13)
  (O6_REG			14)
  (O7_REG			15)
  (L0_REG			16)
  (L1_REG			17)
  (L2_REG			18)
  (L3_REG			19)
  (L4_REG			20)
  (L5_REG			21)
  (L6_REG			22)
  (L7_REG			23)
  (I0_REG			24)
  (I1_REG			25)
  (I2_REG			26)
  (I3_REG			27)
  (I4_REG			28)
  (I5_REG			29)
  (I6_REG			30)
  (I7_REG			31)
  (F0_REG			32)
  (F1_REG			33)
  (F2_REG			34)
  (F3_REG			35)
  (F4_REG			36)
  (F5_REG			37)
  (F6_REG			38)
  (F7_REG			39)
  (F8_REG			40)
  (F9_REG			41)
  (F10_REG			42)
  (F11_REG			43)
  (F12_REG			44)
  (F13_REG			45)
  (F14_REG			46)
  (F15_REG			47)
  (F16_REG			48)
  (F17_REG			49)
  (F18_REG			50)
  (F19_REG			51)
  (F20_REG			52)
  (F21_REG			53)
  (F22_REG			54)
  (F23_REG			55)
  (F24_REG			56)
  (F25_REG			57)
  (F26_REG			58)
  (F27_REG			59)
  (F28_REG			60)
  (F29_REG			61)
  (F30_REG			62)
  (F31_REG			63)
  (F32_REG			64)
  (F34_REG			66)
  (F36_REG			68)
  (F38_REG			70)
  (F40_REG			72)
  (F42_REG			74)
  (F44_REG			76)
  (F46_REG			78)
  (F48_REG			80)
  (F50_REG			82)
  (F52_REG			84)
  (F54_REG			86)
  (F56_REG			88)
  (F58_REG			90)
  (F60_REG			92)
  (F62_REG			94)
  (FCC0_REG			96)
  (FCC1_REG			97)
  (FCC2_REG			98)
  (FCC3_REG			99)
  (CC_REG			100)
  (SFP_REG			101)
  (GSR_REG			102)
 ])

(define_mode_iterator I [QI HI SI DI])
(define_mode_iterator P [(SI "TARGET_ARCH32") (DI "TARGET_ARCH64")])
(define_mode_iterator W [SI (DI "TARGET_ARCH64")])
(define_mode_iterator F [SF DF TF])

;; The upper 32 fp regs on the v9 can't hold SFmode values.  To deal with this
;; a second register class, EXTRA_FP_REGS, exists for the v9 chip.  The name
;; is a bit of a misnomer as it covers all 64 fp regs.  The corresponding
;; constraint letter is 'e'.  To avoid any confusion, 'e' is used instead of
;; 'f' for all DF/TFmode values, including those that are specific to the v8.

;; Attribute for cpu type.
;; These must match the values of enum sparc_processor_type in sparc-opts.h.
(define_attr "cpu"
  "v7,
   cypress,
   v8,
   supersparc,
   hypersparc,
   leon,
   leon3,
   leon5,
   leon3v7,
   sparclite,
   f930,
   f934,
   sparclite86x,
   sparclet,
   tsc701,
   v9,
   ultrasparc,
   ultrasparc3,
   niagara,
   niagara2,
   niagara3,
   niagara4,
   niagara7,
   m8"
  (const (symbol_ref "sparc_cpu_attr")))

;; Attribute for the instruction set.
;; At present we only need to distinguish v9/!v9, but for clarity we
;; test TARGET_V8 too.
(define_attr "isa" "v7,v8,v9,sparclet"
 (const
  (cond [(symbol_ref "TARGET_V9") (const_string "v9")
	 (symbol_ref "TARGET_V8") (const_string "v8")
	 (symbol_ref "TARGET_SPARCLET") (const_string "sparclet")]
	(const_string "v7"))))

(define_attr "cpu_feature" "none,fpu,fpunotv9,v9,vis,vis3,vis4,vis4b"
  (const_string "none"))

(define_attr "enabled" ""
  (cond [(eq_attr "cpu_feature" "none") (const_int 1)
         (eq_attr "cpu_feature" "fpu") (symbol_ref "TARGET_FPU")
         (eq_attr "cpu_feature" "fpunotv9") (symbol_ref "TARGET_FPU && !TARGET_V9")
         (eq_attr "cpu_feature" "v9") (symbol_ref "TARGET_V9")
         (eq_attr "cpu_feature" "vis") (symbol_ref "TARGET_VIS")
         (eq_attr "cpu_feature" "vis3") (symbol_ref "TARGET_VIS3")
         (eq_attr "cpu_feature" "vis4") (symbol_ref "TARGET_VIS4")
         (eq_attr "cpu_feature" "vis4b") (symbol_ref "TARGET_VIS4B")]
        (const_int 0)))

;; The SPARC instructions used by the backend are organized into a
;; hierarchy using the insn attributes "type" and "subtype".
;;
;; The mnemonics used in the list below are the architectural names
;; used in the Oracle SPARC Architecture specs.  A / character
;; separates the type from the subtype where appropriate.  For
;; brevity, text enclosed in {} denotes alternatives, while text
;; enclosed in [] is optional.
;;
;; Please keep this list updated.  It is of great help for keeping the
;; correctness and coherence of the DFA schedulers.
;;
;; ialu:  <empty>
;; ialuX: ADD[X]C SUB[X]C
;; shift: SLL[X] SRL[X] SRA[X]
;; cmove: MOV{A,N,NE,E,G,LE,GE,L,GU,LEU,CC,CS,POS,NEG,VC,VS}
;;        MOVF{A,N,U,G,UG,L,UL,LG,NE,E,UE,GE,UGE,LE,ULE,O}
;;        MOVR{Z,LEZ,LZ,NZ,GZ,GEZ}
;; compare: ADDcc ADDCcc ANDcc ORcc SUBcc SUBCcc XORcc XNORcc
;; imul: MULX SMUL[cc] UMUL UMULXHI XMULX XMULXHI
;; idiv: UDIVX SDIVX
;; flush: FLUSH
;; load/regular: LD{UB,UH,UW} LDFSR
;; load/prefetch: PREFETCH
;; fpload: LDF LDDF LDQF
;; sload: LD{SB,SH,SW}
;; store: ST{B,H,W,X} STFSR
;; fpstore: STF STDF STQF
;; cbcond: CWB{NE,E,G,LE,GE,L,GU,LEU,CC,CS,POS,NEG,VC,VS}
;;         CXB{NE,E,G,LE,GE,L,GU,LEU,CC,CS,POS,NEG,VC,VS}
;; uncond_branch: BA BPA JMPL
;; branch: B{NE,E,G,LE,GE,L,GU,LEU,CC,CS,POS,NEG,VC,VS}
;;         BP{NE,E,G,LE,GE,L,GU,LEU,CC,CS,POS,NEG,VC,VS}
;;         FB{U,G,UG,L,UL,LG,NE,BE,UE,GE,UGE,LE,ULE,O}
;; call: CALL
;; return: RESTORE RETURN
;; fpmove: FABS{s,d,q} FMOV{s,d,q} FNEG{s,d,q}
;; fpcmove: FMOV{S,D,Q}{icc,xcc,fcc}
;; fpcrmove: FMOVR{s,d,q}{Z,LEZ,LZ,NZ,GZ,GEZ}
;; fp: FADD{s,d,q} FSUB{s,d,q} FHSUB{s,d} FNHADD{s,d} FNADD{s,d}
;;     FiTO{s,d,q} FsTO{i,x,d,q} FdTO{i,x,s,q} FxTO{d,s,q} FqTO{i,x,s,d}
;; fpcmp: FCMP{s,d,q} FCMPE{s,d,q}
;; fpmul: FMADD{s,d}  FMSUB{s,d} FMUL{s,d,q} FNMADD{s,d}
;;        FNMSUB{s,d} FNMUL{s,d} FNsMULd FsMULd
;;        FdMULq
;; array: ARRAY{8,16,32}
;; bmask: BMASK
;; edge: EDGE{8,16,32}[L]cc
;; edgen: EDGE{8,16,32}[L]n
;; fpdivs: FDIV{s,q}
;; fpsqrts: FSQRT{s,q}
;; fpdivd: FDIVd
;; fpsqrtd: FSQRTd
;; lzd: LZCNT
;; fga/addsub64: FP{ADD,SUB}64
;; fga/fpu: FCHKSM16 FEXPANd FMEAN16 FPMERGE
;;          FS{LL,RA,RL}{16,32}
;; fga/maxmin: FP{MAX,MIN}[U]{8,16,32}
;; fga/cmask: CMASK{8,16,32}
;; fga/other: BSHUFFLE FALIGNDATAg FP{ADD,SUB}[S]{8,16,32}
;;            FP{ADD,SUB}US{8,16} DICTUNPACK
;; gsr/reg: RDGSR WRGSR
;; gsr/alignaddr: ALIGNADDRESS[_LITTLE]
;; vismv/double:  FSRC2d
;; vismv/single:  MOVwTOs FSRC2s
;; vismv/movstouw: MOVsTOuw
;; vismv/movxtod: MOVxTOd
;; vismv/movdtox: MOVdTOx
;; visl/single: F{AND,NAND,NOR,OR,NOT1}s
;;              F{AND,OR}NOT{1,2}s
;;              FONEs F{ZERO,XNOR,XOR}s FNOT2s
;; visl/double: FONEd FZEROd FNOT1d F{OR,AND,XOR}d F{NOR,NAND,XNOR}d
;;              F{OR,AND}NOT1d F{OR,AND}NOT2d
;; viscmp: FPCMP{LE,GT,NE,EQ}{8,16,32} FPCMPU{LE,GT,NE,EQ}{8,16,32}
;;         FPCMP{LE,GT,EQ,NE}{8,16,32}SHL FPCMPU{LE,GT,EQ,NE}{8,16,32}SHL
;;         FPCMPDE{8,16,32}SHL FPCMPUR{8,16,32}SHL
;; fgm_pack: FPACKFIX FPACK{8,16,32}
;; fgm_mul: FMUL8SUx16 FMUL8ULx16 FMUL8x16 FMUL8x16AL
;;          FMUL8x16AU FMULD8SUx16 FMULD8ULx16
;; pdist: PDIST
;; pdistn: PDISTN

(define_attr "type"
  "ialu,compare,shift,
   load,sload,store,
   uncond_branch,branch,call,sibcall,call_no_delay_slot,return,
   cbcond,uncond_cbcond,
   imul,idiv,
   fpload,fpstore,
   fp,fpmove,
   fpcmove,fpcrmove,
   fpcmp,
   fpmul,fpdivs,fpdivd,
   fpsqrts,fpsqrtd,
   fga,visl,vismv,viscmp,
   fgm_pack,fgm_mul,pdist,pdistn,edge,edgen,gsr,array,bmask,
   cmove,
   ialuX,
   multi,savew,flushw,iflush,trap,lzd"
  (const_string "ialu"))

(define_attr "subtype"
  "single,double,movstouw,movxtod,movdtox,
   addsub64,cmask,fpu,maxmin,other,
   reg,alignaddr,
   prefetch,regular"
  (const_string "single"))

;; True if branch/call has empty delay slot and will emit a nop in it
(define_attr "empty_delay_slot" "false,true"
  (symbol_ref "(empty_delay_slot (insn)
		? EMPTY_DELAY_SLOT_TRUE : EMPTY_DELAY_SLOT_FALSE)"))

;; True if we are making use of compare-and-branch instructions.
;; True if we should emit a nop after a cbcond instruction
(define_attr "emit_cbcond_nop" "false,true"
  (symbol_ref "(emit_cbcond_nop (insn)
                ? EMIT_CBCOND_NOP_TRUE : EMIT_CBCOND_NOP_FALSE)"))

(define_attr "branch_type" "none,icc,fcc,reg"
  (const_string "none"))

(define_attr "pic" "false,true"
  (symbol_ref "(flag_pic != 0
		? PIC_TRUE : PIC_FALSE)"))

(define_attr "calls_alloca" "false,true"
  (symbol_ref "(cfun->calls_alloca != 0
		? CALLS_ALLOCA_TRUE : CALLS_ALLOCA_FALSE)"))

(define_attr "calls_eh_return" "false,true"
   (symbol_ref "(crtl->calls_eh_return != 0
		 ? CALLS_EH_RETURN_TRUE : CALLS_EH_RETURN_FALSE)"))

(define_attr "leaf_function" "false,true"
  (symbol_ref "(crtl->uses_only_leaf_regs != 0
		? LEAF_FUNCTION_TRUE : LEAF_FUNCTION_FALSE)"))

(define_attr "delayed_branch" "false,true"
  (symbol_ref "(flag_delayed_branch != 0
		? DELAYED_BRANCH_TRUE : DELAYED_BRANCH_FALSE)"))

(define_attr "flat" "false,true"
  (symbol_ref "(TARGET_FLAT != 0
		? FLAT_TRUE : FLAT_FALSE)"))

(define_attr "fix_ut699" "false,true"
   (symbol_ref "(sparc_fix_ut699 != 0
		 ? FIX_UT699_TRUE : FIX_UT699_FALSE)"))

(define_attr "fix_b2bst" "false,true"
   (symbol_ref "(sparc_fix_b2bst != 0
		 ? FIX_B2BST_TRUE : FIX_B2BST_FALSE)"))

(define_attr "fix_lost_divsqrt" "false,true"
   (symbol_ref "(sparc_fix_lost_divsqrt != 0
		 ? FIX_LOST_DIVSQRT_TRUE : FIX_LOST_DIVSQRT_FALSE)"))

(define_attr "fix_gr712rc" "false,true"
   (symbol_ref "(sparc_fix_gr712rc != 0
		 ? FIX_GR712RC_TRUE : FIX_GR712RC_FALSE)"))

;; Length (in # of insns).
;; Beware that setting a length greater or equal to 3 for conditional branches
;; has a side-effect (see output_cbranch and output_v9branch).
(define_attr "length" ""
  (cond [(eq_attr "type" "uncond_branch,call")
	   (if_then_else (eq_attr "empty_delay_slot" "true")
	     (const_int 2)
	     (const_int 1))
	 (eq_attr "type" "sibcall")
	   (if_then_else (ior (eq_attr "leaf_function" "true")
	                      (eq_attr "flat" "true"))
	     (if_then_else (eq_attr "empty_delay_slot" "true")
	       (const_int 3)
	       (const_int 2))
	     (if_then_else (eq_attr "empty_delay_slot" "true")
	       (const_int 2)
	       (const_int 1)))
	 (eq_attr "branch_type" "icc")
	   (if_then_else (match_operand 0 "v9_comparison_operator" "")
	     (if_then_else (lt (pc) (match_dup 1))
	       (if_then_else (lt (minus (match_dup 1) (pc)) (const_int 260000))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 2)
		   (const_int 1))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 4)
		   (const_int 3)))
	       (if_then_else (lt (minus (pc) (match_dup 1)) (const_int 260000))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 2)
		   (const_int 1))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 4)
		   (const_int 3))))
	     (if_then_else (eq_attr "empty_delay_slot" "true")
	       (const_int 2)
	       (const_int 1)))
	 (eq_attr "branch_type" "fcc")
	   (if_then_else (match_operand 0 "fcc0_register_operand" "")
	     (if_then_else (eq_attr "empty_delay_slot" "true")
	       (if_then_else (not (match_test "TARGET_V9"))
		 (const_int 3)
		 (const_int 2))
	       (if_then_else (not (match_test "TARGET_V9"))
		 (const_int 2)
		 (const_int 1)))
	     (if_then_else (lt (pc) (match_dup 2))
	       (if_then_else (lt (minus (match_dup 2) (pc)) (const_int 260000))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 2)
		   (const_int 1))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 4)
		   (const_int 3)))
	       (if_then_else (lt (minus (pc) (match_dup 2)) (const_int 260000))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 2)
		   (const_int 1))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
		   (const_int 4)
		   (const_int 3)))))
	 (eq_attr "branch_type" "reg")
	   (if_then_else (lt (pc) (match_dup 2))
	     (if_then_else (lt (minus (match_dup 2) (pc)) (const_int 32000))
	       (if_then_else (eq_attr "empty_delay_slot" "true")
		 (const_int 2)
		 (const_int 1))
	       (if_then_else (eq_attr "empty_delay_slot" "true")
		 (const_int 4)
		 (const_int 3)))
	     (if_then_else (lt (minus (pc) (match_dup 2)) (const_int 32000))
	       (if_then_else (eq_attr "empty_delay_slot" "true")
		 (const_int 2)
		 (const_int 1))
	       (if_then_else (eq_attr "empty_delay_slot" "true")
		 (const_int 4)
		 (const_int 3))))
         (eq_attr "type" "cbcond")
	   (if_then_else (lt (pc) (match_dup 3))
	     (if_then_else (lt (minus (match_dup 3) (pc)) (const_int 500))
               (if_then_else (eq_attr "emit_cbcond_nop" "true")
                 (const_int 2)
                 (const_int 1))
               (const_int 4))
	     (if_then_else (lt (minus (pc) (match_dup 3)) (const_int 500))
               (if_then_else (eq_attr "emit_cbcond_nop" "true")
                 (const_int 2)
                 (const_int 1))
               (const_int 4)))
         (eq_attr "type" "uncond_cbcond")
	   (if_then_else (lt (pc) (match_dup 0))
	     (if_then_else (lt (minus (match_dup 0) (pc)) (const_int 500))
               (if_then_else (eq_attr "emit_cbcond_nop" "true")
                 (const_int 2)
                 (const_int 1))
               (const_int 1))
	     (if_then_else (lt (minus (pc) (match_dup 0)) (const_int 500))
               (if_then_else (eq_attr "emit_cbcond_nop" "true")
                 (const_int 2)
                 (const_int 1))
               (const_int 1)))
	 ] (const_int 1)))

;; FP precision.
(define_attr "fptype" "single,double"
  (const_string "single"))

;; FP precision specific to the UT699.
(define_attr "fptype_ut699" "none,single"
  (const_string "none"))

;; UltraSPARC-III integer load type.
(define_attr "us3load_type" "2cycle,3cycle"
  (const_string "2cycle"))

(define_asm_attributes
  [(set_attr "length" "2")
   (set_attr "type" "multi")])

;; Attributes for branch scheduling
(define_attr "tls_delay_slot" "false,true"
  (symbol_ref "((TARGET_GNU_TLS && HAVE_GNU_LD) != 0
		? TLS_DELAY_SLOT_TRUE : TLS_DELAY_SLOT_FALSE)"))

(define_attr "in_sibcall_delay" "false,true"
  (symbol_ref "(eligible_for_sibcall_delay (insn)
		? IN_SIBCALL_DELAY_TRUE : IN_SIBCALL_DELAY_FALSE)"))

(define_attr "in_return_delay" "false,true"
  (symbol_ref "(eligible_for_return_delay (insn)
		? IN_RETURN_DELAY_TRUE : IN_RETURN_DELAY_FALSE)"))

;; ??? !v9: Should implement the notion of predelay slots for floating-point
;; branches.  This would allow us to remove the nop always inserted before
;; a floating point branch.

;; ??? It is OK for fill_simple_delay_slots to put load/store instructions
;; in a delay slot, but it is not OK for fill_eager_delay_slots to do so.
;; This is because doing so will add several pipeline stalls to the path
;; that the load/store did not come from.  Unfortunately, there is no way
;; to prevent fill_eager_delay_slots from using load/store without completely
;; disabling them.  For the SPEC benchmark set, this is a serious lose,
;; because it prevents us from moving back the final store of inner loops.

(define_attr "in_branch_delay" "false,true"
  (cond [(eq_attr "type" "uncond_branch,branch,cbcond,uncond_cbcond,call,sibcall,call_no_delay_slot,multi")
	   (const_string "false")
	 (and (eq_attr "fix_lost_divsqrt" "true")
	      (eq_attr "type" "fpdivs,fpsqrts,fpdivd,fpsqrtd"))
	   (const_string "false")
	 (and (eq_attr "fix_b2bst" "true") (eq_attr "type" "store,fpstore"))
	   (const_string "false")
	 (and (eq_attr "fix_ut699" "true") (eq_attr "type" "load,sload"))
	   (const_string "false")
	 (and (eq_attr "fix_ut699" "true")
	      (and (eq_attr "type" "fpload,fp,fpmove,fpmul,fpdivs,fpsqrts")
		   (ior (eq_attr "fptype" "single")
		        (eq_attr "fptype_ut699" "single"))))
	   (const_string "false")
	 (eq_attr "length" "1")
	   (const_string "true")
	] (const_string "false")))

(define_attr "in_integer_branch_annul_delay" "false,true"
  (cond [(and (eq_attr "fix_gr712rc" "true")
	      (eq_attr "type" "fp,fpcmp,fpmove,fpcmove,fpmul,
			       fpdivs,fpsqrts,fpdivd,fpsqrtd"))
	   (const_string "false")
	 (eq_attr "in_branch_delay" "true")
	   (const_string "true")
	] (const_string "false")))

(define_delay (eq_attr "type" "sibcall")
  [(eq_attr "in_sibcall_delay" "true") (nil) (nil)])

(define_delay (eq_attr "type" "return")
  [(eq_attr "in_return_delay" "true") (nil) (nil)])

(define_delay (ior (eq_attr "type" "call") (eq_attr "type" "uncond_branch"))
  [(eq_attr "in_branch_delay" "true") (nil) (nil)])

(define_delay (and (eq_attr "type" "branch") (not (eq_attr "branch_type" "icc")))
  [(eq_attr "in_branch_delay" "true")
   (nil)
   (eq_attr "in_branch_delay" "true")])

(define_delay (and (eq_attr "type" "branch") (eq_attr "branch_type" "icc"))
  [(eq_attr "in_branch_delay" "true")
   (nil)
   (eq_attr "in_integer_branch_annul_delay" "true")])

;; Include SPARC DFA schedulers

(include "cypress.md")
(include "supersparc.md")
(include "hypersparc.md")
(include "leon.md")
(include "leon5.md")
(include "sparclet.md")
(include "ultra1_2.md")
(include "ultra3.md")
(include "niagara.md")
(include "niagara2.md")
(include "niagara4.md")
(include "niagara7.md")
(include "m8.md")


;; Operand and operator predicates and constraints

(include "predicates.md")
(include "constraints.md")


;; Compare instructions.

;; These are just the DEFINE_INSNs to match the patterns and the
;; DEFINE_SPLITs for some of the scc insns that actually require
;; more than one machine instruction.  DEFINE_EXPANDs are further down.

(define_insn "*cmpsi_insn"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 0 "register_operand" "r")
		    (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "cmp\t%0, %1"
  [(set_attr "type" "compare")])

(define_insn "*cmpdi_sp64"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operand:DI 0 "register_operand" "r")
		     (match_operand:DI 1 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "cmp\t%0, %1"
  [(set_attr "type" "compare")])

(define_insn "*cmpsi_sne"
  [(set (reg:CCC CC_REG)
	(compare:CCC (not:SI (match_operand:SI 0 "arith_operand" "rI"))
		     (const_int -1)))]
  ""
  "cmp\t%%g0, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmpdi_sne"
  [(set (reg:CCXC CC_REG)
	(compare:CCXC (not:DI (match_operand:DI 0 "arith_operand" "rI"))
		      (const_int -1)))]
  "TARGET_ARCH64"
  "cmp\t%%g0, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmpsf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_register_operand" "=c")
	(compare:CCFPE (match_operand:SF 1 "register_operand" "f")
		       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
{
  if (TARGET_V9)
    return "fcmpes\t%0, %1, %2";
  return "fcmpes\t%1, %2";
}
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpdf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_register_operand" "=c")
	(compare:CCFPE (match_operand:DF 1 "register_operand" "e")
		       (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
{
  if (TARGET_V9)
    return "fcmped\t%0, %1, %2";
  return "fcmped\t%1, %2";
}
  [(set_attr "type" "fpcmp")
   (set_attr "fptype" "double")])

(define_insn "*cmptf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_register_operand" "=c")
	(compare:CCFPE (match_operand:TF 1 "register_operand" "e")
		       (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
{
  if (TARGET_V9)
    return "fcmpeq\t%0, %1, %2";
  return "fcmpeq\t%1, %2";
}
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpsf_fp"
  [(set (match_operand:CCFP 0 "fcc_register_operand" "=c")
	(compare:CCFP (match_operand:SF 1 "register_operand" "f")
		      (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
{
  if (TARGET_V9)
    return "fcmps\t%0, %1, %2";
  return "fcmps\t%1, %2";
}
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpdf_fp"
  [(set (match_operand:CCFP 0 "fcc_register_operand" "=c")
	(compare:CCFP (match_operand:DF 1 "register_operand" "e")
		      (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
{
  if (TARGET_V9)
    return "fcmpd\t%0, %1, %2";
  return "fcmpd\t%1, %2";
}
  [(set_attr "type" "fpcmp")
   (set_attr "fptype" "double")])

(define_insn "*cmptf_fp"
  [(set (match_operand:CCFP 0 "fcc_register_operand" "=c")
	(compare:CCFP (match_operand:TF 1 "register_operand" "e")
		      (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
{
  if (TARGET_V9)
    return "fcmpq\t%0, %1, %2";
  return "fcmpq\t%1, %2";
}
  [(set_attr "type" "fpcmp")])

;; Next come the scc insns.

;; Note that the boolean result (operand 0) takes on DImode
;; (not SImode) when TARGET_ARCH64.

(define_expand "cstoresi4"
  [(use (match_operator 1 "comparison_operator"
         [(match_operand:SI 2 "compare_operand" "")
          (match_operand:SI 3 "arith_operand" "")]))
   (clobber (match_operand:SI 0 "cstore_result_operand"))]
  ""
{
  if (GET_CODE (operands[2]) == ZERO_EXTRACT && operands[3] != const0_rtx)
    operands[2] = force_reg (SImode, operands[2]);
  if (emit_scc_insn (operands)) DONE; else FAIL;
})

(define_expand "cstoredi4"
  [(use (match_operator 1 "comparison_operator"
         [(match_operand:DI 2 "compare_operand" "")
          (match_operand:DI 3 "arith_operand" "")]))
   (clobber (match_operand:SI 0 "cstore_result_operand"))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == ZERO_EXTRACT && operands[3] != const0_rtx)
    operands[2] = force_reg (DImode, operands[2]);
  if (emit_scc_insn (operands)) DONE; else FAIL;
})

(define_expand "cstore<F:mode>4"
  [(use (match_operator 1 "comparison_operator"
         [(match_operand:F 2 "register_operand" "")
          (match_operand:F 3 "register_operand" "")]))
   (clobber (match_operand:SI 0 "cstore_result_operand"))]
  "TARGET_FPU"
{
  if (emit_scc_insn (operands)) DONE; else FAIL;
})

;; The SNE and SEQ patterns are special because they can be done
;; without any branching and do not involve a COMPARE.

(define_insn_and_split "*snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(ne:W (match_operand:SI 1 "register_operand" "r")
	      (const_int 0)))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (ltu:W (reg:CCC CC_REG) (const_int 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*neg_snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (ne:W (match_operand:SI 1 "register_operand" "r")
		     (const_int 0))))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (neg:W (ltu:W (reg:CCC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=&r")
        (ne:W (match_operand:DI 1 "register_operand" "r")
              (const_int 0)))]
  "TARGET_ARCH64 && !TARGET_VIS3"
  "#"
  "&& !reg_overlap_mentioned_p (operands[1], operands[0])"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:W (ne:DI (match_dup 1) (const_int 0))
                                      (const_int 1)
                                      (match_dup 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*snedi<W:mode>_zero_vis3"
  [(set (match_operand:W 0 "register_operand" "=r")
	(ne:W (match_operand:DI 1 "register_operand" "r")
	      (const_int 0)))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_VIS3"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (ltu:W (reg:CCXC CC_REG) (const_int 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*neg_snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=&r")
        (neg:W (ne:W (match_operand:DI 1 "register_operand" "r")
                     (const_int 0))))]
  "TARGET_ARCH64 && !TARGET_SUBXC"
  "#"
  "&& !reg_overlap_mentioned_p (operands[1], operands[0])"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:W (ne:DI (match_dup 1) (const_int 0))
                                      (const_int -1)
                                      (match_dup 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*neg_snedi<W:mode>_zero_subxc"
  [(set (match_operand:W 0 "register_operand" "=&r")
        (neg:W (ne:W (match_operand:DI 1 "register_operand" "r")
                     (const_int 0))))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_SUBXC"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (neg:W (ltu:W (reg:CCXC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*seqsi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(eq:W (match_operand:SI 1 "register_operand" "r")
	      (const_int 0)))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (geu:W (reg:CCC CC_REG) (const_int 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*neg_seqsi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (eq:W (match_operand:SI 1 "register_operand" "r")
		     (const_int 0))))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (neg:W (geu:W (reg:CCC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*seqdi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=&r")
        (eq:W (match_operand:DI 1 "register_operand" "r")
              (const_int 0)))]
  "TARGET_ARCH64"
  "#"
  "&& !reg_overlap_mentioned_p (operands[1], operands[0])"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:W (eq:DI (match_dup 1) (const_int 0))
                                      (const_int 1)
                                      (match_dup 0)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*neg_seqdi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=&r")
        (neg:W (eq:W (match_operand:DI 1 "register_operand" "r")
                     (const_int 0))))]
  "TARGET_ARCH64"
  "#"
  "&& !reg_overlap_mentioned_p (operands[1], operands[0])"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:W (eq:DI (match_dup 1) (const_int 0))
                                      (const_int -1)
                                      (match_dup 0)))]
  ""
  [(set_attr "length" "2")]) 

;; We can also do (x + (i == 0)) and related, so put them in.

(define_insn_and_split "*plus_snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (ne:W (match_operand:SI 1 "register_operand" "r")
		      (const_int 0))
		(match_operand:W 2 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (plus:W (ltu:W (reg:CCC CC_REG) (const_int 0))
			      (match_dup 2)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*plus_plus_snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (ne:W (match_operand:SI 1 "register_operand" "r")
			      (const_int 0))
			(match_operand:W 2 "register_operand" "r"))
                 (match_operand:W 3 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (plus:W (plus:W (ltu:W (reg:CCC CC_REG) (const_int 0))
				      (match_dup 2))
		      (match_dup 3)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*plus_snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (ne:W (match_operand:DI 1 "register_operand" "r")
		      (const_int 0))
		(match_operand:W 2 "register_operand" "r")))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_VIS3"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (plus:W (ltu:W (reg:CCXC CC_REG) (const_int 0))
			      (match_dup 2)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*plus_plus_snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (ne:W (match_operand:DI 1 "register_operand" "r")
			      (const_int 0))
			(match_operand:W 2 "register_operand" "r"))
                 (match_operand:W 3 "register_operand" "r")))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_VIS3"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (plus:W (plus:W (ltu:W (reg:CCXC CC_REG) (const_int 0))
				      (match_dup 2))
		      (match_dup 3)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*minus_snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 2 "register_operand" "r")
		  (ne:W (match_operand:SI 1 "register_operand" "r")
			(const_int 0))))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (minus:W (match_dup 2)
			       (ltu:W (reg:CCC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*minus_minus_snesi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 2 "register_operand" "r")
			  (ne:W (match_operand:SI 1 "register_operand" "r")
				(const_int 0)))
		 (match_operand:W 3 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (minus:W (minus:W (match_dup 2)
				        (ltu:W (reg:CCC CC_REG) (const_int 0)))
			       (match_dup 3)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*minus_snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 2 "register_operand" "r")
		 (ne:W (match_operand:DI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_SUBXC"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (minus:W (match_dup 2)
			       (ltu:W (reg:CCXC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*minus_minus_snedi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 2 "register_operand" "r")
			  (ne:W (match_operand:DI 1 "register_operand" "r")
				(const_int 0)))
		 (match_operand:W 3 "register_operand" "r")))
   (clobber (reg:CCX CC_REG))]
  "TARGET_ARCH64 && TARGET_SUBXC"
  "#"
  "&& 1"
  [(set (reg:CCXC CC_REG) (compare:CCXC (not:DI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (minus:W (minus:W (match_dup 2)
				        (ltu:W (reg:CCXC CC_REG) (const_int 0)))
			       (match_dup 3)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*plus_seqsi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (eq:W (match_operand:SI 1 "register_operand" "r")
		      (const_int 0))
		(match_operand:W 2 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (plus:W (geu:W (reg:CCC CC_REG) (const_int 0))
			      (match_dup 2)))]
  ""
  [(set_attr "length" "2")])

(define_insn_and_split "*minus_seqsi<W:mode>_zero"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 2 "register_operand" "r")
		 (eq:W (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  ""
  [(set (reg:CCC CC_REG) (compare:CCC (not:SI (match_dup 1)) (const_int -1)))
   (set (match_dup 0) (minus:W (match_dup 2)
			       (geu:W (reg:CCC CC_REG) (const_int 0))))]
  ""
  [(set_attr "length" "2")])

;; We can also do GEU and LTU directly, but these operate after a compare.

(define_insn "*sltu<W:mode>_insn"
  [(set (match_operand:W 0 "register_operand" "=r")
	(ltu:W (match_operand 1 "icc_register_operand" "X") (const_int 0)))]
  "GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode"
  "addx\t%%g0, 0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*plus_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (ltu:W (match_operand 2 "icc_register_operand" "X")
		       (const_int 0))
		(match_operand:W 1 "arith_operand" "rI")))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "addx\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*plus_plus_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (ltu:W (match_operand 3 "icc_register_operand" "X")
			       (const_int 0))
			(match_operand:W 1 "register_operand" "%r"))
		(match_operand:W 2 "arith_operand" "rI")))]
  "GET_MODE (operands[3]) == CCmode || GET_MODE (operands[3]) == CCCmode"
  "addx\t%1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_sgeu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (geu:W (match_operand 1 "icc_register_operand" "X")
		      (const_int 0))))]
  "GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode"
  "addx\t%%g0, -1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_sgeusidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (neg:SI (geu:SI (match_operand 1 "icc_register_operand" "X")
					(const_int 0)))))]
  "TARGET_ARCH64
   && (GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode)"
  "addx\t%%g0, -1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_sgeu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 1 "register_operand" "r")
		 (geu:W (match_operand 2 "icc_register_operand" "X")
			(const_int 0))))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "addx\t%1, -1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*addx<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (match_operand:W 1 "register_operand" "%r")
			(match_operand:W 2 "arith_operand" "rI"))
		(ltu:W (match_operand 3 "icc_register_operand" "X")
		       (const_int 0))))]
  "GET_MODE (operands[3]) == CCmode || GET_MODE (operands[3]) == CCCmode"
  "addx\t%1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*sltu<W:mode>_insn_vis3"
  [(set (match_operand:W 0 "register_operand" "=r")
	(ltu:W (match_operand 1 "icc_register_operand" "X") (const_int 0)))]
  "TARGET_ARCH64 && TARGET_VIS3
   && (GET_MODE (operands[1]) == CCXmode || GET_MODE (operands[1]) == CCXCmode)"
  "addxc\t%%g0, %%g0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*plus_sltu<W:mode>_vis3"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (ltu:W (match_operand 2 "icc_register_operand" "X")
		       (const_int 0))
		(match_operand:W 1 "register_operand" "r")))]
  "TARGET_ARCH64 && TARGET_VIS3
   && (GET_MODE (operands[2]) == CCXmode || GET_MODE (operands[2]) == CCXCmode)"
  "addxc\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*plus_plus_sltu<W:mode>_vis3"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (ltu:W (match_operand 3 "icc_register_operand" "X")
			       (const_int 0))
			(match_operand:W 1 "register_operand" "%r"))
		(match_operand:W 2 "register_operand" "r")))]
  "TARGET_ARCH64 && TARGET_VIS3
   && (GET_MODE (operands[3]) == CCXmode || GET_MODE (operands[3]) == CCXCmode)"
  "addxc\t%1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*addxc<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (plus:W (match_operand:W 1 "register_operand" "%r")
			(match_operand:W 2 "register_operand" "r"))
		(ltu:W (match_operand 3 "icc_register_operand" "X")
		       (const_int 0))))]
  "TARGET_ARCH64 && TARGET_VIS3
   && (GET_MODE (operands[3]) == CCXmode || GET_MODE (operands[3]) == CCXCmode)"
  "addxc\t%1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (ltu:W (match_operand 1 "icc_register_operand" "X")
		      (const_int 0))))]
  "GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode"
  "subx\t%%g0, 0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_sltusidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (neg:SI (ltu:SI (match_operand 1 "icc_register_operand" "X")
					(const_int 0)))))]
  "TARGET_ARCH64
   && (GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode)"
  "subx\t%%g0, 0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_neg_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (neg:W (ltu:W (match_operand 2 "icc_register_operand" "X")
			       (const_int 0)))
		 (match_operand:W 1 "arith_operand" "rI")))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "subx\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_plus_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (plus:W (ltu:W (match_operand 2 "icc_register_operand" "X")
			      (const_int 0))
		       (match_operand:W 1 "arith_operand" "rI"))))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "subx\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 1 "register_operand" "r")
		 (ltu:W (match_operand 2 "icc_register_operand" "X")
			(const_int 0))))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "subx\t%1, 0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_minus_sltu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 1 "register_or_zero_operand" "rJ")
			  (ltu:W (match_operand 3 "icc_register_operand" "X")
				 (const_int 0)))
		 (match_operand:W 2 "arith_operand" "rI")))]
  "GET_MODE (operands[3]) == CCmode || GET_MODE (operands[3]) == CCCmode"
  "subx\t%r1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*sgeu<W:mode>_insn"
  [(set (match_operand:W 0 "register_operand" "=r")
	(geu:W (match_operand 1 "icc_register_operand" "X") (const_int 0)))]
  "GET_MODE (operands[1]) == CCmode || GET_MODE (operands[1]) == CCCmode"
  "subx\t%%g0, -1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*plus_sgeu<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(plus:W (geu:W (match_operand 2 "icc_register_operand" "X")
		       (const_int 0))
		(match_operand:W 1 "register_operand" "r")))]
  "GET_MODE (operands[2]) == CCmode || GET_MODE (operands[2]) == CCCmode"
  "subx\t%1, -1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*subx<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 1 "register_or_zero_operand" "rJ")
			  (match_operand:W 2 "arith_operand" "rI"))
		 (ltu:W (match_operand 3 "icc_register_operand" "X")
		        (const_int 0))))]
  "GET_MODE (operands[3]) == CCmode || GET_MODE (operands[3]) == CCCmode"
  "subx\t%r1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_sltu<W:mode>_subxc"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (ltu:W (match_operand 1 "icc_register_operand" "X")
		      (const_int 0))))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[1]) == CCXmode || GET_MODE (operands[1]) == CCXCmode)"
  "subxc\t%%g0, %%g0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_neg_sltu<W:mode>_subxc"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (neg:W (ltu:W (match_operand 2 "icc_register_operand" "X")
			       (const_int 0)))
		 (match_operand:W 1 "register_operand" "r")))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[2]) == CCXmode || GET_MODE (operands[2]) == CCXCmode)"
  "subxc\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*neg_plus_sltu<W:mode>_subxc"
  [(set (match_operand:W 0 "register_operand" "=r")
	(neg:W (plus:W (ltu:W (match_operand 2 "icc_register_operand" "X")
			      (const_int 0))
		       (match_operand:W 1 "register_operand" "r"))))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[2]) == CCXmode || GET_MODE (operands[2]) == CCXCmode)"
  "subxc\t%%g0, %1, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_sltu<W:mode>_subxc"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (match_operand:W 1 "register_operand" "r")
		 (ltu:W (match_operand 2 "icc_register_operand" "X")
			(const_int 0))))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[2]) == CCXmode || GET_MODE (operands[2]) == CCXCmode)"
  "subxc\t%1, %%g0, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*minus_minus_sltu<W:mode>_subxc"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 1 "register_or_zero_operand" "rJ")
			  (ltu:W (match_operand 3 "icc_register_operand" "X")
				 (const_int 0)))
		 (match_operand:W 2 "register_operand" "r")))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[3]) == CCXmode || GET_MODE (operands[3]) == CCXCmode)"
  "subxc\t%r1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_insn "*subxc<W:mode>"
  [(set (match_operand:W 0 "register_operand" "=r")
	(minus:W (minus:W (match_operand:W 1 "register_or_zero_operand" "rJ")
			  (match_operand:W 2 "register_operand" "r"))
		 (ltu:W (match_operand 3 "icc_register_operand" "X")
			(const_int 0))))]
  "TARGET_ARCH64 && TARGET_SUBXC
   && (GET_MODE (operands[3]) == CCXmode || GET_MODE (operands[3]) == CCXCmode)"
  "subxc\t%r1, %2, %0"
  [(set_attr "type" "ialuX")])

(define_split
  [(set (match_operand:W 0 "register_operand" "")
	(match_operator:W 1 "icc_comparison_operator"
	 [(match_operand 2 "icc_register_operand" "") (const_int 0)]))]
  "TARGET_V9
   /* 64-bit LTU is better implemented using addxc with VIS3.  */
   && !(GET_CODE (operands[1]) == LTU
	&& (GET_MODE (operands[2]) == CCXmode
	    || GET_MODE (operands[2]) == CCXCmode)
	&& TARGET_VIS3)
   /* 32-bit LTU/GEU are better implemented using addx/subx.  */
   && !((GET_CODE (operands[1]) == LTU || GET_CODE (operands[1]) == GEU)
	&& (GET_MODE (operands[2]) == CCmode
	    || GET_MODE (operands[2]) == CCCmode))"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0)
	(if_then_else:SI (match_op_dup:W 1 [(match_dup 2) (const_int 0)])
			 (const_int 1)
			 (match_dup 0)))]
  "")

;; These control RTL generation for conditional jump insns

(define_expand "cbranchcc4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand 1 "compare_operand" "")
		        (match_operand 2 "const_zero_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "")

(define_expand "cbranchsi4"
  [(use (match_operator 0 "comparison_operator"
         [(match_operand:SI 1 "compare_operand" "")
          (match_operand:SI 2 "arith_operand" "")]))
   (use (match_operand 3 ""))]
  ""
{
  if (GET_CODE (operands[1]) == ZERO_EXTRACT && operands[2] != const0_rtx)
    operands[1] = force_reg (SImode, operands[1]);
  emit_conditional_branch_insn (operands);
  DONE;
})

(define_expand "cbranchdi4"
  [(use (match_operator 0 "comparison_operator"
         [(match_operand:DI 1 "compare_operand" "")
          (match_operand:DI 2 "arith_operand" "")]))
   (use (match_operand 3 ""))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[1]) == ZERO_EXTRACT && operands[2] != const0_rtx)
    operands[1] = force_reg (DImode, operands[1]);
  emit_conditional_branch_insn (operands);
  DONE;
})

(define_expand "cbranch<F:mode>4"
  [(use (match_operator 0 "comparison_operator"
         [(match_operand:F 1 "register_operand" "")
          (match_operand:F 2 "register_operand" "")]))
   (use (match_operand 3 ""))]
  "TARGET_FPU"
{
  emit_conditional_branch_insn (operands);
  DONE;
})


;; Now match both normal and inverted jump.

;; XXX fpcmp nop braindamage
(define_insn "*normal_branch"
  [(set (pc)
	(if_then_else (match_operator 0 "icc_comparison_operator"
		       [(reg CC_REG) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
{
  return output_cbranch (operands[0], operands[1], 1, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "icc")])

;; XXX fpcmp nop braindamage
(define_insn "*inverted_branch"
  [(set (pc)
	(if_then_else (match_operator 0 "icc_comparison_operator"
		       [(reg CC_REG) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
{
  return output_cbranch (operands[0], operands[1], 1, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "icc")])

;; XXX fpcmp nop braindamage
(define_insn "*normal_fp_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand:CCFP 0 "fcc_register_operand" "c")
			(const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return output_cbranch (operands[1], operands[2], 2, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "fcc")])

;; XXX fpcmp nop braindamage
(define_insn "*inverted_fp_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand:CCFP 0 "fcc_register_operand" "c")
			(const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  return output_cbranch (operands[1], operands[2], 2, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "fcc")])

;; XXX fpcmp nop braindamage
(define_insn "*normal_fpe_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand:CCFPE 0 "fcc_register_operand" "c")
			(const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return output_cbranch (operands[1], operands[2], 2, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "fcc")])

;; XXX fpcmp nop braindamage
(define_insn "*inverted_fpe_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand:CCFPE 0 "fcc_register_operand" "c")
			(const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
{
  return output_cbranch (operands[1], operands[2], 2, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "fcc")])

;; SPARC V9-specific jump insns.  None of these are guaranteed to be
;; in the architecture.

(define_insn "*cbcond_sp32"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:SI 1 "register_operand" "r")
                        (match_operand:SI 2 "arith5_operand" "rA")])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  "TARGET_CBCOND"
{
  return output_cbcond (operands[0], operands[3], insn);
}
  [(set_attr "type" "cbcond")])

(define_insn "*cbcond_sp64"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:DI 1 "register_operand" "r")
                        (match_operand:DI 2 "arith5_operand" "rA")])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  "TARGET_ARCH64 && TARGET_CBCOND"
{
  return output_cbcond (operands[0], operands[3], insn);
}
  [(set_attr "type" "cbcond")])

;; There are no 32-bit brreg insns.

(define_insn "*normal_int_branch_sp64"
  [(set (pc)
	(if_then_else (match_operator 0 "v9_register_comparison_operator"
		       [(match_operand:DI 1 "register_operand" "r")
			(const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "TARGET_ARCH64"
{
  return output_v9branch (operands[0], operands[2], 1, 2, 0,
			  final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			  insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "reg")])

(define_insn "*inverted_int_branch_sp64"
  [(set (pc)
	(if_then_else (match_operator 0 "v9_register_comparison_operator"
		       [(match_operand:DI 1 "register_operand" "r")
			(const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  "TARGET_ARCH64"
{
  return output_v9branch (operands[0], operands[2], 1, 2, 1,
			  final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			  insn);
}
  [(set_attr "type" "branch")
   (set_attr "branch_type" "reg")])


;; Load in operand 0 the (absolute) address of operand 1, which is a symbolic
;; value subject to a PC-relative relocation.  Operand 2 is a helper function
;; that adds the PC value at the call point to register #(operand 3).
;;
;; Even on V9 we use this call sequence with a stub, instead of "rd %pc, ..."
;; because the RDPC instruction is extremely expensive and incurs a complete
;; instruction pipeline flush.

(define_insn "@load_pcrel_sym<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "symbolic_operand" "")
		   (match_operand:P 2 "call_address_operand" "")
		   (match_operand:P 3 "const_int_operand" "")]
		  UNSPEC_LOAD_PCREL_SYM))
   (clobber (reg:P O7_REG))]
  "REGNO (operands[0]) == INTVAL (operands[3])"
{
  return output_load_pcrel_sym (operands);
}
  [(set (attr "type") (const_string "multi"))
   (set (attr "length")
	(if_then_else (eq_attr "delayed_branch" "true")
		      (const_int 3)
		      (const_int 4)))])


;; Integer move instructions

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (QImode, operands))
    DONE;
})

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:QI 1 "input_operand"   "rI,m,rJ"))]
  "(register_operand (operands[0], QImode)
    || register_or_zero_operand (operands[1], QImode))"
  "@
   mov\t%1, %0
   ldub\t%1, %0
   stb\t%r1, %0"
  [(set_attr "type" "*,load,store")
   (set_attr "subtype" "*,regular,*")
   (set_attr "us3load_type" "*,3cycle,*")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (HImode, operands))
    DONE;
})

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:HI 1 "input_operand"   "rI,K,m,rJ"))]
  "(register_operand (operands[0], HImode)
    || register_or_zero_operand (operands[1], HImode))"
  "@
   mov\t%1, %0
   sethi\t%%hi(%a1), %0
   lduh\t%1, %0
   sth\t%r1, %0"
  [(set_attr "type" "*,*,load,store")
   (set_attr "subtype" "*,*,regular,*")
   (set_attr "us3load_type" "*,*,3cycle,*")])

;; We always work with constants here.
(define_insn "*movhi_lo_sum"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (match_operand:HI 1 "register_operand" "%r")
                (match_operand:HI 2 "small_int_operand" "I")))]
  ""
  "or\t%1, %2, %0")

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (SImode, operands))
    DONE;
})

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r, m, r,*f,?*f,?*f,  m,d,d")
	(match_operand:SI 1 "input_operand"        "rI,K,m,rJ,*f, r,  f,  m,?*f,J,P"))]
  "register_operand (operands[0], SImode)
   || register_or_zero_or_all_ones_operand (operands[1], SImode)"
  "@
   mov\t%1, %0
   sethi\t%%hi(%a1), %0
   ld\t%1, %0
   st\t%r1, %0
   movstouw\t%1, %0
   movwtos\t%1, %0
   fmovs\t%1, %0
   ld\t%1, %0
   st\t%1, %0
   fzeros\t%0
   fones\t%0"
  [(set_attr "type" "*,*,load,store,vismv,vismv,fpmove,fpload,fpstore,visl,visl")
   (set_attr "subtype" "*,*,regular,*,movstouw,single,*,*,*,single,single")
   (set_attr "cpu_feature" "*,*,*,*,vis3,vis3,*,*,*,vis,vis")])

(define_insn "*movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "in")))]
  "!flag_pic"
  "or\t%1, %%lo(%a2), %0")

(define_insn "*movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "in")))]
  "!flag_pic"
  "sethi\t%%hi(%a1), %0")

;; The next two patterns must wrap the SYMBOL_REF in an UNSPEC
;; so that CSE won't optimize the address computation away.
(define_insn "movsi_lo_sum_pic"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (unspec:SI [(match_operand:SI 2 "immediate_operand" "in")]
			      UNSPEC_MOVE_PIC)))]
  "flag_pic"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "xor\t%1, %%gdop_lox10(%a2), %0";
#else
  return "or\t%1, %%lo(%a2), %0";
#endif
})

(define_insn "movsi_high_pic"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (high:SI (unspec:SI [(match_operand 1 "" "")] UNSPEC_MOVE_PIC)))]
  "flag_pic && check_pic (1)"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "sethi\t%%gdop_hix22(%a1), %0";
#else
  return "sethi\t%%hi(%a1), %0";
#endif
})

(define_insn "movsi_pic_gotdata_op"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")
	            (match_operand:SI 2 "register_operand" "r")
		    (match_operand 3 "symbolic_operand" "")]
		   UNSPEC_MOVE_GOTDATA))]
  "flag_pic && check_pic (1)"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "ld\t[%1 + %2], %0, %%gdop(%a3)";
#else
  return "ld\t[%1 + %2], %0";
#endif
}
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_expand "movsi_pic_label_ref"
  [(set (match_dup 3) (high:SI
     (unspec:SI [(match_operand:SI 1 "symbolic_operand" "")
		 (match_dup 2)] UNSPEC_MOVE_PIC_LABEL)))
   (set (match_dup 4) (lo_sum:SI (match_dup 3)
     (unspec:SI [(match_dup 1) (match_dup 2)] UNSPEC_MOVE_PIC_LABEL)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 5) (match_dup 4)))]
  "flag_pic"
{
  crtl->uses_pic_offset_table = 1;
  operands[2] = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  if (!can_create_pseudo_p ())
    {
      operands[3] = operands[0];
      operands[4] = operands[0];
    }
  else
    {
      operands[3] = gen_reg_rtx (SImode);
      operands[4] = gen_reg_rtx (SImode);
    }
  operands[5] = pic_offset_table_rtx;
})

(define_insn "*movsi_high_pic_label_ref"
  [(set (match_operand:SI 0 "register_operand" "=r")
      (high:SI
        (unspec:SI [(match_operand:SI 1 "symbolic_operand" "")
		    (match_operand:SI 2 "" "")] UNSPEC_MOVE_PIC_LABEL)))]
  "flag_pic"
  "sethi\t%%hi(%a2-(%a1-.)), %0")

(define_insn "*movsi_lo_sum_pic_label_ref"
  [(set (match_operand:SI 0 "register_operand" "=r")
      (lo_sum:SI (match_operand:SI 1 "register_operand" "r")
        (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")
		    (match_operand:SI 3 "" "")] UNSPEC_MOVE_PIC_LABEL)))]
  "flag_pic"
  "or\t%1, %%lo(%a3-(%a2-.)), %0")

;; Set up the PIC register for VxWorks.

(define_expand "vxworks_load_got"
  [(set (match_dup 0)
	(high:SI (match_dup 1)))
   (set (match_dup 0)
	(mem:SI (lo_sum:SI (match_dup 0) (match_dup 1))))
   (set (match_dup 0)
	(mem:SI (lo_sum:SI (match_dup 0) (match_dup 2))))]
  "TARGET_VXWORKS_RTP"
{
  operands[0] = pic_offset_table_rtx;
  operands[1] = gen_rtx_SYMBOL_REF (SImode, VXWORKS_GOTT_BASE);
  operands[2] = gen_rtx_SYMBOL_REF (SImode, VXWORKS_GOTT_INDEX);
})

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (DImode, operands))
    DONE;
})

;; Be careful, fmovd does not exist when !v9.
;; We match MEM moves directly when we have correct even
;; numbered registers, but fall into splits otherwise.
;; The constraint ordering here is really important to
;; avoid insane problems in reload, especially for patterns
;; of the form:
;;
;; (set (mem:DI (plus:SI (reg:SI 30 %fp)
;;                       (const_int -5016)))
;;      (reg:DI 2 %g2))
;;

(define_insn "*movdi_insn_sp32"
  [(set (match_operand:DI 0 "nonimmediate_operand"
			    "=T,o,r,o,r,r,?*f,  T,?*f,  o,?*e,?*e,  r,?*f,?*e,  T,*b,*b")
        (match_operand:DI 1 "input_operand"
			    " J,J,o,r,i,r,  T,?*f,  o,?*f, *e, *e,?*f,  r,  T,?*e, J, P"))]
  "TARGET_ARCH32
   && (register_operand (operands[0], DImode)
       || register_or_zero_operand (operands[1], DImode))"
  "@
   stx\t%r1, %0
   #
   ldd\t%1, %0
   std\t%1, %0
   #
   #
   ldd\t%1, %0
   std\t%1, %0
   #
   #
   fmovd\t%1, %0
   #
   #
   #
   ldd\t%1, %0
   std\t%1, %0
   fzero\t%0
   fone\t%0"
  [(set_attr "type" "store,*,load,store,*,*,fpload,fpstore,*,*,fpmove,*,*,*,fpload,fpstore,visl,visl")
   (set_attr "subtype" "*,*,regular,*,*,*,*,*,*,*,*,*,*,*,*,*,double,double")
   (set_attr "length" "*,2,*,*,2,2,*,*,2,2,*,2,2,2,*,*,*,*")
   (set_attr "fptype" "*,*,*,*,*,*,*,*,*,*,double,*,*,*,*,*,double,double")
   (set_attr "cpu_feature" "v9,*,*,*,*,*,fpu,fpu,fpu,fpu,v9,fpunotv9,vis3,vis3,fpu,fpu,vis,vis")])

(define_insn "*movdi_insn_sp64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r, m, r,*e,?*e,?*e,  m,b,b")
        (match_operand:DI 1 "input_operand"        "rI,N,m,rJ,*e, r, *e,  m,?*e,J,P"))]
  "TARGET_ARCH64
   && (register_operand (operands[0], DImode)
       || register_or_zero_or_all_ones_operand (operands[1], DImode))"
  "@
   mov\t%1, %0
   sethi\t%%hi(%a1), %0
   ldx\t%1, %0
   stx\t%r1, %0
   movdtox\t%1, %0
   movxtod\t%1, %0
   fmovd\t%1, %0
   ldd\t%1, %0
   std\t%1, %0
   fzero\t%0
   fone\t%0"
  [(set_attr "type" "*,*,load,store,vismv,vismv,fpmove,fpload,fpstore,visl,visl")
   (set_attr "subtype" "*,*,regular,*,movdtox,movxtod,*,*,*,double,double")
   (set_attr "fptype" "*,*,*,*,*,*,double,*,*,double,double")
   (set_attr "cpu_feature" "*,*,*,*,vis3,vis3,*,*,*,vis,vis")])

(define_expand "movdi_pic_label_ref"
  [(set (match_dup 3) (high:DI
     (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")
                 (match_dup 2)] UNSPEC_MOVE_PIC_LABEL)))
   (set (match_dup 4) (lo_sum:DI (match_dup 3)
     (unspec:DI [(match_dup 1) (match_dup 2)] UNSPEC_MOVE_PIC_LABEL)))
   (set (match_operand:DI 0 "register_operand" "=r")
        (minus:DI (match_dup 5) (match_dup 4)))]
  "TARGET_ARCH64 && flag_pic"
{
  crtl->uses_pic_offset_table = 1;
  operands[2] = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  if (!can_create_pseudo_p ())
    {
      operands[3] = operands[0];
      operands[4] = operands[0];
    }
  else
    {
      operands[3] = gen_reg_rtx (DImode);
      operands[4] = gen_reg_rtx (DImode);
    }
  operands[5] = pic_offset_table_rtx;
})

(define_insn "*movdi_high_pic_label_ref"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI
          (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")
                      (match_operand:DI 2 "" "")] UNSPEC_MOVE_PIC_LABEL)))]
  "TARGET_ARCH64 && flag_pic"
  "sethi\t%%hi(%a2-(%a1-.)), %0")

(define_insn "*movdi_lo_sum_pic_label_ref"
  [(set (match_operand:DI 0 "register_operand" "=r")
      (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
        (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")
                    (match_operand:DI 3 "" "")] UNSPEC_MOVE_PIC_LABEL)))]
  "TARGET_ARCH64 && flag_pic"
  "or\t%1, %%lo(%a3-(%a2-.)), %0")

;; SPARC-v9 code model support insns.  See sparc_emit_set_symbolic_const64
;; in sparc.cc to see what is going on here... PIC stuff comes first.

(define_insn "movdi_lo_sum_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "immediate_operand" "in")]
			      UNSPEC_MOVE_PIC)))]
  "TARGET_ARCH64 && flag_pic"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "xor\t%1, %%gdop_lox10(%a2), %0";
#else
  return "or\t%1, %%lo(%a2), %0";
#endif
})

(define_insn "movdi_high_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand 1 "" "")] UNSPEC_MOVE_PIC)))]
  "TARGET_ARCH64 && flag_pic && check_pic (1)"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "sethi\t%%gdop_hix22(%a1), %0";
#else
  return "sethi\t%%hi(%a1), %0";
#endif
})

(define_insn "movdi_pic_gotdata_op"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "r")
	            (match_operand:DI 2 "register_operand" "r")
		    (match_operand 3 "symbolic_operand" "")]
		   UNSPEC_MOVE_GOTDATA))]
  "TARGET_ARCH64 && flag_pic && check_pic (1)"
{
#ifdef HAVE_AS_SPARC_GOTDATA_OP
  return "ldx\t[%1 + %2], %0, %%gdop(%a3)";
#else
  return "ldx\t[%1 + %2], %0";
#endif
}
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "*sethi_di_medlow_embmedany_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (match_operand:DI 1 "medium_pic_operand" "")))]
  "(TARGET_CM_MEDLOW || TARGET_CM_EMBMEDANY) && flag_pic && check_pic (1)"
  "sethi\t%%hi(%a1), %0")

(define_insn "*sethi_di_medlow"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (match_operand:DI 1 "symbolic_operand" "")))]
  "TARGET_CM_MEDLOW && !flag_pic"
  "sethi\t%%hi(%a1), %0")

(define_insn "*losum_di_medlow"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDLOW && !flag_pic"
  "or\t%1, %%lo(%a2), %0")

(define_insn "seth44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")]
			    UNSPEC_SETH44)))]
  "TARGET_CM_MEDMID && !flag_pic"
  "sethi\t%%h44(%a1), %0")

(define_insn "setm44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")]
			      UNSPEC_SETM44)))]
  "TARGET_CM_MEDMID && !flag_pic"
  "or\t%1, %%m44(%a2), %0")

(define_insn "setl44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDMID && !flag_pic"
  "or\t%1, %%l44(%a2), %0")

(define_insn "sethh"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")]
			    UNSPEC_SETHH)))]
  "TARGET_CM_MEDANY && !flag_pic"
  "sethi\t%%hh(%a1), %0")

(define_insn "setlm"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")]
			    UNSPEC_SETLM)))]
  "TARGET_CM_MEDANY && !flag_pic"
  "sethi\t%%lm(%a1), %0")

(define_insn "sethm"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")]
			      UNSPEC_EMB_SETHM)))]
  "TARGET_CM_MEDANY && !flag_pic"
  "or\t%1, %%hm(%a2), %0")

(define_insn "setlo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDANY && !flag_pic"
  "or\t%1, %%lo(%a2), %0")

(define_insn "embmedany_sethi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "data_segment_operand" "")]
			    UNSPEC_EMB_HISUM)))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "sethi\t%%hi(%a1), %0")

(define_insn "embmedany_losum"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "data_segment_operand" "")))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "add\t%1, %%lo(%a2), %0")

(define_insn "embmedany_brsum"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "r")]
	           UNSPEC_EMB_HISUM))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "add\t%1, %_, %0")

(define_insn "embmedany_textuhi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "text_segment_operand" "")]
			    UNSPEC_EMB_TEXTUHI)))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "sethi\t%%uhi(%a1), %0")

(define_insn "embmedany_texthi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "text_segment_operand" "")]
			    UNSPEC_EMB_TEXTHI)))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "sethi\t%%hi(%a1), %0")

(define_insn "embmedany_textulo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "text_segment_operand" "")]
			      UNSPEC_EMB_TEXTULO)))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "or\t%1, %%ulo(%a2), %0")

(define_insn "embmedany_textlo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "text_segment_operand" "")))]
  "TARGET_CM_EMBMEDANY && !flag_pic"
  "or\t%1, %%lo(%a2), %0")

;; Now some patterns to help reload out a bit.
(define_expand "reload_indi"
  [(parallel [(match_operand:DI 0 "register_operand" "=r")
              (match_operand:DI 1 "immediate_operand" "")
              (match_operand:TI 2 "register_operand" "=&r")])]
  "(TARGET_CM_MEDANY || TARGET_CM_EMBMEDANY) && !flag_pic"
{
  sparc_emit_set_symbolic_const64 (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "reload_outdi"
  [(parallel [(match_operand:DI 0 "register_operand" "=r")
              (match_operand:DI 1 "immediate_operand" "")
              (match_operand:TI 2 "register_operand" "=&r")])]
  "(TARGET_CM_MEDANY || TARGET_CM_EMBMEDANY) && !flag_pic"
{
  sparc_emit_set_symbolic_const64 (operands[0], operands[1], operands[2]);
  DONE;
})

;; Split up putting CONSTs and REGs into DI regs when !arch64
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "const_int_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && ((GET_CODE (operands[0]) == REG
        && SPARC_INT_REG_P (REGNO (operands[0])))
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && SPARC_INT_REG_P (REGNO (SUBREG_REG (operands[0])))))"
  [(clobber (const_int 0))]
{
  HOST_WIDE_INT low = trunc_int_for_mode (INTVAL (operands[1]), SImode);
  HOST_WIDE_INT high = trunc_int_for_mode (INTVAL (operands[1]) >> 32, SImode);
  rtx high_part = gen_highpart (SImode, operands[0]);
  rtx low_part = gen_lowpart (SImode, operands[0]);

  emit_move_insn_1 (high_part, GEN_INT (high));

  /* Slick... but this loses if the constant can be done in one insn.  */
  if (low == high && !SPARC_SETHI32_P (high) && !SPARC_SIMM13_P (high))
    emit_move_insn_1 (low_part, high_part);
  else
    emit_move_insn_1 (low_part, GEN_INT (low));

  DONE;
})

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "register_operand" ""))]
  "reload_completed
   && (!TARGET_V9
       || (TARGET_ARCH32
           && sparc_split_reg_reg_legitimate (operands[0], operands[1])))"
  [(clobber (const_int 0))]
{
  sparc_split_reg_reg (operands[0], operands[1], SImode);
  DONE;
})

;; Now handle the cases of memory moves from/to non-even
;; DI mode register pairs.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "memory_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[0], operands[1])"
  [(clobber (const_int 0))]
{
  sparc_split_reg_mem (operands[0], operands[1], SImode);
  DONE;
})

(define_split
  [(set (match_operand:DI 0 "memory_operand" "")
        (match_operand:DI 1 "register_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[1], operands[0])"
  [(clobber (const_int 0))]
{
  sparc_split_mem_reg (operands[0], operands[1], SImode);
  DONE;
})

(define_split
  [(set (match_operand:DI 0 "memory_operand" "")
        (match_operand:DI 1 "const_zero_operand" ""))]
  "reload_completed
   && (!TARGET_V9
       || (TARGET_ARCH32
	   && !mem_min_alignment (operands[0], 8)))
   && offsettable_memref_p (operands[0])"
  [(clobber (const_int 0))]
{
  emit_move_insn_1 (adjust_address (operands[0], SImode, 0), const0_rtx);
  emit_move_insn_1 (adjust_address (operands[0], SImode, 4), const0_rtx);
  DONE;
})

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
	(match_operand:TI 1 "general_operand" ""))]
  "TARGET_ARCH64"
{
  if (sparc_expand_move (TImode, operands))
    DONE;
})

;; We need to prevent reload from splitting TImode moves, because it
;; might decide to overwrite a pointer with the value it points to.
;; In that case we have to do the loads in the appropriate order so
;; that the pointer is not destroyed too early.

(define_insn "*movti_insn_sp64"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=r , o,?*e,?o,b")
        (match_operand:TI 1 "input_operand"        "roJ,rJ, eo, e,J"))]
  "TARGET_ARCH64
   && !TARGET_HARD_QUAD
   && (register_operand (operands[0], TImode)
       || register_or_zero_operand (operands[1], TImode))"
  "#"
  [(set_attr "length" "2,2,2,2,2")
   (set_attr "cpu_feature" "*,*,fpu,fpu,vis")])

(define_insn "*movti_insn_sp64_hq"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=r , o,?*e,?*e,?m,b")
        (match_operand:TI 1 "input_operand"        "roJ,rJ,  e,  m, e,J"))]
  "TARGET_ARCH64
   && TARGET_HARD_QUAD
   && (register_operand (operands[0], TImode)
       || register_or_zero_operand (operands[1], TImode))"
  "@
  #
  #
  fmovq\t%1, %0
  ldq\t%1, %0
  stq\t%1, %0
  #"
  [(set_attr "type" "*,*,fpmove,fpload,fpstore,*")
   (set_attr "length" "2,2,*,*,*,2")])

;; Now all the splits to handle multi-insn TI mode moves.
(define_split
  [(set (match_operand:TI 0 "register_operand" "")
        (match_operand:TI 1 "register_operand" ""))]
  "reload_completed
   && ((TARGET_FPU
        && !TARGET_HARD_QUAD)
       || (!fp_register_operand (operands[0], TImode)
           && !fp_register_operand (operands[1], TImode)))"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_highpart (DImode, set_dest);
  dest2 = gen_lowpart (DImode, set_dest);
  src1 = gen_highpart (DImode, set_src);
  src2 = gen_lowpart (DImode, set_src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movdi (dest2, src2));
      emit_insn (gen_movdi (dest1, src1));
    }
  else
    {
      emit_insn (gen_movdi (dest1, src1));
      emit_insn (gen_movdi (dest2, src2));
    }
  DONE;
})

(define_split
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
        (match_operand:TI 1 "const_zero_operand" ""))]
  "reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx dest1, dest2;

  switch (GET_CODE (set_dest))
    {
    case REG:
      dest1 = gen_highpart (DImode, set_dest);
      dest2 = gen_lowpart (DImode, set_dest);
      break;
    case MEM:
      dest1 = adjust_address (set_dest, DImode, 0);
      dest2 = adjust_address (set_dest, DImode, 8);
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen_movdi (dest1, const0_rtx));
  emit_insn (gen_movdi (dest2, const0_rtx));
  DONE;
})

(define_split
  [(set (match_operand:TI 0 "register_operand" "")
        (match_operand:TI 1 "memory_operand" ""))]
  "reload_completed
   && offsettable_memref_p (operands[1])
   && (!TARGET_HARD_QUAD
       || !fp_register_operand (operands[0], TImode))"
  [(clobber (const_int 0))]
{
  rtx word0 = adjust_address (operands[1], DImode, 0);
  rtx word1 = adjust_address (operands[1], DImode, 8);
  rtx set_dest, dest1, dest2;

  set_dest = operands[0];

  dest1 = gen_highpart (DImode, set_dest);
  dest2 = gen_lowpart (DImode, set_dest);

  /* Now output, ordering such that we don't clobber any registers
     mentioned in the address.  */
  if (reg_overlap_mentioned_p (dest1, word1))

    {
      emit_insn (gen_movdi (dest2, word1));
      emit_insn (gen_movdi (dest1, word0));
    }
  else
   {
      emit_insn (gen_movdi (dest1, word0));
      emit_insn (gen_movdi (dest2, word1));
   }
  DONE;
})

(define_split
  [(set (match_operand:TI 0 "memory_operand" "")
	(match_operand:TI 1 "register_operand" ""))]
  "reload_completed
   && offsettable_memref_p (operands[0])
   && (!TARGET_HARD_QUAD
       || !fp_register_operand (operands[1], TImode))"
  [(clobber (const_int 0))]
{
  rtx set_src = operands[1];

  emit_insn (gen_movdi (adjust_address (operands[0], DImode, 0),
			gen_highpart (DImode, set_src)));
  emit_insn (gen_movdi (adjust_address (operands[0], DImode, 8),
			gen_lowpart (DImode, set_src)));
  DONE;
})


;; Floating point move instructions

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (SFmode, operands))
    DONE;
})

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,f, *r,*r,*r,*r, f,f,*r,m,  m")
	(match_operand:SF 1 "input_operand"         "G,C,f,*rR, Q, S, f,*r,m, m,f,*rG"))]
  "(register_operand (operands[0], SFmode)
    || register_or_zero_or_all_ones_operand (operands[1], SFmode))"
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && (which_alternative == 3
          || which_alternative == 4
          || which_alternative == 5))
    {
      long i;

      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[1]), i);
      operands[1] = GEN_INT (i);
    }

  switch (which_alternative)
    {
    case 0:
      return "fzeros\t%0";
    case 1:
      return "fones\t%0";
    case 2:
      return "fmovs\t%1, %0";
    case 3:
      return "mov\t%1, %0";
    case 4:
      return "sethi\t%%hi(%a1), %0";
    case 5:
      return "#";
    case 6:
      return "movstouw\t%1, %0";
    case 7:
      return "movwtos\t%1, %0";
    case 8:
    case 9:
      return "ld\t%1, %0";
    case 10:
    case 11:
      return "st\t%r1, %0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "visl,visl,fpmove,*,*,*,vismv,vismv,fpload,load,fpstore,store")
   (set_attr "subtype" "single,single,*,*,*,*,movstouw,single,*,regular,*,*")
   (set_attr "cpu_feature" "vis,vis,fpu,*,*,*,vis3,vis3,fpu,*,fpu,*")])

;; The following 3 patterns build SFmode constants in integer registers.

(define_insn "*movsf_lo_sum"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (lo_sum:SF (match_operand:SF 1 "register_operand" "r")
                   (match_operand:SF 2 "fp_const_high_losum_operand" "S")))]
  ""
{
  long i;

  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[2]), i);
  operands[2] = GEN_INT (i);
  return "or\t%1, %%lo(%a2), %0";
})

(define_insn "*movsf_high"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (high:SF (match_operand:SF 1 "fp_const_high_losum_operand" "S")))]
  ""
{
  long i;

  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[1]), i);
  operands[1] = GEN_INT (i);
  return "sethi\t%%hi(%1), %0";
})

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
        (match_operand:SF 1 "fp_const_high_losum_operand" ""))]
  "REG_P (operands[0]) && SPARC_INT_REG_P (REGNO (operands[0]))"
  [(set (match_dup 0) (high:SF (match_dup 1)))
   (set (match_dup 0) (lo_sum:SF (match_dup 0) (match_dup 1)))])

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (DFmode, operands))
    DONE;
})

(define_insn "*movdf_insn_sp32"
  [(set (match_operand:DF 0 "nonimmediate_operand"
			    "=T,o,b,b,e,e,*r, f,  e,T,  f,o, *r,*r, o")
	(match_operand:DF 1 "input_operand"
			    " G,G,G,C,e,e, f,*r,T#F,e,o#F,f,*rF, o,*r"))]
  "TARGET_ARCH32
   && (register_operand (operands[0], DFmode)
       || register_or_zero_or_all_ones_operand (operands[1], DFmode))"
  "@
  stx\t%r1, %0
  #
  fzero\t%0
  fone\t%0
  fmovd\t%1, %0
  #
  #
  #
  ldd\t%1, %0
  std\t%1, %0
  #
  #
  #
  ldd\t%1, %0
  std\t%1, %0"
  [(set_attr "type" "store,*,visl,visl,fpmove,*,*,*,fpload,fpstore,*,*,*,load,store")
   (set_attr "subtype" "*,*,double,double,*,*,*,*,*,*,*,*,*,regular,*")
   (set_attr "length" "*,2,*,*,*,2,2,2,*,*,2,2,2,*,*")
   (set_attr "fptype" "*,*,double,double,double,*,*,*,*,*,*,*,*,*,*")
   (set_attr "cpu_feature" "v9,*,vis,vis,v9,fpunotv9,vis3,vis3,fpu,fpu,fpu,fpu,*,*,*")])

(define_insn "*movdf_insn_sp64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=b,b,e,*r, e,  e,m, *r,*r,  m,*r")
	(match_operand:DF 1 "input_operand"         "G,C,e, e,*r,m#F,e,*rG, m,*rG, F"))]
  "TARGET_ARCH64
   && (register_operand (operands[0], DFmode)
       || register_or_zero_or_all_ones_operand (operands[1], DFmode))"
  "@
  fzero\t%0
  fone\t%0
  fmovd\t%1, %0
  movdtox\t%1, %0
  movxtod\t%1, %0
  ldd\t%1, %0
  std\t%1, %0
  mov\t%r1, %0
  ldx\t%1, %0
  stx\t%r1, %0
  #"
  [(set_attr "type" "visl,visl,fpmove,vismv,vismv,load,store,*,load,store,*")
   (set_attr "subtype" "double,double,*,movdtox,movxtod,regular,*,*,regular,*,*")
   (set_attr "length" "*,*,*,*,*,*,*,*,*,*,2")
   (set_attr "fptype" "double,double,double,double,double,*,*,*,*,*,*")
   (set_attr "cpu_feature" "vis,vis,fpu,vis3,vis3,fpu,fpu,*,*,*,*")])

;; This pattern builds DFmode constants in integer registers.
(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "const_double_operand" ""))]
  "reload_completed
   && REG_P (operands[0])
   && SPARC_INT_REG_P (REGNO (operands[0]))
   && !const_zero_operand (operands[1], GET_MODE (operands[0]))"
  [(clobber (const_int 0))]
{
  operands[0] = gen_raw_REG (DImode, REGNO (operands[0]));

  if (TARGET_ARCH64)
    {
      rtx tem = simplify_subreg (DImode, operands[1], DFmode, 0);
      emit_insn (gen_movdi (operands[0], tem));
    }
  else
    {
      rtx hi = simplify_subreg (SImode, operands[1], DFmode, 0);
      rtx lo = simplify_subreg (SImode, operands[1], DFmode, 4);
      rtx high_part = gen_highpart (SImode, operands[0]);
      rtx low_part = gen_lowpart (SImode, operands[0]);

      gcc_assert (GET_CODE (hi) == CONST_INT);
      gcc_assert (GET_CODE (lo) == CONST_INT);

      emit_move_insn_1 (high_part, hi);

      /* Slick... but this loses if the constant can be done in one insn.  */
      if (lo == hi
	  && !SPARC_SETHI32_P (INTVAL (hi))
	  && !SPARC_SIMM13_P (INTVAL (hi)))
	emit_move_insn_1 (low_part, high_part);
      else
	emit_move_insn_1 (low_part, lo);
    }
  DONE;
})

;; Ok, now the splits to handle all the multi insn and
;; mis-aligned memory address cases.
;; In these splits please take note that we must be
;; careful when V9 but not ARCH64 because the integer
;; register DFmode cases must be handled.
(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "const_zero_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && ((GET_CODE (operands[0]) == REG
	&& SPARC_INT_REG_P (REGNO (operands[0])))
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && SPARC_INT_REG_P (REGNO (SUBREG_REG (operands[0])))))"
  [(clobber (const_int 0))]
{
  emit_move_insn_1 (gen_highpart (SFmode, operands[0]), CONST0_RTX (SFmode));
  emit_move_insn_1 (gen_lowpart (SFmode, operands[0]), CONST0_RTX (SFmode));
  DONE;
})

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "register_operand" ""))]
  "reload_completed
   && (!TARGET_V9
       || (TARGET_ARCH32
	   && sparc_split_reg_reg_legitimate (operands[0], operands[1])))"
  [(clobber (const_int 0))]
{
  sparc_split_reg_reg (operands[0], operands[1], SFmode);
  DONE;
})

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "memory_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[0], operands[1])"
  [(clobber (const_int 0))]
{
  sparc_split_reg_mem (operands[0], operands[1], SFmode);
  DONE;
})

(define_split
  [(set (match_operand:DF 0 "memory_operand" "")
	(match_operand:DF 1 "register_operand" ""))]
  "reload_completed
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[1], operands[0])"
  [(clobber (const_int 0))]
{
  sparc_split_mem_reg (operands[0], operands[1], SFmode);
  DONE;
})

(define_split
  [(set (match_operand:DF 0 "memory_operand" "")
        (match_operand:DF 1 "const_zero_operand" ""))]
  "reload_completed
   && (!TARGET_V9
       || (TARGET_ARCH32
	   && !mem_min_alignment (operands[0], 8)))
   && offsettable_memref_p (operands[0])"
  [(clobber (const_int 0))]
{
  emit_move_insn_1 (adjust_address (operands[0], SFmode, 0), CONST0_RTX (SFmode));
  emit_move_insn_1 (adjust_address (operands[0], SFmode, 4), CONST0_RTX (SFmode));
  DONE;
})

(define_expand "movtf"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(match_operand:TF 1 "general_operand" ""))]
  ""
{
  if (sparc_expand_move (TFmode, operands))
    DONE;
})

(define_insn "*movtf_insn_sp32"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=b, e,o, o,  r")
	(match_operand:TF 1 "input_operand"        " G,oe,e,rG,roG"))]
  "TARGET_ARCH32
   && (register_operand (operands[0], TFmode)
       || register_or_zero_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "4,4,4,4,4")
   (set_attr "cpu_feature" "fpu,fpu,fpu,*,*")])

(define_insn "*movtf_insn_sp64"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=b, e,o, o,  r")
	(match_operand:TF 1 "input_operand"         "G,oe,e,rG,roG"))]
  "TARGET_ARCH64
   && !TARGET_HARD_QUAD
   && (register_operand (operands[0], TFmode)
       || register_or_zero_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "2,2,2,2,2")
   (set_attr "cpu_feature" "fpu,fpu,fpu,*,*")])

(define_insn "*movtf_insn_sp64_hq"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=b,e,e,m, o,  r")
	(match_operand:TF 1 "input_operand"         "G,e,m,e,rG,roG"))]
  "TARGET_ARCH64
   && TARGET_HARD_QUAD
   && (register_operand (operands[0], TFmode)
       || register_or_zero_operand (operands[1], TFmode))"
  "@
  #
  fmovq\t%1, %0
  ldq\t%1, %0
  stq\t%1, %0
  #
  #"
  [(set_attr "type" "*,fpmove,fpload,fpstore,*,*")
   (set_attr "length" "2,*,*,*,2,2")])

;; Now all the splits to handle multi-insn TF mode moves.
(define_split
  [(set (match_operand:TF 0 "register_operand" "")
        (match_operand:TF 1 "register_operand" ""))]
  "reload_completed
   && (TARGET_ARCH32
       || (TARGET_FPU
           && !TARGET_HARD_QUAD)
       || (!fp_register_operand (operands[0], TFmode)
           && !fp_register_operand (operands[1], TFmode)))"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);
  src1 = gen_df_reg (set_src, 0);
  src2 = gen_df_reg (set_src, 1);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movdf (dest2, src2));
      emit_insn (gen_movdf (dest1, src1));
    }
  else
    {
      emit_insn (gen_movdf (dest1, src1));
      emit_insn (gen_movdf (dest2, src2));
    }
  DONE;
})

(define_split
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
        (match_operand:TF 1 "const_zero_operand" ""))]
  "reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx dest1, dest2;

  switch (GET_CODE (set_dest))
    {
    case REG:
      dest1 = gen_df_reg (set_dest, 0);
      dest2 = gen_df_reg (set_dest, 1);
      break;
    case MEM:
      dest1 = adjust_address (set_dest, DFmode, 0);
      dest2 = adjust_address (set_dest, DFmode, 8);
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen_movdf (dest1, CONST0_RTX (DFmode)));
  emit_insn (gen_movdf (dest2, CONST0_RTX (DFmode)));
  DONE;
})

(define_split
  [(set (match_operand:TF 0 "register_operand" "")
        (match_operand:TF 1 "memory_operand" ""))]
  "(reload_completed
    && offsettable_memref_p (operands[1])
    && (TARGET_ARCH32
	|| !TARGET_HARD_QUAD
	|| !fp_register_operand (operands[0], TFmode)))"
  [(clobber (const_int 0))]
{
  rtx word0 = adjust_address (operands[1], DFmode, 0);
  rtx word1 = adjust_address (operands[1], DFmode, 8);
  rtx set_dest, dest1, dest2;

  set_dest = operands[0];

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);

  /* Now output, ordering such that we don't clobber any registers
     mentioned in the address.  */
  if (reg_overlap_mentioned_p (dest1, word1))

    {
      emit_insn (gen_movdf (dest2, word1));
      emit_insn (gen_movdf (dest1, word0));
    }
  else
   {
      emit_insn (gen_movdf (dest1, word0));
      emit_insn (gen_movdf (dest2, word1));
   }
  DONE;
})

(define_split
  [(set (match_operand:TF 0 "memory_operand" "")
	(match_operand:TF 1 "register_operand" ""))]
  "(reload_completed
    && offsettable_memref_p (operands[0])
    && (TARGET_ARCH32
	|| !TARGET_HARD_QUAD
	|| !fp_register_operand (operands[1], TFmode)))"
  [(clobber (const_int 0))]
{
  rtx set_src = operands[1];

  emit_insn (gen_movdf (adjust_address (operands[0], DFmode, 0),
			gen_df_reg (set_src, 0)));
  emit_insn (gen_movdf (adjust_address (operands[0], DFmode, 8),
			gen_df_reg (set_src, 1)));
  DONE;
})


;; SPARC-V9 conditional move instructions

;; We can handle larger constants here for some flavors, but for now we keep
;; it simple and only allow those constants supported by all flavors.
;; Note that emit_conditional_move canonicalizes operands 2,3 so that operand
;; 3 contains the constant if one is present, but we handle either for
;; generality (sparc.cc puts a constant in operand 2).
;;
;; Our instruction patterns, on the other hand, canonicalize such that
;; operand 3 must be the set destination.

(define_expand "mov<I:mode>cc"
  [(set (match_operand:I 0 "register_operand" "")
	(if_then_else:I (match_operand 1 "comparison_operator" "")
			(match_operand:I 2 "arith10_operand" "")
			(match_operand:I 3 "arith10_operand" "")))]
  "TARGET_V9 && !(<I:MODE>mode == DImode && TARGET_ARCH32)"
{
  if (!sparc_expand_conditional_move (<I:MODE>mode, operands))
    FAIL;
  DONE;
})

(define_expand "mov<F:mode>cc"
  [(set (match_operand:F 0 "register_operand" "")
	(if_then_else:F (match_operand 1 "comparison_operator" "")
			(match_operand:F 2 "register_operand" "")
			(match_operand:F 3 "register_operand" "")))]
  "TARGET_V9 && TARGET_FPU"
{
  if (!sparc_expand_conditional_move (<F:MODE>mode, operands))
    FAIL;
  DONE;
})

(define_insn "*mov<I:mode>_cc_v9"
  [(set (match_operand:I 0 "register_operand" "=r")
	(if_then_else:I (match_operator 1 "icc_or_fcc_comparison_operator"
			 [(match_operand 2 "icc_or_fcc_register_operand" "X")
			  (const_int 0)])
			(match_operand:I 3 "arith11_operand" "rL")
			(match_operand:I 4 "register_operand" "0")))]
  "TARGET_V9 && !(<I:MODE>mode == DImode && TARGET_ARCH32)"
  "mov%C1\t%x2, %3, %0"
  [(set_attr "type" "cmove")])

(define_insn "*mov<I:mode>_cc_reg_sp64"
  [(set (match_operand:I 0 "register_operand" "=r")
	(if_then_else:I (match_operator 1 "v9_register_comparison_operator"
		         [(match_operand:DI 2 "register_operand" "r")
			  (const_int 0)])
			(match_operand:I 3 "arith10_operand" "rM")
			(match_operand:I 4 "register_operand" "0")))]
  "TARGET_ARCH64"
  "movr%D1\t%2, %r3, %0"
  [(set_attr "type" "cmove")])

(define_insn "*movsf_cc_v9"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(if_then_else:SF (match_operator 1 "icc_or_fcc_comparison_operator"
			  [(match_operand 2 "icc_or_fcc_register_operand" "X")
			   (const_int 0)])
			 (match_operand:SF 3 "register_operand" "f")
			 (match_operand:SF 4 "register_operand" "0")))]
  "TARGET_V9 && TARGET_FPU"
  "fmovs%C1\t%x2, %3, %0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movsf_cc_reg_sp64"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(if_then_else:SF (match_operator 1 "v9_register_comparison_operator"
			  [(match_operand:DI 2 "register_operand" "r")
			   (const_int 0)])
			 (match_operand:SF 3 "register_operand" "f")
			 (match_operand:SF 4 "register_operand" "0")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "fmovrs%D1\t%2, %3, %0"
  [(set_attr "type" "fpcrmove")])

;; Named because invoked by movtf_cc_v9
(define_insn "movdf_cc_v9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(if_then_else:DF (match_operator 1 "icc_or_fcc_comparison_operator"
			  [(match_operand 2 "icc_or_fcc_register_operand" "X")
			   (const_int 0)])
			 (match_operand:DF 3 "register_operand" "e")
			 (match_operand:DF 4 "register_operand" "0")))]
  "TARGET_V9 && TARGET_FPU"
  "fmovd%C1\t%x2, %3, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "fptype" "double")])

;; Named because invoked by movtf_cc_reg_sp64
(define_insn "movdf_cc_reg_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(if_then_else:DF (match_operator 1 "v9_register_comparison_operator"
			  [(match_operand:DI 2 "register_operand" "r")
			   (const_int 0)])
			 (match_operand:DF 3 "register_operand" "e")
			 (match_operand:DF 4 "register_operand" "0")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "fmovrd%D1\t%2, %3, %0"
  [(set_attr "type" "fpcrmove")
   (set_attr "fptype" "double")])

(define_insn "*movtf_cc_hq_v9"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(if_then_else:TF (match_operator 1 "icc_or_fcc_comparison_operator"
			  [(match_operand 2 "icc_or_fcc_register_operand" "X")
			   (const_int 0)])
			 (match_operand:TF 3 "register_operand" "e")
			 (match_operand:TF 4 "register_operand" "0")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fmovq%C1\t%x2, %3, %0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movtf_cc_reg_hq_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(if_then_else:TF (match_operator 1 "v9_register_comparison_operator"
			  [(match_operand:DI 2 "register_operand" "r")
			   (const_int 0)])
			 (match_operand:TF 3 "register_operand" "e")
			 (match_operand:TF 4 "register_operand" "0")))]
  "TARGET_ARCH64 && TARGET_FPU && TARGET_HARD_QUAD"
  "fmovrq%D1\t%2, %3, %0"
  [(set_attr "type" "fpcrmove")])

(define_insn_and_split "*movtf_cc_v9"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(if_then_else:TF (match_operator 1 "icc_or_fcc_comparison_operator"
			  [(match_operand 2 "icc_or_fcc_register_operand" "X")
			   (const_int 0)])
			 (match_operand:TF 3 "register_operand" "e")
			 (match_operand:TF 4 "register_operand" "0")))]
  "TARGET_V9 && TARGET_FPU && !TARGET_HARD_QUAD"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_srca = operands[3];
  rtx dest1, dest2;
  rtx srca1, srca2;

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);
  srca1 = gen_df_reg (set_srca, 0);
  srca2 = gen_df_reg (set_srca, 1);

  if (reg_overlap_mentioned_p (dest1, srca2))
    {
      emit_insn (gen_movdf_cc_v9 (dest2, operands[1], operands[2],
				  srca2, dest2));
      emit_insn (gen_movdf_cc_v9 (dest1, operands[1], operands[2],
				  srca1, dest1));
    }
  else
    {
      emit_insn (gen_movdf_cc_v9 (dest1, operands[1], operands[2],
				  srca1, dest1));
      emit_insn (gen_movdf_cc_v9 (dest2, operands[1], operands[2],
				  srca2, dest2));
    }
  DONE;
}
  [(set_attr "length" "2")])

(define_insn_and_split "*movtf_cc_reg_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(if_then_else:TF (match_operator 1 "v9_register_comparison_operator"
			  [(match_operand:DI 2 "register_operand" "r")
			   (const_int 0)])
			 (match_operand:TF 3 "register_operand" "e")
			 (match_operand:TF 4 "register_operand" "0")))]
  "TARGET_ARCH64 && TARGET_FPU && !TARGET_HARD_QUAD"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_srca = operands[3];
  rtx dest1, dest2;
  rtx srca1, srca2;

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);
  srca1 = gen_df_reg (set_srca, 0);
  srca2 = gen_df_reg (set_srca, 1);

  if (reg_overlap_mentioned_p (dest1, srca2))
    {
      emit_insn (gen_movdf_cc_reg_sp64 (dest2, operands[1], operands[2],
					srca2, dest2));
      emit_insn (gen_movdf_cc_reg_sp64 (dest1, operands[1], operands[2],
					srca1, dest1));
    }
  else
    {
      emit_insn (gen_movdf_cc_reg_sp64 (dest1, operands[1], operands[2],
				        srca1, dest1));
      emit_insn (gen_movdf_cc_reg_sp64 (dest2, operands[1], operands[2],
					srca2, dest2));
    }
  DONE;
}
  [(set_attr "length" "2")])


;; Zero-extension instructions

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the ldu[hb] insns.

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_16 = GEN_INT (16);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (SImode);
      op1_subbyte *= GET_MODE_SIZE (SImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operands[1],
						op1_subbyte),
			  shift_16));
  emit_insn (gen_lshrsi3 (operands[0], temp, shift_16));
  DONE;
})

(define_insn "*zero_extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "lduh\t%1, %0"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "input_operand" "r,m")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and\t%1, 0xff, %0
   ldub\t%1, %0"
  [(set_attr "type" "*,load")
   (set_attr "subtype" "*,regular")
   (set_attr "us3load_type" "*,3cycle")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "input_operand" "r,m")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and\t%1, 0xff, %0
   ldub\t%1, %0"
  [(set_attr "type" "*,load")
   (set_attr "subtype" "*,regular")
   (set_attr "us3load_type" "*,3cycle")])

(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*zero_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "input_operand" "r,m")))]
  "TARGET_ARCH64 && GET_CODE (operands[1]) != CONST_INT"
  "@
   and\t%1, 0xff, %0
   ldub\t%1, %0"
  [(set_attr "type" "*,load")
   (set_attr "subtype" "*,regular")
   (set_attr "us3load_type" "*,3cycle")])

(define_expand "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:HI 1 "register_operand" "")))]
  "TARGET_ARCH64"
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_48 = GEN_INT (48);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (DImode);
      op1_subbyte *= GET_MODE_SIZE (DImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operands[1],
						op1_subbyte),
			  shift_48));
  emit_insn (gen_lshrdi3 (operands[0], temp, shift_48));
  DONE;
})

(define_insn "*zero_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "lduh\t%1, %0"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

;; ??? Write truncdisi pattern using sra?

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendsidi2_insn_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (match_operand:SI 1 "input_operand" "r,m,*f")))]
  "TARGET_ARCH64
   && GET_CODE (operands[1]) != CONST_INT"
  "@
   srl\t%1, 0, %0
   lduw\t%1, %0
   movstouw\t%1, %0"
  [(set_attr "type" "shift,load,vismv")
   (set_attr "subtype" "*,regular,movstouw")
   (set_attr "cpu_feature" "*,*,vis3")])

(define_insn_and_split "*zero_extendsidi2_insn_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  "operands[2] = gen_lowpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[0]);"
  [(set_attr "length" "2")])

;; Simplify comparisons of extended values.

(define_insn "*cmp_zero_extendqisi2"
  [(set (reg:CC CC_REG)
	(compare:CC (zero_extend:SI (match_operand:QI 0 "register_operand" "r"))
		    (const_int 0)))]
  ""
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_qi"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:QI 0 "register_operand" "r")
		    (const_int 0)))]
  ""
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqisi2_set"
  [(set (reg:CC CC_REG)
	(compare:CC (zero_extend:SI (match_operand:QI 1 "register_operand" "r"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_dup 1)))]
  ""
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqisi2_andcc_set"
  [(set (reg:CC CC_REG)
	(compare:CC (and:SI (match_operand:SI 1 "register_operand" "r")
			    (const_int 255))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (subreg:QI (match_dup 1) 0)))]
  ""
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqidi2"
  [(set (reg:CCX CC_REG)
	(compare:CCX (zero_extend:DI (match_operand:QI 0 "register_operand" "r"))
		     (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_qi_sp64"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operand:QI 0 "register_operand" "r")
		     (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqidi2_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		     (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqidi2_andcc_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX (and:DI (match_operand:DI 1 "register_operand" "r")
			     (const_int 255))
		     (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (subreg:QI (match_dup 1) 0)))]
  "TARGET_ARCH64"
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])

;; Similarly, handle {SI,DI}->QI mode truncation followed by a compare.

(define_insn "*cmp_siqi_trunc"
  [(set (reg:CC CC_REG)
	(compare:CC (subreg:QI (match_operand:SI 0 "register_operand" "r") 3)
		    (const_int 0)))]
  ""
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_siqi_trunc_set"
  [(set (reg:CC CC_REG)
	(compare:CC (subreg:QI (match_operand:SI 1 "register_operand" "r") 3)
		    (const_int 0)))
   (set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_dup 1) 3))]
  ""
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_diqi_trunc"
  [(set (reg:CC CC_REG)
	(compare:CC (subreg:QI (match_operand:DI 0 "register_operand" "r") 7)
		    (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_diqi_trunc_set"
  [(set (reg:CC CC_REG)
	(compare:CC (subreg:QI (match_operand:DI 1 "register_operand" "r") 7)
		    (const_int 0)))
   (set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_dup 1) 7))]
  "TARGET_ARCH64"
  "andcc\t%1, 0xff, %0"
  [(set_attr "type" "compare")])


;; Sign-extension instructions

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the lds[hb] insns.

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_16 = GEN_INT (16);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (SImode);
      op1_subbyte *= GET_MODE_SIZE (SImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operands[1],
						op1_subbyte),
			  shift_16));
  emit_insn (gen_ashrsi3 (operands[0], temp, shift_16));
  DONE;
})

(define_insn "*sign_extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "ldsh\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = GEN_INT (24);
  int op1_subbyte = 0;
  int op0_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (SImode);
      op1_subbyte *= GET_MODE_SIZE (SImode);
      operands[1] = XEXP (operands[1], 0);
    }
  if (GET_CODE (operands[0]) == SUBREG)
    {
      op0_subbyte = SUBREG_BYTE (operands[0]);
      op0_subbyte /= GET_MODE_SIZE (SImode);
      op0_subbyte *= GET_MODE_SIZE (SImode);
      operands[0] = XEXP (operands[0], 0);
    }
  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operands[1],
						op1_subbyte),
			  shift_24));
  if (GET_MODE (operands[0]) != SImode)
    operands[0] = gen_rtx_SUBREG (SImode, operands[0], op0_subbyte);
  emit_insn (gen_ashrsi3 (operands[0], temp, shift_24));
  DONE;
})

(define_insn "*sign_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = GEN_INT (24);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (SImode);
      op1_subbyte *= GET_MODE_SIZE (SImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operands[1],
						op1_subbyte),
			  shift_24));
  emit_insn (gen_ashrsi3 (operands[0], temp, shift_24));
  DONE;
})

(define_insn "*sign_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_expand "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_ARCH64"
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_56 = GEN_INT (56);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (DImode);
      op1_subbyte *= GET_MODE_SIZE (DImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operands[1],
						op1_subbyte),
			  shift_56));
  emit_insn (gen_ashrdi3 (operands[0], temp, shift_56));
  DONE;
})

(define_insn "*sign_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsb\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_expand "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:HI 1 "register_operand" "")))]
  "TARGET_ARCH64"
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_48 = GEN_INT (48);
  int op1_subbyte = 0;

  if (GET_CODE (operands[1]) == SUBREG)
    {
      op1_subbyte = SUBREG_BYTE (operands[1]);
      op1_subbyte /= GET_MODE_SIZE (DImode);
      op1_subbyte *= GET_MODE_SIZE (DImode);
      operands[1] = XEXP (operands[1], 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operands[1],
						op1_subbyte),
			  shift_48));
  emit_insn (gen_ashrdi3 (operands[0], temp, shift_48));
  DONE;
})

(define_insn "*sign_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsh\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_expand "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*sign_extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(sign_extend:DI (match_operand:SI 1 "input_operand" "r,m,*f")))]
  "TARGET_ARCH64"
  "@
  sra\t%1, 0, %0
  ldsw\t%1, %0
  movstosw\t%1, %0"
  [(set_attr "type" "shift,sload,vismv")
   (set_attr "us3load_type" "*,3cycle,*")
   (set_attr "cpu_feature" "*,*,vis3")])


;; Special pattern for optimizing bit-field compares.  This is needed
;; because combine uses this as a canonical form.

(define_insn "*cmp_zero_extract"
  [(set (reg:CC CC_REG)
	(compare:CC
	 (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int_operand" "I")
			  (match_operand:SI 2 "small_int_operand" "I"))
	 (const_int 0)))]
  "INTVAL (operands[2]) > 19"
{
  int len = INTVAL (operands[1]);
  int pos = 32 - INTVAL (operands[2]) - len;
  HOST_WIDE_INT mask = ((1 << len) - 1) << pos;
  operands[1] = GEN_INT (mask);
  return "andcc\t%0, %1, %%g0";
}
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extract_sp64"
  [(set (reg:CCX CC_REG)
	(compare:CCX
	 (zero_extract:DI (match_operand:DI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int_operand" "I")
			  (match_operand:SI 2 "small_int_operand" "I"))
	 (const_int 0)))]
  "TARGET_ARCH64 && INTVAL (operands[2]) > 51"
{
  int len = INTVAL (operands[1]);
  int pos = 64 - INTVAL (operands[2]) - len;
  HOST_WIDE_INT mask = (((unsigned HOST_WIDE_INT) 1 << len) - 1) << pos;
  operands[1] = GEN_INT (mask);
  return "andcc\t%0, %1, %%g0";
}
  [(set_attr "type" "compare")])


;; Conversions between float, double and long double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fstod\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "extendsftf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(float_extend:TF (match_operand:SF 1 "register_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT_EXTEND, operands); DONE;")

(define_insn "*extendsftf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fstoq\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "extenddftf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(float_extend:TF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT_EXTEND, operands); DONE;")

(define_insn "*extenddftf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdtoq\t%1, %0"
  [(set_attr "type" "fp")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "fdtos\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")
   (set_attr "fptype_ut699" "single")])

(define_expand "trunctfsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(float_truncate:SF (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT_TRUNCATE, operands); DONE;")

(define_insn "*trunctfsf2_hq"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtos\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "trunctfdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(float_truncate:DF (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT_TRUNCATE, operands); DONE;")

(define_insn "*trunctfdf2_hq"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_truncate:DF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtod\t%1, %0"
  [(set_attr "type" "fp")])


;; Conversion between fixed point and floating point.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitos\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "single")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitod\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "floatsitf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(float:TF (match_operand:SI 1 "register_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT, operands); DONE;")

(define_insn "*floatsitf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fitoq\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "floatunssitf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(unsigned_float:TF (match_operand:SI 1 "register_operand" "")))]
  "TARGET_FPU && TARGET_ARCH64 && !TARGET_HARD_QUAD"
  "emit_tfmode_cvt (UNSIGNED_FLOAT, operands); DONE;")

;; Now the same for 64 bit sources.

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtos\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "floatunsdisf2"
  [(use (match_operand:SF 0 "register_operand" ""))
   (use (match_operand:DI 1 "general_operand" ""))]
  "TARGET_ARCH64 && TARGET_FPU"
  "sparc_emit_floatunsdi (operands, SFmode); DONE;")

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtod\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "floatunsdidf2"
  [(use (match_operand:DF 0 "register_operand" ""))
   (use (match_operand:DI 1 "general_operand" ""))]
  "TARGET_ARCH64 && TARGET_FPU"
  "sparc_emit_floatunsdi (operands, DFmode); DONE;")

(define_expand "floatditf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(float:TF (match_operand:DI 1 "register_operand" "")))]
  "TARGET_FPU && TARGET_V9 && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FLOAT, operands); DONE;")

(define_insn "*floatditf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fxtoq\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "floatunsditf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(unsigned_float:TF (match_operand:DI 1 "register_operand" "")))]
  "TARGET_FPU && TARGET_ARCH64 && !TARGET_HARD_QUAD"
  "emit_tfmode_cvt (UNSIGNED_FLOAT, operands); DONE;")

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_FPU"
  "fstoi\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "single")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_FPU"
  "fdtoi\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")
   (set_attr "fptype_ut699" "single")])

(define_expand "fix_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(fix:SI (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FIX, operands); DONE;")

(define_insn "*fix_trunctfsi2_hq"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtoi\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "fixuns_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && TARGET_ARCH64 && !TARGET_HARD_QUAD"
  "emit_tfmode_cvt (UNSIGNED_FIX, operands); DONE;")

;; Now the same, for V9 targets

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_V9 && TARGET_FPU"
  "fstox\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "fixuns_truncsfdi2"
  [(use (match_operand:DI 0 "register_operand" ""))
   (use (match_operand:SF 1 "general_operand" ""))]
  "TARGET_ARCH64 && TARGET_FPU"
  "sparc_emit_fixunsdi (operands, SFmode); DONE;")

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU"
  "fdtox\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_expand "fixuns_truncdfdi2"
  [(use (match_operand:DI 0 "register_operand" ""))
   (use (match_operand:DF 1 "general_operand" ""))]
  "TARGET_ARCH64 && TARGET_FPU"
  "sparc_emit_fixunsdi (operands, DFmode); DONE;")

(define_expand "fix_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(fix:DI (match_operand:TF 1 "general_operand" "")))]
  "TARGET_V9 && TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_cvt (FIX, operands); DONE;")

(define_insn "*fix_trunctfdi2_hq"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fqtox\t%1, %0"
  [(set_attr "type" "fp")])

(define_expand "fixuns_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unsigned_fix:DI (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && TARGET_ARCH64 && !TARGET_HARD_QUAD"
  "emit_tfmode_cvt (UNSIGNED_FIX, operands); DONE;")


;; Integer addition/subtraction instructions.

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "arith_double_add_operand" "")))]
  ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_adddi3_sp32 (operands[0], operands[1], operands[2]));
      DONE;
    }
})

;; Turning an add/sub instruction into the other changes the Carry flag
;; so the 4096 trick cannot be used for operations in CCXCmode.

(define_expand "uaddvdi4"
  [(parallel [(set (reg:CCXC CC_REG)
		   (compare:CCXC (plus:DI (match_operand:DI 1 "register_operand")
					  (match_operand:DI 2 "arith_double_operand"))
			         (match_dup 1)))
	      (set (match_operand:DI 0 "register_operand")
		   (plus:DI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ltu (reg:CCXC CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_uaddvdi4_sp32 (operands[0], operands[1], operands[2]));
      rtx x = gen_rtx_LTU (VOIDmode, gen_rtx_REG (CCCmode, SPARC_ICC_REG),
				     const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[3]));
      DONE;
    }
})

;; Turning an add/sub instruction into the other does not change the Overflow
;; flag so the 4096 trick can be used for operations in CCXVmode.

(define_expand "addvdi4"
  [(parallel [(set (reg:CCXV CC_REG)
		   (compare:CCXV (plus:DI (match_operand:DI 1 "register_operand")
					  (match_operand:DI 2 "arith_double_add_operand"))
			         (unspec:DI [(match_dup 1) (match_dup 2)]
					    UNSPEC_ADDV)))
	      (set (match_operand:DI 0 "register_operand")
		   (plus:DI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ne (reg:CCXV CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_addvdi4_sp32 (operands[0], operands[1], operands[2]));
      rtx x = gen_rtx_NE (VOIDmode, gen_rtx_REG (CCVmode, SPARC_ICC_REG),
				    const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[3]));
      DONE;
    }
})

(define_insn_and_split "adddi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (match_operand:DI 1 "register_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC CC_REG))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (plus:SI (match_dup 4) (match_dup 5))
				(match_dup 4)))
	      (set (match_dup 3)
		   (plus:SI (match_dup 4) (match_dup 5)))])
   (set (match_dup 6)
	(plus:SI (plus:SI (match_dup 7) (match_dup 8))
		 (ltu:SI (reg:CCC CC_REG) (const_int 0))))]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart_mode (SImode, DImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "uaddvdi4_sp32"
  [(set (reg:CCC CC_REG)
	(compare:CCC (plus:DI (match_operand:DI 1 "register_operand" "%r")
			      (match_operand:DI 2 "arith_double_operand" "rHI"))
		     (match_dup 1)))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (plus:SI (match_dup 4) (match_dup 5))
				(match_dup 4)))
	      (set (match_dup 3)
		   (plus:SI (match_dup 4) (match_dup 5)))])
   (parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (zero_extend:DI
				  (plus:SI (plus:SI (match_dup 7) (match_dup 8))
					   (ltu:SI (reg:CCC CC_REG)
						   (const_int 0))))
				(plus:DI (plus:DI (zero_extend:DI (match_dup 7))
						  (zero_extend:DI (match_dup 8)))
					 (ltu:DI (reg:CCC CC_REG)
						 (const_int 0)))))
	      (set (match_dup 6)
		   (plus:SI (plus:SI (match_dup 7) (match_dup 8))
			    (ltu:SI (reg:CCC CC_REG)
				    (const_int 0))))])]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart_mode (SImode, DImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "addvdi4_sp32"
  [(set (reg:CCV CC_REG)
	(compare:CCV (plus:DI (match_operand:DI 1 "register_operand" "%r")
			      (match_operand:DI 2 "arith_double_operand" "rHI"))
		     (unspec:DI [(match_dup 1) (match_dup 2)] UNSPEC_ADDV)))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (plus:SI (match_dup 4) (match_dup 5))
				(match_dup 4)))
	      (set (match_dup 3)
		   (plus:SI (match_dup 4) (match_dup 5)))])
   (parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (plus:SI (plus:SI (match_dup 7) (match_dup 8))
					 (ltu:SI (reg:CCC CC_REG)
						 (const_int 0)))
				(unspec:SI [(plus:SI (match_dup 7) (match_dup 8))
					    (ltu:SI (reg:CCC CC_REG)
						     (const_int 0))]
					   UNSPEC_ADDV)))
	      (set (match_dup 6)
		   (plus:SI (plus:SI (match_dup 7) (match_dup 8))
			    (ltu:SI (reg:CCC CC_REG) (const_int 0))))])]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart_mode (SImode, DImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "*addx_extend_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (plus:SI (plus:SI
                                   (match_operand:SI 1 "register_operand" "%r")
                                   (match_operand:SI 2 "arith_operand" "rI"))
                                 (ltu:SI (reg:CCC CC_REG) (const_int 0)))))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (plus:SI (plus:SI (match_dup 1) (match_dup 2))
                               (ltu:SI (reg:CCC CC_REG) (const_int 0))))
   (set (match_dup 4) (const_int 0))]
  "operands[3] = gen_lowpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[0]);"
  [(set_attr "length" "2")])

(define_insn_and_split "*adddi3_extend_sp32"
  [(set (match_operand:DI 0 "register_operand" "=&r")
        (plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
                 (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
                   (compare:CCC (plus:SI (match_dup 3) (match_dup 1))
                                (match_dup 3)))
              (set (match_dup 5) (plus:SI (match_dup 3) (match_dup 1)))])
   (set (match_dup 6)
        (plus:SI (plus:SI (match_dup 4) (const_int 0))
                 (ltu:SI (reg:CCC CC_REG) (const_int 0))))]
  "operands[3] = gen_lowpart (SImode, operands[2]);
   operands[4] = gen_highpart (SImode, operands[2]);
   operands[5] = gen_lowpart (SImode, operands[0]);
   operands[6] = gen_highpart (SImode, operands[0]);"
  [(set_attr "length" "2")])

(define_insn "*adddi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "register_operand" "%r,r")
		 (match_operand:DI 2 "arith_add_operand" "rI,O")))]
  "TARGET_ARCH64"
  "@
   add\t%1, %2, %0
   sub\t%1, -%2, %0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "arith_add_operand" "rI,O")))]
  ""
  "@
   add\t%1, %2, %0
   sub\t%1, -%2, %0")

;; Turning an add/sub instruction into the other changes the Carry flag
;; so the 4096 trick cannot be used for operations in CCCmode.

(define_expand "uaddvsi4"
  [(parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (plus:SI (match_operand:SI 1 "register_operand")
					 (match_operand:SI 2 "arith_operand"))
			        (match_dup 1)))
	      (set (match_operand:SI 0 "register_operand")
		   (plus:SI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ltu (reg:CCC CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 "")

;; Turning an add/sub instruction into the other does not change the Overflow
;; flag so the 4096 trick can be used for operations in CCVmode.

(define_expand "addvsi4"
  [(parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (plus:SI (match_operand:SI 1 "register_operand")
					 (match_operand:SI 2 "arith_add_operand"))
			        (unspec:SI [(match_dup 1) (match_dup 2)]
					   UNSPEC_ADDV)))
	      (set (match_operand:SI 0 "register_operand")
		   (plus:SI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ne (reg:CCV CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 "")

(define_insn "*cmp_ccnz_plus"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (plus:SI (match_operand:SI 0 "register_operand" "%r")
			       (match_operand:SI 1 "arith_operand" "rI"))
		      (const_int 0)))]
  ""
  "addcc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_plus"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (plus:DI (match_operand:DI 0 "register_operand" "%r")
			        (match_operand:DI 1 "arith_operand" "rI"))
		       (const_int 0)))]
  "TARGET_ARCH64"
  "addcc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccnz_plus_set"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (plus:SI (match_operand:SI 1 "register_operand" "%r")
			       (match_operand:SI 2 "arith_operand" "rI"))
		      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "addcc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_plus_set"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (plus:DI (match_operand:DI 1 "register_operand" "%r")
			        (match_operand:DI 2 "arith_operand" "rI"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "addcc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_plus"
  [(set (reg:CCC CC_REG)
	(compare:CCC (plus:SI (match_operand:SI 0 "register_operand" "%r")
			      (match_operand:SI 1 "arith_operand" "rI"))
		     (match_dup 0)))]
  ""
  "addcc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxc_plus"
  [(set (reg:CCXC CC_REG)
	(compare:CCXC (plus:DI (match_operand:DI 0 "register_operand" "%r")
			       (match_operand:DI 1 "arith_operand" "rI"))
		      (match_dup 0)))]
  "TARGET_ARCH64"
  "addcc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_plus_set"
  [(set (reg:CCC CC_REG)
	(compare:CCC (plus:SI (match_operand:SI 1 "register_operand" "%r")
			      (match_operand:SI 2 "arith_operand" "rI"))
		     (match_dup 1)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "addcc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxc_plus_set"
  [(set (reg:CCXC CC_REG)
	(compare:CCXC (plus:DI (match_operand:DI 1 "register_operand" "%r")
			       (match_operand:DI 2 "arith_operand" "rI"))
		      (match_dup 1)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "addcc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_plus_sltu_set"
  [(set (reg:CCC CC_REG)
	(compare:CCC (zero_extend:DI
		       (plus:SI
			 (plus:SI (match_operand:SI 1 "register_operand" "%r")
				  (match_operand:SI 2 "arith_operand" "rI"))
		       (ltu:SI (reg:CCC CC_REG) (const_int 0))))
		     (plus:DI (plus:DI (zero_extend:DI (match_dup 1))
				       (zero_extend:DI (match_dup 2)))
			      (ltu:DI (reg:CCC CC_REG) (const_int 0)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (plus:SI (match_dup 1) (match_dup 2))
		 (ltu:SI (reg:CCC CC_REG) (const_int 0))))]
  ""
  "addxcc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_plus"
  [(set (reg:CCV CC_REG)
	(compare:CCV (plus:SI (match_operand:SI 0 "register_operand" "%r,r")
			      (match_operand:SI 1 "arith_add_operand" "rI,O"))
		     (unspec:SI [(match_dup 0) (match_dup 1)] UNSPEC_ADDV)))]
  ""
  "@
   addcc\t%0, %1, %%g0
   subcc\t%0, -%1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_plus"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (plus:DI (match_operand:DI 0 "register_operand" "%r,r")
			       (match_operand:DI 1 "arith_add_operand" "rI,O"))
		      (unspec:DI [(match_dup 0) (match_dup 1)] UNSPEC_ADDV)))]
  "TARGET_ARCH64"
  "@
   addcc\t%0, %1, %%g0
   subcc\t%0, -%1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_plus_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV (plus:SI (match_operand:SI 1 "register_operand" "%r,r")
			      (match_operand:SI 2 "arith_add_operand" "rI,O"))
		     (unspec:SI [(match_dup 1) (match_dup 2)] UNSPEC_ADDV)))
   (set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   addcc\t%1, %2, %0
   subcc\t%1, -%2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_plus_set"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (plus:DI (match_operand:DI 1 "register_operand" "%r,r")
			       (match_operand:DI 2 "arith_add_operand" "rI,O"))
		      (unspec:DI [(match_dup 1) (match_dup 2)] UNSPEC_ADDV)))
   (set (match_operand:DI 0 "register_operand" "=r,r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "@
   addcc\t%1, %2, %0
   subcc\t%1, -%2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_plus_sltu_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV (plus:SI (plus:SI (match_operand:SI 1 "register_operand" "%r")
				       (match_operand:SI 2 "arith_operand" "rI"))
			      (ltu:SI (reg:CCC CC_REG) (const_int 0)))
		     (unspec:SI [(plus:SI (match_dup 1) (match_dup 2))
				 (ltu:SI (reg:CCC CC_REG) (const_int 0))]
				UNSPEC_ADDV)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (plus:SI (match_dup 1) (match_dup 2))
		 (ltu:SI (reg:CCC CC_REG) (const_int 0))))]
  ""
  "addxcc\t%1, %2, %0"
  [(set_attr "type" "compare")])


(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "arith_double_add_operand" "")))]
  ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_subdi3_sp32 (operands[0], operands[1], operands[2]));
      DONE;
    }
})

;; Turning an add/sub instruction into the other changes the Carry flag
;; so the 4096 trick cannot be used for operations in CCXmode.

(define_expand "usubvdi4"
  [(parallel [(set (reg:CCX CC_REG)
		   (compare:CCX (match_operand:DI 1 "register_or_zero_operand")
				(match_operand:DI 2 "arith_double_operand")))
	      (set (match_operand:DI 0 "register_operand")
		   (minus:DI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ltu (reg:CCX CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 ""
{
  if (operands[1] == const0_rtx)
    {
      emit_insn (gen_unegvdi3 (operands[0], operands[2], operands[3]));
      DONE;
    }

  if (TARGET_ARCH32)
    {
      emit_insn (gen_usubvdi4_sp32 (operands[0], operands[1], operands[2]));
      rtx x = gen_rtx_LTU (VOIDmode, gen_rtx_REG (CCCmode, SPARC_ICC_REG),
				     const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[3]));
      DONE;
    }
})

;; Turning an add/sub instruction into the other does not change the Overflow
;; flag so the 4096 trick can be used for operations in CCXVmode.

(define_expand "subvdi4"
  [(parallel [(set (reg:CCXV CC_REG)
		   (compare:CCXV (minus:DI (match_operand:DI 1 "register_operand")
					   (match_operand:DI 2 "arith_double_add_operand"))
			         (unspec:DI [(match_dup 1) (match_dup 2)]
					    UNSPEC_SUBV)))
	      (set (match_operand:DI 0 "register_operand")
		   (minus:DI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ne (reg:CCXV CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_subvdi4_sp32 (operands[0], operands[1], operands[2]));
      rtx x = gen_rtx_NE (VOIDmode, gen_rtx_REG (CCVmode, SPARC_ICC_REG),
				    const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[3]));
      DONE;
    }
})

(define_insn_and_split "subdi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC CC_REG))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REG)
		   (compare:CC (match_dup 4) (match_dup 5)))
	      (set (match_dup 3)
		   (minus:SI (match_dup 4) (match_dup 5)))])
   (set (match_dup 6)
	(minus:SI (minus:SI (match_dup 7) (match_dup 8))
		  (ltu:SI (reg:CC CC_REG) (const_int 0))))]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart (SImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "usubvdi4_sp32"
  [(set (reg:CCC CC_REG)
	(compare:CCC (match_operand:DI 1 "register_operand" "r")
		     (match_operand:DI 2 "arith_double_operand" "rHI")))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REG)
		   (compare:CC (match_dup 4) (match_dup 5)))
	      (set (match_dup 3)
		   (minus:SI (match_dup 4) (match_dup 5)))])
   (parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (zero_extend:DI
				  (minus:SI (minus:SI (match_dup 7)
						      (ltu:SI (reg:CC CC_REG)
							      (const_int 0)))
					    (match_dup 8)))
				(minus:DI
				  (minus:DI (zero_extend:DI (match_dup 7))
					    (ltu:DI (reg:CC CC_REG)
						    (const_int 0)))
				  (zero_extend:DI (match_dup 8)))))
	      (set (match_dup 6)
		   (minus:SI (minus:SI (match_dup 7)
				       (ltu:SI (reg:CC CC_REG)
					       (const_int 0)))
			     (match_dup 8)))])]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart_mode (SImode, DImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "subvdi4_sp32"
  [(set (reg:CCV CC_REG)
	(compare:CCV (minus:DI (match_operand:DI 1 "register_operand" "%r")
			       (match_operand:DI 2 "arith_double_operand" "rHI"))
		     (unspec:DI [(match_dup 1) (match_dup 2)] UNSPEC_SUBV)))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REG)
		   (compare:CC (match_dup 4) (match_dup 5)))
	      (set (match_dup 3)
		   (minus:SI (match_dup 4) (match_dup 5)))])
   (parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (minus:SI (minus:SI (match_dup 7) (match_dup 8))
					  (ltu:SI (reg:CC CC_REG)
						  (const_int 0)))
				(unspec:SI [(minus:SI (match_dup 7) (match_dup 8))
					    (ltu:SI (reg:CC CC_REG)
						    (const_int 0))]
					   UNSPEC_SUBV)))
	      (set (match_dup 6)
		   (minus:SI (minus:SI (match_dup 7) (match_dup 8))
			     (ltu:SI (reg:CC CC_REG) (const_int 0))))])]
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart_mode (SImode, DImode, operands[1]);
  operands[8] = gen_highpart_mode (SImode, DImode, operands[2]);
}
  [(set_attr "length" "2")])

(define_insn_and_split "*subx_extend_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (minus:SI
				    (match_operand:SI 1 "register_or_zero_operand" "rJ")
				    (match_operand:SI 2 "arith_operand" "rI"))
                                  (ltu:SI (reg:CCC CC_REG) (const_int 0)))))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (minus:SI (minus:SI (match_dup 1) (match_dup 2))
                                (ltu:SI (reg:CCC CC_REG) (const_int 0))))
   (set (match_dup 4) (const_int 0))]
  "operands[3] = gen_lowpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[0]);"
  [(set_attr "length" "2")])

(define_insn_and_split "*subdi3_extend_sp32"
  [(set (match_operand:DI 0 "register_operand" "=&r")
      (minus:DI (match_operand:DI 1 "register_operand" "r")
                (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:CC CC_REG))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REG)
                   (compare:CC (match_dup 3) (match_dup 2)))
              (set (match_dup 5) (minus:SI (match_dup 3) (match_dup 2)))])
   (set (match_dup 6)
        (minus:SI (minus:SI (match_dup 4) (const_int 0))
                  (ltu:SI (reg:CC CC_REG) (const_int 0))))]
  "operands[3] = gen_lowpart (SImode, operands[1]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_lowpart (SImode, operands[0]);
   operands[6] = gen_highpart (SImode, operands[0]);"
  [(set_attr "length" "2")])

(define_insn "*subdi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(minus:DI (match_operand:DI 1 "register_operand" "r,r")
		  (match_operand:DI 2 "arith_add_operand" "rI,O")))]
  "TARGET_ARCH64"
  "@
   sub\t%1, %2, %0
   add\t%1, -%2, %0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "r,r")
		  (match_operand:SI 2 "arith_add_operand" "rI,O")))]
  ""
  "@
   sub\t%1, %2, %0
   add\t%1, -%2, %0")

;; Turning an add/sub instruction into the other changes the Carry flag
;; so the 4096 trick cannot be used for operations in CCmode.

(define_expand "usubvsi4"
  [(parallel [(set (reg:CC CC_REG)
		   (compare:CC (match_operand:SI 1 "register_or_zero_operand")
			       (match_operand:SI 2 "arith_operand")))
	      (set (match_operand:SI 0 "register_operand")
		   (minus:SI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ltu (reg:CC CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 ""
{
  if (operands[1] == const0_rtx)
    {
      emit_insn (gen_unegvsi3 (operands[0], operands[2], operands[3]));
      DONE;
    }
})

;; Turning an add/sub instruction into the other does not change the Overflow
;; flag so the 4096 trick can be used for operations in CCVmode.

(define_expand "subvsi4"
  [(parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (minus:SI (match_operand:SI 1 "register_operand")
					  (match_operand:SI 2 "arith_add_operand"))
			        (unspec:SI [(match_dup 1) (match_dup 2)]
					   UNSPEC_SUBV)))
	      (set (match_operand:SI 0 "register_operand")
		   (minus:SI (match_dup 1) (match_dup 2)))])
   (set (pc) (if_then_else (ne (reg:CCV CC_REG) (const_int 0))
			   (label_ref (match_operand 3))
			   (pc)))]
 "")

(define_insn "*cmp_ccnz_minus"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (minus:SI (match_operand:SI 0 "register_or_zero_operand" "rJ")
				(match_operand:SI 1 "arith_operand" "rI"))
		      (const_int 0)))]
  ""
  "subcc\t%r0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_minus"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (minus:DI (match_operand:DI 0 "register_or_zero_operand" "rJ")
				 (match_operand:DI 1 "arith_operand" "rI"))
		       (const_int 0)))]
  "TARGET_ARCH64"
  "subcc\t%r0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccnz_minus_set"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (minus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
				(match_operand:SI 2 "arith_operand" "rI"))
		      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "subcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_minus_set"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (minus:DI (match_operand:DI 1 "register_or_zero_operand" "rJ")
				 (match_operand:DI 2 "arith_operand" "rI"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "subcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmpsi_set"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 1 "register_or_zero_operand" "rJ")
		    (match_operand:SI 2 "arith_operand" "rI")))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "subcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmpdi_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operand:DI 1 "register_or_zero_operand" "rJ")
		     (match_operand:DI 2 "arith_operand" "rI")))
   (set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "subcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_minus_sltu_set"
  [(set (reg:CCC CC_REG)
	(compare:CCC (zero_extend:DI
		       (minus:SI
			 (minus:SI
			   (match_operand:SI 1 "register_or_zero_operand" "rJ")
			   (ltu:SI (reg:CC CC_REG) (const_int 0)))
			 (match_operand:SI 2 "arith_operand" "rI")))
		     (minus:DI
		       (minus:DI
			 (zero_extend:DI (match_dup 1))
			 (ltu:DI (reg:CC CC_REG) (const_int 0)))
		       (zero_extend:DI (match_dup 2)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_dup 1)
			    (ltu:SI (reg:CC CC_REG) (const_int 0)))
		  (match_dup 2)))]
  ""
  "subxcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_minus"
  [(set (reg:CCV CC_REG)
	(compare:CCV (minus:SI (match_operand:SI 0 "register_or_zero_operand" "rJ,rJ")
			       (match_operand:SI 1 "arith_add_operand" "rI,O"))
		     (unspec:SI [(match_dup 0) (match_dup 1)] UNSPEC_SUBV)))]
  ""
  "@
   subcc\t%r0, %1, %%g0
   addcc\t%r0, -%1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_minus"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (minus:DI (match_operand:DI 0 "register_or_zero_operand" "rJ,rJ")
			        (match_operand:DI 1 "arith_add_operand" "rI,O"))
		      (unspec:DI [(match_dup 0) (match_dup 1)] UNSPEC_SUBV)))]
  "TARGET_ARCH64"
  "@
   subcc\t%r0, %1, %%g0
   addcc\t%r0, -%1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_minus_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV (minus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ,rJ")
			       (match_operand:SI 2 "arith_add_operand" "rI,O"))
		     (unspec:SI [(match_dup 1) (match_dup 2)] UNSPEC_SUBV)))
   (set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   subcc\t%r1, %2, %0
   addcc\t%r1, -%2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_minus_set"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (minus:DI (match_operand:DI 1 "register_or_zero_operand" "rJ,rJ")
			        (match_operand:DI 2 "arith_add_operand" "rI,O"))
		      (unspec:DI [(match_dup 1) (match_dup 2)] UNSPEC_SUBV)))
   (set (match_operand:DI 0 "register_operand" "=r,r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "@
   subcc\t%r1, %2, %0
   addcc\t%r1, -%2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_minus_sltu_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV
	  (minus:SI (minus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
			      (match_operand:SI 2 "arith_operand" "rI"))
		    (ltu:SI (reg:CC CC_REG) (const_int 0)))
	  (unspec:SI [(minus:SI (match_dup 1) (match_dup 2))
		      (ltu:SI (reg:CC CC_REG) (const_int 0))]
		     UNSPEC_SUBV)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_dup 1) (match_dup 2))
		  (ltu:SI (reg:CC CC_REG) (const_int 0))))]
  ""
  "subxcc\t%1, %2, %0"
  [(set_attr "type" "compare")])


;; Integer multiply/divide instructions.

;; The 32-bit multiply/divide instructions are deprecated on v9, but at
;; least in UltraSPARC I, II and IIi it is a win tick-wise.

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "arith_operand" "")
		 (match_operand:SI 2 "arith_operand" "")))]
  "TARGET_HARD_MUL || TARGET_ARCH64"
  "")

(define_insn "*mulsi3_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_HARD_MUL"
  "smul\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_insn "*mulsi3_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "mulx\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_expand "muldi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (match_operand:DI 1 "arith_operand" "")
		 (match_operand:DI 2 "arith_operand" "")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
{
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_muldi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*muldi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "arith_operand" "%r")
		 (match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "mulx\t%1, %2, %0"
  [(set_attr "type" "imul")])

;; V8plus wide multiply.
(define_insn "muldi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r,h")
	(mult:DI (match_operand:DI 1 "arith_operand" "%r,0")
		 (match_operand:DI 2 "arith_operand" "rI,rI")))
   (clobber (match_scratch:SI 3 "=&h,X"))
   (clobber (match_scratch:SI 4 "=&h,X"))]
  "TARGET_V8PLUS"
{
  return output_v8plus_mult (insn, operands, \"mulx\");
}
  [(set_attr "type" "multi")
   (set_attr "length" "9,8")])

(define_insn "*cmp_mul_set"
  [(set (reg:CC CC_REG)
	(compare:CC (mult:SI (match_operand:SI 1 "arith_operand" "%r")
		    (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_dup 1) (match_dup 2)))]
  "TARGET_V8 || TARGET_SPARCLITE || TARGET_DEPRECATED_V8_INSNS"
  "smulcc\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (sign_extend:DI (match_operand:SI 2 "arith_operand" ""))))]
  "TARGET_HARD_MUL"
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	emit_insn (gen_const_mulsidi3_v8plus (operands[0], operands[1],
					      operands[2]));
      else if (TARGET_ARCH32)
	emit_insn (gen_const_mulsidi3_sp32 (operands[0], operands[1],
					    operands[2]));
      else 
	emit_insn (gen_const_mulsidi3_sp64 (operands[0], operands[1],
					    operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_mulsidi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

;; V9 puts the 64-bit product in a 64-bit register.  Only out or global
;; registers can hold 64-bit values in the V8plus environment.
(define_insn "mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\t%1, %2, %L0\n\tsrlx\t%L0, 32, %H0
   smul\t%1, %2, %3\n\tsrlx\t%3, 32, %H0\n\tmov\t%3, %L0"
  [(set_attr "type" "multi")
   (set_attr "length" "2,3")])

(define_insn "const_mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:DI 2 "small_int_operand" "I,I")))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\t%1, %2, %L0\n\tsrlx\t%L0, 32, %H0
   smul\t%1, %2, %3\n\tsrlx\t%3, 32, %H0\n\tmov\t%3, %L0"
  [(set_attr "type" "multi")
   (set_attr "length" "2,3")])

(define_insn "*mulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
{
  return TARGET_SPARCLET
         ? "smuld\t%1, %2, %L0"
         : "smul\t%1, %2, %L0\n\trd\t%%y, %H0";
}
  [(set (attr "type")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_string "imul") (const_string "multi")))
   (set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "*mulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "smul\t%1, %2, %0"
  [(set_attr "type" "imul")])

;; Extra pattern, because sign_extend of a constant isn't valid.

(define_insn "const_mulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:DI 2 "small_int_operand" "I")))]
  "TARGET_HARD_MUL32"
{
  return TARGET_SPARCLET
         ? "smuld\t%1, %2, %L0"
         : "smul\t%1, %2, %L0\n\trd\t%%y, %H0";
}
  [(set (attr "type")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_string "imul") (const_string "multi")))
   (set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "const_mulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:DI 2 "small_int_operand" "I")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "smul\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		     (sign_extend:DI (match_operand:SI 2 "arith_operand" "")))
	    (const_int 32))))]
  "TARGET_HARD_MUL && TARGET_ARCH32"
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_smulsi3_highpart_v8plus (operands[0],
							operands[1],
							operands[2],
							GEN_INT (32)));
	  DONE;
	}
      emit_insn (gen_const_smulsi3_highpart (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_smulsi3_highpart_v8plus (operands[0], operands[1],
					      operands[2], GEN_INT (32)));
      DONE;
    }
})

(define_insn "smulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		     (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
	    (match_operand:SI 3 "small_int_operand" "I,I"))))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\t%1, %2, %0\;srlx\t%0, %3, %0
   smul\t%1, %2, %4\;srlx\t%4, %3, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

;; The combiner changes TRUNCATE in the previous pattern to SUBREG.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(subreg:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		     (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
	    (match_operand:SI 3 "small_int_operand" "I,I")) 4))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\t%1, %2, %0\n\tsrlx\t%0, %3, %0
   smul\t%1, %2, %4\n\tsrlx\t%4, %3, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "const_smulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		     (match_operand:DI 2 "small_int_operand" "I,I"))
	  (match_operand:SI 3 "small_int_operand" "I,I"))))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\t%1, %2, %0\n\tsrlx\t%0, %3, %0
   smul\t%1, %2, %4\n\tsrlx\t%4, %3, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "*smulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		     (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
	  (const_int 32))))]
  "TARGET_HARD_MUL32"
  "smul\t%1, %2, %%g0\n\trd\t%%y, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "const_smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		     (match_operand:DI 2 "small_int_operand" "i"))
	    (const_int 32))))]
  "TARGET_HARD_MUL32"
  "smul\t%1, %2, %%g0\n\trd\t%%y, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (zero_extend:DI (match_operand:SI 2 "uns_arith_operand" ""))))]
  "TARGET_HARD_MUL"
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	emit_insn (gen_const_umulsidi3_v8plus (operands[0], operands[1],
					       operands[2]));
      else if (TARGET_ARCH32)
	emit_insn (gen_const_umulsidi3_sp32 (operands[0], operands[1],
					     operands[2]));
      else 
	emit_insn (gen_const_umulsidi3_sp64 (operands[0], operands[1],
					     operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_umulsidi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   umul\t%1, %2, %L0\n\tsrlx\t%L0, 32, %H0
   umul\t%1, %2, %3\n\tsrlx\t%3, 32, %H0\n\tmov\t%3, %L0"
  [(set_attr "type" "multi")
   (set_attr "length" "2,3")])

(define_insn "*umulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
{
  return TARGET_SPARCLET
         ? "umuld\t%1, %2, %L0"
         : "umul\t%1, %2, %L0\n\trd\t%%y, %H0";
}
  [(set (attr "type")
        (if_then_else (eq_attr "isa" "sparclet")
                      (const_string "imul") (const_string "multi")))
   (set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "*umulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "umul\t%1, %2, %0"
  [(set_attr "type" "imul")])

;; Extra pattern, because sign_extend of a constant isn't valid.

(define_insn "const_umulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:DI 2 "uns_small_int_operand" "")))]
  "TARGET_HARD_MUL32"
{
  return TARGET_SPARCLET
         ? "umuld\t%1, %s2, %L0"
         : "umul\t%1, %s2, %L0\n\trd\t%%y, %H0";
}
  [(set (attr "type")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_string "imul") (const_string "multi")))
   (set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "const_umulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:DI 2 "uns_small_int_operand" "")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "umul\t%1, %s2, %0"
  [(set_attr "type" "imul")])

(define_insn "const_umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:DI 2 "uns_small_int_operand" "")))
   (clobber (match_scratch:SI 3 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\t%1, %s2, %L0\n\tsrlx\t%L0, 32, %H0
   umul\t%1, %s2, %3\n\tsrlx\t%3, 32, %H0\n\tmov\t%3, %L0"
  [(set_attr "type" "multi")
   (set_attr "length" "2,3")])

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		     (zero_extend:DI (match_operand:SI 2 "uns_arith_operand" "")))
	  (const_int 32))))]
  "TARGET_HARD_MUL && TARGET_ARCH32"
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_umulsi3_highpart_v8plus (operands[0],
							operands[1],
							operands[2],
							GEN_INT (32)));
	  DONE;
	}
      emit_insn (gen_const_umulsi3_highpart (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_umulsi3_highpart_v8plus (operands[0], operands[1],
					      operands[2], GEN_INT (32)));
      DONE;
    }
})

(define_insn "umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		     (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
	    (match_operand:SI 3 "small_int_operand" "I,I"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\t%1, %2, %0\n\tsrlx\t%0, %3, %0
   umul\t%1, %2, %4\n\tsrlx\t%4, %3, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "const_umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		     (match_operand:DI 2 "uns_small_int_operand" ""))
	    (match_operand:SI 3 "small_int_operand" "I,I"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\t%1, %s2, %0\n\tsrlx\t%0, %3, %0
   umul\t%1, %s2, %4\n\tsrlx\t%4, %3, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "*umulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		     (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
	    (const_int 32))))]
  "TARGET_HARD_MUL32"
  "umul\t%1, %2, %%g0\n\trd\t%%y, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "const_umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		     (match_operand:DI 2 "uns_small_int_operand" ""))
	    (const_int 32))))]
  "TARGET_HARD_MUL32"
  "umul\t%1, %s2, %%g0\n\trd\t%%y, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])


(define_expand "umulxhi_vis"
  [(set (match_operand:DI 0 "register_operand" "")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (zero_extend:TI (match_operand:DI 1 "arith_operand" ""))
		     (zero_extend:TI (match_operand:DI 2 "arith_operand" "")))
	  (const_int 64))))]
 "TARGET_VIS3"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_umulxhi_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*umulxhi_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (zero_extend:TI (match_operand:DI 1 "arith_operand" "%r"))
		     (zero_extend:TI (match_operand:DI 2 "arith_operand" "rI")))
	  (const_int 64))))]
  "TARGET_VIS3 && TARGET_ARCH64"
  "umulxhi\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_insn "umulxhi_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r,h")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (zero_extend:TI (match_operand:DI 1 "arith_operand" "%r,0"))
		     (zero_extend:TI (match_operand:DI 2 "arith_operand" "rI,rI")))
	  (const_int 64))))
   (clobber (match_scratch:SI 3 "=&h,X"))
   (clobber (match_scratch:SI 4 "=&h,X"))]
  "TARGET_VIS3 && TARGET_ARCH32"
{
  return output_v8plus_mult (insn, operands, \"umulxhi\");
}
  [(set_attr "type" "imul")
   (set_attr "length" "9,8")])

(define_expand "xmulx_vis"
  [(set (match_operand:DI 0 "register_operand" "")
	(truncate:DI
	  (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" ""))
		      (zero_extend:TI (match_operand:DI 2 "arith_operand" ""))]
		     UNSPEC_XMUL)))]
  "TARGET_VIS3"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_xmulx_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*xmulx_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	  (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" "%r"))
		      (zero_extend:TI (match_operand:DI 2 "arith_operand" "rI"))]
		     UNSPEC_XMUL)))]
  "TARGET_VIS3 && TARGET_ARCH64"
  "xmulx\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_insn "xmulx_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r,h")
	(truncate:DI
	  (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" "%r,0"))
		      (zero_extend:TI (match_operand:DI 2 "arith_operand" "rI,rI"))]
		     UNSPEC_XMUL)))
   (clobber (match_scratch:SI 3 "=&h,X"))
   (clobber (match_scratch:SI 4 "=&h,X"))]
  "TARGET_VIS3 && TARGET_ARCH32"
{
  return output_v8plus_mult (insn, operands, \"xmulx\");
}
  [(set_attr "type" "imul")
   (set_attr "length" "9,8")])

(define_expand "xmulxhi_vis"
  [(set (match_operand:DI 0 "register_operand" "")
	(truncate:DI
	  (lshiftrt:TI
	     (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" ""))
			 (zero_extend:TI (match_operand:DI 2 "arith_operand" ""))]
			UNSPEC_XMUL)
	  (const_int 64))))]
  "TARGET_VIS3"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_xmulxhi_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*xmulxhi_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" "%r"))
			(zero_extend:TI (match_operand:DI 2 "arith_operand" "rI"))]
		       UNSPEC_XMUL)
	    (const_int 64))))]
  "TARGET_VIS3 && TARGET_ARCH64"
  "xmulxhi\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_insn "xmulxhi_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r,h")
	(truncate:DI
	  (lshiftrt:TI
	    (unspec:TI [(zero_extend:TI (match_operand:DI 1 "arith_operand" "%r,0"))
		        (zero_extend:TI (match_operand:DI 2 "arith_operand" "rI,rI"))]
		       UNSPEC_XMUL)
	  (const_int 64))))
   (clobber (match_scratch:SI 3 "=&h,X"))
   (clobber (match_scratch:SI 4 "=&h,X"))]
  "TARGET_VIS3 && TARGET_ARCH32"
{
  return output_v8plus_mult (insn, operands, \"xmulxhi\");
}
  [(set_attr "type" "imul")
   (set_attr "length" "9,8")])

(define_expand "divsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (div:SI (match_operand:SI 1 "register_operand" "")
			   (match_operand:SI 2 "input_operand" "")))
	      (clobber (match_scratch:SI 3 ""))])]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
{
  if (TARGET_ARCH64)
    {
      operands[3] = gen_reg_rtx(SImode);
      emit_insn (gen_ashrsi3 (operands[3], operands[1], GEN_INT (31)));
      emit_insn (gen_divsi3_sp64 (operands[0], operands[1], operands[2],
				  operands[3]));
      DONE;
    }
})

;; The V8 architecture specifies that there must be at least 3 instructions
;; between a write to the Y register and a use of it for correct results.
;; We try to fill one of them with a simple constant or a memory load.

(define_insn "divsi3_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(div:SI (match_operand:SI 1 "register_operand" "r,r,r")
		(match_operand:SI 2 "input_operand" "rI,K,m")))
   (clobber (match_scratch:SI 3 "=&r,&r,&r"))]
  "(TARGET_V8 || TARGET_DEPRECATED_V8_INSNS) && TARGET_ARCH32"
{
  output_asm_insn ("sra\t%1, 31, %3", operands);
  output_asm_insn ("wr\t%3, 0, %%y", operands);

  switch (which_alternative)
    {
    case 0:
      if (TARGET_V9)
	return "sdiv\t%1, %2, %0";
      else
	return "nop\n\tnop\n\tnop\n\tsdiv\t%1, %2, %0";
    case 1:
      if (TARGET_V9)
	return "sethi\t%%hi(%a2), %3\n\tsdiv\t%1, %3, %0";
      else
	return "sethi\t%%hi(%a2), %3\n\tnop\n\tnop\n\tsdiv\t%1, %3, %0";
    case 2:
      if (TARGET_V9)
	return "ld\t%2, %3\n\tsdiv\t%1, %3, %0";
      else
	return "ld\t%2, %3\n\tnop\n\tnop\n\tsdiv\t%1, %3, %0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "multi")
   (set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 4) (const_int 6)))])

(define_insn "divsi3_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "input_operand" "rI")))
   (use (match_operand:SI 3 "register_operand" "r"))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "wr\t%%g0, %3, %%y\n\tsdiv\t%1, %2, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(div:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "sdivx\t%1, %2, %0"
  [(set_attr "type" "idiv")])

(define_insn "*cmp_sdiv_cc_set"
  [(set (reg:CC CC_REG)
	(compare:CC (div:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_dup 1) (match_dup 2)))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
{
  output_asm_insn ("sra\t%1, 31, %3", operands);
  output_asm_insn ("wr\t%3, 0, %%y", operands);

  if (TARGET_V9)
    return "sdivcc\t%1, %2, %0";
  else
    return "nop\n\tnop\n\tnop\n\tsdivcc\t%1, %2, %0";
}
  [(set_attr "type" "multi")
   (set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 3) (const_int 6)))])

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(udiv:SI (match_operand:SI 1 "nonimmediate_operand" "")
		 (match_operand:SI 2 "input_operand" "")))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "")

;; The V8 architecture specifies that there must be at least 3 instructions
;; between a write to the Y register and a use of it for correct results.
;; We try to fill one of them with a simple constant or a memory load.

(define_insn "udivsi3_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r,&r,&r,&r")
	(udiv:SI (match_operand:SI 1 "nonimmediate_operand" "r,r,r,m")
		 (match_operand:SI 2 "input_operand" "rI,K,m,r")))]
  "(TARGET_V8 || TARGET_DEPRECATED_V8_INSNS) && TARGET_ARCH32"
{
  output_asm_insn ("wr\t%%g0, 0, %%y", operands);

  switch (which_alternative)
    {
    case 0:
      if (TARGET_V9)
	return "udiv\t%1, %2, %0";
      else
	return "nop\n\tnop\n\tnop\n\tudiv\t%1, %2, %0";
    case 1:
      if (TARGET_V9)
	return "sethi\t%%hi(%a2), %0\n\tudiv\t%1, %0, %0";
      else
	return "sethi\t%%hi(%a2), %0\n\tnop\n\tnop\n\tudiv\t%1, %0, %0";
    case 2:
      if (TARGET_V9)
	return "ld\t%2, %0\n\tudiv\t%1, %0, %0";
      else
	return "ld\t%2, %0\n\tnop\n\tnop\n\tudiv\t%1, %0, %0";
    case 3:
      if (TARGET_V9)
	return "ld\t%1, %0\n\tudiv\t%0, %2, %0";
      else
	return "ld\t%1, %0\n\tnop\n\tnop\n\tudiv\t%0, %2, %0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "multi")
   (set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 3) (const_int 5)))])

(define_insn "udivsi3_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "nonimmediate_operand" "r")
		 (match_operand:SI 2 "input_operand" "rI")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "wr\t%%g0, 0, %%y\n\tudiv\t%1, %2, %0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_insn "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(udiv:DI (match_operand:DI 1 "register_operand" "r")
		 (match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "udivx\t%1, %2, %0"
  [(set_attr "type" "idiv")])

(define_insn "*cmp_udiv_cc_set"
  [(set (reg:CC CC_REG)
	(compare:CC (udiv:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_dup 1) (match_dup 2)))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
{
  output_asm_insn ("wr\t%%g0, 0, %%y", operands);

  if (TARGET_V9)
    return "udivcc\t%1, %2, %0";
  else
    return "nop\n\tnop\n\tnop\n\tudivcc\t%1, %2, %0";
}
  [(set_attr "type" "multi")
   (set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 2) (const_int 5)))])


;; SPARClet multiply/accumulate insns

(define_insn "*smacsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))
		 (match_operand:SI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "smac\t%1, %2, %0"
  [(set_attr "type" "imul")])

(define_insn "*smacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (sign_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (sign_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "smacd\t%1, %2, %L0"
  [(set_attr "type" "imul")])

(define_insn "*umacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (zero_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (zero_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "umacd\t%1, %2, %L0"
  [(set_attr "type" "imul")])


;; Boolean instructions.

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "arith_operand" "%r")
		(match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "and\t%1, %2, %0")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "arith_operand" "%r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "and\t%1, %2, %0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "const_compl_high_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  ""
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (and:SI (not:SI (match_dup 3)) (match_dup 1)))]
{
  operands[4] = GEN_INT (~INTVAL (operands[2]));
})

(define_insn "*and_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "%r"))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_ARCH64"
  "andn\t%2, %1, %0")

(define_insn "*and_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "%r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "andn\t%2, %1, %0")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "arith_operand" "%r")
		(match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "or\t%1, %2, %0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "arith_operand" "%r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "or\t%1, %2, %0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "const_compl_high_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  ""
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (ior:SI (not:SI (match_dup 3)) (match_dup 1)))]
{
  operands[4] = gen_int_mode (~INTVAL (operands[2]), SImode);
})

(define_insn "*or_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_ARCH64"
  "orn\t%2, %1, %0")

(define_insn "*or_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "orn\t%2, %1, %0")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "arith_operand" "%rJ")
		(match_operand:DI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "xor\t%r1, %2, %0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "arith_operand" "%rJ")
		  (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "xor\t%r1, %2, %0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "const_compl_high_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
   ""
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (not:SI (xor:SI (match_dup 3) (match_dup 1))))]
{
  operands[4] = gen_int_mode (~INTVAL (operands[2]), SImode);
})

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (xor:SI (match_operand:SI 1 "register_operand" "")
			(match_operand:SI 2 "const_compl_high_operand" ""))))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  ""
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (xor:SI (match_dup 3) (match_dup 1)))]
{
  operands[4] = gen_int_mode (~INTVAL (operands[2]), SImode);
})

(define_insn "*xor_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "register_or_zero_operand" "rJ")
			(match_operand:DI 2 "arith_operand" "rI"))))]
  "TARGET_ARCH64"
  "xnor\t%r1, %2, %0")

(define_insn "*xor_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
			(match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "xnor\t%r1, %2, %0")

;; These correspond to the above in the case where we also (or only)
;; want to set the condition code.  

(define_insn "*cmp_cc_arith_op"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operator:SI 2 "cc_arith_operator"
		     [(match_operand:SI 0 "arith_operand" "%r")
		      (match_operand:SI 1 "arith_operand" "rI")])
	 (const_int 0)))]
  ""
  "%A2cc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operator:DI 2 "cc_arith_operator"
		      [(match_operand:DI 0 "arith_operand" "%r")
		       (match_operand:DI 1 "arith_operand" "rI")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%A2cc\t%0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_arith_op_set"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operator:SI 3 "cc_arith_operator"
		     [(match_operand:SI 1 "arith_operand" "%r")
		      (match_operand:SI 2 "arith_operand" "rI")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 4 "cc_arith_operator"
         [(match_dup 1) (match_dup 2)]))]
  "GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "%A3cc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operator:DI 3 "cc_arith_operator"
		      [(match_operand:DI 1 "arith_operand" "%r")
		       (match_operand:DI 2 "arith_operand" "rI")])
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(match_operator:DI 4 "cc_arith_operator"
         [(match_dup 1) (match_dup 2)]))]
  "TARGET_ARCH64 && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "%A3cc\t%1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_xor_not"
  [(set (reg:CC CC_REG)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 0 "register_or_zero_operand" "%rJ")
			 (match_operand:SI 1 "arith_operand" "rI")))
	 (const_int 0)))]
  ""
  "xnorcc\t%r0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_xor_not"
  [(set (reg:CCX CC_REG)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 0 "register_or_zero_operand" "%rJ")
			 (match_operand:DI 1 "arith_operand" "rI")))
	 (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc\t%r0, %1, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_xor_not_set"
  [(set (reg:CC CC_REG)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 1 "register_or_zero_operand" "%rJ")
			 (match_operand:SI 2 "arith_operand" "rI")))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_dup 1) (match_dup 2))))]
  ""
  "xnorcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_xor_not_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 1 "register_or_zero_operand" "%rJ")
			 (match_operand:DI 2 "arith_operand" "rI")))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_dup 1) (match_dup 2))))]
  "TARGET_ARCH64"
  "xnorcc\t%r1, %2, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_arith_op_not"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operator:SI 2 "cc_arith_not_operator"
		     [(not:SI (match_operand:SI 0 "arith_operand" "rI"))
		      (match_operand:SI 1 "register_or_zero_operand" "rJ")])
	 (const_int 0)))]
  ""
  "%B2cc\t%r1, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op_not"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operator:DI 2 "cc_arith_not_operator"
		      [(not:DI (match_operand:DI 0 "arith_operand" "rI"))
		       (match_operand:DI 1 "register_or_zero_operand" "rJ")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%B2cc\t%r1, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_arith_op_not_set"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operator:SI 3 "cc_arith_not_operator"
		     [(not:SI (match_operand:SI 1 "arith_operand" "rI"))
		      (match_operand:SI 2 "register_or_zero_operand" "rJ")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 4 "cc_arith_not_operator"
	 [(not:SI (match_dup 1)) (match_dup 2)]))]
  "GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "%B3cc\t%r2, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op_not_set"
  [(set (reg:CCX CC_REG)
	(compare:CCX (match_operator:DI 3 "cc_arith_not_operator"
		      [(not:DI (match_operand:DI 1 "arith_operand" "rI"))
		       (match_operand:DI 2 "register_or_zero_operand" "rJ")])
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(match_operator:DI 4 "cc_arith_not_operator"
         [(not:DI (match_dup 1)) (match_dup 2)]))]
  "TARGET_ARCH64 && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "%B3cc\t%r2, %1, %0"
  [(set_attr "type" "compare")])

;; We cannot use the "neg" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.

(define_expand "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_negdi2_sp32 (operands[0], operands[1]));
      DONE;
    }
})

(define_expand "unegvdi3"
  [(parallel [(set (reg:CCXC CC_REG)
		   (compare:CCXC (not:DI (match_operand:DI 1 "register_operand" ""))
				 (const_int -1)))
	      (set (match_operand:DI 0 "register_operand" "")
		   (neg:DI (match_dup 1)))])
   (set (pc)
        (if_then_else (ltu (reg:CCXC CC_REG) (const_int 0))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
  ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_unegvdi3_sp32 (operands[0], operands[1]));
      rtx x = gen_rtx_LTU (VOIDmode, gen_rtx_REG (CCCmode, SPARC_ICC_REG),
				     const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[2]));
      DONE;
    }
})

(define_expand "negvdi3"
  [(parallel [(set (reg:CCXV CC_REG)
		   (compare:CCXV (neg:DI (match_operand:DI 1 "register_operand" ""))
				 (unspec:DI [(match_dup 1)] UNSPEC_NEGV)))
	      (set (match_operand:DI 0 "register_operand" "")
		   (neg:DI (match_dup 1)))])
   (set (pc)
        (if_then_else (ne (reg:CCXV CC_REG) (const_int 0))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
  ""
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_negvdi3_sp32 (operands[0], operands[1]));
      rtx x = gen_rtx_NE (VOIDmode, gen_rtx_REG (CCVmode, SPARC_ICC_REG),
				    const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (x, XEXP (x, 0), XEXP (x, 1), operands[2]));
      DONE;
    }
})

(define_insn_and_split "negdi2_sp32"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (reg:CC CC_REG))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
                   (compare:CCC (not:SI (match_dup 5)) (const_int -1)))
              (set (match_dup 4) (neg:SI (match_dup 5)))])
   (set (match_dup 2) (minus:SI (minus:SI (const_int 0) (match_dup 3))
                                (ltu:SI (reg:CCC CC_REG) (const_int 0))))]
  "operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[1]);
   operands[4] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_lowpart (SImode, operands[1]);"
  [(set_attr "length" "2")])

(define_insn_and_split "unegvdi3_sp32"
  [(set (reg:CCC CC_REG)
	(compare:CCC (not:DI (match_operand:DI 1 "register_operand" "r"))
		     (const_int -1)))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
                   (compare:CCC (not:SI (match_dup 5)) (const_int -1)))
              (set (match_dup 4) (neg:SI (match_dup 5)))])
   (parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (zero_extend:DI
				  (neg:SI (plus:SI (match_dup 3)
						   (ltu:SI (reg:CCC CC_REG)
							   (const_int 0)))))
				(neg:DI (plus:DI (zero_extend:DI (match_dup 3))
						 (ltu:DI (reg:CCC CC_REG)
							 (const_int 0))))))
	      (set (match_dup 2) (neg:SI (plus:SI (match_dup 3)
						  (ltu:SI (reg:CCC CC_REG)
							  (const_int 0)))))])]
  "operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[1]);
   operands[4] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_lowpart (SImode, operands[1]);"
  [(set_attr "length" "2")])

(define_insn_and_split "negvdi3_sp32"
  [(set (reg:CCV CC_REG)
	(compare:CCV (neg:DI (match_operand:DI 1 "register_operand" "r"))
		     (unspec:DI [(match_dup 1)] UNSPEC_NEGV)))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH32"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CCC CC_REG)
                   (compare:CCC (not:SI (match_dup 5)) (const_int -1)))
              (set (match_dup 4) (neg:SI (match_dup 5)))])
   (parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (neg:SI (plus:SI (match_dup 3)
						 (ltu:SI (reg:CCC CC_REG)
							 (const_int 0))))
				(unspec:SI [(plus:SI (match_dup 3)
						     (ltu:SI (reg:CCC CC_REG)
							     (const_int 0)))]
					   UNSPEC_NEGV)))
	      (set (match_dup 2) (neg:SI (plus:SI (match_dup 3)
						  (ltu:SI (reg:CCC CC_REG)
							  (const_int 0)))))])]
  "operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[1]);
   operands[4] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_lowpart (SImode, operands[1]);"
  [(set_attr "length" "2")])

(define_insn "*negdi2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_ARCH64"
  "sub\t%%g0, %1, %0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub\t%%g0, %1, %0")

(define_expand "unegvsi3"
  [(parallel [(set (reg:CCC CC_REG)
		   (compare:CCC (not:SI (match_operand:SI 1 "register_operand" ""))
				(const_int -1)))
	      (set (match_operand:SI 0 "register_operand" "")
		   (neg:SI (match_dup 1)))])
   (set (pc)
        (if_then_else (ltu (reg:CCC CC_REG) (const_int 0))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
  "")

(define_expand "negvsi3"
  [(parallel [(set (reg:CCV CC_REG)
		   (compare:CCV (neg:SI (match_operand:SI 1 "register_operand" ""))
				(unspec:SI [(match_dup 1)] UNSPEC_NEGV)))
	      (set (match_operand:SI 0 "register_operand" "")
		   (neg:SI (match_dup 1)))])
   (set (pc)
        (if_then_else (ne (reg:CCV CC_REG) (const_int 0))
		      (label_ref (match_operand 2 ""))
		      (pc)))]
"")

(define_insn "*cmp_ccnz_neg"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (neg:SI (match_operand:SI 0 "register_operand" "r"))
		      (const_int 0)))]
  ""
  "subcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_neg"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (neg:DI (match_operand:DI 0 "register_operand" "r"))
		       (const_int 0)))]
  "TARGET_ARCH64"
  "subcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccnz_neg_set"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (neg:SI (match_operand:SI 1 "register_operand" "r"))
		      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  ""
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxnz_neg_set"
  [(set (reg:CCXNZ CC_REG)
	(compare:CCXNZ (neg:DI (match_operand:DI 1 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_neg_set"
  [(set (reg:CCC CC_REG)
	(compare:CCC (not:SI (match_operand:SI 1 "register_operand" "r"))
		     (const_int -1)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  ""
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxc_neg_set"
  [(set (reg:CCXC CC_REG)
	(compare:CCXC (not:DI (match_operand:DI 1 "register_operand" "r"))
		      (const_int -1)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccc_neg_sltu_set"
  [(set (reg:CCC CC_REG)
	(compare:CCC (zero_extend:DI
		       (neg:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
				        (ltu:SI (reg:CCC CC_REG)
						(const_int 0)))))
		     (neg:DI (plus:DI (zero_extend:DI (match_dup 1))
				      (ltu:DI (reg:CCC CC_REG)
					      (const_int 0))))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (plus:SI (match_dup 1)
			 (ltu:SI (reg:CCC CC_REG) (const_int 0)))))]
  ""
  "subxcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_neg"
  [(set (reg:CCV CC_REG)
	(compare:CCV (neg:SI (match_operand:SI 0 "register_operand" "r"))
		     (unspec:SI [(match_dup 0)] UNSPEC_NEGV)))]
  ""
  "subcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_neg"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (neg:DI (match_operand:DI 0 "register_operand" "r"))
		      (unspec:DI [(match_dup 0)] UNSPEC_NEGV)))]
  "TARGET_ARCH64"
  "subcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_neg_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV (neg:SI (match_operand:SI 1 "register_operand" "r"))
		     (unspec:SI [(match_dup 1)] UNSPEC_NEGV)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  ""
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccxv_neg_set"
  [(set (reg:CCXV CC_REG)
	(compare:CCXV (neg:DI (match_operand:DI 1 "register_operand" "r"))
		      (unspec:DI [(match_dup 1)] UNSPEC_NEGV)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "subcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccv_neg_sltu_set"
  [(set (reg:CCV CC_REG)
	(compare:CCV (neg:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
				      (ltu:SI (reg:CCC CC_REG) (const_int 0))))
		     (unspec:SI [(plus:SI (match_dup 1)
				          (ltu:SI (reg:CCC CC_REG)
						  (const_int 0)))]
				UNSPEC_NEGV)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (plus:SI (match_dup 1)
			 (ltu:SI (reg:CCC CC_REG) (const_int 0)))))]
  ""
  "subxcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])


(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "xnor\t%%g0, %1, %0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "xnor\t%%g0, %1, %0")

(define_insn "*cmp_cc_not"
  [(set (reg:CC CC_REG)
	(compare:CC (not:SI (match_operand:SI 0 "arith_operand" "rI"))
		    (const_int 0)))]
  ""
  "xnorcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_not"
  [(set (reg:CCX CC_REG)
	(compare:CCX (not:DI (match_operand:DI 0 "arith_operand" "rI"))
		     (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_set_not"
  [(set (reg:CC CC_REG)
	(compare:CC (not:SI (match_operand:SI 1 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_dup 1)))]
  ""
  "xnorcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_set_not"
  [(set (reg:CCX CC_REG)
	(compare:CCX (not:DI (match_operand:DI 1 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "xnorcc\t%%g0, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_set"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (reg:CC CC_REG)
	(compare:CC (match_dup 1) (const_int 0)))]
  ""
  "orcc\t%1, 0, %0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_set64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (reg:CCX CC_REG)
	(compare:CCX (match_dup 1) (const_int 0)))]
  "TARGET_ARCH64"
  "orcc\t%1, 0, %0"
   [(set_attr "type" "compare")])


;; Floating point arithmetic instructions.

(define_expand "addtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(plus:TF (match_operand:TF 1 "general_operand" "")
		 (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_binop (PLUS, operands); DONE;")

(define_insn "*addtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(plus:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "faddq\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(plus:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "faddd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fadds\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_expand "subtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(minus:TF (match_operand:TF 1 "general_operand" "")
		  (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_binop (MINUS, operands); DONE;")

(define_insn "*subtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(minus:TF (match_operand:TF 1 "register_operand" "e")
		  (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsubq\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(minus:DF (match_operand:DF 1 "register_operand" "e")
		  (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fsubd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fsubs\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_expand "multf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(mult:TF (match_operand:TF 1 "general_operand" "")
		 (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_binop (MULT, operands); DONE;")

(define_insn "*multf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fmulq\t%1, %2, %0"
  [(set_attr "type" "fpmul")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fmuld\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "fptype" "double")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fmuls\t%1, %2, %0"
  [(set_attr "type" "fpmul")])

(define_insn "fmadf4"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (fma:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")
		(match_operand:DF 3 "register_operand" "e")))]
  "TARGET_FMAF"
  "fmaddd\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "fmsdf4"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (fma:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")
		(neg:DF (match_operand:DF 3 "register_operand" "e"))))]
  "TARGET_FMAF"
  "fmsubd\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*nfmadf4"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (neg:DF (fma:DF (match_operand:DF 1 "register_operand" "e")
			(match_operand:DF 2 "register_operand" "e")
			(match_operand:DF 3 "register_operand" "e"))))]
  "TARGET_FMAF"
  "fnmaddd\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*nfmsdf4"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (neg:DF (fma:DF (match_operand:DF 1 "register_operand" "e")
			(match_operand:DF 2 "register_operand" "e")
			(neg:DF (match_operand:DF 3 "register_operand" "e")))))]
  "TARGET_FMAF"
  "fnmsubd\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "fmasf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (fma:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")
		(match_operand:SF 3 "register_operand" "f")))]
  "TARGET_FMAF"
  "fmadds\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "fmssf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (fma:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")
		(neg:SF (match_operand:SF 3 "register_operand" "f"))))]
  "TARGET_FMAF"
  "fmsubs\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*nfmasf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (neg:SF (fma:SF (match_operand:SF 1 "register_operand" "f")
			(match_operand:SF 2 "register_operand" "f")
			(match_operand:SF 3 "register_operand" "f"))))]
  "TARGET_FMAF"
  "fnmadds\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*nfmssf4"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (neg:SF (fma:SF (match_operand:SF 1 "register_operand" "f")
			(match_operand:SF 2 "register_operand" "f")
			(neg:SF (match_operand:SF 3 "register_operand" "f")))))]
  "TARGET_FMAF"
  "fnmsubs\t%1, %2, %3, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*muldf3_extend"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (float_extend:DF (match_operand:SF 1 "register_operand" "f"))
		 (float_extend:DF (match_operand:SF 2 "register_operand" "f"))))]
  "TARGET_FSMULD"
  "fsmuld\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "fptype" "double")])

(define_insn "*multf3_extend"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (float_extend:TF (match_operand:DF 1 "register_operand" "e"))
		 (float_extend:TF (match_operand:DF 2 "register_operand" "e"))))]
  "(TARGET_V8 || TARGET_V9) && TARGET_FPU && TARGET_HARD_QUAD"
  "fdmulq\t%1, %2, %0"
  [(set_attr "type" "fpmul")])

(define_expand "divtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(div:TF (match_operand:TF 1 "general_operand" "")
		(match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_binop (DIV, operands); DONE;")

;; don't have timing for quad-prec. divide.
(define_insn "*divtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(div:TF (match_operand:TF 1 "register_operand" "e")
		(match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdivq\t%1, %2, %0"
  [(set_attr "type" "fpdivs")])

(define_expand "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(div:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "")

(define_insn "*divdf3_nofix"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(div:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU && !sparc_fix_ut699"
  "fdivd\t%1, %2, %0"
  [(set_attr "type" "fpdivd")
   (set_attr "fptype" "double")])

(define_insn "*divdf3_fix"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(div:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU && sparc_fix_ut699"
  "fdivd\t%1, %2, %0\n\tstd\t%0, [%%sp-8]\n\tnop"
  [(set_attr "type" "fpdivd")
   (set_attr "fptype" "double")
   (set_attr "length" "3")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU && !sparc_fix_ut699"
  "fdivs\t%1, %2, %0"
  [(set_attr "type" "fpdivs")])

(define_expand "negtf2"
  [(set (match_operand:TF 0 "register_operand" "")
	(neg:TF (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*negtf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(neg:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fnegq\t%1, %0"
  [(set_attr "type" "fpmove")])

(define_insn_and_split "*negtf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(neg:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && !TARGET_HARD_QUAD"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);
  src1 = gen_df_reg (set_src, 0);
  src2 = gen_df_reg (set_src, 1);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movdf (dest2, src2));
      emit_insn (gen_negdf2 (dest1, src1));
    }
  else
    {
      emit_insn (gen_negdf2 (dest1, src1));
      if (REGNO (dest2) != REGNO (src2))
	emit_insn (gen_movdf (dest2, src2));
    }
  DONE;
}
  [(set_attr "length" "2")])

(define_expand "negdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(neg:DF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn_and_split "*negdf2_notv9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(neg:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && !TARGET_V9"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_highpart (SFmode, set_dest);
  dest2 = gen_lowpart (SFmode, set_dest);
  src1 = gen_highpart (SFmode, set_src);
  src2 = gen_lowpart (SFmode, set_src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movsf (dest2, src2));
      emit_insn (gen_negsf2 (dest1, src1));
    }
  else
    {
      emit_insn (gen_negsf2 (dest1, src1));
      if (REGNO (dest2) != REGNO (src2))
	emit_insn (gen_movsf (dest2, src2));
    }
  DONE;
}
  [(set_attr "length" "2")])

(define_insn "*negdf2_v9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(neg:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_V9"
  "fnegd\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "fptype" "double")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fnegs\t%1, %0"
  [(set_attr "type" "fpmove")])

(define_expand "abstf2"
  [(set (match_operand:TF 0 "register_operand" "")
	(abs:TF (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*abstf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(abs:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fabsq\t%1, %0"
  [(set_attr "type" "fpmove")])

(define_insn_and_split "*abstf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(abs:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && !TARGET_HARD_QUAD"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_df_reg (set_dest, 0);
  dest2 = gen_df_reg (set_dest, 1);
  src1 = gen_df_reg (set_src, 0);
  src2 = gen_df_reg (set_src, 1);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movdf (dest2, src2));
      emit_insn (gen_absdf2 (dest1, src1));
    }
  else
    {
      emit_insn (gen_absdf2 (dest1, src1));
      if (REGNO (dest2) != REGNO (src2))
	emit_insn (gen_movdf (dest2, src2));
    }
  DONE;
}
  [(set_attr "length" "2")])

(define_expand "absdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(abs:DF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn_and_split "*absdf2_notv9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(abs:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && !TARGET_V9"
  "#"
  "&& reload_completed"
  [(clobber (const_int 0))]
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  dest1 = gen_highpart (SFmode, set_dest);
  dest2 = gen_lowpart (SFmode, set_dest);
  src1 = gen_highpart (SFmode, set_src);
  src2 = gen_lowpart (SFmode, set_src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movsf (dest2, src2));
      emit_insn (gen_abssf2 (dest1, src1));
    }
  else
    {
      emit_insn (gen_abssf2 (dest1, src1));
      if (REGNO (dest2) != REGNO (src2))
	emit_insn (gen_movsf (dest2, src2));
    }
  DONE;
}
  [(set_attr "length" "2")])

(define_insn "*absdf2_v9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(abs:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_V9"
  "fabsd\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "fptype" "double")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fabss\t%1, %0"
  [(set_attr "type" "fpmove")])

(define_expand "sqrttf2"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(sqrt:TF (match_operand:TF 1 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "emit_tfmode_unop (SQRT, operands); DONE;")

(define_insn "*sqrttf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(sqrt:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsqrtq\t%1, %0"
  [(set_attr "type" "fpsqrts")])

(define_expand "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(sqrt:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "")

(define_insn "*sqrtdf2_nofix"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(sqrt:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && !sparc_fix_ut699"
  "fsqrtd\t%1, %0"
  [(set_attr "type" "fpsqrtd")
   (set_attr "fptype" "double")])

(define_insn "*sqrtdf2_fix"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(sqrt:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && sparc_fix_ut699"
  "fsqrtd\t%1, %0\n\tstd\t%0, [%%sp-8]\n\tnop"
  [(set_attr "type" "fpsqrtd")
   (set_attr "fptype" "double")
   (set_attr "length" "3")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU && !sparc_fix_ut699"
  "fsqrts\t%1, %0"
  [(set_attr "type" "fpsqrts")])


;; Arithmetic shift instructions.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sll\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
{
  if (TARGET_ARCH32)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	FAIL;
      emit_insn (gen_ashldi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*ashldi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);
  return "sllx\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn "ashldi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(ashift:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		   (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
{
  return output_v8plus_shift (insn ,operands, \"sllx\");
}
  [(set_attr "type" "multi")
   (set_attr "length" "5,5,6")])

(define_insn "*cmp_ccnz_ashift_1"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (ashift:SI (match_operand:SI 0 "register_operand" "r")
				 (const_int 1))
		      (const_int 0)))]
  ""
  "addcc\t%0, %0, %%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccnz_set_ashift_1"
  [(set (reg:CCNZ CC_REG)
	(compare:CCNZ (ashift:SI (match_operand:SI 1 "register_operand" "r")
				 (const_int 1))
		      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_dup 1) (const_int 1)))]
  ""
  "addcc\t%1, %1, %0"
  [(set_attr "type" "compare")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
   operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sra\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn "*ashrsi3_extend0"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (match_operand:SI 2 "arith_operand" "rI"))))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == CONST_INT)
   operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sra\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

;; This handles the case where
;; (sign_extend:DI (ashiftrt:SI (match_operand:SI) (match_operand:SI)))
;; but combiner "simplifies" it for us.
(define_insn "*ashrsi3_extend1"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (ashift:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
				(const_int 32))
		     (match_operand:SI 2 "small_int_operand" "I")))]
  "TARGET_ARCH64 && INTVAL (operands[2]) >= 32 && INTVAL (operands[2]) < 64"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 32);
  return "sra\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

;; This handles the case where
;; (ashiftrt:DI (sign_extend:DI (match_operand:SI)) (const_int))
;; but combiner "simplifies" it for us.
(define_insn "*ashrsi3_extend2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extract:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
			 (match_operand 2 "small_int_operand" "I")
			 (const_int 32)))]
  "TARGET_ARCH64 && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 32"
{
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));
  return "sra\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
{
  if (TARGET_ARCH32)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        FAIL;	/* prefer generic code in this case */
      emit_insn (gen_ashrdi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*ashrdi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);
  return "srax\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn "ashrdi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(ashiftrt:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		     (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
{
  return output_v8plus_shift (insn, operands, \"srax\");
}
  [(set_attr "type" "multi")
   (set_attr "length" "5,5,6")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "srl\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn "*lshrsi3_extend0"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		       (match_operand:SI 2 "arith_operand" "rI"))))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "srl\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

;; This handles the case where
;; (zero_extend:DI (lshiftrt:SI (match_operand:SI) (match_operand:SI)))
;; but combiner "simplifies" it for us.
(define_insn "*lshrsi3_extend1"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (subreg:DI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
					(match_operand:SI 2 "arith_operand" "rI")) 0)
		(match_operand 3 "const_int_operand" "")))]
  "TARGET_ARCH64 && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) == 0xffffffff"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "srl\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

;; This handles the case where
;; (lshiftrt:DI (zero_extend:DI (match_operand:SI)) (const_int))
;; but combiner "simplifies" it for us.
(define_insn "*lshrsi3_extend2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
			 (match_operand 2 "small_int_operand" "I")
			 (const_int 32)))]
  "TARGET_ARCH64 && INTVAL (operands[2]) > 0 && INTVAL (operands[2]) <= 32"
{
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));
  return "srl\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
{
  if (TARGET_ARCH32)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        FAIL;
      emit_insn (gen_lshrdi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "*lshrdi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);
  return "srlx\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn "lshrdi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(lshiftrt:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		     (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
{
  return output_v8plus_shift (insn, operands, \"srlx\");
}
  [(set_attr "type" "multi")
   (set_attr "length" "5,5,6")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (subreg:SI (lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (const_int 32)) 4)
		     (match_operand:SI 2 "small_int_operand" "I")))]
  "TARGET_ARCH64 && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) < 32"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 32);
  return "srax\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (subreg:SI (ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (const_int 32)) 4)
		     (match_operand:SI 2 "small_int_operand" "I")))]
  "TARGET_ARCH64 && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) < 32"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 32);
  return "srlx\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (subreg:SI (ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (match_operand:SI 2 "small_int_operand" "I")) 4)
		     (match_operand:SI 3 "small_int_operand" "I")))]
  "TARGET_ARCH64
   && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) >= 32
   && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) < 32
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[2]) + INTVAL (operands[3])) < 64"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]));

  return "srax\t%1, %2, %0";
}
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (subreg:SI (lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (match_operand:SI 2 "small_int_operand" "I")) 4)
		     (match_operand:SI 3 "small_int_operand" "I")))]
  "TARGET_ARCH64
   && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) >= 32
   && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) < 32
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[2]) + INTVAL (operands[3])) < 64"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]));

  return "srlx\t%1, %2, %0";
}
  [(set_attr "type" "shift")])


;; Unconditional and other jump instructions.

(define_expand "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  "")

(define_insn "*jump_ubranch"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  "!TARGET_CBCOND"
{
  return output_ubranch (operands[0], insn);
}
  [(set_attr "type" "uncond_branch")])

(define_insn "*jump_cbcond"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  "TARGET_CBCOND"
{
  return output_ubranch (operands[0], insn);
}
  [(set_attr "type" "uncond_cbcond")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" "r"))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
{
  gcc_assert (GET_MODE (operands[0]) == CASE_VECTOR_MODE);

  /* In pic mode, our address differences are against the base of the
     table.  Add that base value back in; CSE ought to be able to combine
     the two address loads.  */
  if (flag_pic)
    {
      rtx tmp, tmp2;
      tmp = gen_rtx_LABEL_REF (Pmode, operands[1]);
      tmp2 = operands[0];
      if (CASE_VECTOR_MODE != Pmode)
        tmp2 = gen_rtx_SIGN_EXTEND (Pmode, tmp2);
      tmp = gen_rtx_PLUS (Pmode, tmp2, tmp);
      operands[0] = memory_address (Pmode, tmp);
    }
})

(define_insn "*tablejump<P:mode>"
  [(set (pc) (match_operand:P 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp\t%a0%#"
  [(set_attr "type" "uncond_branch")])


;; Jump to subroutine instructions.

(define_expand "call"
  ;; Note that this expression is not used for generating RTL.
  ;; All the RTL is generated explicitly below.
  [(call (match_operand 0 "call_operand" "")
	 (match_operand 3 "" "i"))]
  ;; operands[2] is next_arg_register
  ;; operands[3] is struct_value_size_rtx.
  ""
{
  rtx fn_rtx;

  gcc_assert (MEM_P (operands[0]) && GET_MODE (operands[0]) == FUNCTION_MODE);

  gcc_assert (GET_CODE (operands[3]) == CONST_INT);

  if (GET_CODE (XEXP (operands[0], 0)) == LABEL_REF)
    {
      /* This is really a PIC sequence.  We want to represent
	 it as a funny jump so its delay slots can be filled. 

	 ??? But if this really *is* a CALL, will not it clobber the
	 call-clobbered registers?  We lose this if it is a JUMP_INSN.
	 Why cannot we have delay slots filled if it were a CALL?  */

      /* We accept negative sizes for untyped calls.  */
      if (TARGET_ARCH32 && INTVAL (operands[3]) != 0)
	emit_jump_insn
	  (gen_rtx_PARALLEL
	   (VOIDmode,
	    gen_rtvec (3,
		       gen_rtx_SET (pc_rtx, XEXP (operands[0], 0)),
		       operands[3],
		       gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));
      else
	emit_jump_insn
	  (gen_rtx_PARALLEL
	   (VOIDmode,
	    gen_rtvec (2,
		       gen_rtx_SET (pc_rtx, XEXP (operands[0], 0)),
		       gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));
      goto finish_call;
    }

  fn_rtx = operands[0];

  /* We accept negative sizes for untyped calls.  */
  if (TARGET_ARCH32 && INTVAL (operands[3]) != 0)
    sparc_emit_call_insn
      (gen_rtx_PARALLEL
       (VOIDmode,
	gen_rtvec (3, gen_rtx_CALL (VOIDmode, fn_rtx, const0_rtx),
		   operands[3],
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))),
       XEXP (fn_rtx, 0));
  else
    sparc_emit_call_insn
      (gen_rtx_PARALLEL
       (VOIDmode,
	gen_rtvec (2, gen_rtx_CALL (VOIDmode, fn_rtx, const0_rtx),
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))),
       XEXP (fn_rtx, 0));

 finish_call:

  DONE;
})

;; We can't use the same pattern for these two insns, because then registers
;; in the address may not be properly reloaded.

(define_insn "*call_address<P:mode>"
  [(call (mem:P (match_operand:P 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (clobber (reg:P O7_REG))]
  ;;- Do not use operand 1 for most machines.
  ""
  "call\t%a0, %1%#"
  [(set_attr "type" "call")])

(define_insn "*call_symbolic<P:mode>"
  [(call (mem:P (match_operand:P 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (clobber (reg:P O7_REG))]
  ;;- Do not use operand 1 for most machines.
  ""
  "call\t%a0, %1%#"
  [(set_attr "type" "call")])

;; This is a call that wants a structure value.
;; There is no such critter for v9 (??? we may need one anyway).
(define_insn "*call_address_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI O7_REG))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_ARCH32 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) > 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0xfff);
  return "call\t%a0, %1\n\t nop\n\tunimp\t%2";
}
  [(set_attr "type" "call_no_delay_slot")
   (set_attr "length" "3")])

;; This is a call that wants a structure value.
;; There is no such critter for v9 (??? we may need one anyway).
(define_insn "*call_symbolic_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI O7_REG))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_ARCH32 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) > 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0xfff);
  return "call\t%a0, %1\n\t nop\n\tunimp\t%2";
}
  [(set_attr "type" "call_no_delay_slot")
   (set_attr "length" "3")])

;; This is a call that may want a structure value.  This is used for
;; untyped_calls.
(define_insn "*call_address_untyped_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI O7_REG))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_ARCH32 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0"
  "call\t%a0, %1\n\t nop\n\tnop"
  [(set_attr "type" "call_no_delay_slot")
   (set_attr "length" "3")])

;; This is a call that may want a structure value.  This is used for
;; untyped_calls.
(define_insn "*call_symbolic_untyped_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI O7_REG))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_ARCH32 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0"
  "call\t%a0, %1\n\t nop\n\tnop"
  [(set_attr "type" "call_no_delay_slot")
   (set_attr "length" "3")])

(define_expand "call_value"
  ;; Note that this expression is not used for generating RTL.
  ;; All the RTL is generated explicitly below.
  [(set (match_operand 0 "register_operand" "")
	(call (match_operand 1 "call_operand" "")
	      (match_operand 4 "" "")))]
  ;; operand 2 is stack_size_rtx
  ;; operand 3 is next_arg_register
  ""
{
  rtx fn_rtx;
  rtvec vec;

  gcc_assert (MEM_P (operands[1]) && GET_MODE (operands[1]) == FUNCTION_MODE);

  fn_rtx = operands[1];

  vec = gen_rtvec (2,
		   gen_rtx_SET (operands[0],
				gen_rtx_CALL (VOIDmode, fn_rtx, const0_rtx)),
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)));

  sparc_emit_call_insn (gen_rtx_PARALLEL (VOIDmode, vec), XEXP (fn_rtx, 0));

  DONE;
})

(define_insn "*call_value_address<P:mode>"
  [(set (match_operand 0 "" "")
	(call (mem:P (match_operand:P 1 "address_operand" "p"))
	      (match_operand 2 "" "")))
   (clobber (reg:P O7_REG))]
  ;;- Do not use operand 2 for most machines.
  ""
  "call\t%a1, %2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_symbolic<P:mode>"
  [(set (match_operand 0 "" "")
	(call (mem:P (match_operand:P 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (clobber (reg:P O7_REG))]
  ;;- Do not use operand 2 for most machines.
  ""
  "call\t%a1, %2%#"
  [(set_attr "type" "call")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand:BLK 1 "memory_operand" "")
	      (match_operand 2 "" "")])]
  ""
{
  rtx valreg1 = gen_rtx_REG (DImode, 8);
  rtx result = operands[1];

  /* Pass constm1 to indicate that it may expect a structure value, but
     we don't know what size it is.  */
  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, constm1_rtx));

  /* Save the function value registers.  */
  emit_move_insn (adjust_address (result, DImode, 0), valreg1);
  if (TARGET_FPU)
    {
      rtx valreg2 = gen_rtx_REG (TARGET_ARCH64 ? TFmode : DFmode, 32);
      emit_move_insn (adjust_address (result, TARGET_ARCH64 ? TFmode : DFmode, 8),
                      valreg2);
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
})


;;  Tail call instructions.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "call_operand" "") (const_int 0))
	      (return)])]
  ""
  "")

(define_insn "*sibcall_symbolic<P:mode>"
  [(call (mem:P (match_operand:P 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (return)]
  ""
{
  return output_sibcall (insn, operands[0]);
}
  [(set_attr "type" "sibcall")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand")
		   (call (match_operand 1 "call_operand" "") (const_int 0)))
	      (return)])]
  ""
  "")

(define_insn "*sibcall_value_symbolic<P:mode>"
  [(set (match_operand 0 "" "")
	(call (mem:P (match_operand:P 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (return)]
  ""
{
  return output_sibcall (insn, operands[1]);
}
  [(set_attr "type" "sibcall")])


;; Special instructions.

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  if (TARGET_FLAT)
    sparc_flat_expand_prologue ();
  else
    sparc_expand_prologue ();
  DONE;
})

;; The "register window save" insn is modelled as follows.  The dwarf2
;; information is manually added in emit_window_save.

(define_insn "window_save"
  [(unspec_volatile [(match_operand 0 "arith_operand" "rI")] UNSPECV_SAVEW)]
  "!TARGET_FLAT"
  "save\t%%sp, %0, %%sp"
  [(set_attr "type" "savew")])

(define_expand "epilogue"
  [(return)]
  ""
{
  if (TARGET_FLAT)
    sparc_flat_expand_epilogue (false);
  else
    sparc_expand_epilogue (false);
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  if (TARGET_FLAT)
    sparc_flat_expand_epilogue (false);
  else
    sparc_expand_epilogue (false);
  DONE;
})

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand" ""))]
  ""
{
  emit_move_insn (gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM), operands[0]);
  emit_jump_insn (gen_eh_return_internal ());
  emit_barrier ();
  DONE;
})

(define_insn_and_split "eh_return_internal"
  [(eh_return)]
  ""
  "#"
  "epilogue_completed"
  [(return)]
{
  if (TARGET_FLAT)
    sparc_flat_expand_epilogue (true);
  else
    sparc_expand_epilogue (true);
})

(define_expand "return"
  [(return)]
  "sparc_can_use_return_insn_p ()"
{
  if (cfun->calls_alloca)
    emit_insn (gen_frame_blockage ());
})

(define_insn "*return_internal"
  [(return)]
  ""
{
  return output_return (insn);
}
  [(set_attr "type" "return")
   (set (attr "length")
	(cond [(eq_attr "calls_eh_return" "true")
	         (if_then_else (eq_attr "delayed_branch" "true")
				(if_then_else (ior (eq_attr "isa" "v9")
						   (eq_attr "flat" "true"))
					(const_int 2)
					(const_int 3))
				(if_then_else (eq_attr "flat" "true")
					(const_int 3)
					(const_int 4)))
	       (ior (eq_attr "leaf_function" "true") (eq_attr "flat" "true"))
		 (if_then_else (eq_attr "empty_delay_slot" "true")
			       (const_int 2)
			       (const_int 1))
	       (eq_attr "empty_delay_slot" "true")
		 (if_then_else (eq_attr "delayed_branch" "true")
			       (const_int 2)
			       (const_int 3))
	      ] (const_int 1)))])

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")])

;; Do not schedule instructions accessing memory before this point.

(define_expand "frame_blockage"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 1)] UNSPEC_FRAME_BLOCKAGE))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
  operands[1] = stack_pointer_rtx;
})

(define_insn "*frame_blockage<P:mode>"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_operand:P 1 "" "")] UNSPEC_FRAME_BLOCKAGE))]
  ""
  ""
  [(set_attr "length" "0")])

;; We use membar #Sync for the speculation barrier on V9.

(define_insn "speculation_barrier"
  [(unspec_volatile [(const_int 0)] UNSPECV_SPECULATION_BARRIER)]
  "TARGET_V9"
  "membar\t64"
  [(set_attr "type" "multi")])

(define_expand "probe_stack"
  [(set (match_operand 0 "memory_operand" "") (const_int 0))]
  ""
{
  operands[0]
    = adjust_address (operands[0], GET_MODE (operands[0]), SPARC_STACK_BIAS);
})

(define_insn "@probe_stack_range<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "0")
			    (match_operand:P 2 "register_operand" "r")]
			    UNSPECV_PROBE_STACK_RANGE))]
  ""
{
  return output_probe_stack_range (operands[0], operands[2]);
}
  [(set_attr "type" "multi")])

;; Prepare to return any type including a structure value.

(define_expand "untyped_return"
  [(match_operand:BLK 0 "memory_operand" "")
   (match_operand 1 "" "")]
  ""
{
  rtx valreg1 = gen_rtx_REG (DImode, 24);
  rtx result = operands[0];

  if (TARGET_ARCH32)
    {
      rtx rtnreg = gen_rtx_REG (SImode, RETURN_ADDR_REGNUM);
      rtx value = gen_reg_rtx (SImode);

      /* Fetch the instruction where we will return to and see if it's an unimp
	 instruction (the most significant 10 bits will be zero).  If so,
	 update the return address to skip the unimp instruction.  */
      emit_move_insn (value,
		      gen_rtx_MEM (SImode, plus_constant (SImode, rtnreg, 8)));
      emit_insn (gen_lshrsi3 (value, value, GEN_INT (22)));
      emit_insn (gen_update_return (rtnreg, value));
    }

  /* Reload the function value registers.
     Put USE insns before the return.  */
  emit_move_insn (valreg1, adjust_address (result, DImode, 0));
  emit_use (valreg1);

  if (TARGET_FPU)
    {
      rtx valreg2 = gen_rtx_REG (TARGET_ARCH64 ? TFmode : DFmode, 32);
      emit_move_insn (valreg2,
		      adjust_address (result, TARGET_ARCH64 ? TFmode : DFmode, 8));
      emit_use (valreg2);
    }

  /* Construct the return.  */
  expand_naked_return ();

  DONE;
})

;; Adjust the return address conditionally. If the value of op1 is equal
;; to all zero then adjust the return address i.e. op0 = op0 + 4.
;; This is technically *half* the check required by the 32-bit SPARC
;; psABI. This check only ensures that an "unimp" insn was written by
;; the caller, but doesn't check to see if the expected size matches
;; (this is encoded in the 12 lower bits). This check is obsolete and
;; only used by the above code "untyped_return".

(define_insn "update_return"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")] UNSPEC_UPDATE_RETURN)]
  "TARGET_ARCH32"
{
  if (flag_delayed_branch)
    return "cmp\t%1, 0\n\tbe,a\t.+8\n\t add\t%0, 4, %0";
  else
    return "cmp\t%1, 0\n\tbne\t.+12\n\t nop\n\tadd\t%0, 4, %0";
}
  [(set (attr "type") (const_string "multi"))
   (set (attr "length")
	(if_then_else (eq_attr "delayed_branch" "true")
		      (const_int 3)
		      (const_int 4)))])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "address_operand" "p"))]
  ""
  "")

(define_insn "*branch<P:mode>"
  [(set (pc) (match_operand:P 0 "address_operand" "p"))]
  ""
 "jmp\t%a0%#"
 [(set_attr "type" "uncond_branch")])
 
(define_expand "save_stack_nonlocal"
  [(set (match_operand 0 "memory_operand" "")
	(match_operand 1 "register_operand" ""))
   (set (match_dup 2) (match_dup 3))]
  ""
{
  operands[0] = adjust_address (operands[0], Pmode, 0);
  operands[2] = adjust_address (operands[0], Pmode, GET_MODE_SIZE (Pmode));
  operands[3] = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
})

(define_expand "restore_stack_nonlocal"
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "memory_operand" ""))]
  ""
{
  operands[1] = adjust_address (operands[1], Pmode, 0);
})

(define_expand "nonlocal_goto"
  [(match_operand 0 "general_operand" "")
   (match_operand 1 "general_operand" "")
   (match_operand 2 "memory_operand" "")
   (match_operand 3 "memory_operand" "")]
  ""
{
  rtx i7 = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
  rtx r_label = operands[1];
  rtx r_sp = adjust_address (operands[2], Pmode, 0);
  rtx r_fp = operands[3];
  rtx r_i7 = adjust_address (operands[2], Pmode, GET_MODE_SIZE (Pmode));

  /* We need to flush all the register windows so that their contents will
     be re-synchronized by the restore insn of the target function.  */
  if (!TARGET_FLAT)
    emit_insn (gen_flush_register_windows ());

  emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

  r_label = copy_to_reg (r_label);

  /* Restore the frame pointer and stack pointer.  We must use a
     temporary since the setjmp buffer may be a local.  */
  r_fp = copy_to_reg (r_fp);
  emit_stack_restore (SAVE_NONLOCAL, r_sp);
  r_i7 = copy_to_reg (r_i7);

  /* Ensure the frame pointer move is not optimized.  */
  emit_insn (gen_blockage ());
  emit_clobber (hard_frame_pointer_rtx);
  emit_move_insn (hard_frame_pointer_rtx, r_fp);
  emit_move_insn (i7, r_i7);

  /* USE of hard_frame_pointer_rtx added for consistency;
     not clear if really needed.  */
  emit_use (hard_frame_pointer_rtx);
  emit_use (stack_pointer_rtx);
  emit_use (i7);

  emit_indirect_jump (r_label);
  DONE;
})

(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "TARGET_VXWORKS_RTP && flag_pic"
{
  load_got_register ();
  DONE;
})

;; Special insn to flush register windows.

(define_insn "flush_register_windows"
  [(unspec_volatile [(const_int 0)] UNSPECV_FLUSHW)]
  ""
{
  return TARGET_V9 ? "flushw" : "ta\t3";
}
  [(set_attr "type" "flushw")])

;; Special pattern for the FLUSH instruction.

(define_insn "@flush<P:mode>"
  [(unspec_volatile [(match_operand:P 0 "memory_operand" "m")] UNSPECV_FLUSH)]
  ""
{
  return TARGET_V9 ? "flush\t%f0" : "iflush\t%f0";
}
  [(set_attr "type" "iflush")])

;; Special insns to load and store the 32-bit FP Status Register.

(define_insn "ldfsr"
  [(unspec_volatile [(match_operand:SI 0 "memory_operand" "m")] UNSPECV_LDFSR)]
  "TARGET_FPU"
  "ld\t%0, %%fsr"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "stfsr"
  [(set (match_operand:SI 0 "memory_operand" "=m")
        (unspec_volatile:SI [(const_int 0)] UNSPECV_STFSR))]
  "TARGET_FPU"
  "st\t%%fsr, %0"
  [(set_attr "type" "store")])


;; Find first set instructions.

(define_expand "popcountdi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (popcount:DI (match_operand:DI 1 "register_operand" "")))]
  "TARGET_POPC"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_popcountdi_v8plus (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "*popcountdi_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (popcount:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_POPC && TARGET_ARCH64"
  "popc\t%1, %0")

(define_insn "popcountdi_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (popcount:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&h"))]
  "TARGET_POPC && TARGET_ARCH32"
{
  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn ("srl\t%L1, 0, %L1", operands);
  return "sllx\t%H1, 32, %2\n\tor\t%L1, %2, %2\n\tpopc\t%2, %L0\n\tclr\t%H0";
}
  [(set_attr "type" "multi")
   (set_attr "length" "5")])

(define_expand "popcountsi2"
  [(set (match_dup 2)
        (zero_extend:DI (match_operand:SI 1 "register_operand" "")))
   (set (match_operand:SI 0 "register_operand" "")
        (truncate:SI (popcount:DI (match_dup 2))))]
  "TARGET_POPC"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_popcountsi_v8plus (operands[0], operands[1]));
      DONE;
    }
  else
    operands[2] = gen_reg_rtx (DImode);
})

(define_insn "*popcountsi_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI
          (popcount:DI (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_POPC && TARGET_ARCH64"
  "popc\t%1, %0")

(define_insn "popcountsi_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (popcount:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_POPC && TARGET_ARCH32"
{
  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn ("srl\t%1, 0, %1", operands);
  return "popc\t%1, %0";
}
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_expand "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (clz:DI (match_operand:DI 1 "register_operand" "")))]
  "TARGET_VIS3"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_clzdi_v8plus (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "*clzdi_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (clz:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_VIS3 && TARGET_ARCH64"
  "lzd\t%1, %0"
  [(set_attr "type" "lzd")])

(define_insn "clzdi_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (clz:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&h"))]
  "TARGET_VIS3 && TARGET_ARCH32"
{
  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn ("srl\t%L1, 0, %L1", operands);
  return "sllx\t%H1, 32, %2\n\tor\t%L1, %2, %2\n\tlzd\t%2, %L0\n\tclr\t%H0";
}
  [(set_attr "type" "multi")
   (set_attr "length" "5")])

(define_expand "clzsi2"
  [(set (match_dup 2)
        (zero_extend:DI (match_operand:SI 1 "register_operand" "")))
   (set (match_dup 3)
        (truncate:SI (clz:DI (match_dup 2))))
   (set (match_operand:SI 0 "register_operand" "")
        (minus:SI (match_dup 3) (const_int 32)))]
  "TARGET_VIS3"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_clzsi_v8plus (operands[0], operands[1]));
      DONE;
    }
  else
    {
      operands[2] = gen_reg_rtx (DImode);
      operands[3] = gen_reg_rtx (SImode);
    }
})

(define_insn "*clzsi_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (truncate:SI
          (clz:DI (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_VIS3 && TARGET_ARCH64"
  "lzd\t%1, %0"
  [(set_attr "type" "lzd")])

(define_insn "clzsi_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clz:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_VIS3 && TARGET_ARCH32"
{
  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn ("srl\t%1, 0, %1", operands);
  return "lzd\t%1, %0\n\tsub\t%0, 32, %0";
}
  [(set_attr "type" "multi")
   (set_attr "length" "3")])


;; Peepholes go at the end.

;; Optimize consecutive loads or stores into ldd and std when possible.
;; The conditions in which we do this are very restricted and are 
;; explained in the code for {registers,memory}_ok_for_ldd functions.

(define_peephole2
  [(set (match_operand:SI 0 "memory_operand" "")
      (const_int 0))
   (set (match_operand:SI 1 "memory_operand" "")
      (const_int 0))]
  "TARGET_V9
   && mems_ok_for_ldd_peep (operands[0], operands[1], NULL_RTX)"
  [(set (match_dup 0) (const_int 0))]
{
  operands[0] = widen_mem_for_ldd_peep (operands[0], operands[1], DImode);
})

(define_peephole2
  [(set (match_operand:SI 0 "memory_operand" "")
      (const_int 0))
   (set (match_operand:SI 1 "memory_operand" "")
      (const_int 0))]
  "TARGET_V9
   && mems_ok_for_ldd_peep (operands[1], operands[0], NULL_RTX)"
  [(set (match_dup 1) (const_int 0))]
{
  operands[1] = widen_mem_for_ldd_peep (operands[1], operands[0], DImode);
})

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && mems_ok_for_ldd_peep (operands[1], operands[3], operands[0])" 
  [(set (match_dup 0) (match_dup 1))]
{
  operands[1] = widen_mem_for_ldd_peep (operands[1], operands[3], DImode);
  operands[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
})

(define_peephole2
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
   && mems_ok_for_ldd_peep (operands[0], operands[2], NULL_RTX)"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[0] = widen_mem_for_ldd_peep (operands[0], operands[2], DImode);
  operands[1] = gen_rtx_REG (DImode, REGNO (operands[1]));
})

(define_peephole2
  [(set (match_operand:SF 0 "register_operand" "")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && mems_ok_for_ldd_peep (operands[1], operands[3], operands[0])"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[1] = widen_mem_for_ldd_peep (operands[1], operands[3], DFmode);
  operands[0] = gen_rtx_REG (DFmode, REGNO (operands[0]));
})

(define_peephole2
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" ""))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" ""))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
  && mems_ok_for_ldd_peep (operands[0], operands[2], NULL_RTX)"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[0] = widen_mem_for_ldd_peep (operands[0], operands[2], DFmode);
  operands[1] = gen_rtx_REG (DFmode, REGNO (operands[1]));
})

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
  && mems_ok_for_ldd_peep (operands[3], operands[1], operands[0])"
  [(set (match_dup 2) (match_dup 3))]
{
  operands[3] = widen_mem_for_ldd_peep (operands[3], operands[1], DImode);
  operands[2] = gen_rtx_REG (DImode, REGNO (operands[2]));
})

(define_peephole2
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" ""))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
  && mems_ok_for_ldd_peep (operands[2], operands[0], NULL_RTX)" 
  [(set (match_dup 2) (match_dup 3))]
{
  operands[2] = widen_mem_for_ldd_peep (operands[2],  operands[0], DImode);
  operands[3] = gen_rtx_REG (DImode, REGNO (operands[3]));
})
 
(define_peephole2
  [(set (match_operand:SF 0 "register_operand" "")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
  && mems_ok_for_ldd_peep (operands[3], operands[1], operands[0])"
  [(set (match_dup 2) (match_dup 3))]
{
  operands[3] = widen_mem_for_ldd_peep (operands[3], operands[1], DFmode);
  operands[2] = gen_rtx_REG (DFmode, REGNO (operands[2]));
})

(define_peephole2
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" ""))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" ""))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
  && mems_ok_for_ldd_peep (operands[2], operands[0], NULL_RTX)"
  [(set (match_dup 2) (match_dup 3))]
{
  operands[2] = widen_mem_for_ldd_peep (operands[2], operands[0], DFmode);
  operands[3] = gen_rtx_REG (DFmode, REGNO (operands[3]));
})
 
;; Optimize the case of following a reg-reg move with a test
;; of reg just moved.  Don't allow floating point regs for operand 0 or 1.
;; This can result from a float to fix conversion.

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 2 "register_operand" "")
		    (const_int 0)))]
  "(rtx_equal_p (operands[2], operands[0])
    || rtx_equal_p (operands[2], operands[1]))
    && !SPARC_FP_REG_P (REGNO (operands[0]))
    && !SPARC_FP_REG_P (REGNO (operands[1]))"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (reg:CC CC_REG)
		   (compare:CC (match_dup 1) (const_int 0)))])]
  "")

(define_peephole2
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "register_operand" ""))
   (set (reg:CCX CC_REG)
	(compare:CCX (match_operand:DI 2 "register_operand" "")
		    (const_int 0)))]
  "TARGET_ARCH64
   && (rtx_equal_p (operands[2], operands[0])
       || rtx_equal_p (operands[2], operands[1]))
   && !SPARC_FP_REG_P (REGNO (operands[0]))
   && !SPARC_FP_REG_P (REGNO (operands[1]))"
  [(parallel [(set (match_dup 0) (match_dup 1))
	      (set (reg:CCX CC_REG)
		   (compare:CCX (match_dup 1) (const_int 0)))])]
  "")


;; Prefetch instructions.

;; ??? UltraSPARC-III note: A memory operation loading into the floating point
;; register file, if it hits the prefetch cache, has a chance to dual-issue
;; with other memory operations.  With DFA we might be able to model this,
;; but it requires a lot of state.
(define_expand "prefetch"
  [(match_operand 0 "address_operand" "")
   (match_operand 1 "const_int_operand" "")
   (match_operand 2 "const_int_operand" "")]
  "TARGET_V9"
{
  if (TARGET_ARCH64)
    emit_insn (gen_prefetch_64 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_prefetch_32 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "prefetch_64"
  [(prefetch (match_operand:DI 0 "address_operand" "p")
	     (match_operand:DI 1 "const_int_operand" "n")
	     (match_operand:DI 2 "const_int_operand" "n"))]
  ""
{
  static const char * const prefetch_instr[2][2] = {
    {
      "prefetch\t[%a0], 1", /* no locality: prefetch for one read */
      "prefetch\t[%a0], 0", /* medium to high locality: prefetch for several reads */
    },
    {
      "prefetch\t[%a0], 3", /* no locality: prefetch for one write */
      "prefetch\t[%a0], 2", /* medium to high locality: prefetch for several writes */
    }
  };
  int read_or_write = INTVAL (operands[1]);
  int locality = INTVAL (operands[2]);

  gcc_assert (IN_RANGE (read_or_write, 0, 2));
  gcc_assert (IN_RANGE (locality, 0, 3));
  return prefetch_instr [read_or_write & 1][locality == 0 ? 0 : 1];
}
  [(set_attr "type" "load")
   (set_attr "subtype" "prefetch")])

(define_insn "prefetch_32"
  [(prefetch (match_operand:SI 0 "address_operand" "p")
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  ""
{
  static const char * const prefetch_instr[2][2] = {
    {
      "prefetch\t[%a0], 1", /* no locality: prefetch for one read */
      "prefetch\t[%a0], 0", /* medium to high locality: prefetch for several reads */
    },
    {
      "prefetch\t[%a0], 3", /* no locality: prefetch for one write */
      "prefetch\t[%a0], 2", /* medium to high locality: prefetch for several writes */
    }
  };
  int read_or_write = INTVAL (operands[1]);
  int locality = INTVAL (operands[2]);

  gcc_assert (IN_RANGE (read_or_write, 0, 2));
  gcc_assert (IN_RANGE (locality, 0, 3));
  return prefetch_instr [read_or_write & 1][locality == 0 ? 0 : 1];
}
  [(set_attr "type" "load")
   (set_attr "subtype" "prefetch")])


;; Trap instructions.

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 5))]
  ""
  "ta\t5"
  [(set_attr "type" "trap")])

(define_expand "ctrapsi4"
  [(trap_if (match_operator 0 "comparison_operator"
	     [(match_operand:SI 1 "compare_operand" "")
	      (match_operand:SI 2 "arith_operand" "")])
	    (match_operand 3 "arith_operand"))]
  ""
{
  operands[1] = gen_compare_reg (operands[0]);
  if (GET_MODE (operands[1]) != CCmode && GET_MODE (operands[1]) != CCXmode)
    FAIL;
  operands[2] = const0_rtx;
})

(define_expand "ctrapdi4"
  [(trap_if (match_operator 0 "comparison_operator"
	     [(match_operand:DI 1 "compare_operand" "")
	      (match_operand:DI 2 "arith_operand" "")])
	    (match_operand 3 "arith_operand"))]
  "TARGET_ARCH64"
{
  operands[1] = gen_compare_reg (operands[0]);
  if (GET_MODE (operands[1]) != CCmode && GET_MODE (operands[1]) != CCXmode)
    FAIL;
  operands[2] = const0_rtx;
})

(define_insn "*trapsi_insn"
  [(trap_if (match_operator 0 "icc_comparison_operator"
	     [(reg:CC CC_REG) (const_int 0)])
	    (match_operand:SI 1 "arith_operand" "rM"))]
  ""
{
  if (TARGET_V9)
    return "t%C0\t%%icc, %1";
  else
    return "t%C0\t%1";
}
  [(set_attr "type" "trap")])

(define_insn "*trapdi_insn"
  [(trap_if (match_operator 0 "icc_comparison_operator"
	     [(reg:CCX CC_REG) (const_int 0)])
	    (match_operand:SI 1 "arith_operand" "rM"))]
  "TARGET_V9"
  "t%C0\t%%xcc, %1"
  [(set_attr "type" "trap")])


;; TLS support instructions.

(define_insn "@tgd_hi22<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
        (high:P (unspec:P [(match_operand 1 "tgd_symbolic_operand" "")]
			  UNSPEC_TLSGD)))]
  "TARGET_TLS"
  "sethi\\t%%tgd_hi22(%a1), %0")

(define_insn "@tgd_lo10<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (unspec:P [(match_operand 2 "tgd_symbolic_operand" "")]
			    UNSPEC_TLSGD)))]
  "TARGET_TLS"
  "add\\t%1, %%tgd_lo10(%a2), %0")

(define_insn "@tgd_add<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(plus:P (match_operand:P 1 "register_operand" "r")
		(unspec:P [(match_operand:P 2 "register_operand" "r")
			   (match_operand 3 "tgd_symbolic_operand" "")]
			  UNSPEC_TLSGD)))]
  "TARGET_TLS"
  "add\\t%1, %2, %0, %%tgd_add(%a3)")

(define_insn "@tgd_call<P:mode>"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:P (unspec:P [(match_operand:P 1 "symbolic_operand" "s")
				(match_operand 2 "tgd_symbolic_operand" "")]
			       UNSPEC_TLSGD))
	      (match_operand 3 "" "")))
   (clobber (reg:P O7_REG))]
  "TARGET_TLS"
  "call\t%a1, %%tgd_call(%a2)%#"
  [(set (attr "type") (if_then_else (eq_attr "tls_delay_slot" "true")
                                    (const_string "call")
                                    (const_string "call_no_delay_slot")))])

(define_insn "@tldm_hi22<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
        (high:P (unspec:P [(const_int 0)] UNSPEC_TLSLDM)))]
  "TARGET_TLS"
  "sethi\\t%%tldm_hi22(%&), %0")

(define_insn "@tldm_lo10<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (unspec:P [(const_int 0)] UNSPEC_TLSLDM)))]
  "TARGET_TLS"
  "add\\t%1, %%tldm_lo10(%&), %0")

(define_insn "@tldm_add<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(plus:P (match_operand:P 1 "register_operand" "r")
		(unspec:P [(match_operand:P 2 "register_operand" "r")]
			  UNSPEC_TLSLDM)))]
  "TARGET_TLS"
  "add\\t%1, %2, %0, %%tldm_add(%&)")

(define_insn "@tldm_call<P:mode>"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:P (unspec:P [(match_operand:P 1 "symbolic_operand" "s")]
			       UNSPEC_TLSLDM))
	      (match_operand 2 "" "")))
   (clobber (reg:P O7_REG))]
  "TARGET_TLS"
  "call\t%a1, %%tldm_call(%&)%#"
  [(set (attr "type") (if_then_else (eq_attr "tls_delay_slot" "true")
                                    (const_string "call")
                                    (const_string "call_no_delay_slot")))])

(define_insn "@tldo_hix22<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
        (high:P (unspec:P [(match_operand 1 "tld_symbolic_operand" "")]
			  UNSPEC_TLSLDO)))]
  "TARGET_TLS"
  "sethi\\t%%tldo_hix22(%a1), %0")

(define_insn "@tldo_lox10<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (unspec:P [(match_operand 2 "tld_symbolic_operand" "")]
			    UNSPEC_TLSLDO)))]
  "TARGET_TLS"
  "xor\\t%1, %%tldo_lox10(%a2), %0")

(define_insn "@tldo_add<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(plus:P (match_operand:P 1 "register_operand" "r")
		(unspec:P [(match_operand:P 2 "register_operand" "r")
			   (match_operand 3 "tld_symbolic_operand" "")]
			  UNSPEC_TLSLDO)))]
  "TARGET_TLS"
  "add\\t%1, %2, %0, %%tldo_add(%a3)")

(define_insn "@tie_hi22<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
        (high:P (unspec:P [(match_operand 1 "tie_symbolic_operand" "")]
			  UNSPEC_TLSIE)))]
  "TARGET_TLS"
  "sethi\\t%%tie_hi22(%a1), %0")

(define_insn "@tie_lo10<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (unspec:P [(match_operand 2 "tie_symbolic_operand" "")]
			    UNSPEC_TLSIE)))]
  "TARGET_TLS"
  "add\\t%1, %%tie_lo10(%a2), %0")

; Note the %%tie_ld operator
(define_insn "tie_ld32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")
		    (match_operand 3 "tie_symbolic_operand" "")]
		   UNSPEC_TLSIE))]
  "TARGET_TLS && TARGET_ARCH32"
  "ld\\t[%1 + %2], %0, %%tie_ld(%a3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

; Note the %%tie_ldx operator
(define_insn "tie_ld64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")
		    (match_operand:DI 2 "register_operand" "r")
		    (match_operand 3 "tie_symbolic_operand" "")]
		   UNSPEC_TLSIE))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldx\\t[%1 + %2], %0, %%tie_ldx(%a3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "@tie_add<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(plus:P (match_operand:P 1 "register_operand" "r")
		(unspec:P [(match_operand:P 2 "register_operand" "r")
			   (match_operand 3 "tie_symbolic_operand" "")]
			  UNSPEC_TLSIE)))]
  "TARGET_SUN_TLS"
  "add\\t%1, %2, %0, %%tie_add(%a3)")

(define_insn "@tle_hix22<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
        (high:P (unspec:P [(match_operand 1 "tle_symbolic_operand" "")]
			  UNSPEC_TLSLE)))]
  "TARGET_TLS"
  "sethi\\t%%tle_hix22(%a1), %0")

(define_insn "@tle_lox10<P:mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (unspec:P [(match_operand 2 "tle_symbolic_operand" "")]
			    UNSPEC_TLSLE)))]
  "TARGET_TLS"
  "xor\\t%1, %%tle_lox10(%a2), %0")

;; Now patterns combining tldo_add with some integer loads or stores
(define_insn "*tldo_ldub<P:mode>"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				  UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r"))))]
  "TARGET_TLS"
  "ldub\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldub1<P:mode>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI
	  (mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "ldub\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldub2<P:mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	  (mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "ldub\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldsb1<P:mode>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "ldsb\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldsb2<P:mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	  (mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "ldsb\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldub3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (mem:QI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldub\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldsb3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (mem:QI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldsb\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_lduh<P:mode>"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				  UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r"))))]
  "TARGET_TLS"
  "lduh\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_lduh1<P:mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	  (mem:HI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "lduh\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldsh1<P:mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	  (mem:HI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			  (match_operand:P 1 "register_operand" "r")))))]
  "TARGET_TLS"
  "ldsh\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_lduh2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (mem:HI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "lduh\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldsh2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (mem:HI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldsh\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_lduw<P:mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				  UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r"))))]
  "TARGET_TLS"
  "ld\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "*tldo_lduw1_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (mem:SI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "lduw\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "*tldo_ldsw1_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	  (mem:SI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				       (match_operand 3 "tld_symbolic_operand" "")]
				      UNSPEC_TLSLDO)
			   (match_operand:DI 1 "register_operand" "r")))))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldsw\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "sload")
   (set_attr "us3load_type" "3cycle")])

(define_insn "*tldo_ldx_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mem:DI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			 (match_operand:DI 1 "register_operand" "r"))))]
  "TARGET_TLS && TARGET_ARCH64"
  "ldx\t[%1 + %2], %0, %%tldo_add(%3)"
  [(set_attr "type" "load")
   (set_attr "subtype" "regular")])

(define_insn "*tldo_stb<P:mode>"
  [(set (mem:QI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				  UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r")))
	(match_operand:QI 0 "register_operand" "r"))]
  "TARGET_TLS"
  "stb\t%0, [%1 + %2], %%tldo_add(%3)"
  [(set_attr "type" "store")])

(define_insn "*tldo_sth<P:mode>"
  [(set (mem:HI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				   UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r")))
	(match_operand:HI 0 "register_operand" "r"))]
  "TARGET_TLS"
  "sth\t%0, [%1 + %2], %%tldo_add(%3)"
  [(set_attr "type" "store")])

(define_insn "*tldo_stw<P:mode>"
  [(set (mem:SI (plus:P (unspec:P [(match_operand:P 2 "register_operand" "r")
				   (match_operand 3 "tld_symbolic_operand" "")]
				  UNSPEC_TLSLDO)
			(match_operand:P 1 "register_operand" "r")))
	(match_operand:SI 0 "register_operand" "r"))]
  "TARGET_TLS"
  "st\t%0, [%1 + %2], %%tldo_add(%3)"
  [(set_attr "type" "store")])

(define_insn "*tldo_stx_sp64"
  [(set (mem:DI (plus:DI (unspec:DI [(match_operand:DI 2 "register_operand" "r")
				     (match_operand 3 "tld_symbolic_operand" "")]
				    UNSPEC_TLSLDO)
			 (match_operand:DI 1 "register_operand" "r")))
	(match_operand:DI 0 "register_operand" "r"))]
  "TARGET_TLS && TARGET_ARCH64"
  "stx\t%0, [%1 + %2], %%tldo_add(%3)"
  [(set_attr "type" "store")])


;; Stack protector instructions.

(define_expand "stack_protect_set"
  [(match_operand 0 "memory_operand" "")
   (match_operand 1 "memory_operand" "")]
  ""
{
#ifdef TARGET_THREAD_SSP_OFFSET
  rtx tlsreg = gen_rtx_REG (Pmode, 7);
  rtx addr = gen_rtx_PLUS (Pmode, tlsreg, GEN_INT (TARGET_THREAD_SSP_OFFSET));
  operands[1] = gen_rtx_MEM (Pmode, addr);
#endif
  if (TARGET_ARCH64)
    emit_insn (gen_stack_protect_set64 (operands[0], operands[1]));
  else
    emit_insn (gen_stack_protect_set32 (operands[0], operands[1]));
  DONE;
})

(define_insn "stack_protect_set32"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(unspec:SI [(match_operand:SI 1 "memory_operand" "m")] UNSPEC_SP_SET))
   (set (match_scratch:SI 2 "=&r") (const_int 0))]
  "TARGET_ARCH32"
{
  if (sparc_fix_b2bst)
    return "ld\t%1, %2\;st\t%2, %0\;mov\t0, %2\;nop";
  else
    return "ld\t%1, %2\;st\t%2, %0\;mov\t0, %2";
}
  [(set_attr "type" "multi")
   (set (attr "length") (if_then_else (eq_attr "fix_b2bst" "true")
		      (const_int 4) (const_int 3)))])

(define_insn "stack_protect_set64"
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m")] UNSPEC_SP_SET))
   (set (match_scratch:DI 2 "=&r") (const_int 0))]
  "TARGET_ARCH64"
  "ldx\t%1, %2\;stx\t%2, %0\;mov\t0, %2"
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

(define_expand "stack_protect_test"
  [(match_operand 0 "memory_operand" "")
   (match_operand 1 "memory_operand" "")
   (match_operand 2 "" "")]
  ""
{
  rtx result, test;
#ifdef TARGET_THREAD_SSP_OFFSET
  rtx tlsreg = gen_rtx_REG (Pmode, 7);
  rtx addr = gen_rtx_PLUS (Pmode, tlsreg, GEN_INT (TARGET_THREAD_SSP_OFFSET));
  operands[1] = gen_rtx_MEM (Pmode, addr);
#endif
  if (TARGET_ARCH64)
    {
      result = gen_reg_rtx (Pmode);
      emit_insn (gen_stack_protect_test64 (result, operands[0], operands[1]));
      test = gen_rtx_EQ (VOIDmode, result, const0_rtx);
      emit_jump_insn (gen_cbranchdi4 (test, result, const0_rtx, operands[2]));
    }
  else
    {
      emit_insn (gen_stack_protect_test32 (operands[0], operands[1]));
      result = gen_rtx_REG (CCmode, SPARC_ICC_REG);
      test = gen_rtx_EQ (VOIDmode, result, const0_rtx);
      emit_jump_insn (gen_cbranchcc4 (test, result, const0_rtx, operands[2]));
    }
  DONE;
})

(define_insn "stack_protect_test32"
  [(set (reg:CC CC_REG)
	(unspec:CC [(match_operand:SI 0 "memory_operand" "m")
		    (match_operand:SI 1 "memory_operand" "m")]
		   UNSPEC_SP_TEST))
   (set (match_scratch:SI 3 "=r") (const_int 0))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_ARCH32"
  "ld\t%0, %2\;ld\t%1, %3\;xorcc\t%2, %3, %2\;mov\t0, %3"
  [(set_attr "type" "multi")
   (set_attr "length" "4")])

(define_insn "stack_protect_test64"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m")
		    (match_operand:DI 2 "memory_operand" "m")]
		   UNSPEC_SP_TEST))
   (set (match_scratch:DI 3 "=r") (const_int 0))]
  "TARGET_ARCH64"
  "ldx\t%1, %0\;ldx\t%2, %3\;xor\t%0, %3, %0\;mov\t0, %3"
  [(set_attr "type" "multi")
   (set_attr "length" "4")])


;; Vector instructions.

(define_mode_iterator VM32 [V1SI V2HI V4QI])
(define_mode_iterator VM64 [V1DI V2SI V4HI V8QI])
(define_mode_iterator VMALL [V1SI V2HI V4QI V1DI V2SI V4HI V8QI])

(define_mode_attr vbits [(V2SI "32") (V4HI "16") (V1SI "32s") (V2HI "16s")
			 (V8QI "8")])
(define_mode_attr vconstr [(V1SI "f") (V2HI "f") (V4QI "f")
			   (V1DI "e") (V2SI "e") (V4HI "e") (V8QI "e")])
(define_mode_attr vfptype [(V1SI "single") (V2HI "single") (V4QI "single")
			   (V1DI "double") (V2SI "double") (V4HI "double")
			   (V8QI "double")])
(define_mode_attr veltmode [(V1SI "si") (V2HI "hi") (V4QI "qi") (V1DI "di")
			    (V2SI "si") (V4HI "hi") (V8QI "qi")])

(define_expand "mov<VMALL:mode>"
  [(set (match_operand:VMALL 0 "nonimmediate_operand" "")
	(match_operand:VMALL 1 "general_operand" ""))]
  "TARGET_VIS"
{
  if (sparc_expand_move (<VMALL:MODE>mode, operands))
    DONE;
})

(define_insn "*mov<VM32:mode>_insn"
  [(set (match_operand:VM32 0 "nonimmediate_operand" "=f,f,f,f,m,m,*r, m,*r,*r, f")
	(match_operand:VM32 1 "input_operand"         "Y,Z,f,m,f,Y, m,*r,*r, f,*r"))]
  "TARGET_VIS
   && (register_operand (operands[0], <VM32:MODE>mode)
       || register_or_zero_or_all_ones_operand (operands[1], <VM32:MODE>mode))"
  "@
  fzeros\t%0
  fones\t%0
  fsrc2s\t%1, %0
  ld\t%1, %0
  st\t%1, %0
  st\t%r1, %0
  ld\t%1, %0
  st\t%1, %0
  mov\t%1, %0
  movstouw\t%1, %0
  movwtos\t%1, %0"
  [(set_attr "type" "visl,visl,vismv,fpload,fpstore,store,load,store,*,vismv,vismv")
   (set_attr "subtype" "single,single,single,*,*,*,regular,*,*,movstouw,single")
   (set_attr "cpu_feature" "vis,vis,vis,*,*,*,*,*,*,vis3,vis3")])

(define_insn "*mov<VM64:mode>_insn_sp64"
  [(set (match_operand:VM64 0 "nonimmediate_operand" "=e,e,e,e,m,m,*r, m,*r, e,*r")
	(match_operand:VM64 1 "input_operand"         "Y,Z,e,m,e,Y, m,*r, e,*r,*r"))]
  "TARGET_VIS
   && TARGET_ARCH64
   && (register_operand (operands[0], <VM64:MODE>mode)
       || register_or_zero_or_all_ones_operand (operands[1], <VM64:MODE>mode))"
  "@
  fzero\t%0
  fone\t%0
  fsrc2\t%1, %0
  ldd\t%1, %0
  std\t%1, %0
  stx\t%r1, %0
  ldx\t%1, %0
  stx\t%1, %0
  movdtox\t%1, %0
  movxtod\t%1, %0
  mov\t%1, %0"
  [(set_attr "type" "visl,visl,vismv,fpload,fpstore,store,load,store,vismv,vismv,*")
   (set_attr "subtype" "double,double,double,*,*,*,regular,*,movdtox,movxtod,*")
   (set_attr "cpu_feature" "vis,vis,vis,*,*,*,*,*,vis3,vis3,*")])

(define_insn "*mov<VM64:mode>_insn_sp32"
  [(set (match_operand:VM64 0 "nonimmediate_operand"
			      "=T,o,e,e,e,*r, f,e,T,f,o,*r,*r, o")
	(match_operand:VM64 1 "input_operand"
			      " Y,Y,Y,Z,e, f,*r,T,e,o,f,*r, o,*r"))]
  "TARGET_VIS
   && TARGET_ARCH32
   && (register_operand (operands[0], <VM64:MODE>mode)
       || register_or_zero_or_all_ones_operand (operands[1], <VM64:MODE>mode))"
  "@
  stx\t%r1, %0
  #
  fzero\t%0
  fone\t%0
  fsrc2\t%1, %0
  #
  #
  ldd\t%1, %0
  std\t%1, %0
  #
  #
  #
  ldd\t%1, %0
  std\t%1, %0"
  [(set_attr "type" "store,*,visl,visl,vismv,*,*,fpload,fpstore,*,*,*,load,store")
   (set_attr "subtype" "*,*,double,double,double,*,*,*,*,*,*,*,regular,*")
   (set_attr "length" "*,2,*,*,*,2,2,*,*,2,2,2,*,*")
   (set_attr "cpu_feature" "*,*,vis,vis,vis,vis3,vis3,*,*,*,*,*,*,*")])

(define_split
  [(set (match_operand:VM64 0 "register_operand" "")
        (match_operand:VM64 1 "register_operand" ""))]
  "reload_completed
   && TARGET_VIS
   && TARGET_ARCH32
   && sparc_split_reg_reg_legitimate (operands[0], operands[1])"
  [(clobber (const_int 0))]
{
  sparc_split_reg_reg (operands[0], operands[1], SImode);
  DONE;
})

(define_split
  [(set (match_operand:VM64 0 "register_operand" "")
        (match_operand:VM64 1 "memory_operand" ""))]
  "reload_completed
   && TARGET_VIS
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[0], operands[1])"
  [(clobber (const_int 0))]
{
  sparc_split_reg_mem (operands[0], operands[1], SImode);
  DONE;
})

(define_split
  [(set (match_operand:VM64 0 "memory_operand" "")
        (match_operand:VM64 1 "register_operand" ""))]
  "reload_completed
   && TARGET_VIS
   && TARGET_ARCH32
   && sparc_split_reg_mem_legitimate (operands[1], operands[0])"
  [(clobber (const_int 0))]
{
  sparc_split_mem_reg (operands[0], operands[1], SImode);
  DONE;
})

(define_split
  [(set (match_operand:VM64 0 "memory_operand" "")
        (match_operand:VM64 1 "const_zero_operand" ""))]
  "reload_completed
   && TARGET_VIS
   && TARGET_ARCH32
   && !mem_min_alignment (operands[0], 8)
   && offsettable_memref_p (operands[0])"
  [(clobber (const_int 0))]
{
  emit_move_insn_1 (adjust_address (operands[0], SImode, 0), const0_rtx);
  emit_move_insn_1 (adjust_address (operands[0], SImode, 4), const0_rtx);
  DONE;
})

(define_expand "vec_init<VMALL:mode><VMALL:veltmode>"
  [(match_operand:VMALL 0 "register_operand" "")
   (match_operand:VMALL 1 "" "")]
  "TARGET_VIS"
{
  sparc_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_code_iterator plusminus [plus minus])
(define_code_attr plusminus_insn [(plus "add") (minus "sub")])

(define_mode_iterator VADDSUB [V1SI V2SI V2HI V4HI])

(define_insn "<plusminus_insn><VADDSUB:mode>3"
  [(set (match_operand:VADDSUB 0 "register_operand" "=<vconstr>")
	(plusminus:VADDSUB (match_operand:VADDSUB 1 "register_operand" "<vconstr>")
			   (match_operand:VADDSUB 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS"
  "fp<plusminus_insn><vbits>\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")
   (set_attr "fptype" "<vfptype>")])

(define_mode_iterator VL [V1SI V2HI V4QI V1DI V2SI V4HI V8QI])
(define_mode_attr vlsuf [(V1SI "s") (V2HI "s") (V4QI "s")
			 (V1DI  "") (V2SI  "") (V4HI  "") (V8QI "")])
(define_code_iterator vlop [ior and xor])
(define_code_attr vlinsn [(ior "or") (and "and") (xor "xor")])
(define_code_attr vlninsn [(ior "nor") (and "nand") (xor "xnor")])

(define_insn "<vlop:code><VL:mode>3"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
	(vlop:VL (match_operand:VL 1 "register_operand" "<vconstr>")
		 (match_operand:VL 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS"
  "f<vlinsn><vlsuf>\t%1, %2, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

(define_insn "*not_<vlop:code><VL:mode>3"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
        (not:VL (vlop:VL (match_operand:VL 1 "register_operand" "<vconstr>")
			 (match_operand:VL 2 "register_operand" "<vconstr>"))))]
  "TARGET_VIS"
  "f<vlninsn><vlsuf>\t%1, %2, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

;; (ior (not (op1)) (not (op2))) is the canonical form of NAND.
(define_insn "*nand<VL:mode>_vis"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
	(ior:VL (not:VL (match_operand:VL 1 "register_operand" "<vconstr>"))
		(not:VL (match_operand:VL 2 "register_operand" "<vconstr>"))))]
  "TARGET_VIS"
  "fnand<vlsuf>\t%1, %2, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

(define_code_iterator vlnotop [ior and])

(define_insn "*<vlnotop:code>_not1<VL:mode>_vis"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
	(vlnotop:VL (not:VL (match_operand:VL 1 "register_operand" "<vconstr>"))
		    (match_operand:VL 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS"
  "f<vlinsn>not1<vlsuf>\t%1, %2, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

(define_insn "*<vlnotop:code>_not2<VL:mode>_vis"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
	(vlnotop:VL (match_operand:VL 1 "register_operand" "<vconstr>")
		    (not:VL (match_operand:VL 2 "register_operand" "<vconstr>"))))]
  "TARGET_VIS"
  "f<vlinsn>not2<vlsuf>\t%1, %2, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

(define_insn "one_cmpl<VL:mode>2"
  [(set (match_operand:VL 0 "register_operand" "=<vconstr>")
	(not:VL (match_operand:VL 1 "register_operand" "<vconstr>")))]
  "TARGET_VIS"
  "fnot1<vlsuf>\t%1, %0"
  [(set_attr "type" "visl")
   (set_attr "fptype" "<vfptype>")])

;; Hard to generate VIS instructions.  We have builtins for these.

(define_insn "fpack16_vis"
  [(set (match_operand:V4QI 0 "register_operand" "=f")
        (unspec:V4QI [(match_operand:V4HI 1 "register_operand" "e")
                      (reg:DI GSR_REG)]
		      UNSPEC_FPACK16))]
  "TARGET_VIS"
  "fpack16\t%1, %0"
  [(set_attr "type" "fgm_pack")
   (set_attr "fptype" "double")])

(define_insn "fpackfix_vis"
  [(set (match_operand:V2HI 0 "register_operand" "=f")
        (unspec:V2HI [(match_operand:V2SI 1 "register_operand" "e")
                      (reg:DI GSR_REG)]
		      UNSPEC_FPACKFIX))]
  "TARGET_VIS"
  "fpackfix\t%1, %0"
  [(set_attr "type" "fgm_pack")
   (set_attr "fptype" "double")])

(define_insn "fpack32_vis"
  [(set (match_operand:V8QI 0 "register_operand" "=e")
        (unspec:V8QI [(match_operand:V2SI 1 "register_operand" "e")
        	      (match_operand:V8QI 2 "register_operand" "e")
                      (reg:DI GSR_REG)]
                     UNSPEC_FPACK32))]
  "TARGET_VIS"
  "fpack32\t%1, %2, %0"
  [(set_attr "type" "fgm_pack")
   (set_attr "fptype" "double")])

(define_insn "fexpand_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V4QI 1 "register_operand" "f")]
         UNSPEC_FEXPAND))]
 "TARGET_VIS"
 "fexpand\t%1, %0"
 [(set_attr "type" "fga")
  (set_attr "subtype" "fpu")
  (set_attr "fptype" "double")])

(define_insn "fpmerge_vis"
  [(set (match_operand:V8QI 0 "register_operand" "=e")
        (vec_select:V8QI
          (vec_concat:V8QI (match_operand:V4QI 1 "register_operand" "f")
                           (match_operand:V4QI 2 "register_operand" "f"))
          (parallel [(const_int 0) (const_int 4)
                     (const_int 1) (const_int 5)
                     (const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
 "TARGET_VIS"
 "fpmerge\t%1, %2, %0"
 [(set_attr "type" "fga")
  (set_attr "subtype" "fpu")
  (set_attr "fptype" "double")])

;; Partitioned multiply instructions
(define_insn "fmul8x16_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V4QI 1 "register_operand" "f")
                      (match_operand:V4HI 2 "register_operand" "e")]
         UNSPEC_MUL8))]
  "TARGET_VIS"
  "fmul8x16\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmul8x16au_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V4QI 1 "register_operand" "f")
                      (match_operand:V2HI 2 "register_operand" "f")]
         UNSPEC_MUL16AU))]
  "TARGET_VIS"
  "fmul8x16au\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmul8x16al_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V4QI 1 "register_operand" "f")
                      (match_operand:V2HI 2 "register_operand" "f")]
         UNSPEC_MUL16AL))]
  "TARGET_VIS"
  "fmul8x16al\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmul8sux16_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V8QI 1 "register_operand" "e")
                      (match_operand:V4HI 2 "register_operand" "e")]
         UNSPEC_MUL8SU))]
  "TARGET_VIS"
  "fmul8sux16\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmul8ulx16_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V8QI 1 "register_operand" "e")
                      (match_operand:V4HI 2 "register_operand" "e")]
         UNSPEC_MUL8UL))]
  "TARGET_VIS"
  "fmul8ulx16\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmuld8sux16_vis"
  [(set (match_operand:V2SI 0 "register_operand" "=e")
        (unspec:V2SI [(match_operand:V4QI 1 "register_operand" "f")
                      (match_operand:V2HI 2 "register_operand" "f")]
         UNSPEC_MULDSU))]
  "TARGET_VIS"
  "fmuld8sux16\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_insn "fmuld8ulx16_vis"
  [(set (match_operand:V2SI 0 "register_operand" "=e")
        (unspec:V2SI [(match_operand:V4QI 1 "register_operand" "f")
                      (match_operand:V2HI 2 "register_operand" "f")]
         UNSPEC_MULDUL))]
  "TARGET_VIS"
  "fmuld8ulx16\t%1, %2, %0"
  [(set_attr "type" "fgm_mul")
   (set_attr "fptype" "double")])

(define_expand "wrgsr_vis"
  [(set (reg:DI GSR_REG) (match_operand:DI 0 "arith_operand" ""))]
  "TARGET_VIS"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_wrgsr_v8plus (operands[0]));
      DONE;
    }
})

(define_insn "*wrgsr_sp64"
  [(set (reg:DI GSR_REG) (match_operand:DI 0 "arith_operand" "rI"))]
  "TARGET_VIS && TARGET_ARCH64"
  "wr\t%%g0, %0, %%gsr"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "reg")])

(define_insn "wrgsr_v8plus"
  [(set (reg:DI GSR_REG) (match_operand:DI 0 "arith_operand" "I,r"))
   (clobber (match_scratch:SI 1 "=X,&h"))]
  "TARGET_VIS && TARGET_ARCH32"
{
  if (GET_CODE (operands[0]) == CONST_INT
      || sparc_check_64 (operands[0], insn))
    return "wr\t%%g0, %0, %%gsr";

  output_asm_insn("srl\t%L0, 0, %L0", operands);
  return "sllx\t%H0, 32, %1\n\tor\t%L0, %1, %1\n\twr\t%%g0, %1, %%gsr";
}
  [(set_attr "type" "multi")])

(define_expand "rdgsr_vis"
  [(set (match_operand:DI 0 "register_operand" "") (reg:DI GSR_REG))]
  "TARGET_VIS"
{
  if (TARGET_ARCH32)
    {
      emit_insn (gen_rdgsr_v8plus (operands[0]));
      DONE;
    }
})

(define_insn "*rdgsr_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r") (reg:DI GSR_REG))]
  "TARGET_VIS && TARGET_ARCH64"
  "rd\t%%gsr, %0"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "reg")])

(define_insn "rdgsr_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r") (reg:DI GSR_REG))
   (clobber (match_scratch:SI 1 "=&h"))]
  "TARGET_VIS && TARGET_ARCH32"
{
  return "rd\t%%gsr, %1\n\tsrlx\t%1, 32, %H0\n\tmov %1, %L0";
}
  [(set_attr "type" "multi")])

;; Using faligndata only makes sense after an alignaddr since the choice of
;; bytes to take out of each operand is dependent on the results of the last
;; alignaddr.
(define_insn "faligndata<VM64:mode>_vis"
  [(set (match_operand:VM64 0 "register_operand" "=e")
        (unspec:VM64 [(match_operand:VM64 1 "register_operand" "e")
                      (match_operand:VM64 2 "register_operand" "e")
                      (reg:DI GSR_REG)]
         UNSPEC_ALIGNDATA))]
  "TARGET_VIS"
  "faligndata\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")
   (set_attr "fptype" "double")])

(define_insn "alignaddrsi_vis"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
                 (match_operand:SI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 3) (const_int 0))
        (zero_extend:DI (plus:SI (match_dup 1) (match_dup 2))))]
  "TARGET_VIS"
  "alignaddr\t%r1, %r2, %0"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "alignaddr")])

(define_insn "alignaddrdi_vis"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (match_operand:DI 1 "register_or_zero_operand" "rJ")
                 (match_operand:DI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 3) (const_int 0))
        (plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_VIS"
  "alignaddr\t%r1, %r2, %0"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "alignaddr")])

(define_insn "alignaddrlsi_vis"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
                 (match_operand:SI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 3) (const_int 0))
        (xor:DI (zero_extend:DI (plus:SI (match_dup 1) (match_dup 2)))
                (const_int 7)))]
  "TARGET_VIS"
  "alignaddrl\t%r1, %r2, %0"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "alignaddr")])

(define_insn "alignaddrldi_vis"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (match_operand:DI 1 "register_or_zero_operand" "rJ")
                 (match_operand:DI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 3) (const_int 0))
        (xor:DI (plus:DI (match_dup 1) (match_dup 2))
                (const_int 7)))]
  "TARGET_VIS"
  "alignaddrl\t%r1, %r2, %0"
  [(set_attr "type" "gsr")
   (set_attr "subtype" "alignaddr")])

(define_insn "pdist_vis"
  [(set (match_operand:DI 0 "register_operand" "=e")
        (unspec:DI [(match_operand:V8QI 1 "register_operand" "e")
                    (match_operand:V8QI 2 "register_operand" "e")
                    (match_operand:DI 3 "register_operand" "0")]
         UNSPEC_PDIST))]
  "TARGET_VIS"
  "pdist\t%1, %2, %0"
  [(set_attr "type" "pdist")
   (set_attr "fptype" "double")])

;; Edge instructions produce condition codes equivalent to a 'subcc'
;; with the same operands.
(define_insn "edge8<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE8))]
  "TARGET_VIS"
  "edge8\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_insn "edge8l<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE8L))]
  "TARGET_VIS"
  "edge8l\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_insn "edge16<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE16))]
  "TARGET_VIS"
  "edge16\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_insn "edge16l<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE16L))]
  "TARGET_VIS"
  "edge16l\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_insn "edge32<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE32))]
  "TARGET_VIS"
  "edge32\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_insn "edge32l<P:mode>_vis"
  [(set (reg:CCNZ CC_REG)
        (compare:CCNZ (minus:P (match_operand:P 1 "register_or_zero_operand" "rJ")
			       (match_operand:P 2 "register_or_zero_operand" "rJ"))
		      (const_int 0)))
   (set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_dup 1) (match_dup 2)] UNSPEC_EDGE32L))]
  "TARGET_VIS"
  "edge32l\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

(define_mode_iterator FPCMP [V8QI V4HI V2SI])
(define_code_iterator fpcmpcond [eq ne le gt])
(define_code_iterator fpcmpucond [leu gtu])
(define_code_attr signed_code [(leu "le") (gtu "gt")])

(define_insn "fpcmp<fpcmpcond:code><FPCMP:vbits><P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
	(fpcmpcond:P (match_operand:FPCMP 1 "register_operand" "e")
		     (match_operand:FPCMP 2 "register_operand" "e")))]
  "TARGET_VIS
   && (<FPCMP:MODE>mode != V8QImode
       || (TARGET_VIS3B && (<fpcmpcond:CODE> == EQ || <fpcmpcond:CODE> == NE))
       || TARGET_VIS4)"
  "fpcmp<fpcmpcond:code><FPCMP:vbits>\t%1, %2, %0"
  [(set_attr "type" "viscmp")])

(define_expand "vec_cmp<FPCMP:mode><P:mode>"
  [(set (match_operand:P 0 "register_operand" "")
        (match_operator:P 1 "vec_cmp_operator"
          [(match_operand:FPCMP 2 "register_operand" "")
           (match_operand:FPCMP 3 "register_operand" "")]))]
  "TARGET_VIS3B"
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (code == LT || code == GE)
    {
      PUT_CODE (operands[1], swap_condition (code));
      std::swap (operands[2], operands[3]);
    }
})

(define_insn "fpcmpu<fpcmpucond:signed_code><FPCMP:vbits><P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
	(fpcmpucond:P (match_operand:FPCMP 1 "register_operand" "e")
		      (match_operand:FPCMP 2 "register_operand" "e")))]
  "TARGET_VIS3B && (<FPCMP:MODE>mode == V8QImode || TARGET_VIS4)"
  "fpcmpu<fpcmpucond:signed_code><FPCMP:vbits>\t%1, %2, %0"
  [(set_attr "type" "viscmp")])

(define_expand "vec_cmpu<FPCMP:mode><P:mode>"
  [(set (match_operand:P 0 "register_operand" "")
        (match_operator:P 1 "vec_cmpu_operator"
          [(match_operand:FPCMP 2 "register_operand" "")
           (match_operand:FPCMP 3 "register_operand" "")]))]
  "TARGET_VIS3B"
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (code == LTU || code == GEU)
    {
      PUT_CODE (operands[1], swap_condition (code));
      std::swap (operands[2], operands[3]);
    }
})

(define_expand "vcond_mask_<FPCMP:mode><P:mode>"
  [(match_operand:FPCMP 0 "register_operand" "")
   (match_operand:FPCMP 1 "register_operand" "")
   (match_operand:FPCMP 2 "register_operand" "")
   (match_operand:P     3 "register_operand" "")]
  "TARGET_VIS3B"
{
  sparc_expand_vcond_mask (<FPCMP:MODE>mode, operands, UNSPEC_CMASK<vbits>);
  DONE;
})

(define_insn "array8<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_ARRAY8))]
  "TARGET_VIS"
  "array8\t%r1, %r2, %0"
  [(set_attr "type" "array")])

(define_insn "array16<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_ARRAY16))]
  "TARGET_VIS"
  "array16\t%r1, %r2, %0"
  [(set_attr "type" "array")])

(define_insn "array32<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_ARRAY32))]
  "TARGET_VIS"
  "array32\t%r1, %r2, %0"
  [(set_attr "type" "array")])

(define_insn "bmaskdi_vis"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (match_operand:DI 1 "register_or_zero_operand" "rJ")
                 (match_operand:DI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 32) (const_int 32))
        (plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_VIS2 && TARGET_ARCH64"
  "bmask\t%r1, %r2, %0"
  [(set_attr "type" "bmask")])

(define_insn "bmasksi_vis"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (plus:SI (match_operand:SI 1 "register_or_zero_operand" "rJ")
                 (match_operand:SI 2 "register_or_zero_operand" "rJ")))
   (set (zero_extract:DI (reg:DI GSR_REG) (const_int 32) (const_int 32))
        (zero_extend:DI (plus:SI (match_dup 1) (match_dup 2))))]
  "TARGET_VIS2"
  "bmask\t%r1, %r2, %0"
  [(set_attr "type" "bmask")])

(define_insn "bshuffle<VM64:mode>_vis"
  [(set (match_operand:VM64 0 "register_operand" "=e")
        (unspec:VM64 [(match_operand:VM64 1 "register_operand" "e")
	              (match_operand:VM64 2 "register_operand" "e")
		      (reg:DI GSR_REG)]
                     UNSPEC_BSHUFFLE))]
  "TARGET_VIS2"
  "bshuffle\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")
   (set_attr "fptype" "double")])

;; Unlike constant permutation, we can vastly simplify the compression of
;; the 64-bit selector input to the 32-bit %gsr value by knowing what the
;; width of the input is.
(define_expand "vec_perm<VM64:mode>"
  [(match_operand:VM64 0 "register_operand" "")
   (match_operand:VM64 1 "register_operand" "")
   (match_operand:VM64 2 "register_operand" "")
   (match_operand:VM64 3 "register_operand" "")]
  "TARGET_VIS2"
{
  sparc_expand_vec_perm_bmask (<MODE>mode, operands[3]);
  emit_insn (gen_bshuffle<VM64:mode>_vis (operands[0], operands[1], operands[2]));
  DONE;
})

;; VIS 2.0 adds edge variants which do not set the condition codes
(define_insn "edge8n<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
	           (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE8N))]
  "TARGET_VIS2"
  "edge8n\t%r1, %r2, %0"
  [(set_attr "type" "edgen")])

(define_insn "edge8ln<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
	           (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE8LN))]
  "TARGET_VIS2"
  "edge8ln\t%r1, %r2, %0"
  [(set_attr "type" "edgen")])

(define_insn "edge16n<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE16N))]
  "TARGET_VIS2"
  "edge16n\t%r1, %r2, %0"
  [(set_attr "type" "edgen")])

(define_insn "edge16ln<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE16LN))]
  "TARGET_VIS2"
  "edge16ln\t%r1, %r2, %0"
  [(set_attr "type" "edgen")])

(define_insn "edge32n<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE32N))]
  "TARGET_VIS2"
  "edge32n\t%r1, %r2, %0"
  [(set_attr "type" "edgen")])

(define_insn "edge32ln<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:P 1 "register_or_zero_operand" "rJ")
                   (match_operand:P 2 "register_or_zero_operand" "rJ")]
                  UNSPEC_EDGE32LN))]
  "TARGET_VIS2"
  "edge32ln\t%r1, %r2, %0"
  [(set_attr "type" "edge")])

;; Conditional moves are possible via fcmpX --> cmaskX -> bshuffle
(define_insn "cmask8<P:mode>_vis"
  [(set (reg:DI GSR_REG)
        (unspec:DI [(match_operand:P 0 "register_or_zero_operand" "rJ")
	            (reg:DI GSR_REG)]
                   UNSPEC_CMASK8))]
  "TARGET_VIS3"
  "cmask8\t%r0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "cmask")])

(define_insn "cmask16<P:mode>_vis"
  [(set (reg:DI GSR_REG)
        (unspec:DI [(match_operand:P 0 "register_or_zero_operand" "rJ")
	            (reg:DI GSR_REG)]
                   UNSPEC_CMASK16))]
  "TARGET_VIS3"
  "cmask16\t%r0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "cmask")])

(define_insn "cmask32<P:mode>_vis"
  [(set (reg:DI GSR_REG)
        (unspec:DI [(match_operand:P 0 "register_or_zero_operand" "rJ")
	            (reg:DI GSR_REG)]
                   UNSPEC_CMASK32))]
  "TARGET_VIS3"
  "cmask32\t%r0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "cmask")])

(define_insn "fchksm16_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (unspec:V4HI [(match_operand:V4HI 1 "register_operand" "e")
                      (match_operand:V4HI 2 "register_operand" "e")]
                     UNSPEC_FCHKSM16))]
  "TARGET_VIS3"
  "fchksm16\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "fpu")])

(define_code_iterator vis3_shift [ashift ss_ashift lshiftrt ashiftrt])
(define_code_attr vis3_shift_insn
  [(ashift "fsll") (ss_ashift "fslas") (lshiftrt "fsrl") (ashiftrt "fsra")])
(define_code_attr vis3_shift_patname
  [(ashift "ashl") (ss_ashift "ssashl") (lshiftrt "lshr") (ashiftrt "ashr")])
   
(define_mode_iterator GCM [V4HI V2SI])

(define_insn "v<vis3_shift_patname><GCM:mode>3"
  [(set (match_operand:GCM 0 "register_operand" "=<vconstr>")
	(vis3_shift:GCM (match_operand:GCM 1 "register_operand" "<vconstr>")
			(match_operand:GCM 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS3"
  "<vis3_shift_insn><vbits>\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "fpu")])

(define_insn "pdistn<P:mode>_vis"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:V8QI 1 "register_operand" "e")
                   (match_operand:V8QI 2 "register_operand" "e")]
         UNSPEC_PDISTN))]
  "TARGET_VIS3"
  "pdistn\t%1, %2, %0"
  [(set_attr "type" "pdistn")
   (set_attr "fptype" "double")])

(define_insn "fmean16_vis"
  [(set (match_operand:V4HI 0 "register_operand" "=e")
        (truncate:V4HI
          (lshiftrt:V4SI
            (plus:V4SI
              (plus:V4SI
                (zero_extend:V4SI
                  (match_operand:V4HI 1 "register_operand" "e"))
                (zero_extend:V4SI
                  (match_operand:V4HI 2 "register_operand" "e")))
              (const_vector:V4SI [(const_int 1) (const_int 1)
                                  (const_int 1) (const_int 1)]))
          (const_int 1))))]
  "TARGET_VIS3"
  "fmean16\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "fpu")])

(define_insn "fp<plusminus_insn>64_vis"
  [(set (match_operand:V1DI 0 "register_operand" "=e")
	(plusminus:V1DI (match_operand:V1DI 1 "register_operand" "e")
			(match_operand:V1DI 2 "register_operand" "e")))]
  "TARGET_VIS3B"
  "fp<plusminus_insn>64\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "addsub64")])

(define_insn "<plusminus_insn>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=e")
        (plusminus:V8QI (match_operand:V8QI 1 "register_operand" "e")
                        (match_operand:V8QI 2 "register_operand" "e")))]
  "TARGET_VIS4"
  "fp<plusminus_insn>8\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")])

(define_mode_iterator VASS [V4HI V2SI V2HI V1SI])
(define_code_iterator vis3_addsub_ss [ss_plus ss_minus])
(define_code_attr vis3_addsub_ss_insn
  [(ss_plus "fpadds") (ss_minus "fpsubs")])
(define_code_attr vis3_addsub_ss_patname
  [(ss_plus "ssadd") (ss_minus "sssub")])

(define_insn "<vis3_addsub_ss_patname><VASS:mode>3"
  [(set (match_operand:VASS 0 "register_operand" "=<vconstr>")
        (vis3_addsub_ss:VASS (match_operand:VASS 1 "register_operand" "<vconstr>")
                             (match_operand:VASS 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS3"
  "<vis3_addsub_ss_insn><vbits>\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")])

(define_mode_iterator VMMAX [V8QI V4HI V2SI])
(define_code_iterator vis4_minmax [smin smax])
(define_code_attr vis4_minmax_insn
  [(smin "fpmin") (smax "fpmax")])
(define_code_attr vis4_minmax_patname
  [(smin "min") (smax "max")])

(define_insn "<vis4_minmax_patname><VMMAX:mode>3"
  [(set (match_operand:VMMAX 0 "register_operand" "=<vconstr>")
        (vis4_minmax:VMMAX (match_operand:VMMAX 1 "register_operand" "<vconstr>")
                           (match_operand:VMMAX 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS4"
  "<vis4_minmax_insn><vbits>\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "maxmin")])

(define_code_iterator vis4_uminmax [umin umax])
(define_code_attr vis4_uminmax_insn
  [(umin "fpminu") (umax "fpmaxu")])
(define_code_attr vis4_uminmax_patname
 [(umin "minu") (umax "maxu")])

(define_insn "<vis4_uminmax_patname><VMMAX:mode>3"
  [(set (match_operand:VMMAX 0 "register_operand" "=<vconstr>")
        (vis4_uminmax:VMMAX (match_operand:VMMAX 1 "register_operand" "<vconstr>")
                            (match_operand:VMMAX 2 "register_operand" "<vconstr>")))]
  "TARGET_VIS4"
  "<vis4_uminmax_insn><vbits>\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "maxmin")])

;; The use of vis3_addsub_ss_patname in the VIS4 instruction below is
;; intended.
(define_insn "<vis3_addsub_ss_patname>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=e")
        (vis3_addsub_ss:V8QI (match_operand:V8QI 1 "register_operand" "e")
                             (match_operand:V8QI 2 "register_operand" "e")))]
  "TARGET_VIS4"
  "<vis3_addsub_ss_insn>8\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")])

(define_mode_iterator VAUS [V4HI V8QI])
(define_code_iterator vis4_addsub_us [us_plus us_minus])
(define_code_attr vis4_addsub_us_insn
  [(us_plus "fpaddus") (us_minus "fpsubus")])
(define_code_attr vis4_addsub_us_patname
  [(us_plus "usadd") (us_minus "ussub")])

(define_insn "<vis4_addsub_us_patname><VAUS:mode>3"
 [(set (match_operand:VAUS 0 "register_operand" "=<vconstr>")
       (vis4_addsub_us:VAUS (match_operand:VAUS 1 "register_operand" "<vconstr>")
                            (match_operand:VAUS 2 "register_operand" "<vconstr>")))]
 "TARGET_VIS4"
 "<vis4_addsub_us_insn><vbits>\t%1, %2, %0"
 [(set_attr "type" "fga")
  (set_attr "subtype" "other")])

(define_insn "*naddsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (neg:SF (plus:SF (match_operand:SF 1 "register_operand" "f")
                         (match_operand:SF 2 "register_operand" "f"))))]
  "TARGET_VIS3"
  "fnadds\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "*nadddf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (neg:DF (plus:DF (match_operand:DF 1 "register_operand" "e")
                         (match_operand:DF 2 "register_operand" "e"))))]
  "TARGET_VIS3"
  "fnaddd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_insn "*nmulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (mult:SF (neg:SF (match_operand:SF 1 "register_operand" "f"))
                 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_VIS3"
  "fnmuls\t%1, %2, %0"
  [(set_attr "type" "fpmul")])

(define_insn "*nmuldf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (mult:DF (neg:DF (match_operand:DF 1 "register_operand" "e"))
                 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_VIS3"
  "fnmuld\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "fptype" "double")])

(define_insn "*nmuldf3_extend"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (mult:DF (neg:DF (float_extend:DF
                           (match_operand:SF 1 "register_operand" "f")))
                 (float_extend:DF
                   (match_operand:SF 2 "register_operand" "f"))))]
  "TARGET_VIS3"
  "fnsmuld\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "fptype" "double")])

(define_insn "fhaddsf_vis"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "register_operand" "f")
                    (match_operand:SF 2 "register_operand" "f")]
                   UNSPEC_FHADD))]
  "TARGET_VIS3"
  "fhadds\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "fhadddf_vis"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (unspec:DF [(match_operand:DF 1 "register_operand" "f")
                    (match_operand:DF 2 "register_operand" "f")]
                   UNSPEC_FHADD))]
  "TARGET_VIS3"
  "fhaddd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_insn "fhsubsf_vis"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (unspec:SF [(match_operand:SF 1 "register_operand" "f")
                    (match_operand:SF 2 "register_operand" "f")]
                   UNSPEC_FHSUB))]
  "TARGET_VIS3"
  "fhsubs\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "fhsubdf_vis"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (unspec:DF [(match_operand:DF 1 "register_operand" "f")
                    (match_operand:DF 2 "register_operand" "f")]
                   UNSPEC_FHSUB))]
  "TARGET_VIS3"
  "fhsubd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

(define_insn "fnhaddsf_vis"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (neg:SF (unspec:SF [(match_operand:SF 1 "register_operand" "f")
                            (match_operand:SF 2 "register_operand" "f")]
                           UNSPEC_FHADD)))]
  "TARGET_VIS3"
  "fnhadds\t%1, %2, %0"
  [(set_attr "type" "fp")])

(define_insn "fnhadddf_vis"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (neg:DF (unspec:DF [(match_operand:DF 1 "register_operand" "f")
                            (match_operand:DF 2 "register_operand" "f")]
                           UNSPEC_FHADD)))]
  "TARGET_VIS3"
  "fnhaddd\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "fptype" "double")])

;; VIS4B instructions (specified in the unpublished OSA 2017)

(define_mode_iterator DUMODE [V8QI V4HI V2SI])

;; Unpack a DUMODE right-justified value from {8,4,2} consecutive bitfields of (opnd 1):
;; for  0 <= (opnd 2) <= 7 : V8QI value from 8 consecutive bitfields of (opnd 2) + 1 bits
;; for  8 <= (opnd 2) <= 15: V4HI value from 4 consecutive bitfields of (opnd 2) + 1 bits
;; for 16 <= (opnd 2) <= 31: V2SI value from 2 consecutive bitfields of (opnd 2) + 1 bits
(define_insn "dictunpack<DUMODE:vbits>"
  [(set (match_operand:DUMODE 0 "register_operand" "=e")
        (unspec:DUMODE [(match_operand:DF 1 "register_operand" "e")
                        (match_operand:SI 2 "imm5_operand_dictunpack<DUMODE:vbits>" "t")]
         UNSPEC_DICTUNPACK))]
  "TARGET_VIS4B"
  "dictunpack\t%1, %2, %0"
  [(set_attr "type" "fga")
   (set_attr "subtype" "other")])

;; Same as fpcmp but the {8,4,2}-bit result is shifted left by (opnd 3) * {8,4,2}
(define_insn "fpcmp<fpcmpcond:code><FPCMP:vbits><P:mode>shl"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(fpcmpcond:FPCMP (match_operand:FPCMP 1 "register_operand" "e")
                                    (match_operand:FPCMP 2 "register_operand" "e"))
                   (match_operand:SI 3 "imm2_operand" "q")]
         UNSPEC_FPCMPSHL))]
   "TARGET_VIS4B"
   "fpcmp<fpcmpcond:code><FPCMP:vbits>shl\t%1, %2, %3, %0"
   [(set_attr "type" "viscmp")])

;; Same as fpcmpu but the {8,4,2}-bit result is shifted left by (opnd 3) * {8,4,2}
(define_insn "fpcmpu<fpcmpucond:signed_code><FPCMP:vbits><P:mode>shl"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(fpcmpucond:FPCMP (match_operand:FPCMP 1 "register_operand" "e")
                                     (match_operand:FPCMP 2 "register_operand" "e"))
                   (match_operand:SI 3 "imm2_operand" "q")]
         UNSPEC_FPCMPUSHL))]
   "TARGET_VIS4B"
   "fpcmpu<fpcmpucond:signed_code><FPCMP:vbits>shl\t%1, %2, %3, %0"
   [(set_attr "type" "viscmp")])

;; Dual Equal comparison: the unshifted result is the OR of two EQ comparisons
;; of (opnd 1) with 1) the 32-bit highpart of (opnd 2) concatenated with itself
;; and 2) the 32-bit lowpart of (opnd 2) concatenated with itself.
(define_insn "fpcmpde<FPCMP:vbits><P:mode>shl"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:FPCMP 1 "register_operand" "e")
                   (match_operand:FPCMP 2 "register_operand" "e")
                   (match_operand:SI 3 "imm2_operand" "q")]
         UNSPEC_FPCMPDESHL))]
   "TARGET_VIS4B"
   "fpcmpde<FPCMP:vbits>shl\t%1, %2, %3, %0"
   [(set_attr "type" "viscmp")])

;; Unsigned Range comparison: the unshifted result is True if (opnd 1) lies in
;; partitioned unsigned range (LB,HB) with LB) the 32-bit highpart of (opnd 2)
;; concatenated with itself and HB) the 32-bit lowpart of (opnd 2) concatenated
;; with itself.
(define_insn "fpcmpur<FPCMP:vbits><P:mode>shl"
  [(set (match_operand:P 0 "register_operand" "=r")
        (unspec:P [(match_operand:FPCMP 1 "register_operand" "e")
                   (match_operand:FPCMP 2 "register_operand" "e")
                   (match_operand:SI 3 "imm2_operand" "q")]
         UNSPEC_FPCMPURSHL))]
   "TARGET_VIS4B"
   "fpcmpur<FPCMP:vbits>shl\t%1, %2, %3, %0"
   [(set_attr "type" "viscmp")])

(include "sync.md")
