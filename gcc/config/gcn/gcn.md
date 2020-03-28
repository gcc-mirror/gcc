;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

(include "predicates.md")
(include "constraints.md")

;; {{{ Constants and enums

; Named registers
(define_constants
  [(FIRST_SGPR_REG		 0)
   (CC_SAVE_REG			 22)
   (LAST_SGPR_REG		 101)
   (FLAT_SCRATCH_REG		 102)
   (FLAT_SCRATCH_LO_REG		 102)
   (FLAT_SCRATCH_HI_REG		 103)
   (XNACK_MASK_REG		 104)
   (XNACK_MASK_LO_REG		 104)
   (XNACK_MASK_HI_REG		 105)
   (VCC_REG			 106)
   (VCC_LO_REG			 106)
   (VCC_HI_REG			 107)
   (VCCZ_REG			 108)
   (TBA_REG			 109)
   (TBA_LO_REG			 109)
   (TBA_HI_REG			 110)
   (TMA_REG			 111)
   (TMA_LO_REG			 111)
   (TMA_HI_REG			 112)
   (TTMP0_REG			 113)
   (TTMP11_REG			 124)
   (M0_REG			 125)
   (EXEC_REG			 126)
   (EXEC_LO_REG			 126)
   (EXEC_HI_REG			 127)
   (EXECZ_REG			 128)
   (SCC_REG			 129)
   (FIRST_VGPR_REG		 160)
   (LAST_VGPR_REG		 415)])

(define_constants
  [(SP_REGNUM 16)
   (LR_REGNUM 18)
   (AP_REGNUM 416)
   (FP_REGNUM 418)])

(define_c_enum "unspecv" [
  UNSPECV_PROLOGUE_USE
  UNSPECV_KERNEL_RETURN
  UNSPECV_BARRIER
  UNSPECV_ATOMIC
  UNSPECV_ICACHE_INV])

(define_c_enum "unspec" [
  UNSPEC_VECTOR
  UNSPEC_BPERMUTE
  UNSPEC_SGPRBASE
  UNSPEC_MEMORY_BARRIER
  UNSPEC_SMIN_DPP_SHR UNSPEC_SMAX_DPP_SHR
  UNSPEC_UMIN_DPP_SHR UNSPEC_UMAX_DPP_SHR
  UNSPEC_PLUS_DPP_SHR
  UNSPEC_PLUS_CARRY_DPP_SHR UNSPEC_PLUS_CARRY_IN_DPP_SHR
  UNSPEC_AND_DPP_SHR UNSPEC_IOR_DPP_SHR UNSPEC_XOR_DPP_SHR
  UNSPEC_MOV_DPP_SHR
  UNSPEC_MOV_FROM_LANE63
  UNSPEC_GATHER
  UNSPEC_SCATTER])

;; }}}
;; {{{ Attributes

; Instruction type (encoding) as described in the ISA specification.
; The following table summarizes possible operands of individual instruction
; types and corresponding constraints.
;
; sop2 - scalar, two inputs, one output
;	 ssrc0/ssrc1: sgpr 0-102; flat_scratch,xnack,vcc,tba,tma,ttmp0-11,exec
;		      vccz,execz,scc,inline immedate,fp inline immediate
;	 sdst: sgpr 0-102; flat_scratch,xnack,vcc,tba,tma,ttmp0-11,exec
;
;	 Constraints "=SD, SD", "SSA,SSB","SSB,SSA"
;
; sopk - scalar, inline constant input, one output
;	 simm16: 16bit inline constant
;	 sdst: same as sop2/ssrc0
;
;	 Constraints "=SD", "J"
;
; sop1 - scalar, one input, one output
;	 ssrc0: same as sop2/ssrc0.  FIXME: manual omit VCCZ
;	 sdst: same as sop2/sdst
;
;	 Constraints "=SD", "SSA"
;
; sopc - scalar, two inputs, one comparsion
;	 ssrc0: same as sop2/ssc0.
;
;	 Constraints "SSI,SSA","SSA,SSI"
;
; sopp - scalar, one constant input, one special
;	 simm16
;
; smem - scalar memory
;	 sbase: aligned pair of sgprs.  Specify {size[15:0], base[47:0]} in
;               dwords
;	 sdata: sgpr0-102, flat_scratch, xnack, vcc, tba, tma
;	 offset: sgpr or 20bit unsigned byte offset
;
; vop2 - vector, two inputs, one output
;	 vsrc0: sgpr0-102,flat_scratch,xnack,vcc,tba,ttmp0-11,m0,exec,
;		inline constant -16 to -64, fp inline immediate, vccz, execz,
;		scc, lds, literal constant, vgpr0-255
;	 vsrc1: vgpr0-255
;	 vdst: vgpr0-255
;	 Limitations: At most one SGPR, at most one constant
;		      if constant is used, SGPR must be M0
;		      Only SRC0 can be LDS_DIRECT
;
;	 constraints: "=v", "vBSv", "v"
;
; vop1 - vector, one input, one output
;	 vsrc0: same as vop2/src0
;	 vdst: vgpr0-255
;
;	 constraints: "=v", "vBSv"
;
; vopc - vector, two inputs, one comparsion output;
;	 vsrc0: same as vop2/src0
;	 vsrc1: vgpr0-255
;	 vdst:
;
;	 constraints: "vASv", "v"
;
; vop3a - vector, three inputs, one output
;	 vdst: vgpr0-255, for v_cmp sgpr or vcc
;	 abs,clamp
;	 vsrc0: sgpr0-102,vcc,tba,ttmp0-11,m0,exec,
;		inline constant -16 to -64, fp inline immediate, vccz, execz,
;		scc, lds_direct
;		FIXME: really missing 1/pi? really 104 SGPRs
;
; vop3b - vector, three inputs, one vector output, one scalar output
;	 vsrc0,vsrc1,vsrc2: same as vop3a vsrc0
;	 vdst: vgpr0-255
;	 sdst: sgpr0-103/vcc/tba/tma/ttmp0-11
;
; vop_sdwa - second dword for vop1/vop2/vopc for specifying sub-dword address
;	 src0: vgpr0-255
;	 dst_sel: BYTE_0-3, WORD_0-1, DWORD
;	 dst_unused: UNUSED_PAD, UNUSED_SEXT, UNUSED_PRESERVE
;	 clamp: true/false
;	 src0_sel: BYTE_0-3, WORD_0-1, DWORD
;	 flags: src0_sext, src0_neg, src0_abs, src1_sel, src1_sext, src1_neg,
  ;		src1_abs
;
; vop_dpp - second dword for vop1/vop2/vopc for specifying data-parallel ops
;	 src0: vgpr0-255
;	 dpp_ctrl: quad_perm, row_sl0-15, row_sr0-15, row_rr0-15, wf_sl1,
;		  wf_rl1, wf_sr1, wf_rr1, row_mirror, row_half_mirror,
;		  bcast15, bcast31
;	 flags: src0_neg, src0_abs, src1_neg, src1_abs
;	 bank_mask: 4-bit mask
;	 row_mask: 4-bit mask
;
; ds - Local and global data share instructions.
;	 offset0: 8-bit constant
;	 offset1: 8-bit constant
;	 flag: gds
;	 addr: vgpr0-255
;	 data0: vgpr0-255
;	 data1: vgpr0-255
;	 vdst: vgpr0-255
;
; mubuf - Untyped memory buffer operation. First word with LDS, second word
;	  non-LDS.
;	 offset: 12-bit constant
;	 vaddr: vgpr0-255
;	 vdata: vgpr0-255
;	 srsrc: sgpr0-102
;	 soffset: sgpr0-102
;	 flags: offen, idxen, glc, lds, slc, tfe
;
; mtbuf - Typed memory buffer operation. Two words
;	 offset: 12-bit constant
;	 dfmt: 4-bit constant
;	 nfmt: 3-bit constant
;	 vaddr: vgpr0-255
;	 vdata: vgpr0-255
;	 srsrc: sgpr0-102
;	 soffset: sgpr0-102
;	 flags: offen, idxen, glc, lds, slc, tfe
;
; flat - flat or global memory operations
;	 flags: glc, slc
;	 addr: vgpr0-255
;	 data: vgpr0-255
;	 vdst: vgpr0-255
;
; mult - expands to multiple instructions (pseudo encoding)
;
; vmult - as mult, when a vector instruction is used.

(define_attr "type"
	     "unknown,sop1,sop2,sopk,sopc,sopp,smem,ds,vop2,vop1,vopc,
	      vop3a,vop3b,vop_sdwa,vop_dpp,mubuf,mtbuf,flat,mult,vmult"
	     (const_string "unknown"))

; Set if instruction is executed in scalar or vector unit

(define_attr "unit" "unknown,scalar,vector"
  (cond [(eq_attr "type" "sop1,sop2,sopk,sopc,sopp,smem,mult")
	    (const_string "scalar")
	 (eq_attr "type" "vop2,vop1,vopc,vop3a,vop3b,ds,
			  vop_sdwa,vop_dpp,flat,vmult")
	    (const_string "vector")]
	 (const_string "unknown")))

; All vector instructions run as 64 threads as predicated by the EXEC
; register.  Scalar operations in vector register require a single lane
; enabled, vector moves require a full set of lanes enabled, and most vector
; operations handle the lane masking themselves.
; The md_reorg pass is responsible for ensuring that EXEC is set appropriately
; according to the following settings:
;   auto   - md_reorg will inspect def/use to determine what to do.
;   none   - exec is not needed.
;   single - disable all but lane zero.
;   full   - enable all lanes.

(define_attr "exec" "auto,none,single,full"
   (const_string "auto"))

; Infer the (worst-case) length from the instruction type by default.  Many
; types can have an optional immediate word following, which we include here.
; "Multiple" types are counted as two 64-bit instructions.  This is just a
; default fallback: it can be overridden per-alternative in insn patterns for
; greater accuracy.

(define_attr "length" ""
  (cond [(eq_attr "type" "sop1") (const_int 8)
	 (eq_attr "type" "sop2") (const_int 8)
	 (eq_attr "type" "sopk") (const_int 8)
	 (eq_attr "type" "sopc") (const_int 8)
	 (eq_attr "type" "sopp") (const_int 4)
	 (eq_attr "type" "smem") (const_int 8)
	 (eq_attr "type" "ds")   (const_int 8)
	 (eq_attr "type" "vop1") (const_int 8)
	 (eq_attr "type" "vop2") (const_int 8)
	 (eq_attr "type" "vopc") (const_int 8)
	 (eq_attr "type" "vop3a") (const_int 8)
	 (eq_attr "type" "vop3b") (const_int 8)
	 (eq_attr "type" "vop_sdwa") (const_int 8)
	 (eq_attr "type" "vop_dpp") (const_int 8)
	 (eq_attr "type" "flat") (const_int 8)
	 (eq_attr "type" "mult") (const_int 16)
	 (eq_attr "type" "vmult") (const_int 16)]
	(const_int 4)))

; Disable alternatives that only apply to specific ISA variants.

(define_attr "gcn_version" "gcn3,gcn5" (const_string "gcn3"))

(define_attr "enabled" ""
  (cond [(eq_attr "gcn_version" "gcn3") (const_int 1)
	 (and (eq_attr "gcn_version" "gcn5")
	      (ne (symbol_ref "TARGET_GCN5_PLUS") (const_int 0)))
	   (const_int 1)]
	(const_int 0)))

; We need to be able to identify v_readlane and v_writelane with
; SGPR lane selection in order to handle "Manually Inserted Wait States".

(define_attr "laneselect" "yes,no" (const_string "no"))

; Identify instructions that require a "Manually Inserted Wait State" if
; their inputs are overwritten by subsequent instructions.

(define_attr "delayeduse" "yes,no" (const_string "no"))

;; }}}
;; {{{ Iterators useful across the wole machine description

(define_mode_iterator SIDI [SI DI])
(define_mode_iterator SFDF [SF DF])
(define_mode_iterator SISF [SI SF])
(define_mode_iterator QIHI [QI HI])
(define_mode_iterator DIDF [DI DF])
(define_mode_iterator FP [HF SF DF])
(define_mode_iterator FP_1REG [HF SF])

;; }}}
;; {{{ Attributes.

; Translate RTX code into GCN instruction mnemonics with and without
; suffixes such as _b32, etc.

(define_code_attr mnemonic
  [(minus "sub%i")
   (plus "add%i")
   (ashift "lshl%b")
   (lshiftrt "lshr%b")
   (ashiftrt "ashr%i")
   (and "and%B")
   (ior "or%B")
   (xor "xor%B")
   (mult "mul%i")
   (smin "min%i")
   (smax "max%i")
   (umin "min%u")
   (umax "max%u")
   (not "not%B")
   (popcount "bcnt_u32%b")])

(define_code_attr bare_mnemonic
  [(plus "add")
   (minus "sub")
   (and "and")
   (ior "or")
   (xor "xor")])

(define_code_attr s_mnemonic
  [(not "not%b")
   (popcount "bcnt1_i32%b")
   (clz "flbit_i32%b")
   (ctz "ff1_i32%b")])

(define_code_attr revmnemonic
  [(minus "subrev%i")
   (ashift "lshlrev%b")
   (lshiftrt "lshrrev%b")
   (ashiftrt "ashrrev%i")])

; Translate RTX code into corresponding expander name.

(define_code_attr expander
  [(and "and")
   (ior "ior")
   (xor "xor")
   (plus "add")
   (minus "sub")
   (ashift "ashl")
   (lshiftrt "lshr")
   (ashiftrt "ashr")
   (mult "mul")
   (smin "smin")
   (smax "smax")
   (umin "umin")
   (umax "umax")
   (not "one_cmpl")
   (popcount "popcount")
   (clz "clz")
   (ctz "ctz")
   (sign_extend "extend")
   (zero_extend "zero_extend")])

;; }}}
;; {{{ Miscellaneous instructions

(define_insn "nop"
  [(const_int 0)]
  ""
  "s_nop\t0x0"
  [(set_attr "type" "sopp")])

; FIXME: What should the value of the immediate be? Zero is disallowed, so
; pick 1 for now.
(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "s_trap\t1"
  [(set_attr "type" "sopp")])

;; }}}
;; {{{ Moves

;; All scalar modes we support moves in.
(define_mode_iterator MOV_MODE [BI QI HI SI DI TI SF DF])

; This is the entry point for creating all kinds of scalar moves,
; including reloads and symbols.

(define_expand "mov<mode>"
  [(set (match_operand:MOV_MODE 0 "nonimmediate_operand")
	(match_operand:MOV_MODE 1 "general_operand"))]
  ""
  {
    if (SUBREG_P (operands[1])
	&& GET_MODE (operands[1]) == SImode
	&& GET_MODE (SUBREG_REG (operands[1])) == BImode)
    {
      /* (reg:BI VCC) has nregs==2 to ensure it gets clobbered as a whole,
	 but (subreg:SI (reg:BI VCC)) doesn't, which causes the LRA liveness
	 checks to assert.  Transform this:
	   (set (reg:SI) (subreg:SI (reg:BI)))
	 to this:
	   (set (subreg:BI (reg:SI)) (reg:BI))  */
      operands[0] = gen_rtx_SUBREG (BImode, operands[0], 0);
      operands[1] = SUBREG_REG (operands[1]);
    }
    if (SUBREG_P (operands[0])
	&& GET_MODE (operands[0]) == SImode
	&& GET_MODE (SUBREG_REG (operands[0])) == BImode)
      {
	/* Likewise, transform this:
	     (set (subreg:SI (reg:BI)) (reg:SI))
	   to this:
	     (set (reg:BI) (subreg:BI (reg:SI))) */
	operands[0] = SUBREG_REG (operands[0]);
	operands[1] = gen_rtx_SUBREG (BImode, operands[1], 0);
      }

    if (MEM_P (operands[0]))
      operands[1] = force_reg (<MODE>mode, operands[1]);

    if (!lra_in_progress && !reload_completed
	&& !gcn_valid_move_p (<MODE>mode, operands[0], operands[1]))
      {
	/* Something is probably trying to generate a move
	   which can only work indirectly.
	   E.g. Move from LDS memory to SGPR hardreg
	     or MEM:QI to SGPR.  */
	rtx tmpreg = gen_reg_rtx (<MODE>mode);
	emit_insn (gen_mov<mode> (tmpreg, operands[1]));
	emit_insn (gen_mov<mode> (operands[0], tmpreg));
	DONE;
      }

    if (<MODE>mode == DImode
	&& (GET_CODE (operands[1]) == SYMBOL_REF
	    || GET_CODE (operands[1]) == LABEL_REF))
      {
	if (lra_in_progress)
	  emit_insn (gen_movdi_symbol_save_scc (operands[0], operands[1]));
	else
	  emit_insn (gen_movdi_symbol (operands[0], operands[1]));
	DONE;
      }
  })

; Split invalid moves into two valid moves

(define_split
  [(set (match_operand:MOV_MODE 0 "nonimmediate_operand")
	(match_operand:MOV_MODE 1 "general_operand"))]
  "!reload_completed && !lra_in_progress
   && !gcn_valid_move_p (<MODE>mode, operands[0], operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (match_dup 2))]
  {
    operands[2] = gen_reg_rtx(<MODE>mode);
  })

; We need BImode move so we can reload flags registers.

(define_insn "*movbi"
  [(set (match_operand:BI 0 "nonimmediate_operand"
				    "=Sg,   v,Sg,cs,cV,cV,Sm,RS, v,RF, v,RM")
	(match_operand:BI 1 "gcn_load_operand"
				    "SSA,vSvA, v,SS, v,SS,RS,Sm,RF, v,RM, v"))]
  ""
  {
    /* SCC as an operand is currently not accepted by the LLVM assembler, so
       we emit bytes directly as a workaround.  */
    switch (which_alternative) {
    case 0:
      if (REG_P (operands[1]) && REGNO (operands[1]) == SCC_REG)
	return "; s_mov_b32\t%0,%1 is not supported by the assembler.\;"
	       ".byte\t0xfd\;"
	       ".byte\t0x0\;"
	       ".byte\t0x80|%R0\;"
	       ".byte\t0xbe";
      else
	return "s_mov_b32\t%0, %1";
    case 1:
      if (REG_P (operands[1]) && REGNO (operands[1]) == SCC_REG)
	return "; v_mov_b32\t%0, %1\;"
	       ".byte\t0xfd\;"
	       ".byte\t0x2\;"
	       ".byte\t((%V0<<1)&0xff)\;"
	       ".byte\t0x7e|(%V0>>7)";
      else
	return "v_mov_b32\t%0, %1";
    case 2:
      return "v_readlane_b32\t%0, %1, 0";
    case 3:
      return "s_cmpk_lg_u32\t%1, 0";
    case 4:
      return "v_cmp_ne_u32\tvcc, 0, %1";
    case 5:
      if (REGNO (operands[1]) == SCC_REG)
	return "; s_mov_b32\t%0, %1 is not supported by the assembler.\;"
	       ".byte\t0xfd\;"
	       ".byte\t0x0\;"
	       ".byte\t0xea\;"
	       ".byte\t0xbe\;"
	       "s_mov_b32\tvcc_hi, 0";
      else
	return "s_mov_b32\tvcc_lo, %1\;"
	       "s_mov_b32\tvcc_hi, 0";
    case 6:
      return "s_load_dword\t%0, %A1\;s_waitcnt\tlgkmcnt(0)";
    case 7:
      return "s_store_dword\t%1, %A0";
    case 8:
      return "flat_load_dword\t%0, %A1%O1%g1\;s_waitcnt\t0";
    case 9:
      return "flat_store_dword\t%A0, %1%O0%g0";
    case 10:
      return "global_load_dword\t%0, %A1%O1%g1\;s_waitcnt\tvmcnt(0)";
    case 11:
      return "global_store_dword\t%A0, %1%O0%g0";
    default:
      gcc_unreachable ();
    }
  }
  [(set_attr "type" "sop1,vop1,vop3a,sopk,vopc,mult,smem,smem,flat,flat,
		     flat,flat")
   (set_attr "exec" "*,*,none,*,*,*,*,*,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,8,12,12,12,12,12,12")])

; 32bit move pattern

(define_insn "*mov<mode>_insn"
  [(set (match_operand:SISF 0 "nonimmediate_operand"
		  "=SD,SD,SD,SD,RB,Sm,RS,v,Sg, v, v,RF,v,RLRG,   v,SD, v,RM")
	(match_operand:SISF 1 "gcn_load_operand"
		  "SSA, J, B,RB,Sm,RS,Sm,v, v,Sv,RF, v,B,   v,RLRG, Y,RM, v"))]
  ""
  "@
  s_mov_b32\t%0, %1
  s_movk_i32\t%0, %1
  s_mov_b32\t%0, %1
  s_buffer_load%s0\t%0, s[0:3], %1\;s_waitcnt\tlgkmcnt(0)
  s_buffer_store%s1\t%1, s[0:3], %0
  s_load_dword\t%0, %A1\;s_waitcnt\tlgkmcnt(0)
  s_store_dword\t%1, %A0
  v_mov_b32\t%0, %1
  v_readlane_b32\t%0, %1, 0
  v_writelane_b32\t%0, %1, 0
  flat_load_dword\t%0, %A1%O1%g1\;s_waitcnt\t0
  flat_store_dword\t%A0, %1%O0%g0
  v_mov_b32\t%0, %1
  ds_write_b32\t%A0, %1%O0
  ds_read_b32\t%0, %A1%O1\;s_waitcnt\tlgkmcnt(0)
  s_mov_b32\t%0, %1
  global_load_dword\t%0, %A1%O1%g1\;s_waitcnt\tvmcnt(0)
  global_store_dword\t%A0, %1%O0%g0"
  [(set_attr "type" "sop1,sopk,sop1,smem,smem,smem,smem,vop1,vop3a,vop3a,flat,
		     flat,vop1,ds,ds,sop1,flat,flat")
   (set_attr "exec" "*,*,*,*,*,*,*,*,none,none,*,*,*,*,*,*,*,*")
   (set_attr "length" "4,4,8,12,12,12,12,4,8,8,12,12,8,12,12,8,12,12")])

; 8/16bit move pattern

(define_insn "*mov<mode>_insn"
  [(set (match_operand:QIHI 0 "nonimmediate_operand"
				 "=SD,SD,SD,v,Sg, v, v,RF,v,RLRG,   v, v,RM")
	(match_operand:QIHI 1 "gcn_load_operand"
				 "SSA, J, B,v, v,Sv,RF, v,B,   v,RLRG,RM, v"))]
  "gcn_valid_move_p (<MODE>mode, operands[0], operands[1])"
  "@
  s_mov_b32\t%0, %1
  s_movk_i32\t%0, %1
  s_mov_b32\t%0, %1
  v_mov_b32\t%0, %1
  v_readlane_b32\t%0, %1, 0
  v_writelane_b32\t%0, %1, 0
  flat_load%o1\t%0, %A1%O1%g1\;s_waitcnt\t0
  flat_store%s0\t%A0, %1%O0%g0
  v_mov_b32\t%0, %1
  ds_write%b0\t%A0, %1%O0
  ds_read%u1\t%0, %A1%O1\;s_waitcnt\tlgkmcnt(0)
  global_load%o1\t%0, %A1%O1%g1\;s_waitcnt\tvmcnt(0)
  global_store%s0\t%A0, %1%O0%g0"
  [(set_attr "type"
	     "sop1,sopk,sop1,vop1,vop3a,vop3a,flat,flat,vop1,ds,ds,flat,flat")
   (set_attr "exec" "*,*,*,*,none,none,*,*,*,*,*,*,*")
   (set_attr "length" "4,4,8,4,4,4,12,12,8,12,12,12,12")])

; 64bit move pattern

(define_insn_and_split "*mov<mode>_insn"
  [(set (match_operand:DIDF 0 "nonimmediate_operand"
			  "=SD,SD,SD,RS,Sm,v, v,Sg, v, v,RF,RLRG,   v, v,RM")
	(match_operand:DIDF 1 "general_operand"
			  "SSA, C,DB,Sm,RS,v,DB, v,Sv,RF, v,   v,RLRG,RM, v"))]
  "GET_CODE(operands[1]) != SYMBOL_REF"
  "@
  s_mov_b64\t%0, %1
  s_mov_b64\t%0, %1
  #
  s_store_dwordx2\t%1, %A0
  s_load_dwordx2\t%0, %A1\;s_waitcnt\tlgkmcnt(0)
  #
  #
  #
  #
  flat_load_dwordx2\t%0, %A1%O1%g1\;s_waitcnt\t0
  flat_store_dwordx2\t%A0, %1%O0%g0
  ds_write_b64\t%A0, %1%O0
  ds_read_b64\t%0, %A1%O1\;s_waitcnt\tlgkmcnt(0)
  global_load_dwordx2\t%0, %A1%O1%g1\;s_waitcnt\tvmcnt(0)
  global_store_dwordx2\t%A0, %1%O0%g0"
  "(reload_completed && !MEM_P (operands[0]) && !MEM_P (operands[1])
    && !gcn_sgpr_move_p (operands[0], operands[1]))
   || (GET_CODE (operands[1]) == CONST_INT && !gcn_constant64_p (operands[1]))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  {
    rtx inlo = gen_lowpart (SImode, operands[1]);
    rtx inhi = gen_highpart_mode (SImode, <MODE>mode, operands[1]);
    rtx outlo = gen_lowpart (SImode, operands[0]);
    rtx outhi = gen_highpart_mode (SImode, <MODE>mode, operands[0]);

    /* Ensure that overlapping registers aren't corrupted.  */
    if (REGNO (outlo) == REGNO (inhi))
      {
	operands[0] = outhi;
	operands[1] = inhi;
	operands[2] = outlo;
	operands[3] = inlo;
      }
    else
      {
	operands[0] = outlo;
	operands[1] = inlo;
	operands[2] = outhi;
	operands[3] = inhi;
      }
  }
  [(set_attr "type" "sop1,sop1,mult,smem,smem,vmult,vmult,vmult,vmult,flat,
		     flat,ds,ds,flat,flat")
   (set_attr "length" "4,8,*,12,12,*,*,*,*,12,12,12,12,12,12")])

; 128-bit move.

(define_insn_and_split "*movti_insn"
  [(set (match_operand:TI 0 "nonimmediate_operand"
				      "=SD,RS,Sm,RF, v,v, v,SD,RM, v,RL, v")
	(match_operand:TI 1 "general_operand"  
				      "SSB,Sm,RS, v,RF,v,Sv, v, v,RM, v,RL"))]
  ""
  "@
  #
  s_store_dwordx4\t%1, %A0
  s_load_dwordx4\t%0, %A1\;s_waitcnt\tlgkmcnt(0)
  flat_store_dwordx4\t%A0, %1%O0%g0
  flat_load_dwordx4\t%0, %A1%O1%g1\;s_waitcnt\t0
  #
  #
  #
  global_store_dwordx4\t%A0, %1%O0%g0
  global_load_dwordx4\t%0, %A1%O1%g1\;s_waitcnt\tvmcnt(0)
  ds_write_b128\t%A0, %1%O0
  ds_read_b128\t%0, %A1%O1\;s_waitcnt\tlgkmcnt(0)"
  "reload_completed
   && REG_P (operands[0])
   && (REG_P (operands[1]) || GET_CODE (operands[1]) == CONST_INT)"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
  {
    operands[6] = gcn_operand_part (TImode, operands[0], 3);
    operands[7] = gcn_operand_part (TImode, operands[1], 3);
    operands[4] = gcn_operand_part (TImode, operands[0], 2);
    operands[5] = gcn_operand_part (TImode, operands[1], 2);
    operands[2] = gcn_operand_part (TImode, operands[0], 1);
    operands[3] = gcn_operand_part (TImode, operands[1], 1);
    operands[0] = gcn_operand_part (TImode, operands[0], 0);
    operands[1] = gcn_operand_part (TImode, operands[1], 0);
  }
  [(set_attr "type" "mult,smem,smem,flat,flat,vmult,vmult,vmult,flat,flat,\
		     ds,ds")
   (set_attr "delayeduse" "*,*,yes,*,*,*,*,*,yes,*,*,*")
   (set_attr "length" "*,12,12,12,12,*,*,*,12,12,12,12")])

;; }}}
;; {{{ Prologue/Epilogue

(define_insn "prologue_use"
  [(unspec_volatile [(match_operand 0)] UNSPECV_PROLOGUE_USE)]
  ""
  ""
  [(set_attr "length" "0")])

(define_expand "prologue"
  [(const_int 0)]
  ""
  {
    gcn_expand_prologue ();
    DONE;
  })

(define_expand "epilogue"
  [(const_int 0)]
  ""
  {
    gcn_expand_epilogue ();
    DONE;
  })

;; }}}
;; {{{ Control flow

; This pattern must satisfy simplejump_p, which means it cannot be a parallel
; that clobbers SCC.  Thus, we must preserve SCC if we're generating a long
; branch sequence.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0)))]
  ""
  {
    if (get_attr_length (insn) == 4)
      return "s_branch\t%0";
    else
      /* !!! This sequence clobbers EXEC_SAVE_REG and CC_SAVE_REG.  */
      return "; s_mov_b32\ts22, scc is not supported by the assembler.\;"
	     ".long\t0xbe9600fd\;"
	     "s_getpc_b64\ts[20:21]\;"
	     "s_add_u32\ts20, s20, %0@rel32@lo+4\;"
	     "s_addc_u32\ts21, s21, %0@rel32@hi+4\;"
	     "s_cmpk_lg_u32\ts22, 0\;"
	     "s_setpc_b64\ts[20:21]";
  }
  [(set_attr "type" "sopp")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 0) (pc))
			       (const_int -131072))
			   (lt (minus (match_dup 0) (pc))
			       (const_int 131072)))
		      (const_int 4)
		      (const_int 32)))])

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:DI 0 "register_operand" "Sg"))]
  ""
  "s_setpc_b64\t%0"
  [(set_attr "type" "sop1")
   (set_attr "length" "4")])

(define_insn "cjump"
  [(set (pc)
	(if_then_else
	  (match_operator:BI 1 "gcn_conditional_operator"
	    [(match_operand:BI 2 "gcn_conditional_register_operand" "ca,cV")
	     (const_int 0)])
	  (label_ref (match_operand 0))
	  (pc)))]
  ""
  {
    if (get_attr_length (insn) == 4)
      return "s_cbranch%C1\t%0";
    else
      {
	/* !!! This sequence clobbers EXEC_SAVE_REG and CC_SAVE_REG but
	       restores SCC.  */
	if (REGNO (operands[2]) == SCC_REG)
	  {
	    if (GET_CODE (operands[1]) == EQ)
	      return "s_cbranch%c1\t.Lskip%=\;"
		     "s_getpc_b64\ts[20:21]\;"
		     "s_add_u32\ts20, s20, %0@rel32@lo+4\;"
		     "s_addc_u32\ts21, s21, %0@rel32@hi+4\;"
		     "s_cmp_lg_u32\t0, 0\;"
		     "s_setpc_b64\ts[20:21]\n"
		     ".Lskip%=:";
	    else
	      return "s_cbranch%c1\t.Lskip%=\;"
		     "s_getpc_b64\ts[20:21]\;"
		     "s_add_u32\ts20, s20, %0@rel32@lo+4\;"
		     "s_addc_u32\ts21, s21, %0@rel32@hi+4\;"
		     "s_cmp_eq_u32\t0, 0\;"
		     "s_setpc_b64\ts[20:21]\n"
		     ".Lskip%=:";
	  }
	else
	  return "s_cbranch%c1\t.Lskip%=\;"
		 "; s_mov_b32\ts22, scc is not supported by the assembler.\;"
		 ".byte\t0xfd\;"
		 ".byte\t0x0\;"
		 ".byte\t0x80|22\;"
		 ".byte\t0xbe\;"
		 "s_getpc_b64\ts[20:21]\;"
		 "s_add_u32\ts20, s20, %0@rel32@lo+4\;"
		 "s_addc_u32\ts21, s21, %0@rel32@hi+4\;"
		 "s_cmpk_lg_u32\ts22, 0\;"
		 "s_setpc_b64\ts[20:21]\n"
		 ".Lskip%=:";
      }
  }
  [(set_attr "type" "sopp")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 0) (pc))
			       (const_int -131072))
			   (lt (minus (match_dup 0) (pc))
			       (const_int 131072)))
		      (const_int 4)
		      (const_int 36)))])

; Returning from a normal function is different to returning from a
; kernel function.

(define_insn "gcn_return"
  [(return)]
  ""
  {
    if (cfun && cfun->machine && cfun->machine->normal_function)
      return "s_setpc_b64\ts[18:19]";
    else
      return "s_waitcnt\tlgkmcnt(0)\;s_dcache_wb\;s_endpgm";
  }
  [(set_attr "type" "sop1")
   (set_attr "length" "12")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (clobber (reg:DI LR_REGNUM))
	      (clobber (match_scratch:DI 2))])]
  ""
  {})

(define_insn "gcn_simple_call"
  [(call (mem (match_operand 0 "immediate_operand" "Y,B"))
	 (match_operand 1 "const_int_operand"))
   (clobber (reg:DI LR_REGNUM))
   (clobber (match_scratch:DI 2 "=&Sg,X"))]
  ""
  "@
  s_getpc_b64\t%2\;s_add_u32\t%L2, %L2, %0@rel32@lo+4\;s_addc_u32\t%H2, %H2, %0@rel32@hi+4\;s_swappc_b64\ts[18:19], %2
  s_swappc_b64\ts[18:19], %0"
  [(set_attr "type" "mult,sop1")
   (set_attr "length" "24,4")])

(define_insn "movdi_symbol"
 [(set (match_operand:DI 0 "nonimmediate_operand" "=Sg")
       (match_operand:DI 1 "general_operand" "Y"))
  (clobber (reg:BI SCC_REG))]
 "GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == LABEL_REF"
  {
    if (SYMBOL_REF_P (operands[1])
	&& SYMBOL_REF_WEAK (operands[1]))
	return "s_getpc_b64\t%0\;"
	       "s_add_u32\t%L0, %L0, %1@gotpcrel32@lo+4\;"
	       "s_addc_u32\t%H0, %H0, %1@gotpcrel32@hi+4\;"
	       "s_load_dwordx2\t%0, %0\;"
	       "s_waitcnt\tlgkmcnt(0)";

    return "s_getpc_b64\t%0\;"
	   "s_add_u32\t%L0, %L0, %1@rel32@lo+4\;"
	   "s_addc_u32\t%H0, %H0, %1@rel32@hi+4";
  }
 [(set_attr "type" "mult")
  (set_attr "length" "32")])

(define_insn "movdi_symbol_save_scc"
 [(set (match_operand:DI 0 "nonimmediate_operand" "=Sg")
       (match_operand:DI 1 "general_operand" "Y"))
  (clobber (reg:BI CC_SAVE_REG))]
 "(GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == LABEL_REF)
  && (lra_in_progress || reload_completed)"
  {
    /* !!! These sequences clobber CC_SAVE_REG.  */

    if (SYMBOL_REF_P (operands[1])
	&& SYMBOL_REF_WEAK (operands[1]))
	return "; s_mov_b32\ts22, scc is not supported by the assembler.\;"
	       ".long\t0xbe9600fd\;"
	       "s_getpc_b64\t%0\;"
	       "s_add_u32\t%L0, %L0, %1@gotpcrel32@lo+4\;"
	       "s_addc_u32\t%H0, %H0, %1@gotpcrel32@hi+4\;"
	       "s_load_dwordx2\t%0, %0\;"
	       "s_cmpk_lg_u32\ts22, 0\;"
	       "s_waitcnt\tlgkmcnt(0)";

    return "; s_mov_b32\ts22, scc is not supported by the assembler.\;"
	   ".long\t0xbe9600fd\;"
	   "s_getpc_b64\t%0\;"
	   "s_add_u32\t%L0, %L0, %1@rel32@lo+4\;"
	   "s_addc_u32\t%H0, %H0, %1@rel32@hi+4\;"
	   "s_cmpk_lg_u32\ts22, 0";
  }
 [(set_attr "type" "mult")
  (set_attr "length" "40")])


(define_insn "gcn_indirect_call"
  [(call (mem (match_operand:DI 0 "register_operand" "Sg"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI LR_REGNUM))
   (clobber (match_scratch:DI 2 "=X"))]
  ""
  "s_swappc_b64\ts[18:19], %0"
  [(set_attr "type" "sop1")
   (set_attr "length" "4")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (clobber (reg:DI LR_REGNUM))
	      (clobber (match_scratch:DI 3))])]
  ""
  {})

(define_insn "gcn_call_value"
  [(set (match_operand 0 "register_operand" "=Sg,Sg")
	(call (mem (match_operand 1 "immediate_operand" "Y,B"))
	      (match_operand 2 "const_int_operand")))
   (clobber (reg:DI LR_REGNUM))
   (clobber (match_scratch:DI 3 "=&Sg,X"))]
  ""
  "@
  s_getpc_b64\t%3\;s_add_u32\t%L3, %L3, %1@rel32@lo+4\;s_addc_u32\t%H3, %H3, %1@rel32@hi+4\;s_swappc_b64\ts[18:19], %3
  s_swappc_b64\ts[18:19], %1"
  [(set_attr "type" "sop1")
   (set_attr "length" "24")])

(define_insn "gcn_call_value_indirect"
  [(set (match_operand 0 "register_operand" "=Sg")
	(call (mem (match_operand:DI 1 "register_operand" "Sg"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI LR_REGNUM))
   (clobber (match_scratch:DI 3 "=X"))]
  ""
  "s_swappc_b64\ts[18:19], %1"
  [(set_attr "type" "sop1")
   (set_attr "length" "4")])

; GCN does not have an instruction to clear only part of the instruction
; cache, so the operands are ignored.

(define_insn "clear_icache"
  [(unspec_volatile
    [(match_operand 0 "") (match_operand 1 "")]
    UNSPECV_ICACHE_INV)]
  ""
  "s_icache_inv"
  [(set_attr "type" "sopp")
   (set_attr "length" "4")])

;; }}}
;; {{{ Conditionals

; 32-bit compare, scalar unit only

(define_insn "cstoresi4"
  [(set (match_operand:BI 0 "gcn_conditional_register_operand"
							 "=cs, cs, cs, cs")
	(match_operator:BI 1 "gcn_compare_operator"
	  [(match_operand:SI 2 "gcn_alu_operand"	 "SSA,SSA,SSB, SS")
	   (match_operand:SI 3 "gcn_alu_operand"	 "SSA,SSL, SS,SSB")]))]
  ""
  "@
   s_cmp%D1\t%2, %3
   s_cmpk%D1\t%2, %3
   s_cmp%D1\t%2, %3
   s_cmp%D1\t%2, %3"
  [(set_attr "type" "sopc,sopk,sopk,sopk")
   (set_attr "length" "4,4,8,8")])

(define_expand "cbranchsi4"
  [(match_operator 0 "gcn_compare_operator"
     [(match_operand:SI 1 "gcn_alu_operand")
      (match_operand:SI 2 "gcn_alu_operand")])
   (match_operand 3)]
  ""
  {
    rtx cc = gen_reg_rtx (BImode);
    emit_insn (gen_cstoresi4 (cc, operands[0], operands[1], operands[2]));
    emit_jump_insn (gen_cjump (operands[3],
			       gen_rtx_NE (BImode, cc, const0_rtx), cc));
    DONE;
  })

; 64-bit compare; either unit, but scalar allows limited operators

(define_expand "cstoredi4"
  [(set (match_operand:BI 0 "gcn_conditional_register_operand")
	(match_operator:BI 1 "gcn_compare_operator"
			   [(match_operand:DI 2 "gcn_alu_operand")
			    (match_operand:DI 3 "gcn_alu_operand")]))]
  ""
  {})

(define_insn "cstoredi4_vec_and_scalar"
  [(set (match_operand:BI 0 "gcn_conditional_register_operand" "= cs,  cV")
	(match_operator:BI 1 "gcn_compare_64bit_operator"
	  [(match_operand:DI 2 "gcn_alu_operand"	       "%SSA,vSvC")
	   (match_operand:DI 3 "gcn_alu_operand"	       " SSC,   v")]))]
  ""
  "@
   s_cmp%D1\t%2, %3
   v_cmp%E1\tvcc, %2, %3"
  [(set_attr "type" "sopc,vopc")
   (set_attr "length" "8")])

(define_insn "cstoredi4_vector"
  [(set (match_operand:BI 0 "gcn_conditional_register_operand" "= cV")
	(match_operator:BI 1 "gcn_compare_operator"
          [(match_operand:DI 2 "gcn_alu_operand"	       "vSvB")
	   (match_operand:DI 3 "gcn_alu_operand"	       "   v")]))]
  ""
  "v_cmp%E1\tvcc, %2, %3"
  [(set_attr "type" "vopc")
   (set_attr "length" "8")])

(define_expand "cbranchdi4"
  [(match_operator 0 "gcn_compare_operator"
     [(match_operand:DI 1 "gcn_alu_operand")
      (match_operand:DI 2 "gcn_alu_operand")])
   (match_operand 3)]
  ""
  {
    rtx cc = gen_reg_rtx (BImode);
    emit_insn (gen_cstoredi4 (cc, operands[0], operands[1], operands[2]));
    emit_jump_insn (gen_cjump (operands[3],
			       gen_rtx_NE (BImode, cc, const0_rtx), cc));
    DONE;
  })

; FP compare; vector unit only

(define_insn "cstore<mode>4"
  [(set (match_operand:BI 0 "gcn_conditional_register_operand" "=cV")
	(match_operator:BI 1 "gcn_fp_compare_operator"
	  [(match_operand:SFDF 2 "gcn_alu_operand"		"vB")
	   (match_operand:SFDF 3 "gcn_alu_operand"		 "v")]))]
  ""
  "v_cmp%E1\tvcc, %2, %3"
  [(set_attr "type" "vopc")
   (set_attr "length" "8")])

(define_expand "cbranch<mode>4"
  [(match_operator 0 "gcn_fp_compare_operator"
     [(match_operand:SFDF 1 "gcn_alu_operand")
      (match_operand:SFDF 2 "gcn_alu_operand")])
   (match_operand 3)]
  ""
  {
    rtx cc = gen_reg_rtx (BImode);
    emit_insn (gen_cstore<mode>4 (cc, operands[0], operands[1], operands[2]));
    emit_jump_insn (gen_cjump (operands[3],
			       gen_rtx_NE (BImode, cc, const0_rtx), cc));
    DONE;
  })

;; }}}
;; {{{ ALU special cases: Plus

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"         "= Sg, Sg, Sg,   v")
        (plus:SI (match_operand:SI 1 "gcn_alu_operand" "%SgA,  0,SgA,   v")
		 (match_operand:SI 2 "gcn_alu_operand" " SgA,SgJ,  B,vBSv")))
   (clobber (match_scratch:BI 3			       "= cs, cs, cs,   X"))
   (clobber (match_scratch:DI 4			       "=  X,  X,  X,  cV"))]
  ""
  "@
   s_add_i32\t%0, %1, %2
   s_addk_i32\t%0, %2
   s_add_i32\t%0, %1, %2
   v_add%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "sop2,sopk,sop2,vop2")
   (set_attr "length" "4,4,8,8")])

(define_expand "addsi3_scc"
  [(parallel [(set (match_operand:SI 0 "register_operand")
		   (plus:SI (match_operand:SI 1 "gcn_alu_operand")
			    (match_operand:SI 2 "gcn_alu_operand")))
	      (clobber (reg:BI SCC_REG))
	      (clobber (scratch:DI))])]
  ""
  {})

; Having this as an insn_and_split allows us to keep together DImode adds
; through some RTL optimisation passes, and means the CC reg we set isn't
; dependent on the constraint alternative (which doesn't seem to work well).

; If v_addc_u32 is used to add with carry, a 32-bit literal constant cannot be
; used as an operand due to the read of VCC, so we restrict constants to the
; inlinable range for that alternative.

(define_insn_and_split "adddi3"
  [(set (match_operand:DI 0 "register_operand"		 "=Sg, v")
	(plus:DI (match_operand:DI 1 "register_operand"  " Sg, v")
		 (match_operand:DI 2 "nonmemory_operand" "SgB,vA")))
   (clobber (match_scratch:BI 3				 "=cs, X"))
   (clobber (match_scratch:DI 4				 "= X,cV"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    rtx cc = gen_rtx_REG (BImode, gcn_vgpr_register_operand (operands[1],
							     DImode)
			  ? VCC_REG : SCC_REG);

    emit_insn (gen_addsi3_scalar_carry
	       (gcn_operand_part (DImode, operands[0], 0),
		gcn_operand_part (DImode, operands[1], 0),
		gcn_operand_part (DImode, operands[2], 0),
		cc));
    rtx val = gcn_operand_part (DImode, operands[2], 1);
    if (val != const0_rtx)
      emit_insn (gen_addcsi3_scalar
		 (gcn_operand_part (DImode, operands[0], 1),
		  gcn_operand_part (DImode, operands[1], 1),
		  gcn_operand_part (DImode, operands[2], 1),
		  cc, cc));
    else
      emit_insn (gen_addcsi3_scalar_zero
		 (gcn_operand_part (DImode, operands[0], 1),
		  gcn_operand_part (DImode, operands[1], 1),
		  cc));
    DONE;
  }
  [(set_attr "type" "mult,vmult")
   (set_attr "length" "8")])

(define_expand "adddi3_scc"
  [(parallel [(set (match_operand:DI 0 "register_operand")
		   (plus:DI (match_operand:DI 1 "register_operand")
			    (match_operand:DI 2 "nonmemory_operand")))
	      (clobber (reg:BI SCC_REG))
	      (clobber (scratch:DI))])]
  ""
  {})

;; Add with carry.

(define_insn "addsi3_scalar_carry"
  [(set (match_operand:SI 0 "register_operand"	       "= Sg, v")
	(plus:SI (match_operand:SI 1 "gcn_alu_operand" "%SgA, v")
		 (match_operand:SI 2 "gcn_alu_operand" " SgB,vB")))
   (set (match_operand:BI 3 "register_operand"	       "= cs,cV")
	(ltu:BI (plus:SI (match_dup 1)
			 (match_dup 2))
		(match_dup 1)))]
  ""
  "@
   s_add_u32\t%0, %1, %2
   v_add%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "sop2,vop2")
   (set_attr "length" "8,8")])

(define_insn "addsi3_scalar_carry_cst"
  [(set (match_operand:SI 0 "register_operand"           "=Sg, v")
        (plus:SI (match_operand:SI 1 "gcn_alu_operand"   "SgA, v")
		 (match_operand:SI 2 "const_int_operand" "  n, n")))
   (set (match_operand:BI 4 "register_operand"           "=cs,cV")
	(geu:BI (plus:SI (match_dup 1)
			 (match_dup 2))
		(match_operand:SI 3 "const_int_operand"  "  n, n")))]
  "INTVAL (operands[2]) == -INTVAL (operands[3])"
  "@
   s_add_u32\t%0, %1, %2
   v_add%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "sop2,vop2")
   (set_attr "length" "4")])

(define_insn "addcsi3_scalar"
  [(set (match_operand:SI 0 "register_operand"			   "= Sg, v")
	(plus:SI (plus:SI (zero_extend:SI
			    (match_operand:BI 3 "register_operand" "= cs,cV"))
			  (match_operand:SI 1 "gcn_alu_operand"    "%SgA, v"))
		 (match_operand:SI 2 "gcn_alu_operand"		   " SgB,vA")))
   (set (match_operand:BI 4 "register_operand"			   "=  3, 3")
	(ior:BI (ltu:BI (plus:SI
			  (plus:SI
			    (zero_extend:SI (match_dup 3))
			    (match_dup 1))
			  (match_dup 2))
			(match_dup 2))
		(ltu:BI (plus:SI (zero_extend:SI (match_dup 3)) (match_dup 1))
			(match_dup 1))))]
  ""
  "@
   s_addc_u32\t%0, %1, %2
   v_addc%^_u32\t%0, vcc, %2, %1, vcc"
  [(set_attr "type" "sop2,vop2")
   (set_attr "length" "8,4")])

(define_insn "addcsi3_scalar_zero"
  [(set (match_operand:SI 0 "register_operand"		  "=Sg, v")
        (plus:SI (zero_extend:SI
		   (match_operand:BI 2 "register_operand" "=cs,cV"))
		 (match_operand:SI 1 "gcn_alu_operand"    "SgA, v")))
   (set (match_dup 2)
	(ltu:BI (plus:SI (zero_extend:SI (match_dup 2))
			 (match_dup 1))
		(match_dup 1)))]
  ""
  "@
   s_addc_u32\t%0, %1, 0
   v_addc%^_u32\t%0, vcc, 0, %1, vcc"
  [(set_attr "type" "sop2,vop2")
   (set_attr "length" "4")])

; "addptr" is the same as "add" except that it must not write to VCC or SCC
; as a side-effect.  Unfortunately GCN does not have a suitable instruction
; for this, so we use a custom VOP3 add with CC_SAVE_REG as a temp.
; Note that it is not safe to save/clobber/restore SCC because doing so will
; break data-flow analysis, so this must use vector registers.
;
; The "v0" should be just "v", but somehow the "0" helps LRA not loop forever
; on testcase pr54713-2.c with -O0. It's only an optimization hint anyway.

(define_insn "addptrdi3"
  [(set (match_operand:DI 0 "register_operand"		 "= v")
	(plus:DI (match_operand:DI 1 "register_operand"	 " v0")
		 (match_operand:DI 2 "nonmemory_operand" "vDA")))]
  ""
  {
    rtx new_operands[4] = { operands[0], operands[1], operands[2],
			    gen_rtx_REG (DImode, CC_SAVE_REG) };

    output_asm_insn ("v_add%^_u32 %L0, %3, %L2, %L1", new_operands);
    output_asm_insn ("v_addc%^_u32 %H0, %3, %H2, %H1, %3", new_operands);

    return "";
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

;; }}}
;; {{{ ALU special cases: Minus

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"          "=Sg, Sg,    v,   v")
	(minus:SI (match_operand:SI 1 "gcn_alu_operand" "SgA,SgA,    v,vBSv")
		  (match_operand:SI 2 "gcn_alu_operand" "SgA,  B, vBSv,   v")))
   (clobber (match_scratch:BI 3				"=cs, cs,    X,   X"))
   (clobber (match_scratch:DI 4				"= X,  X,   cV,  cV"))]
  ""
  "@
   s_sub_i32\t%0, %1, %2
   s_sub_i32\t%0, %1, %2
   v_subrev%^_u32\t%0, vcc, %2, %1
   v_sub%^_u32\t%0, vcc, %1, %2"
  [(set_attr "type" "sop2,sop2,vop2,vop2")
   (set_attr "length" "4,8,8,8")])

(define_insn_and_split "subdi3"
  [(set (match_operand:DI 0 "register_operand"        "=Sg, Sg")
	(minus:DI
		(match_operand:DI 1 "gcn_alu_operand" "SgA,SgB")
		(match_operand:DI 2 "gcn_alu_operand" "SgB,SgA")))
   (clobber (reg:BI SCC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    emit_insn (gen_subsi3_scalar_carry
	       (gcn_operand_part (DImode, operands[0], 0),
		gcn_operand_part (DImode, operands[1], 0),
		gcn_operand_part (DImode, operands[2], 0)));
    rtx val = gcn_operand_part (DImode, operands[2], 1);
    if (val != const0_rtx)
      emit_insn (gen_subcsi3_scalar
		 (gcn_operand_part (DImode, operands[0], 1),
		  gcn_operand_part (DImode, operands[1], 1),
		  gcn_operand_part (DImode, operands[2], 1)));
    else
      emit_insn (gen_subcsi3_scalar_zero
		 (gcn_operand_part (DImode, operands[0], 1),
		  gcn_operand_part (DImode, operands[1], 1)));
    DONE;
  }
  [(set_attr "length" "8")])

(define_insn "subsi3_scalar_carry"
  [(set (match_operand:SI 0 "register_operand"          "=Sg, Sg")
        (minus:SI (match_operand:SI 1 "gcn_alu_operand" "SgA,SgB")
		  (match_operand:SI 2 "gcn_alu_operand" "SgB,SgA")))
   (set (reg:BI SCC_REG)
	(gtu:BI (minus:SI (match_dup 1)
			  (match_dup 2))
		(match_dup 1)))]
  ""
  "s_sub_u32\t%0, %1, %2"
  [(set_attr "type" "sop2")
   (set_attr "length" "8")])

(define_insn "subsi3_scalar_carry_cst"
  [(set (match_operand:SI 0 "register_operand"           "=Sg")
        (minus:SI (match_operand:SI 1 "gcn_alu_operand"  "SgA")
		 (match_operand:SI 2 "const_int_operand" "  n")))
   (set (reg:BI SCC_REG)
	(leu:BI (minus:SI (match_dup 1)
			 (match_dup 2))
		(match_operand:SI 3 "const_int_operand"  "  n")))]
  "INTVAL (operands[2]) == -INTVAL (operands[3])"
  "s_sub_u32\t%0, %1, %2"
  [(set_attr "type" "sop2")
   (set_attr "length" "4")])

(define_insn "subcsi3_scalar"
  [(set (match_operand:SI 0 "register_operand"                    "=Sg, Sg")
        (minus:SI (minus:SI (zero_extend:SI (reg:BI SCC_REG))
			    (match_operand:SI 1 "gcn_alu_operand" "SgA,SgB"))
		 (match_operand:SI 2 "gcn_alu_operand"            "SgB,SgA")))
   (set (reg:BI SCC_REG)
	(ior:BI (gtu:BI (minus:SI (minus:SI (zero_extend:SI (reg:BI SCC_REG))
					    (match_dup 1))
				 (match_dup 2))
			(match_dup 1))
		(gtu:BI (minus:SI (zero_extend:SI (reg:BI SCC_REG))
				  (match_dup 1))
			(match_dup 1))))]
  ""
  "s_subb_u32\t%0, %1, %2"
  [(set_attr "type" "sop2")
   (set_attr "length" "8")])

(define_insn "subcsi3_scalar_zero"
  [(set (match_operand:SI 0 "register_operand"		"=Sg")
        (minus:SI (zero_extend:SI (reg:BI SCC_REG))
		  (match_operand:SI 1 "gcn_alu_operand" "SgA")))
   (set (reg:BI SCC_REG)
	(gtu:BI (minus:SI (zero_extend:SI (reg:BI SCC_REG)) (match_dup 1))
		(match_dup 1)))]
  ""
  "s_subb_u32\t%0, %1, 0"
  [(set_attr "type" "sop2")
   (set_attr "length" "4")])

;; }}}
;; {{{ ALU: mult

; Vector multiply has vop3a encoding, but no corresponding vop2a, so no long
; immediate.
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"	       "= Sg,Sg, Sg,   v")
        (mult:SI (match_operand:SI 1 "gcn_alu_operand" "%SgA, 0,SgA,   v")
		 (match_operand:SI 2 "gcn_alu_operand" " SgA, J,  B,vASv")))]
  ""
  "@
   s_mul_i32\t%0, %1, %2
   s_mulk_i32\t%0, %2
   s_mul_i32\t%0, %1, %2
   v_mul_lo_i32\t%0, %1, %2"
  [(set_attr "type" "sop2,sopk,sop2,vop3a")
   (set_attr "length" "4,4,8,4")])

(define_code_iterator any_extend [sign_extend zero_extend])
(define_code_attr sgnsuffix [(sign_extend "%i") (zero_extend "%u")])
(define_code_attr su [(sign_extend "s") (zero_extend "u")])
(define_code_attr u [(sign_extend "") (zero_extend "u")])
(define_code_attr iu [(sign_extend "i") (zero_extend "u")])
(define_code_attr e [(sign_extend "e") (zero_extend "")])

(define_insn "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"	       "= v")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI
	      (any_extend:DI
		(match_operand:SI 1 "register_operand" "% v"))
	      (any_extend:DI
		(match_operand:SI 2 "register_operand" "vSv")))
	    (const_int 32))))]
  ""
  "v_mul_hi<sgnsuffix>0\t%0, %2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "<u>mulhisi3"
  [(set (match_operand:SI 0 "register_operand"			"=v")
	(mult:SI
	  (any_extend:SI (match_operand:HI 1 "register_operand" "%v"))
	  (any_extend:SI (match_operand:HI 2 "register_operand" " v"))))]
  ""
  "v_mul_<iu>32_<iu>24_sdwa\t%0, %<e>1, %<e>2 src0_sel:WORD_0 src1_sel:WORD_0"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

(define_insn "<u>mulqihi3_scalar"
  [(set (match_operand:HI 0 "register_operand"			"=v")
	(mult:HI
	  (any_extend:HI (match_operand:QI 1 "register_operand" "%v"))
	  (any_extend:HI (match_operand:QI 2 "register_operand" " v"))))]
  ""
  "v_mul_<iu>32_<iu>24_sdwa\t%0, %<e>1, %<e>2 src0_sel:BYTE_0 src1_sel:BYTE_0"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

;; }}}
;; {{{ ALU: generic 32-bit unop

(define_code_iterator bitunop [not popcount])
(define_code_attr popcount_extra_op [(not "") (popcount ", 0")])

(define_insn "<expander>si2"
  [(set (match_operand:SI 0 "register_operand"  "=Sg,   v")
        (bitunop:SI
	  (match_operand:SI 1 "gcn_alu_operand" "SgB,vSvB")))
   (clobber (match_scratch:BI 2			"=cs,   X"))]
  ""
  "@
   s_<s_mnemonic>0\t%0, %1
   v_<mnemonic>0\t%0, %1<popcount_extra_op>"
  [(set_attr "type" "sop1,vop1")
   (set_attr "length" "8")])

(define_code_iterator countzeros [clz ctz])

(define_insn "<expander>si2"
  [(set (match_operand:SI 0 "register_operand"  "=Sg,Sg")
        (countzeros:SI
	  (match_operand:SI 1 "gcn_alu_operand" "SgA, B")))]
  ""
  "s_<s_mnemonic>1\t%0, %1"
  [(set_attr "type" "sop1")
   (set_attr "length" "4,8")])

; The truncate ensures that a constant passed to operand 1 is treated as DImode
(define_insn "<expander>di2"
  [(set (match_operand:SI 0 "register_operand"    "=Sg,Sg")
	(truncate:SI
	  (countzeros:DI
	    (match_operand:DI 1 "gcn_alu_operand" "SgA, B"))))]
  ""
  "s_<s_mnemonic>1\t%0, %1"
  [(set_attr "type" "sop1")
   (set_attr "length" "4,8")])

;; }}}
;; {{{ ALU: generic 32-bit binop

; No plus and mult - they have variant with 16bit immediate
; and thus are defined later.
(define_code_iterator binop [and ior xor smin smax umin umax
				 ashift lshiftrt ashiftrt])
(define_code_iterator vec_and_scalar_com [and ior xor smin smax umin umax])
(define_code_iterator vec_and_scalar_nocom [ashift lshiftrt ashiftrt])

(define_insn "<expander>si3"
  [(set (match_operand:SI 0 "gcn_valu_dst_operand"    "= Sg,   v,RD")
        (vec_and_scalar_com:SI
	  (match_operand:SI 1 "gcn_valu_src0_operand" "%SgA,vSvB, 0")
	  (match_operand:SI 2 "gcn_alu_operand"	      " SgB,   v, v")))
   (clobber (match_scratch:BI 3			      "= cs,   X, X"))]
  ""
  "@
   s_<mnemonic>0\t%0, %1, %2
   v_<mnemonic>0\t%0, %1, %2
   ds_<mnemonic>0\t%A0, %2%O0"
  [(set_attr "type" "sop2,vop2,ds")
   (set_attr "length" "8")])

(define_insn "<expander>si3"
  [(set (match_operand:SI 0 "register_operand"  "=Sg, Sg,   v")
        (vec_and_scalar_nocom:SI
	  (match_operand:SI 1 "gcn_alu_operand" "SgB,SgA,   v")
	  (match_operand:SI 2 "gcn_alu_operand" "SgA,SgB,vSvB")))
   (clobber (match_scratch:BI 3			"=cs, cs,   X"))]
  ""
  "@
   s_<mnemonic>0\t%0, %1, %2
   s_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "sop2,sop2,vop2")
   (set_attr "length" "8")])

(define_expand "<expander>si3_scc"
  [(parallel [(set (match_operand:SI 0 "gcn_valu_dst_operand")
		   (binop:SI
		     (match_operand:SI 1 "gcn_valu_src0_operand")
		     (match_operand:SI 2 "gcn_alu_operand")))
	      (clobber (reg:BI SCC_REG))])]
  ""
  {})

;; }}}
;; {{{ ALU: generic 64-bit

(define_code_iterator vec_and_scalar64_com [and ior xor])

(define_insn_and_split "<expander>di3"
   [(set (match_operand:DI 0 "register_operand"  "= Sg,    v")
	 (vec_and_scalar64_com:DI
	  (match_operand:DI 1 "gcn_alu_operand"  "%SgA,vSvDB")
	   (match_operand:DI 2 "gcn_alu_operand" " SgC,    v")))
   (clobber (match_scratch:BI 3			 "= cs,    X"))]
  ""
  "@
   s_<mnemonic>0\t%0, %1, %2
   #"
  "reload_completed && gcn_vgpr_register_operand (operands[0], DImode)"
  [(parallel [(set (match_dup 4)
		   (vec_and_scalar64_com:SI (match_dup 5) (match_dup 6)))
	      (clobber (match_dup 3))])
   (parallel [(set (match_dup 7)
		   (vec_and_scalar64_com:SI (match_dup 8) (match_dup 9)))
	      (clobber (match_dup 3))])]
  {
    operands[4] = gcn_operand_part (DImode, operands[0], 0);
    operands[5] = gcn_operand_part (DImode, operands[1], 0);
    operands[6] = gcn_operand_part (DImode, operands[2], 0);
    operands[7] = gcn_operand_part (DImode, operands[0], 1);
    operands[8] = gcn_operand_part (DImode, operands[1], 1);
    operands[9] = gcn_operand_part (DImode, operands[2], 1);
  }
  [(set_attr "type" "sop2,vop2")
   (set_attr "length" "8")])

(define_insn "<expander>di3"
  [(set (match_operand:DI 0 "register_operand"   "=Sg, Sg,   v")
	(vec_and_scalar_nocom:DI
	  (match_operand:DI 1 "gcn_alu_operand"  "SgC,SgA,   v")
	  (match_operand:SI 2 "gcn_alu_operand"  "SgA,SgC,vSvC")))
   (clobber (match_scratch:BI 3			 "=cs, cs,   X"))]
  ""
  "@
   s_<mnemonic>0\t%0, %1, %2
   s_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "sop2,sop2,vop2")
   (set_attr "length" "8")])

;; }}}
;; {{{ Atomics

; Each compute unit has it's own L1 cache. The L2 cache is shared between
; all the compute units.  Any load or store instruction can skip L1 and
; access L2 directly using the "glc" flag.  Atomic instructions also skip
; L1.  The L1 cache can be flushed and invalidated using instructions.
;
; Therefore, in order for "acquire" and "release" atomic modes to work
; correctly across compute units we must flush before each "release"
; and invalidate the cache after each "acquire".  It might seem like
; invalidation could be safely done before an "acquire", but since each
; compute unit can run up to 40 threads simultaneously, all reading values
; into the L1 cache, this is not actually safe.
;
; Additionally, scalar flat instructions access L2 via a different cache
; (the "constant cache"), so they have separate constrol instructions.  We
; do not attempt to invalidate both caches at once; instead, atomics
; operating on scalar flat pointers will flush the constant cache, and
; atomics operating on flat or global pointers will flush L1.  It is up to
; the programmer to get this right.

(define_code_iterator atomicops [plus minus and ior xor])
(define_mode_attr X [(SI "") (DI "_X2")])

;; TODO compare_and_swap test_and_set inc dec
;; Hardware also supports min and max, but GCC does not.

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
  {
    operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[0]) = 1;
  })

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
  "buffer_wbinvl1_vol"
  [(set_attr "type" "mubuf")
   (set_attr "length" "4")])

; FIXME: These patterns have been disabled as they do not seem to work
; reliably - they can cause hangs or incorrect results.
; TODO: flush caches according to memory model
(define_insn "atomic_fetch_<bare_mnemonic><mode>"
  [(set (match_operand:SIDI 0 "register_operand"     "=Sm, v, v")
	(match_operand:SIDI 1 "memory_operand"	     "+RS,RF,RM"))
   (set (match_dup 1)
	(unspec_volatile:SIDI
	  [(atomicops:SIDI
	    (match_dup 1)
	    (match_operand:SIDI 2 "register_operand" " Sm, v, v"))]
	   UNSPECV_ATOMIC))
   (use (match_operand 3 "const_int_operand"))]
  "0 /* Disabled.  */"
  "@
   s_atomic_<bare_mnemonic><X>\t%0, %1, %2 glc\;s_waitcnt\tlgkmcnt(0)
   flat_atomic_<bare_mnemonic><X>\t%0, %1, %2 glc\;s_waitcnt\t0
   global_atomic_<bare_mnemonic><X>\t%0, %A1, %2%O1 glc\;s_waitcnt\tvmcnt(0)"
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "gcn5,*,gcn5")])

; FIXME: These patterns are disabled because the instructions don't
; seem to work as advertised.  Specifically, OMP "team distribute"
; reductions apparently "lose" some of the writes, similar to what
; you might expect from a concurrent non-atomic read-modify-write.
; TODO: flush caches according to memory model
(define_insn "atomic_<bare_mnemonic><mode>"
  [(set (match_operand:SIDI 0 "memory_operand"       "+RS,RF,RM")
	(unspec_volatile:SIDI
	  [(atomicops:SIDI
	    (match_dup 0)
	    (match_operand:SIDI 1 "register_operand" " Sm, v, v"))]
	  UNSPECV_ATOMIC))
   (use (match_operand 2 "const_int_operand"))]
  "0 /* Disabled.  */"
  "@
   s_atomic_<bare_mnemonic><X>\t%0, %1\;s_waitcnt\tlgkmcnt(0)
   flat_atomic_<bare_mnemonic><X>\t%0, %1\;s_waitcnt\t0
   global_atomic_<bare_mnemonic><X>\t%A0, %1%O0\;s_waitcnt\tvmcnt(0)"
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "gcn5,*,gcn5")])

(define_mode_attr x2 [(SI "DI") (DI "TI")])
(define_mode_attr size [(SI "4") (DI "8")])
(define_mode_attr bitsize [(SI "32") (DI "64")])

(define_expand "sync_compare_and_swap<mode>"
  [(match_operand:SIDI 0 "register_operand")
   (match_operand:SIDI 1 "memory_operand")
   (match_operand:SIDI 2 "register_operand")
   (match_operand:SIDI 3 "register_operand")]
  ""
  {
    if (MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_LDS)
      {
	emit_insn (gen_sync_compare_and_swap<mode>_lds_insn (operands[0],
							     operands[1],
							     operands[2],
							     operands[3]));
	DONE;
      }

    /* Operands 2 and 3 must be placed in consecutive registers, and passed
       as a combined value.  */
    rtx src_cmp = gen_reg_rtx (<x2>mode);
    emit_move_insn (gen_rtx_SUBREG (<MODE>mode, src_cmp, 0), operands[3]);
    emit_move_insn (gen_rtx_SUBREG (<MODE>mode, src_cmp, <size>), operands[2]);
    emit_insn (gen_sync_compare_and_swap<mode>_insn (operands[0],
						     operands[1],
						     src_cmp));
    DONE;
  })

(define_insn "sync_compare_and_swap<mode>_insn"
  [(set (match_operand:SIDI 0 "register_operand"    "=Sm, v, v")
	(match_operand:SIDI 1 "memory_operand"      "+RS,RF,RM"))
   (set (match_dup 1)
	(unspec_volatile:SIDI
	  [(match_operand:<x2> 2 "register_operand" " Sm, v, v")]
	  UNSPECV_ATOMIC))]
  ""
  "@
   s_atomic_cmpswap<X>\t%0, %1, %2 glc\;s_waitcnt\tlgkmcnt(0)
   flat_atomic_cmpswap<X>\t%0, %1, %2 glc\;s_waitcnt\t0
   global_atomic_cmpswap<X>\t%0, %A1, %2%O1 glc\;s_waitcnt\tvmcnt(0)"
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "gcn5,*,gcn5")
   (set_attr "delayeduse" "*,yes,yes")])

(define_insn "sync_compare_and_swap<mode>_lds_insn"
  [(set (match_operand:SIDI 0 "register_operand"    "= v")
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 1 "memory_operand"   "+RL")]
	  UNSPECV_ATOMIC))
   (set (match_dup 1)
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 2 "register_operand" "  v")
	   (match_operand:SIDI 3 "register_operand" "  v")]
	  UNSPECV_ATOMIC))]
  ""
  "ds_cmpst_rtn_b<bitsize> %0, %1, %2, %3\;s_waitcnt\tlgkmcnt(0)"
  [(set_attr "type" "ds")
   (set_attr "length" "12")])

(define_insn "atomic_load<mode>"
  [(set (match_operand:SIDI 0 "register_operand"  "=Sm, v, v")
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 1 "memory_operand" " RS,RF,RM")]
	  UNSPECV_ATOMIC))
   (use (match_operand:SIDI 2 "immediate_operand" "  i, i, i"))]
  ""
  {
    switch (INTVAL (operands[2]))
      {
      case MEMMODEL_RELAXED:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_load%o0\t%0, %A1 glc\;s_waitcnt\tlgkmcnt(0)";
	  case 1:
	    return "flat_load%o0\t%0, %A1%O1 glc\;s_waitcnt\t0";
	  case 2:
	    return "global_load%o0\t%0, %A1%O1 glc\;s_waitcnt\tvmcnt(0)";
	  }
	break;
      case MEMMODEL_CONSUME:
      case MEMMODEL_ACQUIRE:
      case MEMMODEL_SYNC_ACQUIRE:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_load%o0\t%0, %A1 glc\;s_waitcnt\tlgkmcnt(0)\;"
		   "s_dcache_wb_vol";
	  case 1:
	    return "flat_load%o0\t%0, %A1%O1 glc\;s_waitcnt\t0\;"
		   "buffer_wbinvl1_vol";
	  case 2:
	    return "global_load%o0\t%0, %A1%O1 glc\;s_waitcnt\tvmcnt(0)\;"
		   "buffer_wbinvl1_vol";
	  }
	break;
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_SYNC_SEQ_CST:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_dcache_wb_vol\;s_load%o0\t%0, %A1 glc\;"
		   "s_waitcnt\tlgkmcnt(0)\;s_dcache_inv_vol";
	  case 1:
	    return "buffer_wbinvl1_vol\;flat_load%o0\t%0, %A1%O1 glc\;"
		   "s_waitcnt\t0\;buffer_wbinvl1_vol";
	  case 2:
	    return "buffer_wbinvl1_vol\;global_load%o0\t%0, %A1%O1 glc\;"
		   "s_waitcnt\tvmcnt(0)\;buffer_wbinvl1_vol";
	  }
	break;
      }
    gcc_unreachable ();
  }
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "20")
   (set_attr "gcn_version" "gcn5,*,gcn5")])

(define_insn "atomic_store<mode>"
  [(set (match_operand:SIDI 0 "memory_operand"      "=RS,RF,RM")
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 1 "register_operand" " Sm, v, v")]
	  UNSPECV_ATOMIC))
  (use (match_operand:SIDI 2 "immediate_operand"    "  i, i, i"))]
  ""
  {
    switch (INTVAL (operands[2]))
      {
      case MEMMODEL_RELAXED:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_store%o1\t%1, %A0 glc\;s_waitcnt\tlgkmcnt(0)";
	  case 1:
	    return "flat_store%o1\t%A0, %1%O0 glc\;s_waitcnt\t0";
	  case 2:
	    return "global_store%o1\t%A0, %1%O0 glc\;s_waitcnt\tvmcnt(0)";
	  }
	break;
      case MEMMODEL_RELEASE:
      case MEMMODEL_SYNC_RELEASE:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_dcache_wb_vol\;s_store%o1\t%1, %A0 glc";
	  case 1:
	    return "buffer_wbinvl1_vol\;flat_store%o1\t%A0, %1%O0 glc";
	  case 2:
	    return "buffer_wbinvl1_vol\;global_store%o1\t%A0, %1%O0 glc";
	  }
	break;
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_SYNC_SEQ_CST:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_dcache_wb_vol\;s_store%o1\t%1, %A0 glc\;"
		   "s_waitcnt\tlgkmcnt(0)\;s_dcache_inv_vol";
	  case 1:
	    return "buffer_wbinvl1_vol\;flat_store%o1\t%A0, %1%O0 glc\;"
		   "s_waitcnt\t0\;buffer_wbinvl1_vol";
	  case 2:
	    return "buffer_wbinvl1_vol\;global_store%o1\t%A0, %1%O0 glc\;"
		   "s_waitcnt\tvmcnt(0)\;buffer_wbinvl1_vol";
	  }
	break;
      }
    gcc_unreachable ();
  }
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "20")
   (set_attr "gcn_version" "gcn5,*,gcn5")])

(define_insn "atomic_exchange<mode>"
  [(set (match_operand:SIDI 0 "register_operand"    "=Sm, v, v")
        (match_operand:SIDI 1 "memory_operand"	    "+RS,RF,RM"))
   (set (match_dup 1)
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 2 "register_operand" " Sm, v, v")]
	  UNSPECV_ATOMIC))
   (use (match_operand 3 "immediate_operand"))]
  ""
  {
    switch (INTVAL (operands[3]))
      {
      case MEMMODEL_RELAXED:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_atomic_swap<X>\t%0, %1, %2 glc\;s_waitcnt\tlgkmcnt(0)";
	  case 1:
	    return "flat_atomic_swap<X>\t%0, %1, %2 glc\;s_waitcnt\t0";
	  case 2:
	    return "global_atomic_swap<X>\t%0, %A1, %2%O1 glc\;"
		   "s_waitcnt\tvmcnt(0)";
	  }
	break;
      case MEMMODEL_CONSUME:
      case MEMMODEL_ACQUIRE:
      case MEMMODEL_SYNC_ACQUIRE:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_atomic_swap<X>\t%0, %1, %2 glc\;s_waitcnt\tlgkmcnt(0)\;"
		   "s_dcache_wb_vol\;s_dcache_inv_vol";
	  case 1:
	    return "flat_atomic_swap<X>\t%0, %1, %2 glc\;s_waitcnt\t0\;"
		   "buffer_wbinvl1_vol";
	  case 2:
	    return "global_atomic_swap<X>\t%0, %A1, %2%O1 glc\;"
		   "s_waitcnt\tvmcnt(0)\;buffer_wbinvl1_vol";
	  }
	break;
      case MEMMODEL_RELEASE:
      case MEMMODEL_SYNC_RELEASE:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_dcache_wb_vol\;s_atomic_swap<X>\t%0, %1, %2 glc\;"
		   "s_waitcnt\tlgkmcnt(0)";
	  case 1:
	    return "buffer_wbinvl1_vol\;flat_atomic_swap<X>\t%0, %1, %2 glc\;"
		   "s_waitcnt\t0";
	  case 2:
	    return "buffer_wbinvl1_vol\;"
		   "global_atomic_swap<X>\t%0, %A1, %2%O1 glc\;"
		   "s_waitcnt\tvmcnt(0)";
	  }
	break;
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_SYNC_SEQ_CST:
	switch (which_alternative)
	  {
	  case 0:
	    return "s_dcache_wb_vol\;s_atomic_swap<X>\t%0, %1, %2 glc\;"
		   "s_waitcnt\tlgkmcnt(0)\;s_dcache_inv_vol";
	  case 1:
	    return "buffer_wbinvl1_vol\;flat_atomic_swap<X>\t%0, %1, %2 glc\;"
		   "s_waitcnt\t0\;buffer_wbinvl1_vol";
	  case 2:
	    return "buffer_wbinvl1_vol\;"
		   "global_atomic_swap<X>\t%0, %A1, %2%O1 glc\;"
		   "s_waitcnt\tvmcnt(0)\;buffer_wbinvl1_vol";
	  }
	break;
      }
    gcc_unreachable ();
  }
  [(set_attr "type" "smem,flat,flat")
   (set_attr "length" "20")
   (set_attr "gcn_version" "gcn5,*,gcn5")])

;; }}}
;; {{{ OpenACC / OpenMP

(define_expand "oacc_dim_size"
  [(match_operand:SI 0 "register_operand")
   (match_operand:SI 1 "const_int_operand")]
  ""
  {
    rtx tmp = gcn_oacc_dim_size (INTVAL (operands[1]));
    emit_move_insn (operands[0], gen_lowpart (SImode, tmp));
    DONE;
  })

(define_expand "oacc_dim_pos"
  [(match_operand:SI 0 "register_operand")
   (match_operand:SI 1 "const_int_operand")]
  ""
  {
    emit_move_insn (operands[0], gcn_oacc_dim_pos (INTVAL (operands[1])));
    DONE;
  })

(define_expand "gcn_wavefront_barrier"
  [(set (match_dup 0)
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_BARRIER))]
  ""
  {
    operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[0]) = 1;
  })

(define_insn "*gcn_wavefront_barrier"
  [(set (match_operand:BLK 0 "")
	(unspec_volatile:BLK [(match_dup 0)] UNSPECV_BARRIER))]
  ""
  "s_barrier"
  [(set_attr "type" "sopp")])

(define_expand "oacc_fork"
  [(set (match_operand:SI 0 "")
	(match_operand:SI 1 ""))
   (use (match_operand:SI 2 ""))]
  ""
  {
    /* We need to have oacc_fork/oacc_join named patterns as a pair,
       but the fork isn't actually used.  */
    gcc_unreachable ();
  })

(define_expand "oacc_join"
  [(set (match_operand:SI 0 "")
	(match_operand:SI 1 ""))
   (use (match_operand:SI 2 ""))]
  ""
  {
    emit_insn (gen_gcn_wavefront_barrier ());
    DONE;
  })

;; }}}

(include "gcn-valu.md")
