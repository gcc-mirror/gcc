;; Machine description for RISC-V for GNU compiler.
;; Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
  UNSPEC_FMIN
  UNSPEC_FMAX

  ;; Stack tie
  UNSPEC_TIE

  ;; OR-COMBINE
  UNSPEC_ORC_B
])

(define_c_enum "unspecv" [
  ;; Register save and restore.
  UNSPECV_GPR_SAVE
  UNSPECV_GPR_RESTORE

  ;; Floating-point unspecs.
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

  ;; XTheadFmv unspec
  UNSPEC_XTHEADFMV
  UNSPEC_XTHEADFMV_HW
])

(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (GP_REGNUM 			3)
   (TP_REGNUM			4)
   (T0_REGNUM			5)
   (T1_REGNUM			6)
   (S0_REGNUM			8)
   (S1_REGNUM			9)
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
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,HF,SF,DF,TF,
  VNx1BI,VNx2BI,VNx4BI,VNx8BI,VNx16BI,VNx32BI,VNx64BI,
  VNx1QI,VNx2QI,VNx4QI,VNx8QI,VNx16QI,VNx32QI,VNx64QI,
  VNx1HI,VNx2HI,VNx4HI,VNx8HI,VNx16HI,VNx32HI,
  VNx1SI,VNx2SI,VNx4SI,VNx8SI,VNx16SI,
  VNx1DI,VNx2DI,VNx4DI,VNx8DI,
  VNx1SF,VNx2SF,VNx4SF,VNx8SF,VNx16SF,
  VNx1DF,VNx2DF,VNx4DF,VNx8DF"
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

;; Attribute to control enable or disable instructions.
(define_attr "enabled" "no,yes"
  (cond [(eq_attr "ext_enabled" "no")
	 (const_string "no")]
	(const_string "yes")))

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
;; auipc	integer addition to PC
;; sfb_alu  SFB ALU instruction
;; nop		no operation
;; ghost	an instruction that produces no real code
;; bitmanip	bit manipulation instructions
;; rotate   rotation instructions
;; atomic   atomic instructions
;; condmove	conditional moves
;; crypto cryptography instructions
;; Classification of RVV instructions which will be added to each RVV .md pattern and used by scheduler.
;; rdvlenb     vector byte length vlenb csrr read
;; rdvl        vector length vl csrr read
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
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,store,fpstore,
   mtc,mfc,const,arith,logical,shift,slt,imul,idiv,move,fmove,fadd,fmul,
   fmadd,fdiv,fcmp,fcvt,fsqrt,multi,auipc,sfb_alu,nop,ghost,bitmanip,rotate,
   atomic,condmove,crypto,rdvlenb,rdvl,vsetvl,vlde,vste,vldm,vstm,vlds,vsts,
   vldux,vldox,vstux,vstox,vldff,vldr,vstr,
   vialu,viwalu,vext,vicalu,vshift,vnshift,vicmp,viminmax,
   vimul,vidiv,viwmul,vimuladd,viwmuladd,vimerge,vimov,
   vsalu,vaalu,vsmul,vsshift,vnclip,
   vfalu,vfwalu,vfmul,vfdiv,vfwmul,vfmuladd,vfwmuladd,vfsqrt,vfrecp,
   vfcmp,vfminmax,vfsgnj,vfclass,vfmerge,vfmov,
   vfcvtitof,vfcvtftoi,vfwcvtitof,vfwcvtftoi,
   vfwcvtftof,vfncvtitof,vfncvtftoi,vfncvtftof,
   vired,viwred,vfredu,vfredo,vfwredu,vfwredo,
   vmalu,vmpop,vmffs,vmsfs,vmiota,vmidx,vimovvx,vimovxv,vfmovvf,vfmovfv,
   vslideup,vslidedown,vislide1up,vislide1down,vfslide1up,vfslide1down,
   vgather,vcompress,vmov"
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

(define_insn "addsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" " r,r")
		 (match_operand:SI 2 "arith_operand"    " r,I")))]
  ""
  "add%i2%~\t%0,%1,%2"
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

      emit_insn (gen_addsi3 (operands[0], operands[1], operands[2]));
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
      emit_insn (gen_addsi3 (operands[0], operands[1], operands[2]));
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
	  (match_operator:SI 3 "subreg_lowpart_operator"
	     [(plus:DI (match_operand:DI 1 "register_operand" " r,r")
		       (match_operand:DI 2 "arith_operand"    " r,I"))])))]
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

(define_insn "subsi3"
  [(set (match_operand:SI           0 "register_operand" "= r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " rJ")
		  (match_operand:SI 2 "register_operand" "  r")))]
  ""
  "sub%~\t%0,%z1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

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

      emit_insn (gen_subsi3 (operands[0], operands[1], operands[2]));
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
      emit_insn (gen_subsi3 (operands[0], operands[1], operands[2]));
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

(define_insn "negsi2"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" " r")))]
  ""
  "neg%~\t%0,%1"
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

(define_insn "mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_ZMMUL || TARGET_MUL"
  "mul%~\t%0,%1,%2"
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

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
      emit_insn (gen_ashr<mode>3 (lp, operands[0],
				  GEN_INT (BITS_PER_WORD - 1)));

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

(define_insn "*mulsi3_extended"
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
  emit_insn (gen_mulsi3 (temp, operands[1], operands[2]));
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

(define_insn "<optab>si3"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(any_div:SI (match_operand:SI 1 "register_operand" " r")
		    (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_DIV"
  "<insn>%i2%~\t%0,%1,%2"
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

(define_insn_and_split "*zero_extendsidi2_internal"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(zero_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT && !TARGET_ZBA
   && !(register_operand (operands[1], SImode)
        && reg_or_subregno (operands[1]) == VL_REGNUM)"
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

(define_expand "zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand")
	(zero_extend:GPR
	    (match_operand:HI 1 "nonimmediate_operand")))]
  "")

(define_insn_and_split "*zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand"     "=r,r")
	(zero_extend:GPR
	    (match_operand:HI 1 "nonimmediate_operand" " r,m")))]
  "!TARGET_ZBB"
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

(define_expand "extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI 0 "register_operand")
	(sign_extend:SUPERQI (match_operand:SHORT 1 "nonimmediate_operand")))]
  "")

(define_insn_and_split "*extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI   0 "register_operand"     "=r,r")
	(sign_extend:SUPERQI
	    (match_operand:SHORT 1 "nonimmediate_operand" " r,m")))]
  "!TARGET_ZBB"
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

(define_insn "extendhfsf2"
  [(set (match_operand:SF     0 "register_operand" "=f")
       (float_extend:SF
           (match_operand:HF 1 "register_operand" " f")))]
  "TARGET_ZFHMIN || TARGET_ZHINXMIN"
  "fcvt.s.h\t%0,%1"
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
(define_expand "movhf"
  [(set (match_operand:HF 0 "")
	(match_operand:HF 1 ""))]
  ""
{
  if (riscv_legitimize_move (HFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movhf_hardfloat"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*r,  *r,*r,*m")
	(match_operand:HF 1 "move_operand"         " f,G,m,f,G,*r,*f,*G*r,*m,*r"))]
  "TARGET_ZFHMIN
   && (register_operand (operands[0], HFmode)
       || reg_or_0_operand (operands[1], HFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "HF")])

(define_insn "*movhf_softfloat"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=f, r,r,m,*f,*r")
	(match_operand:HF 1 "move_operand"         " f,Gr,m,r,*r,*f"))]
  "!TARGET_ZFHMIN
   && (register_operand (operands[0], HFmode)
       || reg_or_0_operand (operands[1], HFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,move,load,store,mtc,mfc")
   (set_attr "mode" "HF")])

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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.<GPR:ifmt>.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "fixuns_trunc<ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR      0 "register_operand" "=r")
	(unsigned_fix:GPR
	    (match_operand:ANYF 1 "register_operand" " f")))]
  "TARGET_HARD_FLOAT  || TARGET_ZFINX"
  "fcvt.<GPR:ifmt>u.<ANYF:fmt> %0,%1,rtz"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "float<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>\t%0,%z1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "floatuns<GPR:mode><ANYF:mode>2"
  [(set (match_operand:ANYF    0 "register_operand" "= f")
	(unsigned_float:ANYF
	    (match_operand:GPR 1 "reg_or_0_operand" " rJ")))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fcvt.<ANYF:fmt>.<GPR:ifmt>u\t%0,%z1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "<ANYF:MODE>")])

(define_insn "l<rint_pattern><ANYF:mode><GPR:mode>2"
  [(set (match_operand:GPR       0 "register_operand" "=r")
	(unspec:GPR
	    [(match_operand:ANYF 1 "register_operand" " f")]
	    RINT))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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

;; Pretend to have the ability to load complex const_int in order to get
;; better code generation around them.
;;
;; But avoid constants that are special cased elsewhere.
(define_insn_and_split "*mvconst_internal"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (match_operand:GPR 1 "splittable_const_int_operand" "i"))]
  "!(p2m1_shift_operand (operands[1], <MODE>mode)
     || high_mask_shift_operand (operands[1], <MODE>mode))"
  "#"
  "&& 1"
  [(const_int 0)]
{
  riscv_move_integer (operands[0], operands[0], INTVAL (operands[1]),
                      <MODE>mode, TRUE);
  DONE;
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
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,m,  *f,*f,*r,*f,*m,r")
	(match_operand:DI 1 "move_operand"         " r,i,m,r,*J*r,*m,*f,*f,*f,vp"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fmove,fpstore,rdvlenb")
   (set_attr "mode" "DI")
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
    && !(register_operand (operands[1], SImode)
         && reg_or_subregno (operands[1]) == VL_REGNUM)"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fpstore,rdvlenb")
   (set_attr "mode" "SI")
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
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*th_f_fmv,*th_r_fmv,  *r,*r,*m")
	(match_operand:DF 1 "move_operand"         " f,G,m,f,G,*th_r_fmv,*th_f_fmv,*r*G,*m,*r"))]
  "!TARGET_64BIT && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return riscv_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
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
  if (TARGET_ZIFENCEI)
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
  "TARGET_ZIFENCEI"
  "fence.i")

(define_insn "riscv_pause"
  [(unspec_volatile [(const_int 0)] UNSPECV_PAUSE)]
  ""
  "pause")

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

  return "<insn>%i2%~\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn_and_split "*<optab>si3_mask"
  [(set (match_operand:SI     0 "register_operand" "= r")
	(any_shift:SI
	    (match_operand:SI 1 "register_operand" "  r")
	    (match_operator 4 "subreg_lowpart_operator"
	     [(and:SI
	       (match_operand:SI 2 "register_operand"  "r")
	       (match_operand 3 "const_int_operand"))])))]
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
	    (match_operator 4 "subreg_lowpart_operator"
	     [(and:DI
	       (match_operand:DI 2 "register_operand"  "r")
	       (match_operand 3 "const_int_operand"))])))]
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
	    (match_operator 4 "subreg_lowpart_operator"
	     [(and:SI
	       (match_operand:SI 2 "register_operand"  "r")
	       (match_operand 3 "const_int_operand"))])))]
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
	    (match_operator 4 "subreg_lowpart_operator"
	     [(and:DI
	       (match_operand:DI 2 "register_operand"  "r")
	       (match_operand 3 "const_int_operand"))])))]
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
	     (match_operator 4 "subreg_lowpart_operator"
	      [(and:SI
	        (match_operand:SI 2 "register_operand" " r")
	        (match_operand 3 "const_int_operand"))]))))]
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
	     (match_operator 4 "subreg_lowpart_operator"
	      [(and:DI
	        (match_operand:DI 2 "register_operand" " r")
	        (match_operand 3 "const_int_operand"))]))))]
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
})

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
})

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
			  (match_operand:GPR 2 "reg_or_0_operand")
			  (match_operand:GPR 3 "sfb_alu_operand")))]
  "TARGET_SFB_ALU || TARGET_XTHEADCONDMOV"
{
  if (riscv_expand_conditional_move (operands[0], operands[1],
				     operands[2], operands[3]))
    DONE;
  else
    FAIL;
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
   b%C5\t%1,%z2,1f\t# movcc\;mv\t%0,%z4\n1:
   b%C5\t%1,%z2,1f\t# movcc\;li\t%0,%4\n1:"
  [(set_attr "length" "8")
   (set_attr "type" "sfb_alu")
   (set_attr "mode" "<GPR:MODE>")])

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

(define_expand "@cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "fp_branch_comparison"
		       [(match_operand:ANYF 1 "register_operand")
			(match_operand:ANYF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
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
  rtx tmp = gen_reg_rtx (SImode);
  rtx cmp = gen_rtx_<QUIET_PATTERN> (<X:MODE>mode, op1, op2);
  rtx frflags = gen_rtx_UNSPEC_VOLATILE (SImode, gen_rtvec (1, const0_rtx),
					 UNSPECV_FRFLAGS);
  rtx fsflags = gen_rtx_UNSPEC_VOLATILE (SImode, gen_rtvec (1, tmp),
					 UNSPECV_FSFLAGS);

  emit_insn (gen_rtx_SET (tmp, frflags));
  emit_insn (gen_rtx_SET (op0, cmp));
  emit_insn (fsflags);
  if (HONOR_SNANS (<ANYF:MODE>mode))
    emit_insn (gen_rtx_UNSPEC_VOLATILE (<ANYF:MODE>mode,
					gen_rtvec (2, op1, op2),
					UNSPECV_FSNVSNAN));
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

;; Must use the registers that we save to prevent the rename reg optimization
;; pass from using them before the gpr_save pattern when shrink wrapping
;; occurs.  See bug 95252 for instance.

(define_insn "gpr_save"
  [(match_parallel 1 "gpr_save_operation"
     [(unspec_volatile [(match_operand 0 "const_int_operand")]
	               UNSPECV_GPR_SAVE)])]
  ""
  "call\tt0,__riscv_save_%0")

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
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "frflags\t%0")

(define_insn "riscv_fsflags"
  [(unspec_volatile [(match_operand:SI 0 "csr_operand" "rK")] UNSPECV_FSFLAGS)]
  "TARGET_HARD_FLOAT || TARGET_ZFINX"
  "fsflags\t%0")

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
  [(set_attr "length" "12")])

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
  [(set_attr "length" "12")])

(define_insn "riscv_clean_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_CLEAN)]
  "TARGET_ZICBOM"
  "cbo.clean\t%a0"
)

(define_insn "riscv_flush_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_FLUSH)]
  "TARGET_ZICBOM"
  "cbo.flush\t%a0"
)

(define_insn "riscv_inval_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_INVAL)]
  "TARGET_ZICBOM"
  "cbo.inval\t%a0"
)

(define_insn "riscv_zero_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "register_operand" "r")]
    UNSPECV_ZERO)]
  "TARGET_ZICBOZ"
  "cbo.zero\t%a0"
)

(define_insn "prefetch"
  [(prefetch (match_operand 0 "address_operand" "r")
             (match_operand 1 "imm5_operand" "i")
             (match_operand 2 "const_int_operand" "n"))]
  "TARGET_ZICBOP"
{
  switch (INTVAL (operands[1]))
  {
    case 0: return "prefetch.r\t%a0";
    case 1: return "prefetch.w\t%a0";
    default: gcc_unreachable ();
  }
})

(define_insn "riscv_prefetchi_<mode>"
  [(unspec_volatile:X [(match_operand:X 0 "address_operand" "r")
              (match_operand:X 1 "imm5_operand" "i")]
              UNSPECV_PREI)]
  "TARGET_ZICBOP"
  "prefetch.i\t%a0"
)

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

(include "bitmanip.md")
(include "crypto.md")
(include "sync.md")
(include "peephole.md")
(include "pic.md")
(include "generic.md")
(include "sifive-7.md")
(include "thead.md")
(include "vector.md")
