;; Machine description for GNU compiler, VAX Version
;; Copyright (C) 1987-2023 Free Software Foundation, Inc.

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


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et al.

;; UNSPEC_VOLATILE usage:

(define_c_enum "unspecv" [
  VUNSPEC_BLOCKAGE 	    ; 'blockage' insn to prevent scheduling across an
			    ; insn in the code.
  VUNSPEC_SYNC_ISTREAM      ; sequence of insns to sync the I-stream
  VUNSPEC_PEM		    ; 'procedure_entry_mask' insn.
])

;; UNSPEC usage:

(define_c_enum "unspec" [
  UNSPEC_SETMEM_FILL	    ; 'fill' operand to 'setmem' insn.
])

(define_constants
  [(VAX_AP_REGNUM 12)	    ; Register 12 contains the argument pointer
   (VAX_FP_REGNUM 13)	    ; Register 13 contains the frame pointer
   (VAX_SP_REGNUM 14)	    ; Register 14 contains the stack pointer
   (VAX_PC_REGNUM 15)	    ; Register 15 contains the program counter
   (VAX_PSL_REGNUM 16)	    ; Register 16 contains the processor status
			    ; and condition codes in particular
  ]
)

;; Integer modes supported on VAX, with a mapping from machine mode
;; to mnemonic suffix.  DImode is always a special case.
(define_mode_iterator VAXint [QI HI SI])
(define_mode_iterator VAXintQH [QI HI])
(define_mode_iterator VAXintQHSD [QI HI SI DI])
(define_mode_attr  isfx [(QI "b") (HI "w") (SI "l") (DI "q")])

;; Similar for float modes supported on VAX.
(define_mode_iterator VAXfp [SF DF])
(define_mode_attr  fsfx [(SF "f") (DF "%#")])

;; Some output patterns want integer immediates with a prefix...
(define_mode_attr  iprefx [(QI "B") (HI "H") (SI "N")])

(define_mode_iterator VAXcc [CC CCN CCNZ CCZ])
(define_mode_iterator VAXccnz [CCN CCNZ CCZ])

(define_code_iterator any_extract [sign_extract zero_extract])

;;
(include "constraints.md")
(include "predicates.md")

;; Make instructions that set the N, N+Z, and Z condition codes respectively.
(define_subst "subst_<mode>"
  [(set (match_operand 0 "")
	(match_operand 1 ""))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  ""
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz (match_dup 1)
			 (const_int 0)))
   (set (match_dup 0)
	(match_dup 1))])

(define_subst "subst_f<VAXccnz:mode>"
  [(set (match_operand:VAXfp 0 "")
	(match_operand:VAXfp 1 ""))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  ""
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz (match_dup 1)
			 (const_double_zero:VAXfp)))
   (set (match_dup 0)
	(match_dup 1))])

;; Select all from the attributes below that apply to a given insn that
;; has a clobber on CC for the comparison elimination pass to use it in
;; place of a subsequent comparison instruction matching the mode used
;; by a comparison operator in branch.
;;
;; For example a branch doing `eq' in SImode will use `*cmpsi_ccz', so
;; to eliminate it a `*movsi_ccz', etc. pattern will be required via the
;; `ccz' substitution.  Analogously for the other CC modes.
;;
;; The general `cc' mode, which sets all of the C, N, V and Z condition
;; codes, has to be handled specially as it makes no sense for the usual
;; comparison against zero, so no substitution has been defined for it.
(define_subst_attr "ccn" "subst_ccn" "" "_ccn")
(define_subst_attr "ccnz" "subst_ccnz" "" "_ccnz")
(define_subst_attr "ccz" "subst_ccz" "" "_ccz")
(define_subst_attr "fccn" "subst_fccn" "" "_ccn")
(define_subst_attr "fccnz" "subst_fccnz" "" "_ccnz")
(define_subst_attr "fccz" "subst_fccz" "" "_ccz")

(define_insn "*cmp<VAXint:mode>_<VAXcc:mode>"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc (match_operand:VAXint 0 "general_operand" "nrmT,nrmT")
		       (match_operand:VAXint 1 "general_operand" "I,nrmT")))]
  "reload_completed"
  "@
   tst<VAXint:isfx> %0
   cmp<VAXint:isfx> %0,%1")

;; We don't have a CMPQ instruction, but we can set the N and Z condition
;; codes with MOVQ, and also this comparison can be folded into a preceding
;; operation by the post-reload comparison elimination pass.
(define_insn "*cmpdi_<VAXccnz:mode>"
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz (match_operand:DI 0 "general_operand" "r,nmT")
			 (match_operand:DI 1 "const_zero_operand" "I,I")))
   (clobber (match_scratch:DI 2 "=X,r"))]
  "reload_completed"
  "@
   movq %0,%0
   movq %0,%2")

(define_insn "*cmp<VAXfp:mode>_<VAXccnz:mode>"
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz (match_operand:VAXfp 0 "general_operand" "gF,gF")
			 (match_operand:VAXfp 1 "general_operand" "G,gF")))]
  "reload_completed"
  "@
   tst<VAXfp:fsfx> %0
   cmp<VAXfp:fsfx> %0,%1")

(define_insn "*bit<VAXint:mode>_<VAXccnz:mode>"
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz
	  (and:VAXint (match_operand:VAXint 0 "general_operand" "nrmT")
		      (match_operand:VAXint 1 "general_operand" "nrmT"))
	  (const_int 0)))]
  "reload_completed"
  "bit<VAXint:isfx> %0,%1")

;; The VAX has no sCOND insns.  It does have add/subtract with carry
;; which could be used to implement the sltu and sgeu patterns.  However,
;; to do this properly requires a complete rewrite of the compare insns
;; to keep them together with the sltu/sgeu insns until after the
;; reload pass is complete.  The previous implementation didn't do this
;; and has been deleted.


(define_insn_and_split "mov<mode>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(match_operand:VAXfp 1 "general_operand" "G,gF"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*mov<mode><fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(match_operand:VAXfp 1 "general_operand" "G,gF"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   clr<VAXfp:fsfx> %0
   mov<VAXfp:fsfx> %1,%0")

;; Some VAXen don't support this instruction.
;;(define_insn_and_split "movti"
;;  [(set (match_operand:TI 0 "nonimmediate_operand" "=g")
;;	(match_operand:TI 1 "general_operand" "g"))]
;;  ""
;;  "#"
;;  "reload_completed"
;;  [(parallel
;;     [(set (match_dup 0)
;;	   (match_dup 1))
;;      (clobber (reg:CC VAX_PSL_REGNUM))])]
;;  "")
;;
;;(define_insn "*movti<ccn><ccnz><ccz>"
;;  [(set (match_operand:TI 0 "nonimmediate_operand" "=g")
;;	(match_operand:TI 1 "general_operand" "g"))
;;   (clobber (reg:CC VAX_PSL_REGNUM))]
;;  "reload_completed"
;;  "movo %1,%0")

(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

;; In some cases `vax_output_int_move' splits a `DImode' move into a pair
;; of `SImode' moves, in which case the flags aren't usefully set.  Have
;; separate patterns then, for the cases where the move may and may not be
;; split each.  We use the outer condition only so in some cases we will
;; fail to notice the move does not actually get split, but this is OK.
(define_insn "*movdi_maybe_split"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed && vax_maybe_split_dimode_move (operands)"
  "* return vax_output_int_move (insn, operands, DImode);")

(define_insn "*movdi_unsplit<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed && !vax_maybe_split_dimode_move (operands)"
  "* return vax_output_int_move (insn, operands, DImode);")

;; The VAX move instructions have space-time tradeoffs.  On a MicroVAX
;; register-register mov instructions take 3 bytes and 2 CPU cycles.  clrl
;; takes 2 bytes and 3 cycles.  mov from constant to register takes 2 cycles
;; if the constant is smaller than 4 bytes, 3 cycles for a longword
;; constant.  movz, mneg, and mcom are as fast as mov, so movzwl is faster
;; than movl for positive constants that fit in 16 bits but not 6 bits.  cvt
;; instructions take 4 cycles.  inc takes 3 cycles.  The machine description
;; is willing to trade 1 byte for 1 cycle (clrl instead of movl $0; cvtwl
;; instead of movl).

;; Cycle counts for other models may vary (on a VAX 750 they are similar,
;; but on a VAX 9000 most move and add instructions with one constant
;; operand take 1 cycle).

;;  Loads of constants between 64 and 128 used to be done with
;; "addl3 $63,#,dst" but this is slower than movzbl and takes as much space.

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
#ifdef NO_EXTERNAL_INDIRECT_ADDRESS
  if (flag_pic
      && GET_CODE (operands[1]) == CONST
      && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == SYMBOL_REF
      && !SYMBOL_REF_LOCAL_P (XEXP (XEXP (operands[1], 0), 0)))
    {
      rtx symbol_ref = XEXP (XEXP (operands[1], 0), 0);
      rtx const_int = XEXP (XEXP (operands[1], 0), 1);
      rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);
      emit_move_insn (temp, symbol_ref);
      emit_move_insn (operands[0], gen_rtx_PLUS (SImode, temp, const_int));
      DONE;
    }
#endif
}")

(define_insn_and_split "movsi_2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:SI 1 "nonsymbolic_operand" "nrmT"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movsi_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:SI 1 "nonsymbolic_operand" "nrmT"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "* return vax_output_int_move (insn, operands, SImode);")

(define_insn_and_split "mov<mode>"
  [(set (match_operand:VAXintQH 0 "nonimmediate_operand" "=g")
	(match_operand:VAXintQH 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*mov<mode><ccn><ccnz><ccz>"
  [(set (match_operand:VAXintQH 0 "nonimmediate_operand" "=g")
	(match_operand:VAXintQH 1 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "* return vax_output_int_move (insn, operands, <MODE>mode);")

(define_insn_and_split "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r"))
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (strict_low_part (match_dup 0))
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movstricthi<ccn><ccnz><ccz>"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+r"))
	(match_operand:HI 1 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (CONST_INT_P (operands[1]))
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrw %0\";
      else if ((unsigned int)i < 64)
	return \"movw %1,%0\";
      else if ((unsigned int)~i < 64)
	return \"mcomw %H1,%0\";
      else if ((unsigned int)i < 256)
	return \"movzbw %1,%0\";
    }
  return \"movw %1,%0\";
}")

(define_insn_and_split "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r"))
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (strict_low_part (match_dup 0))
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movstrictqi<ccn><ccnz><ccz>"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "+r"))
	(match_operand:QI 1 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (CONST_INT_P (operands[1]))
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrb %0\";
      else if ((unsigned int)~i < 64)
	return \"mcomb %B1,%0\";
    }
  return \"movb %1,%0\";
}")

;; This is here to accept 4 arguments and pass the first 3 along
;; to the movmemhi1 pattern that really does the work.
(define_expand "cpymemhi"
  [(set (match_operand:BLK 0 "memory_operand" "")
	(match_operand:BLK 1 "memory_operand" ""))
   (use (match_operand:HI 2 "general_operand" ""))
   (match_operand 3 "" "")]
  ""
  "
{
  emit_insn (gen_movmemhi1 (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "movmemhi"
  [(set (match_operand:BLK 0 "memory_operand" "")
	(match_operand:BLK 1 "memory_operand" ""))
   (use (match_operand:HI 2 "general_operand" ""))
   (match_operand 3 "" "")]
  ""
  "
{
  emit_insn (gen_movmemhi1 (operands[0], operands[1], operands[2]));
  DONE;
}")

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.

(define_insn_and_split "movmemhi1"
  [(set (match_operand:BLK 0 "memory_operand" "=o")
	(match_operand:BLK 1 "memory_operand" "o"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (use (match_dup 2))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI 3))
      (clobber (reg:SI 4))
      (clobber (reg:SI 5))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movmemhi1"
  [(set (match_operand:BLK 0 "memory_operand" "=o")
	(match_operand:BLK 1 "memory_operand" "o"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movc3 %2,%1,%0")

;; This is here to accept 4 arguments and pass the first 3 along
;; to the setmemhi1 pattern that really does the work.
(define_expand "setmemhi"
  [(set (match_operand:BLK 0 "memory_operand" "")
	(match_operand:QI 2 "general_operand" ""))
   (use (match_operand:HI 1 "general_operand" ""))
   (match_operand 3 "" "")]
  ""
  "
{
  emit_insn (gen_setmemhi1 (operands[0], operands[1], operands[2]));
  DONE;
}")

;; The srcaddr operand of MOVC5 is not dereferenced if srclen is zero, so we
;; set it to (%ap) somewhat arbitrarily chosen for the shortest encoding.
(define_insn_and_split "setmemhi1"
  [(set (match_operand:BLK 0 "memory_operand" "=o")
	(unspec:BLK [(use (match_operand:QI 2 "general_operand" "g"))]
		    UNSPEC_SETMEM_FILL))
   (use (match_operand:HI 1 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (unspec:BLK [(use (match_dup 2))] UNSPEC_SETMEM_FILL))
      (use (match_dup 1))
      (clobber (reg:SI 0))
      (clobber (reg:SI 1))
      (clobber (reg:SI 2))
      (clobber (reg:SI 3))
      (clobber (reg:SI 4))
      (clobber (reg:SI 5))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*setmemhi1"
  [(set (match_operand:BLK 0 "memory_operand" "=o")
	(unspec:BLK [(use (match_operand:QI 2 "general_operand" "g"))]
		    UNSPEC_SETMEM_FILL))
   (use (match_operand:HI 1 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movc5 $0,(%%ap),%2,%1,%0")

;; Extension and truncation insns.

(define_insn_and_split "truncsiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g")
	(truncate:QI (match_operand:SI 1 "nonimmediate_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (truncate:QI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*truncsiqi2<ccn><ccnz><ccz>"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g")
	(truncate:QI (match_operand:SI 1 "nonimmediate_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtlb %1,%0")

(define_insn_and_split "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(truncate:HI (match_operand:SI 1 "nonimmediate_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (truncate:HI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*truncsihi2<ccn><ccnz><ccz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(truncate:HI (match_operand:SI 1 "nonimmediate_operand" "nrmT")))
      (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtlw %1,%0")

(define_insn_and_split "trunchiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g")
	(truncate:QI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (truncate:QI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*trunchiqi2<ccn><ccnz><ccz>"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g")
	(truncate:QI (match_operand:HI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtwb %1,%0")

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extend:SI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extendhisi2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtwl %1,%0")

(define_insn_and_split "extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extend:HI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extendqihi2<ccn><ccnz><ccz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtbw %1,%0")

(define_insn_and_split "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extend:SI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extendqisi2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtbl %1,%0")

(define_insn_and_split "extendsfdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=g")
	(float_extend:DF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (float_extend:DF (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extendsfdf2<fccn><fccnz><fccz>"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=g")
	(float_extend:DF (match_operand:SF 1 "general_operand" "gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvtf%# %1,%0")

(define_insn_and_split "truncdfsf2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=g")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (float_truncate:SF (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*truncdfsf2<fccn><fccnz><fccz>"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=g")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvt%#f %1,%0")

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extend:SI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*zero_extendhisi2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movzwl %1,%0")

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extend:HI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*zero_extendqihi2<ccn><ccnz><ccz>"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movzbw %1,%0")

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extend:SI (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*zero_extendqisi2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movzbl %1,%0")

;; Fix-to-float conversion insns.

(define_insn_and_split "float<VAXint:mode><VAXfp:mode>2"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g")
	(float:VAXfp (match_operand:VAXint 1 "nonimmediate_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (float:VAXfp (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*float<VAXint:mode><VAXfp:mode>2<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g")
	(float:VAXfp (match_operand:VAXint 1 "nonimmediate_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvt<VAXint:isfx><VAXfp:fsfx> %1,%0")

;; Float-to-fix conversion insns.

(define_insn_and_split "fix_trunc<VAXfp:mode><VAXint:mode>2"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(fix:VAXint (match_operand:VAXfp 1 "general_operand" "gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (fix:VAXint (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*fix_trunc<VAXfp:mode><VAXint:mode>2<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(fix:VAXint (match_operand:VAXfp 1 "general_operand" "gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "cvt<VAXfp:fsfx><VAXint:isfx> %1,%0")

(define_expand "fixuns_trunc<VAXfp:mode><VAXint:mode>2"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "")
	(fix:VAXint (match_operand:VAXfp 1 "general_operand")))]
  "")

;;- All kinds of add instructions.

(define_insn_and_split "add<mode>3"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g,g")
	(plus:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF,gF")
		    (match_operand:VAXfp 2 "general_operand" "gF,0,gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:VAXfp (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*add<mode>3<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g,g")
	(plus:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF,gF")
		    (match_operand:VAXfp 2 "general_operand" "gF,0,gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   add<VAXfp:fsfx>2 %2,%0
   add<VAXfp:fsfx>2 %1,%0
   add<VAXfp:fsfx>3 %1,%2,%0")

(define_insn_and_split "add<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(plus:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")
		     (match_operand:VAXint 2 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:VAXint (match_dup 1)
			(match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*add<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(plus:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")
		     (match_operand:VAXint 2 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "* return vax_output_int_add (insn, operands, <MODE>mode);")

(define_expand "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(plus:DI (match_operand:DI 1 "general_operand" "g")
		 (match_operand:DI 2 "general_operand" "g")))]
  "!reload_in_progress"
  "vax_expand_addsub_di_operands (operands, PLUS); DONE;")

(define_insn_and_split "adcdi3"
  [(set (match_operand:DI 0 "nonimmediate_addsub_di_operand" "=Rr")
	(plus:DI (match_operand:DI 1 "general_addsub_di_operand" "%0")
		 (match_operand:DI 2 "general_addsub_di_operand" "nRr")))]
  "TARGET_QMATH"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:DI (match_dup 1)
		    (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*adcdi3<ccn>"
  [(set (match_operand:DI 0 "nonimmediate_addsub_di_operand" "=Rr")
	(plus:DI (match_operand:DI 1 "general_addsub_di_operand" "%0")
		 (match_operand:DI 2 "general_addsub_di_operand" "nRr")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "TARGET_QMATH && reload_completed"
  "* return vax_output_int_add (insn, operands, DImode);")

;; The add-with-carry (adwc) instruction only accepts two operands.
(define_insn_and_split "adddi3_old"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=ro>,ro>")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,ro>")
		 (match_operand:DI 2 "general_operand" "Fsro,Fs")))]
  "!TARGET_QMATH"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:DI (match_dup 1)
		    (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*adddi3_old<ccn>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=ro>,ro>")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,ro>")
		 (match_operand:DI 2 "general_operand" "Fsro,Fs")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_QMATH && reload_completed"
  "* return vax_output_int_add (insn, operands, DImode);")

;;- All kinds of subtract instructions.

(define_insn_and_split "sub<mode>3"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(minus:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF")
		     (match_operand:VAXfp 2 "general_operand" "gF,gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (minus:VAXfp (match_dup 1)
			(match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*sub<mode>3<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(minus:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF")
		     (match_operand:VAXfp 2 "general_operand" "gF,gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   sub<VAXfp:fsfx>2 %2,%0
   sub<VAXfp:fsfx>3 %2,%1,%0")

(define_insn_and_split "sub<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(minus:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		      (match_operand:VAXint 2 "general_operand" "nrmT,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (minus:VAXint (match_dup 1)
			 (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*sub<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(minus:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		      (match_operand:VAXint 2 "general_operand" "nrmT,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   sub<VAXint:isfx>2 %2,%0
   sub<VAXint:isfx>3 %2,%1,%0")

(define_insn "*sub<mode>3_cc"
  [(set (reg:CC VAX_PSL_REGNUM)
	(compare:CC (match_operand:VAXint 1 "general_operand" "0,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,nrmT")))
   (set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(minus:VAXint (match_dup 1)
		      (match_dup 2)))]
  "reload_completed"
  "@
   sub<VAXint:isfx>2 %2,%0
   sub<VAXint:isfx>3 %2,%1,%0")

(define_expand "subdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(minus:DI (match_operand:DI 1 "general_operand" "g")
		  (match_operand:DI 2 "general_operand" "g")))]
  "!reload_in_progress"
  "vax_expand_addsub_di_operands (operands, MINUS); DONE;")

(define_insn_and_split "sbcdi3"
  [(set (match_operand:DI 0 "nonimmediate_addsub_di_operand" "=Rr,Rr")
	(minus:DI (match_operand:DI 1 "general_addsub_di_operand" "0,I")
		  (match_operand:DI 2 "general_addsub_di_operand" "nRr,Rr")))]
  "TARGET_QMATH"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (minus:DI (match_dup 1)
		     (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*sbcdi3<ccn>"
  [(set (match_operand:DI 0 "nonimmediate_addsub_di_operand" "=Rr,Rr")
	(minus:DI (match_operand:DI 1 "general_addsub_di_operand" "0,I")
		  (match_operand:DI 2 "general_addsub_di_operand" "nRr,Rr")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "TARGET_QMATH && reload_completed"
  "* return vax_output_int_subtract (insn, operands, DImode);")

;; The subtract-with-carry (sbwc) instruction only takes two operands.
(define_insn_and_split "subdi3_old"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=or>,or>")
	(minus:DI (match_operand:DI 1 "general_operand" "0,or>")
		  (match_operand:DI 2 "general_operand" "Fsor,Fs")))]
  "!TARGET_QMATH"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (minus:DI (match_dup 1)
		     (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*subdi3_old<ccn>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=or>,or>")
	(minus:DI (match_operand:DI 1 "general_operand" "0,or>")
		  (match_operand:DI 2 "general_operand" "Fsor,Fs")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_QMATH && reload_completed"
  "* return vax_output_int_subtract (insn, operands, DImode);")

;;- Multiply instructions.

(define_insn_and_split "mul<mode>3"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g,g")
	(mult:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF,gF")
		    (match_operand:VAXfp 2 "general_operand" "gF,0,gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (mult:VAXfp (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*mul<mode>3<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g,g")
	(mult:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF,gF")
		    (match_operand:VAXfp 2 "general_operand" "gF,0,gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   mul<VAXfp:fsfx>2 %2,%0
   mul<VAXfp:fsfx>2 %1,%0
   mul<VAXfp:fsfx>3 %1,%2,%0")

(define_insn_and_split "mul<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(mult:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		     (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (mult:VAXint (match_dup 1)
			(match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*mul<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(mult:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		     (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   mul<VAXint:isfx>2 %2,%0
   mul<VAXint:isfx>2 %1,%0
   mul<VAXint:isfx>3 %1,%2,%0")

(define_insn_and_split "mulsidi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(mult:DI
	  (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	  (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT"))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (mult:DI
	     (sign_extend:DI (match_dup 1))
	     (sign_extend:DI (match_dup 2))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*mulsidi3<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(mult:DI
	  (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	  (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT"))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "emul %1,%2,$0,%0")

(define_insn_and_split "*maddsidi4"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	    (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT")))
	  (sign_extend:DI (match_operand:SI 3 "general_operand" "g"))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:DI
	     (mult:DI
	       (sign_extend:DI (match_dup 1))
	       (sign_extend:DI (match_dup 2)))
	     (sign_extend:DI (match_dup 3))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*maddsidi4_2<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	    (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT")))
	  (sign_extend:DI (match_operand:SI 3 "nonimmediate_operand" "g"))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "emul %1,%2,%3,%0")

;; 'F' constraint means type CONST_DOUBLE
(define_insn_and_split "*maddsidi4_const"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	    (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT")))
	  (match_operand:DI 3 "immediate_operand" "F")))]
  "GET_CODE (operands[3]) == CONST_DOUBLE
   && CONST_DOUBLE_HIGH (operands[3]) == (CONST_DOUBLE_LOW (operands[3]) >> 31)"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:DI
	     (mult:DI
	       (sign_extend:DI (match_dup 1))
	       (sign_extend:DI (match_dup 2)))
	     (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*maddsidi4_const_2<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI (match_operand:SI 1 "general_operand" "nrmT"))
	    (sign_extend:DI (match_operand:SI 2 "general_operand" "nrmT")))
	  (match_operand:DI 3 "immediate_operand" "F")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "GET_CODE (operands[3]) == CONST_DOUBLE
   && CONST_DOUBLE_HIGH (operands[3]) == (CONST_DOUBLE_LOW (operands[3]) >> 31)
   && reload_completed"
  "*
{
  if (CONST_DOUBLE_HIGH (operands[3]))
    operands[3] = GEN_INT (CONST_DOUBLE_LOW (operands[3]));
  return \"emul %1,%2,%3,%0\";
}")

;;- Divide instructions.

(define_insn_and_split "div<mode>3"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(div:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF")
		   (match_operand:VAXfp 2 "general_operand" "gF,gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (div:VAXfp (match_dup 1)
		      (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*div<mode>3<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g,g")
	(div:VAXfp (match_operand:VAXfp 1 "general_operand" "0,gF")
		   (match_operand:VAXfp 2 "general_operand" "gF,gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   div<VAXfp:fsfx>2 %2,%0
   div<VAXfp:fsfx>3 %2,%1,%0")

(define_insn_and_split "div<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(div:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (div:VAXint (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*div<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(div:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   div<VAXint:isfx>2 %2,%0
   div<VAXint:isfx>3 %2,%1,%0")

;; This is left out because it is very slow;
;; we are better off programming around the "lack" of this insn.
;;(define_insn_and_split "divmoddisi4"
;;  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
;;	(div:SI (match_operand:DI 1 "general_operand" "g")
;;		(match_operand:SI 2 "general_operand" "g")))
;;   (set (match_operand:SI 3 "nonimmediate_operand" "=g")
;;	(mod:SI (match_dup 1)
;;		(match_dup 2)))]
;;  ""
;;  "#"
;;  "reload_completed"
;;  [(parallel
;;     [(set (match_dup 0)
;;	   (div:SI (match_dup 1)
;;		   (match_dup 2)))
;;      (set (match_dup 3)
;;	   (mod:SI (match_dup 1)
;;		   (match_dup 2)))
;;      (clobber (reg:CC VAX_PSL_REGNUM))])]
;;  "")
;;
;;(define_insn "*divmoddisi4<ccn><ccnz><ccz>"
;;  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
;;	(div:SI (match_operand:DI 1 "general_operand" "g")
;;		(match_operand:SI 2 "general_operand" "g")))
;;   (set (match_operand:SI 3 "nonimmediate_operand" "=g")
;;	(mod:SI (match_dup 1)
;;		(match_dup 2)))
;;   (clobber (reg:CC VAX_PSL_REGNUM))]
;;  "reload_completed"
;;  "ediv %2,%1,%0,%3")

;; Bit-and on the VAX is done with a clear-bits insn.
(define_expand "and<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "")
	(and:VAXint (not:VAXint (match_operand:VAXint 1 "general_operand" ""))
		    (match_operand:VAXint 2 "general_operand" "")))]
  ""
  "
{
  rtx op1 = operands[1];

  /* If there is a constant argument, complement that one.  */
  if (CONST_INT_P (operands[2]) && ! CONST_INT_P (op1))
    {
      operands[1] = operands[2];
      operands[2] = op1;
      op1 = operands[1];
    }

  if (CONST_INT_P (op1))
    operands[1] = GEN_INT (~INTVAL (op1));
  else
    operands[1] = expand_unop (<MODE>mode, one_cmpl_optab, op1, 0, 1);
}")

(define_insn_and_split "*and<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(and:VAXint (not:VAXint
		      (match_operand:VAXint 1 "general_operand" "nrmT,nrmT"))
		    (match_operand:VAXint 2 "general_operand" "0,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (and:VAXint (not:VAXint
			 (match_dup 1))
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*and<mode>3_2<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(and:VAXint (not:VAXint
		      (match_operand:VAXint 1 "general_operand" "nrmT,nrmT"))
		    (match_operand:VAXint 2 "general_operand" "0,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   bic<VAXint:isfx>2 %1,%0
   bic<VAXint:isfx>3 %1,%2,%0")

;; The following used to be needed because constant propagation can
;; create them starting from the bic insn patterns above.  This is no
;; longer a problem.  However, having these patterns allows optimization
;; opportunities in combine.cc.

(define_insn_and_split "*and<mode>3_const_int"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(and:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		    (match_operand:VAXint 2 "const_int_operand" "n,n")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (and:VAXint (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*and<mode>3_2_const_int<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(and:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT")
		    (match_operand:VAXint 2 "const_int_operand" "n,n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   bic<VAXint:isfx>2 %<VAXint:iprefx>2,%0
   bic<VAXint:isfx>3 %<VAXint:iprefx>2,%1,%0")

;; We have no direct AND operation and consequently the RTL sequence
;; the "and<mode>3" pattern produces does not match the instruction
;; the "*bit<mode>" pattern does for the purpose of the compare
;; elimination pass.  Try to get rid of the extra operation by hand
;; and where the sequence is used to set the condition codes only
;; convert MCOM/BIC => BIT.
(define_peephole2
  [(parallel
     [(set (match_operand:VAXint 0 "register_operand")
	   (not:VAXint (match_operand:VAXint 1 "general_operand")))
      (clobber (reg:CC VAX_PSL_REGNUM))])
   (parallel
     [(set (reg:VAXccnz VAX_PSL_REGNUM)
	   (compare:VAXccnz
	     (and:VAXint (not:VAXint (match_dup 0))
			 (match_operand:VAXint 3 "general_operand"))
	     (const_int 0)))
      (set (match_operand:VAXint 2 "register_operand")
	   (and:VAXint (not:VAXint (match_dup 0))
		       (match_dup 3)))])]
  "peep2_reg_dead_p (2, operands[0]) && peep2_reg_dead_p (2, operands[2])"
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz
	  (and:VAXint (match_dup 1)
		      (match_dup 3))
	  (const_int 0)))]
  "")

;;- Bit set instructions.

(define_insn_and_split "ior<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(ior:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ior:VAXint (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ior<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(ior:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   bis<VAXint:isfx>2 %2,%0
   bis<VAXint:isfx>2 %1,%0
   bis<VAXint:isfx>3 %2,%1,%0")

;;- xor instructions.

(define_insn_and_split "xor<mode>3"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(xor:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (xor:VAXint (match_dup 1)
		       (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*xor<mode>3<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g,g")
	(xor:VAXint (match_operand:VAXint 1 "general_operand" "0,nrmT,nrmT")
		    (match_operand:VAXint 2 "general_operand" "nrmT,0,nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "@
   xor<VAXint:isfx>2 %2,%0
   xor<VAXint:isfx>2 %1,%0
   xor<VAXint:isfx>3 %2,%1,%0")

(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g")
	(neg:VAXfp (match_operand:VAXfp 1 "general_operand" "gF")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (neg:VAXfp (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*neg<mode>2<fccn><fccnz><fccz>"
  [(set (match_operand:VAXfp 0 "nonimmediate_operand" "=g")
	(neg:VAXfp (match_operand:VAXfp 1 "general_operand" "gF")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "mneg<VAXfp:fsfx> %1,%0")

(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(neg:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (neg:VAXint (match_dup 1)))
	 (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*neg<mode>2<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(neg:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "mneg<VAXint:isfx> %1,%0")

(define_insn "*neg<mode>2_cc"
  [(set (reg:CC VAX_PSL_REGNUM)
	(compare:CC (const_int 0)
		    (neg:VAXint
		      (match_operand:VAXint 1 "general_operand" "0,nrmT"))))
   (set (match_operand:VAXint 0 "nonimmediate_operand" "=g,g")
	(neg:VAXint (match_dup 1)))]
  "reload_completed"
  "mneg<VAXint:isfx> %1,%0")

(define_insn_and_split "one_cmpl<mode>2"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(not:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (not:VAXint (match_dup 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*one_cmpl<mode>2<ccn><ccnz><ccz>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=g")
	(not:VAXint (match_operand:VAXint 1 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "mcom<VAXint:isfx> %1,%0")

;; Arithmetic right shift on the VAX works by negating the shift count,
;; then emitting a right shift with the shift count negated.  This means
;; that all actual shift counts in the RTL will be positive.  This
;; prevents converting shifts to ZERO_EXTRACTs with negative positions,
;; which isn't valid.
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  if (! CONST_INT_P(operands[2]))
    operands[2] = gen_rtx_NEG (QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn_and_split "*ashlnegsi3_const_int"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (match_operand:QI 2 "const_int_operand" "n")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ashiftrt:SI (match_dup 1)
			(match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ashlnegsi3_const_int_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (match_operand:QI 2 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "ashl $%n2,%1,%0")

(define_insn_and_split "*ashlnegsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ashiftrt:SI (match_dup 1)
			(neg:QI (match_dup 2))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ashlnegsi3_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "ashl %2,%1,%0")

(define_insn_and_split "ashlsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "nrmT")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ashift:SI (match_dup 1)
		      (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ashlsi3<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "nrmT")
		   (match_operand:QI 2 "general_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (operands[2] == const1_rtx && rtx_equal_p (operands[0], operands[1]))
    return \"addl2 %0,%0\";
  if (REG_P (operands[1]) && CONST_INT_P (operands[2]))
    {
      int i = INTVAL (operands[2]);
      if (i == 1)
	return \"addl3 %1,%1,%0\";
      if (i == 2 && !optimize_size)
	{
	  if (push_operand (operands[0], SImode))
	    return \"pushal 0[%1]\";
	  return \"moval 0[%1],%0\";
	}
      if (i == 3 && !optimize_size)
	{
	  if (push_operand (operands[0], SImode))
	    return \"pushaq 0[%1]\";
	  return \"movaq 0[%1],%0\";
	}
    }
  return \"ashl %2,%1,%0\";
}")

;; Arithmetic right shift on the VAX works by negating the shift count.
(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashiftrt:DI (match_operand:DI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = gen_rtx_NEG (QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn_and_split "ashldi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ashift:DI (match_dup 1)
		      (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ashldi3<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "ashq %2,%D1,%0")

(define_insn_and_split "*ashlnegdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(ashiftrt:DI (match_operand:DI 1 "general_operand" "g")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (ashiftrt:DI (match_dup 1)
			(neg:QI (match_dup 2))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*ashlnegdi3_2<ccn><ccnz><ccz>"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(ashiftrt:DI (match_operand:DI 1 "general_operand" "g")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "ashq %2,%D1,%0")

;; We used to have expand_shift handle logical right shifts by using extzv,
;; but this make it very difficult to do lshrdi3.  Since the VAX is the
;; only machine with this kludge, it's better to just do this with a
;; define_expand and remove that case from expand_shift.

(define_expand "lshrsi3"
  [(set (match_dup 3)
	(minus:QI (const_int 32)
		  (match_dup 4)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_dup 3)
			 (match_operand:SI 2 "register_operand" "g")))]
  ""
  "
{
  operands[3] = gen_reg_rtx (QImode);
  operands[4] = gen_lowpart (QImode, operands[2]);
}")

;; Rotate right on the VAX works by negating the shift count.
(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  if (! CONST_INT_P (operands[2]))
    operands[2] = gen_rtx_NEG (QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn_and_split "rotlsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "nrmT")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (rotate:SI (match_dup 1)
		      (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*rotlsi3<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "nrmT")
		   (match_operand:QI 2 "general_operand" "g")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "rotl %2,%1,%0")

(define_insn_and_split "*rotrsi3_const_int"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (match_operand:QI 2 "const_int_operand" "n")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (rotatert:SI (match_dup 1)
			(match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*rotrsi3_const_int_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (match_operand:QI 2 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "rotl %R2,%1,%0")

(define_insn_and_split "*rotrnegsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (rotatert:SI (match_dup 1)
			(neg:QI (match_dup 2))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*rotrnegsi3_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "nrmT")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "rotl %2,%1,%0")

;; This insn is probably slower than a multiply and an add.
;;(define_insn_and_split "*amulsi4"
;;  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
;;	(mult:SI (plus:SI (match_operand:SI 1 "general_operand" "g")
;;			  (match_operand:SI 2 "general_operand" "g"))
;;		 (match_operand:SI 3 "general_operand" "g")))]
;;  ""
;;  "#"
;;  "reload_completed"
;;  [(parallel
;;     [(set (match_dup 0)
;;	   (mult:SI (plus:SI (match_dup 1)
;;			     (match_dup 2))
;;		    (match_dup 3)))
;;      (clobber (reg:CC VAX_PSL_REGNUM))])]
;;  "")
;;
;;(define_insn "*amulsi4_2<ccn><ccnz><ccz>"
;;  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
;;	(mult:SI (plus:SI (match_operand:SI 1 "general_operand" "g")
;;			  (match_operand:SI 2 "general_operand" "g"))
;;		 (match_operand:SI 3 "general_operand" "g")))
;;   (clobber (reg:CC VAX_PSL_REGNUM))]
;;  "reload_completed"
;;  "index %1,$0x80000000,$0x7fffffff,%3,%2,%0")

;; Special cases of bit-field insns which we should
;; recognize in preference to the general case.
;; These handle aligned 8-bit and 16-bit fields
;; that can be done with move or convert instructions.

(define_insn_and_split "*insv_aligned"
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+ro")
			 (match_operand:QI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(match_operand:SI 3 "general_operand" "g"))]
  "(INTVAL (operands[1]) == 8 || INTVAL (operands[1]) == 16)
   && INTVAL (operands[2]) % INTVAL (operands[1]) == 0
   && (!MEM_P (operands[0])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[0], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[0], 0),
					 MEM_ADDR_SPACE (operands[0]))))
   && (!(REG_P (operands[0])
	 || (SUBREG_P (operands[0]) && REG_P (SUBREG_REG (operands[0]))))
       || INTVAL (operands[2]) == 0)"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (zero_extract:SI (match_dup 0)
			    (match_dup 1)
			    (match_dup 2))
	   (match_dup 3))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*insv_aligned_2<ccn><ccnz><ccz>"
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+ro")
			 (match_operand:QI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(match_operand:SI 3 "general_operand" "g"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "(INTVAL (operands[1]) == 8 || INTVAL (operands[1]) == 16)
   && INTVAL (operands[2]) % INTVAL (operands[1]) == 0
   && (!MEM_P (operands[0])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[0], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[0], 0),
					 MEM_ADDR_SPACE (operands[0]))))
   && (!(REG_P (operands[0])
	 || (SUBREG_P (operands[0]) && REG_P (SUBREG_REG (operands[0]))))
       || INTVAL (operands[2]) == 0)
   && reload_completed"
  "*
{
  if (!REG_P (operands[0]))
    operands[0]
      = adjust_address (operands[0],
			INTVAL (operands[1]) == 8 ? QImode : HImode,
			INTVAL (operands[2]) / 8);
  else
    gcc_assert (INTVAL (operands[2]) == 0);

  if (INTVAL (operands[1]) == 8)
    return \"movb %3,%0\";
  return \"movw %3,%0\";
}")

(define_insn_and_split "*extzv_aligned"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (!MEM_P (operands[1])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[1], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[1], 0),
					 MEM_ADDR_SPACE (operands[1]))))
   && (!(REG_P (operands[1])
	 || (SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1]))))
       || INTVAL (operands[3]) == 0)"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extzv_aligned_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=&g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (!MEM_P (operands[1])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[1], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[1], 0),
					 MEM_ADDR_SPACE (operands[1]))))
   && (!(REG_P (operands[1])
	 || (SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1]))))
       || INTVAL (operands[3]) == 0)
   && reload_completed"
  "*
{
  if (!REG_P (operands[1]))
    operands[1]
      = adjust_address (operands[1],
			INTVAL (operands[2]) == 8 ? QImode : HImode,
			INTVAL (operands[3]) / 8);
  else
    gcc_assert (INTVAL (operands[3]) == 0);

  if (INTVAL (operands[2]) == 8)
    return \"movzbl %1,%0\";
  return \"movzwl %1,%0\";
}")

(define_insn_and_split "*extv_aligned"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (!MEM_P (operands[1])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[1], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[1], 0),
					 MEM_ADDR_SPACE (operands[1]))))
   && (!(REG_P (operands[1])
	 || (SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1]))))
       || INTVAL (operands[3]) == 0)"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extv_aligned_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (!MEM_P (operands[1])
       || ((!flag_pic
	    || vax_acceptable_pic_operand_p (XEXP (operands[1], 0),
					     true, true))
	   && !mode_dependent_address_p (XEXP (operands[1], 0),
					 MEM_ADDR_SPACE (operands[1]))))
   && (!(REG_P (operands[1])
	 || (SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1]))))
       || INTVAL (operands[3]) == 0)
   && reload_completed"
  "*
{
  if (!REG_P (operands[1]))
    operands[1]
      = adjust_address (operands[1],
			INTVAL (operands[2]) == 8 ? QImode : HImode,
			INTVAL (operands[3]) / 8);
  else
    gcc_assert (INTVAL (operands[3]) == 0);

  if (INTVAL (operands[2]) == 8)
    return \"cvtbl %1,%0\";
  return \"cvtwl %1,%0\";
}")

;; Register and non-offsettable-memory SImode cases of bit-field insns.

(define_insn "*cmpv_<mode>"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc
	 (sign_extract:SI (match_operand:SI 0 "nonimmediate_operand" "ro")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "nrmT"))
	 (match_operand:SI 3 "general_operand" "nrmT")))]
  "reload_completed"
  "cmpv %2,%1,%0,%3")

(define_insn "*cmpzv_<mode>"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc
	 (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "ro")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "nrmT"))
	 (match_operand:SI 3 "general_operand" "nrmT")))]
  "reload_completed"
  "cmpzv %2,%1,%0,%3")

;; When the field position and size are constant and the destination
;; is a register, extv and extzv are much slower than a rotate followed
;; by a bicl or sign extension.  Because we might end up choosing ext[z]v
;; anyway, we can't allow immediate values for the primary source operand.

(define_insn_and_split "*extv_non_const"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extv_non_const_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (! CONST_INT_P (operands[3]) || ! CONST_INT_P (operands[2])
      || ! REG_P (operands[0])
      || (INTVAL (operands[2]) != 8 && INTVAL (operands[2]) != 16))
    return \"extv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;cvtbl %0,%0\";
  return \"rotl %R3,%1,%0\;cvtwl %0,%0\";
}")

(define_insn_and_split "*extzv_non_const"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extzv_non_const_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (! CONST_INT_P (operands[3]) || ! CONST_INT_P (operands[2])
      || ! REG_P (operands[0]))
    return \"extzv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;movzbl %0,%0\";
  if (INTVAL (operands[2]) == 16)
    return \"rotl %R3,%1,%0\;movzwl %0,%0\";
  if (INTVAL (operands[3]) & 31)
    return \"rotl %R3,%1,%0\;bicl2 %M2,%0\";
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicl2 %M2,%0\";
  return \"bicl3 %M2,%1,%0\";
}")

;; Non-register cases.
;; nonimmediate_operand is used to make sure that mode-ambiguous cases
;; don't match these (and therefore match the cases above instead).

(define_insn "*cmpv_2_<mode>"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc
	 (sign_extract:SI (match_operand:QI 0 "memory_operand" "m")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "nrmT"))
	 (match_operand:SI 3 "general_operand" "nrmT")))]
  "reload_completed"
  "cmpv %2,%1,%0,%3")

(define_insn "*cmpzv_2_<mode>"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc
	 (zero_extract:SI (match_operand:QI 0 "memory_operand" "m")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "nrmT"))
	 (match_operand:SI 3 "general_operand" "nrmT")))]
  "reload_completed"
  "cmpzv %2,%1,%0,%3")

(define_expand "extv"
  [(set (match_operand:SI 0 "general_operand" "")
	(sign_extract:SI (match_operand:SI 1 "general_operand" "")
			 (match_operand:QI 2 "general_operand" "")
			 (match_operand:SI 3 "general_operand" "")))]
  ""
  "")

(define_insn_and_split "*extv"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (sign_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extv_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(sign_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (!REG_P (operands[0]) || !CONST_INT_P (operands[2])
      || !CONST_INT_P (operands[3])
      || (INTVAL (operands[2]) != 8 && INTVAL (operands[2]) != 16)
      || INTVAL (operands[2]) + INTVAL (operands[3]) > 32
      || side_effects_p (operands[1])
      || (MEM_P (operands[1])
	  && mode_dependent_address_p (XEXP (operands[1], 0),
				       MEM_ADDR_SPACE (operands[1]))))
    return \"extv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;cvtbl %0,%0\";
  return \"rotl %R3,%1,%0\;cvtwl %0,%0\";
}")

(define_expand "extzv"
  [(set (match_operand:SI 0 "general_operand" "")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "")
			 (match_operand:QI 2 "general_operand" "")
			 (match_operand:SI 3 "general_operand" "")))]
  ""
  "")

(define_insn_and_split "*extzv"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (zero_extract:SI (match_dup 1)
			    (match_dup 2)
			    (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*extzv_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(zero_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "nrmT")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  if (!REG_P (operands[0]) || !CONST_INT_P (operands[2])
      || !CONST_INT_P (operands[3])
      || INTVAL (operands[2]) + INTVAL (operands[3]) > 32
      || side_effects_p (operands[1])
      || (MEM_P (operands[1])
	  && mode_dependent_address_p (XEXP (operands[1], 0),
				       MEM_ADDR_SPACE (operands[1]))))
    return \"extzv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;movzbl %0,%0\";
  if (INTVAL (operands[2]) == 16)
    return \"rotl %R3,%1,%0\;movzwl %0,%0\";
  if (MEM_P (operands[1])
      && GET_CODE (XEXP (operands[1], 0)) == PLUS
      && REG_P (XEXP (XEXP (operands[1], 0), 0))
      && CONST_INT_P (XEXP (XEXP (operands[1], 0), 1))
      && CONST_INT_P (operands[2])
      && CONST_INT_P (operands[3]))
    {
      HOST_WIDE_INT o = INTVAL (XEXP (XEXP (operands[1], 0), 1));
      HOST_WIDE_INT l = INTVAL (operands[2]);
      HOST_WIDE_INT v = INTVAL (operands[3]);
      if ((o & 3) && (o & 3) * 8 + v + l <= 32)
	{
	  rtx tmp;
	  tmp = XEXP (XEXP (operands[1], 0), 0);
	  if (o & ~3)
	    tmp = gen_rtx_PLUS (SImode, tmp, GEN_INT (o & ~3));
	  operands[1] = gen_rtx_MEM (QImode, tmp);
	  operands[3] = GEN_INT (v + (o & 3) * 8);
	}
      if (optimize_size)
	return \"extzv %3,%2,%1,%0\";
    }
  return \"rotl %R3,%1,%0\;bicl2 %M2,%0\";
}")

;; Combine EXTV/CMPL and EXTZV/CMPL sequences where the output of
;; extraction is used for the comparison only into CMPV and CMPZV
;; respectively.
(define_peephole2
  [(parallel
     [(set (match_operand:SI 0 "register_operand")
	   (any_extract:SI (match_operand 1 "general_operand")
			   (match_operand:QI 2 "general_operand")
			   (match_operand:SI 3 "general_operand")))
      (clobber (reg:CC VAX_PSL_REGNUM))])
   (set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc (match_dup 0)
		       (match_operand:SI 4 "general_operand")))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc
	  (any_extract:SI (match_dup 1)
			  (match_dup 2)
			  (match_dup 3))
	  (match_dup 4)))]
  "")

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "")
			 (match_operand:QI 1 "general_operand" "")
			 (match_operand:SI 2 "general_operand" ""))
	(match_operand:SI 3 "general_operand" ""))]
  ""
  "")

;; This one actually doesn't change CC.
(define_insn "*insv"
  [(set (zero_extract:SI (match_operand:QI 0 "memory_operand" "+m")
			 (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "nrmT"))
	(match_operand:SI 3 "general_operand" "nrmT"))]
  ""
  "*
{
  if (MEM_P (operands[0])
      && GET_CODE (XEXP (operands[0], 0)) == PLUS
      && REG_P (XEXP (XEXP (operands[0], 0), 0))
      && CONST_INT_P (XEXP (XEXP (operands[0], 0), 1))
      && CONST_INT_P (operands[1])
      && CONST_INT_P (operands[2]))
    {
      HOST_WIDE_INT o = INTVAL (XEXP (XEXP (operands[0], 0), 1));
      HOST_WIDE_INT v = INTVAL (operands[2]);
      HOST_WIDE_INT l = INTVAL (operands[1]);
      if ((o & 3) && (o & 3) * 8 + v + l <= 32)
	{
	  rtx tmp;
	  tmp = XEXP (XEXP (operands[0], 0), 0);
	  if (o & ~3)
	    tmp = gen_rtx_PLUS (SImode, tmp, GEN_INT (o & ~3));
	  operands[0] = gen_rtx_MEM (QImode, tmp);
	  operands[2] = GEN_INT (v + (o & 3) * 8);
	}
    }
  return \"insv %3,%2,%1,%0\";
}")

;; This one actually doesn't change CC.
(define_insn "*insv_2"
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+ro")
			 (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "nrmT"))
	(match_operand:SI 3 "general_operand" "nrmT"))]
  ""
  "insv %3,%2,%1,%0")

;; Unconditional jump
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jbr %l0")

;; Conditional jumps

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "ordered_comparison_operator"
			  [(match_operand:VAXint 1 "general_operand" "")
			   (match_operand:VAXint 2 "general_operand" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
  "")

(define_insn_and_split "*cbranch<VAXint:mode>4_<VAXcc:mode>"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "vax_<VAXcc:mode>_comparison_operator"
			  [(match_operand:VAXint 1 "general_operand" "nrmT")
			   (match_operand:VAXint 2 "general_operand" "nrmT")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
  "#"
  "reload_completed"
  [(set (reg:VAXcc VAX_PSL_REGNUM)
	(compare:VAXcc (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else
	  (match_op_dup 0 [(reg:VAXcc VAX_PSL_REGNUM)
			   (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "")

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "ordered_comparison_operator"
			  [(match_operand:VAXfp 1 "general_operand" "")
			   (match_operand:VAXfp 2 "general_operand" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
  "")

(define_insn_and_split "*cbranch<VAXfp:mode>4_<VAXccnz:mode>"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "vax_<VAXccnz:mode>_comparison_operator"
			  [(match_operand:VAXfp 1 "general_operand" "gF")
			   (match_operand:VAXfp 2 "general_operand" "gF")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
  "#"
  "reload_completed"
  [(set (reg:VAXccnz VAX_PSL_REGNUM)
	(compare:VAXccnz (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else
 	  (match_op_dup 0 [(reg:VAXccnz VAX_PSL_REGNUM)
			   (const_int 0)])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  "")

(define_insn "*branch_<mode>"
  [(set (pc)
	(if_then_else (match_operator 0 "vax_<mode>_comparison_operator"
				      [(reg:VAXcc VAX_PSL_REGNUM)
				       (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "reload_completed"
  "j%k0 %l1")

;; Recognize reversed jumps.
(define_insn "*branch_<mode>_reversed"
  [(set (pc)
	(if_then_else (match_operator 0 "vax_<mode>_comparison_operator"
				      [(reg:VAXcc VAX_PSL_REGNUM)
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  "reload_completed"
  "j%K0 %l1") ; %K0 negates condition

;; Recognize jbs, jlbs, jbc and jlbc instructions.  Note that the operand
;; of jlbs and jlbc insns are SImode in the hardware.  However, if it is
;; memory, we use QImode in the insn.  So we can't use those instructions
;; for mode-dependent addresses.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:QI 0 "memory_operand" "Q,g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,nrmT"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbs %0,%l2
   jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:QI 0 "memory_operand" "Q,g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,nrmT"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbc %0,%l2
   jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r,r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,nrmT"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbs %0,%l2
   jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r,r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,nrmT"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbc %0,%l2
   jbc %1,%0,%l2")

;; Subtract-and-jump and Add-and-jump insns.
;; These are not used when output is for the Unix assembler
;; because it does not know how to modify them to reach far.

;; Normal sob insns.

(define_insn_and_split "*jsobgtr"
  [(set (pc)
	(if_then_else
	 (gt (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (gt (plus:SI (match_dup 0)
			 (const_int -1))
		(const_int 0))
	    (label_ref (match_dup 1))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int -1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jsobgtr_2"
  [(set (pc)
	(if_then_else
	 (gt (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && reload_completed"
  "jsobgtr %0,%l1")

(define_insn_and_split "*jsobgeq"
  [(set (pc)
	(if_then_else
	 (ge (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (ge (plus:SI (match_dup 0)
			 (const_int -1))
		(const_int 0))
	    (label_ref (match_dup 1))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int -1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jsobgeq_2"
  [(set (pc)
	(if_then_else
	 (ge (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && reload_completed"
  "jsobgeq %0,%l1")

;; Normal aob insns.  Define a version for when operands[1] is a constant.
(define_insn_and_split "*jaoblss"
  [(set (pc)
	(if_then_else
	 (lt (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (lt (plus:SI (match_dup 0)
			 (const_int 1))
		(match_dup 1))
	    (label_ref (match_dup 2))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jaoblss_2"
  [(set (pc)
	(if_then_else
	 (lt (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && reload_completed"
  "jaoblss %1,%0,%l2")

(define_insn_and_split "*jaoblss_const"
  [(set (pc)
	(if_then_else
	 (lt (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM && CONST_INT_P (operands[1])"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (lt (match_dup 0)
		(match_dup 1))
	    (label_ref (match_dup 2))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jaoblss_const_2"
  [(set (pc)
	(if_then_else
	 (lt (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && CONST_INT_P (operands[1]) && reload_completed"
  "jaoblss %P1,%0,%l2")

(define_insn_and_split "*jaobleq"
  [(set (pc)
	(if_then_else
	 (le (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (le (plus:SI (match_dup 0)
			 (const_int 1))
		(match_dup 1))
	    (label_ref (match_dup 2))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jaobleq_2"
  [(set (pc)
	(if_then_else
	 (le (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && reload_completed"
  "jaobleq %1,%0,%l2")

(define_insn_and_split "*jaobleq_const"
  [(set (pc)
	(if_then_else
	 (le (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM && CONST_INT_P (operands[1])"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (le (match_dup 0)
		(match_dup 1))
	    (label_ref (match_dup 2))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int 1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jaobleq_const_2"
  [(set (pc)
	(if_then_else
	 (le (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (match_operand:SI 1 "general_operand" "nrmT"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "!TARGET_UNIX_ASM && CONST_INT_P (operands[1]) && reload_completed"
  "jaobleq %P1,%0,%l2")

;; Something like a sob insn, but compares against -1.
;; This finds `while (foo--)' which was changed to `while (--foo != -1)'.

(define_insn_and_split "*jsobneq_minus_one"
  [(set (pc)
	(if_then_else
	 (ne (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (pc)
	   (if_then_else
	    (ne (match_dup 0)
		(const_int 0))
	    (label_ref (match_dup 1))
	    (pc)))
      (set (match_dup 0)
	   (plus:SI (match_dup 0)
		    (const_int -1)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*jsobneq_minus_one_2"
  [(set (pc)
	(if_then_else
	 (ne (match_operand:SI 0 "nonimmediate_operand" "+g")
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "decl %0\;jgequ %l1")

(define_expand "call_pop"
  [(parallel [(call (match_operand:QI 0 "memory_operand" "")
		    (match_operand:SI 1 "const_int_operand" ""))
	      (set (reg:SI VAX_SP_REGNUM)
		   (plus:SI (reg:SI VAX_SP_REGNUM)
			    (match_operand:SI 3 "immediate_operand" "")))])]
  ""
{
  gcc_assert (INTVAL (operands[3]) <= 255 * 4 && INTVAL (operands[3]) % 4 == 0);

  /* Operand 1 is the number of bytes to be popped by DW_CFA_GNU_args_size
     during EH unwinding.  We must include the argument count pushed by
     the calls instruction.  */
  operands[1] = GEN_INT (INTVAL (operands[3]) + 4);
})

(define_insn "*call_pop"
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:SI 1 "const_int_operand" "n"))
   (set (reg:SI VAX_SP_REGNUM) (plus:SI (reg:SI VAX_SP_REGNUM)
					(match_operand:SI 2 "immediate_operand" "i")))]
  ""
{
  operands[1] = GEN_INT ((INTVAL (operands[1]) - 4) / 4);
  return "calls %1,%0";
})

(define_expand "call_value_pop"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:QI 1 "memory_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")))
	      (set (reg:SI VAX_SP_REGNUM)
		   (plus:SI (reg:SI VAX_SP_REGNUM)
			    (match_operand:SI 4 "immediate_operand" "")))])]
  ""
{
  gcc_assert (INTVAL (operands[4]) <= 255 * 4 && INTVAL (operands[4]) % 4 == 0);

  /* Operand 2 is the number of bytes to be popped by DW_CFA_GNU_args_size
     during EH unwinding.  We must include the argument count pushed by
     the calls instruction.  */
  operands[2] = GEN_INT (INTVAL (operands[4]) + 4);
})

(define_insn "*call_value_pop"
  [(set (match_operand 0 "" "")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:SI 2 "const_int_operand" "n")))
   (set (reg:SI VAX_SP_REGNUM) (plus:SI (reg:SI VAX_SP_REGNUM)
					(match_operand:SI 3 "immediate_operand" "i")))]
  ""
  "*
{
  operands[2] = GEN_INT ((INTVAL (operands[2]) - 4) / 4);
  return \"calls %2,%1\";
}")

(define_expand "call"
  [(call (match_operand:QI 0 "memory_operand" "")
      (match_operand:SI 1 "const_int_operand" ""))]
  ""
  "
{
  /* Operand 1 is the number of bytes to be popped by DW_CFA_GNU_args_size
     during EH unwinding.  We must include the argument count pushed by
     the calls instruction.  */
  operands[1] = GEN_INT (INTVAL (operands[1]) + 4);
}")

(define_insn "*call"
   [(call (match_operand:QI 0 "memory_operand" "m")
	  (match_operand:SI 1 "const_int_operand" ""))]
  ""
  "calls $0,%0")

(define_expand "call_value"
  [(set (match_operand 0 "" "")
      (call (match_operand:QI 1 "memory_operand" "")
	    (match_operand:SI 2 "const_int_operand" "")))]
  ""
  "
{
  /* Operand 2 is the number of bytes to be popped by DW_CFA_GNU_args_size
     during EH unwinding.  We must include the argument count pushed by
     the calls instruction.  */
  operands[2] = GEN_INT (INTVAL (operands[2]) + 4);
}")

(define_insn "*call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:SI 2 "const_int_operand" "")))]
  ""
  "calls $0,%1")

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
	      (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call_pop (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_BLOCKAGE)]
  ""
  "")

(define_insn "procedure_entry_mask"
  [(unspec_volatile [(match_operand 0 "const_int_operand")] VUNSPEC_PEM)]
  ""
  ".word %x0")

(define_insn "return"
  [(return)]
  ""
  "ret")

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  vax_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  emit_jump_insn (gen_return ());
  DONE;
}")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; This had a wider constraint once, and it had trouble.
;; If you are tempted to try `g', please don't--it's not worth
;; the risk we will reopen the same bug.
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp (%0)")

;; This is here to accept 5 arguments (as passed by expand_end_case)
;; and pass the first 4 along to the casesi1 pattern that really does
;; the actual casesi work.  We emit a jump here to the default label
;; _before_ the casesi so that we can be sure that the casesi never
;; drops through.
;; This is suboptimal perhaps, but so is much of the rest of this
;; machine description.  For what it's worth, HPPA uses the same trick.
;;
;; operand 0 is index
;; operand 1 is the minimum bound (a const_int)
;; operand 2 is the maximum bound - minimum bound + 1 (also a const_int)
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range (ie. default).
;;
;; We emit:
;;	i = index - minimum_bound
;;	if (i > (maximum_bound - minimum_bound + 1) goto default;
;;	casesi (i, 0, table);
;;
(define_expand "casesi"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand 3 "" "")
   (match_operand 4 "" "")]
  ""
{
  rtx test;

  /* i = index - minimum_bound;
     But only if the lower bound is not already zero.  */
  if (operands[1] != const0_rtx)
    {
      rtx index = gen_reg_rtx (SImode);
      emit_insn (gen_addsi3 (index,
			     operands[0],
			     gen_int_mode (-INTVAL (operands[1]), SImode)));
      operands[0] = index;
    }

  /* if (i > (maximum_bound - minimum_bound + 1)) goto default;  */
  test = gen_rtx_fmt_ee (GTU, VOIDmode, operands[0], operands[2]);
  emit_jump_insn (gen_cbranchsi4 (test, operands[0], operands[2], operands[4]));

  /* casesi (i, 0, table);  */
  emit_jump_insn (gen_casesi1 (operands[0], operands[2], operands[3]));
  DONE;
})

;; This insn is a bit of a lier.  It actually falls through if no case
;; matches.  But, we prevent that from ever happening by emitting a jump
;; before this, see the define_expand above.
(define_insn_and_split "casesi1"
  [(match_operand:SI 1 "const_int_operand" "n")
   (set (pc)
	(plus:SI (sign_extend:SI
		   (mem:HI (plus:SI
			     (mult:SI
			       (match_operand:SI 0 "general_operand" "nrmT")
			       (const_int 2))
			     (pc))))
		 (label_ref:SI (match_operand 2 "" ""))))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(match_dup 1)
      (set (pc)
	   (plus:SI (sign_extend:SI
		      (mem:HI (plus:SI
				(mult:SI
				  (match_dup 0)
				  (const_int 2))
				(pc))))
		    (label_ref:SI (match_dup 2))))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*casesi1"
  [(match_operand:SI 1 "const_int_operand" "n")
   (set (pc)
	(plus:SI (sign_extend:SI
		   (mem:HI (plus:SI
			     (mult:SI
			       (match_operand:SI 0 "general_operand" "nrmT")
			       (const_int 2))
			     (pc))))
		 (label_ref:SI (match_operand 2 "" ""))))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "casel %0,$0,%1")

(define_insn_and_split "*pushsym"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:SI 1 "pic_symbolic_operand" "A"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*pushsym_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:SI 1 "pic_symbolic_operand" "A"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "pushab %a1")

(define_insn_and_split "*movsym"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:SI 1 "pic_symbolic_operand" "A"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movsym_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:SI 1 "pic_symbolic_operand" "A"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "movab %a1,%0")

(define_insn_and_split "*pushsymreg"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(plus:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "pic_symbolic_operand" "A")))]
  "flag_pic"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:SI (match_dup 1)
		    (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*pushsymreg_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(plus:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "pic_symbolic_operand" "A")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "flag_pic && reload_completed"
  "pushab %a2[%1]")

(define_insn_and_split "*movsymreg"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(plus:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "pic_symbolic_operand" "A")))]
  "flag_pic"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (plus:SI (match_dup 1)
		    (match_dup 2)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movsymreg_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(plus:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "pic_symbolic_operand" "A")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "flag_pic && reload_completed"
  "movab %a2[%1],%0")

;;- load or push effective address
;; These come after the move and add/sub patterns
;; because we don't want pushl $1 turned into pushad 1.
;; or addl3 r1,r2,r3 turned into movab 0(r1)[r2],r3.

;; It does not work to use constraints to distinguish pushes from moves,
;; because < matches any autodecrement, not just a push.

(define_insn_and_split "pushaddr<mode>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:VAXintQHSD 1 "address_operand" "p"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*pushaddr<mode><ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:VAXintQHSD 1 "address_operand" "p"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "pusha<VAXintQHSD:isfx> %a1")

(define_insn_and_split "movaddr<mode>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:VAXintQHSD 1 "address_operand" "p"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movaddr<mode><ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:VAXintQHSD 1 "address_operand" "p"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "mova<VAXintQHSD:isfx> %a1,%0")

(define_insn_and_split "pushaddr<mode>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:VAXfp 1 "address_operand" "p"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*pushaddr<mode><ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "push_operand" "=g")
	(match_operand:VAXfp 1 "address_operand" "p"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "pusha<VAXfp:fsfx> %a1")

(define_insn_and_split "movaddr<mode>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:VAXfp 1 "address_operand" "p"))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (match_dup 1))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*movaddr<mode>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:VAXfp 1 "address_operand" "p"))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "mova<VAXfp:fsfx> %a1,%0")

;; These used to be peepholes, but it is more straightforward to do them
;; as single insns.  However, we must force the output to be a register
;; if it is not an offsettable address so that we know that we can assign
;; to it twice.

;; If we had a good way of evaluating the relative costs, these could be
;; machine-independent.

;; Optimize   extzv ...,z;    andl2 ...,z
;; or	      ashl ...,z;     andl2 ...,z
;; with other operands constant.  This is what the combiner converts the
;; above sequences to before attempting to recognize the new insn.

(define_insn_and_split "*andashlnegsi4"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro")
	(and:SI (ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
			     (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[3]) & ~((1 << (32 - INTVAL (operands[2]))) - 1)) == 0"
  "#"
  "&& reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (and:SI (ashiftrt:SI (match_dup 1)
				(match_dup 2))
		   (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*andashlnegsi4_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro")
	(and:SI (ashiftrt:SI (match_operand:SI 1 "general_operand" "nrmT")
			     (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "(INTVAL (operands[3]) & ~((1 << (32 - INTVAL (operands[2]))) - 1)) == 0
   && reload_completed"
  "*
{
  unsigned long mask1 = INTVAL (operands[3]);
  unsigned long mask2 = (1 << (32 - INTVAL (operands[2]))) - 1;

  if ((mask1 & mask2) != mask1)
    operands[3] = GEN_INT (mask1 & mask2);

  return \"rotl %R2,%1,%0\;bicl2 %N3,%0\";
}")

;; left-shift and mask
;; The only case where `ashl' is better is if the mask only turns off
;; bits that the ashl would anyways, in which case it should have been
;; optimized away.

(define_insn_and_split "*andashlsi4"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro")
	(and:SI (ashift:SI (match_operand:SI 1 "general_operand" "nrmT")
			   (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))]
  ""
  "#"
  "reload_completed"
  [(parallel
     [(set (match_dup 0)
	   (and:SI (ashift:SI (match_dup 1)
			      (match_dup 2))
		   (match_dup 3)))
      (clobber (reg:CC VAX_PSL_REGNUM))])]
  "")

(define_insn "*andashlsi4_2<ccn><ccnz><ccz>"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro")
	(and:SI (ashift:SI (match_operand:SI 1 "general_operand" "nrmT")
			   (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))
   (clobber (reg:CC VAX_PSL_REGNUM))]
  "reload_completed"
  "*
{
  operands[3]
    = GEN_INT (INTVAL (operands[3]) & ~((1 << INTVAL (operands[2])) - 1));
  return \"rotl %2,%1,%0\;bicl2 %N3,%0\";
}")

;; Instruction sequence to sync the VAX instruction stream.
(define_insn "sync_istream"
  [(unspec_volatile [(const_int 0)] VUNSPEC_SYNC_ISTREAM)]
  ""
  "movpsl -(%|sp)\;pushal 1(%|pc)\;rei")

(define_expand "nonlocal_goto"
  [(use (match_operand 0 "general_operand" ""))
   (use (match_operand 1 "general_operand" ""))
   (use (match_operand 2 "general_operand" ""))
   (use (match_operand 3 "general_operand" ""))]
  ""
{
  rtx lab = operands[1];
  rtx stack = operands[2];
  rtx fp = operands[3];

  emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

  emit_move_insn (hard_frame_pointer_rtx, fp);
  emit_stack_restore (SAVE_NONLOCAL, stack);

  emit_use (hard_frame_pointer_rtx);
  emit_use (stack_pointer_rtx);

  /* We'll convert this to direct jump via a peephole optimization.  */
  emit_indirect_jump (copy_to_reg (lab));
  emit_barrier ();
  DONE;
})

(include "builtins.md")
