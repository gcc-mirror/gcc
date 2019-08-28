;; Machine description for arc64 architecture.
;; Copyright (C) 2019 Free Software Foundation, Inc.
;; Contributed by Claudiu Zissulescu
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Place holder for mov operand predicate
(define_predicate "arc64_movl_operand"
  (and (match_code "unspec,reg, subreg, mem, const, const_int, symbol_ref, label_ref")
       (ior (match_operand 0 "register_operand")
	    (and (ior (match_code "label_ref")
		      (match_code "symbol_ref"))
		 (match_test "arc64_allow_direct_access_p (op)"))
	    (match_operand 0 "memory_operand")
	    (and (match_code "unspec")
		 (ior (match_test "XINT (op,1) == ARC64_UNSPEC_PCREL")
		      (match_test "XINT (op,1) == ARC64_UNSPEC_TLS_GD")))
	    (and (match_code "const_int")
		 (ior (match_test "UNSIGNED_INT32 (INTVAL (op))")
		      (match_test "SIGNED_INT32 (INTVAL (op))"))))))

(define_predicate "arc64_movf_operand"
  (and (match_code "reg, subreg, mem, const, const_double")
       (ior (match_operand 0 "register_operand")
	    (match_operand 0 "memory_operand")
	    (and (match_code "const_double")
		 (ior (match_test "GET_MODE_SIZE (GET_MODE (op)) <= 4")
		      (match_test "op == CONST0_RTX (DFmode)"))))))

;; A restricted version of the above, still accepting symbols and label refs.
(define_predicate "arc64_regsym_operand"
  (ior (match_operand 0 "register_operand")
       (and (ior (match_code "label_ref")
		 (match_code "symbol_ref"))
	    (match_test "arc64_allow_direct_access_p (op)"))
       (and (match_code "const_int")
	    (ior (match_test "UNSIGNED_INT32 (INTVAL (op))")
		 (match_test "SIGNED_INT32 (INTVAL (op))")))))

(define_predicate "arc64_nonmem_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (ior (match_test "UNSIGNED_INT32 (INTVAL (op))")
		 (match_test "SIGNED_INT32 (INTVAL (op))")))))

(define_predicate "arc64_reg_or_unsig_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "UNSIGNED_INT32 (INTVAL (op))")
	    (match_test "INTVAL (op) > 0"))))

;; Used for HIGH or LO_SUM patterns
(define_predicate "arc64_immediate_or_pic"
  (ior (match_operand 0 "immediate_operand")
       (match_code "unspec")))

;; Acceptable arguments for the call insn.
(define_predicate "arc64_call_insn_operand"
  (ior (and (match_code "symbol_ref")
	    (match_test "!arc64_is_long_call_p (op)"))
       (match_operand 0 "nonmemory_operand")))

; to be used by <op>.f instructions
(define_special_predicate "cc_compare_operator"
  (match_code "compare")
  {
   return GET_MODE (op) == CC_Cmode
	  || GET_MODE (op) == CC_Vmode;
  })

; to be used for b{eq/ne}_s instructions.
(define_predicate "equality_comparison_operator"
  (match_code "eq, ne")
  {
   machine_mode opmode = GET_MODE (XEXP (op, 0));
   return (opmode != CC_Vmode);
  })


; to be used for b{eq/ne/...}_s instructions.
(define_predicate "ccmode_comparison_operator"
  (match_code "eq, ne, gt, ge, lt, le, gtu, geu, ltu, leu,
	       unlt, unle, unge, ungt")
  {
   enum rtx_code code = GET_CODE (op);

   switch (GET_MODE (XEXP (op, 0)))
   {
     case E_CC_FPUmode:
     case E_CC_FPUEmode:
     case E_CCmode:
       return 1;

     case E_CC_ZNmode:
       return (code == EQ || code == NE);

     default:
       return 0;
     }
   })

(define_predicate "core_register_operand"
  (match_code "reg,subreg")
  {
   if (GET_CODE (op) == SUBREG)
     op = SUBREG_REG (op);
   return (REG_P (op)
	   && (REGNO (op) <= BLINK_REGNUM
	       || (REGNO (op)) >= FIRST_PSEUDO_REGISTER));
  })


;; True for integer comparisons and for FP comparisons other then LTGT or UNEQ
(define_special_predicate "arc64_comparison_operator"
  (match_code "eq, ne, le, lt, ge, gt, geu, gtu, leu, ltu, unordered,
	       ordered, unlt, unle, unge, ungt"))

(define_special_predicate "cc_register"
  (match_code "reg")
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (op);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (mode == GET_MODE (op) && GET_CODE (op) == REG && REGNO (op) == CC_REGNUM)
    return TRUE;

  return FALSE;
})

;; Return TRUE if OP is a shimm 6bit value
(define_predicate "S06S0_immediate_operand"
  (and (match_code "const_int")
       (match_test "SIGNED_INT6 (INTVAL (op))"))
)

(define_predicate "vectdup_immediate_operand"
  (and (match_code "const_int")
       (ior (match_test "SIGNED_INT6 (INTVAL (op))")
	    (match_test "UNSIGNED_INT12 (INTVAL (op))")))
)

;; Return true if OP is a MEM that when used as a load or store address will
;; require an 8 byte insn.
;; Load and store instructions don't allow the same possibilities but they're
;; similar enough that this one function will do.
;; This is currently only used when calculating length attributes.  */
(define_predicate "limm_ldst_operand"
  (and (match_code "mem")
       (match_test "arc64_limm_addr_p (op)")))

;; Allows only 1,2,3 values.  It is used with add/sub shifted operations.
(define_predicate "_1_2_3_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2 || INTVAL (op) == 3"))
)

;; Allows only 2,4,8 values.  It is used with add/sub shifted operations.
(define_predicate "_2_4_8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8"))
)

;; Return TRUE if OP can be used as a destination for any move
;; (mov,st,ld) instruction.
(define_predicate "arc64_dest_operand"
  (match_code "reg, subreg, mem")
  {
   if (MEM_P (op))
      return arc64_legitimate_store_address_p (mode, XEXP (op, 0));
   return nonimmediate_operand (op, mode);
  })

(define_predicate "mem_noofs_operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

;; Used by vector floating point instructions.
(define_predicate "arc64_fsimd_register"
  (match_code "reg,subreg")
  {
    if (GET_CODE (op) == SUBREG)
      op = SUBREG_REG (op);

    if (REGNO (op) >= FIRST_PSEUDO_REGISTER)
      return 1;

    /* Check if it is a register. */
    if (!REG_P (op))
      return 0;

    /* FIXME! check: REGNO_REG_CLASS (REGNO (op)) != FP_REGS */

    /* Return true/false depending on the SIMD length.  */
    switch (mode)
      {
	/* All vector modes equal with the size of a fp-register.  */
      case E_V2SFmode:
      case E_V4HFmode:
      case E_V2HFmode:
	return 1;

	/* All vector modes double the size of a fp-register.  */
      case E_V8HFmode:
      case E_V4SFmode:
      case E_V2DFmode:
	return (ARC64_VFP_128 && ((REGNO (op) & 0x01) == 0));

      default:
	gcc_unreachable ();
      }
  })

(define_predicate "arc64_fsimd_moperand"
  (ior (match_operand 0 "arc64_fsimd_register")
       (match_operand 0 "memory_operand")))

(define_predicate "short_immediate_operand"
  (and (match_code "const_int")
       (match_test "SIGNED_INT16 (INTVAL (op))")))

(define_predicate "unsign_immediate_operand"
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT16 (INTVAL (op))")))

(define_predicate "usigned32b_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) > 0")
       (match_test "UNSIGNED_INT32 (INTVAL (op))")))

(define_predicate "signed32b_operand"
  (and (match_code "const_int")
       (match_test "SIGNED_INT32 (INTVAL (op))")))

(define_predicate "bbitimm_operand"
  (and (match_code "const_int")
       (match_test "IS_POWEROF2_P (INTVAL (op))")))

(define_special_predicate "brcc_comparison_operator"
  (match_code "eq, ne, lt, ge, ltu, geu"))

(define_special_predicate "ebrcc_comparison_operator"
  (match_code "gt, gtu, le, leu"))

;; Return true if the symbol requires a @plt34 reloc
(define_predicate "plt34_symbol_p"
  (and (match_code "symbol_ref")
       (match_test "arc64_use_plt34_p (op)")))

;; Return true if OP a (const_int 0) operand.
(define_predicate "const0_operand"
  (and (match_code "const_int")
       (match_test "op == CONST0_RTX (mode)")))
