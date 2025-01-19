;; Predicate definitions for S/390 and zSeries.
;; Copyright (C) 2005-2025 Free Software Foundation, Inc.
;; Contributed by Hartmut Penner (hpenner@de.ibm.com) and
;;                Ulrich Weigand (uweigand@de.ibm.com).
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; OP is the current operation.
;; MODE is the current operation mode.

;; operands --------------------------------------------------------------

;; Return true if OP a const 0 operand (int/float/vector).
(define_predicate "const0_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Return true if OP an all ones operand (int/vector).
(define_predicate "all_ones_operand"
  (and (match_code "const_int, const_wide_int, const_vector")
       (match_test "INTEGRAL_MODE_P (GET_MODE (op))")
       (match_test "op == CONSTM1_RTX (mode)")))

;; Return true if OP is a 4 bit mask operand
(define_predicate "const_mask_operand"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) < 16")))

;; Return true if OP is constant.

(define_special_predicate "consttable_operand"
  (and (match_code "symbol_ref, label_ref, const, const_int, const_wide_int, const_double, const_vector")
       (match_test "CONSTANT_P (op)")))

; An operand used as vector permutation pattern

; This in particular accepts constants which would otherwise be
; rejected.  These constants require special post reload handling

(define_special_predicate "permute_pattern_operand"
  (and (match_code "const_vector,mem,reg,subreg")
       (match_test "GET_MODE (op) == V16QImode")
       (match_test "!MEM_P (op) || s390_mem_constraint (\"R\", op)")))

;; Return true if OP is a valid S-type operand.

(define_predicate "s_operand"
  (and (match_code "subreg, mem")
       (match_operand 0 "general_operand"))
{
  /* Just like memory_operand, allow (subreg (mem ...))
     after reload.  */
  if (reload_completed
      && GET_CODE (op) == SUBREG
      && GET_CODE (SUBREG_REG (op)) == MEM)
    op = SUBREG_REG (op);

  if (GET_CODE (op) != MEM)
    return false;
  if (!s390_legitimate_address_without_index_p (op))
    return false;

  return true;
})

;; Return true of the address of the mem operand plus 16 is still a
;; valid Q constraint address.

(define_predicate "plus16_Q_operand"
  (and (match_code "mem")
       (match_operand 0 "general_operand"))
{
  rtx addr = XEXP (op, 0);
  if (REG_P (addr))
    return true;

  if (GET_CODE (addr) != PLUS
      || !REG_P (XEXP (addr, 0))
      || !CONST_INT_P (XEXP (addr, 1)))
    return false;

  return SHORT_DISP_IN_RANGE (INTVAL (XEXP (addr, 1)) + 16);
})

;; Return true if OP is a valid operand for the BRAS instruction.
;; Allow SYMBOL_REFs and @PLT stubs.

(define_special_predicate "bras_sym_operand"
  (ior (and (match_code "symbol_ref")
	    (ior (match_test "!flag_pic")
		 (match_test "SYMBOL_REF_LOCAL_P (op)")
		 (and (match_test "TARGET_64BIT")
		      (match_test "SYMBOL_REF_FUNCTION_P (op)"))))
       (and (match_code "const")
	    (and (match_test "GET_CODE (XEXP (op, 0)) == UNSPEC")
		 (match_test "XINT (XEXP (op, 0), 1) == UNSPEC_PLT31")))))

;; Return true if OP is a PLUS that is not a legitimate
;; operand for the LA instruction.

(define_predicate "s390_plus_operand"
  (and (match_code "plus")
       (and (match_test "mode == Pmode")
	    (match_test "!legitimate_la_operand_p (op)"))))

;; Return true if OP is a valid operand as scalar shift count or setmem.

(define_predicate "setmem_operand"
  (match_code "reg, subreg, plus, const_int")
{
  HOST_WIDE_INT offset;
  rtx base;

  if (GET_MODE (op) != VOIDmode
      && GET_MODE_CLASS (GET_MODE (op)) != MODE_INT)
    return false;

  /* Extract base register and offset.  */
  if (!s390_decompose_addrstyle_without_index (op, &base, &offset))
    return false;

  /* Don't allow any non-base hard registers.  Doing so without
     confusing reload and/or regrename would be tricky, and doesn't
     buy us much anyway.  */
  if (base && REGNO (base) < FIRST_PSEUDO_REGISTER && !ADDR_REG_P (base))
    return false;

  /* Unfortunately we have to reject constants that are invalid
     for an address, or else reload will get confused.  */
  if (!DISP_IN_RANGE (offset))
    return false;

  return true;
})

; An integer operand with the lowest order 6 bits all ones.
(define_predicate "const_int_6bitset_operand"
 (and (match_code "const_int")
      (match_test "(INTVAL (op) & 63) == 63")))
(define_predicate "nonzero_shift_count_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, GET_MODE_BITSIZE (mode) - 1)")))

;;  Return true if OP a valid operand for the LARL instruction.

(define_predicate "larl_operand"
  (match_code "label_ref, symbol_ref, const")
{
  /* Allow labels and local symbols.  */
  if (GET_CODE (op) == LABEL_REF)
    return true;
  if (SYMBOL_REF_P (op))
    return (!SYMBOL_FLAG_NOTALIGN2_P (op)
	    && SYMBOL_REF_TLS_MODEL (op) == 0
	    && s390_rel_address_ok_p (op));

  /* Everything else must have a CONST, so strip it.  */
  if (GET_CODE (op) != CONST)
    return false;
  op = XEXP (op, 0);

  /* Allow adding *even* in-range constants.  */
  if (GET_CODE (op) == PLUS)
    {
      if (GET_CODE (XEXP (op, 1)) != CONST_INT
          || (INTVAL (XEXP (op, 1)) & 1) != 0)
        return false;
      if (INTVAL (XEXP (op, 1)) >= HOST_WIDE_INT_1 << 31
	  || INTVAL (XEXP (op, 1)) < -(HOST_WIDE_INT_1 << 31))
        return false;
      op = XEXP (op, 0);
    }

  /* Labels and local symbols allowed here as well.  */
  if (GET_CODE (op) == LABEL_REF)
    return true;
  if (SYMBOL_REF_P (op))
    return (!SYMBOL_FLAG_NOTALIGN2_P (op)
	    && SYMBOL_REF_TLS_MODEL (op) == 0
	    && s390_rel_address_ok_p (op));


  /* Now we must have a @GOTENT offset or @PLT stub
     or an @INDNTPOFF TLS offset.  */
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == UNSPEC_GOTENT)
    return true;
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == UNSPEC_PLT31)
    return true;
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == UNSPEC_INDNTPOFF)
    return true;

  return false;
})

; Predicate that always allows wraparound of the one-bit range.
(define_predicate "contiguous_bitmask_operand"
  (match_code "const_int")
{
  return s390_contiguous_bitmask_p (INTVAL (op), true,
                                    GET_MODE_BITSIZE (mode), NULL, NULL);
})

; Same without wraparound.
(define_predicate "contiguous_bitmask_nowrap_operand"
  (match_code "const_int")
{
  return s390_contiguous_bitmask_p
    (INTVAL (op), false, GET_MODE_BITSIZE (mode), NULL, NULL);
})

;; Return true if OP is legitimate for any LOC instruction.

(define_predicate "loc_operand"
  (ior (match_operand 0 "nonimmediate_operand")
      (and (match_code "const_int")
	   (match_test "INTVAL (op) <= 32767 && INTVAL (op) >= -32768"))))

(define_predicate "reload_const_wide_int_operand"
  (and (match_code "const_wide_int")
       (match_test "legitimate_reload_constant_p (op)")))


;; operators --------------------------------------------------------------

;; Return nonzero if OP is a valid comparison operator
;; for a branch condition.

(define_predicate "s390_comparison"
  (match_code "eq, ne, lt, gt, le, ge, ltu, gtu, leu, geu,
	       uneq, unlt, ungt, unle, unge, ltgt,
	       unordered, ordered")
{
  if (GET_CODE (XEXP (op, 0)) != REG
      || REGNO (XEXP (op, 0)) != CC_REGNUM
      || (XEXP (op, 1) != const0_rtx
          && !(CONST_INT_P (XEXP (op, 1))
	       && GET_MODE (XEXP (op, 0)) == CCRAWmode
	       && INTVAL (XEXP (op, 1)) >= 0
               && INTVAL (XEXP (op, 1)) <= 15)))
    return false;

  return (s390_branch_condition_mask (op) >= 0);
})

;; Return true if op is the cc register.
(define_predicate "cc_reg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == CC_REGNUM")))

(define_predicate "s390_signed_integer_comparison"
  (match_code "eq, ne, lt, gt, le, ge")
{
  return (s390_compare_and_branch_condition_mask (op) >= 0);
})

(define_predicate "s390_unsigned_integer_comparison"
  (match_code "eq, ne, ltu, gtu, leu, geu")
{
  return (s390_compare_and_branch_condition_mask (op) >= 0);
})

;; Return nonzero if OP is a valid comparison operator for the
;; cstore expanders -- respectively cstorecc4 and integer cstore.
(define_predicate "s390_eqne_operator"
  (match_code "eq, ne"))

(define_predicate "s390_scond_operator"
  (match_code "ltu, gtu, leu, geu"))

(define_predicate "s390_brx_operator"
  (match_code "le, gt"))

;; Return nonzero if OP is a valid comparison operator
;; for an ALC condition.

(define_predicate "s390_alc_comparison"
  (match_code "zero_extend, sign_extend, ltu, gtu, leu, geu")
{
  while (GET_CODE (op) == ZERO_EXTEND || GET_CODE (op) == SIGN_EXTEND)
    op = XEXP (op, 0);

  if (!COMPARISON_P (op))
    return false;

  if (GET_CODE (XEXP (op, 0)) != REG
      || REGNO (XEXP (op, 0)) != CC_REGNUM
      || (XEXP (op, 1) != const0_rtx
          && !(CONST_INT_P (XEXP (op, 1))
	       && GET_MODE (XEXP (op, 0)) == CCRAWmode
	       && INTVAL (XEXP (op, 1)) >= 0
               && INTVAL (XEXP (op, 1)) <= 15)))
    return false;

  switch (GET_MODE (XEXP (op, 0)))
    {
    case E_CCL1mode:
      return GET_CODE (op) == LTU;

    case E_CCL2mode:
      return GET_CODE (op) == LEU;

    case E_CCL3mode:
      return GET_CODE (op) == GEU;

    case E_CCUmode:
      return GET_CODE (op) == GTU;

    case E_CCURmode:
      return GET_CODE (op) == LTU;

    case E_CCSmode:
      return GET_CODE (op) == UNGT;

    case E_CCSRmode:
      return GET_CODE (op) == UNLT;

    default:
      return false;
    }
})

;; Return nonzero if OP is a valid comparison operator
;; for an SLB condition.

(define_predicate "s390_slb_comparison"
  (match_code "zero_extend, sign_extend, ltu, gtu, leu, geu")
{
  while (GET_CODE (op) == ZERO_EXTEND || GET_CODE (op) == SIGN_EXTEND)
    op = XEXP (op, 0);

  if (!COMPARISON_P (op))
    return false;

  if (GET_CODE (XEXP (op, 0)) != REG
      || REGNO (XEXP (op, 0)) != CC_REGNUM
      || XEXP (op, 1) != const0_rtx)
    return false;

  switch (GET_MODE (XEXP (op, 0)))
    {
    case E_CCL1mode:
      return GET_CODE (op) == GEU;

    case E_CCL2mode:
      return GET_CODE (op) == GTU;

    case E_CCL3mode:
      return GET_CODE (op) == LTU;

    case E_CCUmode:
      return GET_CODE (op) == LEU;

    case E_CCURmode:
      return GET_CODE (op) == GEU;

    case E_CCSmode:
      return GET_CODE (op) == LE;

    case E_CCSRmode:
      return GET_CODE (op) == GE;

    default:
      return false;
    }
})

;; Return true if OP is a load multiple operation.  It is known to be a
;; PARALLEL and the first section will be tested.

(define_special_predicate "load_multiple_operation"
  (match_code "parallel")
{
  machine_mode elt_mode;
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  int i, off;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return false;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);
  elt_mode = GET_MODE (SET_DEST (XVECEXP (op, 0, 0)));

  /* Check, is base, or base + displacement.  */

  if (GET_CODE (src_addr) == REG)
    off = 0;
  else if (GET_CODE (src_addr) == PLUS
	   && GET_CODE (XEXP (src_addr, 0)) == REG
	   && GET_CODE (XEXP (src_addr, 1)) == CONST_INT)
    {
      off = INTVAL (XEXP (src_addr, 1));
      src_addr = XEXP (src_addr, 0);
    }
  else
    return false;

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != elt_mode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != elt_mode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1))
	     != off + i * GET_MODE_SIZE (elt_mode))
	return false;
    }

  return true;
})

;; For an execute pattern the target instruction is embedded into the
;; RTX but will not get checked for validity by recog automatically.
;; The execute_operation predicate extracts the target RTX and invokes
;; recog.
(define_special_predicate "execute_operation"
  (match_code "parallel")
{
  rtx pattern = op;
  rtx_insn *insn;
  int icode;

  /* This is redundant but since this predicate is evaluated
     first when recognizing the insn we can prevent the more
     expensive code below from being executed for many cases.  */
  if (GET_CODE (XVECEXP (pattern, 0, 0)) != UNSPEC
      || XINT (XVECEXP (pattern, 0, 0), 1) != UNSPEC_EXECUTE)
    return false;

  /* Keep in sync with s390_execute_target.  */
  if (XVECLEN (pattern, 0) == 2)
    {
      pattern = copy_rtx (XVECEXP (pattern, 0, 1));
    }
  else
    {
      rtvec vec = rtvec_alloc (XVECLEN (pattern, 0) - 1);
      int i;

      for (i = 0; i < XVECLEN (pattern, 0) - 1; i++)
	RTVEC_ELT (vec, i) = copy_rtx (XVECEXP (pattern, 0, i + 1));

      pattern = gen_rtx_PARALLEL (VOIDmode, vec);
    }

  /* Since we do not have the wrapping insn here we have to build one.  */
  insn = make_insn_raw (pattern);
  icode = recog_memoized (insn);
  if (icode < 0)
    return false;

  extract_insn (insn);

  return constrain_operands (reload_completed, get_enabled_alternatives (insn)) == 1;
})

;; Return true if OP is a store multiple operation.  It is known to be a
;; PARALLEL and the first section will be tested.

(define_special_predicate "store_multiple_operation"
  (match_code "parallel")
{
  machine_mode elt_mode;
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx dest_addr;
  int i, off;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return false;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);
  elt_mode = GET_MODE (SET_SRC (XVECEXP (op, 0, 0)));

  /* Check, is base, or base + displacement.  */

  if (GET_CODE (dest_addr) == REG)
    off = 0;
  else if (GET_CODE (dest_addr) == PLUS
	   && GET_CODE (XEXP (dest_addr, 0)) == REG
	   && GET_CODE (XEXP (dest_addr, 1)) == CONST_INT)
    {
      off = INTVAL (XEXP (dest_addr, 1));
      dest_addr = XEXP (dest_addr, 0);
    }
  else
    return false;

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != elt_mode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != elt_mode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1))
	     != off + i * GET_MODE_SIZE (elt_mode))
	return false;
    }
  return true;
})

(define_predicate "const_shift_by_byte_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT val = INTVAL (op);
  return val <= 128 && val % 8 == 0;
})

;; Certain operations (e.g. CS) cannot access SYMBOL_REF directly, it needs to
;; be loaded into some register first.  In theory, if we put a SYMBOL_REF into
;; a corresponding insn anyway, reload will generate a load for it, but, when
;; coupled with constant propagation, this will lead to an inefficient code
;; (see PR 80080).

(define_predicate "nonsym_memory_operand"
  (match_code "mem")
{
  return memory_operand (op, mode) && !contains_symbol_ref_p (op);
})

;; Check for a valid shift count operand with an implicit
;; shift truncation mask of 63.

(define_predicate "shift_count_operand"
 (and (match_code "reg, subreg, and, plus, const_int")
  (match_test "CONST_INT_P (op) || GET_MODE (op) == E_QImode"))
{
  return s390_valid_shift_count (op, 63);
}
)

;; This is used as operand predicate.  As we do not know
;; the mode of the first operand here and the shift truncation
;; mask depends on the mode, we cannot check the mask.
;; This is supposed to happen in the insn condition which
;; calls s390_valid_shift_count with the proper mode size.
;; We need two separate predicates for non-vector and vector
;; shifts since the (less restrictive) insn condition is checked
;; after the more restrictive operand predicate which will
;; disallow the operand before we can check the condition.

(define_predicate "shift_count_operand_vec"
 (and (match_code "reg, subreg, and, plus, const_int")
  (match_test "CONST_INT_P (op) || GET_MODE (op) == E_QImode"))
{
  return s390_valid_shift_count (op, 0);
}
)

; An integer constant which can be used in a signed add with overflow
; pattern without being reloaded.
(define_predicate "addv_const_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -32768 && INTVAL (op) <= 32767")))

; Match (subreg (reg ...)) operands.
; Used for movstrict destination operands
; When replacing pseudos with hard regs reload strips away the
; subregs. Accept also plain registers then to prevent the insn from
; becoming unrecognizable.
(define_predicate "subreg_register_operand"
  (ior (and (match_code "subreg")
	    (match_test "register_operand (SUBREG_REG (op), GET_MODE (SUBREG_REG (op)))"))
       (and (match_code "reg")
	    (match_test "reload_completed || reload_in_progress")
	    (match_test "register_operand (op, GET_MODE (op))"))))

; Bias value for LEN_LOAD and LEN_STORE.  The bias will be added to the
; length (in bytes for s390) to be loaded.  vll/vstl expect the lowest byte
; to load while LEN_LOAD/LEN_STORE use the actual length in bytes.  This implies
; that we cannot load a length of 0.
(define_predicate "vll_bias_operand"
  (and (match_code "const_int")
       (match_test "op == CONSTM1_RTX (QImode)")))
