/* Predicates for TI C6X
   Copyright (C) 2010-2024 Free Software Foundation, Inc.
   Contributed by Andrew Jenner <andrew@codesourcery.com>
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

(define_predicate "reg_or_const_int_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_int_operand")))

(define_predicate "const_vector_operand"
  (match_code "const_vector"))

(define_predicate "scst5_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_Is5 (op)")))

(define_predicate "reg_or_ucst4_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Iu4 (op)"))))

(define_predicate "reg_or_scst5_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "scst5_operand")))

(define_predicate "reg_or_ucst5_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Iu5 (op)"))))

(define_predicate "addsi_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_IsB (op)"))))

(define_predicate "andsi_operand"
  (ior (match_operand 0 "reg_or_scst5_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Jc (op)"))))

(define_predicate "iorsi_operand"
  (ior (match_operand 0 "reg_or_scst5_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Js (op)"))))

(define_predicate "insv_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "INTVAL (op) == 0 || INTVAL (op) == -1")))

(define_predicate "c6x_jump_operand"
  (match_code "label_ref,symbol_ref,reg"))

(define_predicate "c6x_call_operand"
  (ior (match_code "symbol_ref,reg")
       (and (match_code "subreg")
            (match_test "GET_CODE (XEXP (op, 0)) == REG")))
{
  /* The linker transforms jumps to undefined weak symbols in a way that
     is incompatible with our code generation.  */
  return (GET_CODE (op) != SYMBOL_REF
	  || (!SYMBOL_REF_WEAK (op)
	      && !c6x_long_call_p (op)));
})

;; Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
;; possibly with an offset.
(define_predicate "symbolic_operand"
  (ior (match_code "symbol_ref,label_ref")
       (and (match_code "const")
	    (match_test "GET_CODE (XEXP (op,0)) == PLUS
			 && (GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
			     || GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF)
			 && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT"))))

(define_predicate "const_int_or_symbolic_operand"
  (ior (match_operand 0 "symbolic_operand")
       (match_operand 0 "const_int_operand")))

;; Return nonzero iff OP is one of the integer constants 2, 4 or 8.
(define_predicate "adda_scale_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4
 		    || ((TARGET_INSNS_64 || TARGET_INSNS_67)
 			&& INTVAL (op) == 8)")))

;; Return nonzero iff OP is one of the integer constants 2 or 4.
(define_predicate "suba_scale_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4")))

;; True if this operator is valid for predication.
(define_predicate "predicate_operator"
  (match_code "eq,ne"))

(define_predicate "c6x_comparison_operator"
  (match_code "eq,ltu,gtu,lt,gt"))

(define_predicate "non_c6x_comparison_operator"
  (match_code "ne,leu,geu,le,ge"))

;; FP Comparisons handled by c6x_expand_compare.
(define_predicate "c6x_fp_comparison_operator"
  (ior (match_code "eq,lt,gt,le,ge")
       (and (match_test "TARGET_FP")
	    (match_code "ltgt,uneq,unlt,ungt,unle,unge,ordered,unordered"))))

(define_predicate "c6x_any_comparison_operand"
  (match_code "eq,lt,gt,le,ge,ltu,gtu")
{
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);
  if (ltugtu_operator (op, SImode)
      && register_operand (op0, SImode)
      && ((TARGET_INSNS_64 && reg_or_ucst5_operand (op1, SImode))
	  || (!TARGET_INSNS_64 && reg_or_ucst4_operand (op1, SImode))))
    return true;
  if (eqltgt_operator (op, SImode)
      && register_operand (op0, SImode)
      && reg_or_scst5_operand (op1, SImode))
    return true;
  if (!TARGET_FP)
    return false;
  if (!eqltgt_operator (op, SFmode) && !eqltgt_operator (op, DFmode))
    return false;
  if (register_operand (op0, GET_MODE (op))
      && register_operand (op1, GET_MODE (op)))
    return true;
  return false;
})

(define_predicate "ltugtu_operator"
  (match_code "ltu,gtu"))

(define_predicate "eqltgt_operator"
  (match_code "eq,lt,gt"))

(define_predicate "eqne_operator"
  (match_code "eq,ne"))

(define_predicate "predicate_register"
  (and (match_code "reg")
       (ior (match_test "REGNO_REG_CLASS (REGNO (op)) == PREDICATE_A_REGS")
	    (match_test "REGNO_REG_CLASS (REGNO (op)) == PREDICATE_B_REGS"))))

;; Allow const_ints for things like the real_mult patterns.
(define_predicate "a_register"
  (ior (and (match_code "reg")
	    (match_test "A_REGNO_P (REGNO (op))"))
       (and (match_code "const_int")
	    (match_test "A_REGNO_P (INTVAL (op))"))))

(define_predicate "b_register"
  (ior (and (match_code "reg")
	    (match_test "B_REGNO_P (REGNO (op))"))
       (and (match_code "const_int")
	    (match_test "B_REGNO_P (INTVAL (op))"))))

(define_predicate "pic_register_operand"
  (and (match_code "reg")
       (match_test "op == pic_offset_table_rtx")))

;; True if OP refers to a symbol in the sdata section.
(define_predicate "sdata_symbolic_operand"
  (match_code "symbol_ref,const")
{
  HOST_WIDE_INT offset = 0, size = 0;
  tree t;

  switch (GET_CODE (op))
    {
    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 0)) != SYMBOL_REF
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return false;
      offset = INTVAL (XEXP (op, 1));
      op = XEXP (op, 0);
      /* FALLTHRU */

    case SYMBOL_REF:
      /* For shared libraries, only allow symbols we know are local.
         For executables, the linker knows to create copy relocs if
	 necessary so we can use DP-relative addressing for all small
	 objects.  */
      if ((c6x_initial_flag_pic && !SYMBOL_REF_LOCAL_P (op))
	  || !SYMBOL_REF_SMALL_P (op))
	return false;

     /* Note that in addition to DECLs, we can get various forms
	of constants here.  */
      t = SYMBOL_REF_DECL (op);
      if (DECL_P (t))
        t = DECL_SIZE_UNIT (t);
      else
	t = TYPE_SIZE_UNIT (TREE_TYPE (t));
      if (t && tree_fits_shwi_p (t))
	{
	  size = tree_to_shwi (t);
	  if (size < 0)
	    size = 0;
	}

      /* Don't allow addressing outside the object.  */
      return (offset >= 0 && offset <= size);

    default:
      gcc_unreachable ();
    }
})
