;; Predicate definitions for Sunplus S+CORE.
;; Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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

(define_predicate "const_uimm5"
  (match_code "const_int")
{
  return IMM_IN_RANGE (INTVAL (op), 5, 0);
})

(define_predicate "const_simm12"
  (match_code "const_int")
{
  return IMM_IN_RANGE (INTVAL (op), 12, 1);
})

(define_predicate "const_simm15"
  (match_code "const_int")
{
  return IMM_IN_RANGE (INTVAL (op), 15, 1);
})

(define_predicate "arith_operand"
  (ior (match_code "const_int")
       (match_operand 0 "register_operand")))

(define_predicate "score_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (GET_CODE (op) == REG)
          && (REGNO (op) != CC_REGNUM);
})

(define_predicate "const_call_insn_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum score_symbol_type symbol_type;

  return (score_symbolic_constant_p (op, &symbol_type)
          && (symbol_type == SYMBOL_GENERAL));
})

(define_predicate "call_insn_operand"
  (ior (match_operand 0 "const_call_insn_operand")
       (match_operand 0 "register_operand")))

(define_predicate "hireg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == HI_REGNUM")))

(define_predicate "loreg_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == LO_REGNUM")))

(define_predicate "sr0_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == CN_REGNUM")))

(define_predicate "g32reg_operand"
  (and (match_code "reg")
       (match_test "GP_REG_P (REGNO (op))")))

(define_predicate "branch_n_operator"
  (match_code "lt,ge"))

(define_predicate "branch_nz_operator"
  (match_code "eq,ne,lt,ge"))

(define_predicate "score_load_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_DEST (elt)) != REG
          || GET_MODE (SET_DEST (elt)) != SImode
          || REGNO (SET_DEST (elt)) != (unsigned) (dest_regno + i)
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != POST_INC)
        return 0;
    }

  return 1;
})

(define_predicate "score_store_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != SImode
          || REGNO (SET_SRC (elt)) != (unsigned) (src_regno + i)
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PRE_DEC)
        return 0;
    }

  return 1;
})

