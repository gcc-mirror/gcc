(define_predicate "trapping_target_operand"
  (match_code "if_then_else")
{
  rtx cond, mem, res, tar, and;

  if (GET_MODE (op) != PDImode)
    return 0;
  cond = XEXP (op, 0);
  mem = XEXP (op, 1);
  res = XEXP (op, 2);
  if (GET_CODE (mem) != MEM
      || (GET_CODE (res) != SIGN_EXTEND && GET_CODE (res) != TRUNCATE))
    return 0;
  tar = XEXP (res, 0);
  if (!rtx_equal_p (XEXP (mem, 0), tar)
      || GET_MODE (tar) != Pmode)
    return 0;
  if (GET_CODE (cond) == CONST)
    {
      cond = XEXP (cond, 0);
      if (!EXTRA_CONSTRAINT_Csy (tar))
	return 0;
      if (GET_CODE (tar) == CONST)
	tar = XEXP (tar, 0);
    }
  else if (!arith_reg_operand (tar, VOIDmode)
	   && ! EXTRA_CONSTRAINT_Csy (tar))
    return 0;
  if (GET_CODE (cond) != EQ)
    return 0;
  and = XEXP (cond, 0);
  return (GET_CODE (and) == AND
	  && rtx_equal_p (XEXP (and, 0), tar)
	  && GET_CODE (XEXP (and, 1)) == CONST_INT
	  && GET_CODE (XEXP (cond, 1)) == CONST_INT
	  && INTVAL (XEXP (and, 1)) == 3
	  && INTVAL (XEXP (cond, 1)) == 3);
})
