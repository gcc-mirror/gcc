/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */

typedef struct rtx_def *rtx;
enum rtx_code { MINUS };
union rtunion_def {
  rtx rt_rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def {
  enum rtx_code code: 16;
  union u {
    rtunion fld[1];
  }
  u;
};
rtx simplify_binary_operation (enum rtx_code code, int mode,
			       rtx op0, rtx op1);
struct simplify_plus_minus_op_data {
  rtx op;
  short neg;
};
void simplify_plus_minus (enum rtx_code code, int mode, rtx op0, rtx op1)
{
  struct simplify_plus_minus_op_data ops[8];
  rtx tem = (rtx) 0;
  int n_ops = 2, input_ops = 2;
  int changed, canonicalized = 0;
  int i, j;
  __builtin_memset (ops, 0, sizeof (ops));
  do
    {
      changed = 0;
      for (i = 0; i < n_ops; i++)
	{
	  rtx this_op = ops[i].op;
	  int this_neg = ops[i].neg;
	  enum rtx_code this_code = ((enum rtx_code) (this_op)->code);
	  switch (this_code)
	    {
	    case MINUS:
	      if (n_ops == 7)
		return;
	      n_ops++;
	      input_ops++;
	      changed = 1;
	      canonicalized |= this_neg; 
	      break;
	    }
	}
    }
  while (changed);
  do 
    {
      j =  n_ops - 1;
      for (i = n_ops - 1; j >= 0; j--)
	{
	  rtx lhs = ops[j].op, rhs = ops[i].op;
	  int lneg = ops[j].neg, rneg = ops[i].neg;
	  if (lhs != 0 && rhs != 0)
	    {
	      enum rtx_code ncode = MINUS;
	      if (((enum rtx_code) (lhs)->code) == MINUS)
		tem = simplify_binary_operation (ncode, mode, lhs, rhs);
	      if (tem && ! (((enum rtx_code) (tem)->code) == MINUS 
			    && ((((((tem)->u.fld[0]).rt_rtx))->u.fld[0]).rt_rtx) == lhs
			    && ((((((tem)->u.fld[0]).rt_rtx))->u.fld[1]).rt_rtx) == rhs))
		{
		  lneg &= rneg;
		  ops[i].op = tem;
		  ops[i].neg = lneg;
		  ops[j].op = (rtx) 0;
		  changed = 1;
		  canonicalized = 1;
		}
	    }
	}
      for (i = 0, j = 0; j < n_ops; j++)
	if (ops[j].op)
	  {
	    ops[i] = ops[j];
	    i++;
	  }
    }
  while (changed);
}
