/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -ffloat-store -ftree-coalesce-vars -fdump-rtl-expand" } */

/* Three partitions of v, each with an anonymous representative.  One keeps v
   and the other two need their own decls, so that no two of the three stack
   slots share a MEM_EXPR.  */

double in1, in2, in3, out1, out2, out3, out4, out5, out6;

void __GIMPLE (ssa, startwith ("expand"))
f (void)
{
  double v;
  double _1;
  double _3;
  double _5;

__BB(2):
  _1 = in1;
  v_2 = _1;
  _3 = in2;
  v_4 = _3;
  _5 = in3;
  v_6 = _5;
  out1 = v_2;
  out2 = v_4;
  out3 = v_6;
  out4 = v_2;
  out5 = v_4;
  out6 = v_6;
  return;
}

/* { dg-final { scan-rtl-dump {\[[0-9]+ v\+0} "expand" } } */
/* { dg-final { scan-rtl-dump {\[[0-9]+ D\.[0-9]+\+0} "expand" } } */
