/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -ffloat-store -ftree-coalesce-vars -fdump-rtl-expand" } */

/* The partition holding v_2 has an anonymous representative, _1, so the
   variable that expansion attaches to its stack slot comes from a name other
   than the representative.  -ffloat-store puts both partitions of v in memory,
   and they must not end up sharing a MEM_EXPR.  */

double in1, in2, out1, out2, out3, out4;

void __GIMPLE (ssa, startwith ("expand"))
f (void)
{
  double v;
  double _1;

__BB(2):
  _1 = in1;
  v_2 = _1;
  v_3 = in2;
  out1 = v_2;
  out2 = v_3;
  out3 = v_2;
  out4 = v_3;
  return;
}

/* { dg-final { scan-rtl-dump {\[[0-9]+ v\+0} "expand" } } */
/* { dg-final { scan-rtl-dump {\[[0-9]+ D\.[0-9]+\+0} "expand" } } */
