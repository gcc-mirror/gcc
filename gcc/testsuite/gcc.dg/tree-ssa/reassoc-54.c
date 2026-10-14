/* !(a unord b) | a < b is a ord b, not true.  */
/* { dg-do run } */
/* { dg-options "-O2 -fgimple -fno-trapping-math" } */
/* { dg-add-options ieee } */

int __GIMPLE (ssa,startwith("reassoc1")) __attribute__((noipa))
f (double a, double b)
{
  _Bool M;
  _Bool N;
  _Bool L;
  _Bool R;
  int r;

  __BB(2):
  M_1 = a_2(D) __UNORDERED b_3(D);
  N_4 = M_1 == _Literal (_Bool) 0;
  L_5 = a_2(D) < b_3(D);
  R_6 = N_4 | L_5;
  r_7 = (int) R_6;
  return r_7;
}

int
main (void)
{
  if (f (__builtin_nan (""), 0.0) != 0 || f (0.0, 1.0) != 1
      || f (1.0, 0.0) != 1)
    __builtin_abort ();
  return 0;
}
