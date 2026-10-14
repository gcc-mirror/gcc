/* !(a < b) && a <= b is a == b, not a u== b.  */
/* { dg-do run } */
/* { dg-options "-O1 -fgimple -fno-trapping-math" } */
/* { dg-add-options ieee } */

int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f (double a, double b)
{
  _Bool M;
  int r;

  __BB(2):
  M_1 = a_2(D) < b_3(D);
  if (M_1 != _Literal (_Bool) 0)
    goto __BB5;
  else
    goto __BB3;

  __BB(3):
  if (a_2(D) <= b_3(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  goto __BB6;

  __BB(5):
  goto __BB6;

  __BB(6):
  r_4 = __PHI (__BB4: 1, __BB5: 0);
  return r_4;
}

int
main (void)
{
  if (f (__builtin_nan (""), 0.0) != 0 || f (1.0, 1.0) != 1
      || f (0.0, 1.0) != 0)
    __builtin_abort ();
  return 0;
}
