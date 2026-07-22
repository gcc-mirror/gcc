/* { dg-do run } */
/* { dg-options "-O2 -fgimple" } */

/* Do not use an arbitrary negated operand as a numeric zero-one value in the
   mask identities.  */

__attribute__ ((noipa)) int __GIMPLE ()
zero_else_non_boolean_value (_Bool c, int b)
{
  int nb;
  int r;

  nb = -b_2(D);
  r = c_1(D) ? nb : 0;
  return r;
}

__attribute__ ((noipa)) int __GIMPLE ()
minus_one_else_non_boolean_value (_Bool c, int b)
{
  int nb;
  int r;

  nb = -b_2(D);
  r = c_1(D) ? nb : _Literal (int) -1;
  return r;
}

int
main (void)
{
  if (zero_else_non_boolean_value (1, 2) != -2
      || minus_one_else_non_boolean_value (0, 2) != -1)
    __builtin_abort ();
  return 0;
}
