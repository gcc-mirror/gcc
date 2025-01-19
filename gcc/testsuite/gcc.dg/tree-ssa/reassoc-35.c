/* { dg-do run { target { ! "m68k*-*-* mmix*-*-* bfin*-*-* v850*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-* hppa*-*-* or1k*-*-* pru*-*-*"} } } */

/* { dg-options "-O2 -fno-inline -fdump-tree-reassoc1-details --param logical-op-non-short-circuit=1" } */
/* { dg-additional-options "-mbranch-cost=2" { target branch_cost } } */

int test (unsigned int a, int b, int c)
{
  if ((a - 43) <= 3 || (a - 75) <= 3)
    return b;
  else
    return c;
}
int
main ()
{
  volatile int n43, n47, n75, n79;
  n43 = 43; n47 = n43 + 4; n75 = 75; n79 = n75 + 4;
  int i;
  for (i = -10; i <= 100; i++)
    if (test (i, 2, 3) != 3 - ((i >= n43 && i < n47) || (i >= n75 && i < n79)))
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests" 1 "reassoc1"} }*/
