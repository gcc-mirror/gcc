/* { dg-do run { target { ! "m68k*-*-* mmix*-*-* mep*-*-* bfin*-*-* v850*-*-* picochip*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-* hppa*-*-*"} } } */

/* { dg-options "-O2 -fno-inline -fdump-tree-reassoc1-details" } */
/* { dg-additional-options "-mbranch-cost=2" { target mips*-*-* avr-*-* } } */

int test (int a, int b, int c)
{
  if (a != 10 && a != 12)
    return b;
  else
    return c;
}
int main ()
{
  if (test (10, 20, 30) != 30)
    __builtin_abort ();
  if (test (12, 20, 30) != 30)
    __builtin_abort ();
  if (test (26, 20, 30) != 20)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests" 1 "reassoc1"} }*/
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
