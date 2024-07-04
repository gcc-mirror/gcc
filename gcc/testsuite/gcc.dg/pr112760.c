/* PR rtl-optimization/112760 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-dce -fno-guess-branch-probability --param=max-cse-insns=0" } */
/* { dg-additional-options "-m8bit-idiv -mavx" { target i?86-*-* x86_64-*-* } } */

unsigned g;

__attribute__((__noipa__)) unsigned short
foo (unsigned short a, unsigned short b)
{
  unsigned short x = __builtin_add_overflow_p (a, g, (unsigned short) 0);
  g -= g / b;
  return x;
}

int
main ()
{
  unsigned short x = foo (40, 6);
  if (x != 0)
    __builtin_abort ();
}
