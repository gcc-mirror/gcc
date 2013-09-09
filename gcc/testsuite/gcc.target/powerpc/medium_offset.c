/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "\\+4611686018427387904" } } */

static int x;

unsigned long
foo (void)
{
  return ((unsigned long) &x) - 0xc000000000000000;
}
