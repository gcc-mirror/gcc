/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O -fzero-call-used-regs=used -ffinite-math-only" } */

float
foo (void)
{
  return __builtin_fmod (0, 0);
}

