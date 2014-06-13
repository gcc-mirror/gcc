/* PR rtl-optimization/61446 */

/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=corei7 -mfpmath=387" } */

unsigned long long
foo (float a)
{
  const double dfa = a;
  const unsigned int hi = dfa / 0x1p32f;
  const unsigned int lo = dfa - (double) hi * 0x1p32f;

  return ((unsigned long long) hi << (4 * (8))) | lo;
}
