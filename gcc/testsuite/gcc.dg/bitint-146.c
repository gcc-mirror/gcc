/* PR tree-optimization/127382 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 65
_BitInt(7) m0[8];
unsigned _BitInt(17) g1;
_BitInt(7) p0;

void
foo (int s0)
{
  p0 = g1;
  for (unsigned i = 0; i < 7; i++)
    {
      _BitInt(65) tt = -(_BitInt(65)) (s0 ? i : g1);
      m0[i & 7] = tt / 10;
    }
}
#endif
