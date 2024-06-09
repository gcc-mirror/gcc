/* PR tree-optimization/113102 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

unsigned x;

#if __BITINT_MAXWIDTH__ >= 191
void
foo (void)
{
  unsigned _BitInt(191) b = x;
  ~(b >> x) % 3;
}
#endif
