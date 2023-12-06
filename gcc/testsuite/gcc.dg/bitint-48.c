/* PR tree-optimization/112809 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 512
_BitInt (512) a;
_BitInt (256) b;
_BitInt (256) c;

int
foo (void)
{
  return a == (b | c);
}

void
bar (void)
{
  a /= b - 2;
}
#else
int i;
#endif
