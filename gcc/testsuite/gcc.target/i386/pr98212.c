/* PR rtl-optimization/98212 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -mno-avx" } */
/* { dg-final { scan-assembler-times "\tucomiss\t" 2 } } */
/* { dg-final { scan-assembler-not "\tcomiss\t" } } */

void foo (void);

void
bar (float a, float b)
{
  if (a != b)
    foo ();
}

void
baz (float a, float b)
{
  if (a == b)
    foo ();
}
