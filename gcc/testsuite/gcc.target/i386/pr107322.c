/* PR target/107322 */
/* { dg-do compile } */
/* { dg-options "-fexcess-precision=16 -O -msse2 -mfpmath=sse" } */

int i, j;
float k, l;
__bf16 f;

void
foo (void)
{
  i *= 0 >= f;
}

void
bar (void)
{
  i *= 0 <= f;
}

void
baz (int x, int y)
{
  i = 0 >= f ? x : y;
  j = 0 <= f ? x + 2 : y + 3;
}

void
qux (float x, float y)
{
  k = 0 >= f ? x : y;
  l = 0 <= f ? x + 2 : y + 3;
}
