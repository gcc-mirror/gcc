/* PR target/107969 */
/* { dg-do compile } */
/* { dg-options "-O2 -fexcess-precision=16 -msoft-float -msse2" } */

int i;
__bf16 f;

void
bar (void)
{
  i *= 0 <= f;
}
