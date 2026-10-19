/* PR target/127720 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -mf16c -mfpmath=sse" } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t" } } */
/* { dg-final { scan-assembler "\tvcvtss2sd\t" } } */

double
foo (_Float16 x)
{
  return x;
}
