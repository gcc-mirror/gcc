/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "imull\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-times "subl\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-times "add(?:l|q)\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-not "leal" } } */

void
__attribute__((noipa))
foo (unsigned a[256], unsigned b[256])
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      unsigned tmp = a[i] + 12345U;
      tmp *= 914237U;
      tmp += 12332U;
      tmp *= 914237U;
      tmp += 12332U;
      tmp *= 914237U;
      tmp -= 13U;
      tmp *= 8000U;
      b[i] = tmp;
    }
}
