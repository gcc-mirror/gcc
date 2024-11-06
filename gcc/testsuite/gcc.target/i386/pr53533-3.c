/* { dg-do compile } */
/* { dg-options "-O1 -fwrapv" } */
/* { dg-final { scan-assembler-times "imull\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-times "subl\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-times "add(?:l|q)\[ \t\]" "1" } } */
/* { dg-final { scan-assembler-not "leal" } } */

void
__attribute__((noipa))
foo (int a[256], int b[256])
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      int tmp = a[i] + 12345;
      tmp *= 914237;
      tmp += 12332;
      tmp *= 914237;
      tmp += 12332;
      tmp *= 914237;
      tmp -= 13;
      tmp *= 8000;
      b[i] = tmp;
    }
}

