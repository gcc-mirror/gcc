/* { dg-require-effective-target arm_arch_v7a_thumb_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_arch_v7a_thumb } */

int t (int a, int b, int c, int d)
{
  int r;
  if (a > 0) {
    r = a + b;
    r += 0x456;
    r *= 0x1234567;
    }
  else {
    r = b - a;
    r -= 0x123;
    r *= 0x12387;
    r += d;
   }
  return r;
}

/* { dg-final { scan-assembler-not {\tit[tf]*\t} } } */
