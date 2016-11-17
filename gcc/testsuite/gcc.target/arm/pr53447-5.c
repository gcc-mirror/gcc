/* { dg-do compile } */
/* { dg-options "-Os -mthumb" } */
/* { dg-require-effective-target arm_thumb2_ok } */

void foo(long long* p)
{
  p[1] |= 0x100000001;
  p[2] &= 0x100000001;
  p[3] ^= 0x100000001;
  p[4] += 0x100000001;
  p[5] -= 0x100000001;
  p[6] = ~p[6];
  p[7] <<= 5;
  p[8] >>= 5;
  p[9] -= p[10];
}

/* { dg-final { scan-assembler-times "ldrd" 10 } } */
/* { dg-final { scan-assembler-times "strd" 9 } } */
