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

/* We accept neon instructions vldr.64 and vstr.64 as well.  */
/* { dg-final { scan-assembler-times "(?:ldrd|vldr\\.64)" 10 } } */
/* { dg-final { scan-assembler-times "(?:strd|vstr\\.64)" 9 } } */
