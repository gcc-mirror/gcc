/* { dg-do compile { target { arm_thumb2_ok || arm_thumb1_movt_ok } } } */
/* { dg-options "-O2" } */

long long
movdi (int a)
{
  return 0xF0F0;
}

/* Accept r1 because big endian targets put the low bits in the highest
   numbered register of a pair.  */
/* { dg-final { scan-assembler-times "movw\tr\[01\], #61680" 1 } } */
