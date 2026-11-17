/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zba_zbb -mabi=lp64" } */

unsigned int
rol1 (unsigned long x, unsigned long y, unsigned int n)
{
  return __builtin_stdc_rotate_left (((unsigned int) x << 1)
				     + (unsigned int) y, n);
}

unsigned int
ror2 (unsigned long x, unsigned long y, unsigned int n)
{
  /* This mask changes the rotation count and must be retained.  */
  return __builtin_stdc_rotate_right (((unsigned int) x << 2)
				      + (unsigned int) y, n & 7);
}

unsigned int
ror3 (unsigned int x, unsigned int y)
{
  return __builtin_stdc_rotate_right ((x << 3) + y, 7);
}

/* { dg-final { scan-assembler-times "sh1add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh2add\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add\t" 1 } } */
/* { dg-final { scan-assembler-times "rolw\t" 1 } } */
/* { dg-final { scan-assembler-times "rorw\t" 1 } } */
/* { dg-final { scan-assembler-times "roriw\t" 1 } } */
/* { dg-final { scan-assembler-times {andi\t[^\n]*,7\n} 1 } } */
/* { dg-final { scan-assembler-not "slliw\t" } } */
/* { dg-final { scan-assembler-not "addw\t" } } */
