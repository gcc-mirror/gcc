/* { dg-do compile } */
/* { dg-options "-O2" } */

int exttrunc_si2_hi(int x)
{
  return (short)x;
}

/* Match:
   mov.u32 %r24, %ar0;
   cvt.s32.s16     %value, %r24;

/* { dg-final { scan-assembler-times "mov\.u32\t%r\[0-9\]*, %ar0" 1 } } */
/* { dg-final { scan-assembler-times "mov\." 1 } } */

/* { dg-final { scan-assembler-times "cvt\.s32\.s16" 1 } } */
/* { dg-final { scan-assembler-times "cvt\." 1 } } */
