/* { dg-do compile } */
/* { dg-options "-O2" } */

short exttrunc_hi2_qi(short x)
{
  return (char)x;
}

/* Match:
   mov.u32 %r24, %ar0;
   cvt.u32.u32 %r26, %r24;
   cvt.s32.s8 %value, %r26;
   Todo: Remove cvt.u32.u32.  */

/* { dg-final { scan-assembler-times "mov\.u32\t%r\[0-9\]*, %ar0" 1 } } */
/* { dg-final { scan-assembler-times "mov\." 1 } } */

/* { dg-final { scan-assembler-times "cvt\.u32\.u32" 1 } } */
/* { dg-final { scan-assembler-times "cvt\.s32\.s8" 1 } } */
/* { dg-final { scan-assembler-times "cvt\." 2 } } */
