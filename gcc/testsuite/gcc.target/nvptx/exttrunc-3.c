/* { dg-do compile } */
/* { dg-options "-O2" } */

long exttrunc_di2_qi(long x)
{
  return (char)x;
}

/* Match:
   mov.u64 %r24, %ar0;
   cvt.s64.s8      %value, %r24;  */

/* { dg-final { scan-assembler-times "mov\.u64\t%r\[0-9\]*, %ar0" 1 } } */
/* { dg-final { scan-assembler-times "mov\." 1 } } */

/* { dg-final { scan-assembler-times "cvt\.s64\.s8" 1 } } */
/* { dg-final { scan-assembler-times "cvt\." 1 } } */
