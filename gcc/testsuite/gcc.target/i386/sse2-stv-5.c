/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mno-sse4 -mno-stackrealign -mtune=generic" } */

__int128 m;

void foo()
{
  register __int128 x __asm("xmm0");
  register __int128 y __asm("xmm1");
  m = x ^ y;
}

/* { dg-final { scan-assembler-times "pxor" 1 } } */
