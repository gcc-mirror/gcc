/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mno-sse4 -mno-stackrealign -mtune=generic" } */

__int128 a, b, c, z;

void foo()
{
  register __int128 x __asm("xmm0");
  z = (x ^ a ^ b ^ c);
}

/* { dg-final { scan-assembler-times "pxor" 3 } } */
