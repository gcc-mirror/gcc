/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

__int128 a[16];
__int128 b[16];
__int128 c[16];

void foo()
{
  for (unsigned int i=0; i<16; i++)
    a[i] = b[i] ^ c[i];
}

/* { dg-final { scan-assembler "pxor" } } */
