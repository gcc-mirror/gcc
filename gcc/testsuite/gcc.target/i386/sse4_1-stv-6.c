/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

__int128 a[16];
__int128 b[16];

int foo()
{
  for (unsigned int i=0; i<16; i++)
    if (a[i] == b[i])
      return i;
  return -1;
}

/* { dg-final { scan-assembler "ptest" } } */
