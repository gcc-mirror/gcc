/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

__int128 a,b;
int foo()
{
  return (a & b) != 0;
}

/* { dg-final { scan-assembler-not "pand" } } */
/* { dg-final { scan-assembler "ptest" } } */
