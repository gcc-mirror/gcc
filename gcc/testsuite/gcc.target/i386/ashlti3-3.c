/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(__int128 x)
{
  return x << 1;
}

/* { dg-final { scan-assembler "adcq" } } */
/* { dg-final { scan-assembler-not "shldq" } } */
