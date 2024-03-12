/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(__int128 x, __int128 y)
{
  return x+y;
}

/* { dg-final { scan-assembler-times "movq" 2 } } */
/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
