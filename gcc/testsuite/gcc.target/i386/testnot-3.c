/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

int foo(__int128 x, __int128 y)
{
  return (x & y) == y;
}

/* { dg-final { scan-assembler-not "xorq" } } */
