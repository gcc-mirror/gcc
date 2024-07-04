/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

unsigned __int128 foo(unsigned __int128 x, unsigned __int128 y)
{
  return (x & 1000) * (y & 1000);
}

__int128 bar(__int128 x, __int128 y)
{
  return (x & 1000) * (y & 1000);
}

/* { dg-final { scan-assembler-times "\tmulq" 1 } } */
/* { dg-final { scan-assembler-times "\timulq" 1 } } */
/* { dg-final { scan-assembler-not "addq" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
