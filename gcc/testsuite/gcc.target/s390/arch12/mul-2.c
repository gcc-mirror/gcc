/* { dg-do compile { target int128 } } */

__int128
mgrk (long long a, long long b)
{
  return (__int128)a * (__int128)b;
}

__int128
mg (long long a, long long *b)
{
  return (__int128)a * (__int128)*b;
}

/* { dg-final { scan-assembler-times "\tmgrk\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmg\t" 1 } } */
