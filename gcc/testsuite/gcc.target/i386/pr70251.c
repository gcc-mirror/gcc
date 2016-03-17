/* { dg-do run } */
/* { dg-options "-O3 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

unsigned long long int
hash(unsigned long long int seed, unsigned long long int v)
{
  return seed ^ (v + 0x9e3779b9 + (seed<<6) + (seed>>2));
}

unsigned int a [100];
signed char b [100];
signed char c [100];

void
init ()
{
  for (int i = 0; i < 100; ++i)
    {
      a [i] = 1000L;
      b [i] = 10;
      c [i] = 5;
    }
}

void
foo ()
{
  for (int i = 0; i < 100; ++i)
    b [i] = (!b [i] ^ (a [i] >= b [i])) + c [i];
}

unsigned long long int
checksum ()
{
  unsigned long long int seed = 0ULL;
  for (int i = 0; i < 100; ++i)
    seed = hash (seed, b[i]);
  return seed;
}

void
TEST ()
{
  init ();
  foo ();
  if (checksum () != 5785906989299578598ULL)
    __builtin_abort ();
}
