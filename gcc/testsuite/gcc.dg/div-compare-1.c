/* { dg-do run } */
/* { dg-options "-std=c99" } */

extern void abort(void);

typedef unsigned long long uint64;

int very_large_value (uint64 t)
{
  return (t / 1000000000ULL) > 9223372037ULL;
}

int main(void)
{
  uint64 t = 0xC000000000000000ULL;

  if (!very_large_value (t))
    abort ();

  return 0;
}
