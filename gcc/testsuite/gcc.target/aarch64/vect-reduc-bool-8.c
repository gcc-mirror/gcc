/* { dg-do run } */
/* { dg-options "-O3 -march=armv8-a -mautovec-preference=asimd-only -fdump-tree-vect-details" }*/

long long p[128];

bool __attribute__((noipa))
fxort (int n)
{
  bool r = true;
  for (int i = 0; i < n; ++i)
    r ^= (p[i] != 0);
  return r;
}

bool __attribute__((noipa))
fxorf (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r ^= (p[i] != 0);
  return r;
}

int main()
{
  __builtin_memset (p, 1, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (fxort (n) != !(n & 1))
      __builtin_abort ();

  for (int n = 0; n < 77; ++n)
    if (fxorf (n) != (n & 1))
      __builtin_abort ();

  __builtin_memset (p, 0, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (!fxort (n))
      __builtin_abort ();

  for (int n = 0; n < 77; ++n)
    if (fxorf (n))
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" { target { vect_int && vect_condition } } } } */
