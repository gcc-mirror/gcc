/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */

long long p[128];

bool __attribute__((noipa))
fand (int n)
{
  bool r = true;
  for (int i = 0; i < n; ++i)
    r &= (p[i] != 0);
  return r;
}

bool __attribute__((noipa))
fior (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r |= (p[i] != 0);
  return r;
}

int main()
{
  __builtin_memset (p, 1, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (!fand (n))
      __builtin_abort ();

  p[0] = 0;
  for (int n = 1; n < 77; ++n)
    if (fand (n))
      __builtin_abort ();

  __builtin_memset (p, 0, sizeof(p));

  for (int n = 0; n < 77; ++n)
    if (fior (n))
      __builtin_abort ();

  p[0] = 1;
  for (int n = 1; n < 77; ++n)
    if (!fior (n))
      __builtin_abort ();

  return 0;
}
