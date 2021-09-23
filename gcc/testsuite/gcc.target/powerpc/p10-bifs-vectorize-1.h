#define N 32

typedef unsigned long long uLL;
uLL ull_a[N], ull_b[N], ull_c[N];

__attribute__ ((noipa)) void
test_cfuged ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_cfuged (ull_a[i], ull_b[i]);
}

__attribute__ ((noipa)) void
test_cntlzdm ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_cntlzdm (ull_a[i], ull_b[i]);
}

__attribute__ ((noipa)) void
test_cnttzdm ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_cnttzdm (ull_a[i], ull_b[i]);
}

__attribute__ ((noipa)) void
test_pdepd ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_pdepd (ull_a[i], ull_b[i]);
}

__attribute__ ((noipa)) void
test_pextd ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_pextd (ull_a[i], ull_b[i]);
}

