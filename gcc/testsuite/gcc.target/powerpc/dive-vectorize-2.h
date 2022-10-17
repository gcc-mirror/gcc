#define N 128

typedef signed long long sLL;
typedef unsigned long long uLL;

sLL sll_a[N], sll_b[N], sll_c[N];
uLL ull_a[N], ull_b[N], ull_c[N];

__attribute__ ((noipa)) void
test_divde ()
{
  for (int i = 0; i < N; i++)
    sll_c[i] = __builtin_divde (sll_a[i], sll_b[i]);
}

__attribute__ ((noipa)) void
test_divdeu ()
{
  for (int i = 0; i < N; i++)
    ull_c[i] = __builtin_divdeu (ull_a[i], ull_b[i]);
}

