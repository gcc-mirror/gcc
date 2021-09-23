#define N 128

typedef signed int si;
typedef unsigned int ui;

si si_a[N], si_b[N], si_c[N];
ui ui_a[N], ui_b[N], ui_c[N];

__attribute__ ((noipa)) void
test_divwe ()
{
  for (int i = 0; i < N; i++)
    si_c[i] = __builtin_divwe (si_a[i], si_b[i]);
}

__attribute__ ((noipa)) void
test_divweu ()
{
  for (int i = 0; i < N; i++)
    ui_c[i] = __builtin_divweu (ui_a[i], ui_b[i]);
}

