/* PR rtl-optimization/46235 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=core2" } */

int foo (int a, int x, int y)
{
  if (a & (1<<x))
    return a;
  return 1;
}

int bar_ww (int a, int x, int y, int z)
{
  return (a & (1<<x)) ? y : z;
}

int bar_lw (long long a, int x, int y, int z)
{
  return (a & (1LL<<x)) ? y : z;
}

long long bar_wl (int a, int x, long long y, long long z)
{
  return (a & (1<<x)) ? y : z;
}

long long bar_ll (long long a, int x, long long y, long long z)
{
  return (a & (1LL<<x)) ? y : z;
}

short bar_ws (int a, int x, short y, short z)
{
  return (a & (1<<x)) ? y : z;
}

short bar_ls (long long a, int x, short y, short z)
{
  return (a & (1LL<<x)) ? y : z;
}

/* { dg-final { scan-assembler-times "bt\[lq\]\[ \t\]" 7 } } */
/* { dg-final { scan-assembler-not "sar\[lq\]\[ \t\]" } } */

