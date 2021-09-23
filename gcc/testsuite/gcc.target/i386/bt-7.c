/* PR rtl-optimization/46235 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=core2" } */

unsigned char set1_lb (long long x, int y)
{
  return (x & (1LL<<y)) != 0;
}

unsigned char set2_lb (long long x, int y)
{
  return (x >> y) & 1;
}

unsigned char clr1_lb (long long x, int y)
{
  return (x & (1LL<<y)) == 0;
}

unsigned char clr2_lb (long long x, int y)
{
  return !((x >> y) & 1);
}

int clr1_lw (long long x, int y)
{
  return (x & (1LL<<y)) == 0;
}

int clr2_lw (long long x, int y)
{
  return !((x >> y) & 1);
}

long long clr1_bl (unsigned char x, int y)
{
  return (x & (1<<y)) == 0;
}

long long clr2_bl (unsigned char x, int y)
{
  return !((x >> y) & 1);
}

long long clr1_wl (int x, int y)
{
  return (x & (1<<y)) == 0;
}

long long clr2_wl (int x, int y)
{
  return !((x >> y) & 1);
}

long long clr1_ll (long long x, int y)
{
  return (x & (1LL<<y)) == 0;
}

long long clr2_ll (long long x, int y)
{
  return !((x >> y) & 1);
}

/* { dg-final { scan-assembler-times "bt\[lq\]\[ \t\]" 12 } } */
/* { dg-final { scan-assembler-not "sar\[lq\]\[ \t\]" } } */
/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
/* { dg-final { scan-assembler-not "not\[lq\]\[ \t\]" } } */

