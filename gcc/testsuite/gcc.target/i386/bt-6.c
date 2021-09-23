/* PR rtl-optimization/46235 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

unsigned char set1_bb (unsigned char x, int y)
{
  return (x & (1<<y)) != 0;
}

unsigned char set2_bb (unsigned char x, int y)
{
  return (x >> y) & 1;
}

unsigned char set1_wb (int x, int y)
{
  return (x & (1<<y)) != 0;
}

unsigned char set2_wb (int x, int y)
{
  return (x >> y) & 1;
}

unsigned char clr1_bb (unsigned char x, int y)
{
  return (x & (1<<y)) == 0;
}

unsigned char clr2_bb (unsigned char x, int y)
{
  return !((x >> y) & 1);
}

unsigned char clr1_wb (int x, int y)
{
  return (x & (1<<y)) == 0;
}

unsigned char clr2_wb (int x, int y)
{
  return !((x >> y) & 1);
}

int clr1_bw (unsigned char x, int y)
{
  return (x & (1<<y)) == 0;
}

int clr2_bw (unsigned char x, int y)
{
  return !((x >> y) & 1);
}

int clr1_ww (int x, int y)
{
  return (x & (1<<y)) == 0;
}

int clr2_ww (int x, int y)
{
  return !((x >> y) & 1);
}

/* { dg-final { scan-assembler-times "bt\[lq\]\[ \t\]" 12 } } */
/* { dg-final { scan-assembler-not "sar\[lq\]\[ \t\]" } } */
/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
/* { dg-final { scan-assembler-not "not\[lq\]\[ \t\]" } } */

