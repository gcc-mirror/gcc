/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */

void sink2(int, int);
void sink3(int, int, int);

void cond1(int cond, int x, int y)
{
  if (cond)
    {
      x = x << 4;
      y = 1;
    }

  sink2(x, y);
}

void cond2(int cond, int x, int y)
{
  if (cond)
    {
      x++;
      y++;
    }

  sink2(x, y);
}

void cond3(int cond, int x1, int x2, int x3)
{
  if (cond)
    {
      x1++;
      x2++;
      x3++;
    }

  sink3(x1, x2, x3);
}

void cond4(int cond, int x, int y)
{
  if (cond)
    {
      x += 2;
      y += 3;
    }

  sink2(x, y);
}

void cond5(int cond, int x, int y, int r1, int r2)
{
  if (cond)
    {
      x = r1 + 2;
      y = r2 - 34;
    }

  sink2(x, y);
}

void cond6(int cond, int x, int y)
{
  if (cond)
    {
      x = -x;
      y = ~y;
    }

  sink2(x, y);
}

/* { dg-final { scan-assembler-times "cinc\t" 5 } } */
/* { dg-final { scan-assembler-times "csneg\t" 1 } } */
/* { dg-final { scan-assembler-times "csinv\t" 1 } } */
/* { dg-final { scan-assembler "csel\t" } } */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 6 "ce1" } } */
