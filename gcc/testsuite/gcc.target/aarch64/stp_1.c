/* { dg-options "-O -fpeephole2" } */

int res[2];

void
f1 (int x, int y)
{
  res[0] = res[1] = x + y;
}

void
f2 (int x, int y, int *res)
{
  res[0] = res[1] = x + y;
}

void
f3 (int x, int y)
{
  res[1] = res[0] = x + y;
}

void
f4 (int x, int y, int *res)
{
  res[1] = res[0] = x + y;
}

/* { dg-final { scan-assembler-times {\tstp\t} 4 } } */
