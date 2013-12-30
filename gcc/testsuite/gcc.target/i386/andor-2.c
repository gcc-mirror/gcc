/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

int h(int x, int y)
{
  if ((x >= 0 && x <= 1) && (y >= 0 && y <= 1))
    return x && y;
  else
    return -1;
}

int g(int x, int y)
{
  if ((x >= 0 && x <= 1) && (y >= 0 && y <= 1))
    return x || y;
  else
    return -1;
}

int f(int x, int y)
{
  if (x != 0 && x != 1)
    return -2;

  else
    return !x;
}

/* { dg-final { scan-assembler-not "setne" } } */
/* { dg-final { scan-assembler-not "sete" } } */
