/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1" } */

unsigned int f(unsigned int num)
{
  return num < 1 ? 1 : num;
}

unsigned int g(unsigned int num)
{
  return num > (unsigned)__INT_MAX__ * 2 ? (unsigned)__INT_MAX__ * 2 : num;
}

int h(int num)
{
  return num < -__INT_MAX__ ? -__INT_MAX__ : num;
}

int i(int num)
{
  return num > __INT_MAX__-1 ? __INT_MAX__-1 : num;
}

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "phiopt1" } } */
