/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
f1 (int a, int b)
{
  int tem = a & b;
  return a - tem;
}

int
f2 (int a, int b)
{
  int tem = b & a;
  return a - tem;
}

int
f3 (unsigned int a, int b)
{
  return a - (a & b);
}

int
f4 (int a, unsigned int b)
{
  return a - (a & b);
}

int
f5 (int a, int b)
{
  return a - (unsigned) (b & a);
}

/* { dg-final { scan-tree-dump-not " - " "cddce1" } } */
