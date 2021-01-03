/* Test comparisons of pointers to complete and incomplete types are
   accepted in C11 mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int
f (int (*p)[], int (*q)[3])
{
  return p < q;
}

int
f2 (int (*p)[], int (*q)[3])
{
  return p <= q;
}

int
f3 (int (*p)[], int (*q)[3])
{
  return p > q;
}

int
f4 (int (*p)[], int (*q)[3])
{
  return p >= q;
}

int
g (int (*p)[], int (*q)[3])
{
  return q < p;
}

int
g2 (int (*p)[], int (*q)[3])
{
  return q <= p;
}

int
g3 (int (*p)[], int (*q)[3])
{
  return q > p;
}

int
g4 (int (*p)[], int (*q)[3])
{
  return q >= p;
}
