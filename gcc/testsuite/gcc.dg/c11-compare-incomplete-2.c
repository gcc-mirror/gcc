/* Test comparisons of pointers to complete and incomplete types are
   diagnosed in C11 mode with -Wc99-c11-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wc99-c11-compat" } */

int
f (int (*p)[], int (*q)[3])
{
  return p < q; /* { dg-warning "complete and incomplete" } */
}

int
f2 (int (*p)[], int (*q)[3])
{
  return p <= q; /* { dg-warning "complete and incomplete" } */
}

int
f3 (int (*p)[], int (*q)[3])
{
  return p > q; /* { dg-warning "complete and incomplete" } */
}

int
f4 (int (*p)[], int (*q)[3])
{
  return p >= q; /* { dg-warning "complete and incomplete" } */
}

int
g (int (*p)[], int (*q)[3])
{
  return q < p; /* { dg-warning "complete and incomplete" } */
}

int
g2 (int (*p)[], int (*q)[3])
{
  return q <= p; /* { dg-warning "complete and incomplete" } */
}

int
g3 (int (*p)[], int (*q)[3])
{
  return q > p; /* { dg-warning "complete and incomplete" } */
}

int
g4 (int (*p)[], int (*q)[3])
{
  return q >= p; /* { dg-warning "complete and incomplete" } */
}
