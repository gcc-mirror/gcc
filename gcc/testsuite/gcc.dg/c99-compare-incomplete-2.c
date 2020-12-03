/* Test comparisons of pointers to complete and incomplete types are
   diagnosed in C99 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

int
f (int (*p)[], int (*q)[3])
{
  return p < q; /* { dg-error "complete and incomplete" } */
}

int
f2 (int (*p)[], int (*q)[3])
{
  return p <= q; /* { dg-error "complete and incomplete" } */
}

int
f3 (int (*p)[], int (*q)[3])
{
  return p > q; /* { dg-error "complete and incomplete" } */
}

int
f4 (int (*p)[], int (*q)[3])
{
  return p >= q; /* { dg-error "complete and incomplete" } */
}

int
g (int (*p)[], int (*q)[3])
{
  return q < p; /* { dg-error "complete and incomplete" } */
}

int
g2 (int (*p)[], int (*q)[3])
{
  return q <= p; /* { dg-error "complete and incomplete" } */
}

int
g3 (int (*p)[], int (*q)[3])
{
  return q > p; /* { dg-error "complete and incomplete" } */
}

int
g4 (int (*p)[], int (*q)[3])
{
  return q >= p; /* { dg-error "complete and incomplete" } */
}
