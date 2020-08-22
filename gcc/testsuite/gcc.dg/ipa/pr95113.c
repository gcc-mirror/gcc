/* { dg-do run } */
/* { dg-options "-O2 -fexceptions -fnon-call-exceptions" } */
/* { dg-require-effective-target exceptions } */

int a, b;

static inline long int
foo (long int x, int y)
{
  if (y == 0)
    return 0;

  if (x == -1 && y == -1)
    return 0;

  return x / y;
}

static inline int
bar (int *p)
{
  int c = foo (a, 1) + *p;
  return b;
}

int
main ()
{
  int d = 0;
  b = foo (1, 1);
  bar (&d);
  return 0;
}
