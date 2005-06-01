/* PR c/21536 */
/* { dg-do run } */
/* { dg-options "-O2 -Wuninitialized" } */

typedef __SIZE_TYPE__ size_t;
extern void *malloc (size_t);
extern void free (void *);

void *
foo (int x, int y)
{
  void *d = malloc (x * y * sizeof (double));
  double (*e)[x][y] = d;
  x += 10;
  y += 10;
  if (x > 18)
    (*e)[x - 12][y - 12] = 0.0;
  else
    (*e)[x - 11][y - 11] = 1.0;
  return d;
}

void *
bar (int x, int y)
{
  void *d = malloc (x * y * sizeof (double));
  struct S
    {
      double (*e)[x][y];
      double (*f)[x][y];
    } s;
  s.e = d;
  s.f = d;
  x += 10;
  y += 10;
  if (x > 18)
    (*s.e)[x - 12][y - 12] = 0.0;
  else
    (*s.e)[x - 11][y - 11] = 1.0;
  if (x > 16)
    (*s.f)[x - 13][y - 13] = 0.0;
  else
    (*s.f)[x - 14][y - 14] = 1.0;
  return d;
}

int
main ()
{
  void *d1 = foo (10, 10);
  void *d2 = bar (10, 10);
  free (d1);
  free (d2);
  return 0;
}
