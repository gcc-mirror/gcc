/* PR target/122390 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int f (int);
int g (int);

int f1 (unsigned a, unsigned b)
{
  unsigned t = a < b;
  int tt = a + b + t;
  if (tt < 0)
    return f(tt);
  return g(tt);
}

int f2 (unsigned a, unsigned b)
{
  unsigned t = a < b;
  int tt = a - b - t;
  if (tt < 0)
    return f(tt);
  return g(tt);
}

/* { dg-final { scan-assembler-not "test" } } */
