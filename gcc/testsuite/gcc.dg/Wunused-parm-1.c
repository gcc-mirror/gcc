/* { dg-do compile } */
/* { dg-options "-Wunused -W" } */

long
f1 (unsigned long long x)
{
  unsigned long long a = 1;
  const union { unsigned long long l; unsigned int p[2]; } b = { .l = x };
  const union { unsigned long long l; unsigned int p[2]; } c = { .l = a };
  return b.p[0] + c.p[0];
}

int
f2 (int x, int y)
{
  int a = 1;
  int b[] = { 1, 2, x, a, 3, 4 };
  return b[y];
}

int
f3 (int a,	/* { dg-warning "unused parameter" } */
    int b,	/* { dg-warning "set but not used" } */
    int c)
{
  b = 1;
  c = 1;
  return c;
}
