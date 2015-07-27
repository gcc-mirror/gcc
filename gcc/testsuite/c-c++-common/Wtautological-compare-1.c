/* PR c++/66555 */
/* { dg-do compile } */
/* { dg-options "-Wtautological-compare" } */

#define X 5
#define Y 5
#define A a
enum { U };

void
fn1 (int a, int *p)
{
  if (a > a); /* { dg-warning "self-comparison always evaluates to false" } */
  if (a < a); /* { dg-warning "self-comparison always evaluates to false" } */
  if (a >= a); /* { dg-warning "self-comparison always evaluates to true" } */
  if (a <= a); /* { dg-warning "self-comparison always evaluates to true" } */
  if (a == a); /* { dg-warning "self-comparison always evaluates to true" } */
  if (a != a); /* { dg-warning "self-comparison always evaluates to false" } */
  if (A == A); /* { dg-warning "self-comparison always evaluates to true" } */
  if ((unsigned) a != (unsigned) a);
  if ((a + 1) <= (a + 1)); /* { dg-warning "self-comparison always evaluates to true" } */
  if (1 ? a == a : 0); /* { dg-warning "self-comparison always evaluates to true" } */
  if (fn1 == fn1); /* { dg-warning "self-comparison always evaluates to true" } */
  if (*p == *p); /* { dg-warning "self-comparison always evaluates to true" } */

  volatile int v = 5;
  if (v == v);
  if (v != v);
}

void
fn2 (int a)
{
  if (sizeof (int) >= 4);
  if (sizeof (char) != 1);
  if (sizeof (long) != sizeof (long long));
  if (0 < sizeof (short));
  if (5 != 5);
  if (X > 5);
  if (X == X);
  if (3 + 4 == 6 + 1);
  if ((unsigned) a != (unsigned long) a);
  if (U == U);
  if (U > 0);
}

void
fn3 (int i, int j)
{
  static int a[16];
  static int b[8][8];

  if (a[5] == a[5]);
  if (a[X] != a[Y]);
  if (a[X] != a[X]);
  if (a[i] == a[i]); /* { dg-warning "self-comparison always evaluates to true" } */
  if (b[5][5] == b[5][5]);
  if (b[X][Y] >= b[Y][X]);
  if (b[X][X] == b[Y][Y]);
  if (b[i][j] != b[i][j]); /* { dg-warning "self-comparison always evaluates to false" } */
  if (b[i][Y] < b[i][X]);
  if (b[X][j] < b[X][j]);
  if ((a[i] + 4) == (4 + a[i])); /* { dg-warning "self-comparison always evaluates to true" } */
}

int
fn4 (int x, int y)
{
  return x > x ? 1 : 0; /* { dg-warning "self-comparison always evaluates to false" } */
}
