/* PR c++/107358 */
/* { dg-do compile } */
/* { dg-options "-O2 -fexcess-precision=standard" } */

typedef float __attribute__((vector_size (4 * sizeof (float)))) A;
typedef double __attribute__((vector_size (2 * sizeof (double)))) B;

void
foo (A *x)
{
  *x = *x - 124.225514990f;
}

void
bar (A *x, float y)
{
  *x = *x - y;
}

void
baz (B *x)
{
  *x = *x + 124.225514990f;
}

void
qux (B *x, double y)
{
  *x = *x + y;
}
