/* PR tree-optimization/29637 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

typedef struct __attribute__ ((aligned (8)))
{
    short a, b, c, d;
} A;

typedef struct
{
  A a[24];
} B;

static const A b = { 0, 0, 1, -1 };

void
foo (B *x)
{
  int i;
  for (i = 0; i <= 20; i += 4)
    x->a[i] = b;
}
