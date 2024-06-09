/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize" } */

struct A { float x, y; };
struct B { struct A u; };

extern void bar (struct A *);

float
f3 (struct B *x, int y)
{
  struct A p = {1.0f, 2.0f};
  struct A *q = &x[y].u;

  __builtin_memcpy (&q->x, &p.x, sizeof (float));
  __builtin_memcpy (&q->y, &p.y, sizeof (float));

  bar (&p);

  return x[y].u.x + x[y].u.y;
}
