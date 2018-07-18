/* PR debug/44223 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O3 -fsched-pressure -fschedule-insns -fpic -march=core2 -g" } */

struct S { unsigned int s1; int s2; };
struct T { int t; };

extern void extfn (struct S *);

static inline void
foo (struct S *s, unsigned char *x, int y)
{
  s->s2 = 32;
}

static inline void
bar (struct S *s, int n, unsigned int x)
{
  unsigned int s1;
  int s2;
  s1 = s->s1;
  s2 = s->s2;
  if (n < s2)
    s1 = (s1 << n) | x;
  s->s1 = s1;
}

int
baz (struct T *u, unsigned char *v, int w)
{
  struct S y;
  foo (&y, v, 7);
  bar (&y, 12, 0xfff);
  bar (&y, 2, u->t);
  extfn (&y);
}
