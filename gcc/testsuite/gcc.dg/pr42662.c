/* PR debug/42662 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

struct S { unsigned long s[17]; };

static inline void
foo (struct S *r, struct S *a, unsigned n)
{
  unsigned b = n / 8;
  r->s[0] = (b >= 1 ? : a->s[1 - b]);
}

static inline void
bar (struct S *r, struct S *a)
{
  r->s[0] = a->s[0] << 1;
}

static inline void
baz (struct S *r, struct S *a, struct S *b)
{
  unsigned c = 0;
  int i;
  for (i = 0; i < 3; ++i)
    {
      unsigned long d = a->s[i];
      long e = d + b->s[i];
      if (c)
	++e == 0;
      c = e < d;
      r->s[i] = e;
    }
}

void
test (struct S *r, int s, int d)
{
  struct S u;
  if (s)
    {
      bar (&u, r);
      foo (r, r, 3);
      baz (r, r, &u);
    }
  u.s[0] = d;
  baz (r, r, &u);
}
