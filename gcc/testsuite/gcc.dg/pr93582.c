/* PR tree-optimization/93582 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

struct S {
  unsigned int s1:1;
  unsigned int s2:1;
  unsigned int s3:1;
  unsigned int s4:1;
  unsigned int s5:4;
  unsigned char s6;
  unsigned short s7;
  unsigned short s8;
};
struct T {
  int t1;
  int t2;
};

static inline int
bar (struct S *x)
{
  if (x->s4)
    return ((struct T *)(x + 1))->t1 + ((struct T *)(x + 1))->t2;	/* { dg-bogus "array subscript 1 is outside array bounds of" } */
  else
    return 0;
}

int
foo (int x, int y)
{
  struct S s;								/* { dg-bogus "while referencing" } */
  s.s6 = x;
  s.s7 = y & 0x1FFF;
  s.s4 = 0;
  return bar (&s);
}

static inline int
qux (struct S *x)
{
  int s4 = x->s4;
  if (s4)
    return ((struct T *)(x + 1))->t1 + ((struct T *)(x + 1))->t2;
  else
    return 0;
}

int
baz (int x, int y)
{
  struct S s;
  s.s6 = x;
  s.s7 = y & 0x1FFF;
  s.s4 = 0;
  return qux (&s);
}
