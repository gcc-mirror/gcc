/* { dg-additional-options "-fnon-call-exceptions -O" } */

typedef unsigned char C;
typedef unsigned char __attribute__((__vector_size__ (4))) V;

C m;

static inline void
bar (C c, V v, V *r)
{
  v %= (c | v) % m;
  *r = v;
}

void
foo (void)
{
  V x;
  bar (0, (V){2}, &x);
}
