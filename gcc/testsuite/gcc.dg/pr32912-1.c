/* PR middle-end/32912 */
/* { dg-do run } */
/* { dg-options "-O2 -w" } */
/* { dg-options "-O2 -w -fno-common" { target hppa*-*-hpux* } } */
/* { dg-options "-O2 -w -msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse_runtime { target { i?86-*-* x86_64-*-* } } } */

extern void abort (void);

typedef int __m128i __attribute__ ((__vector_size__ (16)));

__m128i a, b, c, d, e, f;

void
foo (__m128i x)
{
  a = x ^ ~x;
  b = ~x ^ x;
  c = x | ~x;
  d = ~x | x;
  e = x & ~x;
  f = ~x & x;
}

int
main (void)
{
  union { __m128i v; int i[sizeof (__m128i) / sizeof (int)]; } u;
  int i;

  for (i = 0; i < sizeof (u.i) / sizeof (u.i[0]); i++)
    u.i[i] = i * 49 - 36;
  foo (u.v);
#define check(x, val) \
  u.v = (x); \
  for (i = 0; i < sizeof (u.i) / sizeof (u.i[0]); i++) \
    if (u.i[i] != (val)) \
      abort ()

  check (a, ~0);
  check (b, ~0);
  check (c, ~0);
  check (d, ~0);
  check (e, 0);
  check (f, 0);
  return 0;
}
