/* { dg-do run } */
/* { dg-additional-options "-Wno-psabi -w" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */

typedef unsigned V __attribute__ ((__vector_size__ (16)));
union U
{
  V j;
  unsigned long long i __attribute__ ((__vector_size__ (16)));
};

static inline __attribute__((always_inline)) V
foo (unsigned long long a)
{
  union U z = { .j = (V) {} };
  for (unsigned long i = 0; i < 1; i++)
    z.i[i] = a;
  return z.j;
}

static inline __attribute__((always_inline)) V
bar (V a, unsigned long long i, int q)
{
  union U z = { .j = a };
  z.i[q] = i;
  return z.j;
}

int
main ()
{
  union U z = { .j = bar (foo (1729), 2, 1) };
  if (z.i[0] != 1729)
    __builtin_abort ();
  return 0;
}
