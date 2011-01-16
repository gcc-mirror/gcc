/* PR target/46880 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-require-effective-target sse2_runtime } */

typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double (*T)[2];

static __attribute__ ((noinline)) __m128d
foo (__m128d c, __m128d d)
{
  T cp = (T) &c;
  T dp = (T) &d;
  __m128d e = { (*cp)[1], (*dp)[1] };
  return e;
}

int
main ()
{
  __m128d c = { 1.0, 2.0 };
  __m128d d = { 3.0, 4.0 };
  union { __m128d x; double d[2]; } u;
  u.x = foo (c, d);
  if (u.d[0] != 2.0 || u.d[1] != 4.0)
    __builtin_abort ();
  return 0;
}
