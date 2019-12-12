/* PR tree-optimization/92618 */

typedef long long __m128i __attribute__((__may_alias__, __vector_size__(2 * sizeof (long long))));

double a[4];
unsigned long long b[4];

__attribute__((noipa)) __m128i
bar (void)
{
  static int cnt;
  cnt += 2;
  return (__m128i) { cnt, cnt + 1 };
}

#if __SIZEOF_LONG_LONG__ == __SIZEOF_DOUBLE__
typedef double __m128d __attribute__((__may_alias__, __vector_size__(2 * sizeof (double))));

__attribute__((noipa)) __m128i
qux (void)
{
  static double cnt;
  cnt += 2.0;
  return (__m128i) (__m128d) { cnt, cnt + 1.0 };
}
#endif

void
foo (unsigned long long *x)
{
  __m128i c = bar ();
  __m128i d = bar ();
  *(__m128i *) &b[0] = c;
  *(__m128i *) &b[2] = d;
  *x = b[0] + b[1] + b[2] + b[3];
}

void
baz (double *x)
{
#if __SIZEOF_LONG_LONG__ == __SIZEOF_DOUBLE__
  __m128i c = qux ();
  __m128i d = qux ();
  *(__m128i *) &a[0] = c;
  *(__m128i *) &a[2] = d;
  *x = a[0] + a[1] + a[2] + a[3];
#endif
}

int
main ()
{
  unsigned long long c = 0;
  foo (&c);
  if (c != 2 + 3 + 4 + 5)
    __builtin_abort ();
#if __SIZEOF_LONG_LONG__ == __SIZEOF_DOUBLE__
  double d = 0.0;
  baz (&d);
  if (d != 2.0 + 3.0 + 4.0 + 5.0)
    __builtin_abort ();
#endif
}
