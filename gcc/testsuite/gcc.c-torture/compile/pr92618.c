/* PR tree-optimization/92618 */

typedef long long __m128i __attribute__((__may_alias__, __vector_size__(2 * sizeof (long long))));
typedef long long __m256i __attribute__((__may_alias__, __vector_size__(4 * sizeof (long long))));
typedef long long __m512i __attribute__((__may_alias__, __vector_size__(8 * sizeof (long long))));

double a[32];
unsigned long long b[32];
__m128i bar (void);
__m256i qux (void);
__m512i corge (void);

void
foo (unsigned long long *x)
{
  __m128i c = bar ();
  __m128i d = bar ();
  __m256i e = qux ();
  __m256i f = qux ();
  __m256i g = qux ();
  __m512i h = corge ();
  __m512i i = corge ();
  *(__m128i *) &b[0] = c;
  *(__m128i *) &b[2] = d;
  *(__m256i *) &b[4] = e;
  *(__m256i *) &b[8] = f;
  *(__m256i *) &b[12] = g;
  *(__m512i *) &b[16] = h;
  *(__m512i *) &b[24] = i;
  *x = b[0] + b[1] + b[2] + b[3]
     + b[4] + b[5] + b[6] + b[7]
     + b[8] + b[9] + b[10] + b[11]
     + b[12] + b[13] + b[14] + b[15]
     + b[16] + b[17] + b[18] + b[19]
     + b[20] + b[21] + b[22] + b[23]
     + b[24] + b[25] + b[26] + b[27]
     + b[28] + b[29] + b[30] + b[31];
}

void
baz (double *x)
{
#if __SIZEOF_LONG_LONG__ == __SIZEOF_DOUBLE__
  __m128i c = bar ();
  __m128i d = bar ();
  __m256i e = qux ();
  __m256i f = qux ();
  __m256i g = qux ();
  __m512i h = corge ();
  __m512i i = corge ();
  *(__m128i *) &a[0] = c;
  *(__m128i *) &a[2] = d;
  *(__m256i *) &a[4] = e;
  *(__m256i *) &a[8] = f;
  *(__m256i *) &a[12] = g;
  *(__m512i *) &a[16] = h;
  *(__m512i *) &a[24] = i;
  *x = a[0] + a[1] + a[2] + a[3]
     + a[4] + a[5] + a[6] + a[7]
     + a[8] + a[9] + a[10] + a[11]
     + a[12] + a[13] + a[14] + a[15]
     + a[16] + a[17] + a[18] + a[19]
     + a[20] + a[21] + a[22] + a[23]
     + a[24] + a[25] + a[26] + a[27]
     + a[28] + a[29] + a[30] + a[31];
#endif
}
