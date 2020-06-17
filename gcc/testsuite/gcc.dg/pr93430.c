/* PR target/93430 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx -mno-avx2" { target avx } } */

typedef double V __attribute__((vector_size (4 * sizeof (double))));
typedef long long VI __attribute__((vector_size (4 * sizeof (long long))));

#if __SIZEOF_DOUBLE__ == __SIZEOF_LONG_LONG__
void
foo (V *x, V *y)
{
  y[0] = __builtin_shuffle (x[0], x[0], (VI) { 0, 0, 0, 0 });
}

void
bar (V *x, V *y)
{
  y[0] = __builtin_shuffle (x[0], x[0], (VI) { 1, 1, 1, 1 });
}

void
baz (V *x, V *y)
{
  y[0] = __builtin_shuffle (x[0], x[0], (VI) { 2, 2, 2, 2 });
}

void
qux (V *x, V *y)
{
  y[0] = __builtin_shuffle (x[0], x[0], (VI) { 3, 3, 3, 3 });
}
#endif
