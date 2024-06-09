/* PR target/114339 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

typedef long long V __attribute__((vector_size (16)));

__attribute__((noipa)) V
foo (V a)
{
  return a <= (V) {0, __LONG_LONG_MAX__ };
}

int
main ()
{
  V t = foo ((V) { 0, 0 });
  if (t[0] != -1LL || t[1] != -1LL)
    __builtin_abort ();
}
