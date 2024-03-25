/* PR target/112816 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#define N 2
struct S { float x[N]; };
struct T { int x[N]; };

struct T
foo (struct S x)
{
  struct T res;
  for (int i = 0; i < N; ++i)
    res.x[i] = __builtin_signbit (x.x[i]) ? -1 : 0;
  return res;
}
