/* PR tree-optimization/126262 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=gnu23" } */

#if __BITINT_MAXWIDTH__ >= 1024
typedef unsigned _BitInt (512) A;
typedef _BitInt (1024) B;

[[gnu::noipa]] int
foo (signed char x, A y)
{
  B b = -(B) y;
  A c;
  if (__builtin_mul_overflow (1, b, &c))
    c = 42;
  B f;
  if (__builtin_mul_overflow (x, 9, &f))
    f = 42;
  int i;
  if (__builtin_mul_overflow (f, 1, &i))
    i = 42;
  return c + i;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 1024
  if (foo (1, -2) != 51)
    __builtin_abort ();
#endif
}
