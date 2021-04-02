/* PR tree-optimization/98474 */

#ifdef __SIZEOF_INT128__
typedef __uint128_t T;
#define N (__SIZEOF_INT128__ * __CHAR_BIT__ / 2)
#else
typedef unsigned long long T;
#define N (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ / 2)
#endif

__attribute__ ((noipa)) void
foo (T *x)
{
  *x += ((T) 1) << (N + 1);
}

int
main ()
{
  T a = ((T) 1) << (N + 1);
  T b = a;
  T n;
  foo (&b);
  n = b;
  while (n >= a)
    n -= a;
  if ((int) (n >> N) != 0)
    __builtin_abort ();
  return 0;
}
