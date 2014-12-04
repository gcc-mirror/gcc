/* PR middle-end/56917 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

#define INT_MIN (-__INT_MAX__ - 1)
#define LONG_MIN (-__LONG_MAX__ - 1L)
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1LL)

int __attribute__ ((noinline,noclone))
fn1 (unsigned int u)
{
  return (-(int) (u - 1U)) - 1;
}

long __attribute__ ((noinline,noclone))
fn2 (unsigned long int ul)
{
  return (-(long) (ul - 1UL)) - 1L;
}

long long __attribute__ ((noinline,noclone))
fn3 (unsigned long long int ull)
{
  return (-(long long) (ull - 1ULL)) - 1LL;
}

int
main (void)
{
  if (fn1 (__INT_MAX__ + 1U) != INT_MIN
      || fn2 (__LONG_MAX__ + 1UL) != LONG_MIN
      || fn3 (__LONG_LONG_MAX__ + 1ULL) != LLONG_MIN)
    __builtin_abort ();
}
