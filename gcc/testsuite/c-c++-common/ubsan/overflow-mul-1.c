/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#define SCHAR_MAX __SCHAR_MAX__
#define SHRT_MAX __SHRT_MAX__
#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)

void __attribute__((noinline,noclone))
check (int i, int j)
{
  if (i != j)
    __builtin_abort ();
}

int
main (void)
{
  /* Test integer promotion.  */
#if __SCHAR_MAX__ == 127
  volatile signed char a = -2;
  volatile signed char b = SCHAR_MAX;
  volatile signed char c = a * b;
  check (c, 2);
#endif

#if __SHRT_MAX__ == 32767
  volatile short d = SHRT_MAX;
  volatile short e = 2;
  volatile short f = d * e;
  check (f, -2);
#endif

#if __INT_MAX__ == 2147483647
  volatile int m = INT_MAX;
  volatile int n = 1;
  volatile int o = m * n;
  check (o, INT_MAX);

  m = INT_MIN;
  o = m * n;
  check (o, INT_MIN);
#endif

  return 0;
}
