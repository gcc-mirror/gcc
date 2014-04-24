/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable -fno-sanitize-recover" } */

#define SCHAR_MAX __SCHAR_MAX__
#define SCHAR_MIN (-__SCHAR_MAX__ - 1)
#define SHRT_MAX __SHRT_MAX__
#define SHRT_MIN (-__SHRT_MAX__ - 1)
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
#if __INT_MAX__ == 2147483647
  /* Here, nothing should fail.  */
  volatile int i = -1;
  volatile int j = INT_MIN;
  volatile int k = j - i;
  check (k, -2147483647);
  k = i - j;
  check (k, 2147483647);
  j++;
  check (j, -2147483647);

  i = 1;
  j = INT_MAX;
  k = i - j;
  check (k, -2147483646);
  k = j - i;
  check (k, 2147483646);
  j--;
  check (k, 2147483646);
#endif

  /* Test integer promotion.  */
#if __SCHAR_MAX__ == 127
  volatile signed char a = SCHAR_MIN;
  volatile signed char b = 1;
  volatile signed char c = a - b;
  check (c, 127);
  a--;
  check (a, 127);
#endif

#if __SHRT_MAX__ == 32767
  volatile short d = SHRT_MIN;
  volatile short e = 1;
  volatile short f = d - e;
  check (f, 32767);
  d--;
  check (d, 32767);
#endif
  return 0;
}
