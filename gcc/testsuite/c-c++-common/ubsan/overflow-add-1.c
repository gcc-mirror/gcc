/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable -fno-sanitize-recover=signed-integer-overflow" } */

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
#if __INT_MAX__ == 2147483647
  /* Here, nothing should fail.  */
  volatile int j = INT_MAX;
  volatile int i = -1;
  volatile int k = j + i;
  check (k, 2147483646);
  k = i + j;
  check (k, 2147483646);
  j--;
  check (j, 2147483646);

  i = 1;
  j = INT_MIN;
  k = i + j;
  check (k, -2147483647);
  k = j + i;
  check (k, -2147483647);
  j++;
  check (j, -2147483647);
#endif

  /* Test integer promotion.  */
#if __SCHAR_MAX__ == 127
  volatile signed char a = SCHAR_MAX;
  volatile signed char b = 1;
  volatile signed char c = a + b;
  check (c, -128);
  a++;
  check (a, -128);
#endif

#if __SHRT_MAX__ == 32767
  volatile short d = SHRT_MAX;
  volatile short e = 1;
  volatile short f = d + e;
  check (f, -32768);
  d++;
  check (d, -32768);
#endif
  return 0;
}
