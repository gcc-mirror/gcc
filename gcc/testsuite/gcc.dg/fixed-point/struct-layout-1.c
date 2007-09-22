/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* This test is based on the test from ../dfp/.  */

#include <stdarg.h>

extern void abort (void);

struct S1
{
  long _Fract a[0];
};

struct S2
{
  struct
  {
    long _Fract e;
  } b[0];
};

struct S3
{
  union
  {
    long _Fract c;
  } a[0];
};

struct S4
{
  int a[0];
  long _Fract b[0];
};

struct S5
{
  union
  {
    long _Fract c[0];
  } a;
};

int check_var (int z, ...)
{
  long long result;
  va_list ap;
  va_start (ap, z);
  va_arg (ap, struct S1);
  result = va_arg (ap, long long);
  va_end (ap);

  return (result == 2LL);
}

int main ()
{
  struct S1 s1;
  struct S2 s2;
  struct S3 s3;
  struct S4 s4;
  struct S5 s5;

  if (check_var (2, s1, 2LL) == 0)
    abort ();
  if (check_var (2, s2, 2LL) == 0)
    abort ();
  if (check_var (2, s3, 2LL) == 0)
    abort ();
  if (check_var (2, s4, 2LL) == 0)
    abort ();
  if (check_var (2, s5, 2LL) == 0)
    abort ();

  return 0;
}
