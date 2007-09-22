/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Based on the test from ../dfp/.  */

#include <stdarg.h>

extern void abort (void);

struct S1
{
  struct
  {
    _Fract e;
  } b[0];
};

/* Test handling vararg parameters whose size is 0.  */

int check_var(int z,...)
{
  double d;
  struct S1 s1;
  long long result;
  va_list ap;
  va_start (ap, z);
  d = va_arg (ap, double);
  s1 = va_arg (ap, struct S1);
  result = va_arg (ap, long long);
  va_end (ap);
  return (result == 2LL);

}

int
main ()
{
  struct S1 s1;
  struct S1 a1[5];

  if (check_var(5, 1.0, s1, 2LL, a1[2], a1[2]) == 0)
    abort ();

  return 0;
}
