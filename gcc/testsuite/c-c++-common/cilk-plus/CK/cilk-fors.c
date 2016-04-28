/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#if HAVE_IO
#include <stdio.h>
#endif

static void check (int *Array, int start, int end, int incr, int value)
{
  int ii = 0;
  for (ii = start;  ii < end; ii = ii + incr)
    if (Array[ii] != value)
      __builtin_abort ();
#if HAVE_IO
  printf ("Passed\n");
#endif
}

static void check_reverse (int *Array, int start, int end, int incr, int value)
{
  int ii = 0;
  for (ii = start; ii >= end; ii = ii - incr)
    if (Array[ii] != value)
      __builtin_abort ();
#if HAVE_IO
  printf ("Passed\n");
#endif
}


int main (void)
{
  int Array[10];
  int x = 9, y = 0, z = 3;


  _Cilk_for (int ii = 0; ii < 10; ii++)
    Array[ii] = 1133;
  check (Array, 0, 10, 1, 1133);

  _Cilk_for (int ii = 0; ii < 10; ++ii)
    Array[ii] = 3311;
  check (Array, 0, 10, 1, 3311);

  _Cilk_for (int ii = 9; ii > -1; ii--)
    Array[ii] = 4433;
  check_reverse (Array, 9, 0, 1, 4433);

  _Cilk_for (int ii = 9; ii > -1; --ii)
    Array[ii] = 9988;
  check_reverse (Array, 9, 0, 1, 9988);

  _Cilk_for (int ii = 0; ii < 10; ++ii)
    Array[ii] = 3311;
  check (Array, 0, 10, 1, 3311);

  _Cilk_for (int ii = 0; ii < 10; ii += 2)
    Array[ii] = 1328;
  check (Array, 0, 10, 2, 1328);

  _Cilk_for (int ii = 9; ii >= 0; ii -= 2)
    Array[ii] = 1738;
  check_reverse (Array, 9, 0, 2, 1738);


  _Cilk_for (int ii = 0; ii < 10; ii++)
    {
      if (ii % 2)
	Array[ii] = 1343;
      else
	Array[ii] = 3413;
    }

  check (Array, 1, 10, 2, 1343);
  check (Array, 0, 10, 2, 3413);

  _Cilk_for (short cc = 0; cc < 10; cc++)
    Array[cc] = 1343;
  check (Array, 0, 10,  1,1343);

  _Cilk_for (short cc = 9; cc >= 0; cc--)
    Array[cc] = 1348;
  check_reverse (Array, 9, 0, 1, 1348);
  return 0;
}
