/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#if HAVE_IO
#include <stdio.h>
#endif

int main (void)
{
  int Array[10][10];


  for (int ii = 0; ii < 10; ii++)
    for (int jj = 0; jj < 10; jj++)
	{
	  Array[ii][jj] = 0;
	}

  _Cilk_for (int ii = 0; ii < 10; ii++)
    _Cilk_for (int jj = 0; jj < 5; jj++)
      Array[ii][jj] = 5;

  for (int ii = 0; ii < 10; ii++)
    for (int jj = 0; jj < 5; jj++)
      if (Array[ii][jj] != 5)
#if HAVE_IO
	printf("Array[%d][%d] = %d\n", ii, jj, Array[ii][jj]);
#else
	__builtin_abort ();
#endif


  /* One goes up and one goes down.  */
  _Cilk_for (int ii = 0; ii < 10; ii++)
    _Cilk_for (int jj = 9; jj >= 0; jj--)
      Array[ii][jj] = 7;

  for (int ii = 0; ii < 10; ii++)
    for (int jj = 9; jj >= 0; jj--)
      if (Array[ii][jj] != 7)
#if HAVE_IO
	printf("Array[%d][%d] = %d\n", ii, jj, Array[ii][jj]);
#else
	__builtin_abort ();
#endif

  /* different step sizes.  */
  _Cilk_for (int ii = 0; ii < 10; ii++)
    _Cilk_for (int jj = 0; jj < 10; jj += 2)
      Array[ii][jj] = 9;

  for (int ii = 0; ii < 10; ii++)
    for (int jj = 0; jj < 10; jj += 2)
      if (Array[ii][jj] != 9)
#if HAVE_IO
	printf("Array[%d][%d] = %d\n", ii, jj, Array[ii][jj]);
#else
	__builtin_abort ();
#endif

  /* different step sizes.  */
  _Cilk_for (int ii = 0; ii < 10; ii += 2)
    _Cilk_for (int jj = 5; jj < 9; jj++)
      Array[ii][jj] = 11;

  for (int ii = 0; ii < 10; ii += 2)
    for (int jj = 5; jj < 9; jj++)
      if (Array[ii][jj] != 11)
#if HAVE_IO
	printf("Array[%d][%d] = %d\n", ii, jj, Array[ii][jj]);
#else
	__builtin_abort ();
#endif

  return 0;
}

