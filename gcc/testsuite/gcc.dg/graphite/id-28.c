/* { dg-do compile { target cilkplus } } */
/* { dg-options "-fcilkplus -floop-nest-optimize -O3" } */

#if HAVE_IO
#include <stdio.h>
#endif
#include <math.h>
#define NUMBER 5

int func1 (int *a1, int *a2)
{
  return __sec_reduce_add (a1[0:NUMBER] * a2[0:NUMBER:1]);
}

int func2 (int *a1, int *a2)
{
  return (__sec_reduce_add (a1[0:NUMBER] * a2[0:NUMBER]) +
	  __sec_reduce_mul (a1[0:NUMBER] + a2[0:NUMBER]));
}

int func3 (int *a1, int *a2)
{
  return (int) sqrt ((double)(__sec_reduce_add (a1[0:NUMBER] * a2[0:NUMBER]) +
			      a2[0] + a2[1] + a2[3]));
}

int func4 (int *a1, int *a2)
{
  return a1[NUMBER-1] * (__sec_reduce_add (a1[0:NUMBER] * a2[0:NUMBER]) + a2[0] + a2[1] + a2[3])/a1[NUMBER-2];
}
int main(void)
{
  int array[NUMBER], array2[NUMBER];
  int return_value = 0;
  int ii = 0;
  int argc = 1;
  __asm volatile ("" : "+r" (argc));
  for (ii = 0; ii < NUMBER; ii++)
    {
      array[ii] = argc; /* This should calculate to 1.  */
      array2[ii]  = argc * argc + argc;  /* This should calculate to 2.  */
    }

  return_value = func1 (array, array2);
#if HAVE_IO
  printf("Return_value = %d\n", return_value);
#endif
  if (return_value != (2+2+2+2+2))
    return 1;

  return_value = func2 (array2, array);
#if HAVE_IO
  printf("Return_value = %d\n", return_value);
#endif
  if (return_value != (3*3*3*3*3) + (2+2+2+2+2))
    return 2;

  return_value = func3 (array, array2);
#if HAVE_IO
  printf("Return_value = %d\n", return_value);
#endif
  if (return_value != 4)
    return 3;

  return_value = func4 (array, array2);
#if HAVE_IO
  printf("Return_value = %d\n", return_value);
#endif
  if (return_value != 16)
    return 4;

  return 0;
}
