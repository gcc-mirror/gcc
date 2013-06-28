/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 100
#if HAVE_IO
#include <stdio.h>
#endif

int main(void)
{
  int argc = 1;
  int ii,array[NUMBER], y = 0, y_int = 0, array2[NUMBER], y_int2=0, y2=0;
  double x, yy, array3[NUMBER], array4[NUMBER];
  int all_zero, all_nonzero, any_zero, any_nonzero;
  for (ii = 0; ii < NUMBER; ii++)
    {
      array[ii] = 0;
      array2[ii] = 5;
      if (ii%2 && ii)
	array3[ii] = (double)(1.0000/(double)ii);
      else
	array3[ii] = (double) ii + 0.00;
      array4[ii] = (double) (1.00000/ (double)(ii+1));
    }
  __asm volatile ("" : "+r" (argc));
  y_int = __sec_reduce_any_nonzero (array3[:] + array[4]); 
  y_int2 = __sec_reduce_any_zero (array3[:] + array[4]); 
  y = __sec_reduce_all_nonzero ((array3[:] + array4[:]) * (argc-1)); 
  y2 = __sec_reduce_all_zero ((array3[:] + array4[:]) * (argc-1));

  any_zero = 0;
  any_nonzero = 0;
  for (ii = 0; ii < NUMBER; ii++)
    {
      if ((array3[ii] + array[4]) == 0)
	any_zero = 1;
      else
	any_nonzero = 1;
    }

  if (any_nonzero != y_int)
    return 1;
  if (any_zero != y_int2)
    return 2;


  all_zero = 0;
  all_nonzero = 0;
  for (ii = 0; ii < NUMBER; ii++)
    {
      if (((array3[ii] + array4[ii]) * (argc-1)) == 0)
	all_zero = 1;
      else
	all_nonzero = 1;
    }

  if (y != all_nonzero)
    return 3;
  if (all_zero != y2)
    return 4;
 

#if HAVE_IO
  for (ii = 0; ii < NUMBER; ii++) {
    printf("%5.3f ", array3[ii] +array4[ii]);
  }
  printf("\n");
  for (ii = 0; ii < NUMBER; ii++) {
    printf("%5.3f ", (array3[ii] + array4[ii]) * (argc-1));
  }
  printf("\n");
  printf("Any Non-zeros (1st line) = %d\t All non-zeros (1st line) = %d\n", 
	 y_int, y);
  printf("Any zeros (2nd line) = %d\t All zeros (2nd line) = %d\n", y_int2, y2);
#endif
  return 0;
}
