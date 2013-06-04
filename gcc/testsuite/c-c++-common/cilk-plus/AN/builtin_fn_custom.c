/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#if HAVE_IO
#include <stdio.h>
#endif
/* #include <stdlib.h> */

double my_func (double x, double y)
{
  if (x > y)
    return x;
  else
    return y;
}


/* char __sec_reduce_add (int *); */
int main(void)
{
  int ii,array[10], y = 0, y_int = 0, array2[10];
  double x, yy, array3[10], array4[10];
  double max_value = 0.000, min_value = 0.000, add_value, mul_value = 1.00;
  int max_index = 0, min_index = 0;
  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = 1+ii;
      array2[ii]= 2; 
    }

  for (ii = 0; ii < 10; ii++)
    {
      if (ii%2 && ii)
	array3[ii] = (double)(1.0000/(double)ii);
      else
	array3[ii] = (double) ii + 0.10;
      array4[ii] = (double) (1.00000/ (double)(ii+1));
    }

  /* array[:] = 5; */
  x = __sec_reduce (0, array3[:] * array4[:], my_func); 
  y = __sec_reduce_max_ind ( array3[:] * array4[:]);

  /* Initialize it to the first variable.  */
  max_value = array3[0] * array4[0];
  for (ii = 0; ii < 10; ii++)
    if (array3[ii] * array4[ii] > max_value) {
      max_value = array3[ii] * array4[ii];
      max_index = ii;
    }
    
  
  
#if HAVE_IO
  for (ii = 0; ii < 10; ii++) 
    printf("%5.3f ", array3[ii] * array4[ii]);
  printf("\n");
  printf("Max = %5.3f\t Max Index = %2d\n", x, y);
#endif

  if (x != max_value)
    return 1;

  if (y != max_index)
    return 2;

  return 0;
}
