/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 20
#if HAVE_IO
#include <stdio.h>
#endif

float array4[NUMBER][NUMBER][NUMBER][NUMBER];
int main(void)
{
  int array[NUMBER][NUMBER], array2[NUMBER], array3[NUMBER], x = 0, y;
  int x_correct, y_correct, ii, jj = 0, kk = 0, ll = 0;
  for (ii = 0; ii < NUMBER; ii++)
    {
      for (jj = 0; jj < NUMBER; jj++)
	{
	  array[ii][jj] = 1+ii;
	  array2[ii]= 2;
	  array3[ii]= 3;
	}
    }
  
  array[array2[:]][array3[:]] = 1000;

  for (ii = 0; ii < NUMBER; ii++)
    if (array[array2[ii]][array3[ii]] != 1000)
      return 1;
  
#if HAVE_IO
  for (ii = 0; ii < NUMBER; ii++) {
    for (jj = 0; jj < NUMBER; jj++) {
      printf("%4d\t", array[ii][jj]);
    }
    printf("\n");
  }
#endif

  array4[array2[:]][array3[0:NUMBER:1]][array2[0:NUMBER:1]][array3[0:NUMBER:1]] =
    (float)array[array2[:]][array3[:]]; 

  for (ii = 0; ii < NUMBER; ii++)
    if (array4[array2[ii]][array3[ii]][array2[ii]][array3[ii]] !=
	(float)array[array2[ii]][array3[ii]])
      return 2;
  
#if HAVE_IO
  for (ii = 0; ii < NUMBER; ii++) {
      for (jj = 0; jj < NUMBER; jj++) {
	  for (kk = 0; kk < NUMBER; kk++) {
	      for (ll = 0; ll < NUMBER; ll++) {
		  printf("%4d\n", array4[ii][jj][kk][ll]);
	      }
	  }
      }
  }
#endif

  return 0;
}
