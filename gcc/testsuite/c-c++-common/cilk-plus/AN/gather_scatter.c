/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#if HAVE_IO
#include <stdio.h>
#endif

int main(void)
{
  int array[10][10], array2[10], array3[10], x = 0, y;
  int x_correct, y_correct, ii, jj = 0;
  float array4[10][10][10][10];
  for (ii = 0; ii < 10; ii++)
    {
      for (jj = 0; jj < 10; jj++)
	{
	  array[ii][jj] = 1+ii;
	  array2[ii]= 2;
	  array3[ii]= 3;
	}
    }
  
  array[array2[:]][array3[:]] = 1000;

  for (ii = 0; ii < 10; ii++)
    if (array[array2[ii]][array3[ii]] != 1000)
      return 1;
  
#if HAVE_IO
  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj++) {
      printf("%4d\t", array[ii][jj]);
    }
    printf("\n");
  }
#endif

  array4[array2[:]][array3[0:10:1]][array2[0:10:1]][array3[0:10:1]] =
    (float)array[array2[:]][array3[:]]; 

  for (ii = 0; ii < 10; ii++)
    if (array4[array2[ii]][array3[ii]][array2[ii]][array3[ii]] !=
	(float)array[array2[ii]][array3[ii]])
      return 2;
  
#if HAVE_IO
  for (ii = 0; ii < 10; ii++) {
      for (jj = 0; jj < 10; jj++) {
	  for (kk = 0; kk < 10; kk++) {
	      for (ll = 0; ll < 10; ll++) {
		  printf("%4d\n", array4[ii][jj][kk][ll]);
	      }
	  }
      }
  }
#endif

  return 0;
}
