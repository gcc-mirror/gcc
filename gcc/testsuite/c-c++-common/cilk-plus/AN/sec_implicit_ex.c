/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 20

int main(void)
{
  int jj, kk, array_3C[NUMBER][NUMBER][NUMBER];
  int ii,array[NUMBER], y = 0, y_int = 0, array2[NUMBER], 
      array_3[NUMBER][NUMBER][NUMBER];
  double x, yy, array3[NUMBER], array4[NUMBER];

  array[:] = __sec_implicit_index (0);
  array_3[:][:][:] = __sec_implicit_index (1) + __sec_implicit_index(0) +
    __sec_implicit_index (2);

  for (ii = 0; ii < NUMBER; ii++)
    for (jj = 0; jj < NUMBER; jj++)
      for (kk = 0; kk < NUMBER; kk++)
	array_3C[ii][jj][kk] = ii+jj+kk;
	
  for (ii = 0; ii < NUMBER; ii++)
    for (jj = 0; jj < NUMBER; jj++)
      for (kk = 0; kk < NUMBER; kk++)
	if (array_3[ii][jj][kk] != array_3C[ii][jj][kk])
	  return 1;
	
  return 0;
}
