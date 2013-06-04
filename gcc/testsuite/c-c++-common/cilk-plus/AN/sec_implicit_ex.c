/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

void abort (void);
void exit  (int);


int main(void)
{
  int jj, kk, array_3C[10][10][10];
  int ii,array[10], y = 0, y_int = 0, array2[10], array_3[10][10][10];
  double x, yy, array3[10], array4[10];

  array[:] = __sec_implicit_index (0);
  array_3[:][:][:] = __sec_implicit_index (1) + __sec_implicit_index(0) +
    __sec_implicit_index (2);

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	array_3C[ii][jj][kk] = ii+jj+kk;
	
  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	if (array_3[ii][jj][kk] != array_3C[ii][jj][kk])
	  abort ();
	

  exit (0);
  
  return 0;
}
