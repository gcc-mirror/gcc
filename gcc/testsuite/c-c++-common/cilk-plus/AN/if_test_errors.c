/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>
int main (void)
{
  int x = 3, y, z, array[10], array2[10], TwodArray[10][10], jj,kk,ll ;
  int array2_check[10], array2d_check[10][10], array2d[10][10];
  int FourDArray[10][10][10][10], array4[10][10][10][10];
  int array4_check[10][10][10][10];
  int ii = 0;

  x = 5;
  y = 10;
  z = 2;

  if (!array[:]) /* This is OK! */
    array2[:] = 5;
  else
    array2[:] = 10;
  if (!(array[0:10:1] + array[0:10:1])) /* { dg-error "condition and the then-block" "" { target c } } */
    array2d[:][:] = 5; /* { dg-error "rank mismatch with controlling expression of parent" "" { target c++ } } */
  else
    array2[:] = 10;

  if (!(array[0:10:1] + array[0:10:1])) /* { dg-error "condition and the else-block" "" { target c } } */
    array2[:] = 5;
  else
    array2d[:][:] = 10; /* { dg-error "rank mismatch with controlling expression of parent" "" { target c++ } } */


  if (TwodArray[:][:] != 10) /* { dg-error "condition and the then-block" "" { target c } } */
    array2[:] = 10;  /* { dg-error "rank mismatch with controlling expression of parent" "" { target c++ } } */
  else
    array2[:] = 5;

  if (FourDArray[43][:][:][:] != 10) /* This is OK!  */ 
    array4[45][:][:][:] = 10; 
  else
    array4[32][:][:][:] = 5;

  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[42][0:10:1][9:10:-1][0:5:2] != 10) /* { dg-error "condition and the then-block" "" { target c } } */
    array4[0:10:1][0:5:2][9:10:-1][0:5:2] = 10;  /* { dg-error "rank mismatch with controlling expression of parent" "" { target c++ } } */
  else
    array4[0:10:1][0:5:2][9:10:-1][0:5:2] = 5;

  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[0:10:1][0:5:2][9:10:-1][x:y:z] +
      FourDArray[0:10:1][0:5:2][9:-10:1][x:y:z]  != 20) 
    array4[0:10:1][0:5:2][9:10:-1][x:y:z] = 10; 
  else
    array4[0:10][0:5:2][9:10:-1][x:y:z] = 5;

  return 0;
}
