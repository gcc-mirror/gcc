/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 10
#include <stdlib.h>

int ***func2 (int ***a1, int ***a2, int size)
{
  a1[0:size:1][0:size:1][0:size] += (a2[0:size][0:size][0:size:1]  + size);
  return a1;
}

int main (void)
{
  int ii, jj, kk;
  int ***array3, ***array2 = NULL, ***array = NULL;

  array = (int ***) malloc (sizeof (int **) * NUMBER);
  array2 = (int ***) malloc (sizeof (int **) * NUMBER);
  for (ii = 0; ii < NUMBER; ii++) {
    array[ii] = (int **) malloc (sizeof (int *) * NUMBER);
    array2[ii] = (int **) malloc (sizeof (int *) * NUMBER);
    for (jj = 0; jj < NUMBER; jj++) { 
      array[ii][jj] = (int *) malloc (sizeof (int) * NUMBER);
      array2[ii][jj] = (int *) malloc (sizeof (int) * NUMBER);
    } 
  }

  for (ii = 0; ii < NUMBER; ii++) {
    for (jj = 0; jj < NUMBER; jj++) {
      for (kk = 0; kk < NUMBER; kk++) {
	array[ii][jj][kk] = 5;
	array2[ii][jj][kk]= 2;
      }
    }
  }
  array3 = func2 ((int ***)array, (int ***)array2, NUMBER);
  
  for (ii = 0; ii < NUMBER; ii++) {
    for (jj = 0; jj < NUMBER; jj++) {
      for (kk = 0; kk < NUMBER; kk++) {
	if (array3[ii][jj][kk] != (7 + NUMBER))
          return 1;
      }
    }
  }
  return 0;
}
