/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>
int main2 (int argc, char **argv);
int main(int argc, char **argv)
{
  int x = 0;
  if (argc == 1)
    {
      const char *array[] = {"a.out", "10", "15"};	     
      x = main2 (3, (char **) array);
    }
  else if (argc == 3)
    x = main2 (argc, argv);
  else
    return 1;
 
  return x;
}


int main2 (int argc, char **argv)
{
  int x = 3, y, z, array[10], array2[10], TwodArray[10][10], jj,kk,ll ;
  int array2_check[10];
  int FourDArray[10][10][10][10];
  int ii = 0; 

  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = argc%3;
      array2[ii]= 10;
      array2_check[ii] = 10;
    }

  if (!array[:])
    array2[:] = 5;
  else
    array2[:] = 10;


  for (ii = 0; ii < 10; ii++)
    {
      if (!array[ii])
	array2_check[ii] = 5;
      else
	array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    if (array2_check[ii] != array2[ii])
      return 2;
  

  
  if (!(array[0:10:1] + array[0:10:1]))
    array2[:] = 5;
  else
    array2[:] = 10;

  for (ii = 0; ii < 10; ii++)
    {
      if (!(array[ii]+ array[ii]))
	array2_check[ii] = 5;
      else
	array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    if (array2_check[ii] != array2[ii])
      return 3;
  

      
       

  x = atoi (argv[1])-10;
  y = atoi (argv[1])/2;
  z = (atoi (argv[1]))/5;

  for (ii = 0; ii < 10; ii++)
    {
      if (ii % 2)
	array[ii] = 0;
      else
	array[ii] = 1;
    }

  /*printf("x = %2d y = %2d z = %2d\n", x, y, z); */

  for (ii = 0; ii < 10; ii++)
    array[ii] = 10;

  /* This if loop will change all the 10's to 5's */
  if (array[x:y:z] != 9)
    array2[:] = 5;
  else
    array2[:] = 10;

  for (ii = x; ii < (x+y); ii += z)
    {
      if (array[ii] != 9)
	array2_check[ii] = 5;
      else
	array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    if (array2_check[ii] != array2[ii])
      return 4;

  for (ii = 0; ii < 10; ii++)
    array2[ii] = 10;

  /* This if loop will change all the 10's to 5's */
  if (array[atoi(argv[1])-10:atoi(argv[1]): atoi(argv[1])/5])
    array2[:] = 5;
  else
    array2[:] = 10;

  for (ii = atoi(argv[1])-10; ii < atoi(argv[1]) + (atoi (argv[1])-10);
       ii +=atoi(argv[1])/5)
    if (array[ii])
      array2_check[ii] = 5;
    else
      array2_check[ii] = 10;

  for (ii = 0; ii < 10; ii++)
    if (array2_check[ii] != array2[ii])
      return 5;
 

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      TwodArray[ii][jj] = atoi(argv[1]);


  for (ii = 0; ii < 10; ii++)
    {
      array2[ii] = 10;
      array2_check[ii] = 10;
    }

  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (TwodArray[:][:] != 10) 
    array2[:] = 10; 
  else
    array2[:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj++) {
      if (TwodArray[ii][jj] != 10)
	array2_check[ii] = 10;
    }
  }

  for (ii = 0; ii < 10; ii++)
    {
      array2[ii] = 10;
      array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);
  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[:][:][:][:] != 10) 
    array2[:] = 10; 
  else
    array2[:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj++) {
      for (kk = 0; kk < 10; kk++) {
	for (ll = 0; ll < 10; ll++) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array2_check[ii] = 10;
	}
      }
    }
  }
  
  for (ii = 0; ii < 10; ii++)
    {
      array2[ii] = 10;
      array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);
  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[0:10:1][0:5:2][9:10:-1][x:y:z] != 10) 
    array2[:] = 10; 
  else
    array2[:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj += 2) {
      for (kk = 9; kk >= 0; kk--) {
	for (ll = x; ll < 10; ll = ll += z) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array2_check[ii] = 10;
	  else
	    array2_check[ii] = 5;
	}
      }
    }
  }

  for (ii = 0; ii < 10; ii++)
    {
      array2[ii] = 10;
      array2_check[ii] = 10;
    }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);
  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[0:10:1][0:5:2][9:10:-1][x:y:z] +
      FourDArray[0:10:1][0:5:2][9:-10:1][x:y:z]  != 20) 
    array2[:] = 10; 
  else
    array2[:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj += 2) {
      for (kk = 9; kk >= 0; kk--) {
	for (ll = x; ll < 10; ll = ll += z) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array2_check[ii] = 10;
	  else
	    array2_check[ii] = 5;
	}
      }
    }
  }
  return 0;
}
