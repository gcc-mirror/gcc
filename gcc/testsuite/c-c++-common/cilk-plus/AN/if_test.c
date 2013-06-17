/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#if HAVE_IO
#include <stdio.h>
#endif
#include <stdlib.h>
int main2 (char **argv);
int main(int argc, char **argv)
{
  int x = 0; 
  const char *array[] = {"a.out", "10", "15"};	     
  x = main2 ((char **) array);
  return x;
}


int main2 (char **argv)
{
  int x = 3, y, z, array[10], array2[10], TwodArray[10][10], jj,kk,ll ;
  int array2_check[10], array2d_check[10][10], array2d[10][10];
  int FourDArray[10][10][10][10], array4[10][10][10][10];
  int array4_check[10][10][10][10];
  int ii = 0, argc = 3; 
    
  __asm volatile ("" : "+r" (argc));

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

  for (ii = 0; ii < 10; ii++) {
    array2[ii] = 10;
    array2_check[ii] = 10;
  }

  /* This if loop will change all the 10's to 5's */
  if (array[atoi(argv[1])-10:atoi(argv[1])/2: atoi(argv[1])/5])
    array2[atoi(argv[1])-10: atoi (argv[1])/2: atoi(argv[1])/5] = 5;
  else
    array2[atoi(argv[1])-10: atoi (argv[1])/2: atoi(argv[1])/5] = 10;

  for (ii = atoi(argv[1])-10; ii < atoi(argv[1]) + (atoi (argv[1])-10);
       ii +=atoi(argv[1])/5)
    if (array[ii])
      array2_check[ii] = 5;
    else
      array2_check[ii] = 10;

  for (ii = 0; ii < 10; ii++)
    if (array2_check[ii] != array2[ii]) {
#if HAVE_IO
      printf("array2[%2d] = %2d array2_check[%2d] = %2d\n", ii, array2[ii],
	     ii, array2_check[ii]);
#endif
      return 5;
    }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      TwodArray[ii][jj] = atoi(argv[1]);


  for (ii = 0; ii < 10; ii++)
    for (ii = 0; ii < 10; ii++) {
      array2d[ii][jj] = 10;
      array2d_check[ii][jj] = 10;
    }

  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (TwodArray[:][:] != 10) 
    array2d[:][:] = 10; 
  else
    array2d[:][:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj++) {
      if (TwodArray[ii][jj] != 10)
	array2d_check[ii][jj] = 10;
      else
	array2d_check[ii][jj] = 5;
    }
  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      if (array2d[ii][jj] != array2d_check[ii][jj])
	return 6;

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  {
	    array4[ii][jj][kk][ll] = 10;
	    array4_check[ii][jj][kk][ll] = 10;
	  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);
  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[:][:][:][:] != 10) 
    array4[:][:][:][:] = 10; 
  else
    array4[:][:][:][:] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj++) {
      for (kk = 0; kk < 10; kk++) {
	for (ll = 0; ll < 10; ll++) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array4_check[ii][jj][kk][ll] = 10;
	  else
	    array4_check[ii][jj][kk][ll] = 5;
	}
      }
    }
  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  if (array4_check[ii][jj][kk][ll] != array4[ii][jj][kk][ll])
	    return 7;

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  {
	    array4[ii][jj][kk][ll] = 10;
	    array4_check[ii][jj][kk][ll] = 10;
	  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);
  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[0:10:1][0:5:2][9:10:-1][0:5:2] != 10) 
    array4[0:10:1][0:5:2][9:10:-1][0:5:2] = 10; 
  else
    array4[0:10:1][0:5:2][9:10:-1][0:5:2] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj += 2) {
      for (kk = 9; kk >= 0; kk--) {
	for (ll = 0; ll < 10; ll += 2) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array4_check[ii][jj][kk][ll] = 10;
	  else
	    array4_check[ii][jj][kk][ll] = 5;
	}
      }
    }
  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  if (array4_check[ii][jj][kk][ll] != array4[ii][jj][kk][ll]) {
#if HAVE_IO
	      printf("array4_check[%d][%d][%d][%d] = %d\n",ii, jj, kk, ll,
		     array4_check[ii][jj][kk][ll]);
	      printf("array4[%d][%d][%d][%d] = %d\n",ii, jj, kk, ll,
		     array4[ii][jj][kk][ll]);
#endif
	    return 8;
	  }
  
  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++) 
	  FourDArray[ii][jj][kk][ll] = atoi(argv[1]);

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  {
	    array4[ii][jj][kk][ll] = 10;
	    array4_check[ii][jj][kk][ll] = 10;
	  }

  
  /* atoi(argv[1]) == 10, so it will convert all 10's to 5's */
  if (FourDArray[0:10:1][0:5:2][9:10:-1][x:y:z] +
      FourDArray[0:10:1][0:5:2][9:10:-1][x:y:z]  != 20) 
    array4[0:10:1][0:5:2][9:10:-1][x:y:z] = 10; 
  else
    array4[0:10][0:5:2][9:10:-1][x:y:z] = 5;

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj < 10; jj += 2) {
      for (kk = 9; kk >= 0; kk--) {
	for (ll = 0; ll < 10; ll += 2) {
	  if (FourDArray[ii][jj][kk][ll] != 10)
	    array4_check[ii][jj][kk][ll] = 10;
	  else
	    array4_check[ii][jj][kk][ll] = 5;
	}
      }
    }
  }

  for (ii = 0; ii < 10; ii++)
    for (jj = 0; jj < 10; jj++)
      for (kk = 0; kk < 10; kk++)
	for (ll = 0; ll < 10; ll++)
	  if (array4_check[ii][jj][kk][ll] != array4[ii][jj][kk][ll]) {
#if HAVE_IO
	      printf("array4_check[%d][%d][%d][%d] = %d\n",ii, jj, kk, ll,
		     array4_check[ii][jj][kk][ll]);
	      printf("array4[%d][%d][%d][%d] = %d\n",ii, jj, kk, ll,
		     array4[ii][jj][kk][ll]);
#endif
	    return 9; 
	  }

  
  return 0;
}
