/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>
int main2(char **argv);
int main(void)
{
  int x = 0; 
  const char *array[] = {"a.out", "10", "15"};	     
  x = main2 ((char **)array);
  return x;
}

int main2(char **argv)
{  
  int array[10][15], ii = 0, jj = 0,x = 0, z= 1 , y = 10 ;
  int array_2[10][15];
  int argc = 3;
  __asm volatile ("" : "+r" (argc));

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj< 15; jj++) {
      array[ii][jj] = ii+jj;
      array_2[ii][jj] = 0;
    }
  }
  array_2[0:5:2][0:5:3] = array[0:5:2][0:5:3] + 1 + 5 + array[0][5] + x;

  for (ii = 0; ii < 10; ii += 2)
    {
      for (jj = 0; jj < 15; jj += 3)
	{
	  if (array_2[ii][jj] != array[ii][jj] + 1 + 5 + array[0][5] + x)
	    return 2;
	}
    }


  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj< 15; jj++) {
      array[ii][jj] = ii+jj;
      array_2[ii][jj] = 0;
    }
  }
  x = atoi(argv[1]);
  y = atoi(argv[2]);
  array_2[0:x:1][0:y:1] = array[0:x:1][0:y:1] + x + y + array[0:x:1][0:y:1];

  for (ii = 0; ii < x; ii++)
    {
      for (jj = 0; jj < y; jj++)
	{
	  if (array_2[ii][jj] != array[ii][jj] + x + y + array[ii][jj])
	    return 3;
	}
    }

  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj< 15; jj++) {
      array[ii][jj] = ii+jj;
      array_2[ii][jj] = 0;
    }
  }
  x = atoi(argv[1]);
  y = atoi(argv[2]);
  z = (20- atoi (argv[1]))/atoi(argv[1]);
  /* (20-10)/10 evaluates to 1 all the time :-). */
  array_2[0:x:z][0:y:z] = array[0:x:z][0:y:z] + array[0:x:z][0:y:z] + y + z;
  
  for (ii = 0; ii < x; ii += z)
    {
      for (jj = 0; jj < y; jj += z)
	{
	  if (array_2[ii][jj] != array[ii][jj] + array[ii][jj] + y + z)
	    return 4;
	}
    }


 
  for (ii = 0; ii < 10; ii++) {
    for (jj = 0; jj< 15; jj++) {
      array[ii][jj] = ii+jj;
      array_2[ii][jj] = 0;
    }
  }
  x = argc-3;
  y = 20-atoi(argv[1]);
  z = (20- atoi (argv[1]))/atoi(argv[1]);
  /* (20-10)/10 evaluates to 1 all the time :-). */
  array_2[(argc-3):(20-atoi(argv[1])):(20-atoi(argv[1]))/atoi(argv[1])][(argc-3):(30-atoi(argv[2])): ((30-atoi(argv[2]))/atoi(argv[2]))] = array[(argc-3):20-atoi(argv[1]):(20-atoi(argv[1]))/atoi(argv[1])][(argc-3):(30-atoi(argv[2])): (30-atoi(argv[2]))/atoi(argv[2])] + array[(argc-3):20-atoi(argv[1]):(20-atoi(argv[1]))/atoi(argv[1])][(argc-3):(30-atoi(argv[2])): (30-atoi(argv[2]))/atoi(argv[2])] * array[(argc-3):20-atoi(argv[1]):(20-atoi(argv[1]))/atoi(argv[1])][(argc-3):(30-atoi(argv[2])): (30-atoi(argv[2]))/atoi(argv[2])];
  
  for (ii = 0; ii < 10; ii++)
    {
      for (jj = 0; jj < 15; jj++)
	{
	  if (array_2[ii][jj] != array[ii][jj] + array[ii][jj] * array[ii][jj])
	    return 5;
	}
    }
  return 0;
}
