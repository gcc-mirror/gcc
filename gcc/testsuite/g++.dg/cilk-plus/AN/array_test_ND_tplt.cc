/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <cstdlib>
#include<string.h>
template <class T> int main2(char **argv);

int main(void)
{
  int x = 1, y=1, z=1, w = 1;
  char *array[3];
  array[0] = strdup ("a.out");
  array[1] = strdup ("10");
  array[2] = strdup ("15");
  w  = main2<char> (array);
  w += main2<unsigned char> (array);
  x  = main2<int> (array);
  x += main2<unsigned int> (array);
  y  = main2<long> (array);
  y += main2<unsigned long> (array);
  z  = main2<short> (array);
  z += main2<unsigned short> (array);
  return x+y+z;
}

template <class T>
int main2(char **argv)
{  
  T array[10][15];
  T array_2[10][15];
  int ii = 0, jj = 0,x = 0, z= 1 , y = 10 ,argc = 3;
 

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
	    return 1;
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
  z = (20- atoi (argv[1]))/atoi(argv[1]);
  /* (20-10)/10 evaluates to 1 all the time :-). */
  array_2[0:x:z][0:y:z] = array[0:x:z][0:y:z] + array[0:x:z][0:y:z] + y + z;
  
  for (ii = 0; ii < x; ii += z)
    {
      for (jj = 0; jj < y; jj += z)
	{
	  if (array_2[ii][jj] != array[ii][jj] + array[ii][jj] + y + z)
	    return 3;
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
	    return 4;
	}
    }
  return 0;
}
