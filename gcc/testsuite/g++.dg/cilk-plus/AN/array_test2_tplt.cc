/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <cstdlib>
#include <string.h>
template <class T> int main2(char **argv);
int main(void)
{
  int x = 1, y = 1, z = 1, w = 1; 
  char *array[2]; 
  array[0] = strdup ("a.out"); 
  array[1] = strdup ("5");
  w  = main2<short>(array);
  w += main2<unsigned short> (array);
  x  = main2<char> (array);
  x += main2<unsigned char> (array);
  y  = main2<int> (array);
  y += main2<unsigned int> (array);
  z = main2<long> (array);
  z += main2<unsigned long> (array);
  z += main2<long long> (array);
      
  return (w+x+y+z);
}

template<class T>
int main2(char **argv)
{
  T array[10], array2[10]; 
  int  ii = 0, x = 2, z= 0 , y = 0 ;

  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = 10;
      array2[ii] = 5000000;
    }

  array2[0:10:1] = array[0:10:1];

  for (ii = 0; ii < 10; ii++)
    if (array2[ii] != array[ii])
      return 1; 

  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = 10;
      array2[ii] = 5000000;
    }
  
  array2[0:5:2] = array[0:5:2];

  for (ii = 0; ii < 10; ii += 2)
    if (array[ii] != array2[ii])
      return 2;
  
  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = 10;
      array2[ii] = 5000000;
    }
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
 
  array2[x:5:z] = array[x:5:z];

  for (ii = x; ii < 5; ii += z)
    if (array2[ii] != array[ii])
      return 3;

  for (ii = 0; ii < 10; ii++)
    {
      array[ii] = 500;
      array2[ii] = 1000000;
    }
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);

  array2[x:y:z] = array[x:y:z];
  for (ii = x; ii < 10; ii = ii + z)
    if (array2[ii] != array[ii])
      return 4;

  for (ii = 0; ii < 10; ii++)
    {
      array[ii]  = 500;
      array2[ii] = 1000000;
    }
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);

  array[x:y:((10-atoi(argv[1]))/atoi(argv[1]))] = 
    array2[x:y:((10-atoi(argv[1]))/atoi(argv[1]))];

  for (ii = x; ii < 10; ii += z)
    if (array[ii] != array2[ii])
      return 6;
  
  
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
  
  for (ii = 0; ii < 10; ii++)
    {
      array[ii]  = 500;
      array2[ii] = 1000000;
    }
  
  array[atoi(argv[1]):(10-atoi(argv[1])):((10-atoi(argv[1]))/atoi(argv[1]))] =
    array2[atoi(argv[1]):(10-atoi(argv[1])):((10-atoi(argv[1]))/atoi(argv[1]))];
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != array2[ii])
      return 6;

  for (ii = 0; ii < 10; ii++)
    {
      array[ii]  = 4;
      array2[ii] = 2;
    }

  array[atoi("5"):5:1] = array2[atoi("5"):5:1];

  for (ii = atoi ("5"); ii < 10; ii++)
    if (array[ii] != array2[ii])
      return 7;
  
  for (ii = 0; ii < 10; ii++)
    {
      array[ii]  = 5;
      array2[ii] = 1;
    }
  array[atoi("5"):atoi("5"):atoi("1")] = array2[atoi("5"):atoi("5"):atoi("1")];

  for (ii = 5; ii < 10; ii++)
    if (array2[ii] != array[ii])
      return 8;
 
  return 0;
}
