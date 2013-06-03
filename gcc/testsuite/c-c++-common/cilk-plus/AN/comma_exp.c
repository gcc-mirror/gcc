/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>

int main2 (int argc, char **argv);
int main(int argc, char **argv)
{
  int x = 0;
  if (argc == 1)
    {
      const char *array[] = {"a.out", "5"};	     
      x = main2 (2, (char **)array);
    }
  else
    x = main2 (argc, argv);
      
  return x;
}

int main2 (int argc, char **argv)
{
  int array[10], array2[10], ii = 0, x = 2, z= 0 , y = 0 ;

  for (ii = 0; ii < 10; ii++)
    array[ii] = 10;

  for (ii = 0; ii < 10; ii++)
    array2[ii] = 1;

  array[0:10:1] = (array[:], 15);

  for (ii = 0; ii < 10; ii++)
    if (array[ii] != 15)
      return 1;
  array[0:5:2] = (argc+2, 20);

  for (ii = 0; ii < 10; ii += 2)
    if (array[ii] != 20)
      return 2;


  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);

  array[x:5:z] = 50;

  array[:]  = (atoi(argv[1]), (array2[0:10]+5));

  for (ii = 0; ii < 10; ii++)
    if (array[ii] != 6)
      return (3);

  array[:] = (atoi(argv[1]), (array2[0:10]+array2[0:10]));
  for (ii = 0; ii < 10; ii++)
    if (array[ii] != 2)
      return 4;
  
  return 0;
}
