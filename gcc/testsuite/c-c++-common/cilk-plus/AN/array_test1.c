/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>

int main2 (char **argv);

int main(void)
{
  int x = 0; 
  const char *array[] = {"a.out", "5"};	     
  x = main2 ((char **)array);
  return x;
}

int main2 (char **argv)
{
  int array[10], ii = 0, x = 2, z= 0 , y = 0 ;
  for (ii = 0; ii < 10; ii++)
    array[ii] = 10;

  array[0:10:1] = 15;

  for (ii = 0; ii < 10; ii++)
    if (array[ii] != 15)
      return 5;
  array[0:5:2] = 20;

  for (ii = 0; ii < 10; ii += 2)
    if (array[ii] != 20)
      return 4;


  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);

  array[x:5:z] = 50;
  
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != 50)
      return 3;

  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
  
  array[x:y:z] = 505;
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != 505)
      return 4;
    
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
  
  array[x:y:((10-atoi(argv[1]))/atoi(argv[1]))] = 25;

  for (ii = x; ii < 10; ii += z)
    if (array[ii] != 25)
      return 5;
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
 
  array[atoi(argv[1]):(10-atoi(argv[1])):((10-atoi(argv[1]))/atoi(argv[1]))] =
    1400;
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != 1400)
      return 6;
  

  array[atoi("5"):5:1] = 5555;
  
  for (ii = atoi ("5"); ii < 10; ii++)
    if (array[ii] != 5555)
      return 7;
  

  array[atoi("5"):atoi("5"):atoi("1")] = 9999;
  for (ii = atoi ("5"); ii < (atoi ("5") + atoi ("5")); ii += atoi ("1"))
    if (array[ii] != 9999)
      return 8;
  return 0;
}
