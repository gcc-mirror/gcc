/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <cstdlib>
#include <string.h>
#if HAVE_IO
#include <cstdio>
#endif
template <class T> int main2 (char **argv);

int main (void)
{
  int x = 1, y = 1, z = 1;
  char *array[2];
  array[0] = strdup ("a.out");
  array[1] = strdup ("5");
  x  = main2<unsigned char> (array);
  x += main2<char> (array);
  y  = main2<short> (array);
  y += main2<unsigned short> (array);
  y += main2<int> (array);
  y += main2<unsigned int> (array);
  z  = main2<long> (array);
  z += main2<long long> (array);
  y += main2<float> (array);
  z += main2<double> (array);
      
  return x+y+z;
}
template <class T>
int main2 (char **argv)
{
  T array[10];
  int ii = 0, x = 2, z= 0 , y = 0;

  for (ii = 0; ii < 10; ii++)
    array[ii] = 10;

  array[0:10:1] = (T)15;

  for (ii = 0; ii < 10; ii++)
    if (array[ii] != (T)15)
      return 1;
  

  array[0:5:2] = (T)20;

  for (ii = 0; ii < 10; ii += 2)
    if (array[ii] != (T)20)
      return 2;


  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);

  array[x:5:z] = (T)50;
  
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != (T)50)
      return 3;

  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]); /* (10 - 5) / 5 = 1 */
  y = 10-atoi(argv[1]);
  
  array[x:y:z] = (T)52;
#if HAVE_IO
  for (ii = atoi ("5"); ii < (atoi ("5") + atoi ("5")); ii += atoi ("1"))
    std::printf("%d\t", (int)array[ii]);
  std::printf("\n");
#endif
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != (T)52)
      return 4;
    

  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
  
  array[x:y:((10-atoi(argv[1]))/atoi(argv[1]))] = (T)25;

  for (ii = x; ii < 10; ii += z)
    if (array[ii] != (T)25)
      return 5;
  
  x = atoi(argv[1]);
  z = (10-atoi(argv[1]))/atoi(argv[1]);
  y = 10-atoi(argv[1]);
 
  array[atoi(argv[1]):(10-atoi(argv[1])):((10-atoi(argv[1]))/atoi(argv[1]))] =
    (T)14;
  for (ii = x; ii < 10; ii += z)
    if (array[ii] != (T)14)
      return 6;
  

  array[atoi("5"):5:1] = (T)65;
  
  for (ii = atoi ("5"); ii < 10; ii++)
    if (array[ii] != (T)65)
      return 7;
  

  array[atoi("5"):atoi("5"):atoi("1")] = 99;

#if HAVE_IO
  for (ii = atoi ("5"); ii < (atoi ("5") + atoi ("5")); ii += atoi ("1"))
    std::printf("%d\t", (int)array[ii]);
  std::printf("\n");
#endif

  for (ii = atoi ("5"); ii < (atoi ("5") + atoi ("5")); ii += atoi ("1"))
    if (array[ii] != (T)99)
      return 8;

  return 0;
}
