/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 1000

#if HAVE_IO
#include <stdio.h>
#endif

int func (int *x)
{
  int q = *x;
   q++;
   *x = q;
#if HAVE_IO
   printf("%d\n", (q));
#endif
   return *x;
}
int main (void)
{
  char array[NUMBER], array2[NUMBER];
  int ii, d = 2;
#if 1
  for (ii = 0; ii < NUMBER; ii++)  {
   array[ii] = 5;
   array2[ii]= 2;
  }
#endif
  d = func (&d);    /* d = 1 */
  array2[:] = d * array[:] + (char) func (&d); /* 3 * 5 + 4 */
#if HAVE_IO
  for (ii = 0; ii < NUMBER; ii++)
    printf("array2[%d] = %d\n", ii, array2[ii]);
#endif  
  for (ii = 0; ii < NUMBER; ii++)
    if (array2[ii] !=  (3 * 5 + 4))
      return 1;
  
  return 0;
}
  


