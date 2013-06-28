/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <stdlib.h>
int main(void)
{
  int argc = 1;
  short array[1000], array2[1000], *array3, cond[1000], ii = 0;

  __asm volatile ("" : "+r" (argc));
  for (ii = 0; ii < 1000; ii++) {
    cond[ii] = 1;
    array[ii] = 1000;
    array2[ii] = 2000;
  }
  array2[:] = cond[:] ?  array[:] : array2[:];
 
  for (ii = 0; ii < 1000; ii++) {
   if (array2[ii] != 1000)
     return 1;
  }

  array2[0:500:2] = cond[0:500] ? array[0:500:1] : array2[0:500:2];

  for (ii = 0; ii < 1000; ii++) {
   if (array2[ii] != 1000)
     return 2;
  }

  for (ii = 0; ii < 1000; ii++) {
    cond[ii] = ii % 2; /* This should give 0, 1, 0, 1, 0, 1, 0,... */
    array2[ii] = 5;
    array[ii] = 3; 
  }
  array3 = (short *) malloc (sizeof (short) * 1000);
  array3[0:1000:argc] = cond[:] ? array[0:(argc * 1000)] : array2[argc-1:1000];
  
  for (ii = 0; ii < 1000; ii++) {
    if ((cond[ii] == 0 && array3[ii] != 5) 
        || (cond[ii] == 1 && array3[ii] != 3))
     return 3;
  }
  return 0;
}
