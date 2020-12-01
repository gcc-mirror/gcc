/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

#include <setjmp.h>
#include <stdio.h>

/*
   Testing longjmp/setjmp should test.

   0) Nothing special happens with the jmp_buf.
   1) Accesses to scopes jmp'd over are caught.
 */
int __attribute__ ((noinline))
uses_longjmp (int **other_array, int num, jmp_buf env)
{
  int internal_array[100] = {0};
  *other_array = &internal_array[0];
  if (num % 2)
    longjmp (env, num);
  else
    return num % 8;
}

int __attribute__ ((noinline))
uses_setjmp (int num)
{ 
  int big_array[100];
  int *other_array = NULL;
  sigjmp_buf cur_env;
  int temp = 0;
  if ((temp = sigsetjmp (cur_env, 1)) != 0)
    { 
      if (other_array != NULL)
        printf ("Value pointed to in other_array[0]: %d\n",
                other_array[0]);
  
      printf ("Longjmp returned %d.\n", temp);
      return 10;
    }
  else
    {
      return uses_longjmp (&other_array, num, cur_env);
    } 
} 

#ifndef ARG
#define ARG 0
#endif
int __attribute__ ((noinline))
main ()
{
  uses_setjmp (ARG);
  return 0;
}
