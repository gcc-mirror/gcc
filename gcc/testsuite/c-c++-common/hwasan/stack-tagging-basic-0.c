/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

/* Basic tests for stack tagging.

   0) Valid accesses work.
   1) Accesses outside of a variable crash.
*/
int __attribute__ ((noinline))
accessing_pointers (int *left, int *right)
{
  int x = right[2];
  left[3] = right[1];
  return right[1] + left[2];
}

int __attribute__ ((noinline))
using_stack (int num)
{
  int big_array[10];
  int other_array[20];
  accessing_pointers(other_array, big_array);
  return big_array[num];
}

#ifndef ARG
#define ARG 0
#endif

int global;

int __attribute__ ((noinline))
main ()
{
  global += using_stack (ARG);
  return 0;
}
