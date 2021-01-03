/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

/* Handling large aligned variables.
   Large aligned variables take a different code-path through expand_stack_vars
   in cfgexpand.c.  This testcase is just to exercise that code-path.

   The alternate code-path produces a second base-pointer through some
   instructions emitted in the prologue.
   
   Test cases are:
   0) Valid access works without complaint.
   1) Invalid access is caught.  */
int __attribute__ ((noinline))
handle_large_alignment (int num)
{
  int other_array[10];
  int big_array[100] __attribute__ ((aligned (32)));
  return big_array[num] + other_array[num];
}

#ifndef ARG
#define ARG 1
#endif

int global;

int __attribute__ ((noinline))
main ()
{
  global += handle_large_alignment (ARG);
  return 0;
}
