/* { dg-do link } */
/* { dg-options "--param allow-packed-store-data-races=0" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include "simulate-thread.h"

/* This test verifies writes to globals do not write to adjacent
   globals.  This mostly happens on strict-align targets that are not
   byte addressable (old Alphas, etc).  */

char a = 0;
char b = 77;

void simulate_thread_other_threads() 
{
}

int simulate_thread_step_verify()
{
  if (b != 77)
    {
      printf("FAIL: Unexpected value.  <b> is %d, should be 77\n", b);
      return 1;
    }
  return 0;
}

/* Verify that every variable has the correct value.  */
int simulate_thread_final_verify()
{
  int ret = simulate_thread_step_verify ();
  if (a != 66)
    {
      printf("FAIL: Unexpected value.  <a> is %d, should be 66\n", a);
      return 1;
    }
  return ret;
}

__attribute__((noinline))
void simulate_thread_main()
{
  a = 66;
}

int main ()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
