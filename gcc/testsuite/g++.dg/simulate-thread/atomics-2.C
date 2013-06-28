/* { dg-do link } */
/* { dg-options "-std=c++0x" } */
/* { dg-final { simulate-thread } } */
/* { dg-require-effective-target sync_int_long } */

using namespace std;

#include <atomic>
#include <limits.h>
#include <stdio.h>
#include "simulate-thread.h"

atomic_int atomi;

/* Non-atomic.  Use a type wide enough to possibly coerce GCC into
   moving things around.  */
long double j;


/* Test that an atomic store synchronizes with an atomic load.

   In this case, test that the store to <j> happens-before the atomic
   store to <atomi>.  Make sure the compiler does not reorder the
   stores.  */
__attribute__((noinline))
void simulate_thread_main()
{
  j = 13.0;
  atomi.store(1);
}

int main ()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}

void simulate_thread_other_threads()
{
}

/* Verify that side-effects before an atomic store are correctly
   synchronized with the an atomic load to the same location.  */
int simulate_thread_step_verify()
{
  if (atomi.load() == 1 && j != 13.0)
    {
      printf ("FAIL: invalid synchronization for atomic load/store.\n");
      return 1;
    }
  return 0;
}


int simulate_thread_final_verify()
{
  return simulate_thread_step_verify();
}
