/* { dg-do link } */
/* { dg-options "-std=c++0x" } */
/* { dg-final { simulate-thread } } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-require-effective-target sync_int_long } */

/* Test that atomic int and atomic char work properly.  */

using namespace std;

#include <atomic>
#include <limits.h>
#include <stdio.h>
#include "simulate-thread.h"

atomic<int> atomi;
atomic<char> atomc;

/* No need for parallel threads to do anything */
void simulate_thread_other_threads()
{
}

/* Verify after every instruction is executed, that the atmoic int and
   char have one of the 2 legitimate values. */
int simulate_thread_step_verify()
{
  if (atomi != 0 && atomi != INT_MAX)
    {
      printf ("FAIL: invalid intermediate result for atomi (%d).\n",
	      (int)atomi);
      return 1;
    }
  if (atomc != 0 && atomc != CHAR_MAX)
    {
      printf ("FAIL: invalid intermediate result for atomc (%d).\n",
	      (int)atomc);
      return 1;
    }
  return 0;
}


/* Verify that both atmoics have the corerct value.  */
int simulate_thread_final_verify()
{
  if (atomi != INT_MAX)
    {
      printf ("FAIL: invalid final result for atomi (%d).\n",
	      (int)atomi);
      return 1;
    }
  if (atomc != CHAR_MAX)
    {
      printf ("FAIL: invalid final result for atomc (%d).\n",
	      (int)atomc);
      return 1;
    }
  return 0;
}

/* Test a store to an atomic int and an atomic char. */
__attribute__((noinline))
void simulate_thread_main()
{
  atomi = INT_MAX;
  atomc = CHAR_MAX;
}

int main ()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
