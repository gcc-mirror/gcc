/* { dg-do link } */
/* { dg-options "--param allow-store-data-races=0" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include "../../gcc.dg/simulate-thread/simulate-thread.h"

/* Test that we don't store past VAR.A.  */

struct S
{
  volatile unsigned int a : 4;
  unsigned char b;
  unsigned int c : 6;
} var = { 1, 2, 3 };

static int global = 0;

/* Called before each instruction, simulating another thread
   executing.  */
void simulate_thread_other_threads()
{
  global++;
  var.b = global;
  /* Don't go past the 6 bits var.c can hold.  */
  var.c = global % 64;
}

/* Called after each instruction.  Returns 1 if any inconsistency is
   found, 0 otherwise.  */
int simulate_thread_step_verify()
{
  int ret = 0;
  if (var.b != global)
    {
      printf("FAIL: invalid intermediate value for <b>.\n");
      ret = 1;
    }
  if (var.c != global % 64)
    {
      printf("FAIL: invalid intermediate value for <c>.\n");
      ret = 1;
    }
  return ret;
}

/* Called at the end of the program (simulate_thread_fini == 1).  Verifies
   the state of the program and returns 1 if any inconsistency is
   found, 0 otherwise.  */
int simulate_thread_final_verify()
{
  if (var.a != 12)
    {
      printf("FAIL: invalid final result for <a>.\n");
      return 1;
    }
  return 0;
}

__attribute__((noinline))
void simulate_thread_main()
{
  var.a = 12;
}

int main()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
