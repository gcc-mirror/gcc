/* { dg-do link { target { ! int16 } } } */
/* { dg-options "--param allow-store-data-races=0" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include "../../gcc.dg/simulate-thread/simulate-thread.h"

/* Test that we don't store past VAR.K.  */

struct S
{
  volatile int i;
  volatile int j: 32;
  volatile int k: 15;
  volatile unsigned char c[2];
} var;

static int global = 0;

void simulate_thread_other_threads()
{
  global++;
  var.c[0] = global % 256;
  var.c[1] = global % 256;
}

int simulate_thread_step_verify()
{
  if (var.c[0] != global % 256
      || var.c[1] != global % 256)
    {
      printf("FAIL: invalid intermediate result for <var.c[]>.\n");
      return 1;
    }
  return 0;
}

int simulate_thread_final_verify()
{
  if (var.k != 13)
    {
      printf("FAIL: invalid final result\n");
      return 1;
    }
  return 0;
}

__attribute__((noinline))
void simulate_thread_main()
{
  var.k = 13;
}

int main()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
