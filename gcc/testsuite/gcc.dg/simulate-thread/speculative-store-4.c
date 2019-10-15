/* { dg-do link } */
/* { dg-options "-fno-allow-store-data-races" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include <stdlib.h>

#include "simulate-thread.h"

/* PR 54139 */
/* Test that speculative stores do not happen for --param
   allow-store-data-races=0.  */

int g_13=1, insns=1;

__attribute__((noinline))
void simulate_thread_main()
{
  int l_245;

  /* Since g_13 is unilaterally set positive above, there should be
     no store to g_13 below.  */
  for (l_245 = 0; l_245 <= 1; l_245 += 1)
    for (; g_13 <= 0; g_13 = 1)
      ;
}

int main()
{
  simulate_thread_main ();
  simulate_thread_done ();
  return 0;
}

void simulate_thread_other_threads ()
{
  ++g_13;
  ++insns;
}

int simulate_thread_step_verify ()
{
  return 0;
}

int simulate_thread_final_verify ()
{
  if (g_13 != insns)
    {
      printf("FAIL: g_13 was incorrectly cached\n");
      return 1;
    }
  return 0;
}
