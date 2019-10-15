/* { dg-do link } */
/* { dg-options "-fno-allow-store-data-races -O2" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include <stdlib.h>

#include "simulate-thread.h"

/* Test distilled from PR52558.  */

int g_1 = 1;
int g_2 = 0, insns = 0;
int f;

/* Test that g_2 is not written to unless !g_1.  */

__attribute__((noinline))
int funky()
{
  int l;
  for (l = 0; l != 4; l++)
    {
      if (g_1)
	{
	  /* g_1 is globally true so we should always execute here,
	     thus never writing to g_2 under any circumstance in this
	     code path.  */
	  return l;
	}
      for (g_2 = 0; g_2 >= 26; ++g_2)
	;
    }
  return 999;
}

int simulate_thread_final_verify ()
{
  /* If g_2 != insns, someone must have cached `g_2' and stored a
     racy value into it.  */
  if (g_2 != insns)
    {
      printf("FAIL: g_2 was incorrectly cached\n");
      return 1;
    }
  return 0;
}

void simulate_thread_other_threads ()
{
  ++insns;
  ++g_2;
}

int simulate_thread_step_verify ()
{
  return 0;
}

__attribute__((noinline))
void simulate_thread_main()
{
  f = funky();
}

int main()
{
  simulate_thread_main ();
  simulate_thread_done ();
  return 0;
}
