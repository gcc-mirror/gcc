/* { dg-do link } */
/* { dg-options "-fno-allow-store-data-races" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include <stdlib.h>
#include "../../gcc.dg/simulate-thread/simulate-thread.h"

struct bits
{
  char a;
  int b:7;
  int c:9;
  unsigned char d;
} *p;

static int global = 0;

void simulate_thread_other_threads()
{
  global++;
  p->d = global % 256;
}

int simulate_thread_step_verify()
{
  if (p->d != global % 256)
    {
      printf("FAIL: invalid intermediate result\n");
      return 1;
    }
  return 0;
}

int simulate_thread_final_verify()
{
  if (p->c != 55)
    {
      printf("FAIL: invalid final result\n");
      return 1;
    }
  return 0;
}

/* Store into <c> should not clobber <d>.  */
/* We should not use a 32-bit move to store into p->, but a smaller move.  */
__attribute__((noinline))
void simulate_thread_main()
{
  p -> c = 55;
}
  

int main()
{
  p = (struct bits *) calloc (1, sizeof (struct bits));
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
