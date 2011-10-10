/* { dg-do link } */
/* { dg-options "--param allow-store-data-races=0" } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include "../../gcc.dg/simulate-thread/simulate-thread.h"

/* Store into <c> should not clobber <d>.  */

struct bits
{
  char a;
  int b:7;
  int c:9;
  unsigned char d;
} var;

static int global = 0;

void simulate_thread_other_threads()
{
  global++;
  var.d = global;
}

int simulate_thread_step_verify()
{
  if (var.d != global)
    {
      printf("FAIL: invalid intermediate result\n");
      return 1;
    }
  return 0;
}

int simulate_thread_final_verify()
{
  if (var.c != 5)
    {
      printf("FAIL: invalid final result\n");
      return 1;
    }
  return 0;
}

__attribute__((noinline))
void update_c(struct bits *p, int val) 
{
    p -> c = val;
}

__attribute__((noinline))
void simulate_thread_main()
{
  update_c(&var, 5);
}

int main()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
