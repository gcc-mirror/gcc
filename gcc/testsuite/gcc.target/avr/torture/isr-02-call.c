/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include "../isr-test.h"

int volatile v;

__attribute__((noipa))
void inc_v (void)
{
  v++;
}

/**********************************************************************/

ISR (1, signal)
{
  inc_v();
}

MK_RUN_ISR (1, 0)

void test1 (void)
{
  run_isr_1();
  if (v != 1)
    __builtin_abort();
}

/**********************************************************************/

ISR (2, signal)
{
  if (v == 1)
    inc_v();
  else
    v += 2;
}

MK_RUN_ISR (2, 0)

void test2 (void)
{
  run_isr_2();
  if (v != 2)
    __builtin_abort();
  run_isr_2();
  if (v != 4)
    __builtin_abort();
}


/**********************************************************************/

int main (void)
{
  test1();
  test2();
  return 0;
}
