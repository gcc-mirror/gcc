/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include "../isr-test.h"

int volatile v;

/**********************************************************************/

ISR (1, signal)
{
}

MK_RUN_ISR (1, 0)

void test1 (void)
{
  run_isr_1();
}

/**********************************************************************/

ISR (2, signal)
{
  v++;
}

MK_RUN_ISR (2, 0)

void test2 (void)
{
  v = 0;
  run_isr_2();
  if (v != 1)
    __builtin_abort();
}


/**********************************************************************/

ISR (3, signal)
{
  __asm __volatile__ ("$ lds  r27, v"
                      "$ swap r27"
                      "$ sts  v, r27"
                      ::: "memory", "r27");
}

MK_RUN_ISR (3, 0)

void test3 (void)
{
  run_isr_3();
  if (v != 0x10)
    __builtin_abort();
}

/**********************************************************************/

ISR (4, signal)
{
  __asm __volatile__ ("sts v,__zero_reg__" ::: "memory");
}

MK_RUN_ISR (4, 0)

void test4 (void)
{
  run_isr_4();
  if (v != 0)
    __builtin_abort();
}

/**********************************************************************/

ISR (5, signal)
{
  __asm __volatile__ ("clt");
}

MK_RUN_ISR (5, 0)

void test5 (void)
{
  run_isr_5();
}

/**********************************************************************/

int main (void)
{
  test1();
  test2();
  test3();
  test4();
  test5();
  return 0;
}
