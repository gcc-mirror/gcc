/* { dg-do run } */
/* { dg-options "-std=gnu99 -fno-lto -fno-toplevel-reorder" } */

// No LTO for now due to PR lto/68384.

#ifdef __AVR_TINY__
unsigned char reg2;
#else
register unsigned char reg2 __asm("r2");
#endif

#include "../isr-test.h"

#define SET_REG(reg,val)                        \
  do {                                          \
    reg = (val);                                \
    __asm __volatile__("" : "+r" (reg));        \
  } while (0)                                   \

#define GET_REG(reg)                            \
  ({                                            \
    __asm __volatile__("" : "+r" (reg));        \
    reg;                                        \
  })

/**********************************************************************/

ISR (1, signal)
{
  reg2++;
}

MK_RUN_ISR (1, 1ul << 2)

void test1 (void)
{
  SET_REG (reg2, 0);
  run_isr_1();
  if (GET_REG (reg2) != 1)
    __builtin_abort();
}

/**********************************************************************/

__attribute__((noinline,noclone))
void inc_r2 (void)
{
  reg2++;
}

ISR (2, signal)
{
  inc_r2 ();
}

MK_RUN_ISR (2, 1ul << 2)

void test2 (void)
{
  run_isr_2();
  if (GET_REG (reg2) != 2)
    __builtin_abort();
}


/**********************************************************************/

ISR (3, signal)
{
#ifndef __AVR_TINY__
  register char r4 __asm ("r4");
  __asm __volatile ("inc %0" : "+r" (r4));
  __asm __volatile ("inc r5" ::: "r5");
#endif
}

MK_RUN_ISR (3, 0)

void test3 (void)
{
  run_isr_3();
}


/**********************************************************************/

#define CLOBB(reg)                                 \
  do {                                             \
    __asm __volatile__ ("inc " #reg ::: #reg);     \
  } while (0)

ISR (4, signal)
{
  char volatile v;
  v = 1;

#ifndef __AVR_TINY__
  CLOBB (r3);
  CLOBB (r4);
  CLOBB (r5);
  CLOBB (r6);
  CLOBB (r7);
  CLOBB (r8);
  CLOBB (r9);
  CLOBB (r10);
  CLOBB (r11);
  CLOBB (r12);
  CLOBB (r13);
  CLOBB (r14);
  CLOBB (r15);
  CLOBB (r16);
  CLOBB (r17);
#endif

  CLOBB (r18);
  CLOBB (r19);
  CLOBB (r20);
  CLOBB (r21);
  CLOBB (r22);
  CLOBB (r23);
  CLOBB (r24);
  CLOBB (r25);
  CLOBB (r26);
  CLOBB (r27);
  CLOBB (r30);
  CLOBB (r31);
}

MK_RUN_ISR (4, 0)

void test4 (void)
{
  run_isr_4();
}


/**********************************************************************/

int main (void)
{
  test1();
  test2();
  test3();
  test4();
  return 0;
}
