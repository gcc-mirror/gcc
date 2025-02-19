/* { dg-do run } */
/* { dg-options "-std=c99" } */

/* Test ISR psologue / epilogue generation in various situations.  */

#include "../isr-test.h"

#define SI __attribute((signal,noipa)) void

#define S0(x) if (x & 1) __asm ("swap __tmp_reg__")
#define S1(x) if (x & 2) __asm ("swap __zero_reg__")
#define S2(x) if (x & 4) __asm ("swap 26" ::: "26")
#define S3(x) if (x & 8) __asm ("clh")
#define S(x) S0(x); S1(x); S2(x); S3(x)

/* Clobber some GPRs but not SREG */
SI __vector_8 () { S(0); }
SI __vector_1 () { S(1); }
SI __vector_2 () { S(2); }
SI __vector_3 () { S(3); }
SI __vector_4 () { S(4); }
SI __vector_5 () { S(5); }
SI __vector_6 () { S(6); }
SI __vector_7 () { S(7); }

/* Clobber some GPRs and SREG */
SI __vector_10 () { S(8); }
SI __vector_11 () { S(9); }
SI __vector_12 () { S(10); }
SI __vector_13 () { S(11); }
SI __vector_14 () { S(12); }
SI __vector_15 () { S(13); }
SI __vector_16 () { S(14); }
SI __vector_17 () { S(15); }

MK_RUN_ISR (8, 0)
MK_RUN_ISR (1, 0)
MK_RUN_ISR (2, 0)
MK_RUN_ISR (3, 0)
MK_RUN_ISR (4, 0)
MK_RUN_ISR (5, 0)
MK_RUN_ISR (6, 0)
MK_RUN_ISR (7, 0)

MK_RUN_ISR (10, 0)
MK_RUN_ISR (11, 0)
MK_RUN_ISR (12, 0)
MK_RUN_ISR (13, 0)
MK_RUN_ISR (14, 0)
MK_RUN_ISR (15, 0)
MK_RUN_ISR (16, 0)
MK_RUN_ISR (17, 0)

__attribute__((unused, naked, noipa))
static void host_clobbers (void)
{
  __asm __volatile__
  ("nop"
   CR ".global do_clobbers"
   CR ".type   do_clobbers,@function"
   CR "do_clobbers:"

   CR ".irp x,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31"
   /* No value is a multiple of 0x11 so SWAP is not a no-op.  */
   CR "ldi \\x, lo8(17 + 7*\\x)"
   CR ".endr"

#ifndef __AVR_TINY__
   CR "ldi r17, 100"
   CR ".irp x,0,2,3,4,5,6,7,8,9,10,11,12,13,14,15"
   CR "mov \\x, 31-\\x"
   CR ".endr"
 #endif

   CR "ret");
}

#define clobbers \
  __asm volatile ("%~call do_clobbers" :: "s" (host_clobbers) : "memory")

__attribute__((noipa))
void tests (void)
{
  clobbers; run_isr_8 ();
  clobbers; run_isr_1 ();
  clobbers; run_isr_2 ();
  clobbers; run_isr_3 ();
  clobbers; run_isr_4 ();
  clobbers; run_isr_5 ();
  clobbers; run_isr_6 ();
  clobbers; run_isr_7 ();

  clobbers; run_isr_10 ();
  clobbers; run_isr_11 ();
  clobbers; run_isr_12 ();
  clobbers; run_isr_13 ();
  clobbers; run_isr_14 ();
  clobbers; run_isr_15 ();
  clobbers; run_isr_16 ();
  clobbers; run_isr_17 ();
}

int main (void)
{
 tests ();
 return 0;
}
