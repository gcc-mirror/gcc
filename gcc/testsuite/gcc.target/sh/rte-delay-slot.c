/* { dg-do compile { target "sh-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m1 -m2*" }  */
/* { dg-final { scan-assembler-not "\trte\t\n\tmov.l\t@r15\\+" } } */

/* This test checks if the compiler generates a pop instruction
   in the delay slot after rte.  For the sh and sh2, the rte
   instruction reads the return pc from the stack and any pop
   in the delay slot crashes the hardware.

   Incorrect code generated
        mov.l   @r15+,r1
        rte
        mov.l   @r15+,r14

   The right code should be

        mov.l   @r15+,r1
        mov.l   @r15+,r14
        rte
        nop
*/
void INT_MTU2_1_TGIA1 (void)
  __attribute__ ((interrupt_handler));
void
INT_MTU2_1_TGIA1 (void)
{
  volatile int i = 0;
  volatile int x, y;

  for (i = 0; i < 10; i++)
    y = y + x;
}
