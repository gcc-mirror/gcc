/* Verify that __attribute__((naked)) produces functions without implicit
   `exit' instructions in the epilogue.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int __attribute__((naked)) foo()
{
  __asm__ volatile ("exit");
}
/* { dg-final { scan-assembler-times "\texit" 1 } } */
