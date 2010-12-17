/* Verify that prologue and epilogue are correct for functions with
   __attribute__ ((interrupt)).  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

/* This test is not valid when -mthumb.  We just cheat.  */
#ifndef __thumb__
extern void bar (int);
extern void foo (void) __attribute__ ((interrupt("IRQ")));

void foo ()
{
  bar (0);
}
#else
void foo ()
{
  asm ("stmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, lr}");
  asm ("ldmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, pc}^");
}
#endif
/* { dg-final { scan-assembler "stmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, lr}" } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, pc}\\^" } } */
