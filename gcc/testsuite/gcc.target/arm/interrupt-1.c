/* Verify that prologue and epilogue are correct for functions with
   __attribute__ ((interrupt)).  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_nothumb } */
/* { dg-options "-O0 -marm" } */

/* This test is not valid when -mthumb.  */
extern void bar (int);
extern void foo (void) __attribute__ ((interrupt("IRQ")));

void foo ()
{
  bar (0);
}

/* { dg-final { scan-assembler "stmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, lr}" } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, pc}\\^" } } */
