/* Verify that prologue and epilogue are correct for functions with
   __attribute__ ((interrupt)).  */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_nothumb } */
/* { dg-options "-O0 -marm -save-temps" } */

/* This test is not valid when -mthumb.  */
extern void bar (int);
extern void foo (void) __attribute__ ((interrupt("IRQ")));

void foo ()
{
  bar (0);
}

/* { dg-final { scan-assembler "push\t{r0, r1, r2, r3, r4, fp, ip, lr}" { target { ! arm*-*-uclinuxfdpiceabi } } } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, fp, ip, pc}\\^" { target { ! arm*-*-uclinuxfdpiceabi } } } } */
/* { dg-final { scan-assembler "push\t{r0, r1, r2, r3, r4, r5, r9, fp, ip, lr}" { target arm*-*-uclinuxfdpiceabi } } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, r5, r9, fp, ip, pc}\\^" { target arm*-*-uclinuxfdpiceabi } } } */
