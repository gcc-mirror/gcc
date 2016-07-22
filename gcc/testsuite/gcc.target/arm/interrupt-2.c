/* Verify that prologue and epilogue are correct for functions with
   __attribute__ ((interrupt)).  */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_nothumb } */
/* { dg-options "-O1 -marm -save-temps" } */

/* This test is not valid when -mthumb.  */
extern void bar (int);
extern void test (void) __attribute__((__interrupt__));

int foo;
void test()
{
  bar (foo);
  foo = 0;
}

/* { dg-final { scan-assembler "push\t{r0, r1, r2, r3, r4, r5, ip, lr}" } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, r5, ip, pc}\\^" } } */
