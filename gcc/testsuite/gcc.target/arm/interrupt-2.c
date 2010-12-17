/* Verify that prologue and epilogue are correct for functions with
   __attribute__ ((interrupt)).  */
/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This test is not valid when -mthum.  We just cheat.  */
#ifndef __thumb__
extern void bar (int);
extern void test (void) __attribute__((__interrupt__));

int foo;
void test()
{
  funcptrs(foo);
  foo = 0;
}
#else
void test ()
{
  asm ("stmfd\tsp!, {r0, r1, r2, r3, r4, r5, ip, lr}");
  asm ("ldmfd\tsp!, {r0, r1, r2, r3, r4, r5, ip, pc}^");
}
#endif

/* { dg-final { scan-assembler "stmfd\tsp!, {r0, r1, r2, r3, r4, r5, ip, lr}" } } */
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, r4, r5, ip, pc}\\^" } } */
