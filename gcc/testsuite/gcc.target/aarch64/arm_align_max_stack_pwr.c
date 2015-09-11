/* { dg-do compile } */
/* { dg-options "-O1" } */

#define align (1ul << __ARM_ALIGN_MAX_STACK_PWR)
extern void foo (int *x);
extern int bar (int x);

int
dummy ()
{
  int x __attribute__ ((aligned (align)));
  int result;

  foo (&x);
  result = bar (x);

  return result;
}

/* { dg-final { scan-assembler "and\tx\[0-9\]+, x\[0-9\]+, -65536" } } */
