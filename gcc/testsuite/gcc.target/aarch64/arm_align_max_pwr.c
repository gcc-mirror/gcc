/* { dg-do compile } */
/* { dg-options "-O1" } */

#define align (1ul << __ARM_ALIGN_MAX_PWR)
static int x __attribute__ ((aligned (align)));
static int y __attribute__ ((aligned (align)));

extern void foo (int *x, int *y);
extern int bar (int x, int y);

int
dummy ()
{
  int result;

  foo (&x, &y);
  result = bar (x, y);

  return result;
}

/* { dg-final { scan-assembler-times "zero\t4" 2 } } */
/* { dg-final { scan-assembler "zero\t268435452" } } */
