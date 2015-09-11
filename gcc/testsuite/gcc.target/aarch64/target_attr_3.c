/* { dg-do compile } */
/* { dg-options "-O2 -mno-fix-cortex-a53-835769 -save-temps" } */

/* Check that the attribute overrides the command line option
   and the fix is applied once.  */

__attribute__ ((target ("fix-cortex-a53-835769")))
unsigned long
test (unsigned long a, double b, unsigned long c,
      unsigned long d, unsigned long *e)
{
  double result;
  volatile unsigned long tmp = *e;
  __asm__ __volatile ("// %0, %1"
			: "=w" (result)
			: "0" (b)
			:    /* No clobbers.  */);
  return c * d + d;
}

unsigned long
test2 (unsigned long a, double b, unsigned long c,
       unsigned long d, unsigned long *e)
{
  double result;
  volatile unsigned long tmp = *e;
  __asm__ __volatile ("// %0, %1"
			: "=w" (result)
			: "0" (b)
			:   /* No clobbers.  */);
  return c * d + d;
}

/* { dg-final { scan-assembler-times "between mem op and" 1 } } */
