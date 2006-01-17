/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.5.15 Conditional operator.
   Test with decimal float operands.  */

extern void abort (void);
static int failcnt = 0;
                                                                                
/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

volatile _Decimal32 d32a, d32b, d32c;
volatile _Decimal64 d64a, d64b, d64c;
volatile _Decimal128 d128a, d128b, d128c;
volatile int i, yes, no;

void
init ()
{
  d32b = 123.456e94df;
  d64b = 12.3456789012345e383dd;
  d128b = 12345.6789012345678901e4000dl;

  d32c = 1.3df;
  d64c = 1.2dd;
  d128c = 1.1dl;

  i = 2;
  yes = 1;
  no = 0;
}

int
main ()
{
  /* Operands and the result are all the same decimal float type.  */
  d32a = yes ? d32b : d32c;
  if (d32a != d32b)
    FAILURE
  d64a = no ? d64b : d64c;
  if (d64a != d64b)
    FAILURE
  d128a = yes ? d128b : d128c;
  if (d128a != d128b)
    FAILURE

  /* Operand types are different.  */
  d128a = yes ? d32b : d64b;
  if (d128a != d32b)
    FAILURE
  d128a = yes ? d128b : d64b;
  if (d128a != d128b)
    FAILURE
  d128a = no ? d32b : d128b;
  if (d128a != d128b)
    FAILURE

  return 0;
}
