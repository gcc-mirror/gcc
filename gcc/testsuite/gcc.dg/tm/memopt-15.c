/* { dg-do compile { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-fgnu-tm -O -msse2" } */

/* Test the TM vector logging functions.  */

typedef int __attribute__((vector_size (16))) vectype;
extern int something(void) __attribute__((transaction_safe));
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc,transaction_safe));

vectype vecky;

vectype f()
{
  vectype *p;

  p = malloc (sizeof (*p) * 100);

  __transaction_atomic {
    /* p[5] is thread private, but not transaction local since the
       malloc is outside of the transaction.  We can use the logging
       functions for this.  */
    p[5] = vecky;

    if (something())
      __transaction_cancel;
  }
  return p[5];
}

/* { dg-final { scan-assembler "_ITM_LM128" } } */
