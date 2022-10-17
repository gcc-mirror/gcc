/* { dg-do compile } */
/* { dg-options "-O1 -fharden-conditional-branches" } */

/* This is a slightly simplified version of
   gcc.target/s390/vector/long-double-asm-earlyclobber.c.  On x86, the f
   constraints in asm statements imposes some requirements that the testcase
   doesn't meet.  What's unusual is that -fharden-conditional-branches extends
   the effects of the malformed asm onto a different basic blocks, which
   reg-stack did not expect.  */

#include <assert.h>
#include <stdint.h>

void
f (void)
{
  long double res, x = 0;
  asm("" : "=f"(res) /* { dg-error "must specify a single register" } */
      : "0"(x));
  assert (res == x);
}  

void
g (void)
{
  long double res, x = 0;
  asm("" : "=g"(res) /* this is ok.  */
      : "0"(x));
  assert (res == x);
}  
