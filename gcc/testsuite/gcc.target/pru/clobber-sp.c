/* Test inline ASM clobber for SP register */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  */

extern void abort (void);

int
test1 (void)
{
  int res;

  /* { dg-warning "listing the stack pointer register 'sp' in a clobber list is deprecated" "" { target pru-*-* } .+2 } */
  /* { dg-message "note: the value of the stack pointer after an 'asm' statement must be the same as it was before the statement" "" { target pru-*-* } .+1 } */
  asm volatile(
	       "ldi	%[res], 101		\n\t"
	       : [res] "=r" (res)
	       :
	       : "sp");

  return res;
}
