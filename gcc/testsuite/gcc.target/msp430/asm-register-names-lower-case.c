/* { dg-do compile } */
/* { dg-options "-fdump-rtl-expand" } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R4" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R5" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R6" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R7" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R8" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R9" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R10" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R11" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R12" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R13" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R14" expand } } */
/* { dg-final { scan-rtl-dump "(?n)clobber.*R15" expand } } */

/* PR target/70320
   Check that a lower case "r" in register names is accepted in
   an asm statement clobber list.  */

void
foo (void)
{
  __asm__ ("" : : : "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12",
	   "r13", "r14", "r15");
}
