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
   Check that an upper case "r" in register names is accepted in
   an asm statement clobber list.  */

void
foo (void)
{
  __asm__ ("" : : : "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12",
	   "R13", "R14", "R15");
}
