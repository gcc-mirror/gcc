/* { dg-do compile } */
/* { dg-options "-mno-lock" } */
/* Would also like to assemble and check that we get the expected
   "Error: bad instruction" assembler messages, but at the moment our
   testharness can't do that.  */

int f (void *p)
{
  int i;

  __asm__("llock %0, [%1]\n\t"
	  "scond %0, [%1]" : "=&r"(i) : "r"(p));
  return i;
}
