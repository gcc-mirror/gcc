/* { dg-do compile } */
/* { dg-options "-mno-rtsc" } */
/* Would also like to assemble and check that we get the expected
   "Error: bad instruction" assembler messages, but at the moment our
   testharness can't do that.  */

int f (int i)
{
  __asm__("rtsc %1, %1" : "=r"(i) : "r"(i));
  return i;
}
