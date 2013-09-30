/* { dg-do compile } */
/* { dg-options "-mno-xy" } */
/* Would also like to assemble and check that we get the expected
   "Error: bad instruction" assembler messages, but at the moment our
   testharness can't do that.  */

void f (int i)
{
  __asm__("add x0_u0, x0_u0, %0" : :  "r" (i));
}
