/* Alpha -msmall-data didn't transform (mem (symbol_ref)) to
   (mem (lo_sum pic (symbol_ref))) within an asm at the right time.  */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic" } */

void foo()
{
  static int test;
  int dummy;
  asm volatile ("" : "=m"(test), "=r"(dummy) : "m"(test));
}
