/* Alpha -msmall-data didn't transform (mem (symbol_ref)) to
   (mem (lo_sum pic (symbol_ref))) within an asm at the right time.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */

void foo()
{
  static int test;
  asm volatile ("" : "=m"(test) : "m"(test));
}
