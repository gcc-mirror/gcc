/* PR inline-asm/11676 */
/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

void foo(void)
{
  long x = 0;
  asm volatile ("" : "=r"(x) : "r"(x)); /* { dg-bogus "uninitialized" } */
}
