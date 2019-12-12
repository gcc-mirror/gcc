/* { dg-do compile } */

void foo(double d)
{
  __asm__ ("" : "=u" (d));  /* { dg-error "output registers" } */
}
