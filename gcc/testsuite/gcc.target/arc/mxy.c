/* { dg-do compile } */
/* { dg-options "-mxy" } */
/* { dg-do assemble } */

void f (int i)
{
  __asm__("add x0_u0, x0_u0, %0" : :  "r" (i));
}
