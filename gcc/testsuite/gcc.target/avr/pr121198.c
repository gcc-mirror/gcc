/* { dg-do compile } */
/* { dg-options "-Os -mmcu=atmega8" } */

long
test (void)
{
  long x;
  __asm__ ("" : "={r22}" (x));
  return x;
}
