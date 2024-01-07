/* { dg-do compile } */
/* { dg-options "-Os" } */

typedef int HItype __attribute__ ((mode (HI)));
HItype
__mulvhi3 (HItype a, HItype b)
{
  HItype w;

  if (__builtin_mul_overflow (a, b, &w))
    __builtin_trap ();

  return w;
}
