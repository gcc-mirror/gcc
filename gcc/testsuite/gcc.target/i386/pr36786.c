/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */

typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef int TItype __attribute__ ((mode (TI)));

__floattisf (TItype u)
{
  DItype hi = u >> (8 * 8);
  UDItype count, shift;
  hi = u >> shift;
}
