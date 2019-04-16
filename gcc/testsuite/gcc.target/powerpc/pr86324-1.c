/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-mlong-double-128 -mdejagnu-cpu=power7 -mabi=ieeelongdouble -Wno-psabi" } */

typedef __complex float cflt_t __attribute__((mode(KC)));

cflt_t
divide (cflt_t *ptr)
{
  return *ptr;
}
