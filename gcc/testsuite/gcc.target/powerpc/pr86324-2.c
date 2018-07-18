/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-mlong-double-128 -mabi=ibmlongdouble -Wno-psabi" } */

typedef __complex float cflt_t __attribute__((mode(IC)));

cflt_t
divide (cflt_t *ptr)
{
  return *ptr;
}
