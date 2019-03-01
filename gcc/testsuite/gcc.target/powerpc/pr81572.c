/* { dg-do compile { target powerpc64*-*-* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-final { scan-assembler-not "xxlor" } } */

#include <altivec.h>

typedef __vector unsigned char nvec_t;

long testz_and(nvec_t a, nvec_t b)
{
  nvec_t c = vec_andc(a, b);
  return vec_all_eq(a, c);
}
