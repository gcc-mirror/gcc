/* { dg-do compile { target powerpc-*-eabi* } } */
/* { dg-options "-mcpu=8540 -mabi=spe" } */

__ev64_opaque__ o;
#define v __attribute__((vector_size(8)))
v unsigned int *p;

void m()
{
  o = __builtin_spe_evldd(p, 5);
}
