/* { dg-do compile { target powerpc-*-eabi* powerpc*-*-linux*spe* } } */
/* { dg-options "-mcpu=8540 -mspe -mabi=spe -mfloat-gprs=single" } */

__ev64_opaque__ o;
#define v __attribute__((vector_size(8)))
v unsigned int *p;

void m()
{
  o = __builtin_spe_evldd(p, 5);
}
