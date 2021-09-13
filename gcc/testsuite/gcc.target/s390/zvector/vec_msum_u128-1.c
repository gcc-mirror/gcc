/* { dg-do run } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps" } */

#include <vecintrin.h>

typedef vector unsigned char uv16qi;
typedef vector unsigned long long uv2di;

uv2di a = (uv2di){ 12,  42 };
uv2di b = (uv2di){ 54, 120 };
uv2di c = (uv2di){  0, 200 };

int
main ()
{
  uv2di result;

  result = (uv2di)vec_msum_u128 (a, b, (uv16qi)c, 0);

  if (result[1] != a[0] * b[0] + a[1] * b[1] + c[1])
    __builtin_abort();

  result = (uv2di)vec_msum_u128 (a, b, (uv16qi)c, 4);

  if (result[1] != a[0] * b[0] + a[1] * b[1] * 2 + c[1])
    __builtin_abort();

  result = (uv2di)vec_msum_u128 (a, b, (uv16qi)c, 8);

  if (result[1] != a[0] * b[0] * 2 + a[1] * b[1] + c[1])
    __builtin_abort();

  result = (uv2di)vec_msum_u128 (a, b, (uv16qi)c, 12);

  if (result[1] != a[0] * b[0] * 2 + a[1] * b[1] * 2 + c[1])
    __builtin_abort();

  return 0;
}

/* { dg-final { scan-assembler-times "vmslg\t.*0" 1 } } */
/* { dg-final { scan-assembler-times "vmslg\t.*4" 1 } } */
/* { dg-final { scan-assembler-times "vmslg\t.*8" 1 } } */
/* { dg-final { scan-assembler-times "vmslg\t.*12" 1 } } */
