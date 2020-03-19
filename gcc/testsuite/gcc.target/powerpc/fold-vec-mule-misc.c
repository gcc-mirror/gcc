/* PR target/79941 */

/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O2 -save-temps" } */

#include <altivec.h>

__attribute__((noinline)) void 
test_eub_char ()
{
  volatile vector unsigned char v0 = {1, 0, 0, 0, 0, 0, 0, 0};
  volatile vector unsigned char v1 = {0xff, 0, 0, 0, 0, 0, 0, 0};
  vector unsigned short res = vec_vmuleub (v0, v1);
  if (res[0] != (unsigned short)v0[0] * (unsigned short)v1[0])
    __builtin_abort ();
}

__attribute__((noinline)) void 
test_oub_char ()
{
  volatile vector unsigned char v0 = {0, 1, 0, 0, 0, 0, 0, 0};
  volatile vector unsigned char v1 = {0, 0xff, 0, 0, 0, 0, 0, 0};
  vector unsigned short res = vec_vmuloub (v0, v1);
  if (res[0] != (unsigned short)v0[1] * (unsigned short)v1[1])
    __builtin_abort ();
}

__attribute__((noinline)) void 
test_euh_short ()
{
  volatile vector unsigned short v0 = {1, 0, 0, 0};
  volatile vector unsigned short v1 = {0xff, 0, 0, 0};
  vector unsigned int res = vec_vmuleuh (v0, v1);
  if (res[0] != (unsigned int)v0[0] * (unsigned int)v1[0])
    __builtin_abort ();
}

__attribute__((noinline)) void 
test_ouh_short ()
{
  volatile vector unsigned short v0 = {0, 1, 0, 0};
  volatile vector unsigned short v1 = {0, 0xff, 0, 0};
  vector unsigned int res = vec_vmulouh (v0, v1);
  if (res[0] != (unsigned int)v0[1] * (unsigned int)v1[1])
    __builtin_abort ();
}

int main ()
{
  test_eub_char();
  test_oub_char();
  test_euh_short();
  test_ouh_short();
}

/* { dg-final { scan-assembler-times "vmuleub" 1 } } */
/* { dg-final { scan-assembler-times "vmuloub" 1 } } */
/* { dg-final { scan-assembler-times "vmuleuh" 1 } } */
/* { dg-final { scan-assembler-times "vmulouh" 1 } } */

