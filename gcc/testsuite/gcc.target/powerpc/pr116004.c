/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-final { scan-assembler-times {\mvcmpnezb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvcmpnezh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvcmpnezw\M} 2 } } */
/* { dg-final { scan-assembler-not {\mvcmpneb\M} } } */
/* { dg-final { scan-assembler-not {\mvcmpneh\M} } } */
/* { dg-final { scan-assembler-not {\mvcmpnew\M} } } */

#include <altivec.h>
#include <stdint.h>

unsigned int
test_schar (vector signed char a, vector signed char b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}

unsigned int
test_uchar (vector unsigned char a, vector unsigned char b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}

unsigned int
test_sshort (vector signed short a, vector signed short b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}

unsigned int
test_ushort (vector unsigned short a, vector unsigned short b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}

unsigned int
test_sint (vector signed int a, vector signed int b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}

unsigned int
test_uint (vector unsigned int a, vector unsigned int b)
{
  return vec_first_mismatch_or_eos_index (a, b);
}
