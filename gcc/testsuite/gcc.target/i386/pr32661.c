/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));

int fooSI_1(__v4si *val)
{
  return __builtin_ia32_vec_ext_v4si(*val, 1);
}
/* { dg-final { scan-assembler-not "pshufd" } } */

int fooSI_2(__v4si *val)
{
  return __builtin_ia32_vec_ext_v4si(*val, 2);
}
/* { dg-final { scan-assembler-not "punpckhdq" } } */

float fooSF_2(__v4sf *val)
{
  return __builtin_ia32_vec_ext_v4sf(*val, 2);
}
/* { dg-final { scan-assembler-not "unpckhps" } } */

float fooSF_3(__v4sf *val)
{
  return __builtin_ia32_vec_ext_v4sf(*val, 3);
}
/* { dg-final { scan-assembler-not "shufps" } } */
