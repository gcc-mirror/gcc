/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc64*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power8" } */
/* { dg-final { scan-assembler-times "vcmpgtsb" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtub" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsh" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtuh" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsw" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtuw" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsd" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtud" 2 } } */
/* { dg-final { scan-assembler-times "xxlnor" 16 } } */

#include <altivec.h>

vector bool char
cmple_sc (vector signed char x, vector signed char y)
{
  return vec_cmple (x, y);
}

vector bool char
cmple_uc (vector unsigned char x, vector unsigned char y)
{
  return vec_cmple (x, y);
}

vector bool short
cmple_ss (vector signed short x, vector signed short y)
{
  return vec_cmple (x, y);
}

vector bool short
cmple_us (vector unsigned short x, vector unsigned short y)
{
  return vec_cmple (x, y);
}

vector bool int
cmple_si (vector signed int x, vector signed int y)
{
  return vec_cmple (x, y);
}

vector bool int
cmple_ui (vector unsigned int x, vector unsigned int y)
{
  return vec_cmple (x, y);
}

vector bool long long
cmple_sl (vector signed long long x, vector signed long long y)
{
  return vec_cmple (x, y);
}

vector bool long long
cmple_ul (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmple (x, y);
}

vector bool char
cmpge_sc (vector signed char x, vector signed char y)
{
  return vec_cmpge (x, y);
}

vector bool char
cmpge_uc (vector unsigned char x, vector unsigned char y)
{
  return vec_cmpge (x, y);
}

vector bool short
cmpge_ss (vector signed short x, vector signed short y)
{
  return vec_cmpge (x, y);
}

vector bool short
cmpge_us (vector unsigned short x, vector unsigned short y)
{
  return vec_cmpge (x, y);
}

vector bool int
cmpge_si (vector signed int x, vector signed int y)
{
  return vec_cmpge (x, y);
}

vector bool int
cmpge_ui (vector unsigned int x, vector unsigned int y)
{
  return vec_cmpge (x, y);
}

vector bool long long
cmpge_sl (vector signed long long x, vector signed long long y)
{
  return vec_cmpge (x, y);
}

vector bool long long
cmpge_ul (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmpge (x, y);
}

