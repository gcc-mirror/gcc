/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7" } */
/* { dg-final { scan-assembler "xxsldwi"  } } */
/* { dg-final { scan-assembler "xxpermdi" } } */

#include <altivec.h>

vector double
v2df_shift (vector double a, vector double b)
{
  return vec_xxsldwi (a, b, 1);
}

vector float
v4sf_shift (vector float a, vector float b)
{
  return vec_xxsldwi (a, b, 1);
}

vector long long
v2di_shift (vector long long a, vector long long b)
{
  return vec_xxsldwi (a, b, 1);
}

vector unsigned long long
v2diu_shift (vector unsigned long long a, vector unsigned long long b)
{
  return vec_xxsldwi (a, b, 1);
}

vector int
v4si_shift (vector int a, vector int b)
{
  return vec_xxsldwi (a, b, 1);
}

vector unsigned int
v4siu_shift (vector unsigned int a, vector unsigned int b)
{
  return vec_xxsldwi (a, b, 1);
}

vector short
v8hi_shift (vector short a, vector short b)
{
  return vec_xxsldwi (a, b, 1);
}

vector unsigned short
v8hiu_shift (vector unsigned short a, vector unsigned short b)
{
  return vec_xxsldwi (a, b, 1);
}

vector signed char
v16qi_shift (vector signed char a, vector signed char b)
{
  return vec_xxsldwi (a, b, 1);
}

vector unsigned char
v16qiu_shift (vector unsigned char a, vector unsigned char b)
{
  return vec_xxsldwi (a, b, 1);
}

vector double
v2df_permute (vector double a, vector double b)
{
  return vec_xxpermdi (a, b, 1);
}

vector float
v4sf_permute (vector float a, vector float b)
{
  return vec_xxpermdi (a, b, 1);
}

vector long long
v2di_permute (vector long long a, vector long long b)
{
  return vec_xxpermdi (a, b, 1);
}

vector unsigned long long
v2diu_permute (vector unsigned long long a, vector unsigned long long b)
{
  return vec_xxpermdi (a, b, 1);
}

vector int
v4si_permute (vector int a, vector int b)
{
  return vec_xxpermdi (a, b, 1);
}

vector unsigned int
v4siu_permute (vector unsigned int a, vector unsigned int b)
{
  return vec_xxpermdi (a, b, 1);
}

vector short
v8hi_permute (vector short a, vector short b)
{
  return vec_xxpermdi (a, b, 1);
}

vector unsigned short
v8hiu_permute (vector unsigned short a, vector unsigned short b)
{
  return vec_xxpermdi (a, b, 1);
}

vector signed char
v16qi_permute (vector signed char a, vector signed char b)
{
  return vec_xxpermdi (a, b, 1);
}

vector unsigned char
v16qiu_permute (vector unsigned char a, vector unsigned char b)
{
  return vec_xxpermdi (a, b, 1);
}
