/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_spu } */
/* { dg-options "-maltivec" } */

#include <altivec.h>
#include <spu2vmx.h>

vec_uint4 f(vec_uint4 a, vec_uint4 b)
{
  return spu_add(a, b);
}
vec_float4 f(vec_float4 a, vec_float4 b)
{
  return spu_add(a, b);
}
