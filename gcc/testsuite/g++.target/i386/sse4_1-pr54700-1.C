/* PR target/54700 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -msse4 -mno-avx -mno-xop" } */
/* { dg-final { scan-assembler-not "pcmpgt\[bdq]" } } */
/* { dg-final { scan-assembler-times "pblendvb" 2 } } */
/* { dg-final { scan-assembler-times "blendvps" 4 } } */
/* { dg-final { scan-assembler-times "blendvpd" 4 } } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <x86intrin.h>

__attribute__((noipa)) __v16qi
f1 (__v16qi a, __v16qi b, __v16qi c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v16qi
f2 (__v16qi a, __v16qi b, __v16qi c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v4si
f3 (__v4si a, __v4si b, __v4si c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v4si
f4 (__v4si a, __v4si b, __v4si c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v2di
f5 (__v2di a, __v2di b, __v2di c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v2di
f6 (__v2di a, __v2di b, __v2di c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v4sf
f7 (__v4si a, __v4sf b, __v4sf c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v4sf
f8 (__v4si a, __v4sf b, __v4sf c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v2df
f9 (__v2di a, __v2df b, __v2df c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v2df
f10 (__v2di a, __v2df b, __v2df c)
{
  return a >= 0 ? b : c;
}
