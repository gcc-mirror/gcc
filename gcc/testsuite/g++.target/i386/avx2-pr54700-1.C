/* PR target/54700 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx2 -mno-xop -mno-avx512f" } */
/* { dg-final { scan-assembler-not "vpcmpgt\[bdq]" } } */
/* { dg-final { scan-assembler-times "vpblendvb" 2 } } */
/* { dg-final { scan-assembler-times "vblendvps" 4 } } */
/* { dg-final { scan-assembler-times "vblendvpd" 4 } } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <x86intrin.h>

__attribute__((noipa)) __v32qi
f1 (__v32qi a, __v32qi b, __v32qi c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v32qi
f2 (__v32qi a, __v32qi b, __v32qi c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v8si
f3 (__v8si a, __v8si b, __v8si c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v8si
f4 (__v8si a, __v8si b, __v8si c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v4di
f5 (__v4di a, __v4di b, __v4di c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v4di
f6 (__v4di a, __v4di b, __v4di c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v8sf
f7 (__v8si a, __v8sf b, __v8sf c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v8sf
f8 (__v8si a, __v8sf b, __v8sf c)
{
  return a >= 0 ? b : c;
}

__attribute__((noipa)) __v4df
f9 (__v4di a, __v4df b, __v4df c)
{
  return a < 0 ? b : c;
}

__attribute__((noipa)) __v4df
f10 (__v4di a, __v4df b, __v4df c)
{
  return a >= 0 ? b : c;
}
