/* { dg-do compile } */
/* { dg-options "-O2 -misa=sm_53 -mptx=_" } */
/* { dg-additional-options "-mexperimental" } */

_Float16 x;
_Float16 y;

_Bool eq()
{
  return x == y;
}

_Bool ne()
{
  return x != y;
}

_Bool lt()
{
  return x < y;
}

_Bool le()
{
  return x <= y;
}

_Bool gt()
{
  return x < y;
}

_Bool ge()
{
  return x >= y;
}

/* { dg-final { scan-assembler-times "setp\.\[a-z\]*\.f16" 6 } } */
/* { dg-final { scan-assembler-not "cvt.f32.f16" } } */
