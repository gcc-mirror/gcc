/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-evex512" } */

_Float128
foo (_Float128 d, _Float128 e)
{
  return __builtin_copysignf128 (d, e);
}
