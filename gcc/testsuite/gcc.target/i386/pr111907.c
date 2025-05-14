/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-evex512" } */
/* { dg-warning "'-mevex512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

_Float128
foo (_Float128 d, _Float128 e)
{
  return __builtin_copysignf128 (d, e);
}
