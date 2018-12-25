/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vmaxsh\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "vcomish\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vmovsh\[ \\t\]" { target { ! ia32 } } } } */

#include <immintrin.h>

_Float16
foo (_Float16 x, _Float16 y)
{
  x = x > y ? x : y;
  return x;
}
