/* { dg-do compile } */
/* { dg-options "-O -mavx512vl -mavx512fp16" } */

typedef __bf16 __attribute__((__vector_size__ (16))) BF;
typedef float __attribute__((__vector_size__ (32))) F;

BF
foo (F f)
{
  return __builtin_convertvector (f, BF);
}
