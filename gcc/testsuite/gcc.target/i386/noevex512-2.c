/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx512bw -mno-evex512" } */
/* { dg-warning "'-mevex512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

#include <immintrin.h>

long long
foo (long long c)
{
  register long long a __asm ("k7") = c;
  long long b = foo (a);
  asm volatile ("" : "+k" (b));
  return b;
}
