/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

#include <immintrin.h>

long long
foo (long long c)
{
  register long long a __asm ("k7") = c;
  long long b = foo (a);
  asm volatile ("" : "+k" (b));
  return b;
}
