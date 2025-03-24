/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64 -mavx10.1-256 -mavx512f -mno-evex512" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */
/* { dg-warning "'-mevex512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

#include "avx10_1-1.c"
