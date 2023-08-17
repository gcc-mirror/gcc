/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1-512 -mavx10.1-256" } */
/* { dg-warning "The options used for AVX10 have conflict vector width, using the latter 256 as vector width" "" { target *-*-* } 0 } */

#include "avx10_1-1.c"
