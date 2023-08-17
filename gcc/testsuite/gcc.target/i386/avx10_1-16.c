/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1-256 -mavx10.1-512" } */
/* { dg-warning "The options used for AVX10 have conflict vector width, using the latter 512 as vector width" "" { target *-*-* } 0 } */

#include "avx10_1-2.c"
