/* { dg-do compile } */
/* { dg-options "-O0 -march=x86-64 -mavx10.1-256 -Wno-psabi" } */
/* { dg-final { scan-assembler-not ".%zmm" } } */

#include "avx10_1-2.c"
