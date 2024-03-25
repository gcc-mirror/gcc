/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64 -mavx10.1 -mavx512f -mno-evex512" } */

#include "avx10_1-1.c"
