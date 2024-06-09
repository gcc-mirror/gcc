/* PR middle-end/114157 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -Wno-psabi -w -mavx2 -mno-avx512f" } */

#include "../../gcc.dg/bitint-98.c"
