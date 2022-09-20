/* { dg-options "-O3 -fno-math-errno --save-temps" } */

#pragma GCC target "+nosimd+fp"

#include "vect_unary_1.c"
