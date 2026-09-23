/* { dg-do compile } */
/* { dg-options "-O2 -march=znver6" } */

#ifndef __AVX512BMM__
# error AVX512BMM should be enabled for target znver6.
#endif
