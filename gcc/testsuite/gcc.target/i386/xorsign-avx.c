/* { dg-do run { target avx_runtime } } */
/* { dg-options "-O2 -mavx -mfpmath=sse -ftree-vectorize" } */

#include "xorsign.c"
