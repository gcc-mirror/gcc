/* Make certain that we pass __m256i in the correct register for AVX.  */
/* { dg-do compile } */
/* { dg-options "-O1 -mavx" } */
/* { dg-options "-mabi=sysv -O1 -mavx" { target x86_64-*-mingw* } } */

typedef long long __m256i __attribute__ ((__vector_size__ (32)));
__m256i foo (void) { return (__m256i){ 1, 2, 3, 4 }; }

/* { dg-final { scan-assembler-times "ymm0" 1 } } */
