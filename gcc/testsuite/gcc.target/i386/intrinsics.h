/* Check that all x86 intrinsics can be included together.  */
/* { dg-do compile } */
/* { dg-options "-O -pedantic-errors -march=k8 -m3dnow -mavx -msse5 -maes -mpclmul" } */

#include <immintrin.h>
#include <bmmintrin.h>
#include <mm3dnow.h>

int dummy;
