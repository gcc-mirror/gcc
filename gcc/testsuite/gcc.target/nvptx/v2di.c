/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

typedef long long int __v2di __attribute__((__vector_size__(16)));

#define TYPE __v2di
#include "vec.inc"

/* { dg-final { scan-assembler ".reg\\.v2\\.u64" } } */
/* { dg-final { scan-assembler "ld\\.v2\\.u64" } } */
/* { dg-final { scan-assembler "st\\.v2\\.u64" } } */
/* { dg-final { scan-assembler "mov\\.v2\\.u64.*\\{ 1, 2 \\}" } } */
