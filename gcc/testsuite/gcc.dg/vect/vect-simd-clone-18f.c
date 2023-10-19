/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd --param vect-epilogues-nomask=0" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-additional-options "-mno-avx512f" { target { { i?86*-*-* x86_64-*-* } && { ! lp64 } } } } */

#define TYPE __INT64_TYPE__
#include "vect-simd-clone-18.c"

/* Ensure the the in-branch simd clones are used on targets that support them.
 */
/* { dg-final { scan-tree-dump-times {[\n\r] [^\n]* = foo\.simdclone} 2 "vect" } } */

/* The LTO test produces two dump files and we scan the wrong one.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
