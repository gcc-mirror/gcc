/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define TYPE float
#include "vect-simd-clone-16.c"

/* Ensure the the in-branch simd clones are used on targets that support them.
   Some targets use another call for the epilogue loops.  */
/* { dg-final { scan-tree-dump-times {[\n\r] [^\n]* = foo\.simdclone} 2 "vect" { target { ! { avx_runtime || aarch64*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times {[\n\r] [^\n]* = foo\.simdclone} 3 "vect" { target { avx_runtime || aarch64*-*-* } } } } */

/* The LTO test produces two dump files and we scan the wrong one.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
