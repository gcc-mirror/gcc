// PR tree-optimization/91010
// { dg-do compile }
// { dg-require-effective-target size32plus }
// { dg-additional-options "-fopenmp-simd -fno-tree-forwprop" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" { target i?86-*-* x86_64-*-* } } }

#include "simd-5.cc"
