/* PR tree-optimization/63915 */
/* { dg-do run } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-options "-O2 -fopenmp-simd" } */
/* { dg-additional-options "-fpic" { target fpic } } */

#include "pr60823-2.c"
