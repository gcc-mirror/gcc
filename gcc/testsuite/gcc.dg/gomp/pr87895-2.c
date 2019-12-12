/* PR tree-optimization/87895 */
/* { dg-do compile } */
/* { dg-additional-options "-O1" } */

#include "pr87895-1.c"
/* { dg-excess-errors "partial simd clone support" { target { aarch64*-*-* } } }  */
