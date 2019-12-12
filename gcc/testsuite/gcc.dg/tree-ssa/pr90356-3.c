/* PR tree-optimization/90356 */
/* { dg-do compile } */
/* { dg-options "-O2 -frounding-math -fsignaling-nans -fsigned-zeros -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \[+-] 0.0;" 32 "optimized" } } */

#include "pr90356-1.c"
