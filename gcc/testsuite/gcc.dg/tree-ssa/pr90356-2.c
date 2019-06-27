/* PR tree-optimization/90356 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-rounding-math -fno-signaling-nans -fsigned-zeros -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]*.D. \\+ 0.0;" 12 "optimized" } } */
/* { dg-final { scan-tree-dump-times "y_\[0-9]*.D. - 0.0;" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \[+-] 0.0;" 12 "optimized" } } */

#include "pr90356-1.c"
