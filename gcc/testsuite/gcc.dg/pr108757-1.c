/* PR tree-optimization/108757 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <limits.h>
#define N 5
#define M 3
#define GAP 0
typedef unsigned int UINT;
typedef int INT;
#define UMAX UINT_MAX
#define IMAX INT_MAX
#define IMIN INT_MIN
#include "pr108757.h"

/* { dg-final { scan-tree-dump-not " = x_\[0-9\]+\\(D\\) \\+ " "optimized" } } *
/* { dg-final { scan-tree-dump-not " = x_\[0-9\]+\\(D\\) \\- " "optimized" } } */
/* { dg-final { scan-tree-dump-not " = b_\[0-9\]+ \\+ " "optimized" } } */
