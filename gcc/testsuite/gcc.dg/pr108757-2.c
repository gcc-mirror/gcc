/* PR tree-optimization/108757 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fwrapv" } */

#include <limits.h>
#define N 4
#define M 3
#define GAP 2
typedef unsigned int UINT;
typedef int INT;
#define UMAX UINT_MAX
#define IMAX INT_MAX
#define IMIN INT_MIN
#include "pr108757.h"

/* { dg-final { scan-tree-dump-times " = x_\[0-9\]+\\(D\\) \\+ " 16 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = x_\[0-9\]+\\(D\\) \\- " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\+ x_\[0-9\]+\\(D\\)" 3 "optimized" } } */

