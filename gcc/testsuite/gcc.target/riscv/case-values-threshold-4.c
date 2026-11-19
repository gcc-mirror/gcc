/* { dg-do compile } */
/* { dg-options "-O3 -mtune=spacemit-x100 -ffat-lto-objects -fdump-tree-switchlower1" } */

/* Ten cases are just below the speed threshold.  */
#define NUM_CASES 10
#include "case-values-threshold-1.c"

/* { dg-final { scan-tree-dump "case clusters: 0 1 2 3 4 5 6 7 8 9" "switchlower1" } } */
