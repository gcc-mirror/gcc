/* { dg-do compile } */
/* { dg-options "-O3 -mtune=spacemit-x100 -ffat-lto-objects -fdump-tree-switchlower1" } */

/* Eleven cases meet the speed threshold.  */
#define NUM_CASES 11
#include "case-values-threshold-1.c"

/* { dg-final { scan-tree-dump "case clusters: JT:0-10" "switchlower1" } } */
