/* { dg-do compile } */
/* { dg-options "-O3 -mtune=rocket -ffat-lto-objects -fdump-tree-switchlower1" } */

/* Rocket uses the generic threshold.  */
#include "case-values-threshold-1.c"

/* { dg-final { scan-tree-dump "case clusters: JT:0-7" "switchlower1" } } */
