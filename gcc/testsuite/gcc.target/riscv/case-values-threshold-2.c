/* { dg-do compile } */
/* { dg-options "-O3 -mtune=spacemit-x100 -ffat-lto-objects -fdump-tree-switchlower1 --param=case-values-threshold=5" } */

#include "case-values-threshold-1.c"

/* { dg-final { scan-tree-dump "case clusters: JT:0-7" "switchlower1" } } */
