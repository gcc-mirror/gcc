/* { dg-do compile } */
/* { dg-options "-Os -mtune=xt-c9501fdvt -ffat-lto-objects -fdump-tree-switchlower1" } */

#include "case-values-threshold-1.c"

/* { dg-final { scan-tree-dump "case clusters: JT:0-7" "switchlower1" } } */
