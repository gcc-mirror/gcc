/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

#define int unsigned
#include "evrp-trans.c"

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
/* { dg-final { scan-tree-dump-times "keep" 13 "evrp"} } */
