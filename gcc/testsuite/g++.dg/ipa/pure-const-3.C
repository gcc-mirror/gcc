/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-vrp -fdump-tree-optimized -fno-tree-ccp -fdisable-tree-evrp -fdisable-tree-vrp1 -fdisable-tree-vrp2 -fno-thread-jumps -fno-tree-dominator-opts"  } */

#include "pure-const-3.h"

/* { dg-final { scan-tree-dump "barvar"  "optimized"  } } */
