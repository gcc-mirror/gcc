/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-vrp -fno-tree-ccp -fdisable-tree-evrp -fno-thread-jumps -fdump-ipa-cp-details" } */

#include "pure-const-3.h"

/* { dg-final { scan-ipa-dump "Propagated bits info for function int b"  "cp"  } } */
