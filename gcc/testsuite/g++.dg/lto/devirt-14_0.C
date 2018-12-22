/* { dg-lto-do run } */
/* { dg-lto-options "-O2 -fdump-tree-ssa"  } */
#include "../ipa/devirt-14.C"
/* { dg-final { scan-tree-dump-not "A.*foo" "ssa"} } */
