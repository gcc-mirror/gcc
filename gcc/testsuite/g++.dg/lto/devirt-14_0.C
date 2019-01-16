/* { dg-lto-do run } */
/* { dg-lto-options "-O2 -fdump-tree-ssa"  } */
#include "../ipa/devirt-14.C"
/* Ideally we should also { scan-tree-dump-not "A.*foo" "ssa"} } */
