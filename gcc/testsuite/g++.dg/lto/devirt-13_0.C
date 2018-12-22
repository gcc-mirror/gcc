/* { dg-lto-do run } */
/* Call to foo should be devirtualized because there are no derived types of A.  */
/* { dg-lto-options "-O2 -flto -fdump-tree-ssa"  } */
#include "../ipa/devirt-13.C"
/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "ssa"} } */
