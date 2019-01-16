/* { dg-lto-do run } */
/* Call to foo should be devirtualized because there are no derived types of A.  */
/* { dg-lto-options "-O2 -flto"  } */
#include "../ipa/devirt-13.C"
/* Ideally we should also { scan-tree-dump-times "OBJ_TYPE_REF" 0 "ssa"}.  */
