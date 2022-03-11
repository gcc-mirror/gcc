/* { dg-do link } */
/* { dg-do run { target runtime_ptx_isa_version_6_3 } } */
/* { dg-options "-save-temps -malias -mptx=6.3 -O2" } */

#include "alias-3.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.func f;" } } */

/* Static and inlined, so it's deleted.  */
/* { dg-final { scan-assembler-not "\\.func __f;" } } */
