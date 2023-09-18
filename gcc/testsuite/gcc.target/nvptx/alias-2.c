/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alias_ptx } } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options nvptx_alias_ptx } */

#include "alias-1.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.visible \\.func f;" } } */

/* Note extern and inlined, so still there.  */
/* { dg-final { scan-assembler-times "\\.visible \\.func __f;" 1 } } */

