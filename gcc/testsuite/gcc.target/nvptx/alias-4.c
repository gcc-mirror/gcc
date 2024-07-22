/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alias_ptx } } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options nvptx_alias_ptx } */

#include "alias-3.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.func f;" } } */

/* Static and inlined, so it's deleted.  */
/* { dg-final { scan-assembler-not "\\.func __f;" } } */
