/* { dg-do link } */
/* { dg-do run { target runtime_ptx_alias } } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options ptx_alias } */

#include "alias-3.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.func f;" } } */

/* Static and inlined, so it's deleted.  */
/* { dg-final { scan-assembler-not "\\.func __f;" } } */
