/* { dg-do link } */
/* { dg-do run { target runtime_ptx_alias } } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options ptx_alias } */

#include "alias-1.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.visible \\.func f;" } } */

/* Note static and inlined, so still there.  */
/* { dg-final { scan-assembler-times "\\.visible \\.func __f;" 1 } } */

