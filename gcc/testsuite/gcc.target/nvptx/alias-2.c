/* { dg-do link } */
/* { dg-do run { target runtime_ptx_isa_version_6_3 } } */
/* { dg-options "-save-temps -malias -mptx=6.3 -O2" } */

#include "alias-1.c"

/* Inlined, so no alias.  */
/* { dg-final { scan-assembler-not "\\.alias.*;" } } */
/* { dg-final { scan-assembler-not "\\.visible \\.func f;" } } */

/* Note static and inlined, so still there.  */
/* { dg-final { scan-assembler-times "\\.visible \\.func __f;" 1 } } */

