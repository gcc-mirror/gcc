/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2" } */

#include "sync-op-release.x"

/* { dg-final { scan-assembler-times "stlr" 1 } } */
