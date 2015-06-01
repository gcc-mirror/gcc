/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "sync-op-release.x"

/* { dg-final { scan-assembler-times "stlr" 1 } } */
