/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2" } */

#include "sync-op-full.x"

/* { dg-final { scan-assembler-times "ldxr\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 12 } } */
/* { dg-final { scan-assembler-times "stlxr\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 12 } } */
/* { dg-final { scan-assembler-times "dmb\tish" 12 } } */
