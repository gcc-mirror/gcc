/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2 -fno-ipa-icf" } */

#include "atomic-comp-swap-release-acquire.x"

/* { dg-final { scan-assembler-times "ldaxr\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 4 } } */
/* { dg-final { scan-assembler-times "stlxr\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 4 } } */
