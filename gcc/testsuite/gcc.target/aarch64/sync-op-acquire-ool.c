/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2 -moutline-atomics" } */

#include "sync-op-acquire.x"

/* { dg-final { scan-assembler-times "bl.*__aarch64_swp4_sync" 1 } } */
