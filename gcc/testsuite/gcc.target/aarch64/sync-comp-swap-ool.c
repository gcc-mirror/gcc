/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2 -fno-ipa-icf -moutline-atomics" } */

#include "sync-comp-swap.x"

/* { dg-final { scan-assembler-times "bl.*__aarch64_cas4_sync" 1 } } */
