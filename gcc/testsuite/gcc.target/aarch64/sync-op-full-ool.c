/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2 -moutline-atomics" } */

#include "sync-op-full.x"

/* { dg-final { scan-assembler-times "bl.*__aarch64_ldadd4_sync" 1 } } */
/* { dg-final { scan-assembler-times "bl.*__aarch64_ldclr4_sync" 1 } } */
/* { dg-final { scan-assembler-times "bl.*__aarch64_ldeor4_sync" 1 } } */
/* { dg-final { scan-assembler-times "bl.*__aarch64_ldset4_sync" 1 } } */
