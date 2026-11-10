/* { dg-do run { target aarch64_sve2p1_hw } } */
/* { dg-options "-O3 -ffast-math -march=armv9.4-a+sve2p1 -mautovec-preference=sve-only -save-temps" } */

#include "../fdot-autovec-run.h"

/* { dg-final { scan-assembler {\tfdot\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h} } } */
