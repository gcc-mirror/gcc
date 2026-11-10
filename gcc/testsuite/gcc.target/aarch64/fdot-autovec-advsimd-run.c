/* { dg-do run { target aarch64_f16f32dot_hw } } */
/* { dg-options "-O3 -ffast-math -march=armv8.2-a+fp16+f16f32dot -mautovec-preference=asimd-only -save-temps" } */

#include "fdot-autovec-run.h"

/* { dg-final { scan-assembler {\tfdot\tv[0-9]+\.4s, v[0-9]+\.8h, v[0-9]+\.8h} } } */
/* { dg-final { scan-assembler {\tfdot\tv[0-9]+\.2s, v[0-9]+\.4h, v[0-9]+\.4h} } } */
