/* { dg-options "-O2" } */

#pragma GCC aarch64 "arm_sve.h"

static inline svint32_t foo () { return svdup_s32 (32); }
svint32_t bar () { return svadd_x (svptrue_b8 (), foo (), 1); }
