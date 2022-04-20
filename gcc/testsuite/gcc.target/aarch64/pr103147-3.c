/* { dg-options "-fpack-struct=2" } */

#include <arm_neon.h>

int assert1[__alignof__(int32x2_t) == 8 ? 1 : -1];
int assert2[__alignof__(int32x4_t) == 16 ? 1 : -1];
int assert3[__alignof__(int32x2x2_t) == 2 ? 1 : -1];
int assert4[__alignof__(int32x4x2_t) == 2 ? 1 : -1];
int assert5[__alignof__(int32x2x3_t) == 2 ? 1 : -1];
int assert6[__alignof__(int32x4x3_t) == 2 ? 1 : -1];
int assert7[__alignof__(int32x2x4_t) == 2 ? 1 : -1];
int assert8[__alignof__(int32x4x4_t) == 2 ? 1 : -1];
