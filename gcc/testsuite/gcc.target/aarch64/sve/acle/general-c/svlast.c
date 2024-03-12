/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=256" } */

#include <stdint.h>
#include "arm_sve.h"

#define NAME(name, size, pat, sign, ab) \
  name ## _ ## size ## _ ## pat ## _ ## sign ## _ ## ab

#define NAMEF(name, size, pat, sign, ab) \
  name ## _ ## size ## _ ## pat ## _ ## sign ## _ ## ab ## _false

#define SVTYPE(size, sign) \
  sv ## sign ## int ## size ## _t

#define STYPE(size, sign) sign ## int ## size ##_t

#define SVELAST_DEF(size, pat, sign, ab, su) \
  STYPE (size, sign) __attribute__((noinline)) \
  NAME (foo, size, pat, sign, ab) (SVTYPE (size, sign) x) \
  { \
    return svlast ## ab (svptrue_pat_b ## size (pat), x); \
  } \
  STYPE (size, sign) __attribute__((noinline)) \
  NAMEF (foo, size, pat, sign, ab) (SVTYPE (size, sign) x) \
  { \
    return svlast ## ab (svpfalse (), x); \
  }

#define ALL_PATS(SIZE, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL1, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL2, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL3, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL4, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL5, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL6, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL7, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL8, SIGN, AB, SU) \
  SVELAST_DEF (SIZE, SV_VL16, SIGN, AB, SU)

#define ALL_SIGN(SIZE, AB) \
  ALL_PATS (SIZE, , AB, s) \
  ALL_PATS (SIZE, u, AB, u)

#define ALL_SIZE(AB) \
  ALL_SIGN (8, AB) \
  ALL_SIGN (16, AB) \
  ALL_SIGN (32, AB) \
  ALL_SIGN (64, AB)

#define ALL_POS() \
  ALL_SIZE (a) \
  ALL_SIZE (b)


ALL_POS()

/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b} 52 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h} 50 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s} 12 } } */
/* { dg-final { scan-assembler-times {\tfmov\tw0, s0} 24 } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tfmov\tx0, d0} 32 } } */
