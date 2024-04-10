/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mrvv-vector-bits=scalable -mabi=lp64d -O3 -fno-schedule-insns2" } */

#include "def.h"

DEF_RET1_ARG0 (v1si)
DEF_RET1_ARG0 (v2si)
DEF_RET1_ARG0 (v4si)
DEF_RET1_ARG0 (v8si)
DEF_RET1_ARG0 (v16si)
DEF_RET1_ARG0 (v32si)
DEF_RET1_ARG0 (v64si)
DEF_RET1_ARG0 (v128si)
DEF_RET1_ARG0 (v256si)
DEF_RET1_ARG0 (v512si)
DEF_RET1_ARG0 (v1024si)

DEF_RET1_ARG1 (v1si)
DEF_RET1_ARG1 (v2si)
DEF_RET1_ARG1 (v4si)
DEF_RET1_ARG1 (v8si)
DEF_RET1_ARG1 (v16si)
DEF_RET1_ARG1 (v32si)
DEF_RET1_ARG1 (v64si)
DEF_RET1_ARG1 (v128si)
DEF_RET1_ARG1 (v256si)
DEF_RET1_ARG1 (v512si)
DEF_RET1_ARG1 (v1024si)

DEF_RET1_ARG2 (v1si)
DEF_RET1_ARG2 (v2si)
DEF_RET1_ARG2 (v4si)
DEF_RET1_ARG2 (v8si)
DEF_RET1_ARG2 (v16si)
DEF_RET1_ARG2 (v32si)
DEF_RET1_ARG2 (v64si)
DEF_RET1_ARG2 (v128si)
DEF_RET1_ARG2 (v256si)
DEF_RET1_ARG2 (v512si)
DEF_RET1_ARG2 (v1024si)

DEF_RET1_ARG3 (v1si)
DEF_RET1_ARG3 (v2si)
DEF_RET1_ARG3 (v4si)
DEF_RET1_ARG3 (v8si)
DEF_RET1_ARG3 (v16si)
DEF_RET1_ARG3 (v32si)
DEF_RET1_ARG3 (v64si)
DEF_RET1_ARG3 (v128si)
DEF_RET1_ARG3 (v256si)
DEF_RET1_ARG3 (v512si)
DEF_RET1_ARG3 (v1024si)

DEF_RET1_ARG4 (v1si)
DEF_RET1_ARG4 (v2si)
DEF_RET1_ARG4 (v4si)
DEF_RET1_ARG4 (v8si)
DEF_RET1_ARG4 (v16si)
DEF_RET1_ARG4 (v32si)
DEF_RET1_ARG4 (v64si)
DEF_RET1_ARG4 (v128si)
DEF_RET1_ARG4 (v256si)
DEF_RET1_ARG4 (v512si)
DEF_RET1_ARG4 (v1024si)

DEF_RET1_ARG5 (v1si)
DEF_RET1_ARG5 (v2si)
DEF_RET1_ARG5 (v4si)
DEF_RET1_ARG5 (v8si)
DEF_RET1_ARG5 (v16si)
DEF_RET1_ARG5 (v32si)
DEF_RET1_ARG5 (v64si)
DEF_RET1_ARG5 (v128si)
DEF_RET1_ARG5 (v256si)
DEF_RET1_ARG5 (v512si)
DEF_RET1_ARG5 (v1024si)

DEF_RET1_ARG6 (v1si)
DEF_RET1_ARG6 (v2si)
DEF_RET1_ARG6 (v4si)
DEF_RET1_ARG6 (v8si)
DEF_RET1_ARG6 (v16si)
DEF_RET1_ARG6 (v32si)
DEF_RET1_ARG6 (v64si)
DEF_RET1_ARG6 (v128si)
DEF_RET1_ARG6 (v256si)
DEF_RET1_ARG6 (v512si)
DEF_RET1_ARG6 (v1024si)

DEF_RET1_ARG7 (v1si)
DEF_RET1_ARG7 (v2si)
DEF_RET1_ARG7 (v4si)
DEF_RET1_ARG7 (v8si)
DEF_RET1_ARG7 (v16si)
DEF_RET1_ARG7 (v32si)
DEF_RET1_ARG7 (v64si)
DEF_RET1_ARG7 (v128si)
DEF_RET1_ARG7 (v256si)
DEF_RET1_ARG7 (v512si)
DEF_RET1_ARG7 (v1024si)

DEF_RET1_ARG8 (v1si)
DEF_RET1_ARG8 (v2si)
DEF_RET1_ARG8 (v4si)
DEF_RET1_ARG8 (v8si)
DEF_RET1_ARG8 (v16si)
DEF_RET1_ARG8 (v32si)
DEF_RET1_ARG8 (v64si)
DEF_RET1_ARG8 (v128si)
DEF_RET1_ARG8 (v256si)
DEF_RET1_ARG8 (v512si)
DEF_RET1_ARG8 (v1024si)

DEF_RET1_ARG9 (v1si)
DEF_RET1_ARG9 (v2si)
DEF_RET1_ARG9 (v4si)
DEF_RET1_ARG9 (v8si)
DEF_RET1_ARG9 (v16si)
DEF_RET1_ARG9 (v32si)
DEF_RET1_ARG9 (v64si)
DEF_RET1_ARG9 (v128si)
DEF_RET1_ARG9 (v256si)
DEF_RET1_ARG9 (v512si)
DEF_RET1_ARG9 (v1024si)

// RET1_ARG0 tests
/* { dg-final { scan-assembler-times {li\s+a[0-1],\s*0} 7 } } */
/* { dg-final { scan-assembler-times {mv\s+s0,a0\s+call\s+memset\s+mv\s+a0,s0} 3 } } */

// v1si tests: return value (lw) and function prologue (sw)
// 1 lw per test, argnum sw's when args > 1
/* { dg-final { scan-assembler-times {lw\s+a0,\s*[0-9]+\(sp\)} 8 } } */
/* { dg-final { scan-assembler-times {sw\s+a[0-7],\s*[0-9]+\(sp\)} 43 } } */

// v2si and v4si tests: return value (ld) and function prologue (sd)
//   - 1 ld per v2si and 2 ld per v4si with args > 1
//   - argnum sd's per v2si when argnum > 1 
//   - 2 * argnum sd's per v4si when argnum > 0
/* { dg-final { scan-assembler-times {ld\s+a[0-1],\s*[0-9]+\(sp\)} 24 } } */
/* { dg-final { scan-assembler-times {sd\s+a[0-7],\s*[0-9]+\(sp\)} 103 } } */

// v8-1024si tests: return value (vse32.v)
/* { dg-final { scan-assembler-times {vse32.v\s+v[0-9],\s*[0-9]+\(a0\)} 74 } } */
// 256-1024si tests: return value (vse64.v)
// for some reason ARG1 returns using vse64 instead of vse32
/* { dg-final { scan-assembler-times {vse64.v\s+v[0-9],\s*[0-9]+\(a0\)\s+ret} 3 } } */
