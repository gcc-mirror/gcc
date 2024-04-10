/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mrvv-vector-bits=scalable -mabi=lp64d -O3 -fno-schedule-insns2" } */

#include "def.h"

DEF_RET1_ARG0 (v1di)
DEF_RET1_ARG0 (v2di)
DEF_RET1_ARG0 (v4di)
DEF_RET1_ARG0 (v8di)
DEF_RET1_ARG0 (v16di)
DEF_RET1_ARG0 (v32di)
DEF_RET1_ARG0 (v64di)
DEF_RET1_ARG0 (v128di)
DEF_RET1_ARG0 (v256di)
DEF_RET1_ARG0 (v512di)

DEF_RET1_ARG1 (v1di)
DEF_RET1_ARG1 (v2di)
DEF_RET1_ARG1 (v4di)
DEF_RET1_ARG1 (v8di)
DEF_RET1_ARG1 (v16di)
DEF_RET1_ARG1 (v32di)
DEF_RET1_ARG1 (v64di)
DEF_RET1_ARG1 (v128di)
DEF_RET1_ARG1 (v256di)
DEF_RET1_ARG1 (v512di)

DEF_RET1_ARG2 (v1di)
DEF_RET1_ARG2 (v2di)
DEF_RET1_ARG2 (v4di)
DEF_RET1_ARG2 (v8di)
DEF_RET1_ARG2 (v16di)
DEF_RET1_ARG2 (v32di)
DEF_RET1_ARG2 (v64di)
DEF_RET1_ARG2 (v128di)
DEF_RET1_ARG2 (v256di)
DEF_RET1_ARG2 (v512di)

DEF_RET1_ARG3 (v1di)
DEF_RET1_ARG3 (v2di)
DEF_RET1_ARG3 (v4di)
DEF_RET1_ARG3 (v8di)
DEF_RET1_ARG3 (v16di)
DEF_RET1_ARG3 (v32di)
DEF_RET1_ARG3 (v64di)
DEF_RET1_ARG3 (v128di)
DEF_RET1_ARG3 (v256di)
DEF_RET1_ARG3 (v512di)

DEF_RET1_ARG4 (v1di)
DEF_RET1_ARG4 (v2di)
DEF_RET1_ARG4 (v4di)
DEF_RET1_ARG4 (v8di)
DEF_RET1_ARG4 (v16di)
DEF_RET1_ARG4 (v32di)
DEF_RET1_ARG4 (v64di)
DEF_RET1_ARG4 (v128di)
DEF_RET1_ARG4 (v256di)
DEF_RET1_ARG4 (v512di)

DEF_RET1_ARG5 (v1di)
DEF_RET1_ARG5 (v2di)
DEF_RET1_ARG5 (v4di)
DEF_RET1_ARG5 (v8di)
DEF_RET1_ARG5 (v16di)
DEF_RET1_ARG5 (v32di)
DEF_RET1_ARG5 (v64di)
DEF_RET1_ARG5 (v128di)
DEF_RET1_ARG5 (v256di)
DEF_RET1_ARG5 (v512di)

DEF_RET1_ARG6 (v1di)
DEF_RET1_ARG6 (v2di)
DEF_RET1_ARG6 (v4di)
DEF_RET1_ARG6 (v8di)
DEF_RET1_ARG6 (v16di)
DEF_RET1_ARG6 (v32di)
DEF_RET1_ARG6 (v64di)
DEF_RET1_ARG6 (v128di)
DEF_RET1_ARG6 (v256di)
DEF_RET1_ARG6 (v512di)

DEF_RET1_ARG7 (v1di)
DEF_RET1_ARG7 (v2di)
DEF_RET1_ARG7 (v4di)
DEF_RET1_ARG7 (v8di)
DEF_RET1_ARG7 (v16di)
DEF_RET1_ARG7 (v32di)
DEF_RET1_ARG7 (v64di)
DEF_RET1_ARG7 (v128di)
DEF_RET1_ARG7 (v256di)
DEF_RET1_ARG7 (v512di)

DEF_RET1_ARG8 (v1di)
DEF_RET1_ARG8 (v2di)
DEF_RET1_ARG8 (v4di)
DEF_RET1_ARG8 (v8di)
DEF_RET1_ARG8 (v16di)
DEF_RET1_ARG8 (v32di)
DEF_RET1_ARG8 (v64di)
DEF_RET1_ARG8 (v128di)
DEF_RET1_ARG8 (v256di)
DEF_RET1_ARG8 (v512di)

DEF_RET1_ARG9 (v1di)
DEF_RET1_ARG9 (v2di)
DEF_RET1_ARG9 (v4di)
DEF_RET1_ARG9 (v8di)
DEF_RET1_ARG9 (v16di)
DEF_RET1_ARG9 (v32di)
DEF_RET1_ARG9 (v64di)
DEF_RET1_ARG9 (v128di)
DEF_RET1_ARG9 (v256di)
DEF_RET1_ARG9 (v512di)

// RET1_ARG0 tests
/* { dg-final { scan-assembler-times {li\s+a[0-1],\s*0} 6 } } */
/* { dg-final { scan-assembler-times {mv\s+s0,a0\s+call\s+memset\s+mv\s+a0,s0} 3 } } */

// v1di and v2di tests: return value (ld) and function prologue (sd)
//   - 1 ld per v1di and 2 ld per v2di with args > 1
//   - argnum sd's per v1di when argnum > 1 
//   - 2 * argnum sd's per v2di when argnum > 0
/* { dg-final { scan-assembler-times {ld\s+a[0-1],\s*[0-9]+\(sp\)} 24 } } */
/* { dg-final { scan-assembler-times {sd\s+a[0-7],\s*[0-9]+\(sp\)} 103 } } */

// v4-512di tests: return value (vse64.v)
/* { dg-final { scan-assembler-times {vse64.v\s+v[0-9],\s*[0-9]+\(a0\)} 77 } } */
