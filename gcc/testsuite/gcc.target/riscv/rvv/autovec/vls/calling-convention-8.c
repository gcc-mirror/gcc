/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -mrvv-vector-bits=scalable -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** v8qi_RET1_ARG0:
** li\s+a0,\s*0
** ret
*/
DEF_RET1_ARG0 (v8qi)

/*
** v4hi_RET1_ARG1:
** ret
*/
DEF_RET1_ARG1 (v4hi)

/*
** v2si_RET1_ARG2:
** addi\s+sp,\s*sp,\s*-16
** sd\s+a0,\s*0\(sp\)
** sd\s+a1,\s*8\(sp\)
** ...
** ld\s+a0,\s*0\(sp\)
** addi\s+sp,\s*sp,\s*16
** jr\s+ra
*/
DEF_RET1_ARG2 (v2si)

/*
** v1di_RET1_ARG3:
** addi\s+sp,\s*sp,\s*-32
** sd\s+a0,\s*8\(sp\)
** sd\s+a1,\s*16\(sp\)
** sd\s+a2,\s*24\(sp\)
** ...
** ld\s+a0,\s*8\(sp\)
** addi\s+sp,\s*sp,\s*32
** jr\s+ra
*/
DEF_RET1_ARG3 (v1di)
