/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-vector-bits=scalable -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** v4hf_RET1_ARG1:
** ret
*/
DEF_RET1_ARG1 (v4hf)

/*
** v2sf_RET1_ARG2:
** addi\s+sp,\s*sp,\s*-16
** sd\s+a0,\s*0\(sp\)
** sd\s+a1,\s*8\(sp\)
** ...
** ld\s+a0,\s*0\(sp\)
** addi\s+sp,\s*sp,\s*16
** jr\s+ra
*/
DEF_RET1_ARG2 (v2sf)

/*
** v4sf_RET1_ARG2:
** addi\s+sp,\s*sp,\s*-32
** sd\s+a0,\s*0\(sp\)
** sd\s+a1,\s*8\(sp\)
** sd\s+a2,\s*16\(sp\)
** sd\s+a3,\s*24\(sp\)
** ...
** ld\s+a0,\s*0\(sp\)
** ld\s+a1,\s*8\(sp\)
** addi\s+sp,\s*sp,\s*32
** jr\s+ra
*/
DEF_RET1_ARG2 (v4sf)

/*
** v1df_RET1_ARG3:
** addi\s+sp,\s*sp,\s*-32
** sd\s+a0,\s*8\(sp\)
** sd\s+a1,\s*16\(sp\)
** sd\s+a2,\s*24\(sp\)
** ...
** ld\s+a0,\s*8\(sp\)
** addi\s+sp,\s*sp,\s*32
** jr\s+ra
*/
DEF_RET1_ARG3 (v1df)
