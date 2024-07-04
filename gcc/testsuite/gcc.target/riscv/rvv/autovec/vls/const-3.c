/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_CONST (_Float16, 0, 2)
DEF_CONST (_Float16, 0, 4)
DEF_CONST (_Float16, 0, 8)
DEF_CONST (_Float16, 0, 16)
DEF_CONST (_Float16, 0, 32)
DEF_CONST (_Float16, 0, 64)
DEF_CONST (_Float16, 0, 128)
DEF_CONST (_Float16, 0, 256)
DEF_CONST (_Float16, 0, 512)
DEF_CONST (_Float16, 0, 1024)
DEF_CONST (_Float16, 0, 2048)

DEF_CONST (float, 0, 2)
DEF_CONST (float, 0, 4)
DEF_CONST (float, 0, 8)
DEF_CONST (float, 0, 16)
DEF_CONST (float, 0, 32)
DEF_CONST (float, 0, 64)
DEF_CONST (float, 0, 128)
DEF_CONST (float, 0, 256)
DEF_CONST (float, 0, 512)
DEF_CONST (float, 0, 1024)

DEF_CONST (double, 0, 2)
DEF_CONST (double, 0, 4)
DEF_CONST (double, 0, 8)
DEF_CONST (double, 0, 16)
DEF_CONST (double, 0, 32)
DEF_CONST (double, 0, 64)
DEF_CONST (double, 0, 128)
DEF_CONST (double, 0, 256)
DEF_CONST (double, 0, 512)

/* { dg-final { scan-assembler-times {vmv\.v\.i\s+v[0-9]+,\s*0} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
