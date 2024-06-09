/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_CONST (_Float16, 8.88, 2)
DEF_CONST (_Float16, 8.88, 4)
DEF_CONST (_Float16, 8.88, 8)
DEF_CONST (_Float16, 8.88, 16)
DEF_CONST (_Float16, 8.88, 32)
DEF_CONST (_Float16, 8.88, 64)
DEF_CONST (_Float16, 8.88, 128)
DEF_CONST (_Float16, 8.88, 256)
DEF_CONST (_Float16, 8.88, 512)
DEF_CONST (_Float16, 8.88, 1024)
DEF_CONST (_Float16, 8.88, 2048)

DEF_CONST (float, 8.88, 2)
DEF_CONST (float, 8.88, 4)
DEF_CONST (float, 8.88, 8)
DEF_CONST (float, 8.88, 16)
DEF_CONST (float, 8.88, 32)
DEF_CONST (float, 8.88, 64)
DEF_CONST (float, 8.88, 128)
DEF_CONST (float, 8.88, 256)
DEF_CONST (float, 8.88, 512)
DEF_CONST (float, 8.88, 1024)

DEF_CONST (double, 8.88, 2)
DEF_CONST (double, 8.88, 4)
DEF_CONST (double, 8.88, 8)
DEF_CONST (double, 8.88, 16)
DEF_CONST (double, 8.88, 32)
DEF_CONST (double, 8.88, 64)
DEF_CONST (double, 8.88, 128)
DEF_CONST (double, 8.88, 256)
DEF_CONST (double, 8.88, 512)

/* { dg-final { scan-assembler-times {vfmv\.v\.f\s+v[0-9]+,\s*[a-x0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
