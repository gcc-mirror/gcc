/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh -mabi=ilp32 -O3 -Wno-psabi" } */

#include "overloaded_vreinterpret.h"

/* { dg-final { scan-assembler-times {vsetvli\s+[ax][0-9]+,\s*zero,\s*e8,\s*m4,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[ax][0-9]+,\s*zero,\s*e8,\s*m2,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[ax][0-9]+,\s*zero,\s*e8,\s*mf2,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[ax][0-9]+,\s*zero,\s*e16,\s*mf2,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[ax][0-9]+,\s*zero,\s*e32,\s*mf2,\s*ta,\s*ma} 1 } } */
