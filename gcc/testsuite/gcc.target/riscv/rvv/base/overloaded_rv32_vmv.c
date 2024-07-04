/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh -mabi=ilp32 -O3" } */

#include "overloaded_vmv.h"

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e8,\s*m1,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e8,\s*m1,\s*tu,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*m1,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[ax][0-9]+,\s*e16,\s*m1,\s*tu,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*0,\s*e8,\s*m1,\s*ta,\s*ma} 1 } } */
