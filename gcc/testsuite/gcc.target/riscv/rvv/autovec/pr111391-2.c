/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32x_zvl128b -mabi=lp64d -Wno-int-conversion -Wno-implicit-function -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Ofast -ftree-vectorize" } */

#include "pr111391-1.c"

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*2,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 } }
/* { dg-final { scan-assembler-times {vmv\.x\.s} 2 } } */
/* { dg-final { scan-assembler-times {vslidedown.vi\s+v[0-9]+,\s*v[0-9]+,\s*1} 1 } } */
/* { dg-final { scan-assembler-times {slli\s+[a-x0-9]+,[a-x0-9]+,32} 1 } } */
/* { dg-final { scan-assembler-times {or\s+[a-x0-9]+,[a-x0-9]+,[a-x0-9]+} 1 } } */
