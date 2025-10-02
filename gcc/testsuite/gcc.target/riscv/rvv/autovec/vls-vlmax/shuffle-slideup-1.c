/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-O3 -march=rv64gcv -mrvv-max-lmul=m8 -Wno-overflow" { target { rv64 } } } */
/* { dg-options "-O3 -march=rv32gcv -mrvv-max-lmul=m8 -Wno-overflow" { target { rv32 } } } */

#define MASK4_4(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 4 - C, Y)
#define MASK4_8(X, Y, A, B, C) MASK4_4(X, Y, A, B, C), SERIES_4 (X + 4, Y)
#define MASK4_16(X, Y, A, B, C) MASK4_8(X, Y, A, B, C), SERIES_8 (X + 8, Y) 
#define MASK4_32(X, Y, A, B, C) MASK4_16(X, Y, A, B, C), SERIES_16 (X + 16, Y) 
#define MASK4_64(X, Y, A, B, C) MASK4_32(X, Y, A, B, C), SERIES_32 (X + 32, Y) 
#define MASK4_128(X, Y, A, B, C) MASK4_64(X, Y, A, B, C), SERIES_64 (X + 64, Y) 

#define MASK8_8(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 8 - C, Y)
#define MASK8_16(X, Y, A, B, C) MASK8_8(X, Y, A, B, C), SERIES_8 (X + 8, Y) 
#define MASK8_32(X, Y, A, B, C) MASK8_16(X, Y, A, B, C), SERIES_16 (X + 16, Y) 
#define MASK8_64(X, Y, A, B, C) MASK8_32(X, Y, A, B, C), SERIES_32 (X + 32, Y) 
#define MASK8_128(X, Y, A, B, C) MASK8_64(X, Y, A, B, C), SERIES_64 (X + 64, Y) 

#define MASK16_16(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 16 - C, Y)
#define MASK16_32(X, Y, A, B, C) MASK16_16(X, Y, A, B, C), SERIES_16 (X + 16, Y) 
#define MASK16_64(X, Y, A, B, C) MASK16_32(X, Y, A, B, C), SERIES_32 (X + 32, Y) 
#define MASK16_128(X, Y, A, B, C) MASK16_64(X, Y, A, B, C), SERIES_64 (X + 64, Y) 

#define MASK32_32(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 32 - C, Y)
#define MASK32_64(X, Y, A, B, C) MASK32_32(X, Y, A, B, C), SERIES_32 (X + 32, Y) 
#define MASK32_128(X, Y, A, B, C) MASK32_64(X, Y, A, B, C), SERIES_64 (X + 64, Y) 

#define MASK64_64(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 64 - C, Y)
#define MASK64_128(X, Y, A, B, C) MASK64_64(X, Y, A, B, C), SERIES_64 (X + 64, Y) 

#define MASK128_128(X, Y, A, B, C) SERIES_##A (X, Y), SERIES_##B (X + Y,  Y), SERIES_##C (X + 128 - C, Y)

#include "shuffle-slideup-perm.h"

/* { dg-final { scan-assembler-times "vslideup" 490 } } */
/* { dg-final { scan-assembler-not "vslidedown" } } */
/* { dg-final { scan-assembler-not "vrgather" } } */
/* { dg-final { scan-assembler-not "vmerge" } } */
