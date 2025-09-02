/* { dg-do compile } */
/* { dg-options "-march=rv64gcb_zicond -mbranch-cost=3 -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gcb_zicond -mbranch-cost=3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define TYPE int
#else
#define TYPE long
#endif

#define T1(N) TYPE test1_##N (TYPE c) { return c < 0 ? ~0xff : 0; } \
	      TYPE test2_##N (TYPE c) { return c >= 0 ? 0 : ~0xff; } \

T1(0)

/* { dg-final { scan-assembler-times "\\t(srai)" 2 } } */
/* { dg-final { scan-assembler-times "\\t(slli|andi|bclr)" 2 } } */
