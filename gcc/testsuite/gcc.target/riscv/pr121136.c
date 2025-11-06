/* { dg-do compile } */
/* { dg-options "-std=gnu23 -O2 -march=rv64gc -mabi=lp64d" { target rv64} } */
/* { dg-options "-std=gnu23 -O2 -march=rv32gc -mabi=ilp32" { target rv32} } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define ONE 1U
#define TYPE unsigned int
#define CTZ __builtin_ctz
#else
#define ONE 1UL
#define TYPE unsigned long
#define CTZ __builtin_ctzl
#endif

#define F1(C) _Bool func1_##C(TYPE x) { return x <= C; }
#define F2(C) _Bool func2_##C(TYPE x) { return ((x >> CTZ (C+ONE)) == 0); }
#define F3(C) _Bool func3_##C(TYPE x) { return ((x / (C+ONE)) == 0); }

#define F(C) F1(C) F2(C) F3(C)

F (0x1U)
F (0x3U)
F (0x7U)
F (0xfU)
F (0x1fU)
F (0x3fU)
F (0x7fU)
F (0xffU)
F (0x1ffU)
F (0x3ffU)
F (0x7ffU)
F (0xfffU)
F (0x1fffU)
F (0x3fffU)
F (0x7fffU)
F (0xffffU)
F (0x1ffffU)
F (0x3ffffU)
F (0x7ffffU)
F (0xfffffU)
F (0x1fffffU)
F (0x3fffffU)
F (0x7fffffU)
F (0xffffffU)
F (0x1ffffffU)
F (0x3ffffffU)
F (0x7ffffffU)
F (0xfffffffU)
F (0x1fffffffU)
F (0x3fffffffU)
F (0x7fffffffU)
#if __riscv_xlen == 64
F (0xffffffffUL)
F (0x1ffffffffUL)
F (0x3ffffffffUL)
F (0x7ffffffffUL)
F (0xfffffffffUL)
F (0x1fffffffffUL)
F (0x3fffffffffUL)
F (0x7fffffffffUL)
F (0xffffffffffUL)
F (0x1ffffffffffUL)
F (0x3ffffffffffUL)
F (0x7ffffffffffUL)
F (0xfffffffffffUL)
F (0x1fffffffffffUL)
F (0x3fffffffffffUL)
F (0x7fffffffffffUL)
F (0xffffffffffffUL)
F (0x1ffffffffffffUL)
F (0x3ffffffffffffUL)
F (0x7ffffffffffffUL)
F (0xfffffffffffffUL)
F (0x1fffffffffffffUL)
F (0x3fffffffffffffUL)
F (0x7fffffffffffffUL)
F (0xffffffffffffffUL)
F (0x1ffffffffffffffUL)
F (0x3ffffffffffffffUL)
F (0x7ffffffffffffffUL)
F (0xfffffffffffffffUL)
F (0x1fffffffffffffffUL)
F (0x3fffffffffffffffUL)
F (0x7fffffffffffffffUL)
#endif

/* These represent current state.  They are not optimal as some of the cases
   where we shift are better implemented by loading 2^n constant and using
   sltu as the lui has no incoming data dependencies.  */
/* { dg-final { scan-assembler-times "\tsltiu" 30 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tnot" 3 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tsrli" 121 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tsltu" 38 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tseqz" 118 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tli" 38 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "\tsltiu" 30 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tnot" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tsrli" 25 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tsltu" 38 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tseqz" 22 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tli" 38 { target { rv32 } } } } */
