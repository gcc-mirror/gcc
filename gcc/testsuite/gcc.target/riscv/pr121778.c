/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcb -mabi=lp64d" { target rv64} } */
/* { dg-options "-O2 -march=rv32gcb -mabi=ilp32" { target rv32} } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define ONE 1U
#define TYPE unsigned int
#else
#define ONE 1UL
#define TYPE unsigned long
#endif

#define F1(C) TYPE test_01##C (TYPE a) { return (a << (__riscv_xlen - C)) | ((a >> C) ^ 1); }
#define F2(C) TYPE test_02##C (TYPE a) { return ((a >> (__riscv_xlen - C)) ^ 1) | (a << C); }
#define F3(C) TYPE test_03##C (TYPE a) { return ((a << (__riscv_xlen - C)) ^ (ONE << (__riscv_xlen - 1))) | (a >> C); }
#define F4(C) TYPE test_04##C (TYPE a) { return (a >> (__riscv_xlen - C)) | ((a << C) ^ (ONE << (__riscv_xlen - 1))); }

#define F(C) F1(C) F2(C) F3(C) F4(C)


F (1)
F (2)
F (3)
F (4)
F (5)
F (6)
F (7)
F (8)
F (9)
F (10)
F (11)
F (12)
F (13)
F (14)
F (15)
F (16)
F (17)
F (18)
F (19)
F (20)
F (21)
F (22)
F (23)
F (24)
F (25)
F (26)
F (27)
F (28)
F (29)
F (30)
F (31)
#if __riscv_xlen == 64
F (32)
F (33)
F (34)
F (35)
F (36)
F (37)
F (38)
F (39)
F (40)
F (41)
F (42)
F (43)
F (44)
F (45)
F (46)
F (47)
F (48)
F (49)
F (50)
F (51)
F (52)
F (53)
F (54)
F (55)
F (56)
F (57)
F (58)
F (59)
F (60)
F (61)
F (62)
F (63)

/* { dg-final { scan-assembler-times "\trori" 252 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\txori" 126 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\tbinv" 126 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "\trori" 124 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\txori" 62 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\tbinv" 62 { target { rv32 } } } } */
#endif
