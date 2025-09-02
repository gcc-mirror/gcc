/* { dg-do compile } */
/* { dg-options "-march=rv64gcb_zicond -mbranch-cost=3 -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gcb_zicond -mbranch-cost=3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define ONE 1U
#define TYPE int
#else
#define ONE 1ULL
#define TYPE long
#endif

#define T1(N) TYPE test1_##N (TYPE c) { return c < 0 ? -ONE : 0xff; } \
	      TYPE test2_##N (TYPE c) { return c >= 0 ? 0xff : -ONE; } \

T1(0)
T1(1)
T1(2)
T1(3)
T1(4)
T1(5)
T1(6)
T1(7)
T1(8)
T1(9)
T1(10)
T1(11)
T1(12)
T1(13)
T1(14)
T1(15)
T1(16)
T1(17)
T1(18)
T1(19)
T1(20)
T1(21)
T1(22)
T1(23)
T1(24)
T1(25)
T1(26)
T1(27)
T1(28)
T1(29)
T1(30)
T1(31)
#if __riscv_xlen == 64
T1(32)
T1(33)
T1(34)
T1(35)
T1(36)
T1(37)
T1(38)
T1(39)
T1(40)
T1(41)
T1(42)
T1(43)
T1(44)
T1(45)
T1(46)
T1(47)
T1(48)
T1(49)
T1(50)
T1(51)
T1(52)
T1(53)
T1(54)
T1(55)
T1(56)
T1(57)
T1(58)
T1(59)
T1(60)
T1(61)
T1(62)
T1(63)
#endif

/* { dg-final { scan-assembler-times "\\t(srai)" 128 { target rv64 } } } */
/* { dg-final { scan-assembler-times "\\t(ori|bset)" 128 { target rv64 } } } */

/* { dg-final { scan-assembler-times "\\t(srai)" 64 { target rv32 } } } */
/* { dg-final { scan-assembler-times "\\t(ori|bset)" 64 { target rv32 } } } */

