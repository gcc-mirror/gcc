/* { dg-do compile } */
/* { dg-additional-options "-std=gnu99" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" } } */


struct S {
  _Bool b0: 1;
  _Bool b1: 1;
  _Bool b2: 1;
  _Bool b3: 1;
  _Bool b4: 1;
  _Bool b5: 1;
  _Bool b6: 1;
  _Bool b7: 1;
  _Bool b8: 1;
  _Bool b9: 1;
  _Bool b10: 1;
  _Bool b11: 1;
  _Bool b12: 1;
  _Bool b13: 1;
  _Bool b14: 1;
  _Bool b15: 1;
  _Bool b16: 1;
  _Bool b17: 1;
  _Bool b18: 1;
  _Bool b19: 1;
  _Bool b20: 1;
  _Bool b21: 1;
  _Bool b22: 1;
  _Bool b23: 1;
  _Bool b24: 1;
  _Bool b25: 1;
  _Bool b26: 1;
  _Bool b27: 1;
  _Bool b28: 1;
  _Bool b29: 1;
  _Bool b30: 1;
  _Bool b31: 1;
  _Bool b32: 1;
  _Bool b33: 1;
  _Bool b34: 1;
  _Bool b35: 1;
  _Bool b36: 1;
  _Bool b37: 1;
  _Bool b38: 1;
  _Bool b39: 1;
  _Bool b40: 1;
  _Bool b41: 1;
  _Bool b42: 1;
  _Bool b43: 1;
  _Bool b44: 1;
  _Bool b45: 1;
  _Bool b46: 1;
  _Bool b47: 1;
  _Bool b48: 1;
  _Bool b49: 1;
  _Bool b50: 1;
  _Bool b51: 1;
  _Bool b52: 1;
  _Bool b53: 1;
  _Bool b54: 1;
  _Bool b55: 1;
  _Bool b56: 1;
  _Bool b57: 1;
  _Bool b58: 1;
  _Bool b59: 1;
  _Bool b60: 1;
  _Bool b61: 1;
  _Bool b62: 1;
  _Bool b63: 1;
};

#define T(N) void fb##N (struct S *s) { s->b##N = !s->b##N; }

T(0)
T(1)
T(2)
T(3)
T(4)
T(5)
T(6)
T(7)
T(8)
T(9)
T(10)
T(11)
T(12)
T(13)
T(14)
T(15)
T(16)
T(17)
T(18)
T(19)
T(20)
T(21)
T(22)
T(23)
T(24)
T(25)
T(26)
T(27)
T(28)
T(29)
T(30)
T(31)
#if __riscv_xlen == 64
T(32)
T(33)
T(34)
T(35)
T(36)
T(37)
T(38)
T(39)
T(40)
T(41)
T(42)
T(43)
T(44)
T(45)
T(46)
T(47)
T(48)
T(49)
T(50)
T(51)
T(52)
T(53)
T(54)
T(55)
T(56)
T(57)
T(58)
T(59)
T(60)
T(61)
T(62)
T(63)
#endif

/* { dg-final { scan-assembler-times "lbu\t" 64 { target rv64 } } } */
/* { dg-final { scan-assembler-times "lbu\t" 32 { target rv32 } } } */

/* { dg-final { scan-assembler-times "xori\t" 64 { target rv64 } } } */
/* { dg-final { scan-assembler-times "xori\t" 32 { target rv32 } } } */


/* { dg-final { scan-assembler-times "sb\t" 64 { target rv64 } } } */
/* { dg-final { scan-assembler-times "sb\t" 32 { target rv32 } } } */
