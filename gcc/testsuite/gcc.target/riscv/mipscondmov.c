/* { dg-do compile } */
/* { dg-options "-march=rv32imafd_xmipscmov" { target { rv32 } } } */
/* { dg-options "-march=rv64imafd_xmipscmov -mabi=lp64d" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#define MYTEST(name, mytype) \
mytype test1_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a == b) ? c : d; } \
mytype test2_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a != b) ? c : d; } \
mytype test3_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a > b) ? c : d; } \
mytype test4_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a >= b) ? c : d; } \
mytype test5_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a < b) ? c : d; } \
mytype test6_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a <= b) ? c : d; } \
mytype test7_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a == 1) ? c : d; } \
mytype test8_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a != 1) ? c : d; } \
mytype test9_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a > 1) ? c : d; } \
mytype test10_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a >= 1) ? c : d; } \
mytype test11_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a < 1) ? c : d; } \
mytype test12_ ## name (mytype a, mytype b, mytype c, mytype d) { return (a <= 1) ? c : d; }

MYTEST(1, long)
MYTEST(2, unsigned long)
MYTEST(3, int)
MYTEST(4, unsigned int)
MYTEST(5, short)
MYTEST(6, unsigned short)
MYTEST(7, signed char)
MYTEST(8, unsigned char)

/* { dg-final { scan-assembler-times "mips.ccmov" 96 } } */
