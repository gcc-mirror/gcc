/* { dg-do compile } */
/* { dg-require-effective-target cv_bi } */
/* { dg-options "-march=rv32i_xcvbi -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } { "" } } */

/* __builtin_expect is used to provide the compiler with
   branch prediction information and to direct the compiler
   to the expected flow through the code.  */

int
foo1(int a, int x, int y)
{
    a = __builtin_expect(a, -16);
    return a == -16 ? x : y;
}

int
foo2(int a, int x, int y)
{
    a = __builtin_expect(a, 0);
    return a == 0 ? x : y;
}

int
foo3(int a, int x, int y)
{
    a = __builtin_expect(a, 15);
    return a == 15 ? x : y;
}

int
foo4(int a, int x, int y)
{
    a = __builtin_expect(a, -17);
    return a == -17 ? x : y;
}

int
foo5(int a, int x, int y)
{
    a = __builtin_expect(a, 16);
    return a == 16 ? x : y;
}

/* { dg-final { scan-assembler-times "cv\\.bneimm\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),-16,\(\?\:.L\[0-9\]\)" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.bneimm\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0,\(\?\:.L\[0-9\]\)" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.bneimm\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),15,\(\?\:.L\[0-9\]\)" 1 } } */
/* { dg-final { scan-assembler-times "bne\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:.L\[0-9\]+\)" 2 } } */
