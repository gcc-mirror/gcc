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
    a = __builtin_expect(a, 10);
    return a == 10 ? x : y;
}

/* { dg-final { scan-assembler-times "cv\\.bneimm\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),10,\(\?\:.L\[0-9\]\)" 1 } } */
