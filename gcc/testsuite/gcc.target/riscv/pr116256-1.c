/* { dg-do compile } */
/* { dg-options "-std=gnu23 -march=rv32gcb -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-std=gnu23 -march=rv64gcb -mabi=lp64d" { target { rv64 } } } */


_Bool f1(long a)
{
    long b = a << 4;
    return b == -128;
}

/* We want to verify that we have generated addi
   rather than li+add.  */
/* { dg-final { scan-assembler-not "add\t" } } */
/* { dg-final { scan-assembler "addi\t" } } */
