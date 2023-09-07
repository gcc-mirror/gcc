/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zknh -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

unsigned int foo1(unsigned int rs1)
{
    return __builtin_riscv_sha256sig0(rs1);
}

unsigned int foo2(unsigned int rs1)
{
    return __builtin_riscv_sha256sig1(rs1);
}

unsigned int foo3(unsigned int rs1)
{
    return __builtin_riscv_sha256sum0(rs1);
}

unsigned int foo4(unsigned int rs1)
{
    return __builtin_riscv_sha256sum1(rs1);
}

/* { dg-final { scan-assembler-times "sha256sig0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig1" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum1" 1 } } */
