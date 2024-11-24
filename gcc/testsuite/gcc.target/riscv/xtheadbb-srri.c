/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

unsigned long
foo1 (unsigned long rs1)
{
    unsigned sz = sizeof(unsigned long) * 8;
    unsigned shamt = sz - 11;
    return (rs1 << shamt) | (rs1 >> (sz - shamt));
}

unsigned int
foo2 (unsigned int rs1)
{
    unsigned sz = sizeof(unsigned int) * 8;
    unsigned shamt = sz - 11;
    return (rs1 << shamt) | (rs1 >> (sz - shamt));
}

/* { dg-final { scan-assembler-times "th.srri\t" 2 { target { rv32 } } } } */

/* { dg-final { scan-assembler-times "th.srri\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.srriw\t" 1 { target { rv64 } } } } */
