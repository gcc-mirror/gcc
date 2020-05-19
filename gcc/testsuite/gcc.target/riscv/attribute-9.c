/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32i2p0sabc_xbar -mabi=ilp32e" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_sabc2p0_xbar2p0\"" } } */
