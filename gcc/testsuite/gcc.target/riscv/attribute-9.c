/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32i2p0xbar_sabc_sxfoo -mabi=ilp32e" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_xbar2p0_sabc2p0_sxfoo2p0\"" } } */
