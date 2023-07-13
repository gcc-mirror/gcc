/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i2p0xtheadba_xtheadbb -mabi=ilp32" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_xtheadba1p0_xtheadbb1p0\"" } } */
