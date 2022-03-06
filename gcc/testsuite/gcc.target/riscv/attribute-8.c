/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i2p0xabc_xv5 -mabi=ilp32" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_xabc_xv5p0\"" } } */
