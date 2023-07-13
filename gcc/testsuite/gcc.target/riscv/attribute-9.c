/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i2p0svinval_xtheadba -mabi=ilp32e" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_svinval1p0_xtheadba1p0\"" } } */
