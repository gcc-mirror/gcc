/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32if3d -mabi=ilp32 -misa-spec=2.2" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_f3p0_d2p0\"" } } */
