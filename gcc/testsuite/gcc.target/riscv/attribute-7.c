/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32e1p9 -mabi=ilp32e" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32e1p9\"" } } */
