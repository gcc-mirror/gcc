/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mno-strict-align" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 1" } } */
