/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -mno-strict-align" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 1" } } */
