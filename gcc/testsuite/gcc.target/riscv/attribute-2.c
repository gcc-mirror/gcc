/* { dg-do compile } */
/* { dg-options "-O -mno-riscv-attribute" } */
int foo()
{
}
/* { dg-final { scan-assembler-not ".attribute arch" } } */
