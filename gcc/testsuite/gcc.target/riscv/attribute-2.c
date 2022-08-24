/* { dg-do compile } */
/* { dg-options "-mno-riscv-attribute" } */
int foo()
{
}
/* { dg-final { scan-assembler-not ".attribute arch" } } */
