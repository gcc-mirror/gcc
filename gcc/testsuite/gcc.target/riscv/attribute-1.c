/* { dg-do compile } */
/* { dg-options "-mriscv-attribute" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch" } } */
