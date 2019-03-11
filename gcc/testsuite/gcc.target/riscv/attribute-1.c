/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch" } } */
