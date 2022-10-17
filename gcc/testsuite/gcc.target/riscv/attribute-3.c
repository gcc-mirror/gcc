/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mpreferred-stack-boundary=8" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute stack_align, 256" } } */
