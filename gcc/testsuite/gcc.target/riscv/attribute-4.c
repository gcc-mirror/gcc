/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mstrict-align" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 0" } } */
