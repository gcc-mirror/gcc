/* { dg-do compile } */
/* { dg-options "-falign-functions -mcpu=niagara7" } */
/* { dg-final { scan-assembler "\.align 64" } } */
void foo(void) {}
