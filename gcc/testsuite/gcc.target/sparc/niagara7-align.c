/* { dg-do compile } */
/* { dg-options "-falign-functions -mtune=niagara7" } */
/* { dg-final { scan-assembler "\.align 64" } } */
void foo(void) {}
