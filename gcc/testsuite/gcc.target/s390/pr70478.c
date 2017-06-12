/* { dg-do compile } */
/* { dg-options "-w -O3 -march=z9-109" } */
/* { dg-final { scan-assembler-not "stg" } } */

void foo(unsigned int *a, unsigned char *b)
{
  *a &= *b;
}
