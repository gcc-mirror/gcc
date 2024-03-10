/* { dg-do compile } */

void
foo(int a, int b, int *c, int *d)
{
   *c = a / b;
   *d = a % b;
}

/* { dg-final { scan-assembler-times "\tdiv" 1 } } */
/* { dg-final { scan-assembler-times "\trem" 1} } */
