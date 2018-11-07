/* Test that we do not generate useless LAs.  */

/* { dg-do compile } */
/* { dg-options "-march=z10 -O1" } */

int a;

void b()
{
  a /= 100;
  /* { dg-final { scan-assembler-not {(?n)\n\tla\t%r\d+,.+\(%r13\)\n} } } */
}
