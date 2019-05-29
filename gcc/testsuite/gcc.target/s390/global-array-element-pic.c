/* Test accesses to global array elements in PIC code.  */
/* { dg-do compile } */
/* { dg-options "-O1 -march=z900 -fPIC" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void c()
{
  b = a + 4;
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t%r\d+,a@GOTENT\n} } } */
  /* { dg-final { scan-assembler-not {(?n)\n\tlarl\t%r\d+,a[^@]} } } */
}
