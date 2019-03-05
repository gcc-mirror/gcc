/* Test minimum invalid LARL offset.  */
/* { dg-do compile } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void huge()
{
  b = a + 0x80000000ULL;
  /* { dg-final { scan-assembler-not {(?n)\n\tlarl\t%r\d+,a} } } */
}
