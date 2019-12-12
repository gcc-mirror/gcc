/* Test the maximum possible LARL offset.  */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void almost_huge()
{
  b = a + 0x7ffffffeULL;
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t%r\d+,a\+2147483646\n} } } */
}
