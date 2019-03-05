/* Test the minimum LARL offset.  */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void almost_negative_huge()
{
  b = a - 0x80000000ULL;
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t%r\d+,a-2147483648\n} } } */
}
