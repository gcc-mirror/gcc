/* Test maximum invalid LARL offset.  */
/* { dg-do compile } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void negative_huge()
{
  b = a - 0x80000004ULL;
  /* { dg-final { scan-assembler-not {(?n)\n\tlarl\t%r\d+,a} } } */
}
