/* Test accesses to odd global array elements.  */
/* { dg-do compile } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void odd()
{
  b = a + 1;
  /* { dg-final { scan-assembler-not {(?n)\n\tlarl\t%r\d+,a} } } */
}
