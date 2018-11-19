/* Test accesses to even global array elements.  */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O1" } */

extern char a[] __attribute__ ((aligned (2)));
extern char *b;

void even()
{
  b = a + 4;
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t%r\d+,a\+4\n} } } */
}
