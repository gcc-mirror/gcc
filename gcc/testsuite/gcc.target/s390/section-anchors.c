/* Test basic section anchor functionality.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3 -march=z13" } */

int a = 1, b = 2;

void
f ()
{
  a = 1234;
  b = 5678;
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t(%r\d+),\.LANCHOR\d+\n\tmvhi\t\d+\(\1\),1234\n\tmvhi\t\d+\(\1\),5678} } } */
}
