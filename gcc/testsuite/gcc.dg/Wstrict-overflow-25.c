/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=3" } */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (int x, int y)
{
  return x - y < 0; /* { dg-warning "assuming signed overflow does not occur" "correct warning" { xfail *-*-* } } */
}
