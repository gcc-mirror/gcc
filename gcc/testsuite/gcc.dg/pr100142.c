/* { dg-do compile } */
/* { dg-options "-fpreprocessed" } */

void
foo (void)
{
  assert (1); /* { dg-warning "implicit" } */
}
