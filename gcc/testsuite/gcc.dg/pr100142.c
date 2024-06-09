/* { dg-do compile } */
/* { dg-options "-fpermissive -fpreprocessed" } */

void
foo (void)
{
  assert (1); /* { dg-warning "implicit" } */
}
