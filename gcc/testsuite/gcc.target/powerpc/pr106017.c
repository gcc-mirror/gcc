/* PR target/106017 */
/* { dg-options "-O1 -mdejagnu-cpu=power10" } */
/* { dg-require-effective-target power10_ok } */

/* Make sure we do not flag any errors on the following test cases.  */

void takeacc(__vector_quad *);
void
foo (void)
{
  __vector_quad arr[4];
  takeacc (arr);
}

unsigned char *
bar (__vector_quad *a)
{
  return (unsigned char *)a;
}
