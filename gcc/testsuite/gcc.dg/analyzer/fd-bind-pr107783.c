/* { dg-additional-options "-fpermissive" } */

int
foo (void)
{
  return bind (0, 0, 0); /* { dg-warning "implicit declaration of function 'bind'" } */
}
