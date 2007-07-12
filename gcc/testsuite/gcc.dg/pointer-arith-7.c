/* Test diagnostics for sizeof on void and function types.  Test with
   -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

extern const void v;
void f(void);

void
g (void)
{
  sizeof (v); /* { dg-warning "invalid application of 'sizeof' to a void type" } */
  sizeof (void); /* { dg-warning "invalid application of 'sizeof' to a void type" } */
  sizeof (f); /* { dg-warning "invalid application of 'sizeof' to a function type" } */
  sizeof (void (void)); /* { dg-warning "invalid application of 'sizeof' to a function type" } */
}
