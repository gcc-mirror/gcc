/* Test diagnostics for sizeof on void and function types.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

extern const void v;
void f(void);

void
g (void)
{
  sizeof (v); /* { dg-error "error: invalid application of 'sizeof' to a void type" } */
  sizeof (void); /* { dg-error "error: invalid application of 'sizeof' to a void type" } */
  sizeof (f); /* { dg-error "error: invalid application of 'sizeof' to a function type" } */
  sizeof (void (void)); /* { dg-error "error: invalid application of 'sizeof' to a function type" } */
}
