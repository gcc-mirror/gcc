/* Test for non-lvalue arrays: test that they cannot be assigned to
   array variables.  PR 58235.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct s { char c[1]; } x;
struct s f (void) { return x; }

void
g (void)
{
  char c[1];
  c = f ().c; /* { dg-error "array" } */
}

void
h (void)
{
  char c[1] = f ().c; /* { dg-error "array" } */
}
