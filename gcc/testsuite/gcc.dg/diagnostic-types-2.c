/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */
/* Test we're printing the types, like the good compiler we are.  */

extern void foo2 (int *); /* { dg-message "expected 'int \\*' but argument is of type 'int'" } */
extern void foo3 (int); /* { dg-message "expected 'int' but argument is of type 'int \\*'" } */

int *
fn1 (int *p)
{
  p = 1; /* { dg-error "assignment to 'int \\*' from 'int' makes pointer from integer without a cast" } */
  int *q = 1; /* { dg-error "initialization of 'int \\*' from 'int' makes pointer from integer without a cast" } */
  foo2 (1); /* { dg-error "passing argument 1 of 'foo2' makes pointer from integer without a cast" } */
  return 1; /* { dg-error "returning 'int' from a function with return type 'int \\*' makes pointer from integer without a cast" } */
}

int
fn2 (int i, int *p)
{
  i = p; /* { dg-error "assignment to 'int' from 'int \\*' makes integer from pointer without a cast" } */
  int j = p; /* { dg-error "initialization of 'int' from 'int \\*' makes integer from pointer without a cast" } */
  foo3 (p); /* { dg-error "passing argument 1 of 'foo3' makes integer from pointer without a cast" } */
  return p; /* { dg-error "returning 'int \\*' from a function with return type 'int' makes integer from pointer without a cast" } */
}
