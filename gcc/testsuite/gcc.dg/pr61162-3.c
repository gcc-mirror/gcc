/* { dg-do compile } */

int
fn4 (int *a)
{
  return a; /* { dg-error "10:returning 'int \\*' from a function with return type 'int' makes integer from pointer without a cast" } */
}

int *
fn5 (int a)
{
  return a; /* { dg-error "10:returning 'int' from a function with return type 'int \\*' makes pointer from integer without a cast" } */
}
