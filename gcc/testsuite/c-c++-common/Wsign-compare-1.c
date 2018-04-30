/* PR c/81417 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int
fn1 (signed int a, unsigned int b)
{
  return a < b; /* { dg-warning "comparison of integer expressions of different signedness: 'int' and 'unsigned int'" } */
}

int
fn2 (signed int a, unsigned int b)
{
  return b < a; /* { dg-warning "comparison of integer expressions of different signedness: 'unsigned int' and 'int'" } */
}

int
fn3 (signed long int a, unsigned long int b)
{
  return b < a; /* { dg-warning "comparison of integer expressions of different signedness: 'long unsigned int' and 'long int'" } */
}

int
fn4 (signed short int a, unsigned int b)
{
  return b < a; /* { dg-warning "comparison of integer expressions of different signedness: 'unsigned int' and 'short int'" } */
}

int
fn5 (unsigned int a, signed int b)
{
  return a < b; /* { dg-warning "comparison of integer expressions of different signedness: 'unsigned int' and 'int'" } */
}
