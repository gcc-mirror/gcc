/* PR 20644 */
/* { dg-do compile } */
/* { dg-options "-O0 -Wuninitialized" } */
int foo ()
{
  int i = 0;
  int j;

  if (1 == i)
    return j; /* { dg-bogus "uninitialized" "uninitialized" { xfail *-*-* } 10 } */

  return 0;
}

int bar ()
{
  int i = 1;
  int j; 

  if (1 == i)
    return j; /* { dg-warning "uninitialized" "uninitialized" { target *-*-* } 21 } */

  return 0;
}
