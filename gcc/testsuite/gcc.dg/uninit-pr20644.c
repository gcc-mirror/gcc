/* PR 20644 */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */
int foo ()
{
  int i = 0;
  int j;

  if (1 == i)
    return j;

  return 0;
}

int bar ()
{
  int i = 1;
  int j;

  if (1 == i)
    return j; /* { dg-warning "uninitialized" "uninitialized" { target *-*-* } 18 } */

  return 0;
}
