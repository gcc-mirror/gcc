/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int f (int j)
{
  int a [10];
  return a [j]; /* { dg-warning "a|a\\\[j\\\]. is used uninitialized" } */
}
int g (int j)
{
  int a [10];
  return a [j+1]; /* { dg-warning "a|a\\\[<unknown>\\\]. is used uninitialized" } */
}
