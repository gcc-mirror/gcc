/* { dg-do compile } */
/* { dg-options "-O" } */

int
foo (unsigned int x, int y)
{
  return x <= (((y != y) < 0) ? y < 1 : 0);
}
