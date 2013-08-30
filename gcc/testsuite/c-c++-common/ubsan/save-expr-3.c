/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -Wall -Werror -O" } */

int x;

int
foo (int i, int u)
{
  return (i << u) << x;
}

int
bar (int i, int u)
{
  return (i >> u) >> x;
}
