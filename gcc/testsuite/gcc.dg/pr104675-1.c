/* PR tree-optimization/104675 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

_Complex int
foo (_Complex int a)
{
  return (-1 + -1i) - a;
}

_Complex int
bar (_Complex int a)
{
  return -a - (1 + 1i);
}

_Complex int
baz (_Complex int a)
{
  _Complex int b = -1 + -1i;
  return b - a;
}

_Complex int
qux (_Complex int a)
{
  _Complex int b = 1 + 1i;
  return -a - b;
}
