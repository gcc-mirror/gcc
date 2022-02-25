/* PR tree-optimization/104675 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

_Complex unsigned int
foo (_Complex unsigned int x)
{
  return (x / 2) * 2;
}

_Complex unsigned int
bar (_Complex unsigned int x)
{
  return (x * 2) / 2;
}

_Complex unsigned int
baz (_Complex unsigned int x)
{
  _Complex unsigned int y = x / 2;
  return y * 2;
}

_Complex unsigned int
qux (_Complex unsigned int x)
{
  _Complex unsigned int y = x * 2;
  return y / 2;
}
