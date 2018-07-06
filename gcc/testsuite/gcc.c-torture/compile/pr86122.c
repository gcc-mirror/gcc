/* PR middle-end/86122 */

_Complex int
foo (_Complex int x)
{
  x += __INT_MAX__;
  x += 1;
  return x;
}

_Complex int
bar (_Complex int x)
{
  x += 1;
  x += __INT_MAX__;
  return x;
}
