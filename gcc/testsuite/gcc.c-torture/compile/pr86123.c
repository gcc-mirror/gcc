/* PR middle-end/86123 */

int
foo (_Complex unsigned x, _Complex unsigned y)
{
  _Complex unsigned t1 = x / y;
  int t2 = (t1 != 0);
  return t2;
}

int
bar (_Complex unsigned x, _Complex unsigned y)
{
  _Complex unsigned t1 = x / y;
  int t2 = (t1 == 0);
  return t2;
}
