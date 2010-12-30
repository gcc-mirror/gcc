/* { dg-options "-O2 -fgraphite-identity -fprofile-generate" } */

int
extend_options (int h, int map, int x, int y, int dx)
{
  while (dx--)
    {
      if (x != dx && y != -x)
	map++;
    }
  return map;
}
