/* PR tree-optimization/64494 */

int a, b;
unsigned char c;

int
main ()
{
  int d;
  a = 0;
  for (d = 0; d < 2; d++)
    {
      a &= (b >= 1);
      c = (204 > (((unsigned char) ~0) >> a)) ? 0 : 204 << a;
      b = 0;
    }
  return 0;
}
