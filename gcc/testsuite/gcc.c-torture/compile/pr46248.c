/* PR rtl-optimization/46248 */

struct S
{
  int s;
};

void
foo (unsigned char *x, int y, struct S *z)
{
  const int l1 = y;
  const int l2 = y + l1;
  const int l3 = y + l2;
  const int l4 = y + l3;
  const int l5 = y + l4;
  const int l6 = y + l5;
  const int l7 = y + l6;
  int i;
  for (i = 0; i < 8; i++)
    {
      int a = x[l3] - x[l4];
      int b = x[l4] - x[l5];
      int c = x[l5] - x[l6];
      int d = (b >= 0 ? b : -b) - (((a >= 0 ? a : -a) + (c >= 0 ? c : -c)) >> 1);
      if (d < z->s * 2)
	{
	  int v = d * (-b > 0 ? 1 : -1);
	  x[l2] += v >> 3;
	  x[l7] -= v >> 3;
	}
    }
}
