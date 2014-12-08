/* { dg-do compile } */

int a, b, c, d[1], e, f, g, h;

void
fn1 ()
{
  char i = 0;
  for (b = 0; b >= 0;)
    for (b = 0; b < 2; b++)
      {
	d[f] && (e = 0);
	h = 1 & a - i ? 0 : a;
	c = i = h;
	char j = c;
	int k = 0;
	g = j || j > k;
	d[0] = 0;
      }
}
