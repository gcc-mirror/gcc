/* PR debug/53174 */
/* { dg-do compile } */
/* { dg-options "-Ofast -g" } */

int w, h;

void
bar (float (*x)[4], int y, int z)
{
  int a, b, c, d, e, f, g;

  a = 2;
  b = 2;
  c = 274;
  d = 274;
  if (!z)
    a = 12;
  if (!y)
    b = 12;
  if (z + 266 >= h - 2)
    c = 8 + h - z;
  if (y + 266 >= w - 2)
    d = 8 + w - y;
  for (e = a; e < c; e++)
    for (f = b, g = e * 276 + f; f < d; f++, g++)
      {
	float (*h)[4] = x + (g - 277);
	float k = (*h)[0];
	float l = (*h)[1];
	float m = (*h)[2];
	h++;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h++;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h += 274;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h += 2;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h += 274;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h++;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	h++;
	k += (*h)[0];
	l += (*h)[1];
	m += (*h)[2];
	k *= 0.125f;
	l *= 0.125f;
	m *= 0.125f;
	k = k + (x[g][1] - l);
	m = m + (x[g][1] - l);
	x[g][0] = k;
	x[g][2] = m;
      }
}
