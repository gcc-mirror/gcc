/* PR rtl-optimization/63448 */
/* { dg-do compile } */
/* { dg-options "-O -std=c99" } */

int a, d, e, g, h, j;
float b, c, k, l, m, n;
int *__restrict i;
void
foo (void)
{
  int o = e;
  int *p;
  float *q, *r = (float *) 0x1234000;
  float s, t, u, v, w, x;
  do
    {
      for (a = o; a; a--)
	{
	  s += m;
	  t += n;
	  u += m;
	  v += n;
	  w += d;
	  x += d;
	  n = l;
	  s += r[1];
	  t += n;
	  v += r[1];
	  m = k * r[4];
	  n = q[0] * r[4];
	  s += m;
	  m = q[1] * r[4];
	  t += n;
	  q += g;
	  k = *q;
	  n = q[1] * r[4];
	  s += m;
	  t += n;
	  u += r[4];
	  m = q[8] * r[4];
	  q += 1;
	  n = q[1] * r[4];
	  s += m;
	  m = q[4];
	  t += n;
	  q += g;
	  w += m;
	  m = k * r[4];
	  s += m;
	  t += q[0];
	  m = q[1] * r[4];
	  v += q[0];
	  n = q[10] * r[4];
	  s += m;
	  t += n;
	  u += b;
	  m = q[8] * r[4];
	  n = q[2] * r[4];
	  s += m;
	  m = q[4] * r[4];
	  t += n;
	  q++;
	  n = q[2] * r[16];
	  s += m;
	  m = q[4];
	  t += n;
	  s += m;
	  t += r[6];
	  q += g;
	  k = *q;
	  w += m;
	  m = k * r[20];
	  x += r[16];
	  n = q[1] * r[20];
	  s += m;
	  t += n;
	  q += g;
	  k = *q;
	  w += m;
	  m = k * r[2];
	  n = q[1] * r[22];
	  s += m;
	  m = q[4];
	  t += n;
	  q += g;
	  s += m;
	  t += q[0];
	  s += m;
	  u += m;
	  n = q[1] * r[22];
	  s += m;
	  m = q[4] * r[22];
	  t += n;
	  q += g;
	  k = 1;
	  w += m;
	  c = q[10];
	  x += r[22];
	  s += m;
	  t += r[22];
	  u += m;
	  v += r[22];
	  n = q[10] * r[30];
	  d = r[32];
	  l = q[1];
	  b = 0;
	  w += m;
	  m = r[32];
	  x += n;
	  r = 0;
	}
      *i = s;
      p[0] = t;
      p[1] = u;
      p[6] = v;
      p[8] = w;
      p[10] = x;
    }
  while (j);
}
