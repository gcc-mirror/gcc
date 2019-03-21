/* { dg-do compile } */

typedef int a;
void h(a);
void c(a *d, int b)
{
  int e, f, g;
  for (; e; e++)
    for (f = 0; f < 4; f++)
      if (d)
	for (g = e + 1; g; g++)
	  h(d[g]);
}
void i()
{
  a *j;
  int k, l;
  for (; k; k++)
    c(j, l);
}
