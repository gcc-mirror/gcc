/* { dg-do compile } */

int a, b, c, d = 10, e = 1, f, g, h, i;
int main()
{
  int j = -1;
k:
  h = c;
l:
  c = ~c;
  if (e)
  m:
    a = 0;
  if (j > 1)
    goto m;
  if (!e)
    goto l;
  if (c)
    goto p;
n:
  goto m;
o:
  if (f) {
    if (g)
      goto k;
    j = 0;
  p:
    if (d)
      goto o;
    goto n;
  }
  if (i)
    goto l;
  for (; a < 1; a++)
    while (a > d)
      b++;
  return 0;
}
