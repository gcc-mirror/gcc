/* { dg-do compile } */

enum { a = 1, b };
float *c, *e;
float d, h;
int f, g;
void i()
{
  float j = h;
  for (; g;)
    for (; f; f++)
      {
	c[a] = j * d;
	c[b] = h * d;
	j = 0;
	h = e[2];
      }
}
