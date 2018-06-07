/* { dg-do compile } */

short a, d, e;
unsigned char b;
int c, f;
char g, h;
void fn2(int, int);
void fn1() { fn2(e, a); }
void fn2(int p1, int p2)
{
l1:
  b = a;
  for (; h; h--)
    if (p1)
      g = p2 * c;
    else
      {
	c = d;
	if (f)
	  goto l1;
      }
}
