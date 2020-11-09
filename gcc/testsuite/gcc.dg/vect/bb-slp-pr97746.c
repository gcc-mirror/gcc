/* { dg-do compile } */

int a, b;
short c;

extern void f (short*);

void d()
{
  short e[2] = {0, 0};
  while (a)
    {
      f(e);
      int g = 0 || a, h = 8 && c;
      short i = c;
      c = h & g;
      if (b)
	b = g || i;
    }
}
