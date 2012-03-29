/* PR tree-optimization/52760 */

struct T { unsigned short a, b, c, d; };

__attribute__((noinline, noclone)) void
foo (int x, struct T *y)
{
  int i;

  for (i = 0; i < x; i++)
    {
      y[i].a = ((0x00ff & y[i].a >> 8) | (0xff00 & y[i].a << 8));
      y[i].b = ((0x00ff & y[i].b >> 8) | (0xff00 & y[i].b << 8));
      y[i].c = ((0x00ff & y[i].c >> 8) | (0xff00 & y[i].c << 8));
      y[i].d = ((0x00ff & y[i].d >> 8) | (0xff00 & y[i].d << 8));
    }
}

int
main ()
{
  struct T t = { 0x0001, 0x0203, 0x0405, 0x0607 };
  foo (1, &t);
  if (t.a != 0x0100 || t.b != 0x0302 || t.c != 0x0504 || t.d != 0x0706)
    __builtin_abort ();
  return 0;
}
