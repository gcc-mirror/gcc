/* PR rtl-optimization/78527 */

unsigned a;
short b, e;
int *c;
char d;

int
main ()
{
  int f = 80;
  for (;;) {
    if (f > 432)
      *c = a;
    while (b)
      if (d)
        e = -(a >> f);
    c = &f;
    b = e;
  }
}
