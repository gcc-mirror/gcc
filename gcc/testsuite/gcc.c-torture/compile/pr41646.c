/* PR rtl-optimization/41646 */

struct A { unsigned long a; };
struct B { unsigned short b, c, d; };
struct B bar (unsigned long);

char *
foo (char *a, struct A *x)
{
  struct B b = bar (x->a);
  unsigned char c;
  unsigned short d;
  a[3] = ((unsigned char) (b.b % 10) + 48);
  d = b.b / 10;
  a[2] = ((unsigned char) (d % 10) + 48);
  d = d / 10;
  a[1] = ((unsigned char) (d % 10) + 48);
  a[0] = ((unsigned char) ((d / 10) % 10) + 48);
  a[4] = 46;
  c = (unsigned char) b.c;
  a[6] = (c % 10 + 48);
  a[5] = ((c / 10) % 10 + 48);
  a[7] = 46;
  c = b.d;
  a[9] = (c % 10 + 48);
  a[8] = ((c / 10) % 10 + 48);
  return a + 10;
}
