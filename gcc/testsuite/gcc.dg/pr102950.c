/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error(void);

static signed char a;
static short d(unsigned e) {
  signed char b;
  short c;
  a = b = e;
  if (b)
    return 0;
  if (1 >= e) {
    c = e == 0;
    if (c)
      link_error();
  }
  return 0;
}
int main() { d(a ^ 233); }

