/* PR ipa/97404 */
/* { dg-additional-options "-fno-inline" } */

char a, b;
long c;
short d, e;
long *f = &c;
int g;
char h(signed char i) { return 0; }
static short j(short i, int k) { return i < 0 ? 0 : i >> k; }
void l(void);
void m(void)
{
  e = j(d | 9766, 11);
    *f = e;
}
void l(void)
{
  a = 5 | g;
    b = h(a);
}
int main()
{
  m();
  if (c != 4)
    __builtin_abort();
  return 0;
}
