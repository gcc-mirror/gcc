/* { dg-do compile } */

int zot(int);
struct bar {
  ~bar() { }
};
int x;
void doit(int a, int b, int c)
{
  bar pn;
  int b1 = zot(a) * c;
  int b2 = zot(b) * c;
  x = b1 + b2;
}
