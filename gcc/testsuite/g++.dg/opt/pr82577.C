// { dg-additional-options "-O2" }
// PR c++/82577 ICE when optimizing

class a {
public:
  int *b();
};
struct c {
  int d;
  a e;
} f;
void fn1(register c *g) {
  register int *h;
  do
    (h) = g->e.b() + (g)->d;
  while (&f);
}
