// { dg-additional-options "-O2" }
// PR c++/82577 ICE when optimizing

#if __cplusplus > 201500L
// register is no longer a keyword in C++17.
#define reg
#else
#define reg register
#endif

class a {
public:
  int *b();
};
struct c {
  int d;
  a e;
} f;
void fn1(reg c *g) {
  reg int *h;
  do
    (h) = g->e.b() + (g)->d;
  while (&f);
}
