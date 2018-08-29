// { dg-do compile }
// { dg-options "-O3 -std=gnu++11" }

typedef int a;
enum b : a;
class c {
public:
  enum { d };
  virtual b e(int *, int, const int *) = 0;
};
class f : c {
  b e(int *, int, const int *);
  b g();
};
b f::e(int *h, int i, const int *j) {
  if (i == d)
    return g();
  for (;;)
    e(h, i, j);
}
int k;
c *l;
void m() { l->e(&k, c::d, nullptr); }
