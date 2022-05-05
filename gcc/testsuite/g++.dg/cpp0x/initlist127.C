// PR c++/104182
// { dg-do run { target c++11 } }

#include <initializer_list>

int b;

struct stringy {
  const char *p;
  stringy(const char *p): p(p) { ++b; }
  const char& operator[](int i) const { return p[i]; }
  ~stringy() { --b; }
};

struct S
{
  int A;
  stringy B;
};

struct veccy {
  S s;
  veccy (const std::initializer_list<S> &l): s(*l.begin()) {}
  const S& operator[](int i) const { return s; }
};

struct V
{
  veccy v;
};

static const V v{ { { { 237, "2" } } } };

int main()
{
  if (v.v[0].A != 237 || v.v[0].B[0] != '2')
    __builtin_abort();
  return 0;
}
