// PR c++/108474
// { dg-do link { target c++17 } }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template <> struct std::tuple_size <A> { static const int value = 2; };
template <int I> struct std::tuple_element <I, A> { using type = int; };

struct A a;
auto [i, j] = a;
int &r = i;
int s = i;
int *t = &i;

void
foo (int **p, int *q)
{
  static int &u = i;
  static int v = i;
  static int *w = &i;
  int &x = i;
  int y = i;
  int *z = &i;
  *p = &i;
  *q = i;
}

int
main ()
{
}
