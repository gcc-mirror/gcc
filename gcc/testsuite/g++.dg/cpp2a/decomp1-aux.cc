// P1091R3
// { dg-do compile { target { c++17 && c++14_down } } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  A(int x) : i(x) {}
  template <int I> int& get() { return i; }
};
struct B { int a, b, c; };

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

B s = { 1, 2, 3 };

#line 1
static auto [ d, e, f ] = s;
static auto [ g, h ] = A (42);

int &
foo (int x)
{
  switch (x)
    {
    case 0: return d;
    case 1: return e;
    case 2: return f;
    case 3: return g;
    default: return h;
    }
}

int
bar (int x)
{
#line 3
  static auto [ m, n, o ] = s;
  static auto [ p, q ] = A (43);
  switch (x)
    {
    case 0: return ++m;
    case 1: return ++n;
    case 2: return ++o;
    case 3: return ++p;
    default: return ++q;
    }
}
