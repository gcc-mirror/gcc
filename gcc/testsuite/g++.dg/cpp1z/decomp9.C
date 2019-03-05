// { dg-do run { target c++17 } }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

struct B {
  int i;
};
template <int I> int& get(B&& b) { return b.i; }
template <int I> int& get(B& b) { return b.i; }

template<> struct std::tuple_size<B> { static const int value = 2; };
template<int I> struct std::tuple_element<I,B> { using type = int; };

int main()
{
  {
    A a = { 42 };
    auto& [ x, y ] = a;
    assert (&x == &y && &x == &a.i && x == 42);

    auto [ x2, y2 ] = a;
    assert (&x2 == &y2 && &x2 != &a.i && x2 == 42);
  }

  {
    B b = { 42 };
    auto& [ x, y ] = b;
    assert (&x == &y && &x == &b.i && x == 42);

    auto [ x2, y2 ] = b;
    assert (&x2 == &y2 && &x2 != &b.i && x2 == 42);
  }
}
