// P0963R3 - Structured binding declaration as a condition
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct S {
  S () : s (0) {}
  S (int x) : s (x) {}
  S (const S &x) : s (x.s) {}
  ~S () {}
  int s;
};

struct T {
  S a, b, c;
  ~T () {}
  explicit operator bool () const noexcept { return a.s == b.s; }
  template <int I> S get () { return I ? a : b; }
};

template<> struct std::tuple_size<T> { static const int value = 2; };
template<int I> struct std::tuple_element<I,T> { using type = S; };

void
foo (T t, bool x)
{
  while (auto [ i, j ] = T { 1, 1, 3 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (x)
	break;
    }
}

void
bar (T t, bool x)
{
  for (int cnt = 0; auto [ i, j ] = T { 2, 2, 4 }; ++cnt)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (x)
	break;
    }
}
