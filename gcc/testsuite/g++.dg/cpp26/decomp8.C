// P0963R3 - Structured binding declaration as a condition
// { dg-do run { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct S {
  int a, b;
  operator int () const noexcept { return a * 2; }
};

struct T {
  int a, b, c;
  static int d;
  operator long long () const noexcept { d = 42; return a * 4; }
  template <int I> int &get () { if (d != 42) __builtin_abort (); return I ? a : b; }
};
int T::d = 0;

template<> struct std::tuple_size<T> { static const int value = 2; };
template<int I> struct std::tuple_element<I,T> { using type = int; };

template <typename S, typename T>
void
foo (T t)
{
  switch (auto [ i, j ] = S { 53, 62 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
    case 2 * 53:
      if (i != 53 || j != 62)
	__builtin_abort ();
      break;
    default:
      __builtin_abort ();
    }
  T::d = 0;
  switch (int m = 78; auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-warning "init-statement in selection statements only available with" "" { target c++14_down } .-1 }
    case 4 * 42LL:
      if (i != 43 || j != 42 || T::d != 42 || m != 78)
	__builtin_abort ();
      break;
    default:
      break;
    }
  switch (auto m = 15; auto [ i, j ] = S { -1, 1 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-warning "init-statement in selection statements only available with" "" { target c++14_down } .-1 }
    case 2 * -1:
      if (i != -1 || j != 1 || m != 15)
	__builtin_abort ();
      break;
    default:
      __builtin_abort ();
    }
  t.a = -42;
  T::d = 0;
  switch (auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
    case 4LL * -42:
      if (i != 43 || j != -42 || T::d != 42)
	__builtin_abort ();
      break;
    default:
      __builtin_abort ();
    }
}

int
main ()
{
  foo<S, T> ({ 42, 43, 0 });
}
