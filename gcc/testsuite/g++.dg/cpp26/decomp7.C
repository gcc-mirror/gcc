// P0963R3 - Structured binding declaration as a condition
// { dg-do run { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct S {
  int a, b;
  explicit operator bool () const noexcept { return a == b; }
};

struct T {
  int a, b, c;
  static int d;
  explicit operator bool () const noexcept { d = 42; return a == b; }
  template <int I> int &get () { if (d != 42) __builtin_abort (); return I ? a : b; }
};
int T::d = 0;

template<> struct std::tuple_size<T> { static const int value = 2; };
template<int I> struct std::tuple_element<I,T> { using type = int; };

template <typename S, typename T>
void
foo (T t)
{
  if (auto [ i, j ] = S { 1, 1 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (i != 1 || j != 1)
	__builtin_abort ();
    }
  else
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
  T::d = 0;
  if (int m = 78; auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {					// { dg-warning "init-statement in selection statements only available with" "" { target c++14_down } .-1 }
      if (i != 42 || j != 42 || T::d != 42 || m != 78)
	__builtin_abort ();
    }
  else
    {
      ++i;
      ++j;
      ++m;
      __builtin_abort ();
    }
  if (auto m = 15; auto [ i, j ] = S { -1, 1 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {						// { dg-warning "init-statement in selection statements only available with" "" { target c++14_down } .-1 }
      ++i;
      ++j;
      ++m;
      __builtin_abort ();
    }
  else
    {
      if (i != -1 || j != 1 || m != 15)
	__builtin_abort ();
    }
  t.a = -42;
  T::d = 0;
  if (auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
  else
    {
      if (i != 42 || j != -42 || T::d != 42)
	__builtin_abort ();
    }
}

template <typename S, typename T>
void
bar (T t)
{
  int cnt = 0;
  while (auto [ i, j ] = S { 7, 7 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (i != 7 || j != 7)
	__builtin_abort ();
      if (++cnt == 5)
	break;
    }
  if (cnt != 5)
    __builtin_abort ();
  T::d = 0;
  while (auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (i != 31 || j != 31 || T::d != 42)
	__builtin_abort ();
      T::d = 0;
      if (++cnt == 10)
	break;
    }
  if (cnt != 10)
    __builtin_abort ();
  while (auto [ i, j ] = S { 7, -7 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
  t.a = -31;
  T::d = 0;
  while (auto & [ i, j ] = t)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
}

template <typename S, typename T>
void
baz (T t)
{
  int cntc = 0;
  for (int cnt = 0; auto [ i, j ] = S { 12, 12 }; ++cnt)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (i != 12 || j != 12)
	__builtin_abort ();
      cntc = cnt;
      if (cnt == 5)
	break;
    }
  if (cntc != 5)
    __builtin_abort ();
  T::d = 0;
  for (int cnt = 5; auto & [ i, j ] = t; ++cnt)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      if (i != -15 || j != -15 || T::d != 42)
	__builtin_abort ();
      T::d = 0;
      cntc = cnt;
      if (cnt == 10)
	break;
    }
  if (cntc != 10)
    __builtin_abort ();
  for (int cnt = 0; auto [ i, j ] = S { -27, 27 }; ++cnt)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
  t.a = 15;
  T::d = 0;
  for (int cnt = 0; auto & [ i, j ] = t; ++cnt)	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      ++i;
      ++j;
      __builtin_abort ();
    }
}

int
main ()
{
  foo<S, T> ({ 42, 42, 0 });
  bar<S, T> ({ 31, 31, 7 });
  baz<S, T> ({ -15, -15, 6 });
}
