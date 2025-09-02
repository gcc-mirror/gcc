// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct V {
  int i, j;
  template <int I> int &get () { return i; }
};

template<> struct std::tuple_size <V> { static const int value = 5; };
template<int I> struct std::tuple_element <I, V> { using type = int; };

constexpr V c[] = { { 3, 4 }, { 4, 5 }, { 5, 6 }, { 6, 7 }, { 7, 8 } };
struct U {
  constexpr const V *begin () const { return &c[0]; }
  constexpr const V *end () const { return &c[s]; }
  int s;
};
constexpr U u1 = { 3 }, u2 = { 0 };

struct W {
  int w;
  W (int x) : w (x) {}
  ~W () {}
};

struct X {
  V i, j;
  template <int I> V &get () { return j; }
};

template<> struct std::tuple_size <X> { static const int value = 3; };
template<int I> struct std::tuple_element <I, X> { using type = V; };

template <int N>
long long
foo ()
{
  long long r = 0;
  template for (auto h = 2; auto [_, ...i, _] : u1)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      if (sizeof... (i) != 3)				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
	__builtin_abort ();				// { dg-warning "name-independent declarations only available with" "" { target c++23_down } .-3 }
      r += i...[1] + h;					// { dg-warning "pack indexing only available with" "" { target c++23_down } }
    }
  template for (long long h = ++r; auto [...i, j] : u2)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    __builtin_abort ();					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
							// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
  return r;
}

template <int N>
long long
bar ()
{
  long long r = 0;
  template for (W w { 42 }; auto [...i, j] : { V { 42, 10 }, V { 15, 26 }, V { 93, 12 } }) // { dg-warning "'template for' only available with" "" { target c++23_down } }
    {							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      if (sizeof... (i) != 4)				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
	__builtin_abort ();
      r += i...[3] + (w.w == 42);			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
    }
  return r;
}

template <int N>
long long
baz ()
{
  long long r = 0;
  template for (constexpr auto x = 5; auto [ ...j ] : X { { 5, 6 }, { 7, 8 } })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      if (sizeof... (j) != 5)				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } .-2 }
	__builtin_abort ();
      r += j...[4] + j...[0] + (x == 5);		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
    }
  return r;
}

int
main ()
{
  if (foo <0> () != 19)
    __builtin_abort ();
  if (bar <1> () != 153)
    __builtin_abort ();
  if (baz <2> () != 45)
    __builtin_abort ();
}
