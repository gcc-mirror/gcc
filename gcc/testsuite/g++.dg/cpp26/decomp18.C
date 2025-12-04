// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do run { target c++11 } }
// { dg-options "" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

struct S { int a, b, c; };
namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}
struct T {
  int a[3];
  template <int I>
  int &get () { return a[2 - I]; }
};
template <>
struct std::tuple_size<T> { static constexpr int value = 3; };
template <int N>
struct std::tuple_element<N, T> { typedef int type; };

template <int N>
inline int
foo ()
{
  static int a[4] = { N, N + 1, N + 2, N + 3 };
  static auto [...aa] = a;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
  aa...[1]++;				// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  return (... + aa);			// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

template <int N>
inline int
bar ()
{
  static S s = { N, N + 1, N + 2 };
  static auto [...sa] = s;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
  sa...[1]++;				// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  return (sa + ...);			// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

template <int N>
inline int
baz ()
{
  static T t = { { N, N + 1, N + 2 } };
  static auto [ta, ...tb, tc, td] = t;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
  tc++;
  return ((ta + tc + td) + ... + tb);	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

template <int N>
inline int
qux ()
{
  thread_local int a[4] = { N, N + 1, N + 2, N + 3 };
  thread_local auto [...aa] = a;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-2 }
  aa...[1]++;				// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  return (... + aa);			// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

template <int N>
inline int
freddy ()
{
  thread_local S s = { N, N + 1, N + 2 };
  thread_local auto [...sa] = s;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-2 }
  sa...[1]++;				// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  return (sa + ...);			// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

template <int N>
inline int
corge ()
{
  thread_local T t = { { N, N + 1, N + 2 } };
  thread_local auto [ta, ...tb, tc, td] = t;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-2 }
  tc++;
  return ((ta + tc + td) + ... + tb);	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
}

int
main ()
{
  if (foo <2> () != 15 || foo <2> () != 16 || foo <2> () != 17
      || foo <42> () != 175 || foo <42> () != 176
      || bar <5> () != 19 || bar <5> () != 20 || bar <5> () != 21
      || bar <18> () != 58 || bar <18> () != 59
      || baz <3> () != 13 || baz <3> () != 14 || baz <3> () != 15
      || baz <22> () != 70 || baz <22> () != 71)
    __builtin_abort ();
  if (qux <2> () != 15 || qux <2> () != 16 || qux <2> () != 17
      || qux <42> () != 175 || qux <42> () != 176
      || freddy <5> () != 19 || freddy <5> () != 20 || freddy <5> () != 21
      || freddy <18> () != 58 || freddy <18> () != 59
      || corge <3> () != 13 || corge <3> () != 14 || corge <3> () != 15
      || corge <22> () != 70 || corge <22> () != 71)
    __builtin_abort ();
}
