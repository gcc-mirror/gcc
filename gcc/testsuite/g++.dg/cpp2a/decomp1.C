// P1091R3
// { dg-do run { target c++11 } }
// { dg-options "" }
// { dg-additional-sources decomp1-aux.cc }

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

extern int &foo (int);
extern int bar (int);
extern B s;
extern "C" void abort ();
B t = { 4, 5, 6 };

static auto [ d, e, f ] = t;	// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
static auto [ g, h ] = A (44);	// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1  }
// The following warnings are in decomp1-aux.cc with #line directive.
// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } 1 }
// { dg-warning "structured bindings only available with" "" { target c++14_down } 1 }
// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } 2 }
// { dg-warning "structured bindings only available with" "" { target c++14_down } 2 }
// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } 3 }
// { dg-warning "structured bindings only available with" "" { target c++14_down } 3 }
// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } 4 }
// { dg-warning "structured bindings only available with" "" { target c++14_down } 4 }

int &
baz (int x)
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
qux (int x)
{
  static auto [ m, n, o ] = t;	// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1  }
  static auto [ p, q ] = A (45);	// { dg-warning "structured binding declaration can be 'static' only" "" { target c++17_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1  }
  switch (x)
    {
    case 0: return ++m;
    case 1: return ++n;
    case 2: return ++o;
    case 3: return ++p;
    default: return ++q;
    }
}

int
main ()
{
  int *a[10];
  for (int i = 0; i < 5; ++i)
    {
      a[i] = &foo (i);
      a[i + 5] = &baz (i);
    }
  for (int i = 0; i < 10; ++i)
    for (int j = i + 1; j < 10; ++j)
      if (a[i] == a[j] && (j != i + 1 || (i % 5) != 3))
	abort ();
  if (a[1] != a[0] + 1 || a[2] != a[0] + 2 || a[4] != a[3]
      || a[6] != a[5] + 1 || a[7] != a[5] + 2 || a[9] != a[8])
    abort ();
  int b[] = { 1, 2, 3, 43, 43 + 6, 4, 5, 6, 45, 45 + 11 };
  for (int i = 0; i < 10; ++i)
    for (int j = 0; j < 3 + i; ++j)
      if ((i < 5 ? bar (i) : qux (i - 5)) != b[i] + 1 + j)
	abort ();
}
