// P1091R3
// { dg-do run { target c++11 } }
// { dg-options "" }
// { dg-require-effective-target tls }
// { dg-add-options tls }

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

extern "C" void abort ();
B t = { 4, 5, 6 };

thread_local auto [ d, e, f ] = t;	// { dg-warning "structured binding declaration can be 'thread_local' only" "" { target c++17_down } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
thread_local auto [ g, h ] = A (44);	// { dg-warning "structured binding declaration can be 'thread_local' only" "" { target c++17_down } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1  }

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
  thread_local auto [ m, n, o ] = t;	// { dg-warning "structured binding declaration can be 'thread_local' only" "" { target c++17_down } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1  }
  thread_local auto [ p, q ] = A (45);	// { dg-warning "structured binding declaration can be 'thread_local' only" "" { target c++17_down } }
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
  int *a[5];
  for (int i = 0; i < 5; ++i)
    a[i] = &baz (i);
  for (int i = 0; i < 5; ++i)
    for (int j = i + 1; j < 5; ++j)
      if (a[i] == a[j] && (j != i + 1 || i != 3))
	abort ();
  if (a[1] != a[0] + 1 || a[2] != a[0] + 2 || a[4] != a[3])
    abort ();
  int b[] = { 4, 5, 6, 45, 45 + 6 };
  for (int i = 0; i < 5; ++i)
    for (int j = 0; j < 3 + i; ++j)
      if (qux (i) != b[i] + 1 + j)
	abort ();
}
