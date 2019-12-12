// P0018R3 - C++17 lambda capture of *this
// { dg-do run { target c++11 } }
// { dg-options "" }

extern "C" void abort ();

struct A {
  int a, z;
  A () : a (4), z (0) {}
  A (const A &x) : a (x.a), z (1) {}
  void foo () {
    if (z != 0) abort ();
    auto b = [this] { return &a; };
    auto c = [*this] { return &a; };	// { dg-warning "'*this' capture only available with" "" { target c++14_down } }
    auto d = [=] { return &a; }; // { dg-warning "implicit capture" "" { target c++2a } }
    auto e = [&] { return &a; };
    if (b () != &a) abort ();
    if (*b () != 4) abort ();
    auto f = c ();
    if (c () == &a) abort ();
    if (c () != f) abort ();
    if (*c () != 4) abort ();
    if (d () != &a) abort ();
    if (e () != &a) abort ();
    auto g = [this] { return a + z; };
    auto h = [*this] { return a + z; };	// { dg-warning "'*this' capture only available with" "" { target c++14_down } }
    auto i = [=] { return a + z; }; // { dg-warning "implicit capture" "" { target c++2a } }
    auto j = [&] { return a + z; };
    if (g () != 4 || h () != 5 || i () != 4 || j () != 4) abort ();
  }
};

template <int N>
struct B {
  int a, z;
  B () : a (N), z (0) {}
  B (const B &x) : a (x.a), z (1) {}
  void foo () {
    if (z != 0) abort ();
    auto b = [this] { return &a; };
    auto c = [*this] { return &a; };	// { dg-warning "'*this' capture only available with" "" { target c++14_down } }
    auto d = [=] { return &a; }; // { dg-warning "implicit capture" "" { target c++2a } }
    auto e = [&] { return &a; };
    if (b () != &a) abort ();
    if (*b () != 9) abort ();
    auto f = c ();
    if (c () == &a) abort ();
    if (c () != f) abort ();
    if (*c () != 9) abort ();
    if (d () != &a) abort ();
    if (e () != &a) abort ();
    auto g = [this] { return a + z; };
    auto h = [*this] { return a + z; };	// { dg-warning "'*this' capture only available with" "" { target c++14_down } }
    auto i = [=] { return a + z; }; // { dg-warning "implicit capture" "" { target c++2a } }
    auto j = [&] { return a + z; };
    if (g () != 9 || h () != 10 || i () != 9 || j () != 9) abort ();
  }
};

int
main ()
{
  A a;
  a.foo ();
  B<9> b;
  b.foo ();
  return 0;
}
