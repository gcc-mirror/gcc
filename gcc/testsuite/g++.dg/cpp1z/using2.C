// Test for P0195R2 variadic using.
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { void f(); };
struct B { void f(int); };

template <class... Bases> struct C: Bases...
{
  using Bases::f...; // { dg-warning "pack expansion" "" { target c++14_down } }
};

int main()
{
  C<A,B> c;
  c.f();
  c.f(42);
}

