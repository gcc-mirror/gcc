// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test dependent namespaces in typenames.

using info = decltype(^^void);

namespace N {
  struct A { int i; using Q = int; };
  template<typename T>
  struct B { T t; };
  static int glob;
  using U = int;
  int foo (int i) { return i; }
  template<typename T>
  T baz (T t) { return t; }
  template<typename T>
  constexpr T pi = 3.14;

  namespace M {
    struct C { int i; };
    template<typename T>
    struct D { T t; };
    int mglob;
    using V = int;
    void bar () { }
    template<typename T>
    T qux (T t) { return t; }
    template<typename T>
    constexpr T pi2 = 3.14;
  }
}

template<info R>
void
f ()
{
  typename [:R:]::A a{1};
  typename[:R:]::template B<int> b{1};
  typename [:R:]::A::Q i1;
  typename [:R:]::U u;
  [:R:]::M::bar ();
  [:R:]::M::mglob++;
  typename [:R:]::M::V i2;
  typename [:R:]::M::C c{1};
  typename [:R:]::M::template D<int> d{1};

  // Not a TYPENAME_TYPE, but let's test it.
  [:R:]::template baz<int>(42);
  double d1 = [:R:]::template pi<double>;
  int i3 = [:R:]::foo (42);
  [:R:]::glob++;
  [:R:]::M::template qux<int>(42);
  double d2 = [:R:]::M::template pi2<double>;
}

template<info R>
void
g ()
{
  typename [:R:]::C c{1};
  typename [:R:]::template D<int> d{1};
  [:R:]::mglob++;
  typename [:R:]::V v;
  [:R:]::bar ();
  [:R:]::template qux<int>(42);
  double d2 = [:R:]::template pi2<double>;
}

template<info R>
void
bad ()
{
  typename [:R:]::NOTHERE x; // { dg-error ".NOTHERE. is not a member of .N." }
  typename [:R:]::M::NOTHERE y;  // { dg-error ".NOTHERE. is not a member of .N::M." }
}

template<info R>
struct E {
  typename [:R:]::A a;
  typename[:R:]::template B<int> b;
  typename [:R:]::U u;
  typename [:R:]::M::V v;
  typename [:R:]::M::C c{1};
  typename [:R:]::M::template D<int> d{1};
};

void
doit ()
{
  f<^^N>();
  bad<^^N>();
  E<^^N> e;
  g<^^N::M>();
}
