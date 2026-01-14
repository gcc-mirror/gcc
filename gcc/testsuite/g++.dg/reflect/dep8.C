// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

struct S {
  int i;
  static int si;
  static int mfn () { return 1; }
};

int S::si;

template<typename T>
struct C {
  int i;
  static T si;
  static T mfn() { return 1; }
};

template<info R>
void
f ()
{
  typename [:R:] r{1};
  ++[:R:]::si;
  [:R:]::si += [:R:]::mfn ();
  using L = [:R:];
  L r2{2};
}

template<info R>
struct A {
  typename [:R:] r;
  int i = [:R:]::mfn ();
  using L = [:R:];
  L r2;
  int mem () { return [:R:]::si; }
};

template<info R>
void
f2 ()
{
  typename [:R:]<int> r{1};
  ++template [:R:]<int>::si;
  template [:R:]<int>::si += template [:R:]<int>::mfn ();
  using L = [:R:]<int>;
  L r2{2};
}

template<info R>
struct A2 {
  typename [:R:]<int> r;
  int i = template [:R:]<int>::mfn ();
  using L = [:R:]<int>;
  L r2;
  int mem () { return template [:R:]<int>::si; }
};

void
doit ()
{
  f<^^S>();
  f<^^C<int>>();
  A<^^S> a;
  A<^^C<int>> a2;
  f2<^^C>();
  A2<^^C> a3;
}
