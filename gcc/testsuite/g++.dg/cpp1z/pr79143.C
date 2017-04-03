// PR c++/79143
// { dg-do compile }
// { dg-options "-std=c++1z" }

struct base {
  base (int, int) {}
};

template<class>
struct derived : base {
  using base::base;
};

template<class>
struct derived2 : base {
  derived2 (int x, int y) : base (x, y) {}
};

int
main ()
{
  base (13, 42);
  derived<int> (13, 42);
  derived2<int> (13, 42);
  base{13, 42};
  derived<int>{13, 42}; // { dg-bogus "too many initializers" }
  derived2<int>{13, 42};
}
