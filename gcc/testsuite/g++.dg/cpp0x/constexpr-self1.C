// PR c++/82115
// { dg-do compile { target c++11 } }

struct A { int const u = 0; };

struct B : A
{ 
  constexpr B (int const *p) : v (p) {}
  int const *v;
};

constexpr B b (&b.u);

template < typename > void foo () { b; }

template < typename T> void foo2 () {
  constexpr B b2 = &b2.u;
  b2;
}
