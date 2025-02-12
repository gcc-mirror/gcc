// PR c++/113800
// { dg-do compile { target c++20 } }

// DR 2450
struct S { int a; };

template<S s>
void
f ()
{
}

void
test ()
{
  f<{0}>();
  f<{.a= 0}>();
}

// DR 2459
struct A {
  constexpr A (float) {}
};

template<A>
struct X {};
X<1> x;
