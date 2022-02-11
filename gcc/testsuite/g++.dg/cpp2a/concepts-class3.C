// PR c++/103831
// { dg-do compile { target c++20 } }

struct A {
  constexpr int size() { return 42; } // non-static
};

template<class T>
  requires (T::size() == 42) // { dg-error "without object" }
struct B : T { };

template struct B<A>; // { dg-error "constraint" }
