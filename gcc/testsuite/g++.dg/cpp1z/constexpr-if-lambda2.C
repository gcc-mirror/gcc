// PR c++/92654
// { dg-do compile { target c++17 } }

struct A {
  constexpr operator int () { return 42; }
};
template <int I> struct C {
  static const bool ap = I;
};
template <auto I>
void am() {
  [](auto an)
     {
       if constexpr (C<an>::ap) ;
     }(A{});
}
void fn() { am<42>(); }
