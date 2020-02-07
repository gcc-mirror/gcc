// PR c++/92654
// { dg-do compile { target c++17 } }

template <unsigned long> struct C;
template <auto I>
void am() {
  [](auto an)
     {
       if constexpr (C<an>::ap) ; // { dg-error "constant" }
     }(42);
}
void fn() { am<42>(); }
