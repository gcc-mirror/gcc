// { dg-do compile { target c++11 } }
// { dg-bogus "not supported by" "" { target *-*-* } 0 }

template <int...> struct A {};

template <int... N> using B = A<+N...>;

B<int> b; // { dg-error "type/value mismatch" }
          // { dg-message "expected a constant" "expected" { target *-*-* } .-1 }
