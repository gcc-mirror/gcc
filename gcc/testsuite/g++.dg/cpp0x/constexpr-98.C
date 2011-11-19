// { dg-options "-std=c++98" }

constexpr int i = 42;	  // { dg-message "std=c\\+\\+11" }
// { dg-error "constexpr" "" { target *-*-* } 3 }
