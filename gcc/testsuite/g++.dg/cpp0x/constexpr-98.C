// { dg-options "-std=c++98" }

constexpr int i = 42;	  // { dg-message "std=c\\+\\+0x" }
// { dg-error "constexpr" "" { target *-*-* } 3 }
