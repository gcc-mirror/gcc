// PR c++/60249
// { dg-do compile { target c++11 } }

decltype(""_) x; // { dg-error "unable to find string literal operator" }

// { dg-error "invalid type in declaration before" "invalid" { target *-*-* } 4 }
