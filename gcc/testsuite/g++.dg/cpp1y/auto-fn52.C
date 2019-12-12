// PR c++/67012
// { dg-do compile { target c++14 } }

decltype(auto) f() -> int; // { dg-error "1:.f. function with trailing return type has" }
