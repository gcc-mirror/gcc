// PR c++/67012
// { dg-do compile { target c++14 } }

decltype(auto) f() -> int; // { dg-error "function with trailing return type has" }
