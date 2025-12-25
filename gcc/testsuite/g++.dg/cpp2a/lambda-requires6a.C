// PR c++/123080
// { dg-do compile { target c++20 } }
auto f = [](auto x) requires requires { // { dg-error "expected | invalid member template | constraints" }
