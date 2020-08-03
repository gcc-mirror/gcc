// PR c++/96182
// { dg-do compile { target c++11 } }

constexpr int foo () {} // { dg-error "no return statement in 'constexpr' function returning non-void" "" { target c++14 } }
// { dg-error "body of 'constexpr' function 'constexpr int foo\\\(\\\)' not a return-statement" "" { target c++11_only } .-1 }
// { dg-warning "no return statement in function returning non-void" "" { target c++11_only } .-2 }
