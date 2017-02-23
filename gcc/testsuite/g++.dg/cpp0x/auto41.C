// PR c++/58550
// { dg-do compile { target c++11 } }

auto foo();			// { dg-error "auto" "" { target { ! c++14 } } }
auto fp = foo;			// { dg-error "auto" }
