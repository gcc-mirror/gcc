// PR c++/58550
// { dg-do compile { target c++11 } }

auto foo();			// { dg-error "auto" "" { target { ! c++1y } } }
auto fp = foo;			// { dg-error "auto" "" { target c++1y } }
