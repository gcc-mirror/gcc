// PR c++/58550
// { dg-options "-std=c++11" }

auto foo();			// { dg-error "auto" }
auto fp = foo;
