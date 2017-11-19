// PR c++/38798, DR 770
// { dg-do compile { target c++11 } }

struct A {};
auto foo() -> struct A { return A(); }

enum B {};
auto bar() -> enum B { return B(); }

auto baz() -> struct C {} {}	// { dg-error "" }
