// PR c++/38798, DR 770
// { dg-options -std=c++11 }

struct A {};
auto foo() -> struct A {}

enum B {};
auto bar() -> enum B {}

auto baz() -> struct C {} {}	// { dg-error "" }
