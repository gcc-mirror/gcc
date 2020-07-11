// DR 2289
// PR c++/94553
// { dg-do compile { target c++17 } }
// A structured binding must be unique in its declarative region.

void
f ()
{
  int arr[1] = { 1 };
  struct A { };
  auto [A] = arr; // { dg-error "redeclared as different kind of entity" }
  auto [B] = arr;
  struct B { }; // { dg-error "redeclared as different kind of entity" }
}
