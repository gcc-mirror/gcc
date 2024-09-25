// PR c++/105483
// { dg-do compile }

struct X { };
void g () {
  X::X x; // { dg-error "'X::X' names the constructor" }
}
