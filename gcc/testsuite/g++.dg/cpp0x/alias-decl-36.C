// PR c++/53658
// { dg-do compile { target c++11 } }

struct A;
template <typename> using Foo = const A;
template <typename Item> Foo <Item> bar();
