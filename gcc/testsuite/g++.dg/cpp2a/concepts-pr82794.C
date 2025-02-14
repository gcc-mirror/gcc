// PR c++/82794
// { dg-do compile { target c++20 } }

template<typename F, typename G = F>
concept Foo = true;

template<Foo fun>
using foo = void;
