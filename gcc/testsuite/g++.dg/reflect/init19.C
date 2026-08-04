// PR c++/125280
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct C { decltype(^^::) i; };
struct N { int i; };

template<typename T>
struct X : T { };

auto a = X<C>{};
auto b = X<N>{};
