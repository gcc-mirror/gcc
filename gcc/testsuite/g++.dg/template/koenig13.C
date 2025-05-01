// PR c++/119034
// { dg-do compile { target c++14 } }
// A version of koenig12.C involving partial instantiation via a generic lambda.

void foo(...) = delete;

template <class T> void lookup(T t) { [](auto u) { foo(u); }(t); }

namespace N {
 struct A { };
 int foo(A);
}

int main() {
  lookup(N::A{});
}
