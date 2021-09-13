// PR c++/68942
// { dg-do compile { target c++11 } }

void foo(...) = delete;

template <class T> void lookup(T t) { foo(t); }

namespace N {
 struct A { };
 int foo(A);
}

int main() {
  lookup(N::A{});
}
