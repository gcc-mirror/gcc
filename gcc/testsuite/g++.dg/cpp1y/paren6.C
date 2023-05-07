// PR c++/98283
// { dg-do compile { target c++14 } }

struct A {
  static int m;
};

template<class T>
struct B : T {
  decltype(auto) f() { return (this->m);  }
};

using type = decltype(B<A>().f());
using type = int&;
