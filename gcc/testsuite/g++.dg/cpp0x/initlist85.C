// PR c++/60848
// { dg-do compile { target c++11 } }

namespace std
{
  struct initializer_list {};  // { dg-error "declaration" }
}

void foo(std::initializer_list &);

void f()
{
  foo({1, 2});  // { dg-error "invalid initialization" }
}
