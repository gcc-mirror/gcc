// PR c++/104846
// { dg-do compile { target c++14 } }

struct S {
  auto operator delete (void *) {} // { dg-error ".operator delete. must return type .void'" }
  auto operator delete[] (void *) {} // { dg-error ".operator delete. must return type .void'" }
};

