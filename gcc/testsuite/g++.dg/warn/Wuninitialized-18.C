// PR c++/96121
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

struct A {
  A();
  int i;
};
struct B {
  B(A);
  int i;
};

struct composed2 {
  B b_;
  A a_;
  composed2() : b_(a_) {} // { dg-warning "member .composed2::a_. is used uninitialized" }
};

composed2 test() {
    return composed2{};
}
