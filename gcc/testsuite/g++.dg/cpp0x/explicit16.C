// PR c++/109159
// { dg-do compile { target c++11 } }

struct A {
  A(float) {}
  template<class U>
  explicit A(U) {}
};

void f(A t)
{
  t = {1}; // { dg-error "explicit constructor" }
  t = 1;
  A a1{1};
  A a2 = {1}; // { dg-error "explicit constructor" }
  A a3 = 1;
  A a4(1);
}
