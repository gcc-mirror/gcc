// PR c++/58678
// { dg-options "-O3 -flto -shared -fPIC -Wl,--no-undefined" }
// { dg-do link { target { { gld && fpic } && shared } } }
// { dg-require-effective-target lto }

struct A {
  virtual ~A();
};
struct B : A {
  virtual int m_fn1();
};
void fn1(B* b) {
  delete b;
}

int main() {}
