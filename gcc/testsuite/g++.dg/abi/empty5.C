// { dg-options "-fabi-version=0" }

struct A {};

struct B {
  A a;
  virtual void f () {}
};

struct C : public B, public A {};

C c;

int main () {
  if ((void*) (A*) &c != &c)
    return 1;
}
