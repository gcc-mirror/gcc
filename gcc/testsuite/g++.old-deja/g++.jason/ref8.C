// PRMS Id: 5184
// Bug: cast to C& below does not adjust address

struct A {};
struct B {
  virtual void foo () {};
};
struct C : public B, public A {};

int main() {
    C c;

    A& ar = c;
    C& cr = (C&)ar;

    cr.foo();	// this line causes core dump
}
