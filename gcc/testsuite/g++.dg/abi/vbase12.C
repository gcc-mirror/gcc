// { dg-do run }
// { dg-options "-fabi-version=0" }

struct A {};
struct B { A a; virtual void f () {} };
struct C : public B, virtual public A {};
struct D : public C, virtual public A {};

D d;

int main () {
  if (((char*)(A*)&d - (char*)&d) != 0)
    return 1;
}
