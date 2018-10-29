// { dg-options "-fabi-version=0" }

struct A {};

struct B {
  A a;
  virtual void f () {}
};

struct C : public B, public A {};

#if __cpp_attributes
struct C2 : public B
{
  [[no_unique_address]] A a;
} c2;
#endif

C c;

int main () {
  if ((void*) (A*) &c != &c)
    return 1;
#if __cpp_attributes
  if ((void*)&c2.a != &c2)
    return 2;
#endif
}
