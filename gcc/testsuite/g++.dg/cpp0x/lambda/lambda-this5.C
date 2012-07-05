// PR c++/53619
// { dg-do run { target c++11 } }

struct C {
  int x;
};
struct B {
  int q;
};
struct A : public B , C  {
  void foo();
};

void A::foo() {
  auto k = [this]() {return (void *)(&x);};
  if (k() != (void*)&x)
    __builtin_abort();
}

int main(int l, char **) {
  A a; a.foo();
}
