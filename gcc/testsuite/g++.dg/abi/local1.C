// { dg-do run }
// { dg-additional-sources "local1-a.cc" }

#include <typeinfo>

struct B {
  virtual void b() {}
};

static B* f() {
  struct D : public B {
  };

  return new D;
}

extern B* g();

int main () {
  if (typeid (*f()) == typeid (*g()))
    return 1;
}
