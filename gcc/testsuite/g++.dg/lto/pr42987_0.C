// { dg-lto-do run }
// { dg-lto-options { { -flto -g } { -flto -flto-partition=none -g } } }

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
