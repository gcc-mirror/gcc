#include <typeinfo>

struct A {
  virtual ~A() {}
};

int main() {
   A* a = new A;
   typeid(*a).name();
}

