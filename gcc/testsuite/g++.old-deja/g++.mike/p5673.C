// { dg-do run  }
// prms-id: 5673

class A {
public:
  operator int () {
    return 7;
  }
  ~A();
};

int foo() {
  return A();
}

int main() {
  return foo() != 7;
}

A::~A() {
}
