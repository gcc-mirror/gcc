// Special g++ Options: -O -funroll-loops

struct A {
  ~A();
};

void foo();

int main() {
  for (int i=0;i<4;i++) {
    A a;
    foo();
  }
}

void foo() { }
A::~A() { }
