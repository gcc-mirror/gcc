// { dg-do assemble  }
// { dg-options "-fexceptions" }

struct A {
  ~A();
};

int main(int argc, char** argv) {
  A a;
  return 0;
}


A::~A() {
}
