// Special g++ Options: -fexceptions
// Build don't link:

struct A {
  ~A();
};

int main(int argc, char** argv) {
  A a;
  return 0;
}


A::~A() {
}
