struct B {
  void g() { }
};

struct A {
  void f() {
    B &b = b;
    b.g();
  }
};

int main(void) { }
