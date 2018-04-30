// PR c++/45033

struct A {
  operator int*() { return 0; }
  operator int*() const { return 0; }
};

int main() {
  A a;
  int *p = a;
  delete a;
}
