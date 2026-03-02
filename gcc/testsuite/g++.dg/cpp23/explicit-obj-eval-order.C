// PR c++/123989
// { dg-do run { target c++23 } }

struct A {
  int m = 42;

  void f(this A self, int n) {
    if (self.m != 42 || n != 43)
      __builtin_abort();
  }
};

int main() {
  A a;
  a.f(++a.m);
}
