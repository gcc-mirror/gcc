// { dg-do assemble  }
// bool test case


void foo(int i) {
  foo (true);
}

struct C {
  void foo(int i) {
    foo(true);
  }
  void bar(bool b) {
    bar(0);
  }
};
