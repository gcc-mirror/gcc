// bool test case

// Build don't link:

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
