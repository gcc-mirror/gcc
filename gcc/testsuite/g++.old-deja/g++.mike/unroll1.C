// Build don't link:
// Special g++ Options: -g -O -funroll-loops

struct A {
  inline ~A() { }
};

void foo (A) {
  while (1)
    A bar;
}
