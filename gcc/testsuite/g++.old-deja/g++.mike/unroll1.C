// { dg-do assemble  }
// { dg-options "-g -O -funroll-loops" }

struct A {
  inline ~A() { }
};

void foo (A) {
  while (1)
    A bar;
}
