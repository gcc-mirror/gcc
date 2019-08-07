// PR c++/81429 - wrong parsing of constructor with C++11 attribute.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wunused-parameter -Wno-pedantic" }

void fn1([[maybe_unused]] int a) { }
void fn2(int a [[maybe_unused]]) { }
void fn3(__attribute__((unused)) int a) { }
void fn4(int a __attribute__((unused))) { }

struct S1 {
  S1([[maybe_unused]] int a) { }
};

struct S2 {
  S2([[maybe_unused]] int f, [[maybe_unused]] int a) { }
};

struct S3 {
  S3(int a [[maybe_unused]]) { }
};

struct S4 {
  S4(int f [[maybe_unused]], int a [[maybe_unused]]) { }
};

struct S5 {
  S5(__attribute__((unused)) int a) { }
};

struct S6 {
  S6(__attribute__((unused)) int f, __attribute__((unused)) int a) { }
};

struct S7 {
  S7(int a __attribute__((unused))) { }
};

struct S8 {
  S8(int f __attribute__((unused)), int a __attribute__((unused))) { }
};
