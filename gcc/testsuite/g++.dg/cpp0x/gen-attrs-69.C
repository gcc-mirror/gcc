// PR c++/81429 - wrong parsing of constructor with C++11 attribute.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pedantic" }

void fn1([[maybe_unused]] int);
void fn2(int a [[maybe_unused]]);
void fn3(__attribute__((unused)) int);
void fn4(int __attribute__((unused)));

struct S1 {
  S1([[maybe_unused]] int);
};

struct S2 {
  S2([[maybe_unused]] int, [[maybe_unused]] int);
};

struct S3 {
  S3(int a [[maybe_unused]]);
};

struct S4 {
  S4(int a [[maybe_unused]], int b [[maybe_unused]]);
};

struct S5 {
  S5(__attribute__((unused)) int);
};

struct S6 {
  S6(__attribute__((unused)) int, __attribute__((unused)) int);
};

struct S7 {
  S7(int __attribute__((unused)));
};

struct S8 {
  S8(int __attribute__((unused)), int __attribute__((unused)));
};
