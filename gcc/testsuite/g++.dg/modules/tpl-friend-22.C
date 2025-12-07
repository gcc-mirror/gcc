// PR c++/122819
// { dg-do compile { target *-*-*gnu* } }
// { dg-additional-options "-fmodules" }

export module M;

template <typename T> struct S;
void foo(S<float>);

template <typename T> struct S {
  friend void foo(S) {}
};

void foo(S<double>);

void use() {
  foo(S<int>{});
  foo(S<float>{});
  foo(S<double>{});
}

// { dg-final { scan-assembler "_ZW1M3fooS_1SIiE,comdat" } }
// { dg-final { scan-assembler "_ZW1M3fooS_1SIfE,comdat" } }
// { dg-final { scan-assembler "_ZW1M3fooS_1SIdE,comdat" } }
