// PR c++/120040
// { dg-additional-options "-fmodules -std=c++20" }
// { dg-module-cmi M }

export module M;

struct S {
  constexpr ~S() {}
};

export constexpr bool foo() {
  S* a = new S[3];
  delete[] a;
  return true;
}

export constexpr S* bar() {
  return new S[3];
}
