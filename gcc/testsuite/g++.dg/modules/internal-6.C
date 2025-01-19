// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !M }
// Exposures (or not) of TU-local values

export module M;

static void f() {}
auto& fr = f;   // OK
constexpr auto& fr2 = fr;  // { dg-error "initialized to a TU-local value" }
static constexpr auto fp2 = fr;  // OK

struct S { void (&ref)(); } s{ f };  // OK, value is TU-local
constexpr extern struct W { S& s; } wrap{ s };  // OK, value is not TU-local
constexpr S s2{ f };  // { dg-error "initialized to a TU-local value" }

constexpr int a = 123;
static constexpr int b = 456;
struct X {
  union {
    const int* p[2];
  };
};
constexpr X x { &a };  // OK
constexpr X y { &a, &b };  // { dg-error "initialized to a TU-local value" }
