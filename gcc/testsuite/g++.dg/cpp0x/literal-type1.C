// CWG 2096 - constraints on literal unions.
// { dg-do compile { target c++11 } }

struct literal { };
typedef volatile int nonliteral_v;
struct nonliteral {
  nonliteral() {}
};

union U {
  literal l;
  nonliteral n;

  constexpr U() : l{} {}
};

constexpr U u{};

union U2 {
  nonliteral n;
  literal l;

  constexpr U2() : l{} {}
};

constexpr U2 u2{};

union U3 { // { dg-message "not literal" }
  nonliteral_v n; // { dg-message "volatile type" }

  constexpr U3() : n{} {}
};

constexpr U3 u3{}; // { dg-error "not literal" }

union U4 {
  nonliteral n;
  nonliteral_v n2;
  literal l;
  nonliteral n3;

  constexpr U4() : l{} {}
};

constexpr U4 u4{};

union U5 {
  nonliteral_v n;
  literal l;

  constexpr U5() : n{} {}
};

constexpr U5 u5{};
