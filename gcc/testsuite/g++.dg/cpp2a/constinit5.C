// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++2a } }
// Check that we preserve DECL_DECLARED_CONSTINIT_P in duplicate_decls.

int gl = 42;

struct S {
  constinit static int m;
  constinit static int n;
  constinit static const int &r1;
  constinit static const int &r2;
};

int S::m = 42;
int nonconst;
constinit int S::n = nonconst; // { dg-error "variable .S::n. does not have a constant initializer" }
// { dg-error "not usable in a constant expression" "" { target *-*-* } .-1 }

const int &S::r1 = gl;
const int &S::r2 = 42;

struct T {
  constinit static thread_local const int &r1;
  constinit static thread_local const int &r2;
};
constinit thread_local const int &T::r1 = gl;
constinit thread_local const int &T::r2 = 42; // { dg-error "variable .T::r2. does not have a constant initializer|not a constant expression" }
