// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++20 } }

int foo ();
constexpr int constfoo () { return 42; }
int gl = 42;

struct nonliteral {
  int m;
  nonliteral() : m() { }
  nonliteral(int n) : m(n) { }
  ~nonliteral() {}
};

struct literal {
  int m;
  constexpr literal() : m() { }
  constexpr literal(int n) : m(n) { }
};

struct pod {
  int m;
};

struct S {
  static constinit pod p;
  static constinit pod pc;
  static const constinit nonliteral n;
};

struct W {
  int w = 42;
};

constinit W w;

constinit const int &r1 = gl;
constinit thread_local const int &r2 = gl;
constinit const int &r3 // { dg-error "variable .r3. does not have a constant initializer" }
  = foo (); // { dg-error "call to non-.constexpr. function" }
constinit const literal &r4 = 42;
constinit const nonliteral &r5 // { dg-error "variable .r5. does not have a constant initializer" }
  = 42; // { dg-error "call to non-.constexpr. function" }
constinit const int &r6 = nonliteral(2).m; // { dg-error "variable .r6. does not have a constant initializer|call to non-.constexpr. function" }

constinit pod p1;
constinit pod p2 = { 42 };
constinit pod p3 = { constfoo() };
constinit pod p4 = { foo() }; // { dg-error "variable .p4. does not have a constant initializer|call to non-.constexpr. function" }

constexpr literal lit;
constinit literal l1 = lit;
constinit literal l2 = 42;
constinit literal l3 = constfoo();
constinit literal l4 = foo(); // { dg-error "variable .l4. does not have a constant initializer|call to non-.constexpr. function" }
constinit literal l5 = {};
constinit literal l6{};
constinit thread_local literal l7 = lit;
constinit thread_local literal l8 = 42;
constinit thread_local literal l9 = constfoo();
constinit thread_local literal l10 = foo(); // { dg-error "variable .l10. does not have a constant initializer|call to non-.constexpr. function" }
constinit thread_local literal l11{};

pod S::p;
constinit pod S::pc(S::p); // { dg-error "variable .S::pc. does not have a constant initializer|not usable" }

constinit const nonliteral S::n(42); // { dg-error "variable .S::n. does not have a constant initializer|call to non-.constexpr. function" }
constinit int n1 = nonliteral{42}.m; // { dg-error "variable .n1. does not have a constant initializer|temporary of non-literal type" }
constinit int n2 = literal{42}.m;

void
fn1 ()
{
  const int c = 42;
  static constinit const int &l // { dg-error "variable .l. does not have a constant initializer" }
    = c; // { dg-error "not a constant" }
  static const int &l2 = 10;
  static const int &l3 = gl;
}
