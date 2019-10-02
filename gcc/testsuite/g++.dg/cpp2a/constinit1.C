// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++2a } }
// Test basic usage of 'constinit'.

const char *g() { return "dynamic init"; }
constexpr const char *f(bool p) { return p ? "constant init" : g(); } // { dg-error "call to non-.constexpr. function" }

constinit const char *c = f(true);
constinit const char *d = f(false); // { dg-error "variable .d. does not have a constant initializer" }
// { dg-message "in .constexpr. expansion of" "" { target *-*-* } .-1 }
static constinit const char *e = f(true);

constexpr int foo(int x) { return x; }
constinit int i = foo(42);
constinit int j // { dg-error "variable .j. does not have a constant initializer" }
  = foo(i); // { dg-error "not usable in a constant expression" }

int y = 42;
constinit int x // { dg-error "variable .x. does not have a constant initializer" }
  = y; // { dg-error "not usable in a constant expression" }

constinit int z;
const constinit unsigned cst = 1u;

void
fn ()
{
  static constinit int m = foo(42);
  static constinit int n // { dg-error "variable .n. does not have a constant initializer" }
    = foo(m); // { dg-error "not usable in a constant expression" }

  // Make sure we can still modify constinit variables.
  c = "foo";
  i = 10;
  m = 90;
  // ... unless they're 'const'.
  cst *= 2; // { dg-error "assignment of read-only variable" }
}
