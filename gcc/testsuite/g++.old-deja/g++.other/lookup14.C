// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

union U {
  typedef int I;

  struct S {
    void f();
  };
};

void U::S::f() {
  I i;
}
