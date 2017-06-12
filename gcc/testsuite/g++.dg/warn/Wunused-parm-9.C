// PR c++/79746
// { dg-do compile }
// { dg-options "-Wunused-but-set-parameter" }

struct A {
  A (const char *x) : a(x) {}	// { dg-bogus "set but not used" }
  virtual int foo () = 0;
  const char *a;
};
struct B : public virtual A {
  B (const char *x) : A(x) {}	// { dg-bogus "set but not used" }
};
