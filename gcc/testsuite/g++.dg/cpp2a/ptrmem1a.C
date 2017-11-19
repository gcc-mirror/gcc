// P0704R1
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  void ref() & {}
  void cref() const& {}
  void vref() volatile & {}
  void cvref() const volatile & {}
};

void
foo ()
{
  S{}.ref();		// { dg-error "argument discards qualifiers" }
  S{}.cref();
  S{}.vref();		// { dg-error "argument discards qualifiers" }
  S{}.cvref();		// { dg-error "argument discards qualifiers" }

  (S{}.*&S::ref)();	// { dg-error "pointer-to-member-function type 'void \\(S::\\*\\)\\(\\) &' requires an lvalue" }
  (S{}.*&S::cref)();
  (S{}.*&S::vref)();	// { dg-error "pointer-to-member-function type 'void \\(S::\\*\\)\\(\\) volatile &' requires an lvalue" }
  (S{}.*&S::cvref)();	// { dg-error "pointer-to-member-function type 'void \\(S::\\*\\)\\(\\) const volatile &' requires an lvalue" }
}
