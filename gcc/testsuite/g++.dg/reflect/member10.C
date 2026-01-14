// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

/* ??? clang rejects all of these; I'm not entirely sure why.
   [expr.prim.id.general]/2 says that the implicit transformation
   whereby an id-expression denoting a NSDM becomes a class member
   access doesn't apply to a splice-expression.  OK, so we reject
   [:^^k:], but [:^^S:]::k doesn't seem to be the same case?  */

// Non-dependent case
struct S {
  int k;

  void fn2() { }

  void fn() {
    (void) [:^^k:];  // { dg-error "cannot implicitly reference" }
    (void) [:^^S:]::k;
    [:^^fn:]();  // { dg-error "cannot implicitly reference" }
    [:^^S:]::fn2();
  }
};

// Dependent case
struct D {
  int k;

  void fn2() { }

  template <typename T>
  void fn() {
    (void) [:^^T:]::k;
    [:^^T:]::fn2();
  }
};

void runner() {
    D f = {4};
    f.fn<D>();
}
