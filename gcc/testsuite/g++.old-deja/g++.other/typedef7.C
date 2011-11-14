// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef int I;
typedef int I;

// DR56 makes clear that duplicate typedefs in class scopes are
// invalid.

struct A {
  typedef int I; // { dg-message "" }
  typedef int I; // { dg-error "" }
};

template <class T>
struct S {
  typedef int I;  // { dg-message "" }
  typedef int I;  // { dg-error "" }
};

