// { dg-do assemble  }
// { dg-options "-Wall -pedantic" }
// GROUPS passed qualifiers
class bee {
 public:
  int bee::bar;		// { dg-error "extra" } there is an extra bee:: here
};

class foo {
 public:
  int bee::bar;		// { dg-error "invalid use" } you cannot do this
    int me();
};
