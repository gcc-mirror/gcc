// { dg-do assemble  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Aug 1999 <nathan@acm.org>

// Bug 3
// typenames are not injected early enough, [basic.scope.pdecl]3.3.1/4
// indicates this should compile.

struct A {
};

struct B : A {
  typedef A Parent;
  struct F {
  };
};

struct C : B {
  typedef B Parent;
  struct G {};
  struct F : C::Parent::F {
    typedef C::Parent::F Parent;
  };
};

struct D : B {
  typedef B Parent;
  struct F : D::Parent::F { // finds the wrong Parent
    typedef D::Parent::F Parent;
  };
};
