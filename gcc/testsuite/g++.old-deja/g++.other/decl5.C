// { dg-do assemble  }

// Copyright (C) 1999, 2000, 2002, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Apr 1999 <nathan@acm.org>
// derived from a bug report by <rch@larissa.sd.bi.ruhr-uni-bochum.de>
// http://gcc.gnu.org/ml/gcc-bugs/1999-04n/msg00631.html
// the code is wrong, but we fell over badly


struct A {
  int A::fn();        // { dg-warning "" } extra qualification
  int A::m;           // { dg-warning "" } extra qualification
  struct e;
  struct A::e {int i;}; // { dg-warning "" } extra qualification
  struct A::expand {  // { dg-warning "" } extra qualification
  int m;
  };
  struct Z;
  expand me;
  void foo(struct A::e);
  void foo(struct A::z);  // { dg-warning "" } extra qualification
};

struct Q;
struct B {
  struct A::fink {    // { dg-error "" } no such member
  int m;
  };
  struct A::Z {       // { dg-error "" } A::Z not a member of B
    int m;
  };
  int m;
  int n;
  struct ::Q {        // { dg-error "" } ::Q not a member of B
    int m;
  };
  int A::fn() {       // { dg-error "" } A::fn not a member of B
    return 0;
  }
  void fn(struct ::Q &);
  void foo(struct A::y);  // { dg-error "" } no such member
};

struct ::C {          // { dg-warning "" } extra qualification
  int i;
};

namespace N {
  int fn();
  struct F;
}

namespace NMS
{
  void NMS::fn();     // { dg-warning "" "" } extra qualification
  int NMS::i;         // { dg-warning "" "" } extra qualification
  struct NMS::D {     // { dg-warning "" } extra qualification
    int i;
  };
  struct N::E {       // { dg-error "" } no such type
    int i;
  };
  struct ::F {        // { dg-error "" } no such type
    int i;
  };
  int N::fn() {       // { dg-error "" } N::fn not a member of NMS
    return 0;
  }
  struct N::F {       // { dg-error "" } N::F not a member of NMS
    int i;
  };
}

NMS::D thing;
void NMS::fn()
{
  i = 3;
}
