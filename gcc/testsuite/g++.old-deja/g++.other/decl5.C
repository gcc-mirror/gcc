// { dg-do assemble  }

// Copyright (C) 1999, 2000, 2002, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Apr 1999 <nathan@acm.org>
// derived from a bug report by <rch@larissa.sd.bi.ruhr-uni-bochum.de>
// http://gcc.gnu.org/ml/gcc-bugs/1999-04n/msg00631.html
// the code is wrong, but we fell over badly


struct A {
  int A::fn();        // { dg-error "extra qualification" } 
  int A::m;           // { dg-error "extra qualification" } 
  struct e;
  struct A::e {int i;}; // { dg-error "extra qualification" "qual" } 
  // { dg-error "anonymous struct" "anon" { target *-*-* } .-1 }
  struct A::expand {  // { dg-error "qualified name" } 
  int m;
  };
  struct Z;
  expand me;          // { dg-error "'expand' does not name a type" }
  void foo(struct A::e);
  void foo(struct A::z);  // { dg-error "incomplete" }
};

struct Q;
struct B {
  struct A::fink {    // { dg-error "does not name a class before" }
  int m;
  };
  struct A::Z {       // { dg-error "does not enclose" } A::Z not a member of B
    int m;
  };
  int m;
  int n;
  struct ::Q {        // { dg-error "global qual" } ::Q not a member of B
    int m;
  };
  int A::fn() {       // { dg-error "cannot define member" } A::fn not a member of B
    return 0;
  }
  void fn(struct ::Q &);
  void foo(struct A::y);  // { dg-error "does not name a type" } no such member
};

struct ::C {          // { dg-error "invalid before" } extra qualification
  int i;
};

namespace N {
  int fn();
  struct F;
}

namespace NMS
{
  void NMS::fn();     // { dg-error "should have been" }
  int NMS::i;         // { dg-error "should have been" }
  struct NMS::D {     // { dg-error "does not name a class" }
    int i;
  };
  struct N::E {       // { dg-error "does not name a class" } no such type
    int i;
  };
  struct ::F {        // { dg-error "global qual" } no such type
    int i;
  };
  int N::fn() {       // { dg-error "namespace" } N::fn not a member of NMS
    return 0;
  }
  struct N::F {       // { dg-error "namespace" } N::F not a member of NMS
    int i;
  };
}

NMS::D thing;         // { dg-error "'D' in namespace 'NMS' does not name a type" }
void NMS::fn()
{
  i = 3;
}

// From PR c++/15766 - bad parse error recovery (2 bugs)
void confusion1(const UndefinedType& a)  // { dg-error "does not name a type" }
{
}

