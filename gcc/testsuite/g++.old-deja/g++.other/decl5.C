// Build don't link:

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Apr 1999 <nathan@acm.org>
// derived from a bug report by <rch@larissa.sd.bi.ruhr-uni-bochum.de>
// http://gcc.gnu.org/ml/gcc-bugs/1999-04n/msg00631.html
// the code is wrong, but we fell over badly


struct A {
  int A::fn();        // WARNING - extra qualification
  int A::m;           // WARNING - extra qualification
  struct e;
  struct A::e {int i;};
  struct A::expand {  // WARNING - extra qualification
  int m;
  };
  struct Z;
  expand me;
  void foo(struct A::e);
  void foo(struct A::z);  // WARNING - extra qualification
};

struct Q;
struct B {
  struct A::fink {    // ERROR - no such member
  int m;
  };
  struct A::Z {       // ERROR XFAIL - A::Z not a member of B
    int m;
  };
  int m;
  int n;
  struct ::Q {        // ERROR XFAIL - ::Q not a member of B
    int m;
  };
  int A::fn() {       // ERROR - A::fn not a member of B
    return 0;
  }
  void fn(struct ::Q &);
  void foo(struct A::y);  // ERROR - no such member
};

struct ::C {          // WARNING - extra qualification
  int i;
};

namespace N {
  int fn();
  struct F;
}

namespace NMS
{
  void NMS::fn();     // WARNING - extra qualification XFAIL
  int NMS::i;         // WARNING - extra qualification XFAIL
  struct NMS::D {     // WARNING - extra qualification
    int i;
  };
  struct N::E {       // ERROR - no such type
    int i;
  };
  struct ::F {        // ERROR - no such type
    int i;
  };
  int N::fn() {       // ERROR - N::fn not a member of NMS
    return 0;
  }
  struct N::F {       // ERROR XFAIL - N::F not a member of NMS
    int i;
  };
}

NMS::D thing;
void NMS::fn()
{
  i = 3;
}
