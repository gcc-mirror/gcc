// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Verify that a friend class is hidden even if it is hidden by a non-builtin
// function name.

namespace M {
  void F (void);
  class F;
}

namespace N {
  void F(void);
  class A {
    friend class F;
  };
}

using namespace M;
using namespace N;

class F *b;
