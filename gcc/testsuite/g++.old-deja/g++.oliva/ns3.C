// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Ovidiu Toader <ovi@physics.utoronto.ca>

// crash test - XFAIL *-*-*

namespace N {
  template <typename T> class A {
    template <typename T_> friend class A;
  };
}
