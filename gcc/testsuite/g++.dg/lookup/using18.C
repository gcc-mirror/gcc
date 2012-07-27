// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

namespace N1 {
  void f ();
  struct f; // { dg-message "" "candidate" }
}

namespace N2 {
  void f (int);
  struct f; // { dg-message "" "candidate" }
}

namespace M {
  using namespace N1;
  using namespace N2;
}

using M::f; // { dg-error "ambiguous" }
