// Copyright (C) 2005 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

namespace N {
  class A {
    friend class B;
  };
}

class N::B {	// { dg-error "not name a class" }
};
