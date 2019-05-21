// Copyright (C) 2005 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

namespace N {
  class A {
    friend class B;
  };
}

using N::B;	// { dg-error "declared" }
