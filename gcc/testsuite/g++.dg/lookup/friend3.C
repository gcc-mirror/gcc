// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Friend name lookup in class defined outside its namespace

namespace A {
  class B;
  class C;
}

class A::B {
  friend class C;
  typedef int i;
};

class A::C {
  A::B::i j;
};
