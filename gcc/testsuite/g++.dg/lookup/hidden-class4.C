// Copyright (C) 2005 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

class A {
  friend class B;
};

class B *b;
B *c;
