// Copyright (C) 2005 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

class A {
  template <class T> friend class B;
  B<int> *b;	// { dg-error "no type|expected" }
};
