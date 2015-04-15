// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Deferred access checking of variable declaration.

class A {
  typedef int X;	// { dg-message "private" }
  static X a, b, c;
};

A::X A::a;
A::X A::b, x;		// { dg-error "this context" }
A::X y, A::c;		// { dg-error "this context" }
A::X z;			// { dg-error "this context" }
