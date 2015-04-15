// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Access checking during explicit instantiation.

class A {
  typedef int X;		// { dg-message "private" }
};

class X {
  private:
  template <typename T> struct Y;
};

template <> struct X::Y<int> {
  A::X x;			// { dg-error "this context" }
};

template <typename T> struct X::Y {
  typename T::X x;		// { dg-error "this context" }
};

template struct X::Y<A>;	// { dg-message "required from here" }
