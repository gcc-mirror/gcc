// { dg-do compile }

// PR c++/4100
// You can't use friend when defining a class.

class A {
  friend class B { };	// { dg-error "friend" }
};
