// PR c++/51588

enum A {};

struct B : A {			// { dg-error "" }
  int i;
};

int A::* p = &B::i;		// { dg-error "" }
