// { dg-do assemble  }
// { dg-options "-Wall" }

class B {
public:
  B(int) { }
};

class D : public B {
  int member;		                // { dg-warning "" } reordered
  D() : member(0), B(member) { }	// { dg-warning "" } reordered
};
