// { dg-do assemble  }
// PRMS Id: 4900
// Bug: g++ doesn't apply access control uniformly to type conversion operators

struct A {
protected:
  operator int * () const;
};

struct B : public A {
  int * foo () { return A::operator int *(); }
};
