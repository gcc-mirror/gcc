// PRMS Id: 4694
// Bug: g++ doesn't realize that A::i refers to a member of `this' in B().
// Build don't link:

class A {
protected:
  int i;
};

struct B : public A {
  B () { A::i = 0; }
};

struct C : public B {
  C () { B::i = 0; }
};
