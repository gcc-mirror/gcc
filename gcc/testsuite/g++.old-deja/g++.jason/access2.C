// Bug: g++ doesn't deal with friends also being derived classes.
// Build don't link:

class A {
  int i;
  friend class B;
};

class B : public A {
  void f () { i = 1; }
};
